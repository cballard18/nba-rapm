library(tidyverse)
library(data.table)
library(Matrix)
library(MASS)
library(glmnet)

# Get 3 years of play-by-play data from ramiro bentes github
pbp <- rbindlist(lapply(2023:2025, function(x) {
  dt <- read_rds(glue::glue("https://github.com/ramirobentes/nba_pbp_data/raw/main/pbp-final-{x}/data.rds")) |>
    as.data.table()
  dt[, season := x]
  dt
}), fill = TRUE)

# Parse lineups: split comma-separated strings, strip leading jersey numbers
pbp[, `:=`(
  lineup_home = str_split(lineup_home, ",\\s*"),
  lineup_away = str_split(lineup_away, ",\\s*")
)]
# Build player_id → canonical name map before IDs are stripped.
# Takes the most frequently occurring spelling per ID to resolve
# cross-season inconsistencies (e.g. accented vs unaccented names).
.raw <- rbindlist(list(
  data.table(raw = unlist(pbp$lineup_home)),
  data.table(raw = unlist(pbp$lineup_away))
))[grepl("^\\d+\\s+\\S", raw)]
id_name_map <- .raw[,
  .(player_id = str_extract(raw, "^\\d+"), name = str_remove(raw, "^\\d+\\s+"))
][, .N, by = .(player_id, name)][order(-N)][, .(name = first(name)), by = player_id]
rm(.raw)

resolve_lineup_name <- function(raw_vec) {
  ids       <- str_extract(raw_vec, "^\\d+")
  canonical <- id_name_map$name[match(ids, id_name_map$player_id)]
  ifelse(!is.na(canonical), canonical, str_remove(raw_vec, "^\\d+\\s+"))
}

# Expand lineup lists into separate columns (home_1..home_5, away_1..away_5)
for (k in 1:5) {
  set(pbp, j = paste0("home_", k),
      value = resolve_lineup_name(vapply(pbp$lineup_home, `[`, character(1), k)))
  set(pbp, j = paste0("away_", k),
      value = resolve_lineup_name(vapply(pbp$lineup_away, `[`, character(1), k)))
}
pbp[, c("lineup_home", "lineup_away") := NULL]

# ---- Build possession-level data ----
setorder(pbp, game_id, period, secs_game, event_num, number_event)

# Forward-fill possession start time within each game-period
pbp[, start_poss_filled := zoo::na.locf(start_poss, na.rm = FALSE), by = .(game_id, period)]

# New possession when start_poss_filled changes
pbp[, poss_start_flag := start_poss_filled != shift(start_poss_filled, fill = first(start_poss_filled)),
    by = .(game_id, period)]
pbp[, poss_id := cumsum(poss_start_flag), by = .(game_id, period)]

home_cols <- paste0("home_", 1:5)
away_cols <- paste0("away_", 1:5)

# Identify offensive team per possession
poss_df <- pbp[, .(
  home_1 = first(home_1), home_2 = first(home_2), home_3 = first(home_3),
  home_4 = first(home_4), home_5 = first(home_5),
  away_1 = first(away_1), away_2 = first(away_2), away_3 = first(away_3),
  away_4 = first(away_4), away_5 = first(away_5),
  team_home = first(team_home),
  team_away = first(team_away),
  home_pts = sum(shot_pts_home, na.rm = TRUE),
  away_pts = sum(shot_pts_away, na.rm = TRUE),
  off_team = {
    hp <- sum(shot_pts_home, na.rm = TRUE)
    ap <- sum(shot_pts_away, na.rm = TRUE)
    if (hp > 0L) first(team_home)
    else if (ap > 0L) first(team_away)
    else {
      oo <- na.omit(off_team_abb)
      if (length(oo)) last(oo) else NA_character_
    }
  },
  season = first(season)
), by = .(game_id, period, poss_id)]

# Drop possessions where we can't identify the offensive team
poss_df <- poss_df[!is.na(off_team)]
poss_df[, home_on_off := (off_team == team_home)]
poss_df[, poss_idx := .I]
n_poss <- nrow(poss_df)

# Recency weights: most recent season full weight, older seasons discounted
season_weights <- c("2023" = 0.20, "2024" = 0.30, "2025" = 0.50)
w <- season_weights[as.character(poss_df$season)]
sqrt_w <- sqrt(w)

# ---- O/D APM ----
home_long <- melt(poss_df, id.vars = c("poss_idx", "home_on_off"),
                  measure.vars = home_cols, value.name = "player")[!is.na(player)]
away_long <- melt(poss_df, id.vars = c("poss_idx", "home_on_off"),
                  measure.vars = away_cols, value.name = "player")[!is.na(player)]

all_players <- sort(unique(c(home_long$player, away_long$player)))
n_players   <- length(all_players)

home_long[, j_player := match(player, all_players)]
away_long[, j_player := match(player, all_players)]

od_long <- rbind(
  home_long[, .(poss_idx, j = fifelse(home_on_off, j_player, j_player + n_players),
                x = fifelse(home_on_off, 1, -1))],
  away_long[, .(poss_idx, j = fifelse(home_on_off, j_player + n_players, j_player),
                x = fifelse(home_on_off, -1, 1))]
)

col_names <- c(paste0(all_players, "_o"), paste0(all_players, "_d"))

X_od <- sparseMatrix(
  i = od_long$poss_idx,
  j = od_long$j,
  x = od_long$x,
  dims = c(n_poss, n_players * 2L),
  dimnames = list(NULL, col_names)
)

y_od <- fifelse(poss_df$home_on_off, poss_df$home_pts, poss_df$away_pts)

# Weighted least-squares via pseudo-inverse: beta = (X'WX)^{-} X'Wy
X_w <- Diagonal(x = sqrt_w) %*% X_od
y_w <- y_od * sqrt_w
XtX <- as.matrix(crossprod(X_w))
Xty <- as.vector(crossprod(X_w, y_w))
od_coef <- as.vector(ginv(XtX) %*% Xty) * 100  # per-100-possessions

oapm_coef <- od_coef[1:n_players]
dapm_coef <- od_coef[(n_players + 1):(2 * n_players)]

poss_counts <- rbind(home_long[, .(player)], away_long[, .(player)])[, .N, by = player]

apm_df <- tibble(
  player = all_players,
  oapm   = oapm_coef,
  dapm   = dapm_coef,
  apm    = oapm_coef + dapm_coef,
  poss   = poss_counts$N[match(all_players, poss_counts$player)]
) |> arrange(desc(apm))

print(apm_df)

write.csv(apm_df, "datasets/apm.csv", row.names = FALSE)

# ---- RAPM ----
set.seed(42)
cv_fit <- cv.glmnet(
  x           = X_od,
  y           = y_od,
  weights     = w,
  alpha       = 0,
  standardize = FALSE,
  nfolds      = 10
)

rapm_coef <- as.vector(coef(cv_fit, s = "lambda.min"))[-1] * 100  # drop intercept, per-100-poss

orapm_coef <- rapm_coef[1:n_players]
drapm_coef <- rapm_coef[(n_players + 1):(2 * n_players)]

rapm_df <- tibble(
  player = all_players,
  orapm  = orapm_coef,
  drapm  = drapm_coef,
  rapm   = orapm_coef + drapm_coef,
  poss   = poss_counts$N[match(all_players, poss_counts$player)]
) |> arrange(desc(rapm))

print(rapm_df)

write.csv(rapm_df, "datasets/rapm.csv", row.names = FALSE)