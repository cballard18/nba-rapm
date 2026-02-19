library(tidyverse)
library(data.table)
library(Matrix)
library(glmnet)

# ---- Load 3 years of PBP data ----
pbp <- rbindlist(lapply(2023:2025, function(x) {
  dt <- read_rds(glue::glue("https://github.com/ramirobentes/nba_pbp_data/raw/main/pbp-final-{x}/data.rds")) |>
    as.data.table()
  dt[, season := x]
  dt
}), fill = TRUE)

# ---- Parse lineups ----
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

for (k in 1:5) {
  set(pbp, j = paste0("home_", k),
      value = resolve_lineup_name(vapply(pbp$lineup_home, `[`, character(1), k)))
  set(pbp, j = paste0("away_", k),
      value = resolve_lineup_name(vapply(pbp$lineup_away, `[`, character(1), k)))
}
pbp[, c("lineup_home", "lineup_away") := NULL]

# ---- Build possession-level data ----
# msg_type: 1=made FG, 2=missed FG, 3=free throw, 4=rebound, 5=turnover
# opt1: 3=3-pointer, 2=2-pointer, 1=off rebound, 0=def rebound
setorder(pbp, game_id, period, secs_game, event_num, number_event)

pbp[, start_poss_filled := zoo::na.locf(start_poss, na.rm = FALSE), by = .(game_id, period)]
pbp[, poss_start_flag := start_poss_filled != shift(start_poss_filled, fill = first(start_poss_filled)),
    by = .(game_id, period)]
pbp[, poss_id := cumsum(poss_start_flag), by = .(game_id, period)]

home_cols <- paste0("home_", 1:5)
away_cols <- paste0("away_", 1:5)

poss_df <- pbp[, .(
  home_1 = first(home_1), home_2 = first(home_2), home_3 = first(home_3),
  home_4 = first(home_4), home_5 = first(home_5),
  away_1 = first(away_1), away_2 = first(away_2), away_3 = first(away_3),
  away_4 = first(away_4), away_5 = first(away_5),
  team_home = first(team_home),
  team_away = first(team_away),
  home_pts  = sum(shot_pts_home, na.rm = TRUE),
  away_pts  = sum(shot_pts_away, na.rm = TRUE),
  # Four-factor counting stats by side
  fgm_home  = sum(msg_type == 1L & team_abb == team_home,                         na.rm = TRUE),
  fga_home  = sum(msg_type %in% c(1L, 2L) & team_abb == team_home,               na.rm = TRUE),
  fg3m_home = sum(msg_type == 1L & opt1 == 3L & team_abb == team_home,            na.rm = TRUE),
  tov_home  = sum(msg_type == 5L & team_abb == team_home,                         na.rm = TRUE),
  orb_home  = sum(msg_type == 4L & opt1 == 1L & team_abb == team_home,            na.rm = TRUE),
  fta_home  = sum(msg_type == 3L & team_abb == team_home,                         na.rm = TRUE),
  fgm_away  = sum(msg_type == 1L & team_abb == team_away,                         na.rm = TRUE),
  fga_away  = sum(msg_type %in% c(1L, 2L) & team_abb == team_away,               na.rm = TRUE),
  fg3m_away = sum(msg_type == 1L & opt1 == 3L & team_abb == team_away,            na.rm = TRUE),
  tov_away  = sum(msg_type == 5L & team_abb == team_away,                         na.rm = TRUE),
  orb_away  = sum(msg_type == 4L & opt1 == 1L & team_abb == team_away,            na.rm = TRUE),
  fta_away  = sum(msg_type == 3L & team_abb == team_away,                         na.rm = TRUE),
  off_team  = {
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

poss_df <- poss_df[!is.na(off_team)]
poss_df[, home_on_off := (off_team == team_home)]
poss_df[, poss_idx := .I]
n_poss <- nrow(poss_df)

# Derive offensive-team four-factor columns
poss_df[, `:=`(
  off_fgm  = fifelse(home_on_off, fgm_home,  fgm_away),
  off_fga  = fifelse(home_on_off, fga_home,  fga_away),
  off_fg3m = fifelse(home_on_off, fg3m_home, fg3m_away),
  off_tov  = fifelse(home_on_off, tov_home,  tov_away),
  off_orb  = fifelse(home_on_off, orb_home,  orb_away),
  off_fta  = fifelse(home_on_off, fta_home,  fta_away)
)]

# ---- Recency weights ----
season_weights <- c("2023" = 0.20, "2024" = 0.30, "2025" = 0.50)
w <- season_weights[as.character(poss_df$season)]

# ---- Build sparse design matrix (identical to RAPM intro.R) ----
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

poss_counts <- rbind(home_long[, .(player)], away_long[, .(player)])[, .N, by = player]

# ---- Response vectors for the four factors ----
# eFG numerator: FGM + 0.5*FG3M. Ridge coef = per-100-poss eFG RAPM contribution.
y_efg <- poss_df$off_fgm + 0.5 * poss_df$off_fg3m

# TOV: 1 if possession ended in a turnover, 0 otherwise.
# Higher otov_rapm = more turnovers generated (bad); higher dtov_rapm = more turnovers forced (good).
y_tov <- as.numeric(poss_df$off_tov > 0)

# ORB: 1 if offense grabbed an offensive rebound this possession, 0 otherwise.
y_orb <- as.numeric(poss_df$off_orb > 0)

# FTR: free throw attempts drawn per possession.
y_ftr <- as.numeric(poss_df$off_fta)

# ---- Fit one ridge regression per factor ----
fit_factor_rapm <- function(y, label_o, label_d) {
  cat(sprintf("  Fitting %s / %s ...\n", label_o, label_d))
  set.seed(42)
  cv_fit <- cv.glmnet(
    x           = X_od,
    y           = y,
    weights     = w,
    alpha       = 0,           # ridge
    standardize = FALSE,       # +1/-1 encoding is already on the same scale
    nfolds      = 10
  )
  coefs <- as.vector(coef(cv_fit, s = "lambda.min"))[-1] * 100  # drop intercept; scale to per-100
  tibble(
    player      = all_players,
    !!label_o  := coefs[1:n_players],
    !!label_d  := coefs[(n_players + 1L):(2L * n_players)]
  )
}

cat("Running four-factor RAPM ridge regressions...\n")
efg_df <- fit_factor_rapm(y_efg, "oefg_rapm", "defg_rapm")
tov_df <- fit_factor_rapm(y_tov, "otov_rapm", "dtov_rapm")
orb_df <- fit_factor_rapm(y_orb, "oorb_rapm", "dorb_rapm")
ftr_df <- fit_factor_rapm(y_ftr, "oftr_rapm", "dftr_rapm")

# ---- Combine all four factors ----
factor_rapm <- Reduce(
  function(a, b) left_join(a, b, by = "player"),
  list(efg_df, tov_df, orb_df, ftr_df)
) |>
  mutate(poss = poss_counts$N[match(player, poss_counts$player)]) |>
  arrange(desc(oefg_rapm))

print(factor_rapm, n = 30)

write.csv(factor_rapm, "datasets/4factor_rapm.csv", row.names = FALSE)