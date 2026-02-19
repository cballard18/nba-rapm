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

# Build player_id → canonical name map to resolve cross-season spelling inconsistencies.
.raw <- rbindlist(list(
  data.table(raw = unlist(pbp$lineup_home)),
  data.table(raw = unlist(pbp$lineup_away))
))[grepl("^\\d+\\s+\\S", raw)]
id_name_map <- .raw[,
  .(player_id = str_extract(raw, "^\\d+"), name = str_remove(raw, "^\\d+\\s+"))
][, .N, by = .(player_id, name)][order(-N)][, .(name = first(name)), by = player_id]
rm(.raw)

pbp[, `:=`(
  lineup_home = lapply(lineup_home, function(x) str_remove(x, "^\\d+\\s+")),
  lineup_away = lapply(lineup_away, function(x) str_remove(x, "^\\d+\\s+"))
)]

for (k in 1:5) {
  set(pbp, j = paste0("home_", k), value = vapply(pbp$lineup_home, `[`, character(1), k))
  set(pbp, j = paste0("away_", k), value = vapply(pbp$lineup_away, `[`, character(1), k))
}
pbp[, c("lineup_home", "lineup_away") := NULL]

normalize_name <- function(x) {
  x[x == "Nikola Jokic"] <- "Nikola Joki\u0107"
  x
}
for (col in c(paste0("home_", 1:5), paste0("away_", 1:5))) {
  set(pbp, j = col, value = normalize_name(pbp[[col]]))
}

# Apply normalize_name to the map so lineup names and resolved event names agree
id_name_map[, name := normalize_name(name)]

resolve_player <- function(x) {
  ids       <- str_extract(x, "^\\d+")
  canonical <- id_name_map$name[match(ids, id_name_map$player_id)]
  ifelse(!is.na(canonical), canonical, str_remove(x, "^\\d+\\s+"))
}

.hm <- as.matrix(pbp[, paste0("home_", 1:5), with = FALSE])
.am <- as.matrix(pbp[, paste0("away_", 1:5), with = FALSE])
pbp[, off_lineup_key := ifelse(
  team_abb == team_home,
  apply(.hm, 1, function(x) paste(sort(na.omit(x)), collapse = "|")),
  apply(.am, 1, function(x) paste(sort(na.omit(x)), collapse = "|"))
)]
rm(.hm, .am)

# ---- Compute player-level usage events per lineup-season ----
# Usage = FGA + 0.44*FTA + TOV (Dean Oliver formula)
# player1_name = shooter/turnover player; player_ft = free throw shooter
use_events <- rbindlist(list(
  pbp[msg_type %in% c(1L, 2L) & !is.na(player1_name),
      .(player = resolve_player(player1_name), lineup_key = off_lineup_key, use_wt = 1,    season)],
  pbp[msg_type == 3L           & !is.na(player_ft),
      .(player = resolve_player(player_ft),    lineup_key = off_lineup_key, use_wt = 0.44, season)],
  pbp[msg_type == 5L           & !is.na(player1_name),
      .(player = resolve_player(player1_name), lineup_key = off_lineup_key, use_wt = 1,    season)]
))[, .(use_events = sum(use_wt)), by = .(player, lineup_key, season)]

# ---- Build possession-level data ----
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

# Offensive lineup key: sorted names of the 5 players on offense for this possession
.lk_h <- apply(as.matrix(poss_df[, paste0("home_", 1:5), with = FALSE]), 1,
               function(x) paste(sort(na.omit(x)), collapse = "|"))
.lk_a <- apply(as.matrix(poss_df[, paste0("away_", 1:5), with = FALSE]), 1,
               function(x) paste(sort(na.omit(x)), collapse = "|"))
poss_df[, lineup_key := ifelse(home_on_off, .lk_h, .lk_a)]
rm(.lk_h, .lk_a)

poss_df[, poss_idx := .I]
n_poss <- nrow(poss_df)

# Recency weights
season_weights <- c("2023" = 0.20, "2024" = 0.30, "2025" = 0.50)
w <- season_weights[as.character(poss_df$season)]

# ---- Melt lineups into long format (include lineup_key and season for usage rate join) ----
home_long <- melt(poss_df, id.vars = c("poss_idx", "home_on_off", "season", "lineup_key"),
                  measure.vars = home_cols, value.name = "player")[!is.na(player)]
away_long <- melt(poss_df, id.vars = c("poss_idx", "home_on_off", "season", "lineup_key"),
                  measure.vars = away_cols, value.name = "player")[!is.na(player)]

all_players <- sort(unique(c(home_long$player, away_long$player)))
n_players   <- length(all_players)

home_long[, j_player := match(player, all_players)]
away_long[, j_player := match(player, all_players)]

poss_counts <- rbind(home_long[, .(player)], away_long[, .(player)])[, .N, by = player]

# ---- Offensive possessions per player-lineup-season ----
off_poss <- rbind(
  home_long[home_on_off == TRUE,  .(off_poss = .N), by = .(player, lineup_key, season)],
  away_long[home_on_off == FALSE, .(off_poss = .N), by = .(player, lineup_key, season)]
)[, .(off_poss = sum(off_poss)), by = .(player, lineup_key, season)]

# ---- Compute usage rate per player-lineup-season ----
usage_df <- merge(use_events, off_poss, by = c("player", "lineup_key", "season"), all = TRUE)
usage_df[is.na(use_events), use_events := 0]
usage_df[is.na(off_poss) | off_poss == 0L, off_poss := 1L]   # guard against /0
usage_df[, usage_rate := use_events / off_poss]

# ---- Join usage rates to lineup tables ----
home_long[usage_df, usage_rate := i.usage_rate, on = .(player, lineup_key, season)]
away_long[usage_df, usage_rate := i.usage_rate, on = .(player, lineup_key, season)]

# Players appearing in lineups but never touching the ball get rate 0 (ranked last)
home_long[is.na(usage_rate), usage_rate := 0]
away_long[is.na(usage_rate), usage_rate := 0]

# ---- Rank offensive players within each possession ----

home_off <- home_long[home_on_off == TRUE]
away_off <- away_long[home_on_off == FALSE]

home_off[, usage_rank := frank(-usage_rate, ties.method = "first"), by = poss_idx]
away_off[, usage_rank := frank(-usage_rate, ties.method = "first"), by = poss_idx]

all_off <- rbind(home_off, away_off)
all_off[, tier := fcase(usage_rank == 1L, 1L, usage_rank == 2L, 2L, default = 3L)]

# ---- Build expanded design matrix (4N columns) ----
# Cols 1..N      player_o1:  +1 when player is 1st option (highest usage) on offense
# Cols N+1..2N   player_o2:  +1 when player is 2nd option on offense
# Cols 2N+1..3N  player_o3:  +1 when player is 3rd+ option on offense
# Cols 3N+1..4N  player_d:   -1 when player is on defense (same regardless of tier)

od_long_usage <- rbind(
  all_off[, .(poss_idx, j = j_player + (tier - 1L) * n_players, x =  1L)],
  home_long[home_on_off == FALSE, .(poss_idx, j = j_player + 3L * n_players, x = -1L)],
  away_long[home_on_off == TRUE,  .(poss_idx, j = j_player + 3L * n_players, x = -1L)]
)

col_names_usage <- c(
  paste0(all_players, "_o1"),
  paste0(all_players, "_o2"),
  paste0(all_players, "_o3"),
  paste0(all_players, "_d")
)

X_usage <- sparseMatrix(
  i = od_long_usage$poss_idx,
  j = od_long_usage$j,
  x = as.numeric(od_long_usage$x),
  dims = c(n_poss, 4L * n_players),
  dimnames = list(NULL, col_names_usage)
)

y_pts <- fifelse(poss_df$home_on_off, poss_df$home_pts, poss_df$away_pts)

# ---- Standard 2N design matrix (for overall RAPM) ----
od_long <- rbind(
  home_long[, .(poss_idx, j = fifelse(home_on_off, j_player, j_player + n_players),
                x = fifelse(home_on_off, 1, -1))],
  away_long[, .(poss_idx, j = fifelse(home_on_off, j_player + n_players, j_player),
                x = fifelse(home_on_off, -1, 1))]
)

X_od <- sparseMatrix(
  i = od_long$poss_idx,
  j = od_long$j,
  x = as.numeric(od_long$x),
  dims = c(n_poss, 2L * n_players)
)

# ---- Ridge regression 1: overall RAPM ----
cat("Fitting overall RAPM (2N columns)...\n")
set.seed(42)
cv_overall <- cv.glmnet(
  x           = X_od,
  y           = y_pts,
  weights     = w,
  alpha       = 0,
  standardize = FALSE,
  nfolds      = 10
)

overall_coef <- as.vector(coef(cv_overall, s = "lambda.min"))[-1] * 100
orapm <- overall_coef[1:n_players]
drapm <- overall_coef[(n_players + 1L):(2L * n_players)]

# ---- Ridge regression 2: usage-tier RAPM (4N columns) ----
cat("Fitting usage-tier RAPM (4N columns)...\n")
set.seed(42)
cv_usage <- cv.glmnet(
  x           = X_usage,
  y           = y_pts,
  weights     = w,
  alpha       = 0,
  standardize = FALSE,
  nfolds      = 10
)

rapm_coef <- as.vector(coef(cv_usage, s = "lambda.min"))[-1] * 100

o1_rapm <- rapm_coef[1:n_players]
o2_rapm <- rapm_coef[(n_players + 1L):(2L * n_players)]
o3_rapm <- rapm_coef[(2L * n_players + 1L):(3L * n_players)]
d_rapm  <- rapm_coef[(3L * n_players + 1L):(4L * n_players)]

# ---- Combine and display ----
tier_counts <- dcast(
  all_off[, .N, by = .(player, tier)],
  player ~ tier, value.var = "N", fill = 0L
)
setnames(tier_counts, c("player", "1", "2", "3"), c("player", "o1_poss", "o2_poss", "o3_poss"))

usage_rapm <- tibble(
  player  = all_players,
  orapm   = orapm,
  drapm   = drapm,
  rapm    = orapm + drapm,
  o1_rapm = o1_rapm,
  o2_rapm = o2_rapm,
  o3_rapm = o3_rapm,
  d_rapm  = d_rapm,
  poss    = poss_counts$N[match(all_players, poss_counts$player)],
  o1_poss = tier_counts$o1_poss[match(all_players, tier_counts$player)],
  o2_poss = tier_counts$o2_poss[match(all_players, tier_counts$player)],
  o3_poss = tier_counts$o3_poss[match(all_players, tier_counts$player)]
) |>
  arrange(desc(rapm))

print(usage_rapm, n = 30)

write.csv(usage_rapm, "datasets/usage_rapm.csv", row.names = FALSE)