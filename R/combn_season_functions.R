
# functions for generating <x>-season combinations --------------------------------------------

# game types available
game_types <- c("Pre Season", "Regular Season", "Playoffs")

# player - season strings for each season a player played
combn_season_player <- function(df = nba_players, pid = NULL, after_shot_data = TRUE) {

    player_tbl <- if (is_null(pid)) {
        df
    } else {
        df %>%
            filter(player_id == pid)
    }

    player_season_tbl <-
        player_tbl %>%
        group_by(player_id) %>%
        summarize(
            list(
                expand_grid(
                    player_id = player_id,
                    season = create_season_string(seq(from_year, to_year))
                ) %>%
                    mutate(shot_data_available = seq(from_year, to_year) > 1995)
            )
        ) %>%
        ungroup %>%
        extract2(2) %>%
        bind_rows

    if (after_shot_data) {
        player_season_tbl %>%
            filter(shot_data_available) %>%
            select(-shot_data_available)
    } else {
        player_season_tbl %>%
            select(-shot_data_available)
    }
}

# team - season strings for each team
combn_season_team <- function(df = nba_player_shots, tid = NULL) {

    if (is_null(tid)) {
        df %>%
            mutate(season = create_season_string(game_date)) %>%
            distinct(team_id, season) %>%
            arrange(season, team_id)

    } else {
        df %>%
            filter(team_id == tid) %>%
            mutate(season = create_season_string(game_date)) %>%
            distinct(team_id, season) %>%
            arrange(season, team_id)
    }

}

# game - season strings for each game
combn_season_game <- function(df = nba_team_games, tid = NULL) {

    if (is_null(tid)) {
        df %>%
            mutate(season = create_season_string(game_date)) %>%
            distinct(game_id, season, matchup)

    } else {
        df %>%
            filter(team_id == tid) %>%
            mutate(season = create_season_string(game_date)) %>%
            distinct(team_id, game_id, season, matchup)
    }

}
