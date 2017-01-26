
# helper functions for getting/cleaning NBA data ----------------------------------------------

# create season based on current month
create_season_string <- function(x = NULL) {
    if (is.Date(x)) {
        start_year <- ifelse(
            month(x) < 10,
            year(x) - 1,
            year(x)
        )

        season_string <- paste(
            start_year,
            stringr::str_sub(start_year + 1, -2),
            sep = "-"
        )
    } else if (is_numeric(x)) {
        season_string <- paste(
            x,
            stringr::str_sub(x + 1, -2),
            sep = "-"
        )
    } else {
        start_year <- ifelse(
            month(now()) < 10,
            year(now()) - 1,
            year(now())
        )

        season_string <- paste(
            start_year,
            stringr::str_sub(start_year + 1, -2),
            sep = "-"
        )

    }
    season_string
}

# for simplifying combinations in a tidy way
expand_grid <- function(...) {
    expand.grid(..., stringsAsFactors = FALSE) %>% as_data_frame
}

# for discarding errors and keeping results in a chain
discard_errors <- function(x) {
    discard(x, ~ !is_empty(.x[["error"]])) %>%
        map("result")
}

# helper function for cleaning up nulls and blanks
create_nas <- function(x) ifelse(is_null(x), NA, x) %>% dplyr::na_if("")

# for calculating elapsed time in play-by-play data
calc_elapsed_seconds <- function(per, timestring) {
    previous_elapsed <- ifelse(
        per <= 5,
        (per - 1) * 720,
        3180 + (per - 5) * 300
    )
    period_start <- ifelse(per <= 4, 720, 300)
    seconds_in_period <- ms(timestring) %>%
        as.duration %>%
        divide_by(duration(1))
    elapsed_in_period <- period_start - seconds_in_period
    previous_elapsed + elapsed_in_period
}

# helper for loading data from zipfile
nbadata_load_all <- function(path = "./") {
    create_filename <- function(fn) {
        paste0(path, fn, ".rda")
    }

    load(create_filename("nba_teams")) # teams
    load(create_filename("nba_players")) # players
    load(create_filename("nba_team_games")) # team games
    load(create_filename("nba_player_shots")) # player shots
    load(create_filename("nba_play_by_play")) # play by play
}
