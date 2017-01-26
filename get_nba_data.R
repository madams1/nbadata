require(dplyr)
require(purrr)
require(magrittr)
require(lubridate)

devtools::load_all(".")

system.time({


# players
nba_players <- get_player_data(FALSE)

# teams
nba_teams <-
    parallel::mclapply(
        unique(nba_players[["team_id"]]) %>% discard(is.na),
        get_team_data,
        mc.cores = parallel::detectCores()
    ) %>%
    bind_rows

# player shots
player_seasons <- combn_season_player()

nba_player_shots <-
    parallel::mcMap(
        get_player_shot_data %>% safely,
        player_seasons[["player_id"]],
        player_seasons[["season"]],
        mc.cores = parallel::detectCores()
    ) %>%
    discard_errors %>%
    bind_rows %>%
    mutate(
        game_date = ymd(game_date),
        shot_made = as.logical(shot_made_flag)
    ) %>%
    select(
        -grid_type,
        -event_type,
        -shot_attempted_flag,
        -shot_made_flag
    )

# team games
team_seasons <- combn_season_team()

nba_team_games <-
    parallel::mcMap(
        get_team_game_data %>% safely,
        team_seasons[["team_id"]],
        team_seasons[["season"]],
        mc.cores = parallel::detectCores()
    ) %>%
    discard_errors %>%
    bind_rows %>%
    mutate(game_date = mdy(game_date)) %>%
    arrange(team_id, game_date) %>%
    # add running win-loss data for seasons that don't include it
    mutate(season = create_season_string(game_date)) %>%
    group_by(season) %>%
    mutate(
        w = ifelse(is.na(w), cumsum(wl == "W"), w),
        l = ifelse(is.na(l), cumsum(wl == "L"), l),
        w_pct = ifelse(is.na(w_pct), round(w / (w + l), 3), w_pct)
    ) %>%
    ungroup %>%
    select(-season)

# play by play
game_seasons <- combn_season_game()

nba_play_by_play <-
    parallel::mclapply(
        game_seasons %>% transpose,
        get_play_by_play_data %>% safely,
        mc.cores = parallel::detectCores()
    ) %>%
    discard_errors %>%
    bind_rows %>%
    mutate(
        home_factor = ifelse(stringr::str_detect(matchup, "vs"), 1, -1),
        elapsed_seconds = calc_elapsed_seconds(period, pctimestring),
        score = ifelse(elapsed_seconds == 0, "0 - 0", score),
        scoremargin = ifelse(
            elapsed_seconds == 0 | (!is.na(score) & is.na(scoremargin)),
            0,
            strtoi(scoremargin) * home_factor
        )
    ) %>%
    select(-home_factor) %>%
    tidyr::fill(score, scoremargin)


# save data
devtools::use_data(
    nba_players,
    nba_teams,
    nba_player_shots,
    nba_team_games,
    nba_play_by_play,
    compress = "bzip2",
    overwrite = TRUE
)

})

# put in zipfile and cleanup
zip("nbadata", list.files("data", full.names = TRUE))
unlink("data", recursive = TRUE)

# bump version
read.dcf("DESCRIPTION") %>%
    as_data_frame %>%
    mutate(Version = format(as.numeric(Version) + 1, nsmall = 1)) %>%
    write.dcf("DESCRIPTION")

# commit changes to DESCRIPTION
system("git add DESCRIPTION && git commit -m 'bump version' && git push")

# release new version of data on GH
nbadata::mydata_release("refresh data", filename = "nbadata.zip", yes = TRUE)
