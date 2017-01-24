### functions for getting NBA data


# players -------------------------------------------------------------------------------------

get_player_data <- function(is_current_season = TRUE, season = create_season_string()) {

    # request player data from nba.com
    players_list <-
        httr::modify_url(
            url = nba_url,
            path = "stats/commonallplayers",
            query = list(
                IsOnlyCurrentSeason = as.integer(is_current_season),
                LeagueID = "00",
                Season = season
            )
        ) %>%
        jsonlite::fromJSON(simplifyVector = FALSE) %>%
        extract2("resultSets") %>%
        extract2(1)

    # names for the output dataframe
    player_cols <- players_list[["headers"]] %>% tolower

    # create the output dataframe
    map(players_list[["rowSet"]], ~ map(.x, create_nas)) %>% # add NAs when appropriate
        map(set_names, player_cols) %>% # name each list
        map_df(as_data_frame) %>% # convert each list to tibble
        filter(!stringr::str_detect(playercode, "HISTADD")) %>% # exclude historical additions
        # include more appropriate types for cols
        mutate(
            on_roster = as.logical(rosterstatus),
            from_year = as.integer(from_year),
            to_year = as.integer(to_year),
            team_id = na_if(team_id, 0),
            played_games = games_played_flag == "Y"
        ) %>%
        # remove unnecessary cols
        select(
            -rosterstatus,
            -games_played_flag,
            -team_city,
            -team_name,
            -team_abbreviation,
            -team_code
        ) %>%
        rename(player_id = person_id)

}


# teams ---------------------------------------------------------------------------------------

get_team_data <- function(team_id, season = create_season_string()) {

    # request team data from nba.com for team_id
    this_team_list <-
        httr::modify_url(
            url = nba_url,
            path = "stats/teaminfocommon",
            query = list(
                LeagueID = "00",
                Season = season,
                SeasonType = "Regular Season",
                TeamID = team_id
            )
        ) %>%
        jsonlite::fromJSON(simplifyVector = FALSE) %>%
        extract2("resultSets") %>%
        extract2(1)

    # names for the output dataframe
    team_cols <- this_team_list[["headers"]] %>% tolower

    # create the output dataframe
    this_team_list[["rowSet"]] %>%
        extract2(1) %>%
        set_names(team_cols) %>%
        as_data_frame

}


# team games ----------------------------------------------------------------------------------

get_team_game_data <- function(team_id, season = create_season_string()) {

    # request team game data from nba.com for team_id
    this_team_game_list <-
        httr::modify_url(
            url = nba_url,
            path = "stats/teamgamelog",
            query = list(
                LeagueID = "00",
                Season = season,
                SeasonType = "Regular Season",
                TeamID = team_id
            )
        ) %>%
        jsonlite::fromJSON(simplifyVector = FALSE) %>%
        extract2("resultSets") %>%
        extract2(1)

    # names for the output dataframe
    team_game_cols <- this_team_game_list[["headers"]] %>% tolower

    # create the output dataframe
    map(this_team_game_list[["rowSet"]], ~ map(.x, create_nas)) %>%
        map(set_names, team_game_cols) %>%
        map_df(as_data_frame)

}


# player shots --------------------------------------------------------------------------------

get_player_shot_data <- function(player_id, season = create_season_string()) {

    # request player shot data from nba.com for player_id
    this_player_shot_list <-
        httr::modify_url(
            url = nba_url,
            path = "stats/shotchartdetail",
            query = list(
                CFID = 33,
                CFPARAMS = season,
                ContextFilter = "",
                ContextMeasure = "FGA",
                DateFrom = "",
                DateTo = "",
                GameID = "",
                GameSegment = "",
                LastNGames = 0,
                LeagueID = "00",
                Location = "",
                MeasureType = "Base",
                Month = 0,
                OpponentTeamID = 0,
                Outcome = "",
                PaceAdjust = "N",
                PerMode = "PerGame",
                Period = 0,
                PlayerID = player_id,
                PlusMinus = "N",
                PlayerPosition = "",
                Rank = "",
                RookieYear = "",
                Season = season,
                SeasonSegment = "",
                SeasonType = "Regular Season",
                TeamID = 0,
                VsConference = "",
                VsDivision = "",
                mode = "Advanced",
                showDetails = 1,
                showShots = 1,
                showZones = 1
            )
        ) %>%
        jsonlite::fromJSON(simplifyVector = FALSE) %>%
        extract2("resultSets") %>%
        extract2(1)

    # names for the output dataframe
    player_shot_cols <- this_player_shot_list[["headers"]] %>% tolower

    # create the output dataframe
    map(this_player_shot_list[["rowSet"]], set_names, player_shot_cols) %>%
        map_df(as_data_frame)
}


# play by play --------------------------------------------------------------------------------

get_play_by_play_data <- function(game_season) {

    # request play by play data from nba.com
    this_play_by_play_list <-
        httr::modify_url(
            url = nba_url,
            path = "stats/playbyplay",
            query = list(
                EndPeriod = 10,
                EndRange = 55800,
                GameID = game_season[["game_id"]],
                RangeType = 2,
                Season = game_season[["season"]],
                SeasonType = "Regular Season",
                StartPeriod = 1,
                StartRange = 0
            )
        ) %>%
        jsonlite::fromJSON(simplifyVector = FALSE) %>%
        extract2("resultSets") %>%
        extract2(1)

    # names for the output dataframe
    play_by_play_cols <- this_play_by_play_list[["headers"]] %>% tolower

    # create the output dataframe
    map(this_play_by_play_list[["rowSet"]], ~ map(.x, create_nas)) %>%
        map(set_names, play_by_play_cols) %>%
        map_df(as_data_frame) %>%
        mutate(matchup = game_season[["matchup"]])
}
