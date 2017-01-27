# nbadata

This package provides access to a collection of data from the National Basketball Association (http://stats.nba.com) for the seasons 1996-97 to 2016-17. Details on the current release are available [here](https://github.com/madams1/nbadata/releases).

It includes data for:

- `nba_teams`
- `nba_players`
- `nba_team_games`
- `nba_player_shots`
- `nba_play_by_play`

### Install
```r
devtools::install_github("madams1/nbadata")
devtools::install_github("ropenscilabs/datastorr")
```

### Get and load the data
```r
nbadata::nbadata() # downloads data and installs .rda files in working directory
nbadata::nbadata_load_all()
```
