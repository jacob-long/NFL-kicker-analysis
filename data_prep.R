library(readr)
library(dplyr)
library(lubridate)
library(lme4)
library(jtools)
library(rvest)
library(stringr)
library(tidyr)
library(magrittr)
library(brms)
library(ggplot2)
library(purrr)

# Only use this code if you need to add the most recent games
# You will have to edit the `weeks =` argument to suit your needs
# devtools::install_github(repo = "ryurko/nflscrapR")
# In case last week or two isn't up to date
# library(nflscrapR)
# recent_games <- scrape_game_ids(2019, weeks = 13:14) %>%
#   filter(state_of_game == "POST") %>% 
#   pull(game_id) %>%
#   map_dfr(., scrape_game_play_by_play, type = "reg", season = 2019)
# write.csv(recent_games, "recent_games.csv")
# recents <- read_csv("recent_games.csv")
# pbp19 <- read_csv("reg_pbp_2019.csv")
# pbp19 <- bind_rows(pbp19, recents)
# write.csv(pbp19, "reg_pbp_2019_augmented.csv")

## Read in each play by play data file #######################################
pbp19 <- read_csv("reg_pbp_2019_augmented.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = FALSE) %>%
  # Keep only field goal attempts
  filter(play_type == "field_goal")

pbp18 <- read_csv("reg_pbp_2018.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = FALSE) %>%
  filter(play_type == "field_goal")

pbp17 <- read_csv("reg_pbp_2017.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = FALSE) %>%
  filter(play_type == "field_goal")

pbp16 <- read_csv("reg_pbp_2016.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = FALSE) %>%
  filter(play_type == "field_goal")

pbp15 <- read_csv("reg_pbp_2015.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = FALSE) %>%
  # Drop these wonkey blocked player ID columns that break row binding later
  select(-blocked_player_id) %>%
  filter(play_type == "field_goal")

pbp14 <- read_csv("reg_pbp_2014.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = FALSE) %>%
  filter(play_type == "field_goal")

pbp13 <- read_csv("reg_pbp_2013.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = FALSE) %>%
  filter(play_type == "field_goal")

pbp12 <- read_csv("reg_pbp_2012.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = FALSE) %>%
  filter(play_type == "field_goal")

pbp11 <- read_csv("reg_pbp_2011.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = FALSE) %>%
  select(-blocked_player_id) %>%
  filter(play_type == "field_goal")

pbp10 <- read_csv("reg_pbp_2010.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = FALSE) %>%
  filter(play_type == "field_goal")

pbp09 <- read_csv("reg_pbp_2009.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = FALSE) %>%
  filter(play_type == "field_goal")

pbp18p <- read_csv("post_pbp_2018.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = TRUE) %>%
  filter(play_type == "field_goal")

pbp17p <- read_csv("post_pbp_2017.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = TRUE) %>%
  filter(play_type == "field_goal")

pbp16p <- read_csv("post_pbp_2016.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = TRUE) %>%
  filter(play_type == "field_goal")

pbp15p <- read_csv("post_pbp_2015.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = TRUE) %>%
  # Drop these wonkey blocked player ID columns that break row binding later
  select(-blocked_player_id) %>%
  filter(play_type == "field_goal")

pbp14p <- read_csv("post_pbp_2014.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = TRUE) %>%
  filter(play_type == "field_goal")

pbp13p <- read_csv("post_pbp_2013.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = TRUE) %>%
  filter(play_type == "field_goal")

pbp12p <- read_csv("post_pbp_2012.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = TRUE) %>%
  filter(play_type == "field_goal")

pbp11p <- read_csv("post_pbp_2011.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = TRUE) %>%
  select(-blocked_player_id) %>%
  filter(play_type == "field_goal")

pbp10p <- read_csv("post_pbp_2010.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = TRUE) %>%
  filter(play_type == "field_goal")

pbp09p <- read_csv("post_pbp_2009.csv") %>%
  mutate(iced = !is.na(lag(timeout_team)) & lag(timeout_team) != posteam,
         playoffs = TRUE) %>%
  filter(play_type == "field_goal")

# Bind them together into single data frame
pbp <- bind_rows(pbp19, pbp18, pbp17, pbp16, pbp15, pbp14, pbp13, pbp12,
                 pbp11, pbp10, pbp09, pbp18p, pbp17p, pbp16p, pbp15p, pbp14p,
                 pbp13p, pbp12p, pbp11p, pbp10p, pbp09p) %>%
  mutate(game_id = as.character(game_id)) %>%
  # take out pro bowl
  filter(home_team %nin% c("AFC", "NFC", "APR", "NPR", "SAN", "RIC", "CRT",
                           "IRV"))

# save some memory
rm(pbp19, pbp18, pbp17, pbp16, pbp15, pbp14, pbp13, pbp12, pbp11, pbp10, pbp09,
   pbp18p, pbp17p, pbp16p, pbp15p, pbp14p, pbp13p, pbp12p, pbp11p, pbp10p,
   pbp09p)

# Create games data frame to get game metadata
games <- pbp[!duplicated(pbp$game_id), ]

# Generate variables to build pro-football-reference and NFLWeather URLs
games %<>%
  mutate(pfr_team = tolower(home_team),
         pfr_team = case_when(
           pfr_team == "gb" ~ "gnb",
           pfr_team == "ten" ~ "oti",
           pfr_team == "ari" ~ "crd",
           pfr_team == "la" ~ "ram",
           pfr_team == "lac" ~ "sdg",
           pfr_team == "kc" ~ "kan",
           pfr_team == "sf" ~ "sfo",
           pfr_team == "bal" ~ "rav",
           pfr_team == "oak" ~ "rai",
           pfr_team == "no" ~ "nor",
           pfr_team == "ne" ~ "nwe",
           pfr_team == "ind" ~ "clt",
           pfr_team == "tb" ~ "tam",
           pfr_team == "hou" ~ "htx",
           # fixing old format of jacksonville
           pfr_team == "jac" ~ "jax",
           # fixing old san diego
           pfr_team == "sd" ~ "sdg",
           # fixing old Rams
           pfr_team == "stl" ~ "ram",
           TRUE ~ pfr_team
         ),
         pfr_away_team = tolower(away_team),
         pfr_away_team = case_when(
           pfr_away_team == "gb" ~ "gnb",
           pfr_away_team == "ten" ~ "oti",
           pfr_away_team == "ari" ~ "crd",
           pfr_away_team == "la" ~ "ram",
           pfr_away_team == "lac" ~ "sdg",
           pfr_away_team == "kc" ~ "kan",
           pfr_away_team == "sf" ~ "sfo",
           pfr_away_team == "bal" ~ "rav",
           pfr_away_team == "oak" ~ "rai",
           pfr_away_team == "no" ~ "nor",
           pfr_away_team == "ne" ~ "nwe",
           pfr_away_team == "ind" ~ "clt",
           pfr_away_team == "tb" ~ "tam",
           pfr_away_team == "hou" ~ "htx",
           # fixing old format of jacksonville
           pfr_away_team == "jac" ~ "jax",
           # fixing old san diego
           pfr_away_team == "sd" ~ "sdg",
           # fixing old Rams
           pfr_away_team == "stl" ~ "ram",
           TRUE ~ pfr_away_team
         ),
         w_home = case_when(
           pfr_team == "chi" ~ "bears",
           pfr_team == "atl" ~ "falcons",
           pfr_team == "buf" ~ "bills",
           pfr_team == "car" ~ "panthers",
           pfr_team == "cin" ~ "bengals",
           pfr_team == "cle" ~ "browns",
           pfr_team == "clt" ~ "colts",
           pfr_team == "crd" ~ "cardinals",
           pfr_team == "dal" ~ "cowboys",
           pfr_team == "den" ~ "broncos",
           pfr_team == "det" ~ "lions",
           pfr_team == "gnb" ~ "packers",
           pfr_team == "htx" ~ "texans",
           pfr_team == "jax" ~ "jaguars",
           pfr_team == "kan" ~ "chiefs",
           pfr_team == "mia" ~ "dolphins",
           pfr_team == "min" ~ "vikings",
           pfr_team == "nor" ~ "saints",
           pfr_team == "nwe" ~ "patriots",
           pfr_team == "nyg" ~ "giants",
           pfr_team == "nyj" ~ "jets",
           pfr_team == "oti" ~ "titans",
           pfr_team == "phi" ~ "eagles",
           pfr_team == "pit" ~ "steelers",
           pfr_team == "rai" ~ "raiders",
           pfr_team == "ram" ~ "rams",
           pfr_team == "rav" ~ "ravens",
           pfr_team == "sdg" ~ "chargers",
           pfr_team == "sea" ~ "seahawks",
           pfr_team == "sfo" ~ "49ers",
           pfr_team == "tam" ~ "buccaneers",
           pfr_team == "was" ~ "redskins"
         ),
         w_away = case_when(
           pfr_away_team == "chi" ~ "bears",
           pfr_away_team == "atl" ~ "falcons",
           pfr_away_team == "buf" ~ "bills",
           pfr_away_team == "car" ~ "panthers",
           pfr_away_team == "cin" ~ "bengals",
           pfr_away_team == "cle" ~ "browns",
           pfr_away_team == "clt" ~ "colts",
           pfr_away_team == "crd" ~ "cardinals",
           pfr_away_team == "dal" ~ "cowboys",
           pfr_away_team == "den" ~ "broncos",
           pfr_away_team == "det" ~ "lions",
           pfr_away_team == "gnb" ~ "packers",
           pfr_away_team == "htx" ~ "texans",
           pfr_away_team == "jax" ~ "jaguars",
           pfr_away_team == "kan" ~ "chiefs",
           pfr_away_team == "mia" ~ "dolphins",
           pfr_away_team == "min" ~ "vikings",
           pfr_away_team == "nor" ~ "saints",
           pfr_away_team == "nwe" ~ "patriots",
           pfr_away_team == "nyg" ~ "giants",
           pfr_away_team == "nyj" ~ "jets",
           pfr_away_team == "oti" ~ "titans",
           pfr_away_team == "phi" ~ "eagles",
           pfr_away_team == "pit" ~ "steelers",
           pfr_away_team == "rai" ~ "raiders",
           pfr_away_team == "ram" ~ "rams",
           pfr_away_team == "rav" ~ "ravens",
           pfr_away_team == "sdg" ~ "chargers",
           pfr_away_team == "sea" ~ "seahawks",
           pfr_away_team == "sfo" ~ "49ers",
           pfr_away_team == "tam" ~ "buccaneers",
           pfr_away_team == "was" ~ "redskins"
         ),
         year = as.character(year(game_date)),
         month = as.character(month(game_date)),
         day = as.character(day(game_date)),
         month = case_when(
           nchar(month) == 1 ~ paste0("0", month),
           TRUE ~ month
         ),
         # Fix weird case in which Buffalo game was postponed 1 day
         day = case_when(
           pfr_team == "buf" & month == "11" & year == "2014" & day == "23" ~
             "24", 
           TRUE ~ day
         ),
         day = case_when(
           nchar(day) == 1 ~ paste0("0", day),
           TRUE ~ day
         ),
         pfr_url = paste0(
           "https://www.pro-football-reference.com/boxscores/",
           year, month, day, "0", pfr_team, ".htm"
         ),
         # kludge to fix one broken super bowl link
         pfr_url = case_when(
           pfr_url == "https://www.pro-football-reference.com/boxscores/201102060gnb.htm" ~
             "https://www.pro-football-reference.com/boxscores/201102060pit.htm",
           TRUE ~ pfr_url
         )
  ) %>%
  select(game_id, pfr_team, year, month, day, pfr_url, w_away, w_home, playoffs)

# Helper function to scrape data from pro-football-reference
get_game_meta <- function(url) {
  page <- try(read_html(url))
  if (inherits(page, "try-error")) {
    return(list(degrees = NaN, wind = NaN, humidity = NaN, roof = NA_character_,
                time = NA_character_, stadium = NA_character_, pfr_url = url,
                week = NaN))
  }
  week <- as.numeric(str_extract(as.character(page), "(?<=week_)\\d+"))
  game_info_tab <- 
    xml2::read_html(stringr::str_extract(
      str_replace_all(as.character(page), "\\n", ""),
      '<table class="suppress_all sortable stats_table" id="game_info".*</b></td></tr></table>      </div>   </div>-->|<table class="suppress_all sortable stats_table" id="game_info".*Super Bowl MVP</th>.*</tr></table>      </div>   </div>-->')) %>%
    html_table() %>%
    .[[1]]
  weather <- game_info_tab[game_info_tab$X1 == "Weather", 2]
  if (length(weather) > 0) {
    degrees <- as.numeric(str_extract(weather, "\\d+(?= degrees)"))
    wind <- as.numeric(str_extract(weather, "(?<=wind )\\d+(?= mph)"))
    humidity <- as.numeric(str_extract(weather, "(?<=humidity )\\d+"))
  } else {
    degrees <- wind <- humidity <- NaN
  }
  roof <- game_info_tab[game_info_tab$X1 == "Roof", 2]
  scorebox <- html_node(page, "div.scorebox_meta") %>% html_text()
  time <- str_extract(scorebox, "(?<=Start Time\\: )\\d+\\:\\d\\d(am|pm)")
  stadium <- trimws(str_extract(scorebox, "(?<=Stadium\\: ).*(?=Attendance)"))
  return(list(degrees = degrees, wind = wind, humidity = humidity, roof = roof,
              time = time, stadium = stadium, pfr_url = url, week = week))
}

## Don't need to scrape everything if the game_metas.RDS file exists
# game_metas <- purrr::map_dfr(games$pfr_url, get_game_meta)
# saveRDS(game_metas, "game_metas.RDS")

game_metas <- readRDS("game_metas.RDS")

## This code can be used to append new games to existing data object rather
## than rescraping everything
# game_metas_new <- purrr::map_dfr(games$pfr_url %not% game_metas$pfr_url, 
#   get_game_meta)
# game_metas <- bind_rows(game_metas, game_metas_new)
## For combining new scrapes with old
# game_metas_new <- purrr::map_dfr(games$pfr_url %not% game_metas$pfr_url, 
#   get_game_meta)
# game_metas <- bind_rows(game_metas, game_metas_new)
# saveRDS(game_metas, "game_metas.RDS")

# Add other game information to game_metas object
game_metas <- inner_join(game_metas, games, by = "pfr_url")

# Build up NFLWeather URLs
game_metas %<>%
  mutate(
    season = case_when(
      as.numeric(month) > 6 ~ as.numeric(year),
      as.numeric(month) <= 6 ~ as.numeric(year) - 1
    ),
    nflw_week = as.character(week),
    nflw_week = case_when(
      week == 18 ~ "wildcard-weekend",
      week == 19 ~ "divisional-playoffs",
      week == 20 ~ "conf-championships",
      week == 21 ~ "superbowl",
      TRUE ~ paste0("week-", nflw_week)
    ),
    nflw_week = case_when(
      season == 2010 ~ paste0(nflw_week, "-2"),
      TRUE ~ nflw_week
    ),
    w_url = paste0("http://nflweather.com/en/game/", season, "/", 
                   nflw_week, "/", w_away, "-at-", w_home),
    w_url = case_when(
      # kludge for 2010 super bowl
      w_url == "http://nflweather.com/en/game/2010/superbowl-2/steelers-at-packers" ~
        "http://nflweather.com/en/game/2010/superbowl-2/packers-at-steelers",
      # kludge for 2009 super bowl
      w_url == "http://nflweather.com/en/game/2009/superbowl/saints-at-colts" ~
        "http://nflweather.com/en/game/2009/superbowl/colts-at-saints",
      # kludges for NFLWeather typos
      w_url == "http://nflweather.com/en/game/2012/conf-championships/ravens-at-patriots" ~
        "http://nflweather.com/en/game/2012/conf-championships/ravens-at-patiots",
      w_url == "http://nflweather.com/en/game/2013/wildcard-weekend/chiefs-at-colts" ~
        "http://nflweather.com/en/game/2013/wildcard-weekend/cheifs-at-colts",
      
      TRUE ~ w_url
    )
  ) %>%
  # Trying to deal with cardinal direction of stadiums
  mutate(
    stadium_direction = NA_character_,
    stadium_direction = case_when(
      stadium == "Soldier Field" ~ "N",
      str_detect(stadium, "Mile High") ~ "N",
      str_detect(stadium, "Coliseum") ~ "SSE",
      stadium == "Bank of America Stadium" ~ "SE",
      stadium == "Arrowhead Stadium" ~ "SE",
      stadium %in% c("University of Phoenix Stadium", "State Farm Stadium") ~ "SE",
      stadium == "Lincoln Financial Field" ~ "N",
      stadium %in% c("AT&T Stadium", "Cowboys Stadium") ~ "E",
      stadium == "Georgia Dome" ~ "E",
      stadium == "Lambeau Field" ~ "N",
      stadium %in% c("Qwest Field", "CenturyLink Field") ~ "N",
      stadium %in% c("Jacksonville Municipal Stadium", "EverBank Field",
                     "TIAA Bank Stadium") ~ "SSE",
      stadium %in% c("Ralph Wilson Stadium", "New Era Field") ~ "SE",
      stadium == "Lucas Oil Stadium" ~ "NE",
      stadium == "FedExField" ~ "SE",
      stadium == "Paul Brown Stadium" ~ "SE",
      stadium %in% c("NRG Stadium", "Reliant Stadium") ~ "N",
      stadium == "Azteca Stadium" ~ "N",
      stadium == "Candlestick Park" ~ "N",
      stadium == "Levi's Stadium" ~ "SSE",
      stadium %in% c("Cleveland Browns Stadium", "FirstEnergy Stadium") ~ "NE",
      stadium %in% c("Dolphin Stadium", "Hard Rock Stadium",
                     "Sun Life Stadium") ~ "N",
      stadium %in% c("MetLife Stadium", "New Meadowlands Stadium") ~ "N",
      stadium == "Giants Stadium" ~ "SE",
      stadium == "Gillette Stadium" ~ "N",
      stadium == "Heinz Field" ~ "N",
      stadium %in% c("LP Field", "Nissan Stadium") ~ "N",
      stadium == "M&T Bank Stadium" ~ "E",
      stadium == "Qualcomm Stadium" ~ "E",
      stadium == "Raymond James Stadium" ~ "N",
      stadium == "StubHub Center" ~ "N",
      stadium == "TCF Bank Stadium" ~ "E",
      stadium == "Tottenham Stadium" ~ "N",
      stadium == "Twickenham Stadium" ~ "SE",
      stadium == "Wembley Stadium" ~ "E",
      TRUE ~ stadium_direction
    ),
    # Coding multiple stadium names into one
    stadium = case_when(
      str_detect(stadium, "Mile High") ~ "Empower Field at Mile High",
      
      str_detect(stadium, "Coliseum") & !str_detect(stadium, "Los Angeles") ~
        "Ring Central Coliseum",
      stadium %in% c("University of Phoenix Stadium", "State Farm Stadium") ~ 
        "State Farm Stadium",
      stadium %in% c("AT&T Stadium", "Cowboys Stadium") ~
        "AT&T Stadium",
      stadium %in% c("Qwest Field", "CenturyLink Field") ~
        "CenturyLink Field",
      stadium %in% c("Jacksonville Municipal Stadium", "EverBank Field",
                     "TIAA Bank Stadium") ~ 
        "TIAA Bank Stadium",
      stadium %in% c("Ralph Wilson Stadium", "New Era Field") ~
        "New Era Field",
      stadium %in% c("NRG Stadium", "Reliant Stadium") ~ 
        "NRG Stadium",
      stadium %in% c("Cleveland Browns Stadium", "FirstEnergy Stadium") ~
        "FirstEnergy Stadium",
      stadium %in% c("Dolphin Stadium", "Hard Rock Stadium",
                     "Sun Life Stadium") ~
        "Hard Rock Stadium",
      stadium %in% c("MetLife Stadium", "New Meadowlands Stadium") ~ 
        "MetLife Stadium",
      stadium %in% c("LP Field", "Nissan Stadium") ~
        "Nissan Stadium",
      TRUE ~ stadium
    )
  )

# Helper function to scrape weather data from NFLWeather.com
get_weather <- function(url) {
  page <- try(read_html(url))
  if (inherits(page, "try-error")) {
    return(data.frame(forecast = NA_character_, temp = NaN, wind_speed = NaN,
                      wind_dir = NA_character_, precip = NaN, q = NaN, 
                      w_url = url))
  }
  # Note that the data comes quarter by quarter
  q1 <- page %>% html_nodes("div.span3") %>% .[[1]] %>% html_text()
  q1l <- list(
    forecast = trimws(str_extract(
      q1, "(\\s.*)(?=\\n\\s*\\n\\s*Temperature)"
    )),
    temp = as.numeric(str_extract(q1, "(?<=Temperature: )\\d+(?=f)")),
    wind_speed = as.numeric(str_extract(q1, "(?<=Wind: )\\d+(?=mi)")),
    wind_dir = str_extract(q1, "(?<=\\dmi ).*"),
    precip = as.numeric(str_extract(q1, "(?<=Prob: )\\d+(?=%)")),
    q = 1, w_url = url)
  q2 <- page %>% html_nodes("div.span3") %>% .[[2]] %>% html_text()
  q2l <- list(
    forecast = trimws(str_extract(
      q2, "(\\s.*)(?=\\n\\s*\\n\\s*Temperature)"
    )),
    temp = as.numeric(str_extract(q2, "(?<=Temperature: )\\d+(?=f)")),
    wind_speed = as.numeric(str_extract(q2, "(?<=Wind: )\\d+(?=mi)")),
    wind_dir = str_extract(q2, "(?<=\\dmi ).*"),
    precip = as.numeric(str_extract(q2, "(?<=Prob: )\\d+(?=%)")),
    q = 2, w_url = url)
  q3 <- page %>% html_nodes("div.span3") %>% .[[3]] %>% html_text()
  q3l <- list(
    forecast = trimws(str_extract(
      q3, "(\\s.*)(?=\\n\\s*\\n\\s*Temperature)"
    )),
    temp = as.numeric(str_extract(q3, "(?<=Temperature: )\\d+(?=f)")),
    wind_speed = as.numeric(str_extract(q3, "(?<=Wind: )\\d+(?=mi)")),
    wind_dir = str_extract(q3, "(?<=\\dmi ).*"),
    precip = as.numeric(str_extract(q3, "(?<=Prob: )\\d+(?=%)")),
    q = 3, w_url = url)
  q4 <- page %>% html_nodes("div.span3") %>% .[[4]] %>% html_text()
  q4l <- list(
    forecast = trimws(str_extract(
      q4, "(\\s.*)(?=\\n\\s*\\n\\s*Temperature)"
    )),
    temp = as.numeric(str_extract(q4, "(?<=Temperature: )\\d+(?=f)")),
    wind_speed = as.numeric(str_extract(q4, "(?<=Wind: )\\d+(?=mi)")),
    wind_dir = str_extract(q4, "(?<=\\dmi ).*"),
    precip = as.numeric(str_extract(q4, "(?<=Prob: )\\d+(?=%)")),
    q = 4, w_url = url)
  bind_rows(q1l, q2l, q3l, q4l)
}

## Don't need to scrape everything if data are already saved
# game_weathers <- purrr::map_dfr(game_metas$w_url, get_weather)
# saveRDS(game_weathers, "game_weathers.RDS")

game_weathers <- readRDS("game_weathers.RDS")

## Use this to append new data rather than re-scrape everything
# game_weathers_new <- 
  # purrr::map_dfr(game_metas$w_url %not% game_weathers$w_url, get_weather)
# game_weathers <- bind_rows(game_weathers, game_weathers_new)
# saveRDS(game_weathers, "game_weathers.RDS")

# Do some tidying and dealing with missing/incomplete data
game_weathers %<>%
  group_by(w_url) %>%
  mutate(
    temp = case_when(
      is.na(temp) & q != 1 ~ mean(temp, na.rm = TRUE),
      TRUE ~ temp
    ),
    wind_speed = case_when(
      is.na(wind_speed) & q != 1 ~ mean(wind_speed, na.rm = TRUE),
      TRUE ~ wind_speed
    ),
    wind_dir = case_when(
      is.na(wind_dir) & q == 2 ~ lag(wind_dir),
      is.na(wind_dir) & q == 3 ~ lag(wind_dir, 2),
      is.na(wind_dir) & q == 4 ~ lag(wind_dir, 3),
      TRUE ~ wind_dir
    ),
    precip = case_when(
      is.na(precip) & q != 1 ~ mean(precip, na.rm = TRUE),
      TRUE ~ precip
    ),
    forecast = case_when(
      is.na(forecast) & q == 2 ~ lag(forecast),
      is.na(forecast) & q == 3 ~ lag(forecast, 2),
      is.na(forecast) & q == 4 ~ lag(forecast, 3),
      TRUE ~ forecast
    ),
    q = case_when(
      is.nan(q) ~ NA_character_,
      TRUE ~ as.character(q)
    )
  )

# Create an empty row with a quarter "5" to deal with OT
game_weathers[nrow(game_weathers) + 1,] <-
  list(NA, NA, NA, NA, NA, 5, game_weathers$w_url[nrow(game_weathers)])
# Convert quarter to factor with 5 levels
game_weathers$q <- factor(game_weathers$q)
# Use tidyr function to create a quarter 5 for every game so there will
# be weather data for OT kicks
game_weathers %<>% complete(w_url, q)

# Use 4th quarter weather for overtime
game_weathers %<>%
  mutate(
    temp = case_when(
      q == "5" ~ lag(temp),
      TRUE ~ temp
    ),
    wind_speed = case_when(
      q == "5" ~ lag(wind_speed),
      TRUE ~ wind_speed
    ),
    wind_dir = case_when(
      q == "5" ~ lag(wind_dir),
      TRUE ~ wind_dir
    ), 
    precip = case_when(
      q == "5" ~ lag(precip),
      TRUE ~ precip
    ),
    forecast = case_when(
      q == "5" ~ lag(forecast),
      TRUE ~ forecast
    )
  )

# Add this new weather data to game metadata, which will now be 
# quarter by quarter
game_metas <- inner_join(game_metas, game_weathers, by = "w_url")

# Code indoor kicks as 70 degrees and no wind/snow/etc.
game_metas %<>% 
  mutate(
    degrees = case_when(
      roof != "outdoors" & is.na(degrees) ~ 70,
      roof == "dome" ~ 70,
      TRUE ~ degrees
    ),
    temp = case_when(
      roof != "outdoors" & is.na(temp) ~ 70,
      roof == "dome" ~ 70,
      TRUE ~ temp
    ),
    wind = case_when(
      roof != "outdoors" & is.na(wind) ~ 0,
      roof == "dome" ~ 0,
      TRUE ~ wind
    ),
    wind_speed = case_when(
      roof != "outdoors" & is.na(wind_speed) ~ 0,
      roof == "dome" ~ 0,
      TRUE ~ wind_speed
    ),
    precip = case_when(
      roof != "outdoors" & is.na(precip) ~ 0,
      roof == "dome" ~ 0,
      TRUE ~ precip
    ),
    start_time = hour(as.POSIXct(time, format = "%I:%M%p")),
    month = factor(month),
    rain = str_detect(tolower(forecast), "rain|drizzle|mist|shower|thunder|mix") &
      !str_detect(tolower(forecast), "slight|possible|chance"),
    snow = str_detect(tolower(forecast), "snow|blizzard|flurries|mix") &
      !str_detect(tolower(forecast), "slight|possible|chance"),
    fog = str_detect(tolower(forecast), "fog|haze"),
  ) %>%
  rowwise() %>%
  ## Now I try to deal with wind direction, ultimately not used
  mutate(
    wind_dir = paste(unlist(str_extract_all(wind_dir, "(N|S|E|W)")), collapse = ""),
  ) %>%
  mutate(
    cross_wind = case_when(
      stadium_direction == "N" & 
        wind_dir %in% c("E", "W", "ENE", "ESE", "WSW", "WNW") ~ wind_speed,
      stadium_direction == "E" &
        wind_dir %in% c("N", "S", "NNE", "SSE", "NNW", "SSW") ~ wind_speed,
      stadium_direction == "NE" & 
        wind_dir %in% c("SE", "WSW", "ESE", "NW") ~ wind_speed,
      stadium_direction == "SE" & 
        wind_dir %in% c("NE", "ENE", "WSW", "SW") ~ wind_speed,
      stadium_direction == "SSE" & 
        wind_dir %in% c("E", "W", "ENE", "WNW") ~ wind_speed,
      TRUE ~ 0
    ),
    p_cross_wind = case_when(
      stadium_direction == "N" & 
        wind_dir %nin% c("E", "W", "ENE", "ESE", "WSW", "WNW", "N", "S") ~
        wind_speed,
      stadium_direction == "E" &
        wind_dir %nin% c("N", "S", "NNE", "SSE", "NNW", "SSW", "E", "W") ~
        wind_speed,
      stadium_direction == "NE" & 
        wind_dir %nin% c("SE", "WSW", "ESE", "NW", "NE", "SW") ~ wind_speed,
      stadium_direction == "SE" & 
        wind_dir %in% c("NE", "ENE", "WSW", "SW", "SE", "NW") ~ wind_speed,
      stadium_direction == "SSE" & 
        wind_dir %in% c("E", "W", "ENE", "WNW", "SSE", "NNW") ~ wind_speed,
      TRUE ~ 0
    ),
  )

# Now add all this game-level information to the individual kicks
pbp <- inner_join(pbp, game_metas, by = c("game_id", "qtr" = "q"))

# Drop rows with missing data --- needed for poly() to work
pbp_f <- filter(pbp, !is.na(kick_distance), !is.na(temp), !is.na(wind), 
                !is.na(rain))

# Create some new variables (not all used)
pbp_f %<>%
  mutate(
    # Converting to factor makes prediction with GAMs work
    kicker_player_id = factor(kicker_player_id),
    stadium = factor(stadium),
    qtr = factor(qtr),
    score_diff = abs(score_differential),
    all_cross_wind = cross_wind + p_cross_wind,
    kicking_team = posteam_type,
    dome = roof == "dome",
    minutes_remaining = game_seconds_remaining / 60,
    game_progress = (60 - minutes_remaining) / 60,
    made = as.numeric(field_goal_result == "made"),
    tie_go_ahead = score_differential >= -3 & score_differential <= 0,
    late_tga = tie_go_ahead & qtr %in% c("4", "5"),
    # Code kicking team consistently across time to deal with location changes
    # I use PFR's scheme for this out of convenience
    pfr_pos_team = tolower(posteam),
         pfr_pos_team = case_when(
           pfr_pos_team == "gb" ~ "gnb",
           pfr_pos_team == "ten" ~ "oti",
           pfr_pos_team == "ari" ~ "crd",
           pfr_pos_team == "la" ~ "ram",
           pfr_pos_team == "lac" ~ "sdg",
           pfr_pos_team == "kc" ~ "kan",
           pfr_pos_team == "sf" ~ "sfo",
           pfr_pos_team == "bal" ~ "rav",
           pfr_pos_team == "oak" ~ "rai",
           pfr_pos_team == "no" ~ "nor",
           pfr_pos_team == "ne" ~ "nwe",
           pfr_pos_team == "ind" ~ "clt",
           pfr_pos_team == "tb" ~ "tam",
           pfr_pos_team == "hou" ~ "htx",
           # fixing old format of jacksonville
           pfr_pos_team == "jac" ~ "jax",
           # fixing old san diego
           pfr_pos_team == "sd" ~ "sdg",
           # fixing old Rams
           pfr_pos_team == "stl" ~ "ram",
           TRUE ~ pfr_pos_team
         ),
  )

## Replacement kicker stuff #################################
pbp_f %>%
  group_by(pfr_pos_team, season) %>%
  arrange(pfr_pos_team, season, game_date) %>%
  # Creates a list of all kicker IDs for a given team in given season
  summarize(
    all_kickers = list(unique(kicker_player_id))
  ) %>%
  rowwise() %>%
  # Now I create variables for who the first kicker was that year and who the
  # last one was
  mutate(
    first_kicker = all_kickers[1],
    last_kicker = all_kickers[length(all_kickers)]
  ) %>%
  group_by(pfr_pos_team) %>%
  # Now I'm creating variables that look at who the prior year's kicker was
  # and who the next year's kicker is
  mutate(
    prev_kicker = lag(last_kicker),
    next_kicker = lead(first_kicker)
  ) %>%
  select(-all_kickers) ->
  team_kickers

# Now adding this to the main kicks data frame
pbp_f %<>% 
  left_join(team_kickers, pbp_f, by = c("pfr_pos_team", "season"),
            suffix = c(".x", ""))

# Create the replacement kicker variables
pbp_f %<>%
  mutate(
    replacement = (kicker_player_id != first_kicker & kicker_player_id != next_kicker) |
      (kicker_player_id != prev_kicker & kicker_player_id != next_kicker),
    # have to deal with the first and last years differently since they are 
    # missing information --- requires more assumptions
    replacement = case_when(
      season == 2009 ~ (kicker_player_id != first_kicker & kicker_player_id != next_kicker) |
       (kicker_player_id != last_kicker & kicker_player_id != next_kicker),
      season == 2019 ~ (kicker_player_id != first_kicker | 
        (kicker_player_id != prev_kicker & kicker_player_id != last_kicker)),
      TRUE ~ replacement 
    ),
    replacement = as.numeric(replacement)
  ) 
  