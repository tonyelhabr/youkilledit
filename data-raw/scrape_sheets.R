
# Inspiration: https://github.com/doehm/survivoR
library(tidyverse)
library(googlesheets4)
sheets <- googlesheets4::gs4_find()
name_conf <- 'The Challenge Confessional Counts'
name_stats <- 'The Challenge Stats Spreadsheet'
id_conf <- sheets %>% filter(name == name_conf) %>% pull(id)
id_stats <- sheets %>% filter(name == name_stats) %>% pull(id)

# googledrive::drive_auth(path = here::here('.R', 'gargle', 'client_secret.json'))
do_download <-
  function(name,
           pattern = name,
           n_max = 2,
           path = file.path('data-raw', sprintf('%s.xlsx', name)),
           overwrite = FALSE) {

    if(file.exists(path) & !overwrite) {
      message('Returning early')
      return(path)
    }

    drive_files <- googledrive::drive_find(pattern = name, n_max = n_max)
    n_file <- nrow(drive_files)
    if(n_file == 0) {
      stop('Must return at least one file.')
    } else if (n_file > 1L) {
      warning(sprintf('Found %d files. Returning just the first one.', n_file))
    }
    res <-
      drive_files %>%
      head(1) %>%
      googledrive::drive_download(
        path = path,
        overwrite = TRUE
      )
    path
  }
path_stats <- do_download(name_stats, pattern = 'Challenge Stats')
path_conf <- do_download(name_conf, pattern = 'Challenge Confessional')
path_conf
nms_conf <-
  path_conf %>%
  readxl::excel_sheets() %>%
  setdiff(c('Historical Totals', 'Top 10 by Season'))
nms_conf

season_number_allstars <- 37
seasons_df <-
  tibble(
    nickname = nms_conf,
    season_number = seq.int(season_number_allstars, season_number_allstars - length(nms_conf) + 1)
  )
seasons_df

# confessionals ----
tidy_conf_sheet <- function(sheet) {
  # df <- googlesheets4::read_sheet(id_conf, sheet = sheet_name)
  df <- path_conf %>% readxl::read_excel(sheet = sheet)
  res <-
    df %>%
    select(-matches('sum|avg|%')) %>%
    rename(player = 1) %>%
    pivot_longer(
      -player,
      names_to = 'week',
      values_to = 'n'
    ) %>%
    mutate(across(c(week, n), as.integer))
  res
}

f <- possibly(tidy_conf_sheet, otherwise = NULL)
confessionals <-
  nms_conf %>%
  tibble(nickname = .) %>%
  mutate(data = map(nickname, f)) %>%
  unnest(data)
confessionals

# vote_history has:
# season_name, season, episode, day, tribe_status, castaway, immunity, vote

# stats ----
nms_stats <-
  path_stats %>%
  readxl::excel_sheets() %>%
  setdiff(c('Main Page', 'Top 10', 'Challengers by Debut Challenge', 'Special Seasons (in progress)'))
nms_stats

stats_main_init <-
  googlesheets4::read_sheet(
    id_stats,
    sheet = 'Main Page',
    skip = 1L,
    col_names = c('season', 'winners', 'date_air', 'n_episode', 'style', 'host', 'link'),
    col_types = c('c')
  ) %>%
  select(-link) %>%
  janitor::clean_names()
stats_main_init

stats_main_init <-
  path_stats %>%
  readxl::read_excel(
    sheet = 'Main Page',
    skip = 1L,
    col_names = c('season', 'winners', 'date_air', 'n_episode', 'style', 'host', 'link')
  ) %>%
  select(-link) %>%
  janitor::clean_names()
stats_main_init

stats_main <-
  stats_main_init %>%
  mutate(
    across(
      season,
      list(
        id = ~str_replace_all(.x, '(^[0-9]+)(.*$)', '\\1') %>% as.integer(),
        name_long = ~str_replace_all(.x, '(^[0-9]+|^[-]{2})(\\s)(.*$)', '\\3')
      )
    ),
    across(winners, ~str_remove_all(.x, '\\s\\([0-9]+\\)')),
    across(
      n_episode,
      list(
        has_reunion = ~str_detect(.x, 'reunion'),
        n_episode = ~str_remove_all(.x, '\\s+.*$')
      ),
      .names = '{fn}'
    )
  ) %>%
  select(season_id, season_name_long, winners, date_air, n_episode, has_reunion, style)

# challenges from survivoR has:
# season_name, season, episode, title, day, challenge_type, winners
stats_main
# stats_main %>% visdat::vis_miss()
usethis::use_data(confessional, overwrite = TRUE)


# players ----
player <-
  googlesheets4::read_sheet(
    id_stats,
    sheet =  nms_stats[2],
    n_max = 1L
  )

stats_nest <-
  nms_stats %>%
  tibble(sheet_name = .) %>%
  mutate(nms = map(sheet_name, ~googlesheets4::read_sheet(id_stats, sheet = .x, n_max = 1L)))
stats_nest

nms_unnest <-
  stats_nest %>%
  unnest(nms) %>%
  group_by(sheet) %>%
  mutate(idx = row_number())
nms_unnest

player %>%
  select(-1) %>%
  janitor::clean_names() %>%
  pivot_longer(
    matches('.*'),
    names_to = 'name',
    values_to = 'value'
  )

