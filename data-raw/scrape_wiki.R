
library(tidyverse)
library(rvest)
# nms <- urls %>% names()
# nms
# nm <- nms[1]
# url <- as.character(urls[nm])
# url
# page <- xml2::read_html(url)
# nodes <- rvest::html_nodes(page, css = '.wikitable')
# nodes
# # #Game_summary
# headers <- rvest::html_nodes(page, css = '#Game_summary')
# headers %>% html_children()
#
# dfs <- purrr::map(nodes, ~{
#   tryCatch(rvest::html_table(.x, fill = TRUE), error = function(e) NULL)
# })
# dfs

# rvest::html_text(nodes[1])
# 1: male contestants
# 2: female contestants
url_main <- 'https://en.wikipedia.org/wiki/The_Challenge_(TV_series)'
page_main <- xml2::read_html(url_main)
#mw-content-text > div.mw-parser-output > table:nth-child(44) > tbody > tr:nth-child(2) > td:nth-child(2) > i > a

get_attrs <- function(css) {
  page_main %>%
    html_elements(css) %>%
    html_attrs() %>%
    enframe('idx', 'value') %>%
    hoist(value, 'title_full' = 'title', 'url' = 'href') %>%
    select(-value) %>%
    select(idx, title_full, url)
}

urls_all_wo_bos <-
  get_attrs('tr > td > i > a')
urls_all_wo_bos

url_first_wo_bos <- urls_all_wo_bos %>% filter(title_full == 'Road Rules: All Stars') %>% head(1)
url_last_wo_bos <- urls_all_wo_bos %>% filter(title_full == 'The Challenge: Double Agents') %>% head(1)

urls_wo_bos <-
  urls_all_wo_bos %>%
  filter(idx %>% between(url_first_wo_bos$idx, url_last_wo_bos$idx))
urls_wo_bos

urls_bos <-
  get_attrs('tr > td > a') %>%
  filter(title_full %>% str_detect('Battle of the Seasons')) %>%
  select(-idx) %>%
  distinct() %>%
  filter(title_full %>% str_detect('World\\/Road|2012'))
urls_bos

urls <-
  bind_rows(
    urls_wo_bos,
    urls_bos
  ) %>%
  mutate(
    across(url, ~sprintf('https://en.wikipedia.org%s', .x)),
    title = title_full %>% str_replace('(^.*)[:]\\s(.*$)', '\\2')
  ) %>%
  select(-idx)
urls

seasons <-
  page_main %>%
  html_nodes('.wikitable') %>%
  pluck(3) %>%
  html_table() %>%
  set_names(c('order', 'title', 'host', 'format', 'original_release', 'location', 'winners')) %>%
  group_by_at(vars(-winners)) %>%
  summarize(across(winners, ~paste0(.x, collapse = ', ', sep = ''))) %>%
  ungroup()
seasons

url_fixes <-
  tibble(
    order = c(2, 3, 4, 5, 23, 30),
    title_seasons = c('Real World vs. Road Rules', 'Challenge 2000', 'Extreme Challenge', 'Battle of the Seasons', 'Battle of the Seasons', 'XXX: Dirty 30'),
    title_urls = c('Real World/Road Rules Challenge (season)', 'Real World/Road Rules Challenge 2000', 'Real World/Road Rules Extreme Challenge', 'Battle of the Seasons', 'Battle of the Seasons (2012)', 'Dirty 30')
  )

wiki_links <-
  seasons %>%
  select(order, title) %>%
  left_join(url_fixes) %>%
  mutate(
    across(title, ~case_when(!is.na(title_urls) ~ title_urls, TRUE ~ .x))
  ) %>%
  left_join(urls) %>%
  # Change name back
  mutate(
    across(title, ~case_when(!is.na(title_urls) ~ title_seasons, TRUE ~ .x))
  ) %>%
  select(order, title, title_full, url) %>%
  filter(title != 'TBA')
wiki_links

usethis::use_data(wiki_links, internal = TRUE, overwrite = TRUE)

