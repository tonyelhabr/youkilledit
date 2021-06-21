
library(tidyverse)
library(rvest)
data('wiki_links', package = 'youkilledit')

tables <-
  wiki_links %>%
  mutate(
    page = map(url, xml2::read_html)
  ) %>%
  mutate(
    data = map(page, ~possibly(rvest::html_table(.x, fill = TRUE), otherwise = NULL))
  ) %>%
  select(-page)
tables

page <-
  "https://en.wikipedia.org/wiki/Real_World/Road_Rules_Challenge:_The_Gauntlet_III" %>%
  xml2::read_html()
page %>%
  html_elements('.wikitable') %>%
  pluck(4) %>%
  rvest::html_table()

res <-
  tables %>%
  select(order, title, data) %>%
  unnest(data) %>%
  mutate(
    n_col = map_int(data, ncol),
    n_row = map_int(data, nrow),
    cols = map_chr(data, ~names(.x) %>% paste0(collapse = ', ', sep = ''))
  ) %>%
  select(-data)
res
res %>% filter(order == 14)

res %>%
  filter(cols %>% str_detect('.*Episode')) -> z
