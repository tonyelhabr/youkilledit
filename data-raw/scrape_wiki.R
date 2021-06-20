
library(tidyverse)
library(rvest)
urls <-
  list(
    `36` = 'https://en.wikipedia.org/wiki/The_Challenge:_Double_Agents',
    `35` = 'https://en.wikipedia.org/wiki/The_Challenge:_Total_Madness'
  )
nms <- urls %>% names()
nms
nm <- nms[1]
url <- as.character(urls[nm])
url
page <- xml2::read_html(url)
nodes <- rvest::html_nodes(page, css = '.wikitable')
nodes
# #Game_summary
headers <- rvest::html_nodes(page, css = '#Game_summary')
headers %>% html_children()

dfs <- purrr::map(nodes, ~{
  tryCatch(rvest::html_table(.x, fill = TRUE), error = function(e) NULL)
})
dfs

# rvest::html_text(nodes[1])
# 1: male contestants
# 2: female contestants
dfs[3]
url_main <- 'https://en.wikipedia.org/wiki/The_Challenge_(TV_series)'
page_main <- xml2::read_html(url_main)
ul <- html_nodes(page_main, 'ul')
ul
li1 <- html_nodes(page_main, 'ul > li > i > b > a') %>% html_attrs()
li2 <- html_nodes(page_main, 'ul > li > b > i > a') %>% html_attrs()
li3 <- html_nodes(page_main, 'ul > li > a') %>% html_attrs()
titles <- li3 %>% map(~unname(.x['title']))
idx_colon <- titles %>% str_which('Challenge[:]\\s')

li3 %>% pluck('title')

urls <-
  c(
    li1,
    li2,
    li3[idx_colon] %>% unique()
  ) %>%
  enframe('idx', 'value') %>%
  hoist(value, 'title', 'href') %>%
  select(idx, title, href)
urls

res <-
  urls %>%
  mutate(
    page = map(href, ~sprintf('https://en.wikipedia.org%s', .x) %>% xml2::read_html()),
    dfs = map(page, ~tryCatch(rvest::html_table(.x, fill = TRUE), error = function(e) NULL))
  ) %>%
  select(-page)

res_unnest <-
  res %>%
  select(idx, title, dfs) %>%
  unnest(dfs) %>%
  mutate(
    n_col = map_int(dfs, ncol),
    n_row = map_int(dfs, nrow),
    nms = map_chr(dfs, ~names(.x) %>% paste0(collapse = ', ', sep = ''))
  ) %>%
  select(-dfs)
res_unnest
# res_unnest %>% count(n_col, n_row, sort = TRUE)
res_unnest %>%
  filter(nms %>% str_detect('Contestant.*Episode'))
