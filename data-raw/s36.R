
url <- wiki_links$url[36]
page <- url %>% xml2::read_html()

cells <-
  page %>%
  html_elements('.wikitable') %>%
  pluck(4) %>%
  html_elements('td')
cells
txt_trim <- cells %>% html_text(trim = TRUE)
i <- cells %>% html_elements('i') %>% html_text(trim = TRUE) # %>% str_trim()
i
bg <- cells %>% html_attr('bgcolor')
bg
attrs <- cells %>% html_attrs()
attrs
nodes_colors <-
  page %>%
  html_elements('dl > dd > span')
nodes_colors %>% html_elements('span')
nodes_colors %>%
  html_attr('style')


page %>%
  html_elements('.wikitable') %>%
  pluck(4) %>%
  html_children() %>%
  html_children()

df_init <-
  page %>%
  html_elements('.wikitable') %>%
  pluck(4) %>%
  html_table(header = FALSE) %>%
  janitor::remove_empty() %>%
  select(-1)
df_init
eps <-
  df_init %>%
  select(-1) %>%
  slice(2) %>%
  t() %>%
  c() %>%
  str_remove('\\[.*\\]')
eps

df_init %>%
  slice(3:n()) %>%
  set_names(c('contestant', eps)) %>%
  pivot_longer(
    -contestant,
    names_to = 'ep',
    values_to = 'value'
  )

wiki_links %>%
  mutate(
    page = map(url, xml2::read_html)
  ) %>%
  mutate(
    data = map(page, ~possibly(rvest::html_table(.x, fill = TRUE), otherwise = NULL))
  ) %>%
  select(-page)
tables
