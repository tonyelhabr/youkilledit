
library(tidyverse)
library(rvest)
data('wiki_links', package = 'youkilledit')

retrieve_elims <-
  function(season,
           path_elims = file.path('data-raw', sprintf('elims-s%02d.rds', season)),
           path_html = file.path('data-raw', sprintf('%02d.html', season)),
           overwrite = FALSE) {
    season = 34
    path_elims = file.path('data-raw', sprintf('elims-s%02d.rds', season))
    path_html = file.path('data-raw', sprintf('%02d.html', season))
    overwrite = FALSE

    if(file.exists(path_elims) & !overwrite) {
      return(path_elims)
    }

    if(!file.exists(path_html) | overwrite) {
      url <- wiki_links %>% filter(order == !!season) %>% pull(url)
      page <- url %>% xml2::read_html()
      xml2::write_xml(page, path_html)
    } else {
      page <- path_html %>% xml2::read_html()
    }

    colors <-
      page %>%
      html_elements('dl > dd > span') %>%
      html_attr('style') %>%
      as.character() %>%
      str_replace('(background[-]color[:])(.*)([;]\\s?color.*$)', '\\2')
    colors

    labs <-
      page %>%
      html_elements('dl > dd') %>%
      html_text(trim = TRUE)
    labs

    color_labs <-
      tibble(
        # No Yellow, just yellow
        color = tolower(colors),
        lab = labs[1:length(colors)]
      ) %>%
      # some colors might be from a lower legend, so just the first.
      group_by(color) %>%
      slice(1) %>%
      ungroup()
    color_labs

    tds <-
      page %>%
      html_elements('.wikitable') %>%
      pluck(4) %>%
      html_nodes('td')
    tds

    nm <- case_when(
      season == 34 ~ 'background',
      season >= 35 ~ 'bgcolor'
    )


    f <- if(season >= 35) {
      function(x) html_attr(x, 'bgcolor')
    } else if (season == 34) {
      function(x) html_attr(x, 'style') %>% str_remove('background[:]')
    }

    # Doesn't really work for wow?
    supp <-
      tibble(
        value = tds %>% html_text(trim = TRUE),
        color = tds %>% f()
      ) %>%
      mutate(contestant = ifelse(!(value %in% c('SAFE', 'ELIM', 'WIN', 'OUT', 'QUIT', 'DQ', 'WINNER', 'LOSER')), value, NA_character_)) %>%
      filter(value != '') %>%
      fill(contestant) %>%
      filter(value != contestant | is.na(contestant))
    supp

    df_init <-
      page %>%
      html_elements('.wikitable') %>%
      pluck(4) %>%
      html_table(header = FALSE) %>%
      janitor::remove_empty(which = c('rows', 'cols')) %>%
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

    df <-
      df_init %>%
      slice(3:n()) %>%
      set_names(c('contestant', eps)) %>%
      pivot_longer(
        -contestant,
        names_to = 'ep',
        values_to = 'value'
      ) %>%
      filter(value != '') %>%
      mutate(idx = row_number()) %>%
      relocate(idx)
    df
    assertthat::assert_that(nrow(df) == nrow(supp), msg = '`df` and `supp` must have same number of rows')
    res <-
      df %>%
      bind_cols(supp %>% select(color)) %>%
      left_join(color_labs)
    res
    write_rds(res, path_elims)
    path_elims
  }

35:36 %>% map_chr(retrieve_elims)
