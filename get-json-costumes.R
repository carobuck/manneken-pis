# Trying to get data from website, or premade database or scraping was a nightmare (selenium, Docker, UGH.) So, went hunting through HTML manually and found some long lists with all the costume info. Parsing/assembling together here. 
library(jsonlite)
library(tidyverse)

# Saved this xlsx from google sheets -- get ref info on geo and theme IDs 
ref_theme <- readxl::read_xlsx('manneken-pis-costumes-ref.xlsx',sheet='theme') %>%
  separate(name,into = c('trash','id','wip'),sep = "\"") %>%
  separate(wip, into = c('trash2','theme','trash3','trash4'),sep=">|<") %>%
  select(-starts_with('trash'))

ref_geo <- readxl::read_xlsx('manneken-pis-costumes-ref.xlsx',sheet='geography') %>%
  separate(name,into = c('trash','id','wip'),sep = "\"") %>%
  separate(wip, into = c('trash2','geo','trash3','trash4'),sep=">|<") %>%
  select(-starts_with('trash'))

# Load JSONs from file, parse + merge into one df, with ref info attached
costume_theme <- fromJSON("/Users/carobuck/Documents/fun_projects/manneken-pis/theme_to_costumes.json")
costume_location <- fromJSON('/Users/carobuck/Documents/fun_projects/manneken-pis/location_to_costumes.json')

# deal w/ themes
list_of_dfs_no_extra <- lapply(costume_theme, function(df) {
  df[, !names(df) %in% c("t_s")]
})
list_of_dfs_with_id <- imap(list_of_dfs_no_extra, ~ mutate(.x, id_theme = .y))
combined_df_costume <- bind_rows(list_of_dfs_with_id)

# deal w/ geo
list_of_dfs_no_extra <- lapply(costume_location, function(df) {
  df %>% mutate(body = as.character(body)) %>%
    select(-t_s) -> df
  return(df)
})
list_of_dfs_with_id <- imap(list_of_dfs_no_extra, ~ mutate(.x, id_geo = .y))
combined_df_geo <- bind_rows(list_of_dfs_with_id)

# combine + clean up df
combined_df_costume %>%
  full_join(combined_df_geo) %>%
  full_join(ref_geo,by=join_by(id_geo==id)) %>%
  full_join(ref_theme,by=join_by(id_theme==id)) %>%
  mutate(t = str_trim(t),
         body = str_trim(body)) %>%
  distinct() %>%
  glimpse()
