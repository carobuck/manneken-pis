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
  distinct() -> clean_data


## Let's plot a dumb lil chart
library(treemapify)
clean_data %>%
  #count(geo,theme) %>%
  count(theme) %>%
  filter(!is.na(theme)) %>%
  ggplot(aes(y=n,x=fct_rev(fct_reorder(theme,n)))) +
  geom_col(fill='cyan4',color='gray30') +
  geom_label(aes(label=n),hjust = -0.1) +
  coord_flip() +
  theme_bw() +
  labs(y='Number of Costumes',x='',title='Costumes by Themed Category') +
  theme(axis.text = element_text(size=11)) 


clean_data %>%
  #count(geo,theme) %>%
  count(geo) %>%
  filter(n>=10) %>%
  ggplot(aes(area = n, fill = geo, label = paste(n,geo))) +
  geom_treemap(color='white') +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 15) +
  theme(legend.position = 'none') +
  labs(title='Locations with at least 10 Outfits')


# get stats for total number of locations and # themes

clean_data %>%
  count(theme)
