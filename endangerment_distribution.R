library(dplyr)
library(ggplot2)

languages = read.csv('processed_data/language_endangerment.csv')
egids_gb = read.csv('processed_data/merged_dataset.csv')

# children speakers or not
languages$children_speakers = ifelse(languages$EGIDS.int >= 8, 1, 0)
egids_gb$children_speakers = ifelse(egids_gb$EGIDS.int >= 8, 1, 0)

# For all levels of endangerment
overall_count = languages %>% 
  group_by(EGIDS.int) %>% 
  summarise(values = n() / nrow(languages))

gb_count = egids_gb %>% 
  group_by(EGIDS.int) %>% 
  summarise(values = n() / nrow(egids_gb))

df_list = list(overall = overall_count, gb = gb_count)
df = bind_rows(df_list, .id = "names")

ggplot(df, aes(y = values, x = EGIDS.int, fill = names)) + 
  geom_col(position = "dodge") + 
  geom_vline(xintercept = 7.5)

# Children speakers or not
overall_count = languages %>% 
  group_by(children_speakers) %>% 
  summarise(values = n() / nrow(languages))

gb_count = egids_gb %>% 
  group_by(children_speakers) %>% 
  summarise(values = n() / nrow(egids_gb))

df_list = list(overall = overall_count, gb = gb_count)
df = bind_rows(df_list, .id = "names")

ggplot(df, aes(y = values, x = children_speakers, fill = names)) + 
  geom_col(position = "dodge")