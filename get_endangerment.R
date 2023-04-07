## make endangerment csv

library(sf)

language_ranges = readRDS("data/lang_polys_merged_cea.rds")

df = data.frame(language_ranges)

write.csv(df, "data/language_endangerment.csv")
