## make endangerment csv

library(sf)

language_ranges = readRDS("data/lang_polys_merged_cea.rds")

df = data.frame(language_ranges)

keep_vars = c("LANG_ISO", "LMP_POP1", "AES", "EGIDS", "L1_POP")
df = df[,keep_vars]

write.csv(df, "data/language_endangerment.csv", row.names = FALSE)
