## make endangerment csv

library(sf)
library(dplyr)
library(scales)

language_ranges = readRDS("data/lang_polys_merged_cea.rds")

dd = readxl::read_xlsx("data/Bromham_suppdata.xlsx")

df = data.frame(language_ranges)

keep_vars = c("LANG_ISO", "LMP_POP1", "AES", "EGIDS", "L1_POP")
df = df[,keep_vars]

# make the EGIDS scale a factor
df$EGIDS = factor(df$EGIDS, levels = c("1", "2", "3", "4", "5", "6a", "6b", "7", "8a", "8b", "9"))

# https://en.wikipedia.org/wiki/Expanded_Graded_Intergenerational_Disruption_Scale
# The EGDIS factor has the following relationship to numbers:
# 1   1   National
# 2   2   Regional
# 3   3   Trade
# 4   4   Educational
# 5   5   Written
# 6   6a  Vigorous        "Being learned by children as their first language"
# 7   6b  Threatened      "Only some of the child-bearing generation are transmitting it to their children"
# 8   7   Shifting        "None are transmitting it to their children"
# 9   8a  Moribund        "Only speakers are grandparents generation"
# 10  8b  Nearly Extinct  "Grandparents or older with little opportunity to speak the language"
# 11  9   Dormant         "Symbolic proficiency"
# 12  10  Extinct         "No one retains a sense of ethnic identity"

# make EGIDS a integer
df$EGIDS.int = as.numeric(df$EGIDS)

# Match to Glottolog
## Read in Glottolog data
glottolog = read.csv("https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/languages.csv")
df = left_join(df, glottolog, by = c("LANG_ISO" = "ISO639P3code"))

# How many matches between grambank and glottolog
cat("We can match", round(sum(df$LANG_ISO %in% glottolog$ISO639P3code) / nrow(df), 3) * 100, "% languages between EGIDS and Glottolog")

# which languages don't match
egids_notmatched = df$LANG_ISO[!df$LANG_ISO %in% glottolog$ISO639P3code]

cat("There are", nrow(df), "languages in our dataset \n")

cat("There are", sum(df$EGIDS.int >= 8, na.rm = TRUE), "languages that have no child speakers (including extinct and symbolically practiced languages) \n")
cat("There are", sum(df$EGIDS.int >= 8 & df$EGIDS.int <= 11, na.rm = TRUE), "languages that have no child speakers (excluding extinct languages but including symbolically practiced languages)\n")
cat("There are", sum(df$EGIDS.int >= 8 & df$EGIDS.int <= 10, na.rm = TRUE), "languages that have no child speakers (excluding extinct languages and symbolically practiced languages)\n")

cat("There are", sum(df$EGIDS.int == 7, na.rm = TRUE), "languages that are vulnerable and have limited chlidren speakers")
cat("There are", sum(df$EGIDS.int < 7, na.rm = TRUE), "languages that are stable and available to study in children")

cat("There are", label_comma(accuracy = NULL)(sum(df$nochildren)), "languages that are not being taught to all children of a language.\n")
cat("That is", percent(sum(df$nochildren) / nrow(df)), "of contemporary langauges diversity.\n")

write.csv(df, "processed_data/language_endangerment.csv", row.names = FALSE)
