## Cross reference Grambank and EGIDS 

suppressPackageStartupMessages({
  library(tidyr)
  library(dplyr)
  library(scales)
})

### Read in Grambank
### What do ? mean in grambank? vs NAs
grambank = read.csv("submodules/cldf/values.csv", na.strings = "?")

grambank_wide = pivot_wider(
  grambank,
  names_from = "Parameter_ID",
  id_cols = "Language_ID",
  values_from = "Value"
)

glottolog = read.csv("https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/languages.csv")

grambank_wide = left_join(grambank_wide, glottolog, by = c("Language_ID" = "ID"))

# how many languages have ISO codes
sum(!grambank_wide$ISO639P3code == "") / nrow(grambank_wide)
# What if we use the 'closest match' variable
sum(!grambank_wide$Closest_ISO369P3code == "") / nrow(grambank_wide)

## Read in EGIDS data
egids = read.csv('data/language_endangerment.csv')

# How many languages are not being taught to children
# See table of level descriptions here: https://en.wikipedia.org/wiki/Expanded_Graded_Intergenerational_Disruption_Scale
nochildren_levels = c("6b", "7", "8a", "8b", "9")

## No children learners or not
egids$nochildren = ifelse(egids$EGIDS %in% nochildren_levels, 1, 0)

cat("There are", label_comma(accuracy = NULL)(sum(egids$nochildren)), "languages that are not being taught to children.\n")
cat("That is", percent(sum(egids$nochildren) / nrow(egids)), "of contemporary langauges diversity.\n")

gb_egids = left_join(grambank_wide, egids, by = c("Closest_ISO369P3code" = "LANG_ISO"))

cat("There are", label_comma()(nrow(gb_egids)), "languages matched to Grambank.\n")

write.csv(gb_egids, "processed_data/merged_dataset.csv")
