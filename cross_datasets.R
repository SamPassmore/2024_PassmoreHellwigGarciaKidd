## Cross reference Grambank and EGIDS 
## There is no direct link between Grambank and EGIDS data - we need to link them both to Glottolog
## Grambank links to Glottolog through Glottocodes
## EGIDS links to Glottolog through ISO codes

suppressPackageStartupMessages({
  library(tidyr)
  library(dplyr)
  library(scales)
})

## 1. Grambank to Glottolog

### Read in Grambank
### What do ? mean in grambank? vs NAs
grambank = read.csv("submodules/cldf/values.csv", na.strings = "?")

# Make grambank wide
grambank_wide = pivot_wider(
  grambank,
  names_from = "Parameter_ID",
  id_cols = "Language_ID",
  values_from = "Value"
)

# read glottolog
glottolog = read.csv("https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/languages.csv")

# join grambank and glottolog by Glottocode
grambank_wide = left_join(grambank_wide, glottolog, by = c("Language_ID" = "ID"))

# How many matches between grambank and glottolog
cat("We can match", (sum(grambank_wide$Language_ID %in% glottolog$Glottocode) / nrow(grambank_wide)) * 100, "% languages between Grambank and Glottolog")
# how many languages have ISO codes that match
cat("We can match", round(sum(!grambank_wide$ISO639P3code == "") / nrow(grambank_wide), 2) * 100, "% languages on exact ISO codes in Glottolog\n")
# What if we use the 'closest match' variable
cat("We can match", round(sum(!grambank_wide$Closest_ISO369P3code == "") / nrow(grambank_wide), 2) * 100, "% languages on closest ISO codes in Glottolog\n")

# 2. Match EGIDS to Glottolog

# This is done in get_endangerment.R

# 3. Match Grambank to EGIDS

# Join EGIDS to Grambank
gb_egids = inner_join(grambank_wide, egids, by = c("ISO639P3code" = "LANG_ISO", 
                                                   "Name", "Macroarea", "Latitude", "Longitude",
                                                   "Glottocode", "Countries", "Family_ID", "Closest_ISO369P3code",
                                                   "First_Year_Of_Documentation", "Last_Year_Of_Documentation"))

cat("There are", label_comma()(nrow(gb_egids)), "languages matched between Grambank an d EGIDS\n")
cat("That is", round(nrow(gb_egids) / nrow(grambank_wide), 2) * 100, "% of EGIDS matched to Grambank.\n")
cat("We did not match", sum(!grambank_wide$Language_ID %in% gb_egids$Glottocode), "Grambank languages\n")
cat("We did not match", sum(!egids$LANG_ISO %in% gb_egids$ISO639P3code), "Grambank languages. Many of these don't exist in Grambank\n")

# Which languages were not matched from Grambank
unmatched_grambank = grambank_wide[!grambank_wide$Language_ID %in% gb_egids$Glottocode,c("Name", "Glottocode", "ISO639P3code")]
write.csv(unmatched_grambank, "processed_data/unmatched_grambank.csv")

write.csv(gb_egids, "processed_data/merged_dataset.csv")
