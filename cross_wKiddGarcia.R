## We have merged EGIDS and Grambank into one file
## Now we need a file that cross references this file with the results from Kidd & Garcia (2022)

library(dplyr)

languages_studied = read.csv('data/journal_archive_data_2021.csv', sep = ";")
gb_egids = read.csv("processed_data/merged_dataset.csv")

# 1. Summarise what languages were studied by Evans and Garcia
distinct_languagesstudied = languages_studied %>% 
  group_by(language) %>% 
  summarise(papers = n())

## In Evans and Garcia (2022) there are
cat("In Evans and Garcia (2022) there are", n_distinct(languages_studied$language), "languages studied across the sample of papers.\n")
cat("From", n_distinct(languages_studied$family), "language families.\n")

# 2. cross reference these langauges with the GB EGIDS dataset
distinct_matches = inner_join(distinct_languagesstudied, gb_egids, by = c("language" = "Name"))

cat("We can match", nrow(distinct_matches), "languages by name.\n")
cat("That is", round(nrow(distinct_matches) / nrow(distinct_languagesstudied), 2) * 100, "% of total languages, matched by name.\n")

# 3. Identify matches for the unmatched languages
unmatched_languages = distinct_languagesstudied$language[!distinct_languagesstudied$language %in% gb_egids$Name]

distinct_languagesstudied$matching_names = distinct_languagesstudied$language
## Here we fix name matched between Grambank and Kidd & Garcia
# Afrikaans - Does not exist in Grambank
# American Sign Language - Does not exist in Grambank
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Arabic"] = "Standard Arabic"
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Armenian"] = "Classical-Middle Armenian"
# Bengali - Does not exist in Grambank
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Bontok (Central)"] = "Central Bontoc"
# "British Sign Language" - Does not exist in Grambank
# "Bulgarian" - Does not exist in Grambank
# "Cantonese" - Does not exist in Grambank
# Conchucos Quechua - Does not exit in Grambank, but a nearby language Huallaga Huánuco Quechua (hual1241) does.
# We make this match
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Conchucos Quechua"] = "Huallaga Huánuco Quechua"
# North Eastern Cree - Does not exit in Grambank, but a nearby language, Plains Cree does.
# We make this match
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Cree (Northern East)"] = "Plains Cree"
# Esperanto - Does not exist in Grambank
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Farsi"] = "Western Farsi"
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Frisian"] = "Western Frisian"
# German - Does not exist in Grambank
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Greek"] = "Modern Greek"
# Greenlandic - Does not exist in Grambank
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Hebrew"] = "Modern Hebrew"
# Hmong - Does not exist in Grambank
# Home sign - Not sure what language this is
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Indonesian"] = "Standard Indonesian"
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Inuktitut"] = "Western Canadian Inuktitut"
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "K\x92iche\x92"] = "K'iche'"
# Kigiriama - Does not exist in Grambank
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Kurdish"] = "Central Kurdish"
# LIS (Italian Sign Language) - Does not exist in grambank
# Limburgian - Does not exist in grambank
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Lisu"] = "Central Lisu"
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Malay"] = "Standard Malay"
# Mandarin - Does not exist in grambank
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Malay"] = "Standard Malay"
# Min Nan does not exist in Grambank
# Norwegian - Does not exist in Grambank
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Nungon"] = "Yau-Nungon"
# Punjabi - Does not exist in Grambank
# Quebec Sign Language - Does not exist in grambank
# R\xe9union Creole - Does not exist in grambank
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Romanian"] = "Aromanian"
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Serbo-Croatian"] = "Serbian-Croatian-Bosnian"
# Sesotho - Does not exist in Grambank, but sister language Lozi does. We make this match
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Sesotho"] = "Lozi"
# Setswana - Unsure what this language is 
# Signing Exact English - Does not exist in Grambank
# Southern Peruvian Quechua - Does not exist in Grambank, but Cusco Quechua does. We use this match
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Southern Peruvian Quechua"] = "Cusco Quechua"
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Tongan"] = "Tonga (Tonga Islands)"
# Trinidadian Creole English - does not exist in grambank
# Turkish Sign Language - Does not exist in Grambank
# Ukranian - Does not exist in Grambank
# Urdu - Does not exist in Grambank
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Warlpiri (Light)"] = "Warlpiri"
# Wichi - Does not exist in Grambank
distinct_languagesstudied$matching_names[distinct_languagesstudied$language == "Yucatec"] = "Yucatec Maya"
