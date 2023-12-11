library(dplyr)

languages_studied = read.csv('data/journal_archive_data_2021.csv', sep = ";")

## In Evans and Garcia (2022) there are
cat("In Evans and Garcia (2022) there are", n_distinct(languages_studied$language), "languages studied across the sample of papers.\n")
cat("From", n_distinct(languages_studied$family), "language families.\n")

distinct_languagesstudied = languages_studied %>% 
  group_by(language) %>% 
  summarise(papers = n())

# Focus on Morphosyntax
morphosyntax_studied = languages_studied %>% 
  filter(topic == "Morphosyntax")

cat("Within Morphosyntax there are", n_distinct(morphosyntax_studied$language), "languages studied.\n")
cat("From", n_distinct(morphosyntax_studied$family), "language families.\n")

## Identify studied languages in Grambank
grambank_endangerment = read.csv('processed_data/merged_dataset.csv')
endangerment = read.csv('processed_data/language_endangerment.csv')

## Match names 
# Duplicate names list, which we will change to match
distinct_languagesstudied$grambank_format = distinct_languagesstudied$language
distinct_languagesstudied$egids_format = distinct_languagesstudied$language

## Here we fix name matched between Grambank and Kidd & Garcia
# Afrikaans - Does not exist in Grambank
# American Sign Language - Does not exist in Grambank
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Arabic"] = "Standard Arabic"
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Armenian"] = "Classical-Middle Armenian"
# Bengali - Does not exist in Grambank
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Bontok (Central)"] = "Central Bontoc"
# "British Sign Language" - Does not exist in Grambank
# "Bulgarian" - Does not exist in Grambank
# "Cantonese" - Does not exist in Grambank
# Conchucos Quechua - Does not exit in Grambank, but a nearby language Huallaga Huánuco Quechua (hual1241) does.
# We make this match
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Conchucos Quechua"] = "Huallaga Huánuco Quechua"
# North Eastern Cree - Does not exit in Grambank, but a nearby language, Plains Cree does.
# We make this match
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Cree (Northern East)"] = "Plains Cree"
# Esperanto - Does not exist in Grambank
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Farsi"] = "Western Farsi"
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Frisian"] = "Western Frisian"
# German - Does not exist in Grambank
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Greek"] = "Modern Greek"
# Greenlandic - Does not exist in Grambank
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Hebrew"] = "Modern Hebrew"
# Hmong - Does not exist in Grambank
# Home sign - Not sure what language this is
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Indonesian"] = "Standard Indonesian"
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Inuktitut"] = "Western Canadian Inuktitut"
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "K\x92iche\x92"] = "K'iche'"
# Kigiriama - Does not exist in Grambank
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Kurdish"] = "Central Kurdish"
# LIS (Italian Sign Language) - Does not exist in grambank
# Limburgian - Does not exist in grambank
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Lisu"] = "Central Lisu"
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Malay"] = "Standard Malay"
# Mandarin - Does not exist in grambank
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Malay"] = "Standard Malay"
# Min Nan does not exist in Grambank
# Norwegian - Does not exist in Grambank
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Nungon"] = "Yau-Nungon"
# Punjabi - Does not exist in Grambank
# Quebec Sign Language - Does not exist in grambank
# R\xe9union Creole - Does not exist in grambank
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Romanian"] = "Aromanian"
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Serbo-Croatian"] = "Serbian-Croatian-Bosnian"
# Sesotho - Does not exist in Grambank, but sister language Lozi does. We make this match
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Sesotho"] = "Lozi"
# Setswana - Unsure what this language is 
# Signing Exact English - Does not exist in Grambank
# Southern Peruvian Quechua - Does not exist in Grambank, but Cusco Quechua does. We use this match
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Southern Peruvian Quechua"] = "Cusco Quechua"
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Tongan"] = "Tonga (Tonga Islands)"
# Trinidadian Creole English - does not exist in grambank
# Turkish Sign Language - Does not exist in Grambank
# Ukranian - Does not exist in Grambank
# Urdu - Does not exist in Grambank
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Warlpiri (Light)"] = "Warlpiri"
# Wichi - Does not exist in Grambank
distinct_languagesstudied$grambank_format[distinct_languagesstudied$language == "Yucatec"] = "Yucatec Maya"


## Here we make matches between EGIDS and Kidd & Garcia
distinct_languagesstudied$egids_format[distinct_languagesstudied$language == "Armenian"] = "Eastern Armenian"
distinct_languagesstudied$egids_format[distinct_languagesstudied$language == "Bontok (Central)"] = "Central Bontoc"
distinct_languagesstudied$egids_format[distinct_languagesstudied$language == "Hmong"] = "Central Huishui Hmong" # There are lots of Hmong languages but they all have the same endangerment
distinct_languagesstudied$egids_format[distinct_languagesstudied$language == "Kigiriama"] = "Giryama" 
distinct_languagesstudied$egids_format[distinct_languagesstudied$language == "Central Lisu"] = "Lisu" 
distinct_languagesstudied$egids_format[distinct_languagesstudied$language == "Mandarin"] = "Mandarin Chinese" 
distinct_languagesstudied$egids_format[distinct_languagesstudied$language == "Min Nan"] = "Min Nan Chinese" 
distinct_languagesstudied$egids_format[distinct_languagesstudied$language == "Nepali"] = "Nepali Kurux"
distinct_languagesstudied$egids_format[distinct_languagesstudied$language == "Punjabi"] = "Eastern Panjabi"
distinct_languagesstudied$egids_format[distinct_languagesstudied$language == "R\xe9union Creole"] = "Réunion Creole French"
distinct_languagesstudied$egids_format[distinct_languagesstudied$language == "Serbian-Croatian-Bosnian"] = "Serbian Standard" # EGIDS seperates these thre languages, but they are all equally endangered
distinct_languagesstudied$egids_format[distinct_languagesstudied$language == "Setswana"] = "Tswana" 
distinct_languagesstudied$egids_format[distinct_languagesstudied$language == "Ukrainian"] = "Ukrainian" 
distinct_languagesstudied$egids_format[distinct_languagesstudied$language == "Wichi"] = "Wichí Lhamtés Güisnay" 


# Names that were not matched
distinct_languagesstudied$grambank_format[!distinct_languagesstudied$grambank_format %in% grambank_endangerment$N]
distinct_languagesstudied$grambank_format[!distinct_languagesstudied$grambank_format %in% endangerment$Name]

