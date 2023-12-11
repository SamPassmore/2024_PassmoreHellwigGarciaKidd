## This script cross EGIDS with Kidd & Garcia
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  library(patchwork)
  library(scales)
})

egids = read.csv('processed_data/language_endangerment.csv')
kiddgarcia = read.csv("data/journal_archive_data_2021.csv", sep = ";")

distinct_languagesstudied = kiddgarcia %>% 
  group_by(language) %>% 
  na.omit() %>% 
  summarise(count = n())

# Encourage name matching between Kidd
distinct_languagesstudied$egids_matching = distinct_languagesstudied$language
# making name changes to maximise matches
## Here we fix name matched between Grambank and Kidd & Garcia
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Arabic"] = "Standard Arabic"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Armenian"] = "Eastern Armenian"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Bontok (Central)"] = "Bontok"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Cantonese"] = "Yue Chinese"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Conchucos Quechua"] = "Huallaga Huánuco Quechua"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Cree (Northern East)"] = "Plains Cree"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Hmong"] = "Central Huishui Hmong"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Farsi"] = "Western Farsi"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Frisian"] = "Western Frisian"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Greek"] = "Modern Greek"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Hebrew"] = "Modern Hebrew"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Indonesian"] = "Standard Indonesian"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Limburgian"] = "Limburgan"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Inuktitut"] = "Western Canadian Inuktitut"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "K\x92iche\x92"] = "K'iche'"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Kigiriama"] = "Giryama"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Kurdish"] = "Central Kurdish"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Malay"] = "Standard Malay"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Mandarin"] = "Mandarin Chinese"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Min Nan"] = "Min Nan Chinese"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Nepali"] = "Nepali Kurux"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Nungon"] = "Yau-Nungon"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Punjabi"] = "Eastern Panjabi"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "R\xe9union Creole"] = "Réunion Creole French"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Romanian"] = "Aromanian"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Setswana"] = "Tswana"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Ukranian"] = "Ukrainian"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Serbo-Croatian"] = "Serbian Standard"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Southern Peruvian Quechua"] = "Cusco Quechua"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Tongan"] = "Tonga (Tonga Islands)"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Warlpiri (Light)"] = "Warlpiri"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Yucatec"] = "Yucatec Maya"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Wichi"] = "Wichí Lhamtés Güisnay"
distinct_languagesstudied$egids_matching[distinct_languagesstudied$language == "Sesotho"] = "Lozi"

# save name matches
write.csv(distinct_languagesstudied, "processed_data/name_matching.csv")

# Join EGIDS and K&G
kg_egids = left_join(distinct_languagesstudied, egids, by = c("egids_matching" = "Name"))


# which languages did not match
cat("We have matched", sum(!is.na(kg_egids$LANG_ISO)), "languages from the Kidd and Garcia dataset.\n")
cat("From a total of", nrow(distinct_languagesstudied), "languages.\n")

cat("The unmatched languages are:")
cat(paste(distinct_languagesstudied$language[is.na(kg_egids$LANG_ISO)], collapse = "\n"))

## What is the Distribution of endangered languages that were studies
# total languages in each enganderment category
total_egids_category = egids %>% group_by(EGIDS) %>% summarise(total = n())

by_language = kg_egids %>% 
  filter(!is.na(EGIDS)) %>%
  group_by(EGIDS) %>% 
  summarise(n_languages = n(),
            count = sum(count))

by_language$proportion = by_language$n_languages / total_egids_category$total[match(by_language$EGIDS, total_egids_category$EGIDS)]

p1 = ggplot(by_language, aes(x = EGIDS, y = proportion, fill = EGIDS)) + 
  geom_bar(stat='identity') + 
  theme_classic() + 
  scale_fill_manual(values = c(rep("#00b38a", 6), "#f2ac42")) + 
  ylab("Proportion of Languages") + 
  xlab(element_blank()) + 
  theme(legend.position = "none")

p2 = ggplot(by_language, aes(x = EGIDS, y = count, fill = EGIDS)) + 
  geom_bar(stat='identity') + 
  theme_classic() + 
  scale_fill_manual(values = c(rep("#00b38a", 6), "#f2ac42")) + 
  scale_y_continuous(name="Count of Papers", labels = comma) + 
  xlab("Level of Endangerment")  + 
  theme(legend.position = "none")

pp = p1 / p2

ggsave(plot = pp, filename = "figures/kiddgarcia_egids.png", width = 210, height = 290 / 2, units = "mm")

#### Population sizes by endangerment status ####
egids_kg = left_join(egids, distinct_languagesstudied, by = c("Name" = "egids_matching"))
egids_kg$studied = ifelse(is.na(egids_kg$language), "Not Studied", "Studied")
egids_kg$studied = factor(egids_kg$studied, levels = c("Studied", "Not Studied"))

#### Summary statistics ####
stable_languages = sum(egids_kg$EGIDS %in% c(1:5)) / sum(!is.na(egids_kg$EGIDS))
cat(round(stable_languages, 2) * 100, "% of languages are stable (in category 1 - 5)")
stable_languages = sum(egids_kg$EGIDS[egids_kg$studied == "Studied"] %in% c(1:5)) / sum(!is.na(egids_kg$EGIDS[egids_kg$studied == "Studied"]))
cat(round(stable_languages, 2) * 100, "% of languages are stable (in category 1 - 5), and are studied in CAR")

vulnerable_languages = sum(egids_kg$EGIDS %in% c("6a", "6b")) / sum(!is.na(egids_kg$EGIDS))
cat(round(vulnerable_languages, 2) * 100, "% of languages are vulnerable (in category 6a or 6b)")
vulnerable_languages = sum(egids_kg$EGIDS[egids_kg$studied == "Studied"] %in% c("6a", "6b")) / sum(!is.na(egids_kg$EGIDS[egids_kg$studied == "Studied"]))
cat(round(vulnerable_languages, 2) * 100, "% of languages are vulnerable (in category 6a or 6b), and are studied in CAR")

languages_6a = sum(egids_kg$EGIDS %in% c("6a")) / sum(!is.na(egids_kg$EGIDS))
languages_6b = sum(egids_kg$EGIDS %in% c("6b")) / sum(!is.na(egids_kg$EGIDS))
cat(round(languages_6a, 2) * 100, "% of languages are in category 6a")
cat(round(languages_6b, 2) * 100, "% of languages are in category 6b")
languages_6a = sum(egids_kg$EGIDS[egids_kg$studied == "Studied"] %in% c("6a")) / sum(!is.na(egids_kg$EGIDS[egids_kg$studied == "Studied"]))
languages_6b = sum(egids_kg$EGIDS[egids_kg$studied == "Studied"] %in% c("6b")) / sum(!is.na(egids_kg$EGIDS[egids_kg$studied == "Studied"]))
cat(round(languages_6a, 2) * 100, "% of languages are in category 6a")
cat(round(languages_6b, 2) * 100, "% of languages are in category 6b")

sleeping_langauges = sum(egids_kg$EGIDS %in% c("7", "8a", "8b", "9")) / sum(!is.na(egids_kg$EGIDS))
cat(round(sleeping_langauges, 2) * 100, "% of languages are sleeping")

languagesspoken_bychildren = sum(egids_kg$EGIDS[egids_kg$studied == "Studied"] %in% c("1", "2", "3", "4", "5", "6a", "6b")) / 
  sum(egids_kg$EGIDS %in% c("1", "2", "3", "4", "5", "6a", "6b"))
cat("The proportion of langauges that are able to be studied, divided by those available to be studied by CAR is:", 
    round(languagesspoken_bychildren, 3) * 100, "%")

#### Graphs ####

pp_pop = ggplot(egids_kg, aes(x = log(L1_POP), group = studied, fill = factor(studied))) + 
  geom_density(alpha = 0.5) + 
  theme_classic() + 
  ylab("Density") + 
  xlab("Log L1 speaker population") + 
  scale_fill_manual(values = c("#F15854FF", "#FAA43AFF")) + 
  theme(legend.title = element_blank())

ggsave(plot = pp_pop, filename = "figures/population_size_studied.png", width = 210, height = 100, units = "mm")

## key statistics
tapply(egids_kg$L1_POP, egids_kg$studied, summary)
tapply(egids_kg$L1_POP[egids_kg$L1_POP != 0], egids_kg$studied[egids_kg$L1_POP != 0], summary)

## Distribution in 2060
bromham_probabilities = readxl::read_xlsx("data/Bromham_suppdata.xlsx", sheet = 4, skip = 2)
colnames(bromham_probabilities)[1] = "ISO"

bromham_2060 = bromham_probabilities %>%
  select(ISO, contains("40"))

bromham_2060$EGIDS.value = apply(bromham_2060[,2:ncol(bromham_2060)], 1, function(x) names(x)[x == max(x)])
bromham_2060$EGIDS.value = case_match(bromham_2060$EGIDS.value,
                                      "P[Y=1].40" ~ "1a-6a",
                                      "P[Y=2].40" ~ "6b",
                                      "P[Y=5].40" ~ "8b",
                                      "P[Y=7].40" ~ "10")
bromham_2060$EGIDS.value = factor(bromham_2060$EGIDS.value, levels = c("1a-6a", "6b", "8b", "10"))

bromham_summary = bromham_2060 %>% 
  group_by(EGIDS.value) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

bromham_summary = data.frame(studied = "2060",
                             EGIDS = bromham_summary$EGIDS.value,
                             n = bromham_summary$n,
                             freq = bromham_summary$freq)

## Proportion of endangerment in studied languages vs Observed proportions of endangerment
df = egids_kg %>% 
  group_by(studied, EGIDS) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  na.omit()


p_base = ggplot(df, aes(y = freq, x = EGIDS, fill = EGIDS, group = studied)) +
  geom_bar(stat='identity') +
  ylim(c(0, 0.8)) + 
  facet_wrap(~studied, nrow = 3) + 
  ylab("Proportion of languages") + 
  xlab("EGIDS") + 
  scale_fill_manual(values = c(rep("#00b38a", 6), "#f2ac42", rep("#ea324c", 6))) +
  theme_classic(base_size = 20) + 
  theme(legend.position = "none")

pp = p_base + geom_bracket(
  xmin = "7",
  xmax = "9",
  y.position = 0.3,
  tip.length = 0.05,
  vjust = -0.5,
  label = "No children speakers",
  data = data.frame(studied = factor("Not Studied", levels = c("Studied", "Not Studied")), 
                                     EGIDS = "9"))

ggsave(plot = pp, filename = "figures/figure1a_endangermentdistributions.png", width = 210, height = 150, units = "mm")

## Distribution in 2060
bromham_probabilities = readxl::read_xlsx("data/Bromham_suppdata.xlsx", sheet = 4, skip = 2)
colnames(bromham_probabilities)[1] = "ISO"

bromham_2060 = bromham_probabilities %>%
  select(ISO, contains("40"))

bromham_2060$EGIDS.value = apply(bromham_2060[,2:ncol(bromham_2060)], 1, function(x) names(x)[x == max(x)])
bromham_2060$EGIDS.value = case_match(bromham_2060$EGIDS.value,
                                      "P[Y=1].40" ~ "1a-6a",
                                      "P[Y=2].40" ~ "6b",
                                      "P[Y=5].40" ~ "8b",
                                      "P[Y=7].40" ~ "10")
bromham_2060$EGIDS.value = factor(bromham_2060$EGIDS.value, levels = c("1a-6a", "6b", "7", "8a", "8b", "9", "10"))

bromham_summary = bromham_2060 %>% 
  group_by(EGIDS.value) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

bromham_summary = data.frame(studied = "2060",
                             EGIDS = bromham_summary$EGIDS.value,
                             n = bromham_summary$n,
                             freq = bromham_summary$freq)
bromham_summary$facet = "Endangerment in 2060"

bromham_summary$EGIDS = factor(bromham_summary$EGIDS, c("1a-6a", "6b", "7", "8a", "8b", "9", "10"))

pp = ggplot(bromham_summary, aes(y = freq, x = EGIDS, fill = EGIDS)) +
  geom_bar(stat='identity') +
  facet_wrap(~facet) + 
  xlab("EGIDS") + 
  ylab("") + 
  scale_fill_manual(drop = FALSE, values = c("#00b38a", "#f2ac42", rep("#ea324c", 7))) +
  theme_classic(base_size = 20) + 
  theme(legend.position = "none") + 
  scale_x_discrete(drop = FALSE)

ggsave(plot = pp, filename = "figures/figure1b_endangermentdistributions_2060.png", width = 210, height = 80, units = "mm")

