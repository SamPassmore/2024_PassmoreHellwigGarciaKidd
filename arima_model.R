## Model the number of new langauges by year 
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(forecast)
})

kiddgarcia = read.csv('data/journal_archive_data_2021.csv', sep = ";")
egids = read.csv("processed_data/language_endangerment.csv")

# calculate the number of new languages studied per year
by_year = kiddgarcia %>% 
  group_by(year) %>% 
  summarise(n_articles = n())

# Total number of languages studied
years = split(kiddgarcia$language, f = kiddgarcia$year)

by_year$total_languages = NA
studied_languages = unique(years[[1]])
by_year$total_languages[1] = n_distinct(studied_languages)

for(i in 2:length(years)){
  # what languages were studied this year
  new_year = unique(years[[i]])
  
  # How many of those are new languages
  new_total = by_year$total_languages[i - 1] + sum(!new_year %in% studied_languages)
  by_year$total_languages[i] = new_total
  
  # add the new languages to the studied languages pile 
  new_languages = new_year[!new_year %in% studied_languages]
  studied_languages = c(studied_languages, new_languages)
}

# check final value is right
by_year$total_languages[nrow(by_year)] == n_distinct(kiddgarcia$language)

## Plot over time
plot(x = by_year$year, y = by_year$total_languages)


## ARIMA models ####

ts_data = ts(by_year$total_languages, start = c(1974, 1), frequency = 1)
plot(ts_data)
plot(diff(ts_data))

ARIMAfit = auto.arima(ts_data, approximation = FALSE, trace = FALSE)
summary(ARIMAfit)
n_years = 40
fvalues <- forecast(ARIMAfit, n_years)

## Net languages over time
loss_peryear_child = c(0, rep(72, n_years - 1))
loss_peryear_spoke = c(0, rep(12, n_years - 1))
pred = data.frame(fvalues$mean, fvalues$lower, fvalues$upper)

changes_child = apply(pred, 2, function(x) x - cumsum(loss_peryear_child))
changes_child = data.frame(changes_child)

language_loss_child = cbind(years = 2021:2060, changes_child)
language_loss_child$fvalues.mean = ifelse(language_loss_child$fvalues.mean < 0, 0, language_loss_child$fvalues.mean)
language_loss_child$X95. = ifelse(language_loss_child$X95. < 0, 0, language_loss_child$X95.)
language_loss_child$X95..1 = ifelse(language_loss_child$X95..1 < 0, 0, language_loss_child$X95..1)

changes_spoke = apply(pred, 2, function(x) x - cumsum(loss_peryear_spoke))
changes_spoke = data.frame(changes_spoke)

language_loss_spoke = cbind(years = 2021:2060, changes_spoke)
language_loss_spoke$fvalues.mean = ifelse(language_loss_spoke$fvalues.mean < 0, 0, language_loss_spoke$fvalues.mean)
language_loss_spoke$X95. = ifelse(language_loss_spoke$X95. < 0, 0, language_loss_spoke$X95.)
language_loss_spoke$X95..1 = ifelse(language_loss_spoke$X95..1 < 0, 0, language_loss_spoke$X95..1)

pp = 
  ggplot(data = by_year, aes(x = year, y = total_languages)) + 
  geom_line() + 
  # Predicted increase
  geom_smooth(data = pred, aes(y = fvalues.mean, x = 2021:2060, ymin = X95., ymax = X95..1), col = "grey", stat = "identity", alpha = 0.5) + 
  theme_classic(base_size = 20) + 
  # Children spoken languages losees
  geom_line(data = language_loss_child, aes(x = years, y = fvalues.mean), col = "#ea324c", lty = "dashed") +
  geom_smooth(data = language_loss_child, aes(x = years, y = fvalues.mean, ymin = X95., ymax =X95..1), fill = "#ea324c", stat = "identity", linetype=0) +
  # geom_smooth(data = language_loss_child, aes(x = years, y = fvalues.mean, ymin = X95., ymax =X95..1), col = "#3366FF", alpha = 0.4, stat = "identity") + 
  # Spoken Language losees
  geom_line(data = language_loss_spoke, aes(x = years, y = fvalues.mean), col = "#f2ac42", lty = "dotted") +
  geom_smooth(data = language_loss_spoke, aes(x = years, y = fvalues.mean, ymin = X95., ymax =X95..1), fill = "#f2ac42", stat = "identity", linetype=0) +
  # geom_smooth(data = language_loss_spoke, aes(x = years, y = fvalues.mean, ymin = X95., ymax =X95..1), col = "#3366FF", alpha = 0.4, stat = "identity") + 
  ylim(c(0, 210)) +
  xlab("Years") +
  ylab("N languages studied") + 
  geom_vline(aes(xintercept = 2020.5), col = "grey", lty = "dashed") + 
  ggtitle("")

pp

ggsave(plot = pp, file = "figures/arima_forecasts.png", width = 210, height = 100, units = "mm")
