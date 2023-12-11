### MDS of Child Language endangerment

suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(ggExtra)
  library(patchwork)
  library(sf)
  library(hextri)
})

gb_dist = readRDS("data/gb.gower.mfd.RDS")

endangerment_data = read.csv("processed_data/language_endangerment.csv")
kiddgarcia = read.csv("processed_data/name_matching.csv")

mds_gb = gb_dist %>% 
  cmdscale(eig=TRUE, k=2) 

mds_points = data.frame(mds_gb$points)
colnames(mds_points) = c("MDS.X", "MDS.Y")
mds_points$Glottocode = rownames(mds_points)

data = left_join(endangerment_data, mds_points, by = "Glottocode")
data = left_join(data, kiddgarcia, by = c("Name" = "egids_matching"))

data$studied = ifelse(is.na(data$count), "Not Studied", "Studied")
data$count2 = ifelse(is.na(data$count), 0.5, data$count)
data$display_languagefamily = recode(data$Family_ID,
                                     indo1319 = "Indo-European",
                                     atla1278 = "Atlantic-Congo",
                                     aust1307 = "Austronesian",
                                     sino1245 = "Sino-Tibetan",
                                     drav1251 = "Dravidian")
data$display_languagefamily = ifelse(data$display_languagefamily %in% c("Indo-European", "Atlantic-Congo", "Austronesian",
                                                                        "Sino-Tibetan", "Dravidian"), 
                                     data$display_languagefamily,
                                     "Other")

data_studied = data[data$studied == "Studied",]


## Plot Studied vs Not Studied ####
pp_studied = ggplot() + 
  geom_point(data = data, aes(x = MDS.X, y = MDS.Y, fill = studied, size = (count)), shape = 21) + 
  scale_fill_manual(values = c("grey", "red")) + 
  scale_size_continuous(range  = c(1, 20), 
                        limits = c(0, 4257), 
                        breaks = c(1, 100, 500, 1000, 4000)) + 
  theme_classic(base_size = 20) + theme(legend.position = "bottom")
pp_studied

## distribution of papers across the space
dens1 = ggplot() + 
  geom_histogram(data = data_studied, aes(x = MDS.X, y = ..density.., weight = count), bins = 10) + 
  theme_void()

dens2 = ggplot() + 
  geom_histogram(data = data_studied, aes(x = MDS.Y, y = ..density.., weight = count), bins = 10) + 
  theme_void() + 
  coord_flip()


pp_studied = dens1 + plot_spacer() + pp_studied + dens2 + 
  plot_layout(
    ncol = 2, 
    nrow = 2, 
    widths = c(4, 1),
    heights = c(1, 4)
  ) 

# ggsave(plot = pp_studied, filename = "figures/MDS_Studied.png", width = 32, height = 28, units = "cm")

#### Language Names ####

pp_names = ggplot() + 
  geom_point(data = data, aes(x = MDS.X, y = MDS.Y, fill = studied, size = (count)), shape = 21) +
  geom_label_repel(data = data_studied,
             aes(x = MDS.X, y = MDS.Y, label = Name)) + 
  theme_classic(size = 20) + 
  scale_fill_manual(values = c("grey", "red")) + 
  theme(legend.position = "bottom") + 
  scale_size_continuous(range  = c(1, 20), 
                        limits = c(0, 4257), 
                        breaks = c(1, 100, 500, 1000, 4000))
  

pp_names = dens1 + plot_spacer() + pp_names + dens2 + 
  plot_layout(
    ncol = 2, 
    nrow = 2, 
    widths = c(4, 1),
    heights = c(1, 4)
  ) 

# ggsave(plot = pp_names, filename = "figures/MDS_Names.png", width = 32, height = 28, units = "cm")

#### Language Family ####

pp_family = ggplot() + 
  geom_point(data = data, aes(x = MDS.X, y = MDS.Y, size = count), shape = 21) +
  geom_point(data = data_studied,
                   aes(x = MDS.X, y = MDS.Y, fill = display_languagefamily, size = count), shape = 21) + 
  theme_classic(base_size = 20) + 
  theme(legend.position = "bottom") + 
  scale_size_continuous(range  = c(1, 20), 
                        limits = c(0, 4257), 
                        breaks = c(1, 100, 500, 1000, 4000))


pp_family = dens1 + plot_spacer() + pp_family + dens2 + 
  plot_layout(
    ncol = 2, 
    nrow = 2, 
    widths = c(4, 1),
    heights = c(1, 4)
  ) 

# ggsave(plot = pp_family, filename = "figures/MDS_Family.png", width = 32, height = 28, units = "cm")


## Hex Density bins ####

#### With studied language ####
pp_densitystudied = ggplot() + 
  geom_hex(data = data, aes(x = MDS.X, y = MDS.Y), bins = 20) + 
  scale_fill_continuous(low = "grey80", high = "black") + 
  geom_point(data = data_studied, aes(x = MDS.X, y = MDS.Y, col = display_languagefamily, size = count), shape = 19) + 
  scale_size_continuous(range  = c(1, 20), 
                        limits = c(0, 4257), 
                        breaks = c(1, 100, 500, 1000, 4000)) + 
  labs(size = "Number of Papers", col = "Language Family") + 
  theme_classic(base_size = 20) 

# ggsave(plot = pp_densitystudied, filename = "figures/MDS_DensityStudied.png", width = 32, height = 28, units = "cm")

#### By Endangerment ####

pp_densityendangerment = ggplot() + 
  geom_hex(data = data, aes(x = MDS.X, y = MDS.Y), bins = 20) + 
  scale_fill_continuous(low = "grey80", high = "black") + 
  geom_point(data = data[data$EGIDS %in% c("6a", "6b"),], aes(x = MDS.X, y = MDS.Y, col = factor(EGIDS)), shape = 19) + 
  geom_point(data = data_studied, aes(x = MDS.X, y = MDS.Y, size = count), shape = 21, fill = "darkgreen") + 
  scale_size_continuous(range  = c(1, 20), 
                        limits = c(0, 4257), 
                        breaks = c(1, 100, 500, 1000, 4000)) + 
  labs(size = "Number of Papers", col = "Language Family") + 
  theme_classic(base_size = 20) 
# pp_densityendangerment

# ggsave(plot = pp_densityendangerment, filename = "figures/MDS_DensityEndangeredStudied.png", width = 32, height = 28, units = "cm")

# Colour Hex's by endangerment


# Colour by most stable language
ggplot(data = data, aes(x = MDS.X, y = MDS.Y, z = EGIDS.int)) + 
  stat_summary_hex(fun = function(x) min(x), bins = 20) +
  scale_fill_continuous(low = "grey90", high = "#ea324c") +
  theme_classic()

# Colour by most least endangered language
ggplot(data = data, aes(x = MDS.X, y = MDS.Y, z = EGIDS.int)) + 
  stat_summary_hex(fun = function(x) max(x), bins = 20) +
  scale_fill_continuous(low = "grey90", high = "#ea324c") +
  theme_classic()

# Colour by most endangered language
ggplot(data = data, aes(x = MDS.X, y = MDS.Y, z = EGIDS.int)) + 
  stat_summary_hex(fun = function(x) max(x), bins = 20) +
  scale_fill_continuous(low = "grey90", high = "#ea324c") +
  theme_classic()

# Colour by Modal value
ggplot(data = data, aes(x = MDS.X, y = MDS.Y, z = EGIDS.int)) + 
  stat_summary_hex(fun = function(x) as.numeric(names(sort(-table(x)))[1]), bins = 20) +
  scale_fill_continuous(low = "grey90", high = "#ea324c") +
  theme_classic()

# Colour by median value 
ggplot(data = data, aes(x = MDS.X, y = MDS.Y, z = EGIDS.int)) + 
  stat_summary_hex(fun = function(x) median(x), bins = 20) +
  scale_fill_continuous(low = "grey90", high = "#ea324c") +
  geom_point(data = data_studied, aes(x = MDS.X, y = MDS.Y, size = count), col = "black", shape = 19) + 
  scale_size_continuous(range  = c(1, 20), 
                        limits = c(0, 4257), 
                        breaks = c(1, 100, 500, 1000, 4000)) + 
  theme_classic()

# Colour by propotion of child speaker languages
ggplot(data = data, aes(x = MDS.X, y = MDS.Y, z = nochildren)) + 
  stat_summary_hex(fun = function(x) mean(x), bins = 20) +
  scale_fill_continuous(low = "grey90", high = "#ea324c") +
  geom_point(data = data_studied, aes(x = MDS.X, y = MDS.Y, size = count), col = "black", shape = 19) + 
  scale_size_continuous(range  = c(1, 20), 
                        limits = c(0, 4257), 
                        breaks = c(1, 100, 500, 1000, 4000)) +
  theme_classic()

# Count and Endangerment 
png("figures/functional_richness_hextri.png", width = 210, height = 210, units = "mm", res = 100)
hextri_plot = hextri(
  data$MDS.X,
  data$MDS.Y,
  class = data$nochildren,
  colour = c("#00b38a", "#ea324c"),
  nbins = 20,
  style = "alpha",
  # border = "red", 
  diffuse = FALSE, 
  sorted = TRUE,
  minfrac = 0
) 
dev.off()

# plot(data$MDS.X, data$MDS.Y, type = "n")
# border_colours = 
# with(hextri_plot, polygon(x,y,col=col,border="gold"))

# library(hexbin)
# h<-hexbin(data$MDS.X, data$MDS.Y, IDs=TRUE,xbins=30)
# centers<-hcell2xy(h)

# png("figures/functional_richness.png", width = 210, height = 210, units = "mm", res = 100)
# plot(data$MDS.X, data$MDS.Y, type = "n")
# polygon(c(0.1, 0.2, 0, 0), c(0.1, 0.2, 0, -0.1), col = "orange", lty = 2, lwd = 2, border = "red")
# 
# polygon(x = h@xcm, y = h@ycm)
# polygon(x = xx$x, y = xx$y, col = xx$col)
# 
# plot(centers$x, centers$y)
# 
# hexbin::hexbin(data$MDS.X,
#                data$MDS.Y)
# # points(data_studied$MDS.X, data_studied$MDS.Y, cex = log(data_studied$count)  , pch = 21, bg = scales::alpha("grey70", 0.6))
# legend(-0.25, -0.2, legend = c("Children speakers", "No children speakers"), fill = c("#00b38a", "#ea324c"))
# dev.off()

# ggplot(data = data, 
#        aes(x = MDS.X, y = MDS.Y, z = EGIDS.int)) + 
#   stat_summary_hex(fun = function(x) median(x), bins = 20, aes(alpha = ..count..))

# plot(data$MDS.X, data$MDS.Y, type="n")
# hexbin::hexbin(data$MDS.X, data$MDS.Y,IDs=TRUE,xbins=20)

# Count and Child Diversity
data$studied_bin = ifelse(data$studied == "Studied", 1, 0)
data$paper_count = data$count

plot_df = data[,c("MDS.X", "MDS.Y", "paper_count")] 
plot_df$paper_count[is.na(plot_df$paper_count)] = -999
plot_df = na.omit(plot_df)

pp1 = ggplot(data = plot_df, aes(x = MDS.X, y = MDS.Y)) +
  geom_hex() 

pp1 +
  stat_summary_hex(
    aes(
      z = paper_count,
      linewidth = after_stat(value)
    ),
    fun = ~ sum(.x[.x > 0]),
    col = "gold",
    fill = NA,
    alpha = 0.01
  )  +
  scale_linewidth(range = c(0, 1))

ggsave(filename = "figures/hexplot_highlighted.png", plot = pp, width = 32, height = 28, units = "cm")


### Testing zone ####
library(ggplot2)

set.seed(123)

n = 1000

df = data.frame(x = rnorm(n), 
                y = rnorm(n),
                group = sample(0:1, n, prob = c(0.9, 0.1), replace = TRUE))
df$sums[df$group == 1] = runif(sum(df$group == 1), min = 0.5, max = 2)
df$sums[is.na(df$sums)] <- -999

df = data[,c("MDS.X", "MDS.Y", "count")] 
df$count[is.na(df$count)] <- 0
df = df[complete.cases(df),]
colnames(df) = c("x", "y", "sums")

ggplot(df, aes(x = x, y = y)) +
  geom_hex() +
  stat_summary_hex(aes(
    z = sums,
    linewidth = after_stat(value),
  ), fun = ~ sum(.x[.x > 0]), col = "gold", fill = NA) +
  scale_linewidth(range = c(0, 1))
