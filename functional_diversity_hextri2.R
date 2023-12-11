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

# Read in custom Hextri code
source("hextri_grobs.R")

# Grambank Distance matrix
gb_dist = readRDS("data/gb.gower.mfd.RDS")

# Read in endangerment data, and languages studied data (Kidd & Garcia)
endangerment_data = read.csv("processed_data/language_endangerment.csv")
kiddgarcia = read.csv("processed_data/name_matching.csv")

# Calculate the Multi-dimensional space 
mds_gb = gb_dist %>% 
  cmdscale(eig=TRUE, k=2) 

# Format the MDS output
mds_points = data.frame(mds_gb$points)
colnames(mds_points) = c("MDS.X", "MDS.Y")
mds_points$Glottocode = rownames(mds_points)

# Join the MDS data to the endangerment, and studied language data
data = left_join(endangerment_data, mds_points, by = "Glottocode")
data = left_join(data, kiddgarcia, by = c("Name" = "egids_matching"))

#### Make Plots

# Subset to necessary data 
df = data[,c("Name", "MDS.X", "MDS.Y", "count", "nochildren_strict", "EGIDS")] 

# Unstudied langauges have an NA count, but this just means they have 0 papers
df$count[is.na(df$count)] = 0

# Ensure we don't have any missing data otherwise GGplot will complain 
df = df[complete.cases(df),]
colnames(df) = c("Name", "x", "y", "count", "nochildren", "EGIDS")

# If a language has at leaast one paper, then it is a studied langauge
df$studied = ifelse(df$count > 1, 1, 0)

# Reverse code the binary variable so that a value of 1 indicates that a language has children speakers
df$children = 1 - df$nochildren

#### Plot 1: Grammatical diversity displayed as points
p = ggplot(df, aes(x, y, col = factor(children))) + 
  geom_point(aes(shape = factor(studied)), alpha = 0.5) +
  coord_equal() + 
  theme_classic(base_size = 16) + 
  theme(aspect.ratio = 1, legend.position = "right") + 
  scale_linewidth(range = c(0, 3)) + 
  scale_alpha(range = c(0.2, 2)) +
  scale_fill_manual(values = c("#ea324c", "#00b38a"), labels = c("No", "Yes")) + 
  guides(col=guide_legend(title="Children Speakers"),
         shape=guide_legend(title="Studied")) + 
  xlab("") + ylab("")

p

ggsave(plot = p, "figures/hextri_full_points.png")

## Plot 2: Binned grammatical diversity, and proportion of languages with no children speakers

n_bins = 14

p = ggplot(df, aes(x, y, fill = factor(children), col = factor(children))) + 
  geom_hextri(color = "white", linewidth = 0.0, bins = n_bins) + 
  geom_hex(fill = NA, color = "white", bins = n_bins, lwd = 0.5) +
  coord_equal() + 
  theme_classic(base_size = 16) + 
  theme(aspect.ratio = 1, legend.position = "right") + 
  scale_linewidth(range = c(0, 3)) + 
  scale_alpha(range = c(0.2, 2)) +
  scale_fill_manual(values = c("#ea324c", "#00b38a"), labels = c("No", "Yes")) + 
  guides(fill=guide_legend(title="Children Speakers"),
         linewidth=guide_legend(title="Log N Papers"),
         alpha=guide_legend(title="N Languages")) + 
  xlab("") + ylab("")

p
layer_data(p) %>% pull(count) %>% summary
.01 * nrow(df)
ggsave(plot = p, "figures/hextri_full_binned.png")


## Plot 3: Binned grammatical diversity, and proportion of languages with no children speakers
## and labelled cells which have been studied

p = ggplot(df, aes(x, y, fill = factor(children), col = factor(children))) + 
  geom_hextri(color = "white", linewidth = 0.0, bins = 15) + 
  geom_hex(fill = NA, color = "white", bins = 15, lwd = 0.5) +
  stat_summary_hex(aes(
    z = count,
    linewidth = after_stat(value),
  ), fun = ~ sum((.x[.x > 0])), col = "black", fill = NA, bins = 15) +
  coord_equal() + 
  theme_classic(base_size = 16) + 
  theme(aspect.ratio = 1, legend.position = "right") + 
  scale_linewidth(range = c(0, 3)) + 
  scale_alpha(range = c(0.2, 2)) +
  scale_fill_manual(values = c("#ea324c", "#00b38a"), labels = c("No", "Yes")) + 
  guides(fill=guide_legend(title="Children Speakers"),
         linewidth=guide_legend(title="N Papers"),
         alpha=guide_legend(title="N Languages")) + 
  xlab("") + ylab("")

p

ggsave(plot = p, "figures/hextri_full_binned_studied.png")

## Plot 3a: Binned grammatical diversity, and proportion of languages with no children speakers
## and labelled cells which have been studied on a log scale

p = ggplot(df, aes(x, y, fill = factor(children), col = factor(children))) + 
  geom_hextri(color = "white", linewidth = 0.0, bins = 15) + 
  geom_hex(fill = NA, color = "white", bins = 15, lwd = 0.5) +
  stat_summary_hex(aes(
    z = count,
    linewidth = after_stat(value),
  ), fun = ~ sum(log(.x[.x > 0])), col = "black", fill = NA, bins = 15) +
  coord_equal() + 
  theme_classic(base_size = 16) + 
  theme(aspect.ratio = 1, legend.position = "right") + 
  scale_linewidth(range = c(0, 3)) + 
  scale_alpha(range = c(0.2, 2)) +
  scale_fill_manual(values = c("#ea324c", "#00b38a"), labels = c("No", "Yes")) + 
  guides(fill=guide_legend(title="Children Speakers"),
         linewidth=guide_legend(title="Log N Papers"),
         alpha=guide_legend(title="N Languages")) + 
  xlab("") + ylab("")

p

ggsave(plot = p, "figures/hextri_full_binned_studiedlog.png")

## Plot 4: Only show studied diversity

p2 = ggplot(df, aes(x, y, fill = factor(children), col = factor(children))) + 
  geom_hextri(color = "white", linewidth = 0.0, bins = 15) + 
  stat_summary_hex(aes(
    z = count,
    fill = factor(after_stat(value) > 0)
  ), fun = ~ sum(log(.x[.x > 0])), col = NA, bins = 15) +
  geom_hex(fill = NA, color = "white", bins = 15, lwd = 0.5) +
  coord_equal() +
  theme_classic(base_size = 16) +
  theme(aspect.ratio = 1, legend.position = "right") +
  scale_linewidth(range = c(0, 3)) +
  scale_alpha(range = c(0.2, 2)) + 
  scale_fill_manual(values = c("#ea324c", "#00b38a", "white", "transparent"), labels = c("No", "Yes", "", "")) + 
  guides(fill=guide_legend(title="Children Speakers"),
         linewidth=guide_legend(title="Log N Papers"),
         alpha=guide_legend(title="N Languages")) + 
  xlab("") + ylab("")
p2

ggsave(plot = p2, "figures/hextri_studieddiversityonly.png")

## Plot 5: Only show studied undiversity

p3 = ggplot(df, aes(x, y, fill = factor(children), col = factor(children))) + 
  geom_hextri(color = "white", linewidth = 0.0, bins = 15) + 
  stat_summary_hex(aes(
    z = count,
    fill = factor(after_stat(value) > 0)
  ), fun = ~ sum(log(.x[.x > 0])), col = NA, bins = 15) +
  geom_hex(fill = NA, color = "white", bins = 15, lwd = 0.5) +
  # # geom_point(aes(shape = factor(studied))) +
  coord_equal() +
  theme_classic(base_size = 16) +
  theme(aspect.ratio = 1, legend.position = "right") +
  scale_linewidth(range = c(0, 3)) +
  scale_alpha(range = c(0.2, 2)) + 
  scale_fill_manual(values = c("#ea324c", "#00b38a", "transparent", "white"), labels = c("No", "Yes", "", "")) +   guides(fill=guide_legend(title="Children Speakers"),
         linewidth=guide_legend(title="Log N Papers"),
         alpha=guide_legend(title="N Languages")) + 
  xlab("") + ylab("")

p3
ggsave(plot = p3, "figures/hextri_notstudiedonly.png")


data %>% 
  filter(MDS.X < -0.1 & MDS.Y > 0 & MDS.Y < 0.1) %>% 
  pull(Family_ID) %>% 
  table(.)
