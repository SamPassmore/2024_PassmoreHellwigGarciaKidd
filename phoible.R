suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
  library(ggExtra)
  library(patchwork)
  library(sf)
  library(hextri)
  library(patchwork)
})

source("hextri_grobs.R")

# phoible data
phoible = read.csv("submodules/phoible/cldf/values.csv")
# language data
endangerment_data = read.csv("processed_data/language_endangerment.csv")
# studied language data
kiddgarcia = read.csv("processed_data/name_matching.csv")

# Some languages have been coded multiple times (doculets)
# We want to select one inventory per language, but we make no judgement on which inventory is better
# choice is random
phoible_ss =  phoible %>% 
  dplyr::group_by(Language_ID) %>% 
  dplyr::filter(Inventory_ID == sample(unique(Inventory_ID), 1)) 

# The number of language ids matches the number of Inventory IDs 
# (i.e. there is one language code per inventory code)
all(n_distinct(phoible_ss$Language_ID) == n_distinct(phoible_ss$Inventory_ID))

# Make the dataset wide, this will build a dataset where columns are 
# phonemes and rows are languages.
tt = pivot_wider(phoible_ss, id_cols = Language_ID, values_from = Value, names_from = Value)

# This changes the data to a 0 (phoneme is not used) 1 (phoneme is used)
tt_bin = apply(tt[,2:ncol(tt)], 2, function(x) ifelse(is.na(x), 0, 1))
tt_bin = data.frame(Language_ID = tt$Language_ID, tt_bin)

pho_dist_gower <- tt_bin %>% 
  dplyr::select(-Language_ID) %>%
  cluster::daisy(metric = "gower", warnBin = FALSE)

dd = data.frame(matrix(c(0, 1, 0, 0, 1, 1), ncol = 3))
cluster::daisy(dd, metric = "gower", warnBin = FALSE)

# (3) Run MDS on this data
mds_pho = pho_dist_gower %>% 
  cmdscale(eig=TRUE, k=2)

mds_points = data.frame(mds_pho$points)
colnames(mds_points) = c("MDS.X", "MDS.Y")
mds_points$Glottocode = tt_bin$Language_ID

data = left_join(endangerment_data, mds_points, by = "Glottocode")
data = left_join(data, kiddgarcia, by = c("Name" = "egids_matching"))

#### Plots ####

df = data[,c("Name", "MDS.X", "MDS.Y", "count", "nochildren_strict", "EGIDS")] 
df$count[is.na(df$count)] <- 0
df = df[complete.cases(df),]
colnames(df) = c("Name", "x", "y", "count", "nochildren", "EGIDS")
df$studied = ifelse(df$count > 1, 1, 0)
df$children = 1 - df$nochildren
# View(df[df$nochildren == 1 & df$studied == 1,])

n_bins = 12

p = ggplot(df, aes(x, y, fill = factor(children), col = factor(children))) + 
  geom_hextri(color = "white", linewidth = 0.0, bins = n_bins) + 
  geom_hex(fill = NA, color = "white", bins = n_bins, lwd = 0.5) +
  stat_summary_hex(aes(
    z = count,
    linewidth = after_stat(value),
  ), fun = ~ sum(log(.x[.x > 0])), col = "black", fill = NA, bins = n_bins) +
  
  # geom_point(aes(shape = factor(studied))) +
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
ggsave(plot = p, "figures/hextri_phoible_full.png")

p2 = ggplot(df, aes(x, y, fill = factor(children), col = factor(children))) + 
  geom_hextri(color = "white", linewidth = 0.0, bins = n_bins) + 
  stat_summary_hex(aes(
    z = count,
    # linewidth = after_stat(value),
    fill = factor(after_stat(value) > 0)
  ), fun = ~ sum(log(.x[.x > 0])), col = NA, bins = n_bins) +
  geom_hex(fill = NA, color = "white", bins = n_bins, lwd = 0.5) +
  # # geom_point(aes(shape = factor(studied))) +
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

ggsave(plot = p2, "figures/hextri_phoible_studied.png")

p3 = ggplot(df, aes(x, y, fill = factor(children), col = factor(children))) + 
  geom_hextri(color = "white", linewidth = 0.0, bins = n_bins) + 
  stat_summary_hex(aes(
    z = count,
    fill = factor(after_stat(value) > 0)
  ), fun = ~ sum(log(.x[.x > 0])), col = NA, bins = n_bins) +
  geom_hex(fill = NA, color = "white", bins = n_bins, lwd = 0.5) +
  # # geom_point(aes(shape = factor(studied))) +
  coord_equal() +
  theme_classic(base_size = 16) +
  theme(aspect.ratio = 1, legend.position = "right") +
  scale_linewidth(range = c(0, 3)) +
  scale_alpha(range = c(0.2, 2)) + 
  scale_fill_manual(values = c("#ea324c", "#00b38a", "transparent", "white"), 
                    labels = c("No", "Yes", "", "")) +  
  guides(fill=guide_legend(title="Children Speakers"),
          linewidth=guide_legend(title="Log N Papers"), 
                    alpha=guide_legend(title="N Languages")) + 
  xlab("") + ylab("")

p3

ggsave(plot = p3, "figures/hextri_phoible_notstudied.png")

p / (p2 + p3) +
  plot_layout(guides = 'collect')
