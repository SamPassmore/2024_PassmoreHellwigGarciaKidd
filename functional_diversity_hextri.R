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

#### PLots
df = data[,c("MDS.X", "MDS.Y", "count")] 
df$count[is.na(df$count)] <- 0
df = df[complete.cases(df),]
colnames(df) = c("x", "y", "paper_count")

points_sf <- st_as_sf(df, coords = c("x","y"))

# generate 30x30 hex grid to match default bin size of stat_bin_hex(),
# add cell_id
hex_grid <- st_make_grid(points_sf, n = c(30,30), square = FALSE) %>% 
  st_as_sf() %>% 
  mutate(cell_id  = row_number())

# spatial join to match points with cell_id, 
# summarise without geometries (would generate multipoints),
# join hex geometries by cell_id
hex_cells <- points_sf %>% 
  st_join(hex_grid) %>% 
  st_drop_geometry() %>% 
  summarise(count = n(), includes_1 = any(paper_count > 0), paper_count = sum(paper_count), .by = cell_id) %>% 
  right_join(hex_grid, .)
#> Joining with `by = join_by(cell_id)`

# adding higlight as a separate layer with constant color, 
# controlling border color through aesthetics can introduce some artefacts when
# neighbouring cells are from different groups
library(RColorBrewer)

p = ggplot(hex_cells) +
  geom_sf(aes(fill = count)) +
  geom_sf(data = ~ filter(.x, includes_1), aes(linewidth = paper_count), color = "gold", fill = "transparent") + 
  scale_linewidth(range = c(0.4, 3), breaks = c(1, 500, 4000), "N Papers") +
  guides(fill = guide_legend(title="N Languages")) +
  scale_fill_gradientn(breaks = c(1, 5, 10), colours = brewer.pal(8, "Blues")[rev(c(3,6,8))]) + 
  theme_classic(base_size = 20)

ggsave(plot = p, filename = "figures/hexbins_highlighted_rev.pdf")
