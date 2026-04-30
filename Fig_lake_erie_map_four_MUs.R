library(sf)
library(ggplot2)
library(dplyr)

setwd("D:/OneDrive/Postdoctor/Project_5_yellow_perch_lake_erie")

# -----------------------------
# 1. read your Lake Erie shapefile
# -----------------------------
lake_erie <- st_read("Data/Lake_erie_shape/hydro_p_LakeErie.shp")

# make geometry valid
lake_erie <- st_make_valid(lake_erie)

# transform to lon/lat if needed
lake_erie <- st_transform(lake_erie, 4326)

# if multiple polygons exist, combine into one
lake_erie_union <- st_union(lake_erie)

# -----------------------------
# 2. define approximate MU split lines
#    adjust these if needed
# -----------------------------
bbox <- st_bbox(lake_erie_union)

ymin <- bbox["ymin"] - 1
ymax <- bbox["ymax"] + 1

# approximate west -> east MU boundaries for Lake Erie
x_breaks <- c(
  bbox["xmin"] - 1,
  -82.75,   # MU1 / MU2
  -81.85,   # MU2 / MU3
  -80.55,   # MU3 / MU4
  bbox["xmax"] + 1
)

# create rectangular polygons first
make_rect <- function(xmin, xmax, ymin, ymax) {
  st_polygon(list(matrix(
    c(xmin, ymin,
      xmax, ymin,
      xmax, ymax,
      xmin, ymax,
      xmin, ymin),
    ncol = 2,
    byrow = TRUE
  )))
}

mu_rects <- st_sf(
  MU = c("MU1", "MU2", "MU3", "MU4"),
  geometry = st_sfc(
    make_rect(x_breaks[1], x_breaks[2], ymin, ymax),
    make_rect(x_breaks[2], x_breaks[3], ymin, ymax),
    make_rect(x_breaks[3], x_breaks[4], ymin, ymax),
    make_rect(x_breaks[4], x_breaks[5], ymin, ymax),
    crs = 4326
  )
)

# -----------------------------
# 3. clip MU rectangles to Lake Erie
# -----------------------------
mu_clip <- st_intersection(mu_rects, st_as_sf(lake_erie_union))

# -----------------------------
# 4. get label positions
# -----------------------------
mu_labels <- st_point_on_surface(mu_clip)

# -----------------------------
# 5. plot
# -----------------------------
p <- ggplot() +
  geom_sf(data = lake_erie_union, fill = "#d9ecff", color = "black", linewidth = 0.4) +
  geom_sf(data = mu_clip, fill = NA, color = "gray", linewidth = 1.1) +
  geom_sf_text(data = mu_labels, aes(label = MU), fontface = "bold", size = 5) +
  coord_sf(expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title = element_blank()
  )

print(p)

ggsave(
  "Figure/Final/lake_erie_management_units_map.png",
  p,
  width = 7,
  height = 4.5,
  dpi = 600
)

# -----------------------------
# 6. optional: save MU polygons as shapefile
# -----------------------------
st_write(mu_clip, "Figure/Final/lake_erie_management_units.shp", delete_layer = TRUE)