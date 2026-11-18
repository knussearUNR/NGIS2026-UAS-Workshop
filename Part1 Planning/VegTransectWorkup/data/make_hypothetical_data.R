################################################################################
# make_hypothetical_data.R
# Builds the Part 1 example data: a public road network near Searchlight, NV,
# and a set of HYPOTHETICAL vegetation transects. You do not need to run this
# for the workshop; it documents where the data came from.
#
# Roads: BLM Ground Transportation Linear Features (GTLF), public display
#   service, layer 1 "Roads Managed for Limited Public Motorized Use",
#   Nevada segments inside -115.12, 35.30, -114.72, 35.62 (lon/lat).
#   https://gis.blm.gov/arcgis/rest/services/transportation/BLM_Natl_GTLF_Public_Display/MapServer
#   All of these segments are graded primitive roads, so they form the main
#   road layer.
# Rough roads: made up. A few short two-track spurs that leave the main roads
#   toward plots that are far from any road.
# Transects: made up. Survey plots are dropped at random in the road area,
#   each holding a handful of 50 m transects, plus plot T95, the site flown in
#   the Part 2 example. They do not represent real survey locations.
# The layers in this folder are the committed copy. Running the script again
# downloads the roads fresh, so results can differ slightly if BLM has updated
# the service since November 2026.
#
# Usage (from this folder): Rscript make_hypothetical_data.R
################################################################################

library(sf)

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) args[1] else 'data'
n_plots  <- if (length(args) >= 2) as.integer(args[2]) else 55
seed     <- if (length(args) >= 3) as.integer(args[3]) else 1
spread_m <- if (length(args) >= 5) as.numeric(args[5]) else 500   # scatter of transects around a plot centre
n_spurs  <- 4                                                       # hypothetical rough roads
set.seed(seed)

# ---- Roads -------------------------------------------------------------------
url <- paste0('https://gis.blm.gov/arcgis/rest/services/transportation/',
              'BLM_Natl_GTLF_Public_Display/MapServer/1/query?where=1%3D1',
              '&geometry=-115.12,35.30,-114.72,35.62&geometryType=esriGeometryEnvelope',
              '&inSR=4326&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=4326&f=geojson')
gtlf <- if (length(args) >= 4) st_read(args[4], quiet = TRUE) else st_read(url, quiet = TRUE)
gtlf <- gtlf[gtlf$ADMIN_ST == 'NV', ]
gtlf <- st_transform(st_zm(gtlf, drop = TRUE, what = 'ZM'), 32611)

roads <- st_sf(
  route_name = gtlf$ROUTE_PRMRY_NM,
  surface    = tools::toTitleCase(tolower(gtlf$OBSRVE_SRFCE_TYPE)),
  use_class  = gtlf$OBSRVE_ROUTE_USE_CLASS,
  length_m   = round(as.numeric(st_length(gtlf)), 1),
  geometry   = st_geometry(gtlf)
)
roads_main <- roads

# ---- Hypothetical transects --------------------------------------------------
# Plots go anywhere in the road area except the town of Searchlight itself.
town <- st_transform(st_sfc(st_point(c(-114.9194, 35.4655)), crs = 4326), 32611)
area <- st_difference(st_convex_hull(st_union(roads)), st_buffer(town, 2000))
plot_centres <- st_coordinates(st_sample(area, n_plots))

lines <- list(); ids <- character(); plot_ids <- character()
for (p in seq_len(n_plots)) {
  n_tr <- sample(2:8, 1)
  heading <- runif(1, 0, pi)                      # plots share a rough heading
  for (t in seq_len(n_tr)) {
    start <- plot_centres[p, ] + rnorm(2, 0, spread_m)
    angle <- heading + rnorm(1, 0, 0.3)
    end   <- start + 50 * c(cos(angle), sin(angle))
    lines[[length(lines) + 1]] <- st_linestring(rbind(start, end))
    plot_ids <- c(plot_ids, sprintf('H%02d', p))
    ids      <- c(ids, sprintf('H%02d-%02d', p, t))
  }
}
transects <- st_sf(
  tran_id     = ids,
  plot_id     = plot_ids,
  note        = 'Hypothetical, for teaching only',
  length_m    = 50,
  geometry    = st_sfc(lines, crs = 32611)
)

# Plot T95: the seven transects of the Part 2 example flight. Their layout
# matches the photos in Part 2 (which were moved here by the same offset),
# so Part 1 can pick this site and Part 2 can screen the photos over it.
# Columns: x_start, x_end, y_start, y_end (UTM 11N, m).
t95 <- rbind(
  'W2-78'  = c(680487.9, 680437.2, 3920138.3, 3920136.9),
  'W2-43'  = c(680691.9, 680645.6, 3919343.4, 3919339.9),
  'U2-41'  = c(681330.9, 681329.5, 3920290.6, 3920339.2),
  'U2-75'  = c(681218.2, 681215.3, 3919476.2, 3919524.3),
  'U2-180' = c(681247.7, 681250.3, 3919854.0, 3919902.8),
  'U2-136' = c(681539.0, 681539.7, 3919625.9, 3919677.3),
  'U2-284' = c(681064.4, 681066.5, 3919098.2, 3919147.3)
)
t95_lines <- lapply(seq_len(nrow(t95)), function(i) st_linestring(matrix(t95[i, ], ncol = 2)))
t95_sf <- st_sf(tran_id = rownames(t95), plot_id = 'T95',
                note = 'Hypothetical, for teaching only (Part 2 example site)',
                geometry = st_sfc(t95_lines, crs = 32611))
t95_sf$length_m <- round(as.numeric(st_length(t95_sf)), 1)
transects <- rbind(transects, t95_sf[, names(transects)])

# ---- Hypothetical rough roads -------------------------------------------------
# Pick plots 1.5 to 3 km from the nearest main road and run a two-track from
# that road to about 300 m short of the plot, with a bend half way.
main_union <- st_union(st_geometry(roads_main))
centres_sf <- st_sfc(lapply(seq_len(n_plots), function(i) st_point(plot_centres[i, ])), crs = 32611)
dist_road  <- as.numeric(st_distance(centres_sf, main_union))
far <- which(dist_road > 1500 & dist_road < 3000)
far <- far[order(dist_road[far], decreasing = TRUE)][seq_len(min(n_spurs, length(far)))]
spurs <- lapply(far, function(i) {
  link <- st_coordinates(st_nearest_points(centres_sf[i], main_union))
  from <- link[2, 1:2]; to <- link[1, 1:2]
  len  <- sqrt(sum((to - from)^2))
  to   <- from + (to - from) * (len - 300) / len
  bend <- (from + to) / 2 + c(-1, 1) * rev(to - from) / len * runif(1, -200, 200)
  st_linestring(rbind(from, bend, to))
})
roads_rough <- st_sf(
  route_name = sprintf('Hypothetical two-track %d', seq_along(spurs)),
  surface    = 'Natural',
  use_class  = '4WD (hypothetical)',
  geometry   = st_sfc(spurs, crs = 32611)
)
roads_rough$length_m <- round(as.numeric(st_length(roads_rough)), 1)

# ---- Write -------------------------------------------------------------------
write_layer <- function(x, name) {
  d <- file.path(out_dir, name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  st_write(x, file.path(d, paste0(name, '.shp')), delete_dsn = file.exists(file.path(d, paste0(name, '.shp'))), quiet = TRUE)
}
write_layer(roads_main,  'Searchlight_Roads')
write_layer(roads_rough, 'Searchlight_Rough_Roads')
write_layer(transects,   'Searchlight_Veg_Transects_Hypothetical')
cat(sprintf('main roads %d (%.0f km), rough roads %d (%.0f km), transects %d in %d plots\n',
            nrow(roads_main), sum(roads_main$length_m) / 1000,
            nrow(roads_rough), sum(roads_rough$length_m) / 1000,
            nrow(transects), n_plots))
