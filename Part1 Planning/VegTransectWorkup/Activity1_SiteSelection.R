################################################################################
# Activity1_SiteSelection.R
# Part 1 hands-on activity: tune a UAS sampling design   (about 20 minutes)
#
# THE SCENARIO
#   Your field crew has a 5-day window and can fly at most 10 launch sites.
#   Your job is to choose design parameters that reach as many vegetation
#   transects as possible while staying inside that budget, then export the
#   KMLs for the flight planner.
#
# WHAT YOU WILL PRACTICE
#   * Turning a script into a reusable function
#   * Changing one parameter at a time and recording the effect
#   * Comparing two input layers (main roads vs. main + rough roads)
#   * Exporting a final design to KML
#
# HOW TO WORK
#   Set the working directory to the VegTransectWorkup folder, run the SETUP
#   section once, then work through Tasks 1 to 5. Lines marked  # TODO  need
#   you to fill something in: replace each ___ with a value (R stops with
#   an "unexpected input" error until you do). Write your answers in the comment lines marked
#   ANSWER. A worked solution is in Activity1_SiteSelection_SOLUTIONS.R, but
#   try it yourself first.
################################################################################

library(sf)
rm(list = ls())


# ---- SETUP: read the data once ------------------------------------------------
# All input layers live in the `data` folder next to this script. If R cannot
# find it, the working directory is wrong: use Session > Set Working Directory
# > To Source File Location, then run again.
data_dir <- 'data'
if (!dir.exists(data_dir)) stop("Can't find the 'data' folder. Set the working ",
                                "directory to the folder that holds this script.")
roads_main  <- st_read(file.path(data_dir, 'Searchlight_Roads', 'Searchlight_Roads.shp'), quiet = TRUE)
roads_main <- st_zm(roads_main, drop = TRUE, what = 'ZM')

roads_rough <- st_read(file.path(data_dir, 'Searchlight_Rough_Roads', 'Searchlight_Rough_Roads.shp'), quiet = TRUE)
roads_rough <- st_zm(roads_rough, drop = TRUE, what = 'ZM')

transects   <- st_read(file.path(data_dir, 'Searchlight_Veg_Transects_Hypothetical', 'Searchlight_Veg_Transects_Hypothetical.shp'), quiet = TRUE)

# Both road layers must share the transects' CRS (UTM 11N, meters).
roads_rough <- st_transform(roads_rough, st_crs(transects))


# ---- SETUP: the whole design as one function ---------------------------------
# This is Steps 2 to 7 of Transect2KML_NGIS.R packed into a function, so new
# designs only need one line of code. Read through and match each block to the step
# you saw in the main script.
#
# Arguments
#   roads          sf lines that a launch point must sit on
#   linedist       spacing of candidate points along the roads (m)
#   losrad         line-of-sight radius around a launch point (m)
#   tranz_buffer   half-width of the transect image footprint (m)
#   min_transects  minimum transects a site must reach
#   min_sep_m      minimum distance between selected sites (m)
#
# Returns a list with
#   summary    one-row data frame: parameters, n_sites, n_transects_covered
#   sites      the selected launch circles (sf)
#   transects  the buffered transects with a group_id column (0 = not covered)
site_design <- function(roads, linedist = 1000, losrad = 700, tranz_buffer = 30,
                        min_transects = 3, min_sep_m = 5000) {

  # Step 2: candidate points along the roads
  pts <- st_line_sample(suppressWarnings(st_cast(roads, "LINESTRING")),
                        density = 1 / linedist)
  pts <- st_cast(st_sf(geometry = st_combine(pts)), "POINT")

  # Step 3 and 4: line-of-sight circles and transect footprints
  circles <- st_buffer(pts, dist = losrad)
  tz <- st_buffer(transects, dist = tranz_buffer,
                  joinStyle = 'MITRE', endCapStyle = "SQUARE")

  # Step 5: score each circle
  circles$n_transects <- lengths(st_intersects(circles, tz))

  # Step 6: greedy, spatially balanced selection
  cand <- circles[circles$n_transects >= min_transects, ]
  cand <- cand[order(cand$n_transects, decreasing = TRUE), ]
  ctr  <- st_centroid(st_geometry(cand))
  keep <- if (nrow(cand) > 0) 1 else integer(0)
  for (i in seq_len(nrow(cand))[-1]) {
    if (all(as.numeric(st_distance(ctr[i], ctr[keep])) >= min_sep_m)) keep <- c(keep, i)
  }
  sites <- cand[keep, ]
  sites$group_id <- seq_len(nrow(sites))

  # Step 7: tag transects with the site that covers them
  hit <- st_intersects(tz, sites)
  tz$group_id <- vapply(hit, function(h) if (length(h)) sites$group_id[h[1]] else 0L, integer(1))

  list(
    summary = data.frame(linedist, losrad, tranz_buffer, min_transects, min_sep_m,
                         n_sites = nrow(sites),
                         n_transects_covered = sum(tz$group_id > 0)),
    sites = sites,
    transects = tz
  )
}


# ---- Task 1: Reproduce the baseline -------------------------------------------
# Run the function with its default settings on the main roads. You should get
# the same answer as the full script (10 sites).
base <- site_design(roads_main)
base$summary

# ANSWER: sites = ____   transects covered = ____   (out of 294)


# ---- Task 2: One knob at a time ----------------------------------------------
# The May 2026 field plan used a 3 km separation instead of 5 km.
# Change ONLY min_sep_m and run it again.
sep3 <- site_design(roads_main, min_sep_m = ___)   # TODO: replace ___ with 3000
sep3$summary

# ANSWER: sites = ____   transects covered = ____
# QUESTION: Why does a smaller separation give MORE sites?
# ANSWER:


# ---- Task 3: Sweep a parameter ------------------------------------------------
# Instead of editing and re-running by hand, loop over several values and
# stack the summaries into one table. Fill in the separations to try.
seps <- c(___)   # TODO: replace ___ with several values, e.g. 2000, 3000, 4000, 4500, 5000, 6000
sweep <- do.call(rbind, lapply(seps, function(s) site_design(roads_main, min_sep_m = s)$summary))
sweep

# Plot the trade-off: more sites cost more field days but cover more transects.
plot(sweep$n_sites, sweep$n_transects_covered, type = 'b', pch = 19,
     xlab = 'Number of launch sites', ylab = 'Transects covered',
     main = 'Sites vs. coverage')
text(sweep$n_sites, sweep$n_transects_covered, labels = sweep$min_sep_m, pos = 4, cex = 0.8)
abline(v = 10, lty = 2, col = 'red')   # our 10-site budget

# QUESTION: Which separation gives the most transects with 10 or fewer sites?
# ANSWER:


# ---- Task 4: Better roads or a better pilot? ---------------------------------
# Two ways to reach more transects:
#   (a) allow launches from rough roads too (more candidate points), or
#   (b) a bigger line-of-sight radius (e.g. a visual observer, or open terrain).
# Combine the two road layers with rbind() on their geometry, then compare.
roads_all <- st_sf(geometry = c(st_geometry(roads_main), st_geometry(roads_rough)))

opt_a <- site_design(roads_all,  min_sep_m = 3000)     # more roads
opt_b <- site_design(roads_main, min_sep_m = 3000, losrad = ___)   # TODO: replace ___ with 1000
rbind(main_3km = sep3$summary, rough_roads = opt_a$summary, losrad_1000 = opt_b$summary)

# QUESTION: Which change helps more? What is the practical cost of each?
# ANSWER:


# ---- Task 5: Pick your design and export it -----------------------------------
# Choose the settings you would actually fly (10 sites or fewer), then write
# one KML per site. Open one in Google Earth (or drag it into QGIS) to check it.
my_design <- site_design(roads_main, min_sep_m = 3000)   # TODO: your choice
my_design$summary

outdir <- 'Activity1_output'
dir.create(outdir, showWarnings = FALSE)
for (g in my_design$sites$group_id) {
  kml_path <- file.path(outdir, paste0('site_', g, '.kml'))
  write_sf(my_design$transects[my_design$transects$group_id == g, ], kml_path,
           driver = 'KML', delete_dsn = file.exists(kml_path))
}
list.files(outdir)


# ---- Stretch goals (if you finish early) ---------------------------------------
# 1. The greedy loop favors the single best site first. Re-sort the candidates
#    at random (sample()) instead of by n_transects. How much coverage is lost?
# 2. Replace the 700 m circle with a viewshed from a DEM (terra::viewshed()).
#    Which launch sites lose transects behind ridges?
# 3. Add a column with each site's driving distance from camp and break ties
#    in favor of closer sites.
