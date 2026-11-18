################################################################################
# Transect2KML_NGIS.R
# Part 1 of the NVGIS 2026 UAS workshop: sample site selection and flight prep
#
# Ken Nussear, Department of Geography, University of Nevada, Reno
#
# GOAL
#   We have 294 vegetation transects scattered around Searchlight, NV, and a
#   network of roads. We want a small set of UAS launch sites that
#     (a) sit on a road, so we can drive the aircraft and crew to them,
#     (b) can "see" as many transects as possible from one spot
#         (visual line of sight, VLOS), and
#     (c) are spread out across the study area (spatially balanced).
#   For each launch site we then export the transects it covers as a KML that
#   drops straight into flight planning software (UGCS, DJI Pilot, etc.).
#
# WORKFLOW 
#   Step 1  Read the roads and transects
#   Step 2  Drop candidate launch points along the roads every `linedist` m
#   Step 3  Buffer each candidate by the line-of-sight radius `losrad`
#   Step 4  Buffer each transect to make an image-capture footprint
#   Step 5  Count how many transect footprints each candidate circle touches
#   Step 6  Greedy selection: best candidates first, at least `min_sep` apart
#   Step 7  Tag each transect with the launch site (group) that covers it
#   Step 8  Export the launch points
#   Step 9  Export one KML per launch site for flight planning
#
# INPUTS  
#   data/Searchlight_Roads/                       road network (lines, BLM GTLF)
#   data/Searchlight_Veg_Transects_Hypothetical/  vegetation transects (lines)
#   (data/Searchlight_Rough_Roads/ is used in Activity 1)
#   The roads are real public BLM roads. The transects and rough roads are
#   HYPOTHETICAL, made up for teaching (see data/make_hypothetical_data.R).
#   Both layers are in WGS 84 / UTM zone 11N (EPSG:32611), so distances and
#   buffer sizes below are in METERS. If you use your own data, project it to
#   a metric CRS first (see the note in Step 1).
#
# OUTPUTS  (written to the `outdir` folder, all GeoPackage unless noted)
#   rdz_pts.gpkg                 candidate launch points
#   rdz_pts_buff.gpkg            line-of-sight circles around every candidate
#   tranz_buff_30.gpkg           buffered transect footprints
#   rdz_pts_buff_counts.gpkg     circles with a count of transects inside
#   rdz_sampled_5km.gpkg         the selected launch circles
#   rdz_sampled_pts.gpkg         the selected launch POINTS (load these into a
#                                GPS or field map to navigate to each site)
#   tranz_grouped.gpkg           all transects, tagged with group_id
#   group_N/tranz_group_N.kml    transects for launch site N (for UGCS)
#
# PACKAGES: sf (vector spatial data), units (distances with units attached)
#
# HOW TO RUN
#   Open this file in RStudio, set the working directory to the folder that
#   holds this script (Session > Set Working Directory > To Script File
#   Location), then run it line by line with Ctrl+Enter (Cmd+Enter on a Mac)
#   so you can look at each object as it is created.
################################################################################

library(sf)       # simple features: read, buffer, intersect, and write vector data
rm(list = ls())   # start with an empty workspace to remove old objects 


# ---- Parameters --------------------------------------------------------------
# Everything you would want to tune lives here, so you can change the design
# without hunting through the code. All distances are in meters.

linedist      <- 1000   # spacing of candidate launch points along the roads
losrad        <- 700    # line-of-sight radius: how far the pilot can keep the
                        # drone in view from the launch point
tranz_buffer  <- 30     # half-width of the image footprint around a transect
                        # (30 m each side gives a 60 m wide capture strip)
min_transects <- 3      # a launch site must reach at least this many transects
                        # to be worth driving to
min_sep_m     <- 5000   # minimum distance between selected launch sites;
                        # larger = fewer, more spread-out sites
outdir        <- 'UAS_KML_Files'   # where output files are written

dir.create(outdir, showWarnings = FALSE)   # make the output folder if needed

# All input layers live in the `data` folder next to this script. If R cannot
# find it, the working directory is wrong: use Session > Set Working Directory
# > To Source File Location, then run again.
data_dir <- 'data'


# ---- Step 1: Read the roads and the vegetation transects ---------------------
# st_read() reads any vector format GDAL understands (shapefile, GeoPackage,
# KML, GeoJSON, file geodatabase layer, ...) and returns an sf data frame:
# a normal data frame with one extra column, `geometry`, holding the shapes.
rdz   <- st_read(file.path(data_dir, 'Searchlight_Roads', 'Searchlight_Roads.shp'))
tranz <- st_read(file.path(data_dir, 'Searchlight_Veg_Transects_Hypothetical', 'Searchlight_Veg_Transects_Hypothetical.shp'))

# Road layers are often a mix of 2D (XY) and 3D (XYZ) lines, which is common
# when routes come from a GPS. Mixed dimensions break some sf/GDAL operations
# (st_line_sample() in Step 2 fails with "Length of input WKB is too small").
# We only need horizontal positions here, so drop the Z (and M) values.
rdz <- st_zm(rdz, drop = TRUE, what = 'ZM')

# Always check the coordinate reference system (CRS) before measuring anything.
# Both layers should report "WGS 84 / UTM zone 11N" with units of meters.
st_crs(rdz)$input
st_crs(tranz)$input

# NOTE for your own data: if a layer is in longitude/latitude (EPSG:4326),
# project it to a metric CRS before buffering, for example:
#   rdz   <- st_transform(rdz,   32611)   # UTM 11N covers most of Nevada
#   tranz <- st_transform(tranz, 32611)

# Quick look: roads in grey, transects in green.
plot(st_geometry(rdz), col = 'grey50')
plot(st_geometry(tranz), col = 'darkgreen', lwd = 2, add = TRUE)


# ---- Step 2: Candidate launch points along the roads -------------------------
# The road layer is stored as MULTILINESTRINGs (one record can hold several
# disconnected pieces). st_line_sample() works on single lines, so we first
# split every multi-part road into its individual LINESTRING pieces.
# (The warning about "repeating attributes" is expected and harmless.)
head(rdz)
rdz_lines <- st_cast(rdz, "LINESTRING")
head(rdz_lines)

# Sample points along roads at a density of 1 point per `linedist` meters of road.
# The number of points on each piece is round(length / linedist), so pieces
# shorter than half of `linedist` don't get points.
rdz_pts <- st_line_sample(rdz_lines, density = 1 / linedist)

# st_line_sample() returns one MULTIPOINT per road piece. Combine them into a
# single sf object and then explode it into one row per POINT.
rdz_pts <- st_sf(geometry = st_combine(rdz_pts))
head(rdz_pts)
rdz_pts <- st_cast(rdz_pts, "POINT")
head(rdz_pts)

# Give every candidate a permanent ID. We will use it later (Step 8) to find the original point for each selected circle.
rdz_pts$pt_id <- seq_len(nrow(rdz_pts))
nrow(rdz_pts)   # how many candidate launch points did we get?

write_sf(rdz_pts, file.path(outdir, 'rdz_pts.gpkg'))


# ---- Step 3: Line-of-sight circles around each candidate ------------------
# Buffering a point by a distance makes a circle polygon.
# The pt_id column is carried along with the geometry.
rdz_pts_buff <- st_buffer(rdz_pts, dist = losrad)
write_sf(rdz_pts_buff, file.path(outdir, 'rdz_pts_buff.gpkg'))


# ---- Step 4: Image-capture footprints around each transect ----------------
# Each transect is just a line. We buffer it to the strip the drone images.
#   endCapStyle = "SQUARE" extends the strip past the ends of the line so the
#                          transect end points are fully covered.
#   joinStyle   = "MITRE"  keeps sharp corners instead of rounded ones, which
#                          makes a cleaner rectangle for flight planning.
tranz_buff_30 <- st_buffer(tranz, dist = tranz_buffer,
                           joinStyle = 'MITRE', endCapStyle = "SQUARE")
head(tranz_buff_30)   # the attributes are unchanged; only the geometry grew

write_sf(tranz_buff_30, file.path(outdir, 'tranz_buff_30.gpkg'))


# ---- Step 5: Score each candidate by the transects it can reach  -----------
# st_intersects(A, B) returns, for every feature in A, the row numbers of the
# features in B that touch it. lengths() then counts them.
# Result: the number of transect footprints each circle overlaps.
rdz_pts_buff$n_transects <- lengths(st_intersects(rdz_pts_buff, tranz_buff_30))

table(rdz_pts_buff$n_transects)   # how are the scores distributed?

write_sf(rdz_pts_buff, file.path(outdir, 'rdz_pts_buff_counts.gpkg'))


# ---- Step 6: Choose launch sites (greedy, spatially balanced) -------------
# Rule: take the best remaining candidate, then skip any candidate closer than
# `min_sep_m` to a site we already took. Repeat down the list.
# This is a simple "greedy" version of a spatially balanced sample: it favors
# high-value sites but forces them apart so the sample covers the study area.

# 6a. Drop candidates that reach too few transects to justify a flight.
rdz_sorted <- rdz_pts_buff[rdz_pts_buff$n_transects >= min_transects, ]

# 6b. Sort best first (most transects at the top).
rdz_sorted <- rdz_sorted[order(rdz_sorted$n_transects, decreasing = TRUE), ]

# 6c. Measure separation between circle CENTERS.
#   st_distance() used between two polygons returns the edge-to-edge gap.)
rdz_sorted_pts <- st_centroid(st_geometry(rdz_sorted))

# 6d. Attach units so the comparison below is explicitly meter-to-meter.
min_sep <- units::set_units(min_sep_m, "m")

# 6e. The greedy loop. `selected` holds the row numbers we keep; the best
#     candidate (row 1) is always kept. seq_len(...)[-1] visits rows 2..n and
#     is safe even if only 0 or 1 candidates passed the filter in 6a.
selected <- 1 # seed selected
for (i in seq_len(nrow(rdz_sorted))[-1]) {
  # distances from candidate i to every site already selected
  dists <- st_distance(rdz_sorted_pts[i], rdz_sorted_pts[selected])
  # keep candidate i only if it is far enough from ALL of them
  if (all(dists >= min_sep)) {
    selected <- c(selected, i)
  }
}

# 6f. Keep the winners and number them 1, 2, 3, ... in priority order.
#     group_id is the label used for each flight from here on.
rdz_sampled <- rdz_sorted[selected, ]
rdz_sampled$group_id <- seq_len(nrow(rdz_sampled))

nrow(rdz_sampled)              # number of launch sites
sum(rdz_sampled$n_transects)   # transects reachable from those sites

write_sf(rdz_sampled, file.path(outdir, 'rdz_sampled_5km.gpkg'))


# ---- Step 7: Tag each transect with the launch site that covers it ---------
# Every transect starts in group 0 ("not covered by any selected site").
# For each transect, find which selected circles it touches and copy that
# site's group_id. Because selected sites are at least `min_sep_m` apart and
# circles are only 2 * losrad wide, a transect normally touches one circle;
# if it ever touches two, it is assigned to the first (higher priority) one.

tranz_buff_30$group_id <- 0
intersects_matrix <- st_intersects(tranz_buff_30, rdz_sampled)
for (i in seq_len(nrow(tranz_buff_30))) {
  if (length(intersects_matrix[[i]]) > 0) {
    tranz_buff_30$group_id[i] <- rdz_sampled$group_id[intersects_matrix[[i]][1]]
  }
}

table(tranz_buff_30$group_id)   # transects per group (group 0 = not flown)

write_sf(tranz_buff_30, file.path(outdir, 'tranz_grouped.gpkg'))


# ---- Step 8: Export the selected launch POINTS -------------------------------
# rdz_sampled holds circles. For navigation we want the road point at the
# center of each circle. pt_id (created in Step 2) links each circle back to
# its original point, so we pull those rows and copy the scores across.
rdz_sampled_pts <- rdz_pts[match(rdz_sampled$pt_id, rdz_pts$pt_id), ]
rdz_sampled_pts$n_transects <- rdz_sampled$n_transects
rdz_sampled_pts$group_id    <- rdz_sampled$group_id

write_sf(rdz_sampled_pts, file.path(outdir, 'rdz_sampled_pts.gpkg'))

# Also write them as KML so they open directly in Google Earth or on a phone.
# (delete_dsn replaces the file on re-runs; KML files cannot be overwritten in place.)
kml_path <- file.path(outdir, 'rdz_sampled_pts.kml')
write_sf(rdz_sampled_pts, kml_path, driver = 'KML', delete_dsn = file.exists(kml_path))


# ---- Step 9: One KML per launch site, for flight planning --------------------
# UGCS (and most flight planners) import KML polygons as survey areas.
# We write each group's transect footprints to its own folder, e.g.
#   UAS_Files/group_3/tranz_group_3.kml
# GDAL's KML driver reprojects to WGS84 longitude/latitude automatically. Group 0 (transects no site can reach) is
# skipped because those transects are not flown.
unique_groups <- sort(unique(tranz_buff_30$group_id))
unique_groups <- unique_groups[unique_groups != 0]

for (group in unique_groups) {
  group_dir <- file.path(outdir, paste0('group_', group))
  dir.create(group_dir, showWarnings = FALSE, recursive = TRUE)
  group_tranz <- tranz_buff_30[tranz_buff_30$group_id == group, ]
  kml_path <- file.path(group_dir, paste0('tranz_group_', group, '.kml'))
  write_sf(group_tranz, kml_path, driver = 'KML',
           delete_dsn = file.exists(kml_path))   # replace the file on re-runs
}


# ---- Final check: map the design ---------------------------------------------
# Grey roads, selected launch circles in red, covered transects in green,
# uncovered transects in light grey.
plot(st_geometry(rdz), col = 'grey70', main = 'Selected UAS launch sites')
plot(st_geometry(tranz_buff_30[tranz_buff_30$group_id == 0, ]),
     col = 'grey85', border = 'grey85', add = TRUE)
plot(st_geometry(tranz_buff_30[tranz_buff_30$group_id > 0, ]),
     col = 'darkgreen', border = 'darkgreen', add = TRUE)
plot(st_geometry(rdz_sampled), border = 'red', lwd = 2, add = TRUE)
text(st_coordinates(rdz_sampled_pts), labels = rdz_sampled_pts$group_id,
     col = 'red', font = 2)

# OPTIONAL: an interactive map on satellite imagery (needs the mapview package).
# mapview's default first basemap (CartoDB) now needs an API key and shows
# "API KEY REQUIRED" tiles, so we name basemaps that need no key. The first
# one is shown; switch between them with the layers button on the map.
  library(mapview)
  basemaps <- c('Esri.WorldImagery', 'Esri.WorldTopoMap', 'OpenTopoMap')
  design_map <- mapview(tranz_buff_30,map.types = basemaps) + mapview(rdz_sampled, zcol = 'n_transects', layer.name = 'Transects per launch site',  map.types = basemaps)
  print(design_map)   # print() so the map also shows when the script is sourced
