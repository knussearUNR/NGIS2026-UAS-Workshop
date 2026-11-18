################################################################################
# Activity2_PhotoScreening_SOLUTIONS.R   (instructor / answer key)
# Part 2 hands-on activity: read a mission from its metadata   (about 25 min)
#
# THE SCENARIO
#   A colleague hands you an SD card with 904 photos and says "I want to plot the
#   vegetation plot I flew on 28 May 2019."
#   Your PI wants a quick-look orthomosaic built ONLY from:
#     * the east-west survey lines (not the cross lines),
#     * frames taken at survey speed (no turns or hovering), and
#     * photos inside the plot boundary (ExifShiny/AOI_example.kml).
#   You need to deliver the photos plus a manifest that documents the choice.
#
# WHAT YOU WILL PRACTICE
#   * Reconstructing a mission from EXIF: flights, pattern, speed
#   * Writing selection rules as code and logging their effect
#   * Spatial selection with an AOI polygon
#   * Checking your script against the Shiny app
#
# HOW TO WORK
#   Set the working directory to the "Part2 ExifPhotoSorter" folder. Replace
#   each ___ with code or a value (R stops with an "unexpected input" error until
#   you do). Write answers on the lines marked ANSWER. A worked solution is in
#   Activity2_PhotoScreening_SOLUTIONS.R.
################################################################################

library(dplyr)
library(sf)
library(leaflet)


# ---- SETUP: load the metadata -------------------------------------------------
# To save time we start from the table read_exif() produced in the walkthrough
# (ExifShiny/DCIM/exif_raw.csv). If you want the real thing, uncomment:
#   library(exifr)
#   files <- list.files("ExifShiny/DCIM", pattern = "(?i)\\.jpe?g$", recursive = TRUE, full.names = TRUE)
#   exif_raw <- read_exif(files, tags = c("SourceFile", "DateTimeOriginal", "GPSLatitude",
#     "GPSLongitude", "RelativeAltitude", "SpeedX", "SpeedY", "SpeedZ", "GimbalYawDegree"))
exif_raw <- read.csv("ExifShiny/DCIM/exif_raw.csv")

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

exif <- exif_raw %>%
  mutate(
    file_name   = basename(SourceFile),
    time        = as.POSIXct(DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = "UTC"),
    lat         = to_num(GPSLatitude),
    lon         = to_num(GPSLongitude),
    alt_rel_m   = to_num(RelativeAltitude),
    speed_mps   = sqrt(to_num(SpeedX)^2 + to_num(SpeedY)^2 + to_num(SpeedZ)^2),
    heading_deg = to_num(GimbalYawDegree) %% 360,
    heading_sector = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")[
      floor(((heading_deg + 22.5) %% 360) / 45) + 1]
  ) %>%
  arrange(time)

nrow(exif)


# ---- Task 1: How long between photos? ------------------------------------------
# The camera fired on a timer. Tabulate the seconds between consecutive photos.
gap_sec <- c(0, diff(as.numeric(exif$time)))
head(sort(table(gap_sec), decreasing = TRUE))

# ANSWER: the usual interval is 2 s (638 of 903 gaps). The three very long gaps
#   are 324, 351 and 404 s (about 5.5 to 7 minutes): landing and battery swaps.
#   Gaps of 3 to 37 s inside a flight are the turns at the ends of lines.


# ---- Task 2: How many flights? -------------------------------------------------
# Split into flights wherever the gap is longer than gap_minutes.
gap_minutes <- 2
exif$flight <- cumsum(gap_sec > gap_minutes * 60) + 1
table(exif$flight)

# Now try gap_minutes = 0.5 and then 10. Re-run the two lines above each time.
# ANSWER: flights found with 0.5 = 5 , with 2 = 4 , with 10 = 1
# QUESTION: Why do 0.5 and 10 both give the wrong answer?
# ANSWER: 0.5 min (30 s) is shorter than the longest in-flight pause (37 s),
#   so one flight is cut in two. 10 min is longer than every battery swap
#   (max ~6.7 min), so all four flights merge into one. The threshold must sit
#   between the longest pause in a flight and the shortest gap between flights;
#   anywhere from about 1 to 5 minutes works for this mission.

# Put it back to 2 before you continue!
gap_minutes <- 2
exif$flight <- cumsum(gap_sec > gap_minutes * 60) + 1


# ---- Task 3: What pattern was flown? -------------------------------------------
# Cross-tabulate flight by heading sector.
table(exif$flight, exif$heading_sector)

# ANSWER: flights with east-west lines = 1 and 3 ; north-south lines = 2 and 4
# QUESTION: What kind of mission is this, and why would someone fly it?
# ANSWER: A cross-hatch (double-grid) survey flown as two blocks: map the
#   points by flight (or use the app's "Symbolize by Flight") and you see
#   flights 1 + 2 cover the EAST block and 3 + 4 the WEST block. Cross-hatch
#   grids add viewing angles, which improves 3D structure (canopy, terrain,
#   buildings) at the cost of twice the photos and processing time. For a flat
#   2D orthomosaic, one direction is usually enough, hence the PI's request.


# ---- Task 4: What speed was planned? -------------------------------------------
hist(exif$speed_mps, breaks = 30, col = "grey80", main = "Ground speed", xlab = "m/s")
exif %>% group_by(flight) %>% summarise(median_speed = median(speed_mps),
                                        slow_frames = sum(speed_mps < 4))

# ANSWER: planned survey speed is about 6 m/s (median 6.0 to 6.4 by flight);
#   each flight has ~23 to 30 slow frames (< 4 m/s), 103 in total.
# QUESTION: Photos every 2 s at that speed are how many meters apart?
# ANSWER: about 6.2 m/s * 2 s = ~12 m (the median measured spacing is 12.1 m).


# ---- Task 5: Build the PI's selection -------------------------------------------
# Fill in the three rules: east-west flights only, at least 5 m/s.
ew_flights <- c(1, 3)
min_speed  <- 5

selected <- exif %>%
  filter(flight %in% ew_flights,
         speed_mps >= min_speed)
nrow(selected)

# Spatial rule: keep only photos inside the plot boundary.
aoi <- st_transform(st_zm(st_read("ExifShiny/AOI_example.kml", quiet = TRUE)), 4326)
selected_sf <- st_as_sf(selected, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
final_sf <- selected_sf[lengths(st_intersects(selected_sf, aoi)) > 0, ]   # the polygon
nrow(final_sf)

# ANSWER: east-west flights: 453; after the speed rule: 403; inside the AOI: 251

# Plot it.
leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addPolygons(data = aoi, color = "yellow", weight = 2, fillOpacity = 0.05) %>%
  addCircleMarkers(data = st_as_sf(exif, coords = c("lon", "lat"), crs = 4326),
                   radius = 2, stroke = FALSE, fillColor = "grey", fillOpacity = 0.6) %>%
  addCircleMarkers(data = final_sf, radius = 4, stroke = FALSE,
                   fillColor = "red", fillOpacity = 0.9, popup = ~file_name)


# ---- Task 6: Document it --------------------------------------------------------
# Write a cull log and a manifest. (We skip copying the photos this time to save disk
# space; in real use add file.copy() exactly as in Step 9 of the walkthrough.)
out <- "Activity2_output_solution"
dir.create(out, showWarnings = FALSE)

cull_log <- data.frame(
  rule = c("all photos", "east-west flights", "speed >= min_speed", "inside AOI"),
  photos_kept = c(nrow(exif),
                  sum(exif$flight %in% ew_flights),
                  nrow(selected),
                  nrow(final_sf))                     # How many survived the AOI
)
cull_log
write.csv(cull_log, file.path(out, "cull_log.csv"), row.names = FALSE)

write.csv(st_drop_geometry(final_sf)[, c("SourceFile", "time", "flight", "lat", "lon",
                                         "alt_rel_m", "speed_mps", "heading_sector")],
          file.path(out, "export_manifest.csv"), row.names = FALSE)


# ---- Task 7: Check yourself with the Shiny app ----------------------------------
# Launch the app:   shiny::runApp("ExifShiny")
#   1. Click "Load EXIF data" (the default folder is ExifShiny/DCIM).
#   2. Set the Speed slider's lower end to 5.
#   3. Under Flights, uncheck the two north-south flights.
#   4. Spatial selection: "Use an uploaded KML polygon", upload AOI_example.kml.
#   5. Read "Files currently selected" in the sidebar.
# ANSWER: the app selects 251 photos, the same as Task 5. (The app takes
#   heading from FlightYawDegree and altitude from GPSAltitude rather than
#   GimbalYawDegree / RelativeAltitude, but on this aircraft they agree.)
# QUESTION: Switch "Symbolize points by" to Direction, then Flight. What do you see?
# ANSWER: Direction shows rows of E/W points interleaved with columns of N/S
#   points (the cross-hatch). Flight shows two blocks: flights 1-2 east,
#   flights 3-4 west, each block covered once in each direction.


# ---- If you want to dig deeper -----------------------------------------------
# 1. Turn Task 5 into a function select_photos(exif, flights, min_speed, aoi)
#    that returns final_sf, and use it to build a second set for the N-S lines.
# 2. Frames near the ends of each line are fast but still turning. Drop the
#    first and last 2 frames of every straight run (hint: group_by(flight),
#    then use lag()/lead() on heading_sector to find where a run starts).
# 3. Point the code at a folder of your own drone photos. Which tags exist?
#    (names(exifr::read_exif(one_file)))  What changes would the code need?
