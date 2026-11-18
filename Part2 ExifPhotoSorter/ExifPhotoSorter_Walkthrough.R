################################################################################
# ExifPhotoSorter_Walkthrough.R
# Part 2 of the NVGIS 2026 UAS workshop: screening drone images with EXIF data
#
# Ken Nussear, Department of Geography, University of Nevada, Reno
#
# THE PROBLEM
#   A mission returns thousands of image files, but only some belong in the
#   photogrammetry block. Frames from the ground before take-off, climb-out,
#   battery swaps, end-of-line turns, and transit legs have the wrong height,
#   attitude, or motion, and many fall outside the area we actually care
#   about. Feeding them all to Metashape / Pix4D / OpenDroneMap costs hours and
#   can degrade the orthomosaic. Deleting them by hand is slow, tedious and not
#   repeatable.
#
# THE IDEA
#   Images already carry a flight log in their metadata (EXIF and XMP):
#   camera, position, time, altitude, and (for some cameras) velocity and
#   attitude. Read it into one data frame, look at it, and select images with
#   explicit, documented rules. Then copy the keepers and write a manifest
#   that records exactly which source files went into the model.
#
# THE WORKING EXAMPLE follows on from Part 1. Launch site 2 in the Part 1
#   design (our field site T95) covers seven vegetation transects. In this
#   public copy the photos and transects were moved to the Searchlight, NV
#   example area: positions are shifted, heights above ground are unchanged.
#   We flew it on 15 May 2026 with a MicaSense RedEdge-P multispectral
#   camera (1.5 cm ground sample distance):
#     T95_15cm_15May26_raw/    the card, exactly as it came off the camera
#     tranz_group_95.kml       the 7 transect footprints from Part 1
#     T95_W2-78.kml            one of those transects, W2-78: the AOI we
#                              subset and export in the demo
#   The script also runs unchanged on the DJI class example
#   (img_dir <- "ExifShiny/DCIM"): it reads the camera make from the EXIF and
#   picks the matching tags (Step 2, `camera`).
#
#   Step 0  Setup: packages, folders, settings
#   Step 1  Find the image files (and count them at each stage)
#   Step 2  Read the metadata with exifr; detect the camera
#   Step 3  Clean it up: to one standard camera agnostic table
#   Step 4  Split the images into flights
#   Step 5  Derive parameters the camera doesn't record (height, speed, heading)
#   Step 5b Height above GROUND from a terrain model (elevatr)
#   Step 6  Explore: what did the mission actually do?
#   Step 7  Attribute filters and the cull log
#   Step 8  Make it spatial; keep images over the Part 1 transects
#   Step 9  Export: copy whole captures, write a manifest and GIS layers
#   Step 10 From script to app
#   Extras  Renaming, missing GPS
#
# PACKAGES
#   exifr     reads metadata through ExifTool (a free command-line program)
#   dplyr     data wrangling (filter, mutate, group_by, summarise)
#   sf        spatial vector data (points, polygons, intersections)
#   leaflet   interactive web maps
#   leaflet.extras, shiny, miniUI   only for drawing an AOI (aoi_source = "draw")
#
# HOW TO RUN
#   Open in RStudio, set the working directory to the "Part2 ExifPhotoSorter"
#   folder (Session > Set Working Directory > To Source File Location), and
#   run line by line with Ctrl+Enter (Cmd+Enter on a Mac).
################################################################################


# ---- Step 0: Setup ------------------------------------------------------------

# Install anything that is missing (only needs to happen once per computer).
pkgs <- c("exifr", "dplyr", "sf", "leaflet", "leaflet.extras", "shiny", "miniUI", "elevatr")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

library(exifr)
library(dplyr)
library(sf)
library(leaflet)
library(elevatr)     # ground elevation for each image (Step 5b)

# exifr needs the ExifTool program. If read_exif() below complains that it
# cannot find ExifTool, run this once; it downloads a private copy for R:
#   exifr::configure_exiftool()

# Folders and settings. Everything you might change lives here.
img_dir  <- "T95_15cm_15May26_raw"   # the raw card; never written to
aoi_source <- "file"                 # "file" = read aoi_file; "draw" = draw it on a map
aoi_file <- "T95_W2-78.kml"          # transect W2-78; "tranz_group_95.kml" = all 7
out_dir  <- "T95_output"             # everything this script writes goes here

# Multispectral cameras write one file per band for every capture
# (IMG_0123_1.tif ... IMG_0123_6.tif). All bands of a capture share the same
# time and GPS position, so ONE band is enough for screening. Bands 1 to 5 of
# the RedEdge-P are about 3 MB each; band 6 (panchromatic) is about 10 MB, so
# a 1 to 5 band reads faster and tells us exactly the same thing.
use_band <- 1                        # ignored for single-file cameras (DJI)
# The workshop copy of T95 keeps band 1 only (1,543 files, 4.7 GB) so it is
# quick to share. The full card held all 6 bands: 9,133 files, 39.7 GB. The
# script runs the same way on either; only the counts change.

dir.create(out_dir, showWarnings = FALSE)

# A running tally of how many files we are dealing with at each stage.
# add_count() appends one row; we print it as we go and save it in Step 9.
counts <- data.frame(stage = character(), files = integer()) # empty data object
add_count <- function(stage, n) {
  counts <<- rbind(counts, data.frame(stage = stage, files = n))
  counts
}


# ---- Step 1: Find the image files ---------------------------------------------
# list.files() with recursive = TRUE walks every subfolder: the RedEdge-P
# writes 0000SET/000, 0000SET/001, ... (a new SET each time it powers on, a
# new numbered folder every 200 captures). DJI writes 100MEDIA, 101MEDIA, ...
all_files <- list.files(img_dir, recursive = TRUE, full.names = TRUE)
add_count("all files on the card", length(all_files))

# Not everything on the SD card is an image: the RedEdge-P also writes log files
# (gpslog.dat, diag.dat, ...). Keep image files only, with a regular
# expression: (?i) = ignore case, \\. = a literal dot, $ = end of the name.
img_files <- grep("(?i)\\.(jpe?g|tiff?)$", all_files, value = TRUE)
add_count("image files", length(img_files))

# Which band is each file? The band number is the digit(s) between the last
# underscore and ".tif". sub() keeps that part; names without it get NA.
band <- ifelse(grepl("(?i)_[0-9]+\\.tiff?$", img_files),
               sub("(?i).*_([0-9]+)\\.tiff?$", "\\1", img_files), NA)
table(band, useNA = "ifany")          # one row per band: same count in each

# How much disk does each band take? (This is why we skip band 6.)
round(tapply(file.size(img_files), band, sum) / 1e9, 2)   # GB per band

# Keep one band (or everything, for a camera with no band suffix).
files <- if (all(is.na(band))) img_files else img_files[band == use_band]
add_count(if (all(is.na(band))) "images to screen" else
          paste0("band ", use_band, " files (one per capture)"), length(files))
head(files)


# ---- Step 2: Read the metadata ------------------------------------------------
# 2a. DISCOVER. Read EVERYTHING from one image to see which tags this camera
#     writes. Different cameras use different names for the same thing (or
#     do not record it at all), so always look before you pick columns.
one <- read_exif(files[1])
ncol(one)            # roughly 100 to 200 tags, depending on the camera
names(one)           # scroll through the list
one[, intersect(c("Make", "Model", "BandName", "CentralWavelength"), names(one))]

# 2b. DETECT THE CAMERA. The Make tag tells us which tag set to use below.
#     Add a branch here for another camera once you have looked at its tags.
camera <- case_when(
  grepl("micasense", one$Make, ignore.case = TRUE) ~ "micasense",
  grepl("dji|hasselblad", one$Make, ignore.case = TRUE) ~ "dji",
  TRUE ~ "generic"     # anything else: GPS position and time only
)
camera

# Tags every camera should have:
#   GPSLatitude, GPSLongitude   position (decimal degrees, WGS84)
#   GPSAltitude                 height above sea level (m)
#   DateTimeOriginal            shutter time, camera clock
common_tags <- c("SourceFile", "Make", "Model", "DateTimeOriginal", "SubSecTime",
                 "GPSLatitude", "GPSLongitude", "GPSAltitude")

# Camera-specific tags:
#   DJI (in its XMP and maker notes)
#     RelativeAltitude            height above the TAKE-OFF point (m)
#     SpeedX, SpeedY, SpeedZ      velocity components (m/s)
#     GimbalYawDegree             direction the camera points (deg)
#     GimbalPitchDegree           -90 = looking straight down (nadir)
#   MicaSense (in its XMP)
#     BandName                    Blue, Green, Red, Red edge, NIR, Panchro
#     FlightId, CaptureId         one ID per power-on, one per trigger
#     Irradiance                  sunlight reaching the sun sensor (DLS)
#     ExposureTime                shutter time (s)
#   MicaSense records no speed and no height above ground; Steps 5 and 5b derive both.
#   It also writes Yaw, Pitch and Roll, but those describe the sun sensor,
#   which is mounted at an angle on top of the aircraft: on straight survey
#   lines they read 20 to 40 degrees of "roll" that changes with direction.
#   They are NOT the camera's tilt, so we do not use them. Always check that
#   a tag means what its name suggests before you filter on it.
camera_tags <- switch(camera,
  dji       = c("RelativeAltitude", "SpeedX", "SpeedY", "SpeedZ",
                "GimbalYawDegree", "GimbalPitchDegree"),
  micasense = c("BandName", "FlightId", "CaptureId", "Irradiance", "ExposureTime"),
  generic   = character(0)
)

# 2c. READ. Read just those tags from every image. Asking for specific tags
#     is much faster than reading everything (a minute or two instead of
#     many minutes for a few thousand files).
exif_raw <- read_exif(files, tags = c(common_tags, camera_tags))
dim(exif_raw)
head(exif_raw)

# Keep a copy of the raw metadata. Reading is the slow step, so saving the
# table lets you (or a colleague) redo the analysis without the images.
write.csv(exif_raw, file.path(out_dir, "exif_raw.csv"), row.names = FALSE)


# ---- Step 3: Clean it up: one standard table ----------------------------------
# Whatever the camera, we want the same columns at the end of this step:
#   file_name, time, lat, lon, alt_msl_m, and where the camera records them,
#   alt_rel_m (height above take-off), speed_mps, heading_deg, tilt_deg.
# Columns the camera cannot fill are NA here; Step 5 derives what it can.

# Some values come back as text with a sign, e.g. "+68.50". This helper turns
# any column into plain numbers (NA if unreadable or missing).
to_num <- function(x) if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(as.character(x)))

exif <- exif_raw %>%
  mutate(
    # The path inside the SD card, e.g. "0000SET/001/IMG_0249_1.tif". Not just
    # the base name: MicaSense restarts IMG numbering in each SET folder, so
    # IMG_0249_1.tif can exist several times on one card.
    file_name = substring(SourceFile, nchar(img_dir) + 2),
    # EXIF date-times look like "2026:05:15 18:16:38". We parse them as UTC
    # only so R does not shift them; the clock time is the camera's own.
    time = as.POSIXct(DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = "UTC") +
      # SubSecTime adds the fraction of a second. Cameras disagree on how to
      # write it: most write the digits after the decimal point ("07" = 0.07 s),
      # but the RedEdge-P writes NANOSECONDS with the leading zeros dropped
      # ("78308394" = 0.078 s, not 0.78 s). Get this wrong and the speeds in
      # Step 5 are garbage, so it is handled per camera.
      if (!"SubSecTime" %in% names(.)) 0 else
        coalesce(suppressWarnings(
          if (camera == "micasense") to_num(SubSecTime) / 1e9
          else as.numeric(paste0("0.", SubSecTime))), 0),
    lat = to_num(GPSLatitude),
    lon = to_num(GPSLongitude),
    alt_msl_m = to_num(GPSAltitude)
  )

if (camera == "dji") {
  exif <- exif %>% mutate(
    alt_rel_m   = to_num(RelativeAltitude),
    # Ground speed from the velocity components (Pythagoras in 3D).
    speed_mps   = sqrt(to_num(SpeedX)^2 + to_num(SpeedY)^2 + to_num(SpeedZ)^2),
    # DJI reports yaw from -180 to +180; %% 360 wraps it into 0 to 360.
    heading_deg = to_num(GimbalYawDegree) %% 360,
    # Gimbal pitch -90 is straight down; tilt = degrees away from straight down.
    tilt_deg    = 90 - abs(to_num(GimbalPitchDegree))
  )
} else if (camera == "micasense") {
  exif <- exif %>% mutate(
    band_name   = BandName,
    flight_id   = FlightId,
    alt_rel_m   = NA_real_,     # not recorded; derived in Step 5
    speed_mps   = NA_real_,     # not recorded; derived in Step 5
    heading_deg = NA_real_,     # derived from the GPS track in Step 5
    tilt_deg    = NA_real_,     # no trustworthy tilt tag (see Step 2)
    # Sunlight on the sun sensor. Multispectral reflectance depends on it, so
    # sudden drops (a cloud shadow) are worth screening for.
    irradiance  = to_num(Irradiance),
    exposure_ms = to_num(ExposureTime) * 1000
  )
} else {
  exif <- exif %>% mutate(alt_rel_m = NA_real_, speed_mps = NA_real_,
                          heading_deg = NA_real_, tilt_deg = NA_real_)
}

exif <- arrange(exif, time)   # put images in the order they were taken

# Any images without a GPS fix? They cannot be mapped or spatially filtered.
no_gps <- is.na(exif$lat) | is.na(exif$lon) | exif$lat == 0
sum(no_gps)
exif <- exif[!no_gps, ]
add_count("with a GPS position", nrow(exif))


# ---- Step 4: Split the images into flights ------------------------------------
# A new flight starts wherever the gap between consecutive images is longer
# than a few minutes (the time it takes to land and swap a battery).
gap_minutes <- 2
gap_sec <- c(0, diff(as.numeric(exif$time)))    # seconds since previous image
summary(gap_sec)                 # the usual gap is the trigger interval
sort(gap_sec, decreasing = TRUE)[1:6]   # the big ones are battery swaps
new_flight <- gap_sec > gap_minutes * 60

# The MicaSense also stamps a FlightId (and opens a new 000xSET folder) every
# time it powers on, i.e. every battery. On T95 the swaps took under a minute
# (gaps of 57 and 59 s), so the time-gap rule alone merges three sorties into
# one. When the camera tells us, believe the camera: a change of FlightId also
# starts a new flight.
if (camera == "micasense") {
  new_flight <- new_flight | (exif$flight_id != lag(exif$flight_id, default = exif$flight_id[1]))
}
exif$flight <- cumsum(new_flight) + 1

# Each flight should now match exactly one FlightId (MicaSense only).
if (camera == "micasense") table(exif$flight, exif$flight_id)

exif %>%
  group_by(flight) %>%
  summarise(images = n(), start = min(time), end = max(time),
            minutes = round(as.numeric(difftime(max(time), min(time), units = "mins")), 1))


# ---- Step 5: Derive non recorded parameters --------------------------
# Distances in degrees are meaningless, so first project the points to a
# metric CRS (as in Part 1). The UTM zone comes from the longitude:
# zone = floor((lon + 180) / 6) + 1, and EPSG 326zz is UTM North zone zz.
utm_epsg <- 32600 + floor((median(exif$lon) + 180) / 6) + 1
xy <- st_coordinates(st_transform(
  st_as_sf(exif, coords = c("lon", "lat"), crs = 4326), utm_epsg))
exif$x_m <- xy[, 1]
exif$y_m <- xy[, 2]

exif <- exif %>%
  group_by(flight) %>%
  mutate(
    # Height above the launch point: the camera starts recording on the
    # ground, so the first few images of each flight give the launch altitude.
    launch_alt = median(head(alt_msl_m, 3)),
    alt_rel_m  = ifelse(is.na(alt_rel_m), alt_msl_m - launch_alt, alt_rel_m),

    # Ground speed and direction of travel from consecutive positions:
    #   speed   = distance moved / time elapsed   (Pythagoras on x and y)
    #   heading = atan2(east step, north step), 0 = N, 90 = E
    dx = x_m - lag(x_m), dy = y_m - lag(y_m),
    dt = as.numeric(time) - lag(as.numeric(time)),
    gps_speed_mps   = sqrt(dx^2 + dy^2) / dt,
    gps_heading_deg = (atan2(dx, dy) * 180 / pi) %% 360,
    speed_mps   = ifelse(is.na(speed_mps), gps_speed_mps, speed_mps),
    heading_deg = ifelse(is.na(heading_deg), gps_heading_deg, heading_deg)
  ) %>%
  ungroup() %>%
  mutate(
    # Bin headings into 8 compass sectors, 45 degrees wide, centered on N.
    heading_sector = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")[
      floor(((heading_deg + 22.5) %% 360) / 45) + 1]
  )

summary(exif[, c("alt_rel_m", "speed_mps", "heading_deg", "tilt_deg")])


# ---- Step 5b: Height above GROUND ---------------------------------------------
# Height above launch is only height above ground if the ground is flat. At
# T95 the land is not flat, so survey legs flown at a steady 21 to 28 m above
# ground read anywhere from 14 to 29 m "above launch". What sets image scale
# (and ground sample distance) is the height above the ground UNDER the drone.
#
# elevatr::get_elev_point() looks up the ground elevation at each point.
# src = "aws" reads AWS Terrain Tiles (USGS 3DEP in the US); z = 14 is the
# finest zoom, cells of roughly 8 to 10 m here. It takes seconds for
# thousands of points. (src = "epqs", the USGS point service, also works but
# queries one point at a time: about 25 minutes for this flight.)
# Note: this sends the image coordinates to an outside service.
ground <- get_elev_point(st_as_sf(exif, coords = c("lon", "lat"), crs = 4326),
                         src = "aws", z = 14)
exif$ground_m <- ground$elevation

# GPS altitude and the terrain model each carry a few meters of vertical
# error. Calibrate with frames we KNOW are on the ground: the first frames of
# a flight, if the camera was not moving and was at launch height. On the
# ground, height above ground should be zero, so their median difference is
# the offset to remove.
exif <- exif %>%
  group_by(flight) %>%
  mutate(
    on_ground = row_number() <= 5 & coalesce(speed_mps, 0) < 1 & abs(alt_rel_m) < 2,
    ground_offset = if (any(on_ground)) median((alt_msl_m - ground_m)[on_ground]) else NA_real_,
    alt_agl_m = alt_msl_m - ground_m - ground_offset
  ) %>%
  ungroup()

# No ground frames, no calibration. The DJI class set is like this: the
# camera starts shooting at mission height, and its GPSAltitude is ~30 m off
# the terrain model. Its height above ground stays NA, and Step 7 falls back
# to height above launch (which the DJI records directly).
if (any(is.na(exif$alt_agl_m)))
  message("No ground frames for flight(s) ",
          paste(unique(exif$flight[is.na(exif$alt_agl_m)]), collapse = ", "),
          ": the height rule uses height above launch for them.")

# The offsets should be small (a few meters). A large one means the GPS
# altitude is on a different datum (e.g. ellipsoid height) or the "ground"
# frames were not on the ground.
exif %>% group_by(flight) %>% summarise(ground_offset = round(first(ground_offset), 1))
summary(exif$alt_agl_m)


# ---- Step 6: Explore: what did the mission actually do? -----------------------
# Treat the images as a flight log and plot each variable. The goal is to see
# the difference between the flight PLAN and what the aircraft really did.

# 6a. Ground track, colored by flight.
plot(exif$x_m, exif$y_m, type = "l", col = "grey70", asp = 1,
     xlab = "UTM easting (m)", ylab = "UTM northing (m)", main = "Ground track")
points(exif$x_m, exif$y_m, pch = 20, cex = 0.5, col = exif$flight)

# 6b. Height through time: a flat top for each flight, plus the ground,
#     climb, and descent at either end.
plot(exif$time, exif$alt_rel_m, pch = 20, cex = 0.5, col = exif$flight,
     xlab = "Time", ylab = "m above launch", main = "Height above launch")

# 6b2. The same thing above GROUND (Step 5b). Where the drone followed the
#      terrain (flights 3 and 4) the survey legs flatten out; where it held
#      one altitude (flight 1) height above ground changes with the land.
plot(exif$time, exif$alt_rel_m, pch = 20, cex = 0.4, col = "grey75",
     xlab = "Time", ylab = "m", main = "Above launch (grey) vs above ground (color)")
points(exif$time, exif$alt_agl_m, pch = 20, cex = 0.5, col = exif$flight)

# 6c. Speed. Survey legs cluster near the planned speed; the ground, turns,
#     and hovers sit near zero.
hist(exif$speed_mps, breaks = 40, col = "grey80", main = "Ground speed", xlab = "m/s")

# 6d. Heading. Back-and-forth survey lines give two opposite peaks.
hist(exif$heading_deg, breaks = seq(0, 360, 22.5), col = "grey80",
     main = "Direction of travel", xlab = "degrees (0 = N, 90 = E)")
table(exif$heading_sector)

# 6e. Tilt. Banking in the turns tips the camera off nadir. (Only for cameras
#     that record it: all NA for the MicaSense, so nothing to plot.)
if (any(!is.na(exif$tilt_deg)))
  hist(exif$tilt_deg, breaks = 40, col = "grey80", main = "Camera tilt", xlab = "degrees")

# 6f. (Multispectral) Sunlight through time. Dips are cloud shadow or the
#     sun sensor tipping away from the sun in a turn; both matter for
#     reflectance, so this is a screening variable too.
if (camera == "micasense")
  plot(exif$time, exif$irradiance, pch = 20, cex = 0.5, col = exif$flight,
       xlab = "Time", ylab = "Irradiance", main = "Sun sensor irradiance")


# ---- Step 7: Attribute filters ------------------------------------------------
# Write each rule as an explicit threshold. Because the rules are code, the
# selection is documented and repeatable. Look back at the Step 6 plots to
# choose the numbers.
# The height rule uses height above GROUND from Step 5b. The Step 6 plot
# shows why: flight 1 held a constant altitude, so over rising ground near
# W2-43 it dips to about 13 m above ground; flights 3 and 4 followed the
# terrain at 20 to 28 m. 10 m drops the ground, take-off and landing without
# clipping any survey leg. Where Step 5b had no ground frames to calibrate
# with, height above launch stands in (coalesce() takes the first non-NA).
min_height  <- 10              # m above ground; drops ground, take-off, landing
min_speed   <- 2               # m/s; drops hovers and the slowest turns
max_speed   <- 8               # m/s; drops fast transits between sites (~13 m/s)
max_tilt    <- 15              # degrees off straight down
keep_sector <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")   # all, for now
keep_flight <- sort(unique(exif$flight))                      # all, for now

height_m    <- coalesce(exif$alt_agl_m, exif$alt_rel_m)
rule_height <- height_m >= min_height
rule_speed  <- exif$speed_mps >= min_speed & exif$speed_mps <= max_speed
rule_tilt   <- is.na(exif$tilt_deg) | exif$tilt_deg <= max_tilt   # NA = not recorded
rule_sector <- exif$heading_sector %in% keep_sector
rule_flight <- exif$flight %in% keep_flight

keep <- rule_height & rule_speed & rule_tilt & rule_sector & rule_flight
selected <- exif[keep %in% TRUE, ]     # %in% TRUE treats NA (unknown) as "drop"

# The cull log: how many images does each rule keep ON ITS OWN, and how many
# survive all of them together? This is the table for a methods section.
cull_log <- data.frame(
  rule = c("all images", "height", "speed", "tilt", "heading", "flight", "ALL RULES"),
  kept = c(nrow(exif), sum(rule_height, na.rm = TRUE), sum(rule_speed, na.rm = TRUE),
           sum(rule_tilt, na.rm = TRUE), sum(rule_sector), sum(rule_flight), nrow(selected))
)
if (all(is.na(exif$tilt_deg))) cull_log <- cull_log[cull_log$rule != "tilt", ]   # not recorded
cull_log
add_count("pass the attribute rules", nrow(selected))

# Check the filter visually: removed images in grey, kept images in red.
plot(exif$time, height_m, pch = 20, cex = 0.5, col = "grey70",
     xlab = "Time", ylab = "m above ground", main = "Kept (red) vs removed (grey)")
points(exif$time[keep %in% TRUE], height_m[keep %in% TRUE], pch = 20, cex = 0.5, col = "red")


# ---- Step 8: Make it spatial; keep images over the Part 1 transects -----------
# st_as_sf() turns the longitude/latitude columns into point geometry.
# crs = 4326 means WGS84 longitude/latitude, which is what GPS records.
exif_sf     <- st_as_sf(exif,     coords = c("lon", "lat"), crs = 4326, remove = FALSE)
selected_sf <- st_as_sf(selected, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# The area of interest (AOI). Two ways to get one, chosen by aoi_source at
# the top of the script:
#
# OPTION A (aoi_source = "file"): read a polygon file. st_read() takes a KML
#   from Google Earth, a shapefile, a GeoPackage, GeoJSON ... For the demo
#   this is the Part 1 output for transect W2-78 (T95_W2-78.kml); set
#   aoi_file to "tranz_group_95.kml" to keep all seven transects.
#
# OPTION B (aoi_source = "draw"): draw it by hand on a satellite map. This
#   small "gadget" opens a map of the images that passed Step 7; draw one or
#   more polygons or rectangles with the toolbar, then click Done. (Needs an
#   interactive session, e.g. RStudio.)
draw_aoi <- function(pts) {
  library(shiny); library(miniUI); library(leaflet.extras)
  ui <- miniPage(
    gadgetTitleBar("Draw a polygon or rectangle, then click Done"),
    miniContentPanel(leafletOutput("map", height = "100%"), padding = 0)
  )
  server <- function(input, output, session) {
    shapes <- reactiveVal(list())
    output$map <- renderLeaflet({
      leaflet(pts) %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addCircleMarkers(radius = 3, color = "red", stroke = FALSE, fillOpacity = 0.8) %>%
        addDrawToolbar(polylineOptions = FALSE, circleOptions = FALSE,
                       markerOptions = FALSE, circleMarkerOptions = FALSE)
    })
    # Each finished shape arrives as GeoJSON; turn its ring into an sf polygon.
    observeEvent(input$map_draw_new_feature, {
      xy <- matrix(unlist(input$map_draw_new_feature$geometry$coordinates[[1]]),
                   ncol = 2, byrow = TRUE)
      shapes(c(shapes(), list(st_polygon(list(xy)))))
    })
    observeEvent(input$done, {
      stopApp(if (length(shapes())) st_sf(geometry = st_sfc(shapes(), crs = 4326)) else NULL)
    })
    observeEvent(input$cancel, stopApp(NULL))
  }
  runGadget(ui, server, viewer = paneViewer())
}

if (aoi_source == "draw") {
  aoi <- draw_aoi(selected_sf)
  if (is.null(aoi)) stop("No AOI drawn. Draw a polygon or rectangle, then click Done.")
  # Save what you drew, so the selection can be repeated (set aoi_source back
  # to "file" and aoi_file to this file next time).
  aoi_file <- file.path(out_dir, "drawn_aoi.kml")
  st_write(aoi, aoi_file, driver = "KML", delete_dsn = file.exists(aoi_file), quiet = TRUE)
} else {
  aoi <- st_read(aoi_file, quiet = TRUE)
}
aoi <- st_transform(st_zm(aoi), 4326)   # drop Z values, make sure it is WGS84

# An image covers ground AROUND its center point, so a photo centered just
# outside a 60 m footprint still sees part of it. Widen the AOI by a margin
# before testing (in meters, so buffer in UTM, then go back to WGS84).
aoi_margin_m <- 30
aoi_wide <- st_transform(st_buffer(st_transform(aoi, utm_epsg), aoi_margin_m), 4326)

# st_intersects() asks "which polygon does each point touch?". lengths() > 0
# is TRUE for points inside (or on the edge of) any AOI polygon.
inside   <- lengths(st_intersects(selected_sf, aoi_wide)) > 0
final_sf <- selected_sf[inside, ]
add_count(paste0("within ", aoi_margin_m, " m of the AOI"), nrow(final_sf))

# Which transect does each kept image cover? (useful for per-transect models)
aoi_label <- if ("Transect_I" %in% names(aoi)) aoi$Transect_I else seq_len(nrow(aoi))
final_sf$transect <- aoi_label[vapply(st_intersects(final_sf, aoi_wide), `[`, integer(1), 1)]
table(final_sf$transect)

# Map on satellite imagery: transects in yellow, all images grey, kept red.
leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addPolygons(data = aoi, color = "yellow", weight = 2, fillOpacity = 0.1,
              label = aoi_label) %>%
  addCircleMarkers(data = exif_sf, radius = 2, stroke = FALSE,
                   fillColor = "grey", fillOpacity = 0.6, group = "All images") %>%
  addCircleMarkers(data = final_sf, radius = 3, stroke = FALSE,
                   fillColor = "red", fillOpacity = 0.9, popup = ~file_name,
                   group = "Selected") %>%
  addLayersControl(overlayGroups = c("All images", "Selected"))



# ---- Step 9: Export -----------------------------------------------------------
# 9a. We screened ONE band, but the photogrammetry software needs EVERY band
#     of each kept capture. The band files share a stem (IMG_0123_), so swap
#     the band number to list the full set for each kept capture.
export_bands <- 1:5          # add 6 to include the panchromatic band
if (camera == "micasense") {
  stems <- sub("(?i)_[0-9]+\\.tiff?$", "", final_sf$SourceFile)
  ext   <- sub(".*(\\.[^.]+)$", "\\1", final_sf$SourceFile)
  # every stem with every band: IMG_0249 -> IMG_0249_1.tif ... IMG_0249_5.tif
  export_files <- paste0(rep(stems, each = length(export_bands)), "_",
                         export_bands, rep(ext, each = length(export_bands)))
} else {
  export_files <- final_sf$SourceFile
}
# Keep only files that exist (the workshop copy has band 1 only).
export_files <- export_files[file.exists(export_files)]
add_count(if (camera == "micasense") "files to export (all bands of kept captures)"
          else "files to export", length(export_files))
round(sum(file.size(export_files)) / 1e9, 2)   # GB to copy

# 9b. Copy everything into ONE folder: that is what Pix4D, Metashape and
#     web uploaders want. But the camera restarts its numbering, so the card
#     can hold IMG_0249_1.tif in several folders. Fold the folder path into
#     the name to keep every file unique:
#       0000SET/001/IMG_0249_1.tif  ->  0000SET_001_IMG_0249_1.tif
#     The band suffix (_1 ... _5) stays at the end, which is how the
#     photogrammetry software groups the bands of one capture.
#     The originals are never moved or changed. Set copy_files <- TRUE to run.
copy_files <- FALSE
rel_path <- substring(export_files, nchar(img_dir) + 2)    # path inside the card
new_name <- gsub("/", "_", rel_path)
stopifnot(!anyDuplicated(new_name))                        # every name unique
target   <- file.path(out_dir, "selected", new_name)
head(data.frame(rel_path, new_name))
if (copy_files) {
  dir.create(file.path(out_dir, "selected"), showWarnings = FALSE)
  copied <- file.copy(export_files, target, overwrite = FALSE, copy.date = TRUE)
  sum(copied)
}

# 9c. Manifest: one row per exported file, linking it to its source and to
#     the values that got its capture selected. This is the audit trail.
manifest <- data.frame(source_file = export_files, target_file = target,
                       stem = sub("(?i)_[0-9]+\\.tiff?$", "", export_files))
capture_info <- final_sf %>%
  st_drop_geometry() %>%
  transmute(stem = sub("(?i)_[0-9]+\\.tiff?$", "", SourceFile), time, flight,
            transect, lat, lon, alt_rel_m, alt_agl_m, speed_mps, heading_deg, tilt_deg)
manifest <- left_join(manifest, capture_info, by = "stem")
write.csv(manifest, file.path(out_dir, "export_manifest.csv"), row.names = FALSE)

# 9d. Record the rules themselves and the counts, so the selection can be
#     repeated exactly.
rules <- data.frame(
  parameter = c("image folder", "camera", "band screened", "date run",
                "min height (m above ground)", "min speed (m/s)", "max speed (m/s)", "max tilt (deg)",
                "headings kept", "flights kept", "AOI file", "AOI margin (m)",
                "bands exported"),
  value = c(img_dir, paste(one$Make, one$Model), use_band, format(Sys.time()),
            min_height, min_speed, max_speed, max_tilt, paste(keep_sector, collapse = " "),
            paste(keep_flight, collapse = " "), aoi_file, aoi_margin_m,
            paste(export_bands, collapse = " "))
)
write.csv(rules,    file.path(out_dir, "selection_rules.csv"), row.names = FALSE)
write.csv(cull_log, file.path(out_dir, "cull_log.csv"),        row.names = FALSE)
write.csv(counts,   file.path(out_dir, "file_counts.csv"),     row.names = FALSE)

# 9e. GIS layers to check the selection in QGIS / ArcGIS / Google Earth.
#     A GeoPackage can hold several layers in one file.
#     Keep the standard columns only (the raw tag columns are in exif_raw.csv).
gis_cols <- c("file_name", "SourceFile", "time", "flight", "transect", "lat", "lon",
              "alt_msl_m", "ground_m", "alt_rel_m", "alt_agl_m", "speed_mps", "heading_deg", "heading_sector", "tilt_deg")
gpkg <- file.path(out_dir, "selected_images.gpkg")
st_write(final_sf[intersect(gis_cols, names(final_sf))], gpkg, layer = "selected_images",
         delete_dsn = TRUE, quiet = TRUE)
st_write(aoi, gpkg, layer = "aoi", append = FALSE, quiet = TRUE)
kml <- file.path(out_dir, "selected_images.kml")
st_write(final_sf["file_name"], kml, driver = "KML", delete_dsn = file.exists(kml), quiet = TRUE)

counts      # the whole story in one table


# ---- Step 10: From script to app ----------------------------------------------
# ExifShiny/app.R does Steps 1 to 9 with buttons and sliders instead of
# edited numbers. Where to find each step in the app's code:
#
#   This script                     app.R
#   ------------------------------  -------------------------------------------
#   Step 1  list.files()            find_image_files()
#   Step 2  read_exif(tags = ...)   prepare_exif_table()  (exif_tags list)
#   Step 3  one standard table      parse_exif_numeric(), coalesce_numeric_columns()
#                                   (tries several tag names per variable)
#   Step 4  flights from time gaps  assign_flights() + "new flight after" box
#   Step 5  speed, heading          speed_from_components(), track_speed_direction(),
#                                   normalize_degrees(), direction_sector()
#   Step 5b height above ground     not in the app: its altitude slider is the
#                                   GPS altitude (m above sea level)
#   Step 6  plots                   "Histograms" tab, plot_selection_histogram()
#   Step 7  filter() thresholds     sliders/checkboxes -> filtered_sf()
#   Step 8  sf, leaflet, AOI        prepare_exif_sf(), output$map, draw toolbar /
#                                   KML upload -> selected_sf()
#   Step 9  copy + manifest         "Copy selected files" (export_manifest.csv)
#           CSV / KML / GPKG        "Save selection data"
#
# Run the app from R with:
#   shiny::runApp("ExifShiny")
# The reactive pattern: when an input (slider) changes, Shiny re-runs only
# the code that depends on it (filtered_sf -> selected_sf -> map, table,
# histograms). It is the same filter() you wrote in Step 7, re-run for you.


# ---- Extras: patterns from real projects --------------------------------------

# E1. Merging cards. Step 9 makes names unique within one card by folding in
#     the folder path. Two cards from the same camera can still repeat
#     "0000SET_001_IMG_0249_1.tif", so add a card or date prefix too:
#   new_name <- paste0("T95_15May26_", gsub("/", "_", rel_path))

# E2. Splitting a multispectral set by band (some software wants one folder
#     per band):
#   for (b in export_bands) {
#     these <- grep(paste0("_", b, "\\.tif$"), export_files, value = TRUE)
#     dir.create(file.path(out_dir, paste0("band_", b)), showWarnings = FALSE)
#     file.copy(these, file.path(out_dir, paste0("band_", b), basename(these)))
#   }

# E3. Images with no GPS fix (common in the first seconds after power-on) were
#     dropped in Step 3. List them so nobody wonders where they went:
#   exif_raw %>% filter(is.na(GPSLatitude)) %>% select(SourceFile, DateTimeOriginal)
