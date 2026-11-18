# DronePhotoSelector: screen UAS photos by their EXIF metadata, then pick and export the keepers.

required_packages <- c("shiny", "leaflet", "leaflet.extras", "sf", "exifr", "dplyr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)] # check for required packages

if (length(missing_packages) > 0) {
  stop(
    "Install the required packages before running this app: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

# Load libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(exifr)
library(dplyr)

# set up default file paths and WD
app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
default_source_dir <- file.path(app_dir, "DCIM")
default_export_dir <- file.path(app_dir, "export")
default_flight_gap_min <- 2
direction_sector_names <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")

#default exif tags DJI
exif_tags <- c(
  "SourceFile", "Directory", "DateTimeOriginal", "GPSLongitude", "GPSLatitude",
  "GPSAltitude", "AbsoluteAltitude", "RelativeAltitude", "GPSSpeed",
  "SpeedX", "SpeedY", "SpeedZ", "GPSImgDirection", "GPSTrack",
  "GPSDestBearing", "Yaw", "CameraYaw", "GimbalYawDegree", "FlightYawDegree",
  "GPSPosition",
  # Camera identity and MicaSense-specific tags (see detect_camera()).
  "Make", "Model", "SubSecTime", "FlightId", "BandName"
)

# Which kind of camera wrote these files? 
#   dji       records speed (SpeedX/Y/Z) and heading (GimbalYawDegree).
#   micasense records neither. Its Yaw/Pitch/Roll describe the sun sensor (DLS),
#             in radians, so speed and direction are derived from the GPS track.
#   generic   anything else - assess tags then derive what is missing.
detect_camera <- function(make) {
  make <- make[!is.na(make)][1]
  if (is.na(make) || length(make) == 0) return("generic")
  if (grepl("micasense", make, ignore.case = TRUE)) return("micasense")
  if (grepl("dji|hasselblad", make, ignore.case = TRUE)) return("dji")
  "generic"
}

# Micasense Multispectral cameras write one file per band (IMG_0123_1.tif ... _6.tif)
# all with similar time and GPS position per shot. Screening one band is enough for culling
# so keep the lowest band number present. Files without a band suffix are all kept.
screen_one_band <- function(files) {
  has_band <- grepl("(?i)_[0-9]\\.tiff?$", files)
  if (!any(has_band)) return(files)
  band <- suppressWarnings(as.integer(sub("(?i).*_([0-9])\\.tiff?$", "\\1", files)))
  band[!has_band] <- NA_integer_
  files[!has_band | band == min(band, na.rm = TRUE)]
}

# All band files that exist for the captures of the given screening files,
# e.g. ".../IMG_0249_1.tif" -> ".../IMG_0249_1.tif" ... ".../IMG_0249_5.tif".
capture_band_files <- function(files) {
  unlist(lapply(files, function(f) {
    if (!grepl("(?i)_[0-9]\\.tiff?$", f)) return(f)
    stem <- sub("(?i)_[0-9]\\.tiff?$", "", basename(f))
    siblings <- list.files(dirname(f), pattern = paste0("^", stem, "_[0-9]\\.tiff?$"),
                           full.names = TRUE, ignore.case = TRUE)
    if (length(siblings) == 0) f else siblings
  }), use.names = FALSE)
}

# Ground speed (m/s) and direction of travel (degrees, 0 = N) from consecutive
# GPS positions, for cameras that do not record direction Points are projected to
# UTM so distances are in meters. Steps that cross a change of `group` (a new
# FlightId) or a pause longer than `max_gap_s` are left NA.
track_speed_direction <- function(lon, lat, times, group = NULL, max_gap_s = 60) {
  n <- length(lon)
  out <- list(speed = rep(NA_real_, n), direction = rep(NA_real_, n))
  ok <- which(!is.na(lon) & !is.na(lat) & !is.na(times))
  if (length(ok) < 2) return(out)
  if (is.null(group)) group <- rep(1, n)
  group[is.na(group)] <- "none"
  ord <- ok[order(as.character(group[ok]), as.numeric(times[ok]))]
  epsg <- 32600 + floor((stats::median(lon[ok]) + 180) / 6) + 1
  if (stats::median(lat[ok]) < 0) epsg <- epsg + 100     # UTM South
  xy <- st_coordinates(st_transform(st_as_sf(data.frame(x = lon[ord], y = lat[ord]),
                                             coords = c("x", "y"), crs = 4326), epsg))
  dx <- c(NA, diff(xy[, 1]))
  dy <- c(NA, diff(xy[, 2]))
  dt <- c(NA, diff(as.numeric(times[ord])))
  same <- c(FALSE, group[ord][-1] == group[ord][-length(ord)])
  valid <- same & !is.na(dt) & dt > 0 & dt <= max_gap_s
  speed <- ifelse(valid, sqrt(dx^2 + dy^2) / dt, NA_real_)
  direction <- ifelse(valid, (atan2(dx, dy) * 180 / pi) %% 360, NA_real_)
  # The first photo of each flight (or after a pause) has no previous photo. So
  # give it the value of the step that follows, so the sliders do not drop it.
  first <- which(!valid & c(valid[-1], FALSE))
  speed[first] <- speed[first + 1]
  direction[first] <- direction[first + 1]
  out$speed[ord] <- speed
  out$direction[ord] <- direction
  out
}

# List all JPEG, TIFF, and PNG image files in a folder (optionally including subfolders).
find_image_files <- function(path, recursive = TRUE) {
  list.files(
    path = path,
    pattern = "(?i)\\.(jpe?g|tiff?|png)$",
    recursive = recursive,
    full.names = TRUE
  )
}

# Convert raw EXIF values (which may include units or text) to numbers, returning NA when unparseable.
parse_exif_numeric <- function(x) {
  if (is.null(x)) {
    return(numeric())
  }

  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  suppressWarnings(as.numeric(gsub("[^0-9+.-]+", "", x_chr)))
}

# Parse several EXIF columns as numbers and return the first non-missing value per row.
coalesce_numeric_columns <- function(data, columns) {
  available <- intersect(columns, names(data))

  if (length(available) == 0) {
    return(rep(NA_real_, nrow(data)))
  }

  values <- lapply(available, function(column_name) parse_exif_numeric(data[[column_name]]))
  result <- values[[1]]

  if (length(values) > 1) {
    for (idx in 2:length(values)) {
      result <- dplyr::coalesce(result, values[[idx]])
    }
  }

  result
}

# Compute overall speed from the DJI SpeedX/SpeedY/SpeedZ velocity components.
speed_from_components <- function(data) {
  component_names <- intersect(c("SpeedX", "SpeedY", "SpeedZ"), names(data))

  if (length(component_names) == 0) {
    return(rep(NA_real_, nrow(data)))
  }

  component_matrix <- do.call(
    cbind,
    lapply(component_names, function(column_name) parse_exif_numeric(data[[column_name]]))
  )

  component_matrix <- as.matrix(component_matrix)
  component_speed <- sqrt(rowSums(component_matrix^2, na.rm = TRUE))
  component_speed[rowSums(!is.na(component_matrix)) == 0] <- NA_real_
  component_speed
}

# Wrap angles into the 0 to 360 degree range.
normalize_degrees <- function(x) {
  normalized <- x %% 360
  normalized[normalized < 0] <- normalized[normalized < 0] + 360
  normalized
}

# Read EXIF tags from the image files and build a clean table of location, altitude, speed, and direction.
prepare_exif_table <- function(files) {
  exif_raw <- exifr::read_exif(files, tags = exif_tags)

  if (nrow(exif_raw) == 0) {
    return(exif_raw)
  }

  camera <- detect_camera(if ("Make" %in% names(exif_raw)) exif_raw$Make else NA)
  flight_id <- if ("FlightId" %in% names(exif_raw)) as.character(exif_raw$FlightId) else rep(NA_character_, nrow(exif_raw))
  taken <- parse_exif_datetime(if ("DateTimeOriginal" %in% names(exif_raw)) exif_raw$DateTimeOriginal else NA)

  # Sub-second time matters when speed is derived from 2 s photo spacing. The
  # RedEdge-P writes SubSecTime in NANOSECONDS with leading zeros dropped
  # ("78308394" = 0.078 s), so it is divided by 1e9 rather than read as a fraction.
  if (camera == "micasense" && "SubSecTime" %in% names(exif_raw)) {
    taken <- taken + dplyr::coalesce(parse_exif_numeric(exif_raw$SubSecTime) / 1e9, 0)
  }

  altitude_m <- coalesce_numeric_columns(exif_raw, c("GPSAltitude", "AbsoluteAltitude", "RelativeAltitude"))
  track <- track_speed_direction(
    parse_exif_numeric(exif_raw$GPSLongitude), parse_exif_numeric(exif_raw$GPSLatitude),
    taken, group = flight_id
  )

  if (camera == "micasense") {
    # Nothing recorded to trust: both come from the GPS track.
    speed_mps <- track$speed
    direction_deg <- track$direction
  } else {
    # Recorded values first; the GPS track fills any gaps.
    speed_mps <- dplyr::coalesce(
      coalesce_numeric_columns(exif_raw, c("GPSSpeed")),
      speed_from_components(exif_raw),
      track$speed
    )
    direction_deg <- normalize_degrees(dplyr::coalesce(
      coalesce_numeric_columns(
        exif_raw,
        c("GPSImgDirection", "GPSTrack", "GPSDestBearing", "FlightYawDegree", "GimbalYawDegree", "CameraYaw")
      ),
      track$direction
    ))
  }

  # Add calculated values to the exif table, filter out rows with missing values, and remove duplicates
  exif_tbl <- exif_raw %>%
    mutate(
      source_file = SourceFile,
      file_name = basename(SourceFile),
      directory = dirname(SourceFile),
      datetime_original = if ("DateTimeOriginal" %in% names(exif_raw)) as.character(DateTimeOriginal) else NA_character_,
      datetime_taken = taken,
      camera = camera,
      flight_id = flight_id,
      gps_longitude = parse_exif_numeric(GPSLongitude),
      gps_latitude = parse_exif_numeric(GPSLatitude),
      altitude_m = altitude_m,
      speed_mps = speed_mps,
      direction_deg = direction_deg,
      direction_sector = as.character(direction_sector(direction_deg))
    ) %>%
    filter(!is.na(source_file), !is.na(gps_longitude), !is.na(gps_latitude)) %>%
    distinct(source_file, .keep_all = TRUE)

  exif_tbl
}

# Convert EXIF date/time text (e.g. "2019:05:28 12:55:46") to a date-time leaving the camera's local time is kept as-is.
parse_exif_datetime <- function(x) {
  as.POSIXct(substr(as.character(x), 1, 19), format = "%Y:%m:%d %H:%M:%S", tz = "UTC")
}

# Assign each heading to one of 8 compass directions, 45 degrees wide and centered on N, NE, E, ... (N is 337.5 to 22.5).
direction_sector <- function(direction_deg) {
  sector_index <- floor(((direction_deg + 22.5) %% 360) / 45) + 1
  factor(direction_sector_names[sector_index], levels = direction_sector_names)
}

# Number the flights: sort photos by time and start a new flight wherever the gap between photos exceeds `gap_minutes`.
# If the camera stamps a flight ID at power-on (MicaSense FlightId), a change of ID also starts a new flight: battery

assign_flights <- function(times, gap_minutes = default_flight_gap_min, ids = NULL) {
  if (!is.numeric(gap_minutes) || length(gap_minutes) != 1 || is.na(gap_minutes) || gap_minutes <= 0) {
    gap_minutes <- default_flight_gap_min
  }

  flight <- rep(NA_integer_, length(times))
  timed <- which(!is.na(times))

  if (length(timed) == 0) {
    return(flight)
  }

  time_order <- timed[order(times[timed])]
  gaps_sec <- c(0, diff(as.numeric(times[time_order])))
  new_flight <- gaps_sec > gap_minutes * 60

  if (!is.null(ids) && any(!is.na(ids))) {
    ordered_ids <- as.character(ids[time_order])
    changed <- c(FALSE, ordered_ids[-1] != ordered_ids[-length(ordered_ids)])
    new_flight <- new_flight | (!is.na(changed) & changed)
  }

  flight[time_order] <- as.integer(cumsum(new_flight) + 1L)
  flight
}

# Time format for labels: clock time, plus the date when the photos span more than one day.
time_label_format <- function(times, with_seconds = FALSE) {
  multi_day <- length(unique(as.Date(times[!is.na(times)]))) > 1
  clock <- if (with_seconds) "%H:%M:%S" else "%H:%M"
  if (multi_day) paste("%m-%d", clock) else clock
}

# Convert the EXIF table to an sf point layer in WGS84 (EPSG:4326).
prepare_exif_sf <- function(exif_tbl) {
  st_as_sf(exif_tbl, coords = c("gps_longitude", "gps_latitude"), crs = 4326, remove = FALSE)
}

# Build a range slider spanning the observed values, or a message if no values are present.
make_slider_ui <- function(input_id, label, values, step = 1) {
  finite_values <- values[is.finite(values)]

  if (length(finite_values) == 0) {
    return(tags$p(sprintf("%s: no values found in the loaded EXIF data.", label)))
  }

  min_value <- min(finite_values)
  max_value <- max(finite_values)

  if (identical(min_value, max_value)) {
    min_value <- min_value - step
    max_value <- max_value + step
  }

  # Round the ends outward to whole numbers and start with the handles at those ends. (If the handles start at the
  # raw min/max, Shiny snaps them to the slider step, which can silently exclude the most extreme images on load.)
  slider_min <- floor(min_value)
  slider_max <- ceiling(max_value)

  sliderInput(
    inputId = input_id,
    label = label,
    min = slider_min,
    max = slider_max,
    value = c(slider_min, slider_max),
    step = step
  )
}

# Build a time range slider spanning the photo times (rounded out to whole minutes).
make_time_slider_ui <- function(input_id, label, times) {
  times <- times[!is.na(times)]
  min_time <- as.POSIXct(floor(as.numeric(min(times)) / 60) * 60, origin = "1970-01-01", tz = "UTC")
  max_time <- as.POSIXct(ceiling(as.numeric(max(times)) / 60) * 60, origin = "1970-01-01", tz = "UTC")

  if (max_time <= min_time) {
    max_time <- min_time + 60
  }

  sliderInput(
    inputId = input_id,
    label = label,
    min = min_time,
    max = max_time,
    value = c(min_time, max_time),
    step = 1,
    timeFormat = time_label_format(times, with_seconds = TRUE),
    timezone = "+0000"
  )
}

# Checkbox group with small "all" / "none" links beside its label (links have ids <input_id>_all and <input_id>_none).
checkbox_group_with_links <- function(input_id, label, choices, inline = FALSE) {
  tagList(
    tags$div(
      style = "display: flex; gap: 10px; align-items: baseline;",
      tags$label(label),
      actionLink(paste0(input_id, "_all"), "all"),
      actionLink(paste0(input_id, "_none"), "none")
    ),
    checkboxGroupInput(input_id, NULL, choices = choices, selected = unname(choices), inline = inline)
  )
}

# Checkbox choices for the compass sectors, labelled with how many photos fall in each, e.g. "E (228)".
sector_choices <- function(sectors) {
  counts <- table(factor(sectors, levels = direction_sector_names))
  setNames(direction_sector_names, sprintf("%s (%s)", direction_sector_names, as.integer(counts)))
}

# Checkbox choices for the flights, e.g. "Flight 2: 13:14 to 13:26 (212 images)".
flight_choices <- function(flights, times) {
  flight_ids <- sort(unique(flights[!is.na(flights)]))
  label_format <- time_label_format(times)

  labels <- vapply(flight_ids, function(flight_id) {
    flight_times <- times[flights %in% flight_id]
    sprintf(
      "Flight %s: %s to %s (%s images)",
      flight_id,
      format(min(flight_times), label_format, tz = "UTC"),
      format(max(flight_times), label_format, tz = "UTC"),
      length(flight_times)
    )
  }, character(1))

  setNames(as.character(flight_ids), labels)
}

# Keep only rows whose value in the given column is one of the checked choices (nothing checked keeps nothing).
apply_choice_filter <- function(data, column_name, chosen) {
  data[as.character(data[[column_name]]) %in% chosen, , drop = FALSE]
}

# Keep only rows whose value in the given column falls inside the slider range.
apply_range_filter <- function(data, column_name, range_values) {
  if (is.null(range_values) || !column_name %in% names(data)) {
    return(data)
  }

  data %>%
    filter(!is.na(.data[[column_name]]), .data[[column_name]] >= range_values[1], .data[[column_name]] <= range_values[2])
}

# Convert a polygon/rectangle drawn on the Leaflet map (GeoJSON list) into an sf polygon.
feature_to_sf <- function(feature) {
  if (is.null(feature) || is.null(feature$geometry) || !identical(feature$geometry$type, "Polygon")) {
    return(NULL)
  }

  polygon_rings <- lapply(feature$geometry$coordinates, function(ring) {
    ring_matrix <- do.call(rbind, lapply(ring, function(point) c(as.numeric(point[[1]]), as.numeric(point[[2]]))))

    if (!all(ring_matrix[1, ] == ring_matrix[nrow(ring_matrix), ])) {
      ring_matrix <- rbind(ring_matrix, ring_matrix[1, ])
    }

    ring_matrix
  })

  st_sf(name = "drawn", geometry = st_sfc(st_polygon(polygon_rings), crs = 4326))
}

# Read a KML file and keep each polygon separate (multipolygons are split into their parts), in WGS84.
read_kml_geometry <- function(path) {
  kml_data <- st_read(path, quiet = TRUE)
  kml_data <- st_transform(st_zm(kml_data, drop = TRUE, what = "ZM"), 4326)
  polygon_rows <- st_geometry_type(kml_data) %in% c("POLYGON", "MULTIPOLYGON")
  # error check
  if (!any(polygon_rows)) {
    stop("The uploaded KML file does not contain polygon geometry.", call. = FALSE)
  }

  polygon_data <- st_make_valid(kml_data[polygon_rows, , drop = FALSE])
  polygon_data <- polygon_data[st_geometry_type(polygon_data) %in% c("POLYGON", "MULTIPOLYGON"), , drop = FALSE]
  polygon_data <- suppressWarnings(st_cast(st_cast(polygon_data, "MULTIPOLYGON"), "POLYGON"))

  # Use the KML Name where there is one, otherwise number the polygons.
  polygon_names <- if ("Name" %in% names(polygon_data)) as.character(polygon_data$Name) else rep(NA_character_, nrow(polygon_data))
  polygon_names[is.na(polygon_names) | !nzchar(polygon_names)] <- paste("Polygon", which(is.na(polygon_names) | !nzchar(polygon_names)))

  st_sf(name = polygon_names, geometry = st_geometry(polygon_data))
}

# Return only the points that fall inside any of the selection polygons.
subset_by_geometry <- function(points_sf, polygon_sf) {
  if (nrow(points_sf) == 0 || is.null(polygon_sf)) {
    return(points_sf[0, , drop = FALSE])
  }

  inside <- lengths(st_intersects(points_sf, polygon_sf)) > 0
  points_sf[inside, , drop = FALSE]
}

# Grow (positive distance) or shrink (negative distance) the selection polygons by a distance in metres.
# The buffer is done in the local UTM zone so the distance is true metres; polygons that shrink away are dropped.
buffer_polygons <- function(polygon_sf, distance_m) {
  if (is.null(polygon_sf) || is.null(distance_m) || is.na(distance_m) || distance_m == 0) {
    return(polygon_sf)
  }

  centre <- st_coordinates(suppressWarnings(st_centroid(st_union(st_geometry(polygon_sf)))))
  utm_epsg <- (if (centre[1, "Y"] >= 0) 32600 else 32700) + floor((centre[1, "X"] + 180) / 6) + 1

  buffered <- st_buffer(st_transform(polygon_sf, utm_epsg), distance_m)
  buffered <- buffered[!st_is_empty(buffered), , drop = FALSE]
  buffered <- st_transform(buffered, 4326)

  if ("name" %in% names(buffered)) {
    buffered$name <- sprintf("%s (buffer %+g m)", buffered$name, distance_m)
  }

  buffered
}

# Build a target path in the export folder, adding _1, _2, ... so existing files are never overwritten.
next_export_path <- function(export_dir, file_name) {
  base_name <- tools::file_path_sans_ext(file_name)
  extension <- tools::file_ext(file_name)
  suffix <- if (nzchar(extension)) paste0(".", extension) else ""

  candidate <- file.path(export_dir, file_name)
  counter <- 1L

  while (file.exists(candidate)) {
    candidate <- file.path(export_dir, paste0(base_name, "_", counter, suffix))
    counter <- counter + 1L
  }

  candidate
}

# Pick a sensible starting folder for the picker - either the typed path, its nearest existing parent, or the app folder.
picker_start_dir <- function(path) {
  path <- if (is.null(path)) "" else trimws(path)

  while (nzchar(path) && !dir.exists(path)) {
    parent <- dirname(path)
    if (identical(parent, path)) break
    path <- parent
  }

  if (nzchar(path) && dir.exists(path)) normalizePath(path, winslash = "/") else app_dir
}

# Run a command-line dialog tool and return its output path, or NA if the user cancelled.
run_dialog_command <- function(command, args) {
  output <- suppressWarnings(system2(command, shQuote(args), stdout = TRUE, stderr = FALSE))
  status <- attr(output, "status")

  if ((!is.null(status) && status != 0) || length(output) == 0 || !nzchar(output[[1]])) {
    return(NA_character_)
  }

  output[[1]]
}

# Windows: show the built-in Windows folder browser.
choose_folder_windows <- function(prompt, start_dir) {
  utils::choose.dir(default = normalizePath(start_dir, winslash = "\\"), caption = prompt)
}

# macOS: show the Finder "Choose Folder" dialog through AppleScript.
choose_folder_macos <- function(prompt, start_dir) {
  escape <- function(x) gsub("\"", "\\\\\"", x)
  script <- c(
    "tell application \"System Events\"",
    "activate",
    sprintf(
      "set chosen_folder to choose folder with prompt \"%s\" default location (POSIX file \"%s\")",
      escape(prompt), escape(start_dir)
    ),
    "end tell",
    "POSIX path of chosen_folder"
  )
  run_dialog_command("osascript", as.vector(rbind("-e", script)))
}

# Linux: show a GTK (zenity) or KDE (kdialog) folder dialog, whichever is installed; NULL if neither is.
choose_folder_linux <- function(prompt, start_dir) {
  if (nzchar(Sys.which("zenity"))) {
    return(run_dialog_command(
      "zenity",
      c("--file-selection", "--directory", paste0("--title=", prompt), paste0("--filename=", start_dir, "/"))
    ))
  }

  if (nzchar(Sys.which("kdialog"))) {
    return(run_dialog_command("kdialog", c("--getexistingdirectory", start_dir, "--title", prompt)))
  }

  NULL
}

# Fallback pickers for any platform: the RStudio dialog when running inside RStudio, then the Tcl/Tk dialog.
choose_folder_fallback <- function(prompt, start_dir) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    chosen <- rstudioapi::selectDirectory(caption = prompt, path = start_dir)
    return(if (is.null(chosen)) NA_character_ else chosen)
  }

  if (isTRUE(capabilities("tcltk")) && requireNamespace("tcltk", quietly = TRUE)) {
    return(tcltk::tk_choose.dir(default = start_dir, caption = prompt))
  }

  NULL
}

# Open the operating system's native folder picker and return the chosen folder, or NULL if the user cancelled.
# The dialog opens on the machine running R, so the app must be run locally (e.g. shiny::runApp()).
choose_folder_native <- function(prompt = "Select a folder", current_path = NULL) {
  start_dir <- picker_start_dir(current_path)
  os_picker <- switch(
    Sys.info()[["sysname"]],
    Windows = choose_folder_windows,
    Darwin = choose_folder_macos,
    choose_folder_linux
  )

  chosen <- tryCatch(os_picker(prompt, start_dir), error = function(e) NULL)

  if (is.null(chosen)) {
    chosen <- choose_folder_fallback(prompt, start_dir)
  }

  if (is.null(chosen)) {
    stop("No native folder picker is available on this system. Type the folder path instead.", call. = FALSE)
  }

  if (length(chosen) == 0 || is.na(chosen) || !nzchar(chosen)) {
    return(NULL)
  }

  normalizePath(sub("[/\\\\]+$", "", chosen), winslash = "/", mustWork = FALSE)
}

# Build a folder path text box with a "Browse..." button beside it that opens the native folder picker.
folder_input_ui <- function(text_id, button_id, label, value) {
  div(
    style = "display: flex; gap: 6px; align-items: flex-end;",
    div(style = "flex: 1; min-width: 0;", textInput(text_id, label, value = value, width = "100%")),
    div(style = "margin-bottom: 15px;", actionButton(button_id, "Browse...", icon = icon("folder-open")))
  )
}

# Browse button handler: open the native picker and copy the chosen folder into the matching text box.
browse_into_text_input <- function(session, text_id, prompt, current_path) {
  chosen <- tryCatch(
    choose_folder_native(prompt, current_path),
    error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    }
  )

  if (!is.null(chosen)) {
    updateTextInput(session, text_id, value = chosen)
  }
}

# Map button (magnifier): click it, then drag a box on the map to zoom to that area (a plain click zooms in one level there).
# Click it again (X icon) to cancel. Shift+drag also box-zooms at any time without the button.
zoom_box_button <- function() {
  start_zoom_box <- JS("
    function(btn, map) {
      var container = map.getContainer();
      var start = null;
      var box = null;

      var finish = function() {
        map.off('mousedown', onDown);
        map.off('mousemove', onMove);
        map.off('mouseup', onUp);
        if (box) { map.removeLayer(box); box = null; }
        map.dragging.enable();
        container.style.cursor = '';
        map._zoomBoxCancel = null;
        btn.state('zoom-off');
      };

      var onDown = function(e) {
        start = e.latlng;
        box = L.rectangle(L.latLngBounds(start, start), {
          color: '#ffcc00', weight: 2, dashArray: '5,5', fillOpacity: 0.1, interactive: false
        }).addTo(map);
      };

      var onMove = function(e) {
        if (box) { box.setBounds(L.latLngBounds(start, e.latlng)); }
      };

      var onUp = function(e) {
        if (!box) { return; }
        var bounds = L.latLngBounds(start, e.latlng);
        var startPx = map.latLngToContainerPoint(start);
        var endPx = map.latLngToContainerPoint(e.latlng);
        finish();
        if (startPx.distanceTo(endPx) < 5) {
          map.setZoomAround(e.latlng, map.getZoom() + 1);
        } else {
          map.fitBounds(bounds);
        }
      };

      map.dragging.disable();
      container.style.cursor = 'crosshair';
      map.on('mousedown', onDown);
      map.on('mousemove', onMove);
      map.on('mouseup', onUp);
      map._zoomBoxCancel = finish;
      btn.state('zoom-on');
    }
  ")

  cancel_zoom_box <- JS("
    function(btn, map) {
      if (map._zoomBoxCancel) { map._zoomBoxCancel(); } else { btn.state('zoom-off'); }
    }
  ")

  easyButton(
    position = "topleft",
    states = list(
      easyButtonState(stateName = "zoom-off", icon = "fa-search-plus", title = "Drag a box to zoom in", onClick = start_zoom_box),
      easyButtonState(stateName = "zoom-on", icon = "fa-times", title = "Cancel box zoom", onClick = cancel_zoom_box)
    )
  )
}

# Map button: zoom back out to show all loaded image points (handled on the server via input$zoom_to_data).
zoom_to_data_button <- function() {
  easyButton(
    position = "topleft",
    icon = "fa-expand",
    title = "Zoom to all loaded images",
    onClick = JS("function(btn, map) { Shiny.setInputValue('zoom_to_data', Date.now()); }")
  )
}

# Zoom the map to the bounding box of a set of points.
fit_map_to_points <- function(map_id, points_sf) {
  bbox <- st_bbox(points_sf)
  leafletProxy(map_id) %>%
    fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
}

# Build the attribute table written to the CSV/KML/GeoPackage exports (one row per selected image).
selection_export_table <- function(points_sf) {
  table_data <- st_drop_geometry(points_sf)
  export_columns <- intersect(
    c(
      "file_name", "datetime_original", "datetime_taken", "flight", "gps_latitude", "gps_longitude",
      "altitude_m", "speed_mps", "direction_deg", "direction_sector", "source_file"
    ),
    names(table_data)
  )
  as.data.frame(table_data[, export_columns, drop = FALSE])
}

# Write the selected images to a CSV file with coordinates, so it can be added to a GIS as XY points (WGS84).
write_selection_csv <- function(points_sf, path) {
  write.csv(selection_export_table(points_sf), path, row.names = FALSE, na = "")
}

# Write the selected images (and the selection polygon, if any) to a KML file for checking in Google Earth or a GIS.
write_selection_kml <- function(points_sf, polygon_sf, path) {
  point_table <- selection_export_table(points_sf)
  point_table$Name <- point_table$file_name
  point_table$Description <- sprintf(
    "Taken: %s<br/>Flight: %s<br/>Altitude: %s m<br/>Speed: %s m/s<br/>Direction: %s deg (%s)<br/>%s",
    point_table$datetime_original,
    point_table$flight,
    round(point_table$altitude_m, 2),
    round(point_table$speed_mps, 2),
    round(point_table$direction_deg, 2),
    point_table$direction_sector,
    point_table$source_file
  )
  geometries <- st_geometry(points_sf)

  if (!is.null(polygon_sf)) {
    polygon_names <- if ("name" %in% names(polygon_sf)) polygon_sf$name else "Selection polygon"
    polygon_table <- data.frame(Name = polygon_names, Description = "Area used to select images")
    point_table <- bind_rows(point_table, polygon_table)
    geometries <- c(geometries, st_geometry(st_transform(polygon_sf, 4326)))
  }

  if (length(geometries) == 0) {
    stop("Nothing is selected, so there is nothing to write to KML.", call. = FALSE)
  }

  point_table <- point_table[, c("Name", "Description", setdiff(names(point_table), c("Name", "Description")))]
  kml_sf <- st_sf(point_table, geometry = geometries)
  st_write(kml_sf, path, driver = "KML", layer = "selected_images", quiet = TRUE, delete_dsn = file.exists(path))
}

# Write the selected images to a GeoPackage, with the selection polygon (if any) as a second layer.
write_selection_gpkg <- function(points_sf, polygon_sf, path) {
  points_layer <- st_sf(selection_export_table(points_sf), geometry = st_geometry(points_sf))
  st_write(points_layer, path, layer = "selected_images", driver = "GPKG", quiet = TRUE, delete_dsn = file.exists(path))

  if (!is.null(polygon_sf)) {
    polygon_names <- if ("name" %in% names(polygon_sf)) polygon_sf$name else "Selection polygon"
    polygon_layer <- st_sf(name = polygon_names, geometry = st_geometry(st_transform(polygon_sf, 4326)))
    st_write(polygon_layer, path, layer = "selection_polygon", driver = "GPKG", quiet = TRUE, append = FALSE)
  }
}

# Make a time-stamped file name for a selection export, e.g. selected_images_20260924_101500.kml.
selection_file_name <- function(extension, stamp = format(Sys.time(), "%Y%m%d_%H%M%S")) {
  paste0("selected_images_", stamp, ".", extension)
}

# Map colors. Selection mode: grey = excluded by filters, blue = passes filters, red = selected (same red as the histograms).
selection_colors <- c(excluded = "#9e9e9e", filtered = "#2b8cbe", selected = "#f03b20")
# Categorical colors (fixed order; neighbouring colors stay distinct for color-blind viewers). Used for compass
# sectors in order N, NE, E, ... so adjacent headings get adjacent, distinguishable colors, and for flights 1 to 8.
categorical_colors <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#4a3aa7", "#e34948")
# Diverging ramp for continuous values (altitude, speed, time): dark blue (low) through a light neutral (median) to
# dark red (high). The red arm matches the blue arm's lightness step for step, so both sides read as equally strong.
diverging_colors <- c(
  "#0d366b", "#184f95", "#2a78d6", "#6da7ec", "#b7d3f6",
  "#f0efec",
  "#ffbcb3", "#ff6e62", "#e40015", "#9c0008", "#6f0004"
)
missing_value_color <- "#808080"

# Color function for the diverging ramp, centered on `midpoint`: values from the minimum to the midpoint use the
# whole blue half, and values from the midpoint to the maximum use the whole red half, for maximum contrast.
diverging_palette <- function(domain, midpoint) {
  ramp <- colorNumeric(diverging_colors, domain = c(0, 1), na.color = missing_value_color)
  lower_span <- max(midpoint - domain[1], .Machine$double.eps)
  upper_span <- max(domain[2] - midpoint, .Machine$double.eps)

  function(x) {
    x <- as.numeric(x)
    position <- ifelse(
      x < midpoint,
      0.5 * (x - domain[1]) / lower_span,
      0.5 + 0.5 * (x - midpoint) / upper_span
    )
    ramp(pmin(pmax(position, 0), 1))
  }
}

# Legend swatch styles: the color plus a thin grey border, so light colors (like the neutral median) stay visible
# on the legend's white background. Leaflet inserts them into the swatch's CSS "background" style.
legend_swatches <- function(colors) {
  paste0(colors, "; border: 1px solid #bdbdbd; box-sizing: border-box")
}

# Color each map point by the chosen "Symbolize by" variable and build the matching legend.
# `status` is "excluded", "filtered", or "selected" for each point. Returns list(colors, legend), where legend is a
# list of addLegend() arguments (or NULL when there is nothing to show).
point_symbology <- function(points, symbolize_by, status) {
  legend_note <- "<br/><small>Selected: larger, white outline</small>"

  # Continuous values: diverging ramp centered on the median with a legend of five labelled color samples
  # (min, halfway to the median, median, halfway to the max, max). `format_value` turns legend numbers into text.
  continuous <- function(values, title, format_value = function(x) format(round(x, 1), nsmall = 1)) {
    values <- as.numeric(values)
    finite_values <- values[is.finite(values)]

    if (length(finite_values) == 0) {
      return(list(colors = rep(missing_value_color, length(values)), legend = NULL))
    }

    domain <- range(finite_values)
    midpoint <- median(finite_values)
    if (domain[1] == domain[2]) domain <- domain + c(-1, 1)
    pal <- diverging_palette(domain, midpoint)

    key_values <- c(domain[1], (domain[1] + midpoint) / 2, midpoint, (midpoint + domain[2]) / 2, domain[2])
    key_labels <- paste0(format_value(key_values), c(" (min)", "", " (median)", "", " (max)"))
    legend <- list(colors = legend_swatches(pal(key_values)), labels = key_labels, title = paste0(title, legend_note), opacity = 1)
    list(colors = pal(values), legend = legend)
  }

  # Categories: one fixed color per level, legend lists only the levels present.
  categorical <- function(values, levels, colors, title, labels = levels) {
    values <- as.character(values)
    color_lookup <- setNames(colors, levels)
    point_colors <- unname(color_lookup[values])
    point_colors[is.na(point_colors)] <- missing_value_color
    present <- levels %in% values
    legend <- list(colors = legend_swatches(colors[present]), labels = labels[present], title = paste0(title, legend_note), opacity = 1)
    list(colors = point_colors, legend = legend)
  }

  switch(
    symbolize_by,
    altitude_m = continuous(points$altitude_m, "Altitude (m)"),
    speed_mps = continuous(points$speed_mps, "Speed (m/s)"),
    datetime_taken = continuous(
      points$datetime_taken, "Time taken",
      format_value = function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), time_label_format(points$datetime_taken), tz = "UTC")
    ),
    direction_sector = categorical(points$direction_sector, direction_sector_names, categorical_colors, "Direction"),
    flight = {
      flight_ids <- sort(unique(points$flight[!is.na(points$flight)]))
      if (length(flight_ids) <= length(categorical_colors)) {
        categorical(points$flight, as.character(flight_ids), categorical_colors[seq_along(flight_ids)], "Flight", paste("Flight", flight_ids))
      } else {
        continuous(points$flight, "Flight number", format_value = function(x) trimws(format(round(x))))
      }
    },
    categorical(status, names(selection_colors), unname(selection_colors), "Selection",
                c("Excluded by filters", "Passes filters", "Selected"))
  )
}

# Popup text for map points: file name, path, and EXIF values.
point_popup <- function(points) {
  paste0(
    "<strong>", htmltools::htmlEscape(points$file_name), "</strong><br/>",
    htmltools::htmlEscape(points$source_file), "<br/>",
    "Taken: ", format(points$datetime_taken, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "<br/>",
    "Flight: ", points$flight, "<br/>",
    "Altitude: ", round(points$altitude_m, 2), " m<br/>",
    "Speed: ", round(points$speed_mps, 2), " m/s<br/>",
    "Direction: ", round(points$direction_deg, 1), " deg (", points$direction_sector, ")"
  )
}

histogram_colors <- c(all = "#c9c9c6", selected = "#f03b20", grid = "#e8e8e6", axis = "#d0d0cc", text = "#5f6368")

# Legend shown above the histograms: grey bars are all loaded images, red bars are the current selection (same red as the map).
histogram_legend_ui <- function() {
  swatch <- function(color, label) {
    tags$span(
      style = "display: inline-flex; align-items: center; gap: 6px; margin-right: 18px;",
      tags$span(style = sprintf("width: 14px; height: 14px; border-radius: 3px; background: %s; display: inline-block;", color)),
      label
    )
  }

  tags$div(
    style = sprintf("margin: 14px 0 4px; color: %s;", histogram_colors[["text"]]),
    swatch(histogram_colors[["all"]], "All loaded images"),
    swatch(histogram_colors[["selected"]], "Selected images")
  )
}

# Draw one histogram: all loaded images as grey bars with the selected images as red bars in front.
# `breaks` are passed to hist(); `draw_x_axis` optionally replaces the default x axis (used for direction and time).
plot_selection_histogram <- function(all_values, selected_values, label, breaks = "Sturges", draw_x_axis = NULL) {
  all_values <- as.numeric(all_values)
  selected_values <- as.numeric(selected_values)
  all_values <- all_values[is.finite(all_values)]
  selected_values <- selected_values[is.finite(selected_values)]

  old_par <- par(mar = c(4.2, 4.2, 3.4, 1), mgp = c(2.4, 0.7, 0), las = 1, cex.axis = 0.9,
                 col.axis = histogram_colors[["text"]], col.lab = histogram_colors[["text"]])
  on.exit(par(old_par))

  if (length(all_values) == 0) {
    plot.new()
    title(main = label, adj = 0, font.main = 1)
    text(0.5, 0.5, "No values found in the loaded EXIF data.", col = histogram_colors[["text"]])
    return(invisible(NULL))
  }

  # Compute bin edges as doubles; hist() can build integer edges for large values (e.g. times), which overflow.
  if (is.character(breaks)) breaks <- nclass.Sturges(all_values)
  if (length(breaks) == 1) breaks <- as.numeric(pretty(range(all_values), n = breaks, min.n = 1))

  bins <- hist(all_values, breaks = breaks, plot = FALSE)
  selected_counts <- if (length(selected_values) > 0) {
    hist(selected_values, breaks = bins$breaks, plot = FALSE)$counts
  } else {
    rep(0L, length(bins$counts))
  }

  left <- head(bins$breaks, -1)
  right <- tail(bins$breaks, -1)
  y_ticks <- pretty(c(0, max(bins$counts)))
  y_ticks <- y_ticks[y_ticks == round(y_ticks)]

  plot(NA, xlim = range(bins$breaks), ylim = c(0, max(y_ticks)), xlab = label, ylab = "Number of images", axes = FALSE)
  abline(h = y_ticks, col = histogram_colors[["grid"]], lwd = 1)
  rect(left, 0, right, bins$counts, col = histogram_colors[["all"]], border = "white", lwd = 2)

  has_selected <- selected_counts > 0
  if (any(has_selected)) {
    rect(left[has_selected], 0, right[has_selected], selected_counts[has_selected],
         col = histogram_colors[["selected"]], border = "white", lwd = 2)
  }

  axis(2, at = y_ticks, col = NA, col.ticks = NA)
  if (is.null(draw_x_axis)) axis(1, col = histogram_colors[["axis"]]) else draw_x_axis()

  title(main = label, adj = 0, font.main = 1, line = 1.8)
  mtext(
    sprintf("%s of %s images selected", length(selected_values), length(all_values)),
    side = 3, adj = 0, line = 0.5, cex = 0.85, col = histogram_colors[["text"]]
  )
}

# X axis for flight direction: compass labels every 90 degrees.
draw_direction_axis <- function() {
  axis(1, at = seq(0, 360, 90), labels = c("N 0", "E 90", "S 180", "W 270", "N 360"), col = histogram_colors[["axis"]])
}

# X axis for time taken: readable clock times (with the date when the photos span more than one day).
make_time_axis <- function(times) {
  function() {
    ticks <- pretty(times)
    labels <- format(ticks, time_label_format(times), tz = "UTC")
    axis(1, at = as.numeric(ticks), labels = labels, col = histogram_colors[["axis"]])
  }
}

# Summary statistics (count, min, median, max) for each variable, for the selection and for all loaded images.
histogram_summary_table <- function(all_data, selected_data) {
  variables <- c("Altitude (m)" = "altitude_m", "Speed (m/s)" = "speed_mps", "Direction (deg)" = "direction_deg")

  # Format min / median / max of a numeric vector as text.
  describe <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) return(c(n = "0", min = "", median = "", max = ""))
    c(n = as.character(length(x)), min = format(round(min(x), 1)), median = format(round(median(x), 1)), max = format(round(max(x), 1)))
  }

  rows <- lapply(names(variables), function(label) {
    column_name <- variables[[label]]
    selected_stats <- describe(selected_data[[column_name]])
    all_stats <- describe(all_data[[column_name]])
    data.frame(
      Variable = label,
      `Selected n` = selected_stats[["n"]], `Selected min` = selected_stats[["min"]],
      `Selected median` = selected_stats[["median"]], `Selected max` = selected_stats[["max"]],
      `All n` = all_stats[["n"]], `All min` = all_stats[["min"]],
      `All median` = all_stats[["median"]], `All max` = all_stats[["max"]],
      check.names = FALSE
    )
  })

  do.call(rbind, rows)
}

# UI Page layout: sidebar with folder, filter, spatial, and export controls; map and selected-file table in the main panel.
ui <- fluidPage(
  titlePanel("DronePhotoSelector", windowTitle = "DronePhotoSelector"),
  sidebarLayout(
    sidebarPanel(
      folder_input_ui("source_dir", "browse_source", "Source image folder", default_source_dir),
      checkboxInput("recursive", "Search folders recursively", value = TRUE),
      actionButton("load_files", "Load EXIF data"),
      tags$hr(),
      uiOutput("filter_ui"),
      tags$hr(),
      radioButtons(
        "spatial_mode",
        "Spatial selection",
        choices = c(
          "Use all filtered files" = "none",
          "Use the drawn polygon / rectangle" = "draw",
          "Use an uploaded KML polygon" = "kml"
        ),
        selected = "none"
      ),
      conditionalPanel(
        "input.spatial_mode === 'kml'",
        fileInput("kml_file", "Upload KML polygon", accept = c(".kml", "application/vnd.google-earth.kml+xml"))
      ),
      conditionalPanel(
        "input.spatial_mode !== 'none'",
        numericInput("aoi_buffer_m", "Buffer the polygon (m)", value = 0, step = 5),
        helpText("Positive values grow the area, negative values shrink it. 0 uses the polygon as is.")
      ),
      actionButton("clear_spatial", "Clear spatial selection"),
      tags$hr(),
      folder_input_ui("export_dir", "browse_export", "Export folder", default_export_dir),
      actionButton("export_files", "Copy selected files"),
      tags$br(),
      tags$br(),
      checkboxGroupInput(
        "export_formats",
        "Selection data to save in the export folder (to check in a GIS)",
        choices = c("CSV" = "csv", "KML" = "kml", "GeoPackage" = "gpkg"),
        selected = c("csv", "kml", "gpkg"),
        inline = TRUE
      ),
      actionButton("save_selection_files", "Save selection data", icon = icon("floppy-disk")),
      tags$br(),
      tags$br(),
      verbatimTextOutput("summary_text"),
      verbatimTextOutput("export_text")
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Map",
          radioButtons(
            "symbolize_by",
            "Symbolize points by",
            choices = c(
              "Selection" = "selection", "Altitude" = "altitude_m", "Speed" = "speed_mps",
              "Direction" = "direction_sector", "Time taken" = "datetime_taken", "Flight" = "flight"
            ),
            selected = "selection",
            inline = TRUE
          ),
          leafletOutput("map", height = 650),
          tags$hr(),
          h4("Selected files"),
          tableOutput("selected_table")
        ),
        tabPanel(
          "Histograms",
          histogram_legend_ui(),
          fluidRow(
            column(6, plotOutput("hist_altitude", height = 280)),
            column(6, plotOutput("hist_speed", height = 280))
          ),
          fluidRow(
            column(6, plotOutput("hist_direction", height = 280)),
            column(6, plotOutput("hist_time", height = 280))
          ),
          tags$hr(),
          h4("Summary: selected vs. all loaded images"),
          tableOutput("histogram_summary")
        )
      )
    )
  )
)

# Server logic -  loads EXIF data, applies filters and spatial selections, draws the map, and exports files.
server <- function(input, output, session) {
  # Shared app state: loaded EXIF data, current selection polygons, and the latest export status message.
  rv <- reactiveValues(
    exif_table = NULL,
    exif_sf = NULL,
    drawn_polygon = NULL,
    kml_polygon = NULL,
    export_message = "No export has been run yet."
  )

  # Browse button for the source folder: pick the image folder with the native file browser.
  observeEvent(input$browse_source, {
    browse_into_text_input(session, "source_dir", "Select the source image folder", input$source_dir)
  })

  # Browse button for the export folder: pick where selected files will be copied.
  observeEvent(input$browse_export, {
    browse_into_text_input(session, "export_dir", "Select the export folder", input$export_dir)
  })

  # Load button: find images in the source folder, read their EXIF data, and reset any spatial selection.
  observeEvent(input$load_files, {
    source_dir <- normalizePath(input$source_dir, winslash = "/", mustWork = FALSE)

    if (!dir.exists(source_dir)) {
      showNotification("The source image folder does not exist.", type = "error")
      return()
    }

    image_files <- find_image_files(source_dir, recursive = isTRUE(input$recursive))
    n_image_files <- length(image_files)
    # Multispectral: one band per capture is enough to screen (export takes every band).
    image_files <- screen_one_band(image_files)

    if (length(image_files) == 0) {
      rv$exif_table <- NULL
      rv$exif_sf <- NULL
      showNotification("No supported image files were found in the source folder.", type = "warning")
      return()
    }

    withProgress(message = "Reading EXIF data", value = 0, {
      incProgress(0.25, detail = sprintf("Scanning %s files", length(image_files)))
      exif_table <- prepare_exif_table(image_files)
      incProgress(0.75, detail = "Building spatial features")
      rv$exif_table <- exif_table
      rv$exif_sf <- prepare_exif_sf(exif_table)
    })

    rv$drawn_polygon <- NULL
    rv$kml_polygon <- NULL
    camera <- if (nrow(rv$exif_table) > 0) rv$exif_table$camera[1] else "unknown"
    band_note <- if (length(image_files) < n_image_files) {
      sprintf(" (one band screened: %s of %s image files)", length(image_files), n_image_files)
    } else {
      ""
    }
    rv$export_message <- sprintf(
      "Loaded %s images with EXIF GPS coordinates. Camera: %s%s.",
      nrow(rv$exif_table), camera, band_note
    )

    showNotification(rv$export_message, type = "message")
  })

  # Build the altitude and speed sliders, direction sector checkboxes, and flight / time controls from the loaded data.
  output$filter_ui <- renderUI({
    if (is.null(rv$exif_table) || nrow(rv$exif_table) == 0) {
      return(tags$p("Load a folder to enable altitude, speed, direction, and time filters."))
    }

    direction_ui <- if (any(!is.na(rv$exif_table$direction_sector))) {
      checkbox_group_with_links(
        "direction_sectors", "Direction (compass sectors, 45 degrees each)",
        sector_choices(rv$exif_table$direction_sector), inline = TRUE
      )
    } else {
      tags$p("Direction: no values found in the loaded EXIF data.")
    }

    time_ui <- if (any(!is.na(rv$exif_table$datetime_taken))) {
      tagList(
        numericInput("flight_gap", "Start a new flight after a gap of (minutes)", value = default_flight_gap_min, min = 0.5, step = 0.5),
        uiOutput("flight_ui"),
        make_time_slider_ui("time_range", "Time taken", rv$exif_table$datetime_taken)
      )
    } else {
      tags$p("Time: no photo dates found in the loaded EXIF data, so flight and time filters are off.")
    }

    tagList(
      make_slider_ui("altitude_range", "Altitude (m)", rv$exif_table$altitude_m, step = 1),
      make_slider_ui("speed_range", "Speed (m/s)", rv$exif_table$speed_mps, step = 0.1),
      direction_ui,
      time_ui
    )
  })

  # All loaded images, numbered into flights using the current flight gap setting.
  loaded_sf <- reactive({
    req(rv$exif_sf)
    loaded <- rv$exif_sf
    loaded$flight <- assign_flights(loaded$datetime_taken, input$flight_gap, loaded$flight_id)
    loaded
  })

  # One checkbox per detected flight (rebuilt, all checked, when data loads or the flight gap changes).
  output$flight_ui <- renderUI({
    loaded <- loaded_sf()
    checkbox_group_with_links("flights", "Flights", flight_choices(loaded$flight, loaded$datetime_taken))
  })

  # "all" / "none" links for the direction sector checkboxes.
  observeEvent(input$direction_sectors_all, {
    updateCheckboxGroupInput(session, "direction_sectors", selected = direction_sector_names)
  })
  observeEvent(input$direction_sectors_none, {
    updateCheckboxGroupInput(session, "direction_sectors", selected = character(0))
  })

  # "all" / "none" links for the flight checkboxes.
  observeEvent(input$flights_all, {
    updateCheckboxGroupInput(session, "flights", selected = as.character(unique(na.omit(loaded_sf()$flight))))
  })
  observeEvent(input$flights_none, {
    updateCheckboxGroupInput(session, "flights", selected = character(0))
  })

  # Read an uploaded KML file into the KML selection polygon.
  observeEvent(input$kml_file, {
    req(input$kml_file$datapath)
    rv$kml_polygon <- read_kml_geometry(input$kml_file$datapath)
    showNotification("KML polygon loaded.", type = "message")
  })

  # Store a newly drawn map polygon/rectangle as the drawn selection.
  observeEvent(input$map_draw_new_feature, {
    drawn_polygon <- feature_to_sf(input$map_draw_new_feature)

    if (is.null(drawn_polygon)) {
      showNotification("Draw a polygon or rectangle to create a spatial selection.", type = "warning")
      return()
    }

    rv$drawn_polygon <- drawn_polygon
    showNotification("Drawn polygon loaded.", type = "message")
  })

  # Forget the drawn selection when it is deleted from the map.
  observeEvent(input$map_draw_deleted_features, {
    rv$drawn_polygon <- NULL
  })

  # Clear button: remove both the drawn and KML selection polygons.
  observeEvent(input$clear_spatial, {
    rv$drawn_polygon <- NULL
    rv$kml_polygon <- NULL
    showNotification("Spatial selections cleared.", type = "message")
  })

  # Points that pass the altitude/speed sliders, the checked direction sectors and flights, and the time slider.
  filtered_sf <- reactive({
    loaded <- loaded_sf()
    filtered <- loaded
    filtered <- apply_range_filter(filtered, "altitude_m", input$altitude_range)
    filtered <- apply_range_filter(filtered, "speed_mps", input$speed_range)

    if (any(!is.na(loaded$direction_sector))) {
      filtered <- apply_choice_filter(filtered, "direction_sector", input$direction_sectors)
    }

    if (any(!is.na(loaded$flight))) {
      filtered <- apply_choice_filter(filtered, "flight", input$flights)
      filtered <- apply_range_filter(filtered, "datetime_taken", input$time_range)
    }

    filtered
  })

  # The drawn or uploaded polygon for the current spatial mode, before any buffer (NULL in "all files" mode).
  source_polygon <- reactive({
    if (identical(input$spatial_mode, "draw")) {
      return(rv$drawn_polygon)
    }

    if (identical(input$spatial_mode, "kml")) {
      return(rv$kml_polygon)
    }

    NULL
  })

  # The buffer distance in metres (0 when the box is empty).
  buffer_distance <- reactive({
    distance_m <- input$aoi_buffer_m
    if (is.null(distance_m) || is.na(distance_m)) 0 else distance_m
  })

  # The selection polygon actually used: the source polygon grown or shrunk by the buffer.
  active_polygon <- reactive({
    buffer_polygons(source_polygon(), buffer_distance())
  })

  # Final selection: filtered points, further limited to the active polygon when one is in use.
  selected_sf <- reactive({
    current_filtered <- filtered_sf()

    if (identical(input$spatial_mode, "none")) {
      return(current_filtered)
    }

    polygon_sf <- active_polygon()

    if (is.null(polygon_sf)) {
      return(current_filtered[0, , drop = FALSE])
    }

    subset_by_geometry(current_filtered, polygon_sf)
  })

  # Base map with satellite imagery, zoom buttons, and the polygon/rectangle drawing toolbar.
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addEasyButton(zoom_box_button()) %>%
      addEasyButton(zoom_to_data_button()) %>%
      addDrawToolbar(
        targetGroup = "drawn-selection",
        polygonOptions = drawPolygonOptions(showArea = TRUE, repeatMode = FALSE),
        rectangleOptions = drawRectangleOptions(repeatMode = FALSE),
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        polylineOptions = FALSE,
        editOptions = editToolbarOptions()
      )
  })

  # Zoom the map to the extent of newly loaded points.
  observeEvent(rv$exif_sf, {
    req(rv$exif_sf)
    fit_map_to_points("map", rv$exif_sf)
  })

  # Zoom-to-data map button: zoom back out to all loaded points.
  observeEvent(input$zoom_to_data, {
    if (is.null(rv$exif_sf) || nrow(rv$exif_sf) == 0) {
      showNotification("Load EXIF data first.", type = "warning")
      return()
    }

    fit_map_to_points("map", rv$exif_sf)
  })

  # Redraw all loaded points colored by the "Symbolize by" choice (selected points larger with a white outline),
  # the legend, and the selection polygon (green) whenever any of them change.
  observe({
    loaded <- loaded_sf()
    filtered_ids <- filtered_sf()$source_file
    selected_ids <- selected_sf()$source_file
    polygon_sf <- active_polygon()

    status <- ifelse(
      loaded$source_file %in% selected_ids, "selected",
      ifelse(loaded$source_file %in% filtered_ids, "filtered", "excluded")
    )
    symbology <- point_symbology(loaded, input$symbolize_by, status)
    loaded$point_color <- symbology$colors
    loaded$popup <- point_popup(loaded)
    is_selected <- status == "selected"

    proxy <- leafletProxy("map") %>%
      clearGroup("image-points") %>%
      clearGroup("selected-points") %>%
      clearGroup("selection-polygon") %>%
      removeControl("symbol-legend")

    if (any(!is_selected)) {
      proxy %>%
        addCircleMarkers(
          data = loaded[!is_selected, ],
          lng = ~gps_longitude,
          lat = ~gps_latitude,
          group = "image-points",
          radius = 4,
          stroke = TRUE,
          color = "#ffffff",
          weight = 0.5,
          opacity = 0.6,
          fillColor = ~point_color,
          fillOpacity = 0.55,
          popup = ~popup
        )
    }

    if (any(is_selected)) {
      proxy %>%
        addCircleMarkers(
          data = loaded[is_selected, ],
          lng = ~gps_longitude,
          lat = ~gps_latitude,
          group = "selected-points",
          radius = 6,
          stroke = TRUE,
          color = "#ffffff",
          weight = 2,
          opacity = 1,
          fillColor = ~point_color,
          fillOpacity = 0.95,
          popup = ~popup
        )
    }

    if (!is.null(symbology$legend)) {
      do.call(addLegend, c(list(map = proxy, position = "bottomright", layerId = "symbol-legend"), symbology$legend))
    }

    # With a buffer, show the original polygon as a thin dashed outline under the buffered one.
    original_sf <- source_polygon()
    if (!is.null(original_sf) && buffer_distance() != 0) {
      proxy %>%
        addPolygons(
          data = original_sf,
          group = "selection-polygon",
          color = "#ffffff",
          weight = 1.5,
          dashArray = "6 4",
          fill = FALSE
        )
    }

    if (!is.null(polygon_sf) && nrow(polygon_sf) > 0) {
      proxy %>%
        addPolygons(
          data = polygon_sf,
          group = "selection-polygon",
          color = "#31a354",
          weight = 2,
          fillOpacity = 0.08
        )
    }
  })

  # Text summary of how many files are loaded, filtered, and selected.
  output$summary_text <- renderText({
    if (is.null(rv$exif_table)) {
      return("No EXIF data loaded yet.")
    }

    spatial_summary <- switch(
      input$spatial_mode,
      none = "Using all filtered files.",
      draw = if (is.null(rv$drawn_polygon)) "Draw mode is active, but no polygon has been drawn yet." else "Draw mode is active.",
      kml = if (is.null(rv$kml_polygon)) "KML mode is active, but no KML polygon has been uploaded yet." else "KML mode is active."
    )

    if (!identical(input$spatial_mode, "none") && !is.null(source_polygon()) && buffer_distance() != 0) {
      spatial_summary <- paste(
        spatial_summary,
        if (nrow(active_polygon()) == 0) {
          sprintf("Buffer of %g m shrinks the polygon away entirely.", buffer_distance())
        } else {
          sprintf("Polygon buffered by %+g m.", buffer_distance())
        }
      )
    }

    paste(
      sprintf("Files with EXIF GPS data: %s", nrow(rv$exif_table)),
      sprintf("Files after filters: %s", nrow(filtered_sf())),
      sprintf("Files currently selected: %s", nrow(selected_sf())),
      spatial_summary,
      sep = "\n"
    )
  })

  # Table of the first 25 selected files with key EXIF values.
  output$selected_table <- renderTable({
    current_selected <- selected_sf()

    if (nrow(current_selected) == 0) {
      return(data.frame(Message = "No files are currently selected."))
    }

    table_data <- st_drop_geometry(current_selected)
    table_columns <- intersect(
      c("file_name", "datetime_original", "flight", "altitude_m", "speed_mps", "direction_deg", "direction_sector", "source_file"),
      names(table_data)
    )

    head(table_data[, table_columns, drop = FALSE], 25)
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  # Histogram of one EXIF column: all loaded images (grey) vs. the current selection (red).
  render_selection_histogram <- function(column_name, label, ...) {
    renderPlot({
      plot_selection_histogram(loaded_sf()[[column_name]], selected_sf()[[column_name]], label, ...)
    })
  }

  output$hist_altitude <- render_selection_histogram("altitude_m", "Altitude (m)")
  output$hist_speed <- render_selection_histogram("speed_mps", "Speed (m/s)")
  output$hist_direction <- render_selection_histogram(
    "direction_deg", "Direction (degrees)",
    breaks = seq(0, 360, by = 22.5), draw_x_axis = draw_direction_axis
  )

  # Time-taken histogram - for spotting separate flights or batteries within one folder.
  output$hist_time <- renderPlot({
    loaded <- loaded_sf()
    plot_selection_histogram(
      loaded$datetime_taken, selected_sf()$datetime_taken, "Time taken",
      breaks = 30, draw_x_axis = make_time_axis(loaded$datetime_taken)
    )
  })

  # Summary table under the histograms.
  output$histogram_summary <- renderTable({
    histogram_summary_table(st_drop_geometry(loaded_sf()), st_drop_geometry(selected_sf()))
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  # Export button: copy newly selected files to the export folder and record them in export_manifest.csv.
  observeEvent(input$export_files, {
    current_selected <- selected_sf()

    if (nrow(current_selected) == 0) {
      rv$export_message <- "No files are selected for export."
      showNotification(rv$export_message, type = "warning")
      return()
    }

    export_dir <- normalizePath(input$export_dir, winslash = "/", mustWork = FALSE)

    if (!dir.exists(export_dir)) {
      dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
    }

    manifest_path <- file.path(export_dir, "export_manifest.csv")
    existing_manifest <- if (file.exists(manifest_path)) {
      read.csv(manifest_path, stringsAsFactors = FALSE)
    } else {
      data.frame(source_file = character(), target_file = character(), stringsAsFactors = FALSE)
    }

    selected_sources <- unique(current_selected$source_file)
    # Multispectral captures: export every band file present, not just the screened band.
    selected_sources <- unique(capture_band_files(selected_sources))
    new_sources <- setdiff(selected_sources, existing_manifest$source_file)
    source_root <- normalizePath(input$source_dir, winslash = "/", mustWork = FALSE)

    if (length(new_sources) == 0) {
      rv$export_message <- "All currently selected files were already exported earlier."
      showNotification(rv$export_message, type = "message")
      return()
    }

    copied_rows <- vector("list", length(new_sources))
    copied_count <- 0L

    for (idx in seq_along(new_sources)) {
      source_file <- new_sources[[idx]]
      # One folder with no structure. Cameras restart numbering in each
      # folder (0000SET/001/IMG_0249_1.tif can repeat), so fold the subfolders into the name:
      # 0000SET_001_IMG_0249_1.tif. The band suffix stays at the end.
      relative_path <- if (startsWith(source_file, paste0(source_root, "/"))) {
        substring(source_file, nchar(source_root) + 2)
      } else {
        basename(source_file)
      }
      target_file <- next_export_path(export_dir, gsub("/", "_", relative_path))
      copied <- file.copy(from = source_file, to = target_file, overwrite = FALSE, copy.date = TRUE)

      if (isTRUE(copied)) {
        copied_count <- copied_count + 1L
        copied_rows[[copied_count]] <- data.frame(
          source_file = source_file,
          target_file = normalizePath(target_file, winslash = "/", mustWork = FALSE),
          stringsAsFactors = FALSE
        )
      }
    }

    copied_rows <- copied_rows[seq_len(copied_count)]
## error notifications
    if (copied_count == 0) {
      rv$export_message <- "No files were copied. Check file permissions and source paths."
      showNotification(rv$export_message, type = "error")
      return()
    }

    updated_manifest <- bind_rows(existing_manifest, bind_rows(copied_rows))
    write.csv(updated_manifest, manifest_path, row.names = FALSE)

    rv$export_message <- sprintf(
      "Copied %s new files to %s. Skipped %s files that were already exported.",
      copied_count,
      export_dir,
      length(selected_sources) - copied_count
    )

    showNotification(rv$export_message, type = "message")
  })

  # Show the latest export status message.
  output$export_text <- renderText({
    rv$export_message
  })

  # Save button: write the selected images to the export folder in each checked format (CSV, KML, GeoPackage).
  observeEvent(input$save_selection_files, {
    if (is.null(rv$exif_sf)) {
      showNotification("Load EXIF data first.", type = "warning")
      return()
    }

    formats <- input$export_formats

    if (length(formats) == 0) {
      showNotification("Check at least one file type (CSV, KML, or GeoPackage) to save.", type = "warning")
      return()
    }

    current_selected <- selected_sf()

    if (nrow(current_selected) == 0) {
      rv$export_message <- "No files are selected, so no selection data was saved."
      showNotification(rv$export_message, type = "warning")
      return()
    }

    export_dir <- normalizePath(input$export_dir, winslash = "/", mustWork = FALSE)
    dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
    polygon_sf <- if (identical(input$spatial_mode, "none")) NULL else active_polygon()
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    saved_files <- character()

    for (file_format in formats) {
      path <- file.path(export_dir, selection_file_name(file_format, stamp))
      saved <- tryCatch({
        switch(
          file_format,
          csv = write_selection_csv(current_selected, path),
          kml = write_selection_kml(current_selected, polygon_sf, path),
          gpkg = write_selection_gpkg(current_selected, polygon_sf, path)
        )
        TRUE
      }, error = function(e) {
        showNotification(sprintf("Could not save %s: %s", toupper(file_format), conditionMessage(e)), type = "error")
        FALSE
      })

      if (saved) saved_files <- c(saved_files, basename(path))
    }

    if (length(saved_files) > 0) {
      rv$export_message <- sprintf(
        "Saved %s selected images to %s:\n%s",
        nrow(current_selected), export_dir, paste(saved_files, collapse = "\n")
      )
      showNotification(sprintf("Saved %s", paste(saved_files, collapse = ", ")), type = "message")
    }
  })
}

shinyApp(ui = ui, server = server)
