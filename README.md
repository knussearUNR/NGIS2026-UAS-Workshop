# NGIS2026-UAS-Workshop

R code and data for the NVGIS 2026 workshop on planning UAS (drone) surveys and screening the photos afterwards.

- **Part 1, Planning:** pick UAS launch sites along roads that can see as many vegetation transects as possible, then export one KML per site for flight planning software.
- **Part 2, Photo screening:** read the EXIF metadata of each photo, split flights, derive speed and height above ground, apply rules, select by area of interest, and export one flat folder of keepers ready for Pix4D or similar.

## Getting started

1.  Install [R](https://cran.r-project.org/) (RStudio is optional but handy).

2.  Clone the repo, or use **Code \> Download ZIP** on GitHub and unzip it.

3.  Install the packages:

    ``` r
    install.packages(c("sf", "dplyr", "leaflet", "leaflet.extras", "exifr",
                       "elevatr", "shiny", "miniUI", "mapview"))
    ```

    Part 2 reads metadata with [ExifTool](https://exiftool.org/). If `exifr` can't find it, run `exifr::install_exiftool()` once.

4.  Set the working directory to the folder of the part you are running (each script uses paths relative to its own folder):

    - Part 1: `Part1 Planning/VegTransectWorkup`
    - Part 2: `Part2 ExifPhotoSorter`

## What's in here

Slides: `Part1 Planning/NVGIS2026_Part1_UAS_Planning.pptx` and `Part2 ExifPhotoSorter/NVGIS2026_Part2_EXIF_Screening.pptx`.

### Part 1 Planning/VegTransectWorkup

| File | What it is |
|------------------------------------|------------------------------------|
| `Transect2KML_NGIS.R` | The worked example: candidate launch points, line-of-sight circles, greedy site selection, one KML per site |
| `Activity1_SiteSelection.R` | Hands-on activity with blanks to fill in |
| `Activity1_SiteSelection_SOLUTIONS.R` | Answer key |
| `data/` | Example data near Searchlight, NV: BLM public roads, plus hypothetical rough roads and vegetation transects (`make_hypothetical_data.R` shows how they were made) |

Running `Transect2KML_NGIS.R` writes its outputs to `UAS_KML_Files/`.

### Part 2 ExifPhotoSorter

| File | What it is |
|------------------------------------|------------------------------------|
| `ExifPhotoSorter_Walkthrough.R` | The worked example on a MicaSense RedEdge-P flight (field site T95, launch site 2 in Part 1) |
| `Activity2_PhotoScreening.R` | Hands-on activity on a DJI flight, with blanks to fill in |
| `Activity2_PhotoScreening_SOLUTIONS.R` | Answer key |
| `ExifShiny/app.R` | **DronePhotoSelector**, a Shiny app to filter photos by flight, speed, height, direction and area, then export them |
| `T95_W2-78.kml`, `tranz_group_95.kml` | Areas of interest for the walkthrough |
| `ExifShiny/AOI_example.kml` | Area of interest for Activity 2 |
| `T95_15cm_15May26_raw/` | MicaSense RedEdge-P card, band 1 only (see note below) |
| `ExifShiny/DCIM/` | DJI card, 904 photos (see note below), plus `exif_raw.csv` used by Activity 2 |

To start the app from the `Part2 ExifPhotoSorter` folder:

``` r
shiny::runApp("ExifShiny")
```

## A note on the photos

The photos in this repo are **shrunken copies**. Each image was resized to 64 pixels across and the original EXIF and XMP metadata (GPS, altitude, time stamps, camera, MicaSense band and flight tags, DJI speed and gimbal tags) was copied back in. The folder structure and file names match the original cards.

The scripts and app only read metadata, so they give the same results on these copies as on the full-size photos. What differs:

- The MicaSense (T95) photos were moved to the Searchlight, NV example area to match the hypothetical Part 1 data: each GPS position is shifted by the same distance, and each GPS altitude is adjusted for the change in ground elevation, so the flight pattern, speeds and heights above ground are unchanged. The walkthrough gives the same counts as on the original photos.
- The images are thumbnails, so they can't be used to build an orthomosaic.
- File sizes are tiny. On the real cards each RedEdge-P band was about 4.8 GB (band 6, panchromatic, much more) and the DJI set was 7.3 GB.
- The MicaSense log files (`gpslog.dat`, `diag.dat`, ...) are empty placeholders, kept so file counts match the real data from the SD card.

## Data credits and license

Part 1 roads: Bureau of Land Management, Ground Transportation Linear Features (GTLF), public display service. The Part 1 vegetation transects and rough roads are hypothetical, made up for teaching, and do not show real survey locations.

Part 2 photos were collected by Ken Nussear, Jeffrey Best and Bryden Baker.

DronePhotoSelector and exif processing scripts coded by Ken Nussear

## License

- **Code** (all `.R` files): MIT License, see [LICENSE](LICENSE).
- **Data** (shapefiles, KMLs, photos): [Creative Commons Attribution 4.0 (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/), unless the source above says otherwise. Please credit the sources listed above.
