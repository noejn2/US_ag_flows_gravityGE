shpfileSeeker <- function() {
  # ----: Check if shapefile has been unzipped and if the zip exists ----
  if (!file.exists('assets/WGS 84 (EPSG 4326)/')) {
    # If the shapefile directory doesn't exist, check for the zip file
    if (!file.exists('assets/WGS 84 (EPSG 4326).zip')) {
      stop("The file 'assets/WGS 84 (EPSG 4326).zip' does not exist.") # Stop if zip is missing
    }
    # Unzip the shapefile if the directory is missing
    utils::unzip('assets/WGS 84 (EPSG 4326).zip', exdir = 'assets')
  }

  # ----: If unzipped, there should be 9 files :----
  len_HexBin_files <- function() {
    # Count the number of files matching the pattern in the shapefile directory
    length(list.files(
      "assets/WGS 84 (EPSG 4326)/",
      pattern = "^US_HexBinStates",
      full.names = TRUE
    ))
  }

  c <- 0
  # Ensure the directory contains exactly 9 files; retry unzipping if not
  while (len_HexBin_files() != 9 & file.exists('assets/WGS 84 (EPSG 4326)/')) {
    unlink('assets/WGS 84 (EPSG 4326)/', recursive = TRUE) # Remove incomplete directory
    utils::unzip('assets/WGS 84 (EPSG 4326).zip', exdir = 'assets') # Retry unzipping
    c <- c + 1
    if (c > 1) stop("The file 'assets/WGS 84 (EPSG 4326).zip' is corrupted.") # Stop after 1 retry
  }

  # ----: Delete Alaska, DC and Hawaii from Shpfile :----
  shp_file <- sf::st_read(
    'assets/WGS 84 (EPSG 4326)/US_HexBinStates_EPSG4326.shp' # Read the shapefile
  )
  # Remove rows corresponding to Alaska, DC, and Hawaii
  shp_file <- shp_file[!shp_file$Code %in% c('AK', 'DC', 'HI'), ]

  return(shp_file) # Return the processed shapefile
}
