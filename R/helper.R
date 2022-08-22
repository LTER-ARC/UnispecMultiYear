# ---
# title: "Combine Index files"
# author: "jiml"
# date: "2022-08-04"
# ---

## Required Packages
packages <- c("knitr","rstudioapi","lubridate","purrr",
              "tidyverse","openxlsx","data.table")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
#Function for getting the current year index.rds files given a folder
 
get_current_indices <- function(data_path) {
  index_files <-
    list.files(
      path = data_path,
      pattern = "index.rds$",
      full.names = T,
      recursive = T
    )
  # combine the rds files.  Using data.table since variable "location" can be either numeric or character.
  # rbinlist will coerced the columns type to highest type.
  dat_files <-
    lapply(index_files, function (x)
      data.table((readRDS(x))))
  index_data <- rbindlist(dat_files, fill = TRUE) %>%
    mutate(across(where(is.factor), as.character))
  index_data_ndvi <- index_data %>%
    select(-Spectra) %>%
    unnest(Indices) %>%
    select(-BandDefinition) %>%
    spread(Index, Value) %>%
    
    # Select useful columns
    select(DateTime,
           Date,
           Site,
           Block,
           Treatment,
           Replicate,
           FileNum,
           NDVI,
           EVI,
           EVI2) %>%
    
    # Create additional columns
    mutate(Year = lubridate::year(DateTime),
           DOY = lubridate::yday(DateTime)) %>%
    mutate(Block = as.numeric(str_extract(Block, "\\d"))) %>%
    mutate(Replicate = as.character(Replicate)) %>%
    mutate(collection_year = "Current") %>%
    
    # remove non-data (ref, dark, throwaway) scans
    filter(!str_detect(Treatment, "REF|DARK|THROWAWAY")) %>%
    # Standardize Site names from 2019 version to 2020 on wards
    mutate(Site = ifelse(Site %in% c("WSG1", "WSG23", "WSG"), "WSG89", Site))  %>%
    mutate(Site = ifelse(Site %in% c("DHT", "HTH", "HEATH"), "DHT89", Site)) %>%
    mutate(Site = ifelse(Site %in% c("MAT", "MAT-SH"), "MAT89", Site)) %>%
    mutate(Site = ifelse(Site %in% c("LMAT"), "MAT06", Site)) %>%
    mutate(Site = ifelse(Site %in% c("HIST", "HIST81"), "MAT81", Site)) %>%
    mutate(Site = ifelse(Site %in% c("SHB2", "SHB"), "SHB89", Site)) %>%
    mutate(Site = ifelse(Site %in% c("MNAT"), "MNT97", Site)) %>%
    mutate(Site = ifelse(Site %in% c("NANT", "NNT97"), "MNN97", Site))
  return(index_data_ndvi)
}