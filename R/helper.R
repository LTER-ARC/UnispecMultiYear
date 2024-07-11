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

# Function to rename the sites to standard names.

standard_site_names <- function(unispec_file) {
  # Standardize Site names from 2019 version to 2020 onwards
   unispec_file <- unispec_file %>%
    mutate(Site = ifelse(Site %in% c("WSG1", "WSG23", "WSG"), "WSG89", Site))  %>%
    mutate(Site = ifelse(Site %in% c("DHT", "HTH", "HEATH"), "DHT89", Site)) %>%
    mutate(Site = ifelse(Site %in% c("MAT", "MAT-SH"), "MAT89", Site)) %>%
    mutate(Site = ifelse(Site %in% c("LMAT","LOF"), "MAT06", Site)) %>%
    mutate(Site = ifelse(Site %in% c("HIST", "HIST81"), "MAT81", Site)) %>%
    mutate(Site = ifelse(Site %in% c("SHB2", "SHB", "SHRB"), "SHB89", Site)) %>%
    mutate(Site = ifelse(Site %in% c("MNAT"), "MNT97", Site)) %>%
    mutate(Site = ifelse(Site %in% c("NANT", "NNT97"), "MNN97", Site))
  return(unispec_file)
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
    # Standardize Site names from 2019 version to 2020 onwards
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

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Calculates NDVI, EVI, and EVI2 from dataframe !!That is not nested!! 
#  The dataframe' Spectra is unnested with Wavelength and Reflectance columns
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
calculate_indices2 <-
  ## inputs: spectra - dataframe with Wavelength, Reflectance columns
  ##         band_defns : dataframe defining wavelengths definining colors
  ##         instrument : e.g. MODIS, SKYE, ITEX
  ##         indicies   : the index or indices to return
  ## output: returns the dataframe with a list named Index (name of vegetation index
  ##         BandDefinition - name of "instrument" or spectral band definition used
  ##         Value - value of index, with the band definition used).
  function(spectra,
           band_defns = band_defns,
           instrument = "MODIS",
           indices = "NDVI") {
    
    # Color band definitions for calculate_indices function
    band_defns <- tribble(
      ~definition, ~color, ~min, ~max,
      "ITEX", "red", 560, 600,
      "ITEX", "nir", 725, 1000,
      "MODIS", "red", 620, 670, 
      "MODIS", "nir", 841, 876,
      "MODIS", "blue", 459,479,
      "SKYE", "red", 620, 680,
      "SKYE", "nir", 830, 880,
      "SKYE", "blue", 455, 480,
      "ToolikGIS_Drone_2018", "red", 640, 680,
      "ToolikGIS_Drone_2018", "nir", 820, 890,
      "ToolikGIS_MicaSense_2019", "blue", 455, 495,
      "ToolikGIS_MicaSense_2019", "green", 540, 580,
      "ToolikGIS_MicaSense_2019", "red", 658, 678,
      "ToolikGIS_MicaSense_2019", "red_edge", 707, 727,
      "ToolikGIS_MicaSense_2019", "near_ir", 800, 880,
      "ToolikEDC", "red", 560, 680,
      "ToolikEDC", "nir", 725, 1000
    )
    
    bands <- band_defns %>%
      filter(definition == instrument)
    
    blue <- bands %>% filter(color == "blue") %>% select(min, max) %>% as.numeric()
    nir <- bands %>% filter(color == "nir") %>% select(min, max) %>% as.numeric()
    red <- bands %>% filter(color == "red") %>% select(min, max) %>% as.numeric()
    
    spectra_bands <- spectra %>%
      mutate(color = ifelse(
        Wavelength >= blue[1] & Wavelength <= blue[2],
        "blue",
        ifelse(
          Wavelength >= red[1] & Wavelength <= red[2],
          "red",
          ifelse(Wavelength >= nir[1] &
                   Wavelength <= nir[2], "nir",
                 "other")
        )
      )) %>%
      group_by(Date,
               Site,
               Block,
               Treatment,
               Replicate,
              # spu_filename,
              # DateTime,
               color) %>%
      summarize(Reflectance = mean(Reflectance), .groups = "drop")
    
    index_data <- spectra_bands %>%
      spread(color, Reflectance) %>%
      
      ## INDEX DEFINITIONS
      mutate(NDVI = if("NDVI"%in% indices) signif((nir - red) / (nir + red), digits = 4),
             EVI = if("EVI" %in% indices) signif(2.5*((nir-red)/(nir+6*red-7.5*blue + 1)),digits = 4),
             EVI2 = if("EVI2" %in% indices)  signif(2.5 * ((nir - red) / (nir + 2.4 * red + 1)),digis = 4)) %>%
      
      # Add Spectral Band Definition convention
      select(-nir, -red, -other, -blue) %>%
      mutate(BandDefinition = instrument) %>%
      pivot_longer(cols =indices,names_to = "Index",values_to = "Value") %>% 
      nest(Indices =c("Index","BandDefinition","Value")) %>% 
      left_join({spectra %>% nest(Spectra =c("Wavelength","Reflectance"))},
                by = all_of(c("Date", "Site", "Block", "Treatment", "Replicate" 
                       ))) %>% 
      relocate(Indices,.after = last_col())
  }
