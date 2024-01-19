# packages and ancillary data ---------------------------------------------
library(terra)
library(sf)
library(tidyverse)
library(readr)

# sf objects for lima and block centroids

lima = st_read('data/raw/distritos/') |>
  filter(PROVINCIA %in% c('LIMA', 'CALLAO'))

ses_2017 = st_read('data/raw/data_inei_2017/')
block_centroids = ses_2017 |>
  select(geometry) |>
  st_centroid() |>
  bind_cols(OBJECTID_1 = ses_2017$OBJECTID_1,
            DISTRITO = ses_2017$DISTRITO,
            NSE_PREDOM = ses_2017$NSE_PREDOM,
            HIGH_SES = (ses_2017$HOGNSEA17 + ses_2017$HOGNSEB17) /
              ses_2017$HOG17) |>
  subset(NSE_PREDOM != 'NA') # removing blocks without SES data
original_crs = st_crs(block_centroids) # for writing file

# GHS-SMOD settlements data -----------------------------------------------

# settlements 2020

filepath = "data/raw/GHS_SMOD_E2020_GLOBE_R2022A_54009_1000_V1_0_R11_C11/GHS_SMOD_E2020_GLOBE_R2022A_54009_1000_V1_0_R11_C11.tif"
s2020_raw = rast(filepath)

# cropping and masking
lima = st_transform(lima, crs(s2020_raw)) # aligning projections
s2020_cropped = crop(s2020_raw, lima)
s2020 = mask(s2020_cropped, vect(lima))

# settlements 2005

filepath = "data/raw/GHS_SMOD_E2005_GLOBE_R2022A_54009_1000_V1_0_R11_C11/GHS_SMOD_E2005_GLOBE_R2022A_54009_1000_V1_0_R11_C11.tif"
s2005_raw = rast(filepath)

# cropping and masking
lima = st_transform(lima, crs(s2005_raw)) # aligning projections
s2005_cropped = crop(s2005_raw, lima)
s2005 = mask(s2005_cropped, vect(lima))

# settlements 1990

filepath = "data/raw/GHS_SMOD_E1990_GLOBE_R2022A_54009_1000_V1_0_R11_C11/GHS_SMOD_E1990_GLOBE_R2022A_54009_1000_V1_0_R11_C11.tif"
s1990_raw = rast(filepath)

# cropping and masking
lima = st_transform(lima, crs(s1990_raw)) # aligning projections
s1990_cropped = crop(s1990_raw, lima)
s1990 = mask(s1990_cropped, vect(lima))

# settlements 1975

filepath = "data/raw/GHS_SMOD_E1975_GLOBE_R2022A_54009_1000_V1_0_R11_C11/GHS_SMOD_E1975_GLOBE_R2022A_54009_1000_V1_0_R11_C11.tif"
s1975_raw = rast(filepath)

# cropping and masking
lima = st_transform(lima, crs(s1975_raw)) # aligning projections
s1975_cropped = crop(s1975_raw, lima)
s1975 = mask(s1975_cropped, vect(lima))

# extracting --------------------------------------------------------------

# aligning projections
block_centroids = st_transform(block_centroids, crs(s2020))

# step 1: 2020
urb2020 = terra::extract(s2020, vect(block_centroids))
centroids_urb1 = cbind(block_centroids, urb2020) |>
  rename(urb2020 = GHS_SMOD_E2020_GLOBE_R2022A_54009_1000_V1_0_R11_C11)|>
  select(-ID)

# step 2: 2005
urb2005 = terra::extract(s2005, vect(block_centroids))
centroids_urb2 = cbind(centroids_urb1, urb2005) |>
  rename(urb2005 = GHS_SMOD_E2005_GLOBE_R2022A_54009_1000_V1_0_R11_C11)|>
  select(-ID)

# step 3: 1990
urb1990 = terra::extract(s1990, vect(block_centroids))
centroids_urb3 = cbind(centroids_urb2, urb1990) |>
  rename(urb1990 = GHS_SMOD_E1990_GLOBE_R2022A_54009_1000_V1_0_R11_C11) |>
  select(-ID)

# step 4: 2020
urb1975 = terra::extract(s1975, vect(block_centroids))
centroids_urb4 = cbind(centroids_urb3, urb1975) |>
  rename(urb1975 = GHS_SMOD_E1975_GLOBE_R2022A_54009_1000_V1_0_R11_C11)|>
  select(-ID)

# combining all
centroids_urbage = centroids_urb4 |>
  mutate(across(urb2020:urb1975,
                ~ factor(.x,
                         levels = c(10, 11, 12, 13, 21, 22, 23, 30),
                         labels = c('water',
                                    'very low density rural',
                                    'low density rural',
                                    'rural cluster',
                                    'suburban or peri-urban',
                                    'semi-dense urban cluster',
                                    'dense urban cluster',
                                    'urban centre'),
                         ordered = TRUE)))
rm(list = setdiff(ls(), 'centroids_urbage'))
