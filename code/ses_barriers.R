# packages ------------------------------------------------------------
library(sf)
library(tidyverse)
library(segregation)
library(patchwork)
library(classInt)
library(tmap)
library(RColorBrewer)

# preloading urbanization age data ----------------------------------------

source('code/lima_urbanization_lite.R', echo = FALSE)

# processing --------------------------------------------------------------

ses_2017 = st_read('data/raw/data_inei_2017/') |>
  mutate(NSE_PREDOM = factor(NSE_PREDOM, ordered = TRUE))

# formatting data for the `segregation` package
dat = ses_2017 |>
  gather(HOGNSE17, n, HOGNSEA17:HOGNSEE17) |> # data to long format
  select(c(OBJECTID_1, UBIGEO, DISTRITO, HOGNSE17, n)) |>
  st_drop_geometry() |> tibble()

# local segregation by district -- compares each block to its district
lima_ls17 = dat |>
  group_by(UBIGEO) |>
  group_modify(~ mutual_local(.x, 'HOGNSE17', 'OBJECTID_1',
                              weight = 'n', wide = TRUE))

# creating breaks for map
breaks_qt = classIntervals(c(min(lima_ls17$ls) - 0.0001,
                             lima_ls17$ls), n = 5, style = 'quantile')

# appending ls and p to geography and adding breaks
ses_2017 = ses_2017 |>
  left_join(lima_ls17) |>
  mutate(density = POB17/AREA,
    ls_cat = cut(ls, breaks_qt$brks))

# ses diversity: district vs city -----------------------------------------

# formatting the data
dat2 = dat |>
  group_by(UBIGEO, HOGNSE17) |>
  summarize(n = sum(n))

# calculating the ls scores
ls2 = mutual_local(dat2, 'HOGNSE17', 'UBIGEO',
                   weight = 'n', wide = TRUE)

# creating breaks for map
breaks_qt2 = classIntervals(c(min(ls2$ls) - 0.0001,
                             ls2$ls), n = 5, style = 'quantile')

# loading map of city and districts
distritos = st_read('data/raw/distritos/')
limametro = distritos |>
  filter(PROVINCIA %in% c('LIMA', 'CALLAO'))

# reshaping nse data to district level
distritos_nse = dat2 |>
  pivot_wider(names_from = HOGNSE17, values_from = n) |>
  mutate(NSE_PREDOM = case_when(
    pmax(HOGNSEA17, HOGNSEB17, HOGNSEC17, HOGNSED17, HOGNSEE17) == HOGNSEA17 ~ 'A',
    pmax(HOGNSEA17, HOGNSEB17, HOGNSEC17, HOGNSED17, HOGNSEE17) == HOGNSEB17 ~ 'B',
    pmax(HOGNSEA17, HOGNSEB17, HOGNSEC17, HOGNSED17, HOGNSEE17) == HOGNSEC17 ~ 'C',
    pmax(HOGNSEA17, HOGNSEB17, HOGNSEC17, HOGNSED17, HOGNSEE17) == HOGNSED17 ~ 'D',
    pmax(HOGNSEA17, HOGNSEB17, HOGNSEC17, HOGNSED17, HOGNSEE17) == HOGNSEE17 ~ 'E'))

# adding data
limametro_ls17 = limametro |>
  rename(UBIGEO = IDDIST) |>
  left_join(distritos_nse) |>
  left_join(ls2) |>
  mutate(ls_cat = cut(ls, breaks_qt2$brks, labels = c('low', 'medium low', 'medium', 'medium high', 'high')))

# circuity averages & number of barriers for 1km distance -----------------

# loading data from osm
buffers_data = st_read('data/processed/blocks_buffers.gpkg')
ubigeo = ses_2017 |> st_drop_geometry() |>
  select(OBJECTID_1, UBIGEO, density)
temp = left_join(buffers_data, ubigeo)
lima_buffers = left_join(temp, lima_ls17)

# adding urbanization age (sourced at the top) ----------------------------

lima_buffersage = left_join(lima_buffers,
                            st_drop_geometry(centroids_urbage)) |>
  mutate(
    pre1975 =
      ifelse(urb1975 %in% c('urban centre', 'dense urban cluster'), 1, 0),
    change19752020 =
      ifelse(!(urb1975 %in% c('urban centre', 'dense urban cluster')) &
               urb2020 %in% c('urban centre', 'dense urban cluster'), 1, 0),
    pre1990 =
      ifelse(urb1990 %in% c('urban centre', 'dense urban cluster'), 1, 0),
    change19902020 =
      ifelse(!(urb1990 %in% c('urban centre', 'dense urban cluster')) &
               urb2020 %in% c('urban centre', 'dense urban cluster'), 1, 0)
  )

# writing file for barriers analysis --------------------------------------

st_write(lima_buffersage, 'data/processed/blocks_data.gpkg',
         append = FALSE)

# collapsing data for survey analysis -------------------------------------

# NEEDS EDITING FROM HERE FOR USING AGE IN SURVEYS

distritos_barriers = lima_buffersage |>
  st_drop_geometry() |>
  group_by(DISTRITO, UBIGEO) |>
  summarize(circuity_avg_mean = mean(circuity_avg),
            intersection_mean = mean(intersection_count),
            barriers_00_mean = mean(barriers_buff0),
            barriers_01_mean = mean(barriers_buff1),
            barriers_03_mean = mean(barriers_buff3),
            barriers_05_mean = mean(barriers_buff5),
            barriers_10_mean = mean(barriers_buff10),
            dist_node_mean = mean(dist_node),
            pre1975_prop = sum(pre1975)/n(),
            pre1990_prop = sum(pre1990)/n(),
            change19752020_prop = sum(change19752020)/n(),
            change19902020_prop = sum(change19902020)/n()
            )

# adding nse and segregation by district

distritos_data = left_join(limametro_ls17, distritos_barriers,
                           by = "UBIGEO") |>
  select(-DISTRITO.x) |>
  rename(DISTRITO = DISTRITO.y) |>
  relocate(DISTRITO)

# visualization of collapsed data -----------------------------------------

pal = brewer.pal(5, 'YlGnBu')
map1 = distritos_data |>
  st_transform(epgs=3857) |> 
  tm_shape() +
  tm_borders('Gray', lwd = .1, alpha = .1) +
  tm_fill(col = 'pre1975_prop',
          alpha = .8,
          palette = pal,
          showNA = FALSE,
          style = "kmeans") +
  tm_layout(legend.outside = TRUE)
map1

# writing collapsed file --------------------------------------------------

st_write(distritos_data, 'data/processed/distritos_data.gpkg',
         append = FALSE)

