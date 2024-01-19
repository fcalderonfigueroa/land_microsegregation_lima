# packages ----------------------------------------------------------------

library(haven)
library(tidyverse)
library(sf)

# wrangling ---------------------------------------------------------------

# loading data
raw1019 = read_sav("data/raw/BD_EncuestaLCV/BD_Lima2010-2019.sav")
head(raw1019)

# selecting and processing vars
lcv1019 = raw1019 |>
  mutate(
    year = as.numeric(as.character(as_factor(zap_label(SERIE)))),
    id = as.numeric(INDICE),
    petition = ifelse(PC3A == 1, 1, 0),
    mass_media_complaining = ifelse(PC3B == 1, 1, 0),
    neighbourhood_org = ifelse(PC3C == 1, 1, 0), # if needed add NA condition to deal with 1 NA in original column: & !is.na(PC3C)
    elected_position = ifelse(PC3D == 1, 1, 0),
    volunteering = ifelse(PC3E == 1, 1, 0),
    participatory_budget = ifelse(PC3F == 1, 1, 0),
    membership = ifelse(PC3G == 1, 1, 0), # only 2015-2019
    demonstrating = ifelse(PC3H == 1, 1, 0), # only 2015-2019
    online_groups = ifelse(PC3I == 1, 1, 0), # only 2017-2019
    social_media_complaining = ifelse(PC3J == 1, 1, 0), # 2017-2019
    community_project = ifelse(PC3K == 1, 1, 0), # 2018-2019
    female = ifelse(as_factor(SEXO) == 'Mujer', 1, 0),
    age = as.numeric(ANIOS),
    UBIGEO = as.character(UBIGEO),
    district = as_factor(zap_label(UBIGEO)),
    borough = as_factor(zap_label(ESTRATOS_IOP)),
    ses_factor = as.numeric(as_factor(zap_label(NSE_IOP1))),
    ses_rev = min(ses_factor, na.rm = TRUE) - ses_factor + 
      max(ses_factor, na.rm = TRUE), # reversed to ascending order
    lowered = ifelse(DG1 %in% c(1:3), 1, 0), # only 2010-2016, up to secondary school (complete or not)
    old_timer = ifelse(EG2 == 1 | EG2OTRO>=10, 1, 0), # 10 years or more
    safety_lima = as.numeric(na_if(EG4, 9)-1), # recoded into 0 to 4
    workstudy_outside = ifelse(MT1 == 1 | MT1 == 2 | MT1 == 3, 1, 0),
    works_home = ifelse(MT1 == 4 | MT1 == 6, 1, 0),# works from home | household chores 
    works_not = ifelse(MT1 == 5 | MT1 == 7, 1, 0), # unemployed or retired
    walk_bike = ifelse(MT3 %in% c(1,2), 1, 0),
    transit = ifelse(MT3 %in% c(5,6,10:13), 1, 0),
    drive_or_taxi = ifelse(MT3 %in% c(3,8,9), 1, 0),
    commute_min = as.numeric(MT21TOTAL), # only 2015-2019
    safety_hood = as.numeric(na_if(VS1, 9)-1),
    victimization = ifelse(VS5A == 1 | VS5B == 1 | VS5C == 1 |
                             VS5D == 1 | VS5E == 1, 1, 0), # all years
    fences_supporter = ifelse(`VS10$1` == 5 | `VS10$2` == 5 |
                                `VS10$3` == 5, 1, 0), # only 2018
    children_under15 = ifelse(EP4 < 5, 1, 0), # not 2015, 2017, 2019
    trust_neighbours = as.numeric(na_if(RC1, 9)-1), # only 2010-2014
    civiceng_core = ifelse(PC3A == 1 | PC3B == 1 | 
                          PC3C == 1 | PC3D == 1 |
                          PC3E == 1, 1, 0), # vars in all years
    civiceng_allvars = ifelse(PC3A == 1 | PC3B == 1 | PC3C == 1 |
                             PC3D == 1 | PC3E == 1 | PC3F == 1 |
                             PC3G == 1 | PC3H == 1 | PC3I == 1 |
                             PC3J == 1 | PC3K == 1,
                           1, 0), # includes 2015-19 vars, extra NAs
    .keep = 'none'
    )

# adding contextual data --------------------------------------------------

districts = st_read('data/processed/distritos_data.gpkg')
contextual = st_drop_geometry(districts) |>
  select(-c(IDDPTO:PROVINCIA), -c(CAPITAL:FUENTE))

lcv1019_ctx = left_join(lcv1019, contextual)

# writing analytic file ---------------------------------------------------

write_csv(lcv1019_ctx, 'data/processed/lcv1019_ctx.csv',
          append = FALSE)
# note that this file has no geography.