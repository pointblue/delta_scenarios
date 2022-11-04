# ECONOMIC METRICS
# includes both benefits to landowners (crop production values) and to workers
# (ag jobs and wages)

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')

# key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')
key = readxl::read_excel('GIS/VEG_key.xlsx')

# CROP PRODUCTION VALUE------------
#
# Crop production values were obtained from the USDA NASS database, source:
# https://www.nass.usda.gov/Statistics_by_State/California/Publications/AgComm/index.php.
# - 2014-2019 data
# - crop production value for rangelands/grasslands based on forage production,
# but note contribution to livestock production (and more value elsewhere)
# - gross production value does not take into account production costs, which
# are often higher for high value crops


## compile raw data-----
cropdat_raw = purrr::map_dfr(
  paste0('data_orig/AgComm/', list.files('data_orig/AgComm')),
  read_csv,
  col_types = cols('Crop Name' = 'f',
                   Unit = 'f')
  ) %>%
  filter(County %in% c('Sacramento', 'San Joaquin', 'Yolo', 'Contra Costa',
                       'Solano')) %>% #leave out Alameda - mostly not in Delta
  # combine new columns from 2020
  mutate(price_per_unit = case_when(!is.na(`Price P/U`) ~ `Price P/U`,
                                    !is.na(`Price (Dollars/Unit)`) ~ `Price (Dollars/Unit)`,
                                    TRUE ~ `Price P/U`),
         value = case_when(!is.na(Value) ~ Value,
                           !is.na(`Value (Dollars)`) ~ `Value (Dollars)`,
                           TRUE ~ Value)) %>%
  select(year = Year, crop = `Crop Name`, county = County,
         harvest_ac = `Harvested Acres`, #may be << total acres
         production = Production, # total number of "units" produced
         unit = Unit,
         price_per_unit, #$ per production unit
         value #dollars = price per unit * production
         ) %>%
  filter(!grepl('apiary|cattle|chicken|fish|livestock|manure|milk|nursery|poultry|flowers foliage|flowering & foliage|sheep|turkey|wool|firewood',
                crop, ignore.case = TRUE))

## classify landcovers----------
# match to land cover classes & subclasses as in baseline

cropdat_classify = codify_agstats(cropdat_raw)


## estimate avg annual value per ha----------
# note that we can't get value per acre simply by dividing total value by total
# acreage because we're using total values for entire counties, only a portion
# of which are actually in the Delta proper; instead, follow the method used in
# the State of Delta Agriculture report, to apply the average yield per acre to
# Delta acres

cropdat_county = cropdat_classify %>%
  # sum over all crops within SUBCLASS
  group_by(county, CODE_NAME, SUBCLASS, year) %>%
  summarize(across(c(harvest_ac, production, value), sum, na.rm = TRUE),
            .groups = 'drop')

cropdat_sum = cropdat_classify %>%
  # sum over all Delta counties in each year
  group_by(CODE_NAME, SUBCLASS, year) %>%
  summarize(across(c(harvest_ac, production, value), sum, na.rm = TRUE),
            .groups = 'drop')
write_csv(cropdat_sum, 'data_orig/cropdat_summary.csv')

## calculate average production value------
# $1000 per ha of each land cover class and subclass, averaged over all 5 years

## by subclass:
cropdat_subclass = cropdat_sum %>%
  # calculate (weighted) average production value per acre per subclass
  mutate(harvest_ha = harvest_ac / 2.47105,
         value_ha = value / harvest_ha) %>%
  group_by(CODE_NAME, SUBCLASS) %>%
  #average over all years (and convert from thousands to millions)
  summarize(SCORE_MEAN = mean(value_ha, na.rm = TRUE) / 1000,
            SCORE_SE = sd(value_ha, na.rm = TRUE)/sqrt(length(!is.na(value_ha))) / 1000,
            SCORE_MIN = min(value_ha, na.rm = TRUE) / 1000,
            SCORE_MAX = max(value_ha, na.rm = TRUE) / 1000,
            .groups = 'drop') %>%
  mutate(METRIC_CATEGORY = 'economy',
         METRIC_SUBTYPE = 'production value',
         METRIC = 'Gross Production Value',
         UNIT = 'USD per ha per year (millions)') %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, CODE_NAME, SUBCLASS,
         SCORE_MEAN, SCORE_SE, SCORE_MIN, SCORE_MAX, UNIT)
write_csv(cropdat_subclass, 'data/crop_production_value_subclass.csv')

## by class:
cropdat_class = cropdat_sum %>%
  group_by(CODE_NAME, year) %>%
  summarize(across(c(harvest_ac, value), sum), .groups = 'drop') %>%
  # calculate (weighted) average production value per acre per class
  mutate(harvest_ha = harvest_ac / 2.47105,
         value_ha = value / harvest_ha) %>%
  #average over all years (and convert to millions)
  group_by(CODE_NAME) %>%
  summarize(SCORE_MEAN = mean(value_ha) / 1000,
            SCORE_SE = sd(value_ha, na.rm = TRUE)/sqrt(length(!is.na(value_ha))) / 1000,
            SCORE_MIN = min(value_ha) / 1000,
            SCORE_MAX = max(value_ha) / 1000,
            .groups = 'drop') %>%
  mutate(METRIC_CATEGORY = 'economy',
         METRIC_SUBTYPE = 'production value',
         METRIC = 'Gross Production Value',
         UNIT = 'USD per ha per year (millions)') %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, CODE_NAME, SCORE_MEAN,
         SCORE_SE, SCORE_MIN, SCORE_MAX, UNIT)

# check for missing:
cropdat_class %>% select(METRIC, CODE_NAME, SCORE_MEAN) %>%
  pivot_wider(names_from = 'METRIC', values_from = 'SCORE_MEAN') %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = key %>% pull(CODE_NAME))) %>%
  complete(CODE_NAME) %>% print(n = 40)
# IDLE, URBAN, RIPARIAN, WETLAND_MANAGED, WETLAND_OTHER, WATER, WOODLAND&SCRUB,
# BARREN: assume zero crop production value

cropdat_class_fill = cropdat_class %>%
  bind_rows(
    expand_grid(
      tibble(CODE_NAME = c('IDLE', 'URBAN', 'RIPARIAN', 'RIPARIAN_FOREST_POFR',
                           'RIPARIAN_FOREST_QULO', 'RIPARIAN_FOREST_SALIX',
                           'RIPARIAN_FOREST_MIXED', 'RIPARIAN_SCRUB_INTRO',
                           'RIPARIAN_SCRUB_SALIX', 'RIPARIAN_SCRUB_MIXED',
                           'WETLAND_MANAGED', 'WETLAND_MANAGED_PERENNIAL',
                           'WETLAND_MANAGED_SEASONAL', 'WETLAND_TIDAL',
                           'WETLAND_OTHER', 'WATER',
                           'WOODLAND', 'SCRUB', 'BARREN')),
      cropdat_class %>% filter(CODE_NAME == 'GRASSLAND') %>%
        mutate(SCORE_MEAN = 0, SCORE_SE = NA, SCORE_MIN = NA, SCORE_MAX = NA) %>%
        select(-CODE_NAME)
    )
  )
# check for missing:
cropdat_class_fill %>% select(METRIC, CODE_NAME, SCORE_MEAN) %>%
  pivot_wider(names_from = 'METRIC', values_from = 'SCORE_MEAN') %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = key %>% pull(CODE_NAME))) %>%
  complete(CODE_NAME) %>% print(n = 40)

write_csv(cropdat_class_fill, 'data/crop_production_value.csv')

# summary plots
cropdat_class_fill %>%
  mutate(CODE_NAME = factor(
    CODE_NAME,
    levels = cropdat_class_fill %>% arrange(SCORE_MEAN) %>% pull(CODE_NAME))) %>%
  ggplot(aes(SCORE_MEAN, CODE_NAME, xmin = SCORE_MEAN-SCORE_SE, xmax = SCORE_MEAN+SCORE_SE)) +
  geom_col() + geom_errorbar(width = 0) +
  labs(x = 'value ($/ha, millions)', y = NULL)
# highest gross production value in ORCHARD_DECIDUOUS, ROW, VINEYARD
# ORCHARD_CITRUS&SUBTROPICAL

cropdat_subclass %>%
  mutate(SUBCLASS = factor(SUBCLASS,
                           levels = cropdat_subclass %>% arrange(SCORE_MEAN) %>%
                             pull(SUBCLASS))) %>%
  # filter(CODE_NAME %in% c('ORCHARD_DECIDUOUS', 'VINEYARD', 'ROW')) %>%
  ggplot(aes(SCORE_MEAN/1000, SUBCLASS, xmin = (SCORE_MEAN-SCORE_SE)/1000,
             xmax = (SCORE_MEAN+SCORE_SE)/1000)) +
  geom_col() + geom_errorbar(width = 0) +
  labs(x = 'value ($/ha, millions)', y = NULL)
# within these classes, highest rates for BUSH BERRIES and STRAWBERRIES


# ## compare to report---------
# # The State of Delta Agriculture
# library(tabulizer)
# report_acres = extract_tables(
#   'data_orig/Ag-ESP-update-agricultural-trends-FINAL-508.pdf',
#   pages = 9, output = 'data.frame')[[1]] %>%
#   slice(3:15) %>%
#   rlang::set_names(c('Crop_Category', 'Alameda', 'Contra Costa', 'Sacramento',
#               'San Joaquin', 'Solano', 'Yolo', 'Total')) %>%
#   select(Crop_Category, Acres = Total) %>%
#   mutate(
#     Crop_Category = case_when(
#       Crop_Category == 'Truck, Nursery &' ~ 'Truck, Nursery & Berry Crops',
#       Crop_Category == 'Deciduous Fruit &' ~ 'Deciduous Fruit & Nuts',
#       Crop_Category == '' ~ 'Citrus & Subtropical',
#       TRUE ~ Crop_Category)) %>%
#   filter(!Acres == '') %>%
#   mutate(Acres = gsub(',', '', Acres),
#          Acres = as.numeric(Acres),
#          ha = Acres / 2.47105)
# # very similar to baseline_area$CLASS_AREA estimates, with increases in
# # ORCHARD_DECIDUOUS since 2016
#
# report_revenue = extract_tables(
#   'data_orig/Ag-ESP-update-agricultural-trends-FINAL-508.pdf',
#   pages = 21, output = 'data.frame')[[1]] %>%
#   slice(2:14) %>%
#   select(Crop_Category = Crop.Category, Revenue = TOTAL) %>%
#   mutate(
#     Crop_Category = case_when(
#       Crop_Category == 'Deciduous Fruit' ~ 'Deciduous Fruit & Nuts',
#       Crop_Category == 'Citrus &' ~ 'Citrus & Subtropical',
#       Crop_Category == 'Grain & Hay' ~ 'Grain & Hay Crops',
#       Crop_Category == 'Truck, Nursery &' ~ 'Truck, Nursery & Berry Crops',
#       TRUE ~ Crop_Category
#     )
#   ) %>%
#   filter(!Revenue == '') %>%
#   mutate(Revenue = gsub(',', '', Revenue),
#          Revenue = as.numeric(Revenue) * 1000) %>% #reported in thousands
#   left_join(report_acres) %>%
#   mutate(revenue_ha = Revenue/ha)
# # fairly similar -- correct order of magnitude and general rankings
# # - lower in the report: ORCHARD_DECIDUOUS, RICE
# # - higher in the report: ROW/TRUCK, CITRUS, VINEYARD
# # - rest generally in the right range

# "Truck crops have high revenue per acre, as this category generates about
# one-third of total Delta crop revenue on about one-eighth of Delta crop land.
# Vineyards are the second highest category at more than $200 million, while
# Deciduous Fruit and Nuts is third at nearly $120 million in revenue in 2016.
# Field and pasture crops also are important contributors with each over $100
# million in revenue, but these lower value crop categories are found on over
# half the Delta crop acreage. "


# JOBS & WAGES-------
# Data on average monthly employees & average weekly wages for CA, 2015-2019,
# for Sacramento, San Joaquin, Solano, Yolo, <Alameda>, Contra Costa counties
#
# Source: CA Employment Development Dept
# https://data.edd.ca.gov/Industry-Information-/Quarterly-Census-of-Employment-and-Wages-QCEW-/fisq-v939/data
#
# Delta Plan performance measures website uses "Regional opportunity index (ROI)
# for People and Place
# (https://interact.regionalchange.ucdavis.edu/roi/index.html) - combines
# metrics for "Civic Life", "Health/Env", "Housing", "Economy" and "Education"
# --> a lot of census data, but includes air quality (annual mean concentration of PM2.5),
# job availability (# of jobs/1000 people within 5miles), job quality (% high paying jobs within 5miles)
# --> what counts as "high paying"??
#
# report from UC Davis researchers on "a number of socioeconomic indicators for the Delta":
# (https://regionalchange.ucdavis.edu/sites/g/files/dgvnsk986/files/inline-files/Delta-SocioEconomic-Indicators-FINAL-m_1.pdf)
# - notes tourism benefits of wineries/orchards, higher economic returns

## compile raw data------
edat_raw <- read_csv('data_orig/Quarterly_Census_of_Employment_and_Wages__QCEW_2014-2020.csv',
                 col_types = cols()) %>%
  # filter(Quarter == 'Annual') %>%
  # # as above, leave out Alameda county data, because only a sliver in Delta proper
  # filter(`Area Name` %in%
  #          c('Sacramento County', 'San Joaquin County', 'Yolo County',
  #            'Contra Costa County', 'Solano County')) %>%
  select(county = `Area Name`,
         year = Year,
         industry = `Industry Name`,
         est = Establishments,
         n_employees_monthly = `Average Monthly Employment`,
         wages_total = `Total Wages (All Workers)`,
         # wages_weekly = `Average Weekly Wages`,
         NAICS_level = `NAICS Level`,
         NAICS_code = `NAICS Code`) %>%
  mutate(county = gsub(' County', '', county))


# subset to relevant industries:

# NAICS levels & codes:
# level 3 = sector/subsector
# level 4 = industry group
# level 5 = industry
# level 6 = national industry

# find relevant sectors:
edat_raw %>% filter(NAICS_level == 3) %>% select(NAICS_code, industry) %>%
  distinct() %>% arrange(NAICS_code) %>% print(n = 95)

# create table of potentially relevant codes:
# 111 = crop production
# 112 = animal production
# 115 = ag/forestry support activities (soil prep, farm labor mgmt)

edat_table = edat_raw %>%
  filter(NAICS_level == 6 & grepl('^111', NAICS_code)) %>%
  # filter(grepl('^111|^1151|^31213|^487|^712|^72121|^81331|^924', NAICS_code)) %>%
  select(NAICS_level, NAICS_code, industry) %>%
  distinct() %>% arrange(NAICS_code)

# subset to relevant industries:
edat_subset = edat_raw %>%
  filter(NAICS_code %in%
           c(111120, 111140, 111150, 111160, 111191, 111199, #oilseed & grain (including rice, corn)
             111219, #vegetable/melon
             111331, 111332, 111335, 111336, 111339, #fruit & tree nut
             111940, #hay
             111998 #other misc crops
             # 11511, #support activities for crop production
             # 312130, #wineries
             # 487, #sightseeing transportation
             # 712190, #nature parks/similar
             # 72121, #RV Parks/recreational camps
             # 813312, #env/conservation orgs
             # 924120 #conservation programs
           ))

## classify landcovers---------
edat_classify = edat_subset %>% codify_edd() %>%
  select(CODE_NAME, SUBCLASS, industry, county, year, est:wages_total)

## estimate avg annual wages and jobs per ha----------
# as above, note that we can't get the number of jobs per ha simply by dividing
# total jobs by total area in the baseline map because we're using total values
# for entire counties, only a portion of which are actually in the Delta proper;
# instead, use harvested acres from crop production value data above

# need to align acreage info to subclasses from edat -- note county-specific and
# year-specific variation in reporting?
cropdat_county %>% filter(county == 'Sacramento') %>%
  select(county:harvest_ac) %>%
  pivot_wider(names_from = year, values_from = harvest_ac) %>% print(n = Inf)
edat_classify %>% filter(county == 'Sacramento') %>%
  select(county, CODE_NAME:SUBCLASS, year, n = n_employees_monthly) %>%
  group_by(county, CODE_NAME, SUBCLASS, year) %>% summarize(n = sum(n)) %>%
  arrange(county, year, CODE_NAME, SUBCLASS) %>%
  pivot_wider(names_from = year, values_from = n)
# Sacramento:
#   OTHER: in edat every year; assume it includes all FIELD_OTHER, WHEAT, plus
#      years missing from CORN, GRAIN&HAY_OTHER, RICE below?
#   CORN: no edat for 2018-19, but there is harvest dat
#   GRAIN&HAY_OTHER: no edat for 2014 or 2017-2020, but there is harvest dat
#   RICE: no edat for 2014-2017 or 2020, but there is harvest dat
#   ORCHARD_DECIDUOUS: MISC or TREE NUTS in every year (combine)
#   GRASSLAND in harvest dat - include in PASTURE (ALL TYPES)? (combine)
#   ROW & VINEYARD are ok
# Yolo:
#   OTHER: in edat every year; assume it includes all GRAIN&HAY,
#     ORCHARD_CITRUS&SUBTROPICAL, plus missing years for CORN, OILSEEDS
#   every year: PASTURE (include GRASSLAND?), RICE, ROW, VINEYARD, TREE NUTS
#   ORCHARD_DECIDUOUS (2014-2017) seems to switch to MISC_DECIDUOUS (2018-2020)
#     (except both reported in 2014)
#   FIELD_CORN: edat only 2018 but harvest dat every year
#   OILSEEDS: only 2014-2017, but harvest dat every year
# San Joaquin:
#   OTHER: in edat every year but 2015; assume it includes FIELD_OTHER, GRAIN&HAY,
#    GRASSLAND, ORCHARD_CITRUS&SUBTROPICAL; PASTURE; ROW
#   every year: CORN, ORCHARD_DECIDUOUS (including mixed tree/nut); RICE, VINEYARD
# Solano:
#   OTHER: in edat every year; assume it includes all CORN, FIELD_OTHER, GRAIN&HAY_OTHER,
#     ORCHARD_CITRUS&SUBTROPICAL, and missing years for ROW?
#   every year: WHEAT, ORCHARD_DECIDUOUS, PASTURE (including GRASSLAND?), VINEYARD
#   ROW: edat only 2014
# Contra Costa:
#   OTHER: only in 2014
#   VINEYARD: missing 2015-2017
#   ORCHARD_DECIDUOUS: missing 2014
#   ROW: missing 2014
#   --> MISSING DATA FOR CORN, FIELD_OTHER, GRAIN&HAY, GRASSLAND, PASTURE,
#       ORCHARD_CITRUS&SUBTROPICAL, and missing years for ROW
# Alameda: no edat or cropdat

##--> match county-year-CODE_NAME specific data to estimate average numbers of
## jobs per ha of each land cover class (assuming it wasn't always reported every
## year)
edat_align = full_join(
  cropdat_county %>%
    mutate(
      CODE_NAME = case_when(
        CODE_NAME %in% c('PASTURE_ALFALFA', 'PASTURE_OTHER') ~ 'PASTURE',
        TRUE ~ CODE_NAME
      )) %>%
    group_by(county, year, CODE_NAME) %>%
    summarize(harvest_ac = sum(harvest_ac),
              .groups = 'drop'),
  edat_classify %>% group_by(county, year, CODE_NAME) %>%
    summarize(n_employees_monthly = sum(n_employees_monthly),
              wages_total = sum(wages_total),
            .groups = 'drop'),
  by = c('county', 'year', 'CODE_NAME'))
write_csv(edat_align, 'data_orig/employment_summary.csv')

edat_sum = edat_align %>% drop_na() %>%
  mutate(harvest_ha = harvest_ac / 2.47105,
         jobs_ha = n_employees_monthly / harvest_ha,
         wages_yr = wages_total / n_employees_monthly)

# annual variation?
edat_sum %>%
  select(county, CODE_NAME:year, jobs_ha, wages_yr) %>%
  mutate(jobs_ha = jobs_ha * 100,
         wages_yr = wages_yr / 1000) %>%
  pivot_longer(jobs_ha:wages_yr) %>%
  # filter(county != 'Contra Costa' & county != 'Solano') %>%
  # summarize overall counties in each year
  group_by(CODE_NAME, year, name) %>%
  summarize(SCORE_MEAN = mean(value),
            SCORE_SE = sd(value, na.rm = TRUE)/sqrt(length(!is.na(value))),
            .groups = 'drop') %>%
  ggplot(aes(year, SCORE_MEAN, color = CODE_NAME)) +
  facet_wrap(~name, scales = 'free_y') +
  geom_line() + geom_point() +
  # geom_errorbar(aes(ymin = SCORE_MEAN - SCORE_SE, ymax = SCORE_MEAN + SCORE_SE),
  #               width = 0.25) +
  scale_color_viridis_d()
# --> suspicious bump in ORCHARD_DECIDUOUS in 2016 (due to one record in Contra
# Costa); wages overall climbing between 2014 and 2020, except for a
# suspiciously high peak in WHEAT in 2014 (one record from Solano county)

## calculate averages------
# employees per ha and wages per employee for each land cover class, averaged
# over all counties and years

edat_class = edat_sum %>%
  # drop outliers and suspicious values
  filter(!(CODE_NAME == 'GRAIN&HAY_WHEAT' & county == 'Solano' & year == 2014)) %>%
  filter(!(CODE_NAME == 'ORCHARD_DECIDUOUS' & county == 'Contra Costa' & year == 2016)) %>%
  filter(!(CODE_NAME == 'ROW' & county == 'Contra Costa')) %>%
  select(county, year, CODE_NAME, jobs_ha, wages_yr) %>%
  pivot_longer(jobs_ha:wages_yr, names_to = 'METRIC') %>%
  group_by(METRIC, CODE_NAME) %>%
  summarize(SCORE_MEAN = mean(value),
            SCORE_SE = sd(value, na.rm = TRUE)/sqrt(length(!is.na(value))),
            SCORE_MIN = min(value),
            SCORE_MAX = max(value),
            .groups = 'drop') %>%
  mutate(METRIC_CATEGORY = 'economy',
         METRIC_SUBTYPE = 'livelihoods',
         UNIT = case_when(METRIC == 'jobs_ha' ~ 'number of employees per ha',
                          METRIC == 'wages_yr' ~ 'annual dollars per employee'),
         METRIC = recode(METRIC,
                         jobs_ha = 'Agricultural Jobs',
                         wages_yr = 'Annual Wages')) %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, CODE_NAME,
         SCORE_MEAN, SCORE_SE, SCORE_MIN, SCORE_MAX, UNIT) %>%
  arrange(METRIC)

ggplot(edat_class, aes(SCORE_MEAN, CODE_NAME)) + facet_wrap(~METRIC, scales = 'free_x') +
  geom_col() +
  geom_errorbar(aes(xmin = SCORE_MEAN - SCORE_SE, xmax = SCORE_MEAN + SCORE_SE),
                width = 0.25)

edat_align %>% filter(CODE_NAME == 'ORCHARD_DECIDUOUS') %>%
  filter(!(CODE_NAME == 'ORCHARD_DECIDUOUS' & county == 'Contra Costa' &
             year == 2016)) %>%
  mutate(jobs_ha = n_employees_monthly / harvest_ac) %>%
  select(county, year, CODE_NAME, jobs_ha) %>%
  pivot_wider(names_from = county, values_from = jobs_ha)
# --> Sacramento county employs a lot more workers per ha in orchards than other
# counties (?)

# check for missing:
edat_class %>% select(METRIC, CODE_NAME, SCORE_MEAN) %>%
  pivot_wider(names_from = 'METRIC', values_from = 'SCORE_MEAN') %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = key %>% pull(CODE_NAME))) %>%
  complete(CODE_NAME) %>% print(n = 40)
# ORCHARD_CITRUS&SUBTROPICAL -- assume same as other orchard
# PASTURE -- assume overall value applies to both PASTURE_ALFALFA and
# PASTURE_OTHER (assume equivalent)

# IDLE, GRASSLAND, URBAN, RIPARIAN, WETLAND_MANAGED, WETLAND_OTHER, WATER,
# WOODLAND&SCRUB, BARREN -- assume these provide zero ag jobs -- BUT note that
# grassland does have a crop production value

edat_class_fill = edat_class %>%
  bind_rows(
    edat_class %>% filter(CODE_NAME == 'ORCHARD_DECIDUOUS') %>%
      mutate(CODE_NAME = 'ORCHARD_CITRUS&SUBTROPICAL'),
    edat_class %>% filter(CODE_NAME == 'PASTURE') %>%
      mutate(CODE_NAME = 'PASTURE_ALFALFA'),
    edat_class %>% filter(CODE_NAME == 'PASTURE') %>%
      mutate(CODE_NAME = 'PASTURE_OTHER'),
    expand_grid(
      tibble(CODE_NAME = c('IDLE', 'GRASSLAND', 'URBAN', 'RIPARIAN', 'RIPARIAN_FOREST_POFR',
                           'RIPARIAN_FOREST_QULO', 'RIPARIAN_FOREST_SALIX',
                           'RIPARIAN_FOREST_MIXED', 'RIPARIAN_SCRUB_INTRO',
                           'RIPARIAN_SCRUB_SALIX', 'RIPARIAN_SCRUB_MIXED',
                           'WETLAND_MANAGED', 'WETLAND_MANAGED_PERENNIAL',
                           'WETLAND_MANAGED_SEASONAL', 'WETLAND_TIDAL',
                           'WETLAND_OTHER', 'WATER', 'WOODLAND', 'SCRUB', 'BARREN')),
      edat_class %>% filter(CODE_NAME == 'VINEYARD') %>%
        mutate(SCORE_MEAN = 0, SCORE_SE = NA, SCORE_MIN = NA, SCORE_MAX = NA) %>%
        select(-CODE_NAME)
    )
  ) %>%
  filter(CODE_NAME != 'PASTURE') #drop general pasture entry

edat_class_fill %>% select(METRIC, CODE_NAME, SCORE_MEAN) %>%
  pivot_wider(names_from = 'METRIC', values_from = 'SCORE_MEAN') %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = key %>% pull(CODE_NAME))) %>%
  complete(CODE_NAME) %>% print(n = 40)

# FINALIZE UNITS
edat_class_final = edat_class_fill %>%
  mutate(across(c(SCORE_MEAN, SCORE_SE, SCORE_MIN, SCORE_MAX),
                ~if_else(METRIC == 'Agricultural Jobs', .x * 100, .x / 1000)),
         UNIT = if_else(METRIC == 'Agricultural Jobs',
                        'number of employees per 100ha',
                        'annual dollars per employee (thousands)'))

write_csv(edat_class_final, 'data/livelihoods.csv')


# summary plots:
edat_class_final %>%
  mutate(CODE_NAME = factor(
    CODE_NAME,
    levels = edat_class_fill %>% filter(METRIC == 'Agricultural Jobs') %>%
      arrange(SCORE_MEAN) %>% pull(CODE_NAME))) %>%
  ggplot(aes(SCORE_MEAN, CODE_NAME, xmin = SCORE_MEAN-SCORE_SE, xmax = SCORE_MEAN+SCORE_SE)) +
  geom_col() + geom_errorbar(width = 0) + facet_wrap(~METRIC, scales = 'free_x')
