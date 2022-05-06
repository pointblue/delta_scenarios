# ECONOMIC METRICS
# includes both benefits to landowners (crop production values) and to workers
# (ag jobs and wages)

# PACKAGES & FUNCTIONS
source('R/packages.R')


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

cropdat_classify = cropdat_raw %>%
  # assign to landcover classes, matching major classes as in baseline
  mutate(
    crop = as.character(crop),
    CODE_NAME = case_when(
      grepl('almond|apple|apricot|cherr|fruits & nuts|nectarines|orchard',
            crop, ignore.case = TRUE) ~ 'ORCHARD_DECIDUOUS',
      grepl('orchard|peaches|pears|plums|walnuts|pistachios',
            crop, ignore.case = TRUE) ~ 'ORCHARD_DECIDUOUS',
      grepl('olive|carob', crop, ignore.case = TRUE) ~ 'ORCHARD_CITRUS&SUBTROPICAL',
      grepl('grape', crop, ignore.case = TRUE) ~ 'VINEYARD',
      grepl('corn|sorghum|sudan', crop, ignore.case = TRUE) ~ 'FIELD_CORN',
      grepl('alfalfa', crop, ignore.case = TRUE) ~ 'PASTURE_ALFALFA',
      grepl('rice', crop, ignore.case = TRUE) ~ 'RICE',
      grepl('wheat', crop, ignore.case = TRUE) ~ 'GRAIN&HAY_WHEAT',
      grepl('hay other|oat|grain|triticale|silage',
            crop, ignore.case = TRUE) ~ 'GRAIN&HAY',
      grepl('pasture irrigated|ryegrass|seed grass', crop, ignore.case = TRUE) ~ 'PASTURE',
      grepl('pasture range|hay wild', crop, ignore.case = TRUE) ~ 'GRASSLAND',
      grepl('safflower|sunflower|beans dry|field crops',
            crop, ignore.case = TRUE) ~ 'FIELD',
      grepl('asparagus|beans|berries|carrot|cucumber|garlic|melon|onion|pepper',
            crop, ignore.case = TRUE) ~ 'ROW',
      grepl('potato|pumpkin|squash|tomato|vegetable|seed',
            crop, ignore.case = TRUE) ~ 'ROW',
      TRUE ~ 'UNKNOWN'),
    SUBCLASS = case_when(
      grepl('ALMOND', crop) ~ 'ALMONDS',
      crop == 'APPLES ALL' ~ 'APPLES',
      grepl('PEAR', crop) ~ 'PEARS',
      grepl('APRICOTS|NECTARINES|PEACHES|PLUMS|CHERRIES', crop) ~ 'STONE FRUIT',
      crop == 'WALNUTS ENGLISH' ~ 'WALNUTS',
      CODE_NAME == 'ORCHARD_DECIDUOUS' ~ 'MISCELLANEOUS DECIDUOUS',
      crop == 'OLIVES' ~ 'OLIVES',
      crop == 'CAROBS' ~ 'CAROBS',
      CODE_NAME == 'PASTURE_ALFALFA' ~ 'ALFALFA & ALFALFA MIXTURES',
      CODE_NAME == 'FIELD_CORN' ~ 'CORN, SORGHUM, OR SUDAN',
      CODE_NAME == 'GRAIN&HAY_WHEAT' ~ 'WHEAT',
      CODE_NAME == 'GRAIN&HAY' ~ 'MISCELLANEOUS GRAIN AND HAY',
      grepl('SUNFLOWER', crop) ~ 'SUNFLOWERS',
      grepl('BEANS DRY', crop) ~ 'BEANS (DRY)',
      crop == 'SAFFLOWER' ~ 'SAFFLOWER',
      CODE_NAME == 'FIELD' ~ 'MISCELLANEOUS FIELD',
      CODE_NAME == 'RICE' ~ 'RICE (ALL TYPES)',
      CODE_NAME %in% c('VINEYARD', 'GRASSLAND') ~ CODE_NAME,
      crop == 'PASTURE IRRIGATED' ~ 'MIXED PASTURE',
      crop %in% c('RYEGRASS PERENNIAL ALL', 'SEED GRASS UNSPECIFIED') ~ 'MISCELLANEOUS GRASSES',
      grepl('BLUEBERRIES', crop) ~ 'BUSH BERRIES',
      grepl('CARROTS', crop) ~ 'CARROTS',
      grepl('CUCUMBER|MELON|PUMPKIN|SQUASH', crop) ~
        'MELONS, SQUASH, AND CUCUMBERS (ALL TYPES)',
      grepl('GARLIC|ONION', crop) ~ 'ONIONS & GARLIC',
      grepl('PEPPERS', crop) ~ 'PEPPERS (CHILI, BELL, ETC)',
      grepl('POTATO', crop) ~ 'POTATO OR SWEET POTATO',
      grepl('STRAWBERRIES', crop) ~ 'STRAWBERRIES',
      crop == 'TOMATOES PROCESSING' ~ 'TOMATOES (PROCESSING)',
      grepl('ASPARAGUS|BEANS|TOMATO|VEGETABLE|SEED', crop) ~ 'MISCELLANEOUS TRUCK'
      ))


## estimate avg annual value per ha----------
# note that we can't get value per acre simply by dividing total value by total
# acreage because we're using total values for entire counties, only a portion
# of which are actually in the Delta proper; instead, follow the method used in
# the State of Delta Agriculture report, to apply the average yield per acre to
# Delta acres

cropdat_sum = cropdat_classify %>%
  # sum over all Delta counties in each year
  group_by(CODE_NAME, SUBCLASS, year) %>%
  summarize(across(c(harvest_ac, production, value), sum, na.rm = TRUE),
            .groups = 'drop')
write_csv(cropdat_sum, 'data_orig/cropdat_summary.csv')

## calculate average production value------
# $ per ha of each land cover class and subclass, averaged over all 5 years

## by subclass:
cropdat_subclass = cropdat_sum %>%
  # calculate (weighted) average production value per acre per subclass
  mutate(harvest_ha = harvest_ac / 2.47105,
         value_ha = value / harvest_ha) %>%
  group_by(CODE_NAME, SUBCLASS) %>%
  #average over all years
  summarize(SCORE = mean(value_ha, na.rm = TRUE),
            SCORE_MIN = min(value_ha, na.rm = TRUE),
            SCORE_MAX = max(value_ha, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(METRIC_CATEGORY = 'economy',
         METRIC_SUBTYPE = 'production value',
         METRIC = 'gross production value',
         UNIT = 'USD per ha per year (thousands)') %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, CODE_NAME, SUBCLASS,
         SCORE, SCORE_MIN, SCORE_MAX, UNIT)
write_csv(cropdat_subclass, 'data/crop_production_value_subclass.csv')

## by class:
cropdat_class = cropdat_sum %>%
  group_by(CODE_NAME, year) %>%
  summarize(across(c(harvest_ac, value), sum), .groups = 'drop') %>%
  # calculate (weighted) average production value per acre per class
  mutate(harvest_ha = harvest_ac / 2.47105,
         value_ha = value / harvest_ha) %>%
  #average over all years
  group_by(CODE_NAME) %>%
  summarize(SCORE = mean(value_ha),
            SCORE_MIN = min(value_ha),
            SCORE_MAX = max(value_ha),
            .groups = 'drop') %>%
  mutate(METRIC_CATEGORY = 'economy',
         METRIC_SUBTYPE = 'production value',
         METRIC = 'gross production value',
         UNIT = 'USD per ha per year (thousands)') %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, CODE_NAME, SCORE,
         SCORE_MIN, SCORE_MAX, UNIT)

# check for missing:
cropdat_class %>% select(METRIC, CODE_NAME, SCORE) %>%
  pivot_wider(names_from = 'METRIC', values_from = 'SCORE') %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = key %>% pull(CODE_NAME))) %>%
  complete(CODE_NAME) %>% print(n = 40)
# IDLE, URBAN, RIPARIAN, WETLAND_MANAGED, WETLAND_OTHER, WATER, WOODLAND&SCRUB,
# BARREN: assume zero crop production value

cropdat_class_fill = cropdat_class %>%
  bind_rows(
    expand_grid(
      tibble(CODE_NAME = c('IDLE', 'URBAN', 'RIPARIAN', 'WETLAND_MANAGED',
                           'WETLAND_OTHER', 'WOODLAND&SCRUB', 'BARREN')),
      cropdat_class %>% filter(CODE_NAME == 'GRASSLAND') %>%
        mutate(SCORE = 0, SCORE_MIN = NA, SCORE_MAX = NA) %>% select(-CODE_NAME)
    )
  )
write_csv(cropdat_class_fill, 'data/crop_production_value.csv')

# summary plots
cropdat_class_fill %>%
  mutate(CODE_NAME = factor(
    CODE_NAME,
    levels = cropdat_class_fill %>% arrange(SCORE) %>% pull(CODE_NAME))) %>%
  ggplot(aes(SCORE, CODE_NAME, xmin = SCORE_MIN, xmax = SCORE_MAX)) +
  geom_col() + geom_errorbar(width = 0)
# highest gross production value in ORCHARD_DECIDUOUS, ROW, VINEYARD
# ORCHARD_CITRUS&SUBTROPICAL

cropdat_subclass %>%
  mutate(SUBCLASS = factor(SUBCLASS,
                           levels = cropdat_subclass %>% arrange(SCORE) %>%
                             pull(SUBCLASS))) %>%
  # filter(CODE_NAME %in% c('ORCHARD_DECIDUOUS', 'VINEYARD', 'ROW')) %>%
  ggplot(aes(SCORE/1000, SUBCLASS, xmin = SCORE_MIN/1000, xmax = SCORE_MAX/1000)) +
  geom_col() + geom_errorbar(width = 0) +
  labs(x = 'value ($/ha, thousands)', y = NULL)
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
edat_classify = edat_subset %>%
  mutate(
    CODE_NAME = case_when(
      NAICS_code == 111140 ~ 'GRAIN&HAY_WHEAT', #wheat
      NAICS_code == 111150 ~ 'FIELD_CORN', #corn
      NAICS_code == 111160 ~ 'RICE', #rice
      NAICS_code == 111120 ~ 'FIELD', #oilseed (SUNFLOWER, SAFFLOWER OIL SEEDS)
      NAICS_code == 111940 ~ 'PASTURE', #hay farming (probably includes alfalfa?)
      NAICS_code %in% c(111191, 111199) ~ 'GRAIN&HAY', #oilseed and grain combo; all other grain farming
      NAICS_code %in% c(111331, 111335, 111336, 111339) ~ 'ORCHARD_DECIDUOUS', #apple, grape, tree nut, fruit/treenut combo
      NAICS_code == 111332 ~ 'VINEYARD',
      NAICS_code == 111219 ~ 'ROW', #other vegetable and melon
      NAICS_code == 111998 ~ 'OTHER' # all other misc
    ),
    SUBCLASS = case_when(
      NAICS_code == 111331 ~ 'APPLES',
      NAICS_code == 111335 ~ 'TREE NUTS',
      NAICS_code %in% c(111336, 111339) ~ 'MISCELLANEOUS DECIDUOUS',
      NAICS_code == 111120 ~ 'OILSEEDS',
      NAICS_code == 111150 ~ 'CORN, SORGHUM, OR SUDAN',
      NAICS_code == 111160 ~ 'RICE (ALL TYPES)',
      NAICS_code == 111140 ~ 'WHEAT',
      NAICS_code %in% c(111191, 111199) ~ 'MISCELLANEOUS GRAIN AND HAY',
      NAICS_code == 111940 ~ 'PASTURE (ALL TYPES)',
      NAICS_code == 111219 ~ 'ROW (ALL TYPES)',
      TRUE ~ CODE_NAME
    )) %>%
  select(CODE_NAME, SUBCLASS, industry, county, year, est:wages_total)

## estimate avg annual wages and jobs per ha----------
# as above, note that we can't get the number of jobs per ha simply by dividing
# total jobs by total area in the baseline map because we're using total values
# for entire counties, only a portion of which are actually in the Delta proper;
# instead, use harvested acres from crop production value data above

edat_sum = edat_classify %>%
  # sum over all Delta counties in each year
  group_by(CODE_NAME, SUBCLASS, year) %>%
  summarize(across(c(n_employees_monthly, wages_total), sum),
            .groups = 'drop') %>%
  # add acreage info from cropdat above
  left_join(cropdat_sum %>% select(CODE_NAME, SUBCLASS, year, harvest_ac) %>%
              mutate(
                SUBCLASS = case_when(
                  SUBCLASS %in% c('SAFFLOWER', 'SUNFLOWER') ~ 'OILSEEDS',
                  SUBCLASS %in% c('ALMONDS', 'WALNUTS') ~ 'TREE NUTS',
                  CODE_NAME == 'PASTURE' ~ 'PASTURE (ALL TYPES)',
                  CODE_NAME == 'ROW' ~ 'ROW (ALL TYPES)',
                  TRUE ~ SUBCLASS
                )) %>%
              group_by(CODE_NAME, SUBCLASS, year) %>%
              summarize(harvest_ac = sum(harvest_ac), .groups = 'drop'),
            by = c('CODE_NAME', 'SUBCLASS', 'year')) %>%
  mutate(harvest_ha = harvest_ac / 2.47105)
write_csv(edat_sum, 'data_orig/employment_summary.csv')

## calculate averages------
# employees per ha and wages per employee for each land cover class and
# subclass, averaged over all 5 years

## by subclass:
edat_subclass = edat_sum %>%
  # annual values
  mutate(JOBS = n_employees_monthly / harvest_ha * 1000, #employees per 1000 ha
         WAGES = wages_total/n_employees_monthly / 1000) %>%  # per person wages
  pivot_longer(JOBS:WAGES, names_to = 'METRIC') %>%
  filter(CODE_NAME != 'OTHER') %>%
  group_by(CODE_NAME, SUBCLASS, METRIC) %>%
  #average over all years
  summarize(SCORE = mean(value),
            SCORE_MIN = min(value),
            SCORE_MAX = max(value),
            .groups = 'drop') %>%
  mutate(METRIC_CATEGORY = 'economy',
         METRIC_SUBTYPE = 'livelihoods',
         UNIT = case_when(METRIC == 'JOBS' ~ 'number of employees per 1000ha',
                          METRIC == 'WAGES' ~ 'annual dollars per employee (thousands)')) %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, CODE_NAME, SUBCLASS, SCORE,
         SCORE_MIN, SCORE_MAX, UNIT) %>%
  arrange(METRIC)
write_csv(edat_subclass, 'data/livelihoods_subclass.csv')

## by class:
edat_class = edat_sum %>%
  group_by(CODE_NAME, harvest_ha, year) %>%
  summarize(across(c(n_employees_monthly, wages_total), sum), .groups = 'drop') %>%
  # annual values
  mutate(JOBS = n_employees_monthly / harvest_ha * 1000, #employees per 1000 ha
         WAGES = wages_total/n_employees_monthly / 1000) %>% #dollars per employee (thousands)
  pivot_longer(JOBS:WAGES, names_to = 'METRIC') %>%
  filter(CODE_NAME != 'OTHER') %>%
  group_by(CODE_NAME, METRIC) %>%
  #average over all years
  summarize(SCORE = mean(value),
            SCORE_MIN = min(value),
            SCORE_MAX = max(value),
            .groups = 'drop') %>%
  mutate(METRIC_CATEGORY = 'economy',
         METRIC_SUBTYPE = 'livelihoods',
         UNIT = case_when(METRIC == 'JOBS' ~ 'number of employees per 1000ha',
                          METRIC == 'WAGES' ~ 'annual dollars per employee (thousands)')) %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, CODE_NAME, SCORE,
         SCORE_MIN, SCORE_MAX, UNIT) %>%
  arrange(METRIC)

# check for missing:
edat_class %>% select(METRIC, CODE_NAME, SCORE) %>%
  pivot_wider(names_from = 'METRIC', values_from = 'SCORE') %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = key %>% pull(CODE_NAME))) %>%
  complete(CODE_NAME) %>% print(n = 40)
# ORCHARD_CITRUS&SUBTROPICAL, PASTURE_ALFALFA -- assume same as other orchard
# and general grain & hay?

# IDLE, GRASSLAND, URBAN, RIPARIAN, WETLAND_MANAGED, WETLAND_OTHER, WATER,
# WOODLAND&SCRUB, BARREN -- assume these provide zero ag jobs -- BUT note that
# grassland does have a crop production value

edat_class_fill = edat_class %>%
  bind_rows(
    edat_class %>% filter(CODE_NAME == 'ORCHARD_DECIDUOUS') %>%
      mutate(CODE_NAME = 'ORCHARD_CITRUS&SUBTROPICAL'),
    edat_class %>% filter(CODE_NAME == 'GRAIN&HAY') %>%
      mutate(CODE_NAME = 'PASTURE_ALFALFA'),
    expand_grid(
      tibble(CODE_NAME = c('IDLE', 'GRASSLAND', 'URBAN', 'RIPARIAN',
                           'WETLAND_MANAGED', 'WETLAND_OTHER', 'WOODLAND&SCRUB',
                           'BARREN')),
      edat_class %>% filter(CODE_NAME == 'VINEYARD') %>%
        mutate(SCORE = 0, SCORE_MIN = NA, SCORE_MAX = NA) %>% select(-CODE_NAME)
    )
  )
edat_class_fill %>% select(METRIC, CODE_NAME, SCORE) %>%
  pivot_wider(names_from = 'METRIC', values_from = 'SCORE') %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = key %>% pull(CODE_NAME))) %>%
  complete(CODE_NAME) %>% print(n = 40)


write_csv(edat_class_fill, 'data/livelihoods.csv')


# summary plots:
edat_class_fill %>%
  mutate(CODE_NAME = factor(
    CODE_NAME,
    levels = edat_class_fill %>% filter(METRIC == 'WAGES') %>%
      arrange(SCORE) %>% pull(CODE_NAME))) %>%
  ggplot(aes(SCORE, CODE_NAME, xmin = SCORE_MIN, xmax = SCORE_MAX)) +
  geom_col() + geom_errorbar(width = 0) + facet_wrap(~METRIC, scales = 'free')
# highest wages in RICE & lowest in VINEYARD
# highest number of jobs in ORCHARD_DECIDUOUS, lowest in WHEAT/GRAINS

edat_subclass %>%
  mutate(SUBCLASS = factor(
    SUBCLASS,
    levels = edat_subclass %>% filter(METRIC == 'WAGES') %>%
      arrange(SCORE) %>% pull(SUBCLASS))) %>%
  # filter(CODE_NAME %in% c('ORCHARD_DECIDUOUS', 'VINEYARD', 'ROW')) %>%
  ggplot(aes(SCORE/1000, SUBCLASS, xmin = SCORE_MIN/1000, xmax = SCORE_MAX/1000)) +
  geom_col() + geom_errorbar(width = 0) +
  labs(x = 'value ($/ha, thousands)', y = NULL) + facet_wrap(~METRIC, scales = 'free')
# not much subclass data available....
