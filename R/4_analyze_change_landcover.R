# ANALYZE LAND COVER CHANGE---------
# Calculate and map the net change in the area of each land cover class,
# within the Delta boundary, for the baseline and each scenario

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')

# REFERENCE DATA

# key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')
key = readxl::read_excel('GIS/VEG_key.xlsx')
delta = rast('GIS/boundaries/delta.tif')
county_raster = rast('GIS/landscape_rasters/boundaries/counties.tif')

# CALCULATE TOTALS--------

# scenario maps:
mapstack = c(rast('GIS/landscape_rasters/veg_baseline.tif'),
             rast('GIS/scenario_rasters/scenario1_restoration.tif'),
             rast('GIS/scenario_rasters/scenario2_perennialexpand.tif'))
mapstack_mask = mapstack %>% mask(delta)

# calculate total area, within the Delta boundary only
area_delta = mapstack_mask %>% as.list() %>%
  purrr::set_names(names(mapstack_mask)) %>%
  purrr::map_dfr(calculate_area, unit = 'ha', .id = 'scenario') %>%
  select(scenario, CODE_NAME = class, area, area_unit)

area_delta %>%
  pivot_wider(names_from = scenario, values_from = area) %>%
  print(n = Inf)

# add roll-up of riparian & managed wetland subtypes
area_sum = bind_rows(
  area_delta,
  area_delta %>% filter(grepl('RIPARIAN_|WETLAND_MANAGED', CODE_NAME)) %>%
    mutate(CODE_NAME = case_when(
      grepl('RIPARIAN_', CODE_NAME) ~ 'RIPARIAN',
      grepl('WETLAND_MANAGED', CODE_NAME) ~ 'WETLAND_MANAGED',
      TRUE ~ CODE_NAME)) %>%
    group_by(scenario, CODE_NAME, area_unit) %>%
    summarize(area = sum(area), .groups = 'drop')
  )
area_sum %>% pivot_wider(names_from = scenario, values_from = area) %>%
  arrange(CODE_NAME) %>% print(n = Inf)
write_csv(area_sum, 'output/land_cover_totals.csv')

## by county--------
# repeat, but total by county within the Delta instead

area_county = mapstack_mask %>% as.list() %>%
  purrr::set_names(names(mapstack_mask)) %>%
  purrr::map_dfr(calculate_area, unit = 'ha', zones = county_raster,
                 .id = 'scenario') %>%
  select(scenario, county = zone, CODE_NAME = class, area, area_unit)
area_county %>%
  pivot_wider(names_from = scenario:county, values_from = area) %>%
  print(n = Inf)

# add roll-up of riparian & wetland subtypes
area_sum_county = bind_rows(
  area_county,
  area_county %>% filter(grepl('RIPARIAN_|WETLAND_MANAGED', CODE_NAME)) %>%
    mutate(class = case_when(
      grepl('RIPARIAN_', CODE_NAME) ~ 'RIPARIAN',
      grepl('WETLAND_MANAGED', CODE_NAME) ~ 'WETLAND_MANAGED',
      TRUE ~ CODE_NAME)) %>%
    group_by(scenario, county, CODE_NAME, area_unit) %>%
    summarize(area = sum(area), .groups = 'drop')
)
write_csv(area_sum_county, 'output/land_cover_totals_county.csv')


# NET CHANGE-------

change = bind_rows(
  calculate_change(baseline = mapstack_mask$baseline,
                   scenario = mapstack_mask$scenario1_restoration,
                   type = 'landcover', units = 'ha', return = 'table') %>%
    mutate(scenario = 'scenario1_restoration'),
  calculate_change(baseline = mapstack_mask$baseline,
                   scenario = mapstack_mask$scenario2_perennialexpand,
                   type = 'landcover', units = 'ha', return = 'table') %>%
    mutate(scenario = 'scenario2_perennialexpand')) %>%
  left_join(key %>% select(class = CODE_NAME, LABEL), by = 'class')

# add roll-up of riparian & managed wetland subtypes
change_sum = bind_rows(
  change,
  change %>% filter(grepl('RIPARIAN_|WETLAND_MANAGED', class)) %>%
    mutate(
      class = case_when(
        grepl('RIPARIAN_', class) ~ 'RIPARIAN',
        grepl('WETLAND_MANAGED', class) ~ 'WETLAND_MANAGED',
        TRUE ~ class),
      LABEL = case_when(
        class == 'RIPARIAN' ~ 'Riparian',
        class == 'WETLAND_MANAGED' ~ 'Managed Wetland',
        TRUE ~ LABEL)) %>%
    group_by(scenario, class, LABEL, area_unit) %>%
    summarize(across(c(value_baseline, value_scenario, net_change), sum),
              .groups = 'drop')
)

write_csv(change_sum, 'output/land_cover_change.csv')

## by county---------
change_county = bind_rows(
  calculate_change(baseline = mapstack_mask$baseline,
                   scenario = mapstack_mask$scenario1_restoration,
                   zones = county_raster,
                   type = 'landcover', units = 'ha', return = 'table') %>%
    mutate(scenario = 'scenario1_restoration'),
  calculate_change(baseline = mapstack_mask$baseline,
                   scenario = mapstack_mask$scenario2_perennialexpand,
                   zones = county_raster,
                   type = 'landcover', units = 'ha', return = 'table') %>%
    mutate(scenario = 'scenario2_perennialexpand')) %>%
  left_join(key %>% select(class = CODE_NAME, LABEL), by = 'class')

# add roll-up of riparian & managed wetland subtypes
change_sum_county = bind_rows(
  change_county,
  change_county %>% filter(grepl('RIPARIAN_|WETLAND_MANAGED', class)) %>%
    mutate(
      class = case_when(
        grepl('RIPARIAN_', class) ~ 'RIPARIAN',
        grepl('WETLAND_MANAGED', class) ~ 'WETLAND_MANAGED',
        TRUE ~ class),
      LABEL = case_when(
        class == 'RIPARIAN' ~ 'Riparian',
        class == 'WETLAND_MANAGED' ~ 'Managed Wetland',
        TRUE ~ LABEL)) %>%
    group_by(scenario, class, LABEL, zone, area_unit) %>%
    summarize(across(c(value_baseline, value_scenario, net_change), sum),
              .groups = 'drop')
)

write_csv(change_sum_county, 'output/land_cover_change_county.csv')
