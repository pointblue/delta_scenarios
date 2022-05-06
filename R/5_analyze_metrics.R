# ANALYZE NON-SPATIAL METRICS---------
# Script to calculate the net change in multiple metrics based solely on the net
# change in land cover classes/sub-classes, and not dependent on spatial
# location (i.e. no spatial distribution model involved)

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')

# REFERENCE DATA
delta = rast('GIS/boundaries/delta.tif')
county_raster = rast('GIS/landscape_rasters/counties.tif')

# scenario maps:
mapstack = c(rast('GIS/landscape_rasters/veg_baseline.tif'),
             rast('GIS/scenario_rasters/scenario1_restoration.tif'),
             rast('GIS/scenario_rasters/scenario2_perennialexpand.tif')
             # rast('GIS/scenario_rasters/scenario3_floodrisk.tif')
             )
key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')
levels(mapstack[[1]]) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
coltab(mapstack[[1]]) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
names(mapstack)[1] = 'baseline'


# LANDCOVER AREA---------

## totals--------
# within the Delta boundary only, for baseline map and each scenario
mapstack_mask = mapstack %>% mask(delta)

area = purrr::map_dfr(names(mapstack_mask) %>% setNames(names(mapstack_mask)),
                      ~calculate_area(mapstack_mask[[.x]], unit = 'ha'),
                      .id = 'scenario')
area %>% pivot_wider(names_from = scenario, values_from = area) %>%
  print(n = 28)
# Note: no generic "RIPARIAN" within Delta boundary

# add roll-up of riparian & wetland subtypes
area_sum = bind_rows(
  area,
  area %>% filter(grepl('RIPARIAN_|WETLAND_MANAGED', class)) %>%
    mutate(class = case_when(
      grepl('RIPARIAN_', class) ~ 'RIPARIAN',
      grepl('WETLAND_MANAGED', class) ~ 'WETLAND_MANAGED',
      TRUE ~ class)) %>%
    group_by(scenario, class, zone, area_unit) %>%
    summarize(area = sum(area), .groups = 'drop')
  )
area_sum %>% pivot_wider(names_from = scenario, values_from = area) %>%
  arrange(class) %>% print(n = 30)

# by county
area_county = purrr::map_dfr(
  names(mapstack_mask) %>% setNames(names(mapstack_mask)),
  ~calculate_area(mapstack_mask[[.x]], unit = 'ha', zones = county_raster),
  .id = 'scenario')

# add roll-up of riparian & wetland subtypes
area_sum_county = bind_rows(
  area_county,
  area_county %>% filter(grepl('RIPARIAN_|WETLAND_MANAGED', class)) %>%
    mutate(class = case_when(
      grepl('RIPARIAN_', class) ~ 'RIPARIAN',
      grepl('WETLAND_MANAGED', class) ~ 'WETLAND_MANAGED',
      TRUE ~ class)) %>%
    group_by(scenario, class, zone, area_unit) %>%
    summarize(area = sum(area), .groups = 'drop')
)

## map land cover change-------
delta_shp = read_sf('GIS/boundaries/Legal_Delta_Boundary.shp') %>%
  st_transform(crs = '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')
counties = read_sf('GIS/original_source_data/county_boundaries_simplified.shp')

scenario1_added = rast('GIS/scenario_inputs/restoration_added_ripdetail.tif') %>%
  mask(delta) %>%
  subst(c(70:79), 70)
levels(scenario1_added) <- key %>% select(CODE_BASELINE, CODE_NAME, LABEL) %>%
  mutate(LABEL = if_else(CODE_NAME == 'RIPARIAN', 'Riparian', LABEL)) %>%
  # mutate(LABEL = case_when(grepl('INTRO', CODE_NAME) ~ 'Introduced Scrub',
  #                          grepl('SCRUB_MIXED', CODE_NAME) ~ 'Mixed Shrub',
  #                          grepl('SCRUB_SALIX', CODE_NAME) ~ 'Willow Shrub',
  #                          grepl('FOREST_SALIX', CODE_NAME) ~ 'Willow Forest',
  #                          grepl('FOREST_MIXED', CODE_NAME) ~ 'Mixed Forest',
  #                          grepl('POFR', CODE_NAME) ~ 'Cottonwood',
  #                          grepl('QULO', CODE_NAME) ~ 'Valley Oak',
  #                          TRUE ~ LABEL)) %>%
  # mutate(LABEL = gsub('Managed Wetland - ', 'Managed - ', LABEL)) %>% drop_na() %>%
  drop_na() %>% select(CODE_BASELINE, LABEL) %>% as.data.frame()
coltab(scenario1_added) <- key %>% select(CODE_BASELINE, CODE_NAME, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
names(scenario_restoration) = 'scenario_restoration'

scenario2_added = rast('GIS/scenario_rasters/scenario2_perennialexpand.tif') %>%
  mask(rast('GIS/scenario_inputs/perex_added.tif')) %>%
  mask(delta)
levels(scenario2_added) <- key %>% select(CODE_BASELINE, CODE_NAME, LABEL) %>%
  drop_na() %>% select(CODE_BASELINE, LABEL) %>% as.data.frame()
coltab(scenario2_added) <- key %>% select(CODE_BASELINE, CODE_NAME, COLOR) %>%
  mutate(COLOR = case_when(CODE_NAME == 'ORCHARD_CITRUS&SUBTROPICAL' ~ '#f7941d',
                           CODE_NAME == 'ORCHARD_DECIDUOUS' ~ '#74B743',
                           TRUE ~ COLOR)) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)

plot(c(scenario1_added, scenario2_added))

addstack = c(scenario1_added, scenario2_added)
names(addstack) = c('Scenario 1. Habitat Restoration', 'Scenario 2. Perennial Crop Expansion')

plot(addstack, 1, legend = 'topright', axes = FALSE, mar = c(2, 2, 2, 2))
plot(vect(counties), add = TRUE, border = 'gray80')
plot(vect(delta_shp), add = TRUE)

plot(addstack, 2, legend = 'topright', axes = FALSE, mar = c(2, 2, 2, 2))
plot(vect(counties), add = TRUE, border = 'gray80')
plot(vect(delta_shp), add = TRUE)


# dat_added = as.data.frame(addstack, xy = TRUE) %>%
#   pivot_longer(!x:y, names_to = 'scenario') %>%
#   mutate(scenario = recode(scenario,
#                            Scenario.1 = 'Scenario 1. Habitat Restoration',
#                            Scenario.2 = 'Scenario 2. Perennial Crop Expansion'))
#
# colkey = key %>% select(CODE_NAME, LABEL, COLOR) %>%
#   mutate(LABEL = if_else(CODE_NAME == 'RIPARIAN', 'Riparian', LABEL))
#   # mutate(LABEL = case_when(grepl('INTRO', CODE_NAME) ~ 'Riparian - Introduced Scrub',
#   #                          grepl('SCRUB_MIXED', CODE_NAME) ~ 'Riparian - Mixed Shrub',
#   #                          grepl('SCRUB_SALIX', CODE_NAME) ~ 'Riparian - Willow Shrub',
#   #                          grepl('FOREST_SALIX', CODE_NAME) ~ 'Riparian - Willow Forest',
#   #                          grepl('FOREST_MIXED', CODE_NAME) ~ 'Riparian - Mixed Forest',
#   #                          grepl('POFR', CODE_NAME) ~ 'Cottonwood',
#   #                          grepl('QULO', CODE_NAME) ~ 'Valley Oak',
#   #                          TRUE ~ LABEL))
# collist = colkey %>% drop_na() %>% pull(COLOR) %>%
#   setNames(colkey %>% drop_na() %>% pull(LABEL))
#
# tiff(filename = 'fig/changemap_added_landcovers.tif',
#      width = 10000, height = 8000, pointsize = 100)
# par(mfrow=c(1,2))
# plot(addstack[[1]],
#      main = 'Scenario 1. Habitat Restoration', adj = 0,
#      legend = 'bottomright', axes = FALSE,
#      mar = c(0.1, 0.1, 2, 0))
# polys(vect(delta_shp), cex = 200)
#
# plot(addstack[[2]], main = 'Scenario 2. Perennial Crop Expansion',
#      legend = 'bottomright', axes = FALSE,
#      mar = c(0.1, 0, 2, 0.1))
# polys(vect(delta_shp), cex = 500)
# dev.off()
#
#
# ggplot(dat_added) + geom_tile(aes(x, y, fill = value)) +
#   scale_fill_manual(values = collist) +
#   facet_wrap(~scenario) +
#   geom_sf(data = delta_shp, fill = NA) +
#   labs(x = NULL, y = NULL, fill = "Land Cover Class") +
#   theme_minimal() +
#   theme(axis.text = element_blank(),
#         panel.grid = element_blank(),
#         strip.text = element_text(size = 8, hjust = 0),
#         legend.title = element_text(size = 8),
#         legend.text = element_text(size = 8),
#         legend.margin = margin(0, 0, 0, 5.5, unit = 'pt'),
#         plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
#         panel.spacing = unit(3, 'pt'))
# ggsave(filename = 'fig/changemap_added_landcovers.png',
#        height = 6.5, width = 9)



## net change-------

## SCENARIO 1: RESTORATION
change1 = calculate_change(baseline = mapstack_mask$baseline,
                           scenario = mapstack_mask$scenario_restoration,
                           type = 'landcover', units = 'ha', return = 'table') %>%
  left_join(key %>% select(class = CODE_NAME, LABEL), by = 'class')
write_csv(change1, 'output/change_scenario1_landcover.csv')

## regroup for plotting
change1_detail = bind_rows(
    # major classes
    change1 %>%
      mutate(LABEL = case_when(grepl('Orchard|Vineyard', LABEL) ~ 'Perennial Crops',
                               grepl('Riparian', LABEL) ~ 'Riparian',
                               grepl('Wetland', LABEL) ~ 'Wetlands',
                               grepl('Row|Field|Grain', LABEL) ~ 'Row & Field Crops',
                               grepl('Grassland|Pasture', LABEL) ~ 'Grassland & Pasture',
                               TRUE ~ LABEL)) %>%
      group_by(LABEL) %>%
      summarize(across(value_baseline:net_change, sum), .groups = 'drop') %>%
      arrange(net_change) %>%
      select(class = LABEL, net_change) %>%
      mutate(group = 'A'),
    # riparian details
    change1 %>% filter(grepl('RIPARIAN', class)) %>%
      mutate(LABEL = case_when(grepl('INTRO', class) ~ 'Introduced Scrub',
                               grepl('SCRUB_MIXED', class) ~ 'Mixed Shrub',
                               grepl('SCRUB_SALIX', class) ~ 'Willow Shrub',
                               grepl('FOREST_SALIX', class) ~ 'Willow Forest',
                               grepl('FOREST_MIXED', class) ~ 'Mixed Forest',
                               grepl('POFR', class) ~ 'Cottonwood',
                               grepl('QULO', class) ~ 'Valley Oak')) %>%
      arrange(net_change) %>%
      select(class = LABEL, net_change) %>%
      mutate(group = 'B'),
    # wetland details
    change1 %>% filter(grepl('WETLAND', class)) %>%
      mutate(LABEL = gsub('Managed Wetland - ', 'Managed - ', LABEL)) %>%
      arrange(net_change) %>%
      select(class = LABEL, net_change) %>%
      mutate(group = 'C')
    )
write_csv(change1_detail, 'output/change_scenario1_landcover_detail.csv')

plot_change(change1_detail, scale = 1000) +
  labs(x = bquote(' ' *Delta~ 'total area (ha, thousands)'), y = NULL) +
  ggforce::facet_col(~group, scales = 'free_y', space = 'free') +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14, hjust = 0),
        strip.background = element_blank()) +
  xlim(-3, 4)
ggsave('fig/change_scenario1_restoration_all.png', height = 8.5, width = 6)

## SCENARIO 2: PERENNIAL EXPANSION
change2 = calculate_change(baseline = mapstack_mask$baseline,
                           scenario = mapstack_mask$scenario_perennial_expansion,
                           type = 'landcover', units = 'ha', return = 'table') %>%
  left_join(key %>% select(class = CODE_NAME, LABEL), by = 'class')
write_csv(change2, 'output/change_scenario2_landcover.csv')

## simplified version for plotting
change2_detail = bind_rows(
  # major classes
  change2 %>%
    mutate(LABEL = case_when(grepl('Orchard|Vineyard', LABEL) ~ 'Perennial Crops',
                             grepl('Riparian', LABEL) ~ 'Riparian',
                             grepl('Wetland', LABEL) ~ 'Wetlands',
                             grepl('Row|Field|Grain', LABEL) ~ 'Row & Field Crops',
                             grepl('Grassland|Pasture', LABEL) ~ 'Grassland & Pasture',
                             TRUE ~ LABEL)) %>%
    group_by(LABEL) %>%
    summarize(net_change = sum(net_change), .groups = 'drop') %>%
    arrange(net_change) %>%
    select(class = LABEL, net_change) %>%
    mutate(group = 'A'),
  # riparian details
  change2 %>% filter(grepl('Orchard|Vineyard', LABEL)) %>%
    mutate(LABEL = gsub('Orchard - ', '', LABEL)) %>%
    arrange(net_change) %>%
    select(class = LABEL, net_change) %>%
    mutate(group = 'B')
)
write_csv(change2_detail, 'output/change_scenario2_landcover_detail.csv')

plot_change(change2_detail, scale = 1000) +
  labs(x = bquote(' ' *Delta~ 'total area (ha, thousands)'), y = NULL) +
  ggforce::facet_col(~group, scales = 'free_y', space = 'free') +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14, hjust = 0),
        strip.background = element_blank()) +
  xlim(-6, 20)
ggsave('fig/change_scenario2_perex_all.png', height = 6.5, width = 6)

## regional change-----------


## SCENARIO 1: RESTORATION
change1_county = calculate_change(baseline = mapstack_mask$baseline,
                                  scenario = mapstack_mask$scenario_restoration,
                                  zones = county_raster,
                                  type = 'landcover', units = 'ha', return = 'table') %>%
  left_join(key %>% select(class = CODE_NAME, LABEL), by = 'class')
write_csv(change1_county, 'output/change_scenario1_landcover_county.csv')

change1_county %>%
  mutate(LABEL = case_when(grepl('Orchard|Vineyard', LABEL) ~ 'Perennial Crops',
                           grepl('Riparian', LABEL) ~ 'Riparian',
                           grepl('Wetland', LABEL) ~ 'Wetlands',
                           grepl('Row|Field|Grain', LABEL) ~ 'Row & Field Crops',
                           grepl('Grassland|Pasture', LABEL) ~ 'Grassland & Pasture',
                           TRUE ~ LABEL)) %>%
  group_by(zone, LABEL) %>%
  summarize(across(value_baseline:net_change, sum), .groups = 'drop') %>%
  arrange(zone, net_change) %>%
  select(class = LABEL, county = zone, net_change) %>%
  mutate(class = factor(class, levels = change1_detail %>%
                          filter(group == 'A') %>% pull(class)),
         county = factor(county, levels = rev(
           c('San Joaquin', 'Yolo', 'Sacramento', 'Solano',
             'Contra Costa & Alameda')))) %>%
  ggplot(aes(net_change / 1000, class, fill = county)) +
  geom_col(position = position_dodge()) +
  scale_fill_viridis_d() +
  labs(x = bquote(' ' *Delta~ 'total area (ha, thousands)'), y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14, hjust = 0),
        strip.background = element_blank(),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.background = element_blank())
ggsave('fig/change_scenario1_restoration_county.png', height = 5, width = 6)

## SCENARIO 2: PERENNIAL EXPANSION
change2_county = calculate_change(baseline = mapstack_mask$baseline,
                                  scenario = mapstack_mask$scenario_perennial_expansion,
                                  zones = county_raster,
                                  type = 'landcover', units = 'ha', return = 'table') %>%
  left_join(key %>% select(class = CODE_NAME, LABEL), by = 'class')
write_csv(change2_county, 'output/change_scenario2_landcover_county.csv')

change2_county %>%
  mutate(LABEL = case_when(grepl('Orchard|Vineyard', LABEL) ~ 'Perennial Crops',
                           grepl('Riparian', LABEL) ~ 'Riparian',
                           grepl('Wetland', LABEL) ~ 'Wetlands',
                           grepl('Row|Field|Grain', LABEL) ~ 'Row & Field Crops',
                           grepl('Grassland|Pasture', LABEL) ~ 'Grassland & Pasture',
                           TRUE ~ LABEL)) %>%
  group_by(zone, LABEL) %>%
  summarize(across(value_baseline:net_change, sum), .groups = 'drop') %>%
  arrange(zone, net_change) %>%
  select(class = LABEL, county = zone, net_change) %>%
  mutate(class = factor(class, levels = change2_detail %>%
                          filter(group == 'A') %>% pull(class)),
         county = factor(county, levels = rev(
           c('San Joaquin', 'Yolo', 'Sacramento', 'Solano',
             'Contra Costa & Alameda')))) %>%
  ggplot(aes(net_change / 1000, class, fill = county)) +
  geom_col(position = position_dodge()) +
  scale_fill_viridis_d() +
  labs(x = bquote(' ' *Delta~ 'total area (ha, thousands)'), y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14, hjust = 0),
        strip.background = element_blank(),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.background = element_blank())
ggsave('fig/change_scenario2_restoration_county.png', height = 5, width = 6)

# METRICS----------
# apply metrics specific to each land cover, for baseline map and each scenario

metrics = bind_rows(
  read_csv('data/pesticide_exposure.csv', col_types = cols()) %>%
    filter(METRIC %in%
             c('aquatic contaminant', 'critical pesticides',
               'groundwater contaminant')) %>%
    mutate(METRIC_CATEGORY = 'water quality'),
  read_csv('data/crop_production_value.csv', col_types = cols()),
  read_csv('data/livelihoods.csv', col_types = cols()),
  read_csv('data/climate_change_sensitivity.csv', col_types = cols()),
  read_csv('data/avian_conservation_score.csv', col_types = cols())) %>%
  left_join(key %>% select(LABEL, CODE_NAME, CODE_BASELINE), by = 'CODE_NAME') %>%
  mutate(CODE_NAME = factor(CODE_NAME,
                            levels = key$CODE_NAME %>% na.omit()),
         LABEL = case_when(
           CODE_NAME == 'RIPARIAN' ~ 'Riparian',
           CODE_NAME == 'WETLAND_MANAGED' ~ 'Managed Wetland',
           CODE_NAME == 'GRAIN&HAY_WHEAT' ~ 'Wheat',
           LABEL == 'Grain & Hay - Alfalfa' ~ 'Alfalfa',
           TRUE ~ LABEL),
         METRIC = case_when(
           METRIC == 'JOBS' ~ 'agricultural jobs',
           METRIC == 'WAGES' ~ 'annual wages',
           METRIC == 'air pollution' ~ 'air pollutant',
           METRIC == 'Breeding_waterbirds' ~ 'Breeding Waterbirds (Other)',
           METRIC == 'Wintering_waterbirds' ~ 'Wintering Waterbirds (Other)',
           METRIC == 'aquatic contaminant' ~ 'Risk to Aquatic Organisms',
           TRUE ~ METRIC),
         METRIC = gsub('Oaksavannah', 'Oak Savannah', METRIC),
         # METRIC_CATEGORY = case_when(
         #   METRIC == 'aquatic contaminant' ~ 'healthy environment',
         #   TRUE ~ METRIC_CATEGORY),
         METRIC_CATEGORY = factor(METRIC_CATEGORY,
                                  levels = c('economy', 'water quality',
                                             'climate', 'biodiversity')),
         METRIC = gsub('_', ' ', METRIC),
         METRIC = capwords(METRIC)) %>%
  mutate_at(vars(METRIC_SUBTYPE:METRIC), factor) %>%
  arrange(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, CODE_NAME)

# add overall climate change score
metrics = metrics %>%
  bind_rows(metrics %>% filter(METRIC_CATEGORY == 'climate') %>%
              group_by(CODE_NAME, LABEL, METRIC_CATEGORY, METRIC_SUBTYPE, UNIT) %>%
              summarize(SCORE = mean(SCORE),
                        METRIC = 'Overall',
                        .groups = 'drop'))

metrics %>% select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC) %>% distinct() %>%
  drop_na() %>%
  print(n = 28)
# biodiversity: avian conservation (several spp/groups)
# climate: sensitivity (flood, drought, salinity, temp, overall)
# economy: livelihoods (jobs, wages), production value
# healthy environment: pesticide application rates (air, carcinogen, critical
#   pesticides, groundwater, organophosphorus, reproductive toxicity, total)

metric.order = metrics$METRIC %>% unique()
label.order = metrics %>%
  mutate(CODE_NAME = recode(CODE_NAME,
                            PASTURE_ALFALFA = 'GRAIN&HAY_ALFALFA')) %>%
  arrange(CODE_NAME) %>% pull(LABEL) %>% unique()
label.order = label.order[c(1:5, 12, 6:9, 11, 13, 10, 14:22)]
# check which land cover classes/subclasses accounted for in each metric
metrics %>% mutate(METRIC = factor(METRIC, levels = metric.order)) %>%
  select(CODE_NAME, METRIC) %>% distinct() %>% table()
# -->little to no change in some land covers for each scenario, so these may be
# ignored: URBAN, WETLAND_TIDAL, WATER, WOODLAND&SCRUB, BARREN
# --no general data for broad categories of: PERENNIAL_CROPS, ANNUAL_CROPS,
# GRASSLAND&PASTURE, WETLAND
# --no subclass data for: RIPARIAN (except climate change sensitivity) or
# WETLAND_MANAGED -- assume general stats apply to all subclasses

## comparison of metrics------

### economy---------
metrics %>% filter(METRIC_CATEGORY == 'economy') %>%
  mutate(LABEL = factor(LABEL, levels = rev(label.order)),
         SCORE = case_when(METRIC == 'Gross Production Value' ~ SCORE/1000, #from thousands to millions
                           METRIC == 'Agricultural Jobs' ~ SCORE,
                           TRUE ~ SCORE),
         METRIC = recode(METRIC,
                         `Gross Production Value` = 'Gross Crop\nProduction Value')) %>%
  select(LABEL, CODE_NAME, METRIC, SCORE) %>%
  ggplot(aes(SCORE, LABEL)) + geom_col(fill = pointblue.palette[4]) +
  # scale_fill_distiller(palette = 'Blues', direction = 1) +
  facet_wrap(~METRIC, ncol = 4, scales = 'free_x') +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')
ggsave('fig/metrics_economy.png', height = 4, width = 8)

### pesticides-------
metrics %>% filter(METRIC_CATEGORY == 'water quality') %>%
  # filter(METRIC != 'Reproductive Toxicity') %>%
  mutate(LABEL = factor(LABEL, levels = rev(label.order)),
         METRIC = case_when(
           METRIC %in% c('Critical Pesticides', 'Groundwater Contaminant') ~
             gsub(' ', '\n', METRIC),
           TRUE ~ 'Risk To Aquatic\nOrganisms')) %>%
  select(LABEL, CODE_NAME, METRIC, SCORE) %>%
  ggplot(aes(SCORE, LABEL)) + geom_col(fill = pointblue.palette[3]) +
  facet_wrap(~METRIC) +
  # scale_fill_gradient(low = palette[5], high = palette[7]) +
  labs(x = 'Pesticide application rate (lbs/ha/yr)', y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')
ggsave('fig/metrics_pesticides.png', height = 4, width = 8)

### climate-------
metrics %>% filter(METRIC_CATEGORY == 'climate') %>%
  # bind_rows(metrics %>% filter(METRIC_CATEGORY == 'climate') %>%
  #             group_by(CODE_NAME, LABEL, METRIC_CATEGORY, METRIC_SUBTYPE, UNIT) %>%
  #             summarize(SCORE = mean(SCORE),
  #                       METRIC = 'Overall',
  #                       .groups = 'drop')) %>%
  filter(!LABEL %in% c('Riparian Forest', 'Riparian Scrub', 'Wheat')) %>%
  mutate(LABEL = factor(LABEL, levels = rev(label.order)),
         METRIC = factor(METRIC, levels = c('Heat', 'Drought', 'Flood', 'Salinity', 'Overall'))) %>%
  select(LABEL, CODE_NAME, METRIC, SCORE) %>%
  ggplot(aes(SCORE, LABEL)) + geom_col(fill = 'seagreen') +
  facet_wrap(~METRIC, ncol = 5) +
  # scale_fill_viridis_c(option = 'D')
  # scale_fill_gradient(low = palette[7], high = palette[5]) +
  # scale_fill_distiller(palette = 'Greens', direction = 1) +
  labs(x = 'Climate change resilience score', y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')
ggsave('fig/metrics_climate.png', height = 4, width = 8)

### avian conservation score-------
acs = metrics %>% filter(METRIC_SUBTYPE == 'avian conservation contribution') %>%
  mutate(
    METRIC = recode(METRIC,
                    `Breeding Waterbirds (Other)` = 'Breeding Waterbirds\n(Other)',
                    `Wintering Waterbirds (Other)` = 'Wintering Waterbirds\n(Other)'),
    METRIC = factor(METRIC,
                    levels = c('Breeding Landbirds', 'Breeding Waterbirds', 'Wintering Waterbirds',
                               'Grassland Landbirds', 'Oak Savannah Landbirds', 'Riparian Landbirds',
                               'Breeding Waterfowl', 'Breeding Shorebirds', 'Breeding Waterbirds\n(Other)',
                               'Wintering Waterfowl', 'Wintering Shorebirds', 'Wintering Waterbirds\n(Other)'))
  )

# exclude overview scores:
acs %>%
  filter(!METRIC %in% c('Breeding Landbirds', 'Breeding Waterbirds', 'Wintering Waterbirds')) %>%
  filter(LABEL != 'Wheat') %>%
  mutate(LABEL = factor(LABEL, levels = rev(label.order))) %>%
  select(LABEL, CODE_NAME, METRIC, SCORE) %>%
  ggplot(aes(SCORE, LABEL)) + geom_col(fill = pointblue.palette[1]) +
  facet_wrap(~METRIC, ncol = 3) +
  # scale_fill_distiller(palette = 'Purples', direction = 1) +
  labs(x = 'Avian conservation score', y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 11, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')
ggsave('fig/metrics_acs.png', height = 9, width = 8)



## totals--------
# combine metrics with the areas of each land cover class in each scenario,
# eliminating land covers for which we have few data, and which don't change in
# these scenarios
scores = full_join(area_sum,
                   metrics %>% select(-SCORE_MIN, -SCORE_MAX, class = CODE_NAME),
                   by = 'class') %>%
  filter(!class %in% c('WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL',
                       'WETLAND_TIDAL', 'WATER')) %>%
  mutate(SCORE_TOTAL = area * SCORE) %>%
  filter(!(class == 'RIPARIAN' & METRIC_CATEGORY == 'climate')) #keep more specific subclass data instead

# check missing:
scores %>% filter(scenario == 'baseline') %>%
  filter(METRIC %in% c('Breeding Landbirds', 'Drought', 'Agricultural Jobs', 'Total Pesticides')) %>%
  select(class, METRIC, SCORE) %>%
  pivot_wider(names_from = METRIC, values_from = SCORE) %>% print(n = 26)

# summarize scores for each entire landscape:
# - generally the sum of the score for each ha
# - for wages, the average wage across all agricultural ha (with a wage value) times the number of jobs

scores_sum1 = scores %>%
  filter(METRIC != 'Annual Wages') %>%
  group_by(scenario, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, UNIT) %>%
  summarize(SCORE_TOTAL = sum(SCORE_TOTAL),
            .groups = 'drop') %>%
  filter(!METRIC %in% c('Breeding Landbirds',
                        'Breeding Waterbirds', 'Wintering Waterbirds'))

scores_sum2 = scores %>% filter(METRIC == 'Annual Wages' & SCORE > 0) %>%
  group_by(scenario, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, UNIT) %>%
  summarize(SCORE_TOTAL = sum(SCORE_TOTAL),
            area = sum(area),
            .groups = 'drop') %>%
  mutate(avg = SCORE_TOTAL / area) %>%
  left_join(scores_sum1 %>% filter(METRIC == 'Agricultural Jobs') %>%
              select(scenario, JOBS = SCORE_TOTAL), by = 'scenario') %>%
  mutate(SCORE_TOTAL = avg * JOBS) %>%
  select(-area, -JOBS, -avg)

scores_sum = bind_rows(scores_sum1, scores_sum2)

## net change------
change_scores = left_join(
  scores_sum %>% filter(scenario != 'baseline') %>%
    rename(SCORE_SCENARIO = SCORE_TOTAL),
  scores_sum %>% filter(scenario == 'baseline') %>%
    rename(SCORE_BASELINE = SCORE_TOTAL) %>% select(-scenario),
  by = c('METRIC_CATEGORY', 'METRIC_SUBTYPE', 'METRIC', 'UNIT')) %>%
  mutate(METRIC_CATEGORY = recode(METRIC_CATEGORY,
                                  'economy' = 'Agricultural Livelihoods',
                                  'water quality' = 'Water Quality',
                                  'climate' = 'Climate Change Resilience',
                                  'biodiversity' = 'Biodiversity Support')) %>%
  mutate(net_change = SCORE_SCENARIO - SCORE_BASELINE,
         SCORE_PCT = net_change/SCORE_BASELINE,
         SCORE_PCT = if_else(METRIC_CATEGORY == 'Water Quality', -1 * SCORE_PCT, SCORE_PCT),
         # net_change = case_when(METRIC == 'Gross Production Value' ~ net_change / 1000000, #from thousands to billions
         #                        METRIC == 'Annual Wages' ~ net_change / 1000000, # from thousands to millions
         #                        METRIC == 'Agricultural Jobs' ~ net_change / 10000, #hundreds
         #                        METRIC_CATEGORY == 'Water Quality' ~ -1 * net_change / 1000,
         #                        METRIC_CATEGORY == 'Biodiversity Support' ~ net_change / 1000,
         #                        METRIC_CATEGORY == 'Climate Change Resilience' ~ net_change / 1000,
         #                        TRUE ~ net_change),
         METRIC = factor(METRIC,
                         levels = rev(c('Agricultural Jobs', 'Annual Wages',
                                    'Gross Production Value',
                                    'Critical Pesticides',
                                    'Groundwater Contaminant',
                                    'Risk To Aquatic Organisms',
                                    'Heat', 'Drought', 'Flood', 'Salinity', 'Overall',
                                    'Grassland Landbirds', 'Oak Savannah Landbirds',
                                    'Riparian Landbirds', 'Breeding Waterfowl',
                                    'Breeding Shorebirds', 'Breeding Waterbirds (Other)',
                                    'Wintering Waterfowl', 'Wintering Shorebirds',
                                    'Wintering Waterbirds (Other)'))))

# change1_metrics = calculate_change(
#   baseline = mapstack_mask$baseline,
#   scenario = mapstack_mask$scenario_restoration,
#   type = 'metric',
#   metrics = metrics %>%
#     select(class = CODE_NAME, METRIC_CATEGORY, METRIC, value = SCORE),
#   return = 'table') %>%
#   group_by(METRIC_CATEGORY, METRIC) %>%
#   summarize(net_change = sum(net_change, na.rm = TRUE), .groups = 'drop')


# scenario1:
a = change_scores %>%
  # rename(class = METRIC) %>%
  filter(scenario == 'scenario_restoration') %>%
  mutate(bin = if_else(SCORE_PCT > 0, 'benefit', 'trade-off')) %>%
  ggplot(aes(SCORE_PCT, METRIC, fill = bin)) + geom_col() +
  geom_vline(xintercept = 0, size = 0.2) +
  # facet_wrap(~METRIC_CATEGORY, scales = 'free') +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  scale_fill_manual(values = c('blue', 'orange')) +
  # labs(x = bquote(' ' *Delta~ 'SCORE'), y = NULL) +
  labs(x = '% change', y = NULL, title = 'Scenario 1.\nHabitat Restoration') +
  xlim(-0.5, 0.5) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12),
        strip.text = element_text(size = 10.5, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')

# ggsave('fig/change_scenario1_restoration_all.png', height = 8.5, width = 6)

# scenario2
b = change_scores %>%
  # rename(class = METRIC) %>%
  filter(scenario == 'scenario_perennial_expansion') %>%
  mutate(bin = if_else(SCORE_PCT > 0, 'benefit', 'trade-off')) %>%
  ggplot(aes(SCORE_PCT, METRIC, fill = bin)) + geom_col() +
  geom_vline(xintercept = 0, size = 0.2) +
  # facet_wrap(~METRIC_CATEGORY, scales = 'free') +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  scale_fill_manual(values = c('blue', 'orange')) +
  # labs(x = bquote(' ' *Delta~ 'total area (ha, thousands)'), y = NULL) +
  labs(x = '% change', y = NULL, title = 'Scenario 2.\nPerennial Crop Expansion') +
  xlim(-0.5, 0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12),
        strip.text = element_text(size = 10.5, color = 'white'),
        strip.background = element_blank(),
        legend.position = 'none')
# library(patchwork)
a + b + plot_layout(widths = c(1, 1))
ggsave('fig/netchange_metrics.png', height = 6, width = 6)

change_scores %>%
  ggplot(aes(SCORE_PCT, METRIC, fill = landscape)) +
  geom_col(position = 'dodge') +
  geom_vline(xintercept = 0) +
  facet_wrap(~METRIC_CATEGORY, scales = 'free_y', nrow = 4)

change_scores %>%
  filter(METRIC_CATEGORY == 'biodiversity') %>%
  filter(grepl('habitat for', METRIC)) %>%
  rename(scenario = landscape) %>%
  mutate(scenario = recode(scenario,
                           scenario_perennial_expansion = 'A',
                           scenario_restoration = 'B'),
         METRIC = gsub('habitat for ', '', METRIC)) %>%
  ggplot(aes(METRIC, SCORE_CHANGE, fill = scenario)) +
  geom_col(position = 'dodge') +
  geom_hline(yintercept = 0) +
  ylab('net change from baseline') + xlab('taxonomic group') +
  theme_bw() +
  theme(legend.position = c(0.95, 0.95), legend.justification = c(1,1))


## regional change---------
scores_county = full_join(area_sum_county,
                          metrics %>% select(-SCORE_MIN, -SCORE_MAX, class = CODE_NAME),
                          by = 'class') %>%
  filter(!class %in% c('WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL',
                       'WETLAND_TIDAL', 'WATER')) %>%
  mutate(SCORE_TOTAL = area * SCORE) %>%
  filter(!(class == 'RIPARIAN' & METRIC_CATEGORY == 'climate')) #keep more specific subclass data instead

# check missing:
scores_county %>% filter(scenario == 'baseline' & zone == 'Yolo') %>%
  filter(METRIC %in% c('Breeding Landbirds', 'Drought', 'Agricultural Jobs', 'Total Pesticides')) %>%
  select(class, zone, METRIC, SCORE) %>%
  pivot_wider(names_from = METRIC, values_from = SCORE) %>% print(n = 30)

# summarize scores for each entire landscape:
# - generally the sum of the score for each ha
# - for wages, the average wage across all agricultural ha (with a wage value) times the number of jobs

scores_county_sum1 = scores_county %>%
  filter(METRIC != 'Annual Wages') %>%
  group_by(scenario, zone, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, UNIT) %>%
  summarize(SCORE_TOTAL = sum(SCORE_TOTAL),
            .groups = 'drop') %>%
  filter(!METRIC %in% c('Reproductive Toxicity', 'Breeding Landbirds',
                        'Breeding Waterbirds', 'Wintering Waterbirds'))

scores_county_sum2 = scores_county %>% filter(METRIC == 'Annual Wages' & SCORE > 0) %>%
  group_by(scenario, zone, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, UNIT) %>%
  summarize(SCORE_TOTAL = sum(SCORE_TOTAL),
            area = sum(area),
            .groups = 'drop') %>%
  mutate(avg = SCORE_TOTAL / area) %>%
  left_join(scores_county_sum1 %>% filter(METRIC == 'Agricultural Jobs') %>%
              select(scenario, zone, JOBS = SCORE_TOTAL),
            by = c('scenario', 'zone')) %>%
  mutate(SCORE_TOTAL = avg * JOBS) %>%
  select(-area, -JOBS, -avg)

scores_county_sum = bind_rows(scores_county_sum1, scores_county_sum2)

change_scores_county = left_join(
  scores_county_sum %>% filter(scenario != 'baseline') %>%
    rename(SCORE_SCENARIO = SCORE_TOTAL),
  scores_county_sum %>% filter(scenario == 'baseline') %>%
    rename(SCORE_BASELINE = SCORE_TOTAL) %>% select(-scenario),
  by = c('zone', 'METRIC_CATEGORY', 'METRIC_SUBTYPE', 'METRIC', 'UNIT')) %>%
  mutate(METRIC_CATEGORY = recode(METRIC_CATEGORY,
                                  'economy' = 'Economic Metrics',
                                  'healthy environment' = 'Pesticide Application Rates',
                                  'climate' = 'Climate Change Resilience',
                                  'biodiversity' = 'Avian Conservation Score')) %>%
  mutate(net_change = SCORE_SCENARIO - SCORE_BASELINE,
         SCORE_PCT = net_change/SCORE_BASELINE,
         net_change = case_when(METRIC == 'Gross Production Value' ~ net_change / 1000000, #from thousands to billions
                                METRIC == 'Annual Wages' ~ net_change / 1000000, # from thousands to millions
                                METRIC == 'Agricultural Jobs' ~ net_change / 10000, #hundreds
                                METRIC_SUBTYPE == 'pesticide application rate' ~ net_change / 1000,
                                METRIC_CATEGORY == 'Avian Conservation Score' ~ net_change / 1000,
                                METRIC_CATEGORY == 'Climate Change Resilience' ~ net_change / 1000,
                                TRUE ~ net_change),
         METRIC = factor(METRIC,
                         levels = rev(c('Agricultural Jobs', 'Annual Wages',
                                        'Gross Production Value',
                                        'Total Pesticides', 'Air Pollutant',
                                        'Carcinogen', 'Critical Pesticides',
                                        'Aquatic Contaminant',
                                        'Groundwater Contaminant',
                                        'Drought', 'Flood', 'Heat', 'Salinity',
                                        'Grassland Landbirds', 'Oak Savannah Landbirds',
                                        'Riparian Landbirds', 'Breeding Waterfowl',
                                        'Breeding Shorebirds', 'Breeding Waterbirds (Other)',
                                        'Wintering Waterfowl', 'Wintering Shorebirds',
                                        'Wintering Waterbirds (Other)'))),
         # reverse color coding for pesticides
         color = case_when(
           METRIC_CATEGORY == 'Pesticide Application Rates' & net_change < 0 ~ 'blue',
           METRIC_CATEGORY == 'Pesticide Application Rates' ~ 'orange',
           net_change < 0 ~ 'orange',
           TRUE ~ 'blue'))

# scenario1:
c = change_scores_county %>%
  # rename(class = METRIC) %>%
  filter(scenario == 'scenario_restoration') %>%
  ggplot(aes(SCORE_PCT, METRIC, fill = zone)) + geom_col(position = position_dodge()) +
  # facet_wrap(~METRIC_CATEGORY, scales = 'free') +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  # scale_fill_manual(values = c('blue' = 'blue', 'orange' = 'orange')) +
  # labs(x = bquote(' ' *Delta~ 'SCORE'), y = NULL) +
  labs(x = '% change', y = NULL, title = 'Scenario 1.\nHabitat Restoration') + xlim(-0.45, 0.45) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12),
        strip.text = element_text(size = 10.5, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')

# scenario2
d = change_scores_county %>%
  # rename(class = METRIC) %>%
  filter(scenario == 'scenario_perennial_expansion') %>%
  ggplot(aes(SCORE_PCT, METRIC, fill = zone)) + geom_col(position = position_dodge()) +
  # facet_wrap(~METRIC_CATEGORY, scales = 'free') +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  # scale_fill_manual(values = c('blue' = 'blue', 'orange' = 'orange')) +
  # labs(x = bquote(' ' *Delta~ 'total area (ha, thousands)'), y = NULL) +
  labs(x = '% change', y = NULL, title = 'Scenario 2.\nPerennial Crop Expansion') + xlim(-0.45, 0.45) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12),
        strip.text = element_text(size = 10.5, color = 'white'),
        strip.background = element_blank(),
        legend.position = 'none')
# library(patchwork)
c + d + plot_layout(widths = c(1, 1))

# APPLY TO METRICS---------

analyze_metrics = function(metrics_df, baseline, scenario = NULL) {
  if (!'METRIC' %in% names(metrics_df) | !"SCORE" %in% names(metrics_df)) {
    stop('metrics_df must contain at least the fields "METRIC" and "SCORE"')
  }
  if (class(baseline) == 'SpatRaster') {
    # calculate the area of each land cover classification

    if (length(levels(baseline)[[1]]) == 1) {
      # no levels/labels defined
      warning('No levels defined for baseline raster; using numeric values only.')
    }
    area_baseline = calculate_area(baseline, unit = 'ha')

  } else if ('data.frame' %in% class(baseline)) {
    # assume area already calculated
    if (!'area' %in% names(baseline) |
        (!'label' %in% names(baseline) & !'value' %in% names(baseline))) {
      # check that landcover1 contains the necessary fields
      stop('baseline must contain fields "area" and either "label" or "value", or provide a SpatRaster')
    } else {
      # proceed below
      area_baseline = baseline
    }
  } else {
    # not a spatraster or a tibble/dataframe
    stop('baseline must be a SpatRaster (from package terra), or a tibble or data.frame.')
  }

  metrics_base = full_join(area1, df) %>%  #join by value and/or label fields
    # calculate total score
    mutate(SCORE_BASELINE = SCORE * area) %>%
    select(-area, -SCORE) %>%
    group_by(across(c(-label, -value, -SCORE_BASELINE))) %>%
    summarize(SCORE_BASELINE = sum(SCORE_BASELINE, na.rm = TRUE),
              .groups = 'drop') %>%
    drop_na()

  if (!is.null(scenario)) {
    # calculate change as well as total metrics
    if (class(scenario) == 'SpatRaster') {
      if (length(levels(scenario)[[1]]) == 1) {
        # no levels/labels defined
        warning('No levels defined for scenario raster; using numeric values only.')
      }
      area_scenario = calculate_area(scenario)
    } else if ('data.frame' %in% class(scenario)) {
      # assume area already calculated
      if (!'area' %in% names(scenario) |
          (!'label' %in% names(scenario) & !'value' %in% names(scenario))) {
        # check that landcover2 contains the necessary fields
        stop('scenario must contain fields "area" and either "label" or "value", or provide a SpatRaster')
      } else {
        # proceed below
        area_scenario = scenario
      }
    } else {
      # not a spatraster or a tibble/dataframe
      stop('scenario must be a SpatRaster (from package terra), or a tibble or data.frame.')
    }

    metrics_scenario = full_join(area_scenario, df) %>%  #join by value and/or label fields
      # calculate total score
      mutate(SCORE_SCENARIO = SCORE * area) %>%
      select(-area, -SCORE) %>%
      group_by(across(c(-label, -value, -SCORE_SCENARIO))) %>%
      summarize(SCORE_SCENARIO = sum(SCORE_SCENARIO, na.rm = TRUE),
                .groups = 'drop') %>%
      drop_na()

    metrics_change = full_join(metrics_base, metrics_scenario) %>%
      mutate(SCORE_CHANGE = SCORE_SCENARIO - SCORE_BASELINE)

    return(metrics_change)
  } else {
    return(metrics_base)
  }

}


# TRY VISUALIZATION
change_score %>%
  # filter(METRIC_CATEGORY == 'biodiversity') %>%
  ggplot(aes(change_perc, METRIC, fill = change_perc)) +
  geom_col(color = 'black') + scale_fill_gradient2() +
  facet_wrap(~METRIC_CATEGORY + scenario, nrow = 4, scales = 'free_y') + xlim(-1, 1.25)
change_score %>%
  filter(METRIC_CATEGORY == 'economy') %>%
  ggplot(aes(change_perc, METRIC, fill = change_perc)) +
  geom_col(color = 'black') + scale_fill_gradient2() +
  facet_wrap(~scenario) + xlim(-1, 1.25)
change_score %>%
  filter(METRIC_CATEGORY == 'climate') %>%
  ggplot(aes(change_perc, METRIC, fill = change_perc)) +
  geom_col(color = 'black') + scale_fill_gradient2() +
  facet_wrap(~scenario)

# HIGH-LEVEL SUMMARY-------
change_summary = bind_rows(
  change_scores %>%
    filter(METRIC %in% c('gross production value',
                         'habitat for breeding waterbirds',
                         'habitat for grassland/oak savannah landbirds',
                         'climate change resilience', 'total pesticides',
                         'aquatic contaminant')),
  change_scores %>%
    filter(METRIC_CATEGORY == 'economy' & METRIC_SUBTYPE == 'livelihoods') %>%
    select(scenario:METRIC, base_score:scen_score) %>%
    pivot_longer(base_score:scen_score) %>%
    pivot_wider(names_from = METRIC, values_from = value) %>%
    mutate(value = JOBS * WAGES) %>%
    select(-JOBS, -WAGES) %>%
    pivot_wider() %>%
    mutate(METRIC = 'wages from agricultural jobs',
           change_score = scen_score - base_score,
           change_perc = change_score / base_score)
  ) %>%
  mutate(METRIC = recode(METRIC,
                         'total pesticides' = 'pesticide application rate',
                         'gross production value' = 'gross crop production value'))

# visualize
change_summary %>%
  mutate(METRIC_CATEGORY = toupper(METRIC_CATEGORY)) %>%
  unite(label, METRIC_CATEGORY, METRIC, sep = ': ') %>%
  ggplot(aes(change_perc, label, fill = change_perc)) +
  geom_col(color = 'black') + scale_fill_gradient2() +
  facet_wrap(~scenario, nrow = 4) +
  labs(x = '% change from baseline')

# --> need to figure out how to show/label "disservices" (i.e. pesticide
# application rate, aquatic contaminants) so that a decrease is obviously a good
# thing

