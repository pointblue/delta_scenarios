# README---------
# Script to calculate the net change in spatial distribution predictions


# PACKAGES & FUNCTIONS
source('R/packages.R')
source('R/functions.R')

# reference data:
delta = rast('GIS/boundaries/delta.tif')
delta_shp = read_sf('GIS/boundaries/Legal_Delta_Boundary.shp') %>%
  st_transform(crs = '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')
palette = c("#2b83ba", "#80bfab", "#c7e8ad", "#ffffbf", "#fdc980", "#f07c4a",
            "#d7191c")
palette2 = c("darkblue", "white", "darkred")

rip_spp_key = tibble(
  species = c('NUWO', 'ATFL', 'BHGR', 'LAZB', 'COYE', 'YEWA', 'SPTO', 'SOSP',
              'YBCH'),
  plot_lab = LETTERS[1:9]) %>%
  mutate(species = factor(species,
                          levels = c('NUWO', 'ATFL', 'BHGR', 'LAZB', 'COYE',
                                     'YEWA', 'SPTO', 'SOSP', 'YBCH')),
         species_name = recode(species,
                       NUWO = "Nuttall's Woodpecker",
                       ATFL = 'Ash-throated Flycatcher',
                       BHGR = 'Black-headed Grosbeak',
                       LAZB = 'Lazuli Bunting',
                       COYE = 'Common Yellowthroat',
                       YEWA = 'Yellow Warbler',
                       SPTO = 'Spotted Towhee',
                       SOSP = 'Song Sparrow',
                       YBCH = 'Yellow-breasted Chat')) %>%
  unite('name', plot_lab, species_name, sep = '. ', remove = FALSE) %>%
  mutate(name = as.factor(name))

waterbird_spp_key = tibble(
  species = c('dblr', 'shore', 'cicon', 'crane', 'geese', 'divduck'),
  plot_lab = LETTERS[1:6]) %>%
  mutate(species = factor(species,
                          levels = c('dblr', 'shore', 'cicon', 'crane', 'geese', 'divduck')),
         species_name = recode(species,
                               dblr = 'Dabbling Ducks',
                               shore = 'Shorebirds',
                               cicon = 'Herons/Egrets',
                               crane = 'Cranes',
                               geese = 'Geese',
                               divduck = 'Diving Ducks')) %>%
  unite('name', plot_lab, species_name, sep = '. ', remove = FALSE) %>%
  mutate(name = as.factor(name))

# PREDICTIONS------

## RIPARIAN
riparian = list(
  'baseline' = list.files('GIS/prediction_rasters/riparian/baseline', '.tif$',
                          full.names = TRUE) %>% rast(),
  'scenario1' = list.files('GIS/prediction_rasters/riparian/scenario1', '.tif$',
                           full.names = TRUE) %>% rast(),
  'scenario2' = list.files('GIS/prediction_rasters/riparian/scenario2', '.tif$',
                           full.names = TRUE) %>% rast()
)
ripcombos = expand_grid(scenario = names(riparian),
                        species = names(riparian$baseline))

# WATERBIRDS FALL
fall = list(
  'baseline' = list.files('GIS/prediction_rasters/waterbirds_fall/baseline', '.tif$',
                          full.names = TRUE) %>% rast(),
  'scenario1' = list.files('GIS/prediction_rasters/waterbirds_fall/scenario1', '.tif$',
                           full.names = TRUE) %>% rast(),
  'scenario2' = list.files('GIS/prediction_rasters/waterbirds_fall/scenario2', '.tif$',
                           full.names = TRUE) %>% rast()
)
fallcombos = expand_grid(scenario = names(fall),
                         species = names(fall$baseline))

# WATERBIRDS WINTER
winter = list(
  'baseline' = list.files('GIS/prediction_rasters/waterbirds_win/baseline', '.tif$',
                          full.names = TRUE) %>% rast(),
  'scenario1' = list.files('GIS/prediction_rasters/waterbirds_win/scenario1', '.tif$',
                           full.names = TRUE) %>% rast(),
  'scenario2' = list.files('GIS/prediction_rasters/waterbirds_win/scenario2', '.tif$',
                           full.names = TRUE) %>% rast()
)
wintercombos = expand_grid(scenario = names(winter),
                           species = names(winter$baseline))

# CHANGE MAPS--------

## scenario 1------

### create change rasters-----

# RIPARIAN
changestack_riparian1 = purrr::pmap(
  ripcombos %>% filter(scenario == 'scenario1') %>% as.list(),
  ~calculate_change(type = 'species',
                    baseline = riparian$baseline[[.y]],
                    scenario = riparian[[.x]][[.y]],
                    return = 'raster')) %>%
  rast()
names(changestack_riparian1) = ripcombos %>%
  filter(scenario == 'scenario1') %>% pull(species)
writeRaster(changestack_riparian1,
            paste0('GIS/change_rasters/riparian/scenario1/',
                   names(changestack_riparian1), '.tif'))

# WATERBIRDS FALL
changestack_fall1 = purrr::pmap(
  fallcombos %>% filter(scenario == 'scenario1') %>% as.list(),
  ~calculate_change(type = 'species',
                    baseline = fall$baseline[[.y]],
                    scenario = fall[[.x]][[.y]],
                    return = 'raster')) %>%
  rast()
names(changestack_fall1) = fallcombos %>%
  filter(scenario == 'scenario1') %>% pull(species)
writeRaster(changestack_fall1,
            paste0('GIS/change_rasters/waterbirds_fall/scenario1/',
                   names(changestack_fall1), '.tif'))

# WATERBIRDS WINTER
changestack_winter1 = purrr::pmap(
  wintercombos %>% filter(scenario == 'scenario1') %>% as.list(),
  ~calculate_change(type = 'species',
                    baseline = winter$baseline[[.y]],
                    scenario = winter[[.x]][[.y]],
                    return = 'raster')) %>%
  rast()
names(changestack_winter1) = wintercombos %>%
  filter(scenario == 'scenario1') %>% pull(species)
writeRaster(changestack_winter1,
            paste0('GIS/change_rasters/waterbirds_win/scenario1/',
                   names(changestack_winter1), '.tif'))


### plot change rasters--------
# ggplot2-based maps (nicer, but slower)

# RIPARIAN
changestack_riparian_scenario1 = as.data.frame(changestack_riparian1, xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'species') %>%
  left_join(rip_spp_key, by = 'species')
ggsave(
  plot = map_predictions(
    changestack_riparian_scenario1,
    boundary_polygon = delta_shp,
    range = c(-1, 1),
    palette = rev(palette2),
    fill_lab = 'Change in\nprobability\nof presence'),
  filename = 'fig/changemap_scenario1_riparian.png',
  height = 8, width = 6.5)

# WATERBIRDS FALL
changestack_fall_scenario1 = as.data.frame(changestack_fall1, xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'species') %>%
  left_join(waterbird_spp_key, by = 'species')
ggsave(
  plot = map_predictions(
    changestack_fall_scenario1,
    boundary_polygon = delta_shp,
    range = c(-1, 1),
    palette = rev(palette2),
    fill_lab = 'Change in\nprobability\nof presence'),
  filename = 'fig/changemap_scenario1_waterbirds_fall.png',
  height = 5.33, width = 6.5)

# WATERBIRDS WINTER
changestack_winter_scenario1 = as.data.frame(changestack_winter1, xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'species') %>%
  left_join(waterbird_spp_key, by = 'species')
ggsave(
  plot = map_predictions(
    changestack_winter_scenario1,
    boundary_polygon = delta_shp,
    range = c(-1, 1),
    palette = rev(palette2),
    fill_lab = 'Change in\nprobability\nof presence'),
  filename = 'fig/changemap_scenario1_waterbirds_winter.png',
  height = 5.33, width = 6.5)

## scenario 2--------

### create change rasters-----

# RIPARIAN
changestack_riparian2 = purrr::pmap(
  ripcombos %>% filter(scenario == 'scenario2') %>% as.list(),
  ~calculate_change(type = 'species',
                    baseline = riparian$baseline[[.y]],
                    scenario = riparian[[.x]][[.y]],
                    return = 'raster')) %>%
  rast()
names(changestack_riparian2) = ripcombos %>%
  filter(scenario == 'scenario2') %>% pull(species)
writeRaster(changestack_riparian2,
            paste0('GIS/change_rasters/riparian/scenario2/',
                   names(changestack_riparian2), '.tif'))

# WATERBIRDS FALL
changestack_fall2 = purrr::pmap(
  fallcombos %>% filter(scenario == 'scenario2') %>% as.list(),
  ~calculate_change(type = 'species',
                    baseline = fall$baseline[[.y]],
                    scenario = fall[[.x]][[.y]],
                    return = 'raster')) %>%
  rast()
names(changestack_fall2) = fallcombos %>%
  filter(scenario == 'scenario2') %>% pull(species)
writeRaster(changestack_fall2,
            paste0('GIS/change_rasters/waterbirds_fall/scenario2/',
                   names(changestack_fall2), '.tif'))

# WATERBIRDS WINTER
changestack_winter2 = purrr::pmap(
  wintercombos %>% filter(scenario == 'scenario2') %>% as.list(),
  ~calculate_change(type = 'species',
                    baseline = winter$baseline[[.y]],
                    scenario = winter[[.x]][[.y]],
                    return = 'raster')) %>%
  rast()
names(changestack_winter2) = wintercombos %>%
  filter(scenario == 'scenario2') %>% pull(species)
writeRaster(changestack_winter2,
            paste0('GIS/change_rasters/waterbirds_win/scenario2/',
                   names(changestack_winter2), '.tif'))

### plot change rasters--------
# ggplot2-based maps (nicer, but slower)

# RIPARIAN
changestack_riparian_scenario2 = as.data.frame(changestack_riparian2, xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'species') %>%
  left_join(rip_spp_key, by = 'species')
ggsave(
  plot = map_predictions(
    changestack_riparian_scenario2,
    boundary_polygon = delta_shp,
    range = c(-1, 1),
    palette = rev(palette2),
    fill_lab = 'Change in\nprobability\nof presence'),
  filename = 'fig/changemap_scenario2_riparian.png',
  height = 8, width = 6.5)

# WATERBIRDS FALL
changestack_fall_scenario2 = as.data.frame(changestack_fall2, xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'species') %>%
  left_join(waterbird_spp_key, by = 'species')
ggsave(
  plot = map_predictions(
    changestack_fall_scenario2,
    boundary_polygon = delta_shp,
    range = c(-1, 1),
    palette = rev(palette2),
    fill_lab = 'Change in\nprobability\nof presence'),
  filename = 'fig/changemap_scenario2_waterbirds_fall.png',
  height = 5.33, width = 6.5)

# WATERBIRDS WINTER
changestack_winter_scenario2 = as.data.frame(changestack_winter2, xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'species') %>%
  left_join(waterbird_spp_key, by = 'species')
ggsave(
  plot = map_predictions(
    changestack_winter_scenario2,
    boundary_polygon = delta_shp,
    range = c(-1, 1),
    palette = rev(palette2),
    fill_lab = 'Change in\nprobability\nof presence'),
  filename = 'fig/changemap_scenario2_waterbirds_winter.png',
  height = 5.33, width = 6.5)


# NET CHANGE----------
# (consider threshold values? or specifying change in "high suitability" areas?)

## calculate net change------

change_riparian = purrr::pmap_dfr(
  ripcombos %>% filter(scenario != 'baseline') %>% as.list(),
  ~calculate_change(type = 'species',
                    baseline = riparian$baseline[[.y]],
                    scenario = riparian[[.x]][[.y]],
                    return = 'table'),
  .id = 'combo') %>%
  left_join(ripcombos %>% filter(scenario != 'baseline') %>%
              tibble::rownames_to_column('combo'), by = 'combo') %>%
  left_join(rip_spp_key, by = 'species') %>%
  mutate(change_area = net_change * .09/1000, #convert to thousands of ha
         change_pct = net_change / value_baseline,
         scenario_lab = recode(scenario,
                               scenario1 = 'Scenario 1.\nHabitat Restoration',
                               scenario2 = 'Scenario 2.\nPerennial Crop Expansion')) %>%
  arrange(scenario, species_name)
write_csv(change_riparian, 'output/change_riparian.csv')
# spec_sens: net_change is negative for most, but positive for SOSP, SPTO, YEWA
# kappa: negative for all but SPTO, YEWA (and smaller for YEWA) - loss of INTROSCRUB?
# --> losing GRASSPAS_2000: very low and very high GRASSPAS = higher YEWA probability

# kappa + cv-wide thresholds: decline for all but LAZB (0 change) AND SPTO (increase)
# straight sum of probabilities: decline for all but SPTO

change_waterbird_fall = purrr::pmap_dfr(
  fallcombos %>% filter(scenario != 'baseline') %>% as.list(),
  ~calculate_change(type = 'species',
                    baseline = fall$baseline[[.y]],
                    scenario = fall[[.x]][[.y]],
                    return = 'table'),
  .id = 'combo') %>%
  left_join(fallcombos %>% filter(scenario != 'baseline') %>%
              tibble::rownames_to_column('combo'), by = 'combo') %>%
  left_join(waterbird_spp_key, by = 'species') %>%
  mutate(change_area = net_change * .09/1000, #convert to thousands of ha
         change_pct = net_change / value_baseline,
         scenario_lab = recode(scenario,
                               scenario1 = 'Scenario 1.\nHabitat Restoration',
                               scenario2 = 'Scenario 2.\nPerennial Crop Expansion')) %>%
  arrange(scenario, species_name)
write_csv(change_waterbird_fall, 'output/change_waterbird_fall.csv')

change_waterbird_winter = purrr::pmap_dfr(
  wintercombos %>% filter(scenario != 'baseline') %>% as.list(),
  ~calculate_change(type = 'species',
                    baseline = winter$baseline[[.y]],
                    scenario = winter[[.x]][[.y]],
                    return = 'table'),
  .id = 'combo') %>%
  left_join(wintercombos %>% filter(scenario != 'baseline') %>%
              tibble::rownames_to_column('combo'), by = 'combo') %>%
  left_join(waterbird_spp_key, by = 'species') %>%
  mutate(change_area = net_change * .09/1000, #convert to thousands of ha
         change_pct = net_change / value_baseline,
         scenario_lab = recode(scenario,
                               scenario1 = 'Scenario 1.\nHabitat Restoration',
                               scenario2 = 'Scenario 2.\nPerennial Crop Expansion')) %>%
  arrange(scenario, species_name)
write_csv(change_waterbird_winter, 'output/change_waterbird_win.csv')

## plot net change------

net_change = bind_rows(
  change_riparian %>% mutate(group = 'Riparian Landbirds'),
  change_waterbird_fall %>% mutate(group = 'Waterbirds (fall)'),
  change_waterbird_winter %>% mutate(group = 'Waterbirds (winter)')
)
net_change$species_name = factor(net_change$species_name,
                                 levels = rev(levels(net_change$species_name)))

a = net_change %>% filter(scenario == 'scenario1') %>%
  select(group, scenario_lab, species = species_name, net_change = change_pct) %>%
  plot_change() +
  geom_vline(xintercept = 0, size = 0.2) +
  ggforce::facet_col(~group, scales = 'free_y', space = 'free') +
  labs(x = '% change', y = NULL, title = 'Scenario 1.\nHabitat Restoration') +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12),
        strip.text = element_text(size = 10.5, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none') +
  xlim(-0.25, 0.55)
b = net_change %>% filter(scenario == 'scenario2') %>%
  select(group, scenario_lab, species = species_name, net_change = change_pct) %>%
  plot_change() +
  geom_vline(xintercept = 0, size = 0.2) +
  ggforce::facet_col(~group, scales = 'free_y', space = 'free') +
  labs(x = '% change', y = NULL, title = 'Scenario 2.\nPerennial Crop Expansion') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12),
        strip.text = element_text(size = 10.5, color = 'white'),
        strip.background = element_blank(),
        legend.position = 'none') +
  xlim(-0.25, 0.55)
a+b
ggsave('fig/netchange_distributions.png', height = 6, width = 6)

change_riparian %>%
  mutate(species_name = factor(species_name,
                               levels = levels(change_riparian$species_name) %>%
                                 rev())) %>%
  select(scenario_lab, species = species_name, net_change = change_pct) %>%
  plot_change() + facet_wrap(~scenario_lab) +
  geom_vline(xintercept = 0, size = 0.2) +
  labs(x = bquote(' ' *Delta~ 'total area (ha, thousands)'), y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12),
        strip.text = element_text(size = 12, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none') +
  xlim(-0.25, 0.25)


# REGIONAL CHANGE---------

## RIPARIAN--------
change_riparian_county = purrr::pmap_dfr(
  ripcombos %>% filter(scenario != 'baseline') %>% as.list(),
  ~calculate_change(type = 'species',
                    baseline = riparian$baseline[[.x]],
                    scenario = riparian[[.y]][[.x]],
                    zones = county_raster,
                    return = 'table'),
  .id = 'rowname') %>%
  full_join(ripcombos %>% filter(scenario != 'baseline') %>%
              tibble::rownames_to_column(), by = 'rowname') %>%
  mutate(change_pct = net_change / value_baseline)
write_csv(change_riparian_county,
          'output/change_riparian_county.csv')

## WATERBIRDS FALL--------
change_waterbird_fall_county = purrr::pmap_dfr(
  combos %>% filter(scenario != 'baseline') %>% as.list(),
  ~calculate_change(type = 'species',
                    baseline = preds$baseline[[.x]],
                    scenario = preds[[.y]][[.x]],
                    zones = county_raster,
                    return = 'table'),
  .id = 'rowname') %>%
  full_join(combos %>% filter(scenario != 'baseline') %>%
              tibble::rownames_to_column(), by = 'rowname')
write_csv(change_waterbird_fall_county,
          'output/change_waterbird_fall_county.csv')

## WATERBIRDS WINTER--------

change_waterbird_win_county = purrr::pmap_dfr(
  combos %>% filter(scenario != 'baseline') %>% as.list(),
  ~calculate_change(type = 'species',
                    baseline = preds$baseline[[.x]],
                    scenario = preds[[.y]][[.x]],
                    zones = county_raster,
                    return = 'table'),
  .id = 'rowname') %>%
  full_join(combos %>% filter(scenario != 'baseline') %>%
              tibble::rownames_to_column(), by = 'rowname')
write_csv(change_waterbird_win_county,
          'output/change_waterbird_win_county.csv')


## baseline totals------
# plot baseline total suitable habitat overall and by county

change_riparian %>%
  filter(scenario == 'scenario1') %>%
  select(species, value_baseline, area_unit) %>%
  mutate(area_ha = value_baseline * .09) %>%
  ggplot(aes(area_ha, species)) + geom_col()

# baseline spatial variation across counties
change_riparian_county %>%
  filter(scenario == 'scenario1') %>%
  select(species, zone, value_baseline, area_unit) %>%
  left_join(freq(county_raster) %>% select(zone = label, count), by = 'zone') %>%
  mutate(area_baseline = value_baseline * .09,
         area_total = count * .09,
         suitable_density = area_baseline / area_total) %>%
  ggplot(aes(suitable_density, zone)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~species)
# --> more variation by species than by county

## format & plot--------




ggplot(change_riparian_county, aes(net_change, species, fill = zone)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~scenario)


# percent change by county
change_riparian_county %>%
  mutate(perc_change = net_change / value_baseline) %>%
  ggplot(aes(net_change, zone, fill = species)) +
  geom_col() +
  facet_wrap(~scenario)


# WATERBIRDS FALL---------

## net change-----

# calculate net change for each scenario, overall and by county


# test plots:
ggplot(change_waterbird_fall, aes(net_change, species)) +
  geom_col() +
  facet_wrap(~scenario)

ggplot(change_waterbird_fall_county, aes(net_change, species, fill = zone)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~scenario)

# baseline spatial variation across counties
change_waterbird_fall_county %>% filter(scenario == 'scenario1') %>%
  left_join(freq(county_raster) %>% select(zone = label, count), by = 'zone') %>%
  mutate(baseline_density = value_baseline / count) %>%
  ggplot(aes(baseline_density, zone)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~species)

# percent change by county
change_waterbird_fall_county %>%
  mutate(perc_change = net_change / value_baseline) %>%
  ggplot(aes(net_change, zone, fill = species)) +
  geom_col() +
  facet_wrap(~scenario)


# WATERBIRDS WINTER---------

## change maps--------






## net change--------
# calculate net change for each scenario, overall and by county


# test plots:
ggplot(change_waterbird_win, aes(net_change, species)) +
  geom_col() +
  facet_wrap(~scenario)

ggplot(change_waterbird_win_county, aes(net_change, species, fill = zone)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~scenario)

# baseline spatial variation across counties
change_waterbird_win_county %>% filter(scenario == 'scenario1') %>%
  left_join(freq(county_raster) %>% select(zone = label, count), by = 'zone') %>%
  mutate(baseline_density = value_baseline / count) %>%
  ggplot(aes(baseline_density, zone)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~species)

# percent change by county
change_waterbird_win_county %>%
  mutate(perc_change = net_change / value_baseline) %>%
  ggplot(aes(net_change, zone, fill = species)) +
  geom_col() +
  facet_wrap(~scenario)

# change rasters
changestack_waterbird_win = purrr::pmap(
  combos %>% filter(scenario != 'baseline') %>% as.list(),
  ~calculate_change(type = 'species',
                    baseline = preds$baseline[[.x]],
                    scenario = preds[[.y]][[.x]],
                    return = 'raster')) %>%
  rast()
names(changestack_waterbird_win) = combos %>%
  filter(scenario != 'baseline') %>%
  unite('name', species, scenario) %>% pull(name)
writeRaster(changestack_waterbird_win,
            paste0('GIS/change_rasters/waterbirds_win/',
                   names(changestack_waterbird_win), '.tif'))

plot(changestack_waterbird_win, range = c(-1, 1), col = rev(palette))

