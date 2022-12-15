# README---------
# - compile land cover totals for the baseline and each scenario
# - calculate the net impact of each scenario on the coverage of each land cover
#     class
# - plot the land cover change for each major class and select subclasses
# - plot the map of each scenario

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')
library(patchwork)
library(showtext)
font_add_google('Source Sans Pro', 'sourcesans')

# REFERENCE DATA
key = readxl::read_excel('GIS/VEG_key.xlsx')

# LAND COVER CHANGE----------
# total area of each land cover class/subclass in each landscape:
landcover = DeltaMultipleBenefits::sum_landcover(
  pathin = 'GIS/scenario_rasters',
  maskpath = 'GIS/boundaries/delta.tif',
  pixel_area = 0.09,
  rollup = TRUE) %>%
  # add LABEL fields
  left_join(key %>% select(CODE_NAME, LABEL), by = 'CODE_NAME') %>%
  select(scenario, CODE_NAME, LABEL, area) %>%
  arrange(scenario, CODE_NAME)
write_csv(landcover, 'output/landcover_totals.csv')

# net change
# have to align field names to match fields expected in sum_change()
landcover_change = landcover %>% filter(!grepl('win', scenario)) %>%
  rename(SCORE_TOTAL = area) %>%
  mutate(SCORE_TOTAL_SE = 0) %>%
  DeltaMultipleBenefits::sum_change(scoredat = .) %>%
  select(-ends_with('SE', ignore.case = TRUE))
write_csv(landcover_change, 'output/netchange_landcover.csv')

# PLOT CHANGE-------
landcover_change = read_csv('output/netchange_landcover.csv')

change_summary = bind_rows(
  # major landcover classes:
  landcover_change %>%
    # remove riparian and wetland subclasses
    filter(!grepl('RIPARIAN_|WETLAND_MANAGED_|WATER', CODE_NAME)) %>%
    # lump other classes
    mutate(LABEL = case_when(grepl('ORCHARD_|VINEYARD', CODE_NAME) ~ 'Perennial Crops',
                             grepl('GRAIN|HAY', CODE_NAME) ~ 'Grain & Hay',
                             grepl('FIELD_CORN', CODE_NAME) ~ 'Corn', #specify so it doesn't combine with field & row
                             grepl('FIELD|ROW', CODE_NAME) ~ 'Field & Row Crops',
                             grepl('ALFALFA', CODE_NAME) ~ 'Alfalfa',
                             grepl('GRASS|PASTURE', CODE_NAME) ~ 'Grassland & Pasture',
                             TRUE ~ LABEL),
           LABEL = factor(LABEL,
                          levels = c('Perennial Crops', 'Grain & Hay',
                                     'Field & Row Crops', 'Corn', 'Rice',
                                     'Idle', 'Grassland & Pasture', 'Alfalfa',
                                     'Urban', 'Riparian', 'Managed Wetland',
                                     'Tidal Wetland', 'Other Wetland',
                                     'Woodland', 'Scrub', 'Barren') %>%
                            rev())) %>%
    group_by(scenario, LABEL) %>%
    summarize(net_change = sum(net_change),
              change_pct = net_change/sum(BASELINE) * 100,
              .groups = 'drop') %>%
    # filter(!LABEL %in% c('Urban', 'Barren', 'Water', 'Woodland', 'Scrub')) %>%
    mutate(bin = if_else(net_change > 0, 'increase', 'decrease'),
           group = 'Major classes') %>%
    arrange(scenario, LABEL),
  # wetland and riparian subclass detail
  landcover_change %>% filter(grepl('RIPARIAN_|WETLAND_MANAGED_|ORCHARD|VINEYARD', CODE_NAME)) %>%
    filter(CODE_NAME != 'WETLAND_MANAGED') %>%
    mutate(LABEL = factor(LABEL,
                          levels = c('Orchard - Deciduous Fruits & Nuts',
                                     'Orchard - Citrus & Subtropical',
                                     'Vineyard',
                                     'Riparian - Cottonwood',
                                     'Riparian - Valley Oak',
                                     'Riparian - Willow',
                                     'Riparian - Willow Shrub',
                                     'Riparian - Mixed Forest',
                                     'Riparian - Mixed Shrub',
                                     'Riparian - Introduced Scrub',
                                     'Managed Wetland - Perennial',
                                     'Managed Wetland - Seasonal',
                                     'Tidal Wetland',
                                     'Other Wetland') %>% rev()),
           bin = if_else(net_change > 0, 'increase', 'decrease'),
           group = 'Subclasses') %>%
    arrange(scenario, LABEL)
) %>%
  mutate(scenario = recode(scenario,
                           'scenario1_restoration' = 'Scenario 1:\nHabitat restoration',
                           'scenario2_perennialexpand' = 'Scenario 2:\nPerennial crop\nexpansion',
                           'scenario3_combo' = 'Scenario 3:\nCombination'))

## for manuscript---------
part1 = change_summary %>% filter(group == 'Major classes') %>%
  ggplot(aes(net_change/1000, LABEL)) +
  facet_wrap(~scenario, ncol = 3) +
  geom_col(fill = 'gray60') +
  geom_text(data = change_summary %>% filter(group == 'Major classes' & bin == 'increase'),
            aes(x = net_change/1000 + 2,
                label = paste0(round(change_pct, digits = 0), '%')), size = 2) +
  geom_text(data = change_summary %>% filter(group == 'Major classes' & bin == 'decrease'),
            aes(x = net_change/1000 - 2,
                label = paste0(round(change_pct, digits = 0), '%')), size = 2) +
  labs(y = NULL,
       x = NULL,
       title = 'A'
       ) +
  xlim(-7, 20) +
  geom_vline(xintercept = 0, size = 0.2) +
  theme_bw() +
  theme(
    strip.placement = 'outside',
        # strip.text = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 9, vjust = 0),
        strip.background = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 10),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = 'sourcesans', size = 8, hjust = 1),
        axis.title = element_text(family = 'sourcesans', size = 8),
        legend.position = 'none')

part2 = change_summary %>% filter(group != 'Major classes') %>%
  ggplot(aes(net_change/1000, LABEL)) +
  facet_wrap(~scenario, ncol = 3) +
  geom_col(fill = 'gray60') +
  geom_text(data = change_summary %>% filter(group != 'Major classes' & bin == 'increase'),
            aes(x = net_change/1000 + 2,
                label = paste0(round(change_pct, digits = 0), '%')), size = 2) +
  geom_text(data = change_summary %>% filter(group != 'Major classes' & bin == 'decrease'),
            aes(x = net_change/1000 - 2,
                label = paste0(round(change_pct, digits = 0), '%')), size = 2) +
  labs(x = expression(paste(Delta, ' total area (ha, thousands)')),
       y = NULL,
       title = 'B'
       ) +
  xlim(-7, 20) +
  geom_vline(xintercept = 0, size = 0.2) +
  theme_bw() +
  theme(
    strip.placement = 'outside',
        strip.text = element_blank(),
        # strip.text = element_text(family = 'sourcesans', size = 10, vjust = 1),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(family = 'sourcesans', size = 8),
        axis.text.y = element_text(hjust = 1),
        axis.title = element_text(family = 'sourcesans', size = 8),
        plot.title = element_text(family = 'sourcesans', size = 10),
        legend.position = 'none')

showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
part1/part2 + plot_layout(heights = c(0.55, 0.45))
  # plot_annotation(tag_levels = 'A') &
  # theme(plot.tag = element_text(family = 'sourcesans', size = 10))
ggsave('fig/netchange_land_cover.jpg',
       height = 6, width = 6.5, units = 'in', dpi = 300)
showtext_auto(FALSE)


## for presentation-------
showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
change_summary %>% filter(group == 'Major classes') %>%
  ggplot(aes(net_change/1000, LABEL)) + facet_wrap(~scenario) +
  geom_col(aes(fill = bin)) +
  geom_text(data = change_summary %>% filter(group == 'Major classes' & bin == 'increase'),
            aes(x = net_change/1000 + 1.5,
                label = paste0(round(change_pct, digits = 0), '%')), size = 3.5) +
  geom_text(data = change_summary %>% filter(group == 'Major classes' & bin == 'decrease'),
            aes(x = net_change/1000 - 1.5,
                label = paste0(round(change_pct, digits = 0), '%')), size = 3.5) +
  scale_fill_manual(values = c(pointblue.palette[c(3, 1)])) +
  labs(x = expression(paste(Delta, ' total area (ha, thousands)')),
       y = NULL) +
  geom_vline(xintercept = 0, size = 0.2) +
  xlim(-7, 20) +
  theme_bw() +
  theme(strip.text = element_text(family = 'sourcesans', size = 18, hjust = 0, face = 'bold'),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(family = 'sourcesans', size = 16),
        axis.text.y = element_text(hjust = 1),
        axis.title = element_text(family = 'sourcesans', size = 18),
        legend.position = 'none')
ggsave('fig/netchange_land_cover_presentation.jpg',
       height = 5, width = 10, units = 'in', dpi = 300)
showtext_auto(FALSE)

# SCENARIO MAP--------
# plot (simplified) baseline and the pixels that change in each scenario

# REFERENCE DATA
delta = rast('GIS/boundaries/delta.tif')
# county_raster = rast('GIS/landscape_rasters/boundaries/counties.tif')

delta = rast('GIS/boundaries/delta.tif')
delta_shp = read_sf('GIS/boundaries/Legal_Delta_Boundary.shp') %>%
  st_transform(st_crs('EPSG:32610'))

# original landscape maps:
fl = list.files('GIS/scenario_rasters', '.tif$', full.names = TRUE)
scenarios = terra::rast(fl[!grepl('win', fl)])

key = readxl::read_excel('GIS/VEG_key.xlsx')

label.order = key %>% select(CODE_NAME, LABEL) %>% distinct() %>%
  filter(!CODE_NAME %in% c(
    'PERENNIAL_CROPS', 'ANNUAL_CROPS', 'GRAIN&HAY', 'FIELD',
    'GRASSLAND&PASTURE', 'WOODLAND&SCRUB', 'WATER', 'WETLAND',
    'WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL')) %>%
  filter(!grepl('RIPARIAN_', CODE_NAME) & !is.na(CODE_NAME)) %>%
  pull(LABEL) %>% unique()
label.order = label.order[c(1:9, 11:13, 10, 14:22)]
# simplify baseline to major land cover classes for manuscript
baseline_simple = classify(scenarios$baseline,
                           rcl = matrix(
                             c(10.5, 19.5, 10, #Perennial crops
                               21.5, 24.5, 21, #Grain & hay
                               24.5, 25.5, 25, #Field & row
                               26.5, 28.5, 25, #Field & row
                               50.5, 51.5, 50, #Grassland & pasture
                               52.5, 57.5, 50, #Grassland & pasture
                               70.5, 79.5, 70, #Riparian
                               80.5, 89.5, 80), #Wetlands
                             byrow = TRUE, ncol = 3)
)
levels(baseline_simple) <- key %>% select(CODE_BASELINE, LABEL) %>%
  filter(CODE_BASELINE %in% c(10, 21, 25, 26, 30, 40, 50, 52, 60, 70, 80, 90,
                              110, 120, 130)) %>%
  mutate(LABEL = if_else(LABEL == 'Field Crops', 'Field & Row Crops', LABEL)) %>%
  as.data.frame()
coltab(baseline_simple) <- key %>% select(CODE_BASELINE, COLOR) %>% drop_na() %>%
  filter(CODE_BASELINE %in% c(10, 21, 25, 26, 30, 40, 50, 52, 60, 70, 80, 90:130)) %>%
  mutate(COLOR = case_when(CODE_BASELINE == 10 ~ '#aa66cd',
                           CODE_BASELINE == 21 ~ '#ffebaf',
                           CODE_BASELINE == 25 ~ '#ffa77f',
                           CODE_BASELINE == 26 ~ '#ffff00',
                           CODE_BASELINE == 30 ~ '#e600a9',
                           CODE_BASELINE == 40 ~ '#e1e1e1',
                           CODE_BASELINE == 50 ~ '#e9ffbe',
                           CODE_BASELINE == 52 ~ '#00a884',
                           CODE_BASELINE == 60 ~ '#686868',
                           CODE_BASELINE == 70 ~ '#ff0000',
                           CODE_BASELINE == 80 ~ '#004da8',
                           CODE_BASELINE == 90 ~ '#bee8ff',
                           CODE_BASELINE == 110 ~ '#737300',
                           CODE_BASELINE == 120 ~ '#734c00',
                           TRUE ~ COLOR)) %>%
  complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
plot(baseline_simple) # preview basic plot

## scenario 1
scenario1_added = rast('GIS/scenario_inputs/restoration_added_ripdetail.tif') %>%
  subst(c(70:79), 70) %>% subst(c(81:82), 80)
levels(scenario1_added) <- data.frame(CODE_BASELINE = c(70, 80),
                                      LABEL = c('Riparian', 'Wetland'))
coltab(scenario1_added) <- data.frame(CODE_BASELINE = c(70, 80),
                                      COLOR = c('#ff0000', '#004da8')) %>%
  complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
plot(scenario1_added)

## scenario 2
scenario2_added = rast('GIS/scenario_rasters/scenario2_perennialexpand.tif') %>%
  mask(rast('GIS/scenario_inputs/perex_added.tif')) %>%
  subst(c(10:19), 10)
levels(scenario2_added) <- data.frame(CODE_BASELINE = 10,
                                      LABEL = 'Perennial Crops')
coltab(scenario2_added) <- data.frame(CODE_BASELINE = 10,
                                      COLOR = '#aa66cd') %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
plot(scenario2_added)

## scenario 3
scenario3_added = cover(scenario1_added, scenario2_added)
levels(scenario3_added) <- data.frame(CODE_BASELINE = c(10, 70, 80),
                                      LABEL = c('Perennial Crops', 'Riparian', 'Wetland'))
coltab(scenario3_added) <- data.frame(CODE_BASELINE = c(10, 70, 80),
                                      COLOR = c('#aa66cd', '#ff0000', '#004da8')) %>%
  complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
plot(scenario3_added)

## combine all:
mapset = c(baseline_simple, scenario1_added, scenario2_added, scenario3_added)
names(mapset) = c('A', 'B', 'C', 'D')

mapset_df = as.data.frame(mapset, xy = TRUE, na.rm = FALSE) %>%
  pivot_longer(A:D, names_to = 'scenario') %>% drop_na() %>%
  mutate(COLOR = case_when(value == 'Perennial Crops' ~ '#aa66cd',
                           value == 'Riparian' ~ '#ff0000',
                           value == 'Wetland' ~ '#004da8',
                           value == 'Grain & Hay' ~ '#ffebaf',
                           value == 'Field & Row Crops' ~ '#ffa77f',
                           value == 'Corn' ~ '#ffff00',
                           value == 'Rice' ~ '#e600a9',
                           value == 'Idle' ~ '#e1e1e1',
                           value == 'Grassland & Pasture' ~ '#e9ffbe',
                           value == 'Alfalfa' ~ '#00a884',
                           value == 'Urban' ~ '#686868',
                           value == 'Riparian' ~ '#004da8',
                           value == 'Wetland' ~ '#004da8',
                           value == 'Water' ~ '#bee8ff',
                           value == 'Woodland' ~ '#737300',
                           value == 'Scrub' ~ '#734c00',
                           value == 'Barren' ~ '#ffffff'))

pal = mapset_df %>% select(value, COLOR) %>% distinct() %>%
  mutate(value = factor(value,
                        levels = levels(baseline_simple)[[1]] %>%
                          as_tibble('value') %>% drop_na() %>% pull(value))) %>%
  arrange(value)
pal2 = structure(pal$COLOR, names = as.character(pal$value))

showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
ggplot(mapset_df) + facet_wrap(~scenario, ncol = 4) +
  geom_raster(aes(x, y, fill = as.factor(value))) +
  geom_sf(data = delta_shp, fill = NA, color = 'black') +
  scale_fill_manual(values = pal2) +
  guides(fill = guide_legend(ncol = 5)) +
  labs(x = NULL, y = NULL, fill = 'Land cover') +
  theme_void() +
  theme(aspect.ratio = 1.4,
        strip.text = element_text(family = 'sourcesans', face = 'bold',
                                  hjust = 0),
        legend.title = element_text(family = 'sourcesans', face = 'bold',
                                    size = 9.5),
        legend.text = element_text(family = 'sourcesans', size = 8),
        legend.key.size = unit(9, 'pt'),
        legend.position = 'bottom',
        panel.spacing = unit(-.5, "lines"))
ggsave(filename = 'fig/map_baseline&change.jpg',
       height = 4, width = 9, units = 'in', dpi = 300)
showtext_auto(FALSE)

