# README---------
# - compile land cover totals for the baseline and each scenario
# - calculate the net impact of each scenario on the coverage of each land cover
#     class
# - plot the land cover change for each major class and select subclasses
# - plot the map of each scenario

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')
library(tidyterra)
library(patchwork)
library(showtext)
font_add_google('Source Sans Pro', 'sourcesans')

# REFERENCE DATA
key = readxl::read_excel('GIS/VEG_key.xlsx')

# LAND COVER CHANGE----------
# total area of each land cover class/subclass in each landscape:
landcover = DeltaMultipleBenefits::sum_landcover(
  landscapes = 'GIS/scenario_rasters',
  mask = 'GIS/boundaries/delta.tif',
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
  DeltaMultipleBenefits::sum_change() %>%
  select(-ends_with('SE', ignore.case = TRUE))
write_csv(landcover_change, 'output/netchange_landcover.csv')

# PLOT CHANGE-------
landcover_change = read_csv('output/netchange_landcover.csv') %>%
  filter(grepl('scenario1|alt', scenario))

change_summary = bind_rows(
  # major landcover classes:
  landcover_change %>%
    # remove riparian and wetland subclasses
    filter(!grepl('RIPARIAN_|WETLAND_MANAGED_|WATER', CODE_NAME)) %>%
    # lump other classes
    mutate(LABEL = case_when(grepl('ORCHARD_|VINEYARD', CODE_NAME) ~ 'Perennial Crops',
                             grepl('GRAIN|HAY', CODE_NAME) ~ 'Grain & Hay',
                             # grepl('FIELD_CORN', CODE_NAME) ~ 'Corn', #specify so it doesn't combine with field & row
                             grepl('FIELD|ROW', CODE_NAME) ~ 'Field & Row Crops',
                             # grepl('ALFALFA', CODE_NAME) ~ 'Alfalfa',
                             grepl('GRASS|PASTURE', CODE_NAME) ~ 'Grassland & Pasture',
                             grepl('WOODLAND|SCRUB', CODE_NAME) ~ 'Woodland & Scrub',
                             grepl('WETLAND', CODE_NAME) ~ 'Wetland',
                             TRUE ~ LABEL),
           LABEL = factor(LABEL,
                          levels = c('Perennial Crops', 'Grain & Hay',
                                     'Field & Row Crops', #'Corn',
                                     'Rice', 'Grassland & Pasture', #'Alfalfa',
                                     'Idle', 'Urban', 'Riparian',
                                     'Wetland',
                                     'Woodland & Scrub', 'Barren') %>%
                            rev())) %>%
    group_by(scenario, LABEL) %>%
    summarize(net_change = sum(net_change),
              change_pct = net_change/sum(BASELINE) * 100,
              .groups = 'drop') %>%
    # filter(!LABEL %in% c('Urban', 'Barren', 'Water', 'Woodland', 'Scrub')) %>%
    mutate(bin = if_else(net_change >= 0, 'increase', 'decrease'),
           group = 'Major classes') %>%
    arrange(scenario, LABEL),
  # wetland and riparian subclass detail
  landcover_change %>% filter(grepl('RIPARIAN_|WETLAND_|ORCHARD|VINEYARD', CODE_NAME)) %>%
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
                                     'Other Wetland') %>% rev())) %>%
    group_by(scenario, LABEL) %>%
    summarize(net_change = sum(net_change),
              change_pct = net_change/sum(BASELINE) * 100,
              .groups = 'drop') %>%
    # filter(!LABEL %in% c('Urban', 'Barren', 'Water', 'Woodland', 'Scrub')) %>%
    mutate(bin = if_else(net_change >= 0, 'increase', 'decrease'),
           group = 'Subclasses') %>%
    arrange(scenario, LABEL))

## for manuscript---------
change_major = change_summary %>% filter(group == 'Major classes') %>%
  mutate(scenario = recode(scenario,
                           'scenario1_restoration' = 'A',
                           'scenario2_perennialexpand_alt' = 'B',
                           'scenario3_combo_alt' = 'C'))

part1 = ggplot(change_major, aes(net_change/1000, LABEL)) +
  facet_wrap(~scenario, ncol = 3) +
  geom_col(aes(fill = bin)) +
  scale_fill_manual(values = c(pointblue.palette[c(3, 1)])) +
  geom_text(data = change_major %>% filter(group == 'Major classes' & bin == 'increase'),
            aes(x = net_change/1000 + 3,
                label = paste0(round(change_pct, digits = 0), '%')), size = 2.5) +
  geom_text(data = change_major %>% filter(group == 'Major classes' & bin == 'decrease'),
            aes(x = net_change/1000 - 3,
                label = paste0(round(change_pct, digits = 0), '%')), size = 2.5) +
  labs(y = NULL, x = NULL) +
  xlim(-12, 20) +
  geom_vline(xintercept = 0, size = 0.2) +
  theme_bw() +
  theme(
    strip.placement = 'outside',
    strip.text = element_text(family = 'sourcesans', size = 9, vjust = 0, hjust = 0, face = 'bold'),
    strip.background = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = 'sourcesans', size = 9, hjust = 1),
    axis.title = element_text(family = 'sourcesans', size = 9),
    plot.title = element_text(family = 'sourcesans', size = 9, face = 'bold'),
    legend.position = 'none')

change_minor = change_summary %>% filter(group != 'Major classes') %>%
  mutate(scenario = recode(scenario,
                           'scenario1_restoration' = 'D',
                           'scenario2_perennialexpand_alt' = 'E',
                           'scenario3_combo_alt' = 'F'))

part2 = change_minor %>%
  ggplot(aes(net_change/1000, LABEL)) +
  facet_wrap(~scenario, ncol = 3) +
  geom_col(aes(fill = bin)) +
  scale_fill_manual(values = c(pointblue.palette[c(3, 1)])) +
  geom_text(data = change_minor %>% filter(bin == 'increase'),
            aes(x = net_change/1000 + 3.5,
                label = paste0(round(change_pct, digits = 0), '%')), size = 2.5) +
  geom_text(data = change_minor %>% filter(bin == 'decrease'),
            aes(x = net_change/1000 - 3,
                label = paste0(round(change_pct, digits = 0), '%')), size = 2.5) +
  labs(x = expression(paste(Delta, ' total area (ha, thousands)')),
       y = NULL) +
  xlim(-12, 20) +
  geom_vline(xintercept = 0, size = 0.2) +
  theme_bw() +
  theme(
    strip.placement = 'outside',
    strip.text = element_text(family = 'sourcesans', size = 9, vjust = 0, hjust = 0, face = 'bold'),
    strip.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(family = 'sourcesans', size = 9, hjust = 1),
    axis.title = element_text(family = 'sourcesans', size = 9),
    plot.title = element_text(family = 'sourcesans', size = 9, face = 'bold'),
    legend.position = 'none')

showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
part1/part2 + plot_layout(heights = c(0.4, 0.6))
ggsave('fig/netchange_land_cover.jpg',
       height = 5, width = 6.5, units = 'in', dpi = 300)
showtext_auto(FALSE)


## for presentation-------
showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
change_summary %>% filter(group == 'Major classes') %>%
  ggplot(aes(net_change/1000, LABEL)) + facet_wrap(~scenario) +
  geom_col(aes(fill = bin)) +
  geom_text(data = change_summary %>% filter(group == 'Major classes' & bin == 'increase'),
            aes(x = net_change/1000 + 2,
                label = paste0(round(change_pct, digits = 0), '%')), size = 3.5) +
  geom_text(data = change_summary %>% filter(group == 'Major classes' & bin == 'decrease'),
            aes(x = net_change/1000 - 2.25,
                label = paste0(round(change_pct, digits = 0), '%')), size = 3.5) +
  scale_fill_manual(values = c(pointblue.palette[c(3, 1)])) +
  labs(x = expression(paste(Delta, ' total area (ha, thousands)')),
       y = NULL) +
  geom_vline(xintercept = 0, size = 0.2) +
  xlim(-10, 20) +
  theme_bw() +
  theme(strip.text = element_text(family = 'sourcesans', size = 18, hjust = 0, face = 'bold'),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(family = 'sourcesans', size = 16),
        axis.text.y = element_text(hjust = 1),
        axis.title = element_text(family = 'sourcesans', size = 18),
        legend.position = 'none')
ggsave('fig/presentations/netchange_land_cover.jpg',
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

# label.order = key %>% select(CODE_NAME, LABEL) %>% distinct() %>%
#   filter(!CODE_NAME %in% c(
#     'PERENNIAL_CROPS', 'ANNUAL_CROPS', 'GRAIN&HAY', 'FIELD',
#     'GRASSLAND&PASTURE', 'WOODLAND&SCRUB', 'WATER', 'WETLAND',
#     'WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL')) %>%
#   filter(!grepl('RIPARIAN_', CODE_NAME) & !is.na(CODE_NAME)) %>%
#   pull(LABEL) %>% unique()
# label.order = key %>% select(CODE_NAME, LABEL) %>% distinct() %>%
#   filter(CODE_NAME %in% c(
#     'PERENNIAL_CROPS', 'GRAIN&HAY', 'FIELD', 'RICE', 'IDLE',
#     'GRASSLAND&PASTURE', 'URBAN', 'RIPARIAN', 'WETLAND_MANAGED',
#     'WETLAND_TIDAL', 'WETLAND_OTHER', 'WATER', 'WOODLAND&SCRUB', 'BARREN')) %>%
#   pull(LABEL) %>% unique()
#
# label.order = label.order[c(1:9, 11:13, 10, 14:22)]

# simplify baseline to major land cover classes for manuscript
baseline_simple = classify(scenarios$baseline,
                           rcl = matrix(
                             c(10.5, 19.5, 10, #Perennial crops
                               21.5, 24.5, 21, #Grain & hay
                               24.5, 28.5, 25, #Field & row (incl Corn)
                               49.5, 57.5, 50, #Grassland & pasture (incl Alfalfa)
                               70.5, 79.5, 70, #Riparian
                               80.5, 89.5, 80, #Wetlands
                               99.5, 120.5, 100), #woodland & scrub
                             byrow = TRUE, ncol = 3)
)
levels(baseline_simple) <- key %>% select(CODE_BASELINE, LABEL) %>%
  filter(CODE_BASELINE %in% c(10, 21, 25, 30, 40, 50, 60, 70, 80, 90, 100, 130)) %>%
  mutate(LABEL = if_else(LABEL == 'Field Crops', 'Field & Row Crops', LABEL)) %>%
  as.data.frame()
# coltab(baseline_simple) <- key %>% select(CODE_BASELINE, COLOR) %>% drop_na() %>%
#   filter(CODE_BASELINE %in% c(10, 21, 25, 26, 30, 40, 50, 52, 60, 70, 80, 90:130)) %>%
#   mutate(COLOR = case_when(CODE_BASELINE == 10 ~ '#aa66cd',
#                            CODE_BASELINE == 21 ~ '#ffebaf',
#                            CODE_BASELINE == 25 ~ '#ffa77f',
#                            CODE_BASELINE == 26 ~ '#ffff00',
#                            CODE_BASELINE == 30 ~ '#e600a9',
#                            CODE_BASELINE == 40 ~ '#e1e1e1',
#                            CODE_BASELINE == 50 ~ '#e9ffbe',
#                            CODE_BASELINE == 52 ~ '#00a884',
#                            CODE_BASELINE == 60 ~ '#686868',
#                            CODE_BASELINE == 70 ~ '#ff0000',
#                            CODE_BASELINE == 80 ~ '#004da8',
#                            CODE_BASELINE == 90 ~ '#bee8ff',
#                            CODE_BASELINE == 110 ~ '#737300',
#                            CODE_BASELINE == 120 ~ '#734c00',
#                            TRUE ~ COLOR)) %>%
#   complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
# plot(baseline_simple) # preview basic plot

# water
water = baseline_simple %>% mask(delta)
levels(water) <- NULL
water = subst(water, c(0:89, 91:150), NA)
levels(water) <- data.frame(CODE_BASELINE = 90,
                            LABEL = 'Water')

## scenario 1
scenario1_added = rast('GIS/scenario_inputs/restoration_added_ripdetail.tif') %>%
  subst(c(70:79), 70) %>% subst(c(81:82), 80) %>%
  cover(water)
levels(scenario1_added) <- data.frame(CODE_BASELINE = c(70, 80, 90),
                                      LABEL = c('Riparian', 'Wetland', 'Water'))
coltab(scenario1_added) <- data.frame(CODE_BASELINE = c(70, 80, 90),
                                      COLOR = c('#ff0000', '#004da8', '#bee8ff')) %>%
  complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
plot(scenario1_added)

## scenario 2
scenario2_added = rast('GIS/scenario_rasters/scenario2_perennialexpand_alt.tif') %>%
  mask(rast('GIS/scenario_inputs/perex_added_alt.tif')) %>%
  cover(water)
scenario2_added = classify(scenario2_added,
  rcl = data.frame(from = c(10:19), to = 10) %>% as.matrix())
levels(scenario2_added) <- data.frame(CODE_BASELINE = c(10, 90),
                                      LABEL = c('Perennial Crops', 'Water'))
coltab(scenario2_added) <- data.frame(CODE_BASELINE = c(10, 90),
                                      COLOR = c('#aa66cd', '#bee8ff')) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
plot(scenario2_added)

## scenario 3
scenario3_added = cover(scenario1_added, scenario2_added) %>% cover(water)
levels(scenario3_added) <- data.frame(CODE_BASELINE = c(10, 70, 80, 90),
                                      LABEL = c('Perennial Crops', 'Riparian', 'Wetland', 'Water'))
coltab(scenario3_added) <- data.frame(CODE_BASELINE = c(10, 70, 80, 90),
                                      COLOR = c('#aa66cd', '#ff0000', '#004da8', '#bee8ff')) %>%
  complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
plot(scenario3_added)

# city labels
library(maps)
citydat = us.cities
citydat = citydat %>% filter(country.etc == 'CA') %>%
  filter(grepl('Sacramento|Stockton|Antioch|Tracy', name)) %>%
  filter(!grepl('Parkway|West', name)) %>%
  mutate(name = gsub(' CA', '', name)) %>%
  st_as_sf(coords = c('long', 'lat'))
st_crs(citydat) <- 4326
citydat = citydat %>% st_transform(crs = st_crs(delta_shp))
citydat = bind_cols(citydat, st_coordinates(citydat) %>% as_tibble())


## combine all:
mapset = c(baseline_simple, scenario1_added, scenario2_added, scenario3_added)
names(mapset) = c('A', 'B', 'C', 'D')

## for manuscript--------
pal = freq(mapset$A) %>%
  mutate(COLOR = case_when(value == 'Perennial Crops' ~ '#9939ca',
                           value == 'Riparian' ~ '#ff0000',
                           value == 'Wetland' ~ '#004da8',
                           value == 'Grain & Hay' ~ '#ffebaf',
                           #value == 'Field & Row Crops' ~ '#ff844c',
                           value == 'Field & Row Crops' ~ '#ffffa6',
                           #value == 'Corn' ~ '#ffff00',
                           #value == 'Rice' ~ '#e600a9',
                           value == 'Rice' ~ '#f163cc',
                           value == 'Idle' ~ '#ffddce',
                           value == 'Grassland & Pasture' ~ '#D2D59A',
                           #value == 'Alfalfa' ~ '#00a884',
                           value == 'Urban' ~ '#474747',
                           value == 'Riparian' ~ '#004da8',
                           value == 'Wetland' ~ '#004da8',
                           value == 'Water' ~ 'white',
                           value == 'Woodland & Scrub' ~ '#737300',
                           #value == 'Scrub' ~ '#734c00',
                           value == 'Barren' ~ '#ddccac'))
pal2 = pal %>%
  mutate(COLOR = if_else(value == 'Water', '#bee8ff', COLOR))

pal = structure(pal$COLOR, names = as.character(pal$value))
pal2 = structure(pal2$COLOR, names = as.character(pal2$value))


p1 = ggplot() +
  geom_sf(data = delta_shp, fill = 'gray80', color = 'black', size = 0.4) +
  geom_spatraster(data = mapset$A) +
  scale_fill_manual(values = pal, na.value = 'transparent') +
  geom_sf(data = delta_shp, fill = NA, color = 'black', size = 0.4) +
  # geom_text(data = citydat, aes(X, Y, label = name), size = 2, color = 'white', fontface = 'bold') +
  geom_label(
    data = citydat,
    aes(X, Y, label = name),
    color = "black", size = 2,
    hjust = c(1, 0, 0, 1),
    nudge_x = c(0, 100, 250, -200),
    nudge_y = c(-3000, 0, 0, -3000),
    fill = 'white',
    label.padding = unit(0.15, 'lines'),
    label.size = 0.15
  ) +
  labs(x = NULL, y = NULL, fill = 'Land cover')

# scenarios
p2 = ggplot() +
  geom_sf(data = delta_shp, fill = 'gray80', color = 'black', size = 0.4) +
  geom_spatraster(data = mapset$B) +
  scale_fill_manual(values = pal, na.value = 'transparent') +
  geom_sf(data = delta_shp, fill = NA, color = 'black', size = 0.4) +
  labs(x = NULL, y = NULL, fill = 'Land cover')

p3 = ggplot() +
  geom_sf(data = delta_shp, fill = 'gray80', color = 'black', size = 0.4) +
  geom_spatraster(data = mapset$C) +
  scale_fill_manual(values = pal, na.value = 'transparent') +
  geom_sf(data = delta_shp, fill = NA, color = 'black', size = 0.4) +
  labs(x = NULL, y = NULL, fill = 'Land cover')

p4 = ggplot() +
  geom_sf(data = delta_shp, fill = 'gray80', color = 'black', size = 0.4) +
  geom_spatraster(data = mapset$D) +
  scale_fill_manual(values = pal, na.value = 'transparent') +
  geom_sf(data = delta_shp, fill = NA, color = 'black', size = 0.4) +
  labs(x = NULL, y = NULL, fill = 'Land cover') +
  theme_void()


showtext_auto()
showtext_opts(dpi = 300) #default for ggsave

patchwork = (p1 + p2)/(p3 + p4)
patchwork +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = 'collect') &
  theme_void() +
  theme(aspect.ratio = 1.4,
        legend.position = 'bottom',
        legend.title = element_text(family = 'sourcesans', face = 'bold',
                                    size = 9.5),

        legend.text = element_text(family = 'sourcesans', size = 8),
        legend.key.size = unit(9, 'pt'),
        panel.spacing = unit(-1, "lines"),
        plot.tag = element_text(size = 9, family = 'sourcesans', face = 'bold'))

ggsave(filename = 'fig/map_scenarios.jpg',
       height = 8, width = 6, units = 'in', dpi = 300)
showtext_auto(FALSE)

## for presentation-------
patchwork2 = p1 | p2 | p3 | p4
patchwork2 +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = 'collect') &
  theme_void() +
  theme(aspect.ratio = 1.4,
        legend.position = 'bottom',
        legend.title = element_text(family = 'sourcesans', face = 'bold',
                                    size = 9.5),

        legend.text = element_text(family = 'sourcesans', size = 8),
        legend.key.size = unit(9, 'pt'),
        panel.spacing = unit(-1, "lines"),
        plot.tag = element_text(size = 9, family = 'sourcesans', face = 'bold'))

ggsave(filename = 'fig/presentations/map_scenarios.jpg',
       height = 5, width = 10, units = 'in', dpi = 300)


# WRITE METADATA--------
# original landscape maps:
scenarios = terra::rast(list.files('GIS/scenario_rasters', '.tif$', full.names = TRUE))

freq = freq(scenarios)
rat = freq %>% select(layer, CODE = value, count, CODE_NAME = label) %>%
  left_join(key %>% select(CODE_NAME, LABEL), by = 'CODE_NAME') %>%
  select(layer, CODE, CODE_NAME, LABEL, count) %>%
  split(.$layer)
names(rat) = names(scenarios)

purrr::map(names(rat),
           ~rat[[.x]] %>% select(-layer) %>%
             foreign::write.dbf(file = paste0('GIS/scenario_rasters/', .x, '.tif.vat.dbf')))

