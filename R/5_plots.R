# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')

# REFERENCE DATA
delta = rast('GIS/boundaries/delta.tif')
county_raster = rast('GIS/landscape_rasters/boundaries/counties.tif')

delta = rast('GIS/boundaries/delta.tif')
delta_shp = read_sf('GIS/boundaries/Legal_Delta_Boundary.shp') %>%
  st_transform(st_crs('EPSG:32610'))

# original landscape maps:
mapstack = c(rast('GIS/landscape_rasters/veg_baseline.tif'),
             rast('GIS/scenario_rasters/scenario1_restoration.tif'),
             rast('GIS/scenario_rasters/scenario2_perennialexpand.tif'))
key = readxl::read_excel('GIS/VEG_key.xlsx')

label.order = key %>% select(CODE_NAME, LABEL) %>% distinct() %>%
  filter(!CODE_NAME %in% c(
    'PERENNIAL_CROPS', 'ANNUAL_CROPS', 'GRAIN&HAY', 'FIELD',
    'GRASSLAND&PASTURE', 'WOODLAND&SCRUB', 'WATER', 'WETLAND',
    'WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL')) %>%
  filter(!grepl('RIPARIAN_', CODE_NAME) & !is.na(CODE_NAME)) %>%
  pull(LABEL) %>% unique()
label.order = label.order[c(1:9, 11:13, 10, 14:22)]

# SCENARIO MAP--------
# plot (simplified) baseline and the pixels that change in each scenario

# simplify baseline to major land cover classes for manuscript
baseline_simple = classify(mapstack$baseline,
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

mapset = c(baseline_simple, scenario1_added, scenario2_added)
names(mapset) = c('A', 'B', 'C')

mapset_df = as.data.frame(mapset, xy = TRUE, na.rm = FALSE) %>%
  pivot_longer(A:C, names_to = 'scenario') %>% drop_na() %>%
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

library(showtext)
font_add_google('Source Sans Pro', 'sourcesans')

showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
ggplot(mapset_df) + facet_wrap(~scenario) +
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
       height = 4.5, width = 8, units = 'in', dpi = 300)
showtext_auto(FALSE)


# LAND COVER CHANGE-------
# plot changes in major land cover classes
change = read_csv('output/land_cover_change.csv')
change_county = read_csv('output/land_cover_change_county.csv')

change_summary = bind_rows(
  # major landcover classes:
  change %>%
    # remove riparian and wetland subclasses
    filter(!grepl('RIPARIAN_|WETLAND_MANAGED_', class)) %>%
    # lump other classes
    mutate(LABEL = case_when(grepl('ORCHARD_|VINEYARD', class) ~ 'Perennial Crops',
                             grepl('GRAIN|HAY', class) ~ 'Grain & Hay',
                             grepl('FIELD_CORN', class) ~ 'Corn', #specify so it doesn't combine with field & row
                             grepl('FIELD|ROW', class) ~ 'Field & Row Crops',
                             grepl('ALFALFA', class) ~ 'Alfalfa',
                             grepl('GRASS|PASTURE', class) ~ 'Grassland & Pasture',
                             grepl('wetland', class, ignore.case = TRUE) ~ 'Wetland',
                             TRUE ~ LABEL)) %>%
    group_by(scenario, LABEL) %>%
    summarize(net_change = sum(net_change),
              change_pct = net_change/sum(value_baseline) * 100,
              .groups = 'drop') %>%
    # filter(!LABEL %in% c('Urban', 'Barren', 'Water', 'Woodland', 'Scrub')) %>%
    mutate(LABEL = factor(LABEL,
                          levels = c('Perennial Crops', 'Grain & Hay', 'Field & Row Crops',
                                     'Corn', 'Rice', 'Idle', 'Grassland & Pasture', 'Alfalfa', 'Urban',
                                     'Riparian', 'Wetland', 'Water', 'Woodland', 'Scrub', 'Barren') %>% rev()),
           bin = if_else(net_change > 0, 'increase', 'decrease'),
           group = 'Major classes') %>%
    arrange(scenario, LABEL),
  # wetland and riparian subclass detail
  change %>% filter(grepl('RIPARIAN_|WETLAND_', class)) %>%
    filter(class != 'WETLAND_MANAGED') %>%
    mutate(LABEL = factor(LABEL,
                          levels = c('Riparian - Cottonwood',
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
           group = 'Riparian & Wetland subclasses') %>%
    select(scenario, LABEL, net_change, bin, group)
) %>%
  mutate(scenario = recode(scenario,
                           'scenario1_restoration' = 'A',
                           'scenario2_perennialexpand' = 'B'))
# --> but why show riparian and wetland subclasses and not others?

library(showtext)
font_add_google('Source Sans Pro', 'sourcesans')
showtext_auto()
showtext_opts(dpi = 300) #default for ggsave

change_summary %>% filter(group == 'Major classes') %>%
  ggplot(aes(net_change/1000, LABEL)) +
  facet_wrap(~scenario, ncol = 2) +
  geom_col(fill = 'gray60') +
  geom_text(data = change_summary %>% filter(group == 'Major classes' & bin == 'increase'),
            aes(x = net_change/1000 + 2,
                label = paste0(round(change_pct, digits = 0), '%')), size = 2) +
  geom_text(data = change_summary %>% filter(group == 'Major classes' & bin == 'decrease'),
            aes(x = net_change/1000 - 2,
                label = paste0(round(change_pct, digits = 0), '%')), size = 2) +
  labs(x = expression(paste(Delta, ' total area (ha, thousands)')),
       y = NULL) +
  xlim(-7, 20) +
  geom_vline(xintercept = 0, size = 0.2) +
  theme_bw() +
  theme(strip.text = element_text(family = 'sourcesans', size = 9.5, hjust = 0, face = 'bold'),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(family = 'sourcesans', size = 8),
        axis.text.y = element_text(hjust = 1),
        axis.title = element_text(family = 'sourcesans', size = 8),
        legend.position = 'none')
ggsave('fig/netchange_land_cover.jpg',
       height = 3, width = 5, units = 'in', dpi = 300)
showtext_auto(FALSE)


# for presentation
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


# METRICS------
# illustrate overall changes in metrics

net_change = read_csv('output/scenario_change.csv')
net_change_county = read_csv('output/scenario_change_county.csv')

metriclist = unique(net_change$METRIC)

net_change_format = net_change %>%
  mutate(
    net_change = if_else(METRIC_CATEGORY == 'Water Quality', -1 * net_change, net_change),
    METRIC = case_when(
      METRIC_SUBTYPE == 'species distribution: waterbird_fall' ~ paste0(METRIC, ' (fall)'),
      METRIC_SUBTYPE == 'species distribution: waterbird_win' ~ paste0(METRIC, ' (winter)'),
      TRUE ~ METRIC),
    METRIC_CATEGORY = case_when(
      METRIC_SUBTYPE == 'species distribution: riparian' ~ 'Riparian landbird distributions',
      METRIC_SUBTYPE == 'species distribution: waterbird_fall' ~ 'Waterbird distributions',
      METRIC_SUBTYPE == 'species distribution: waterbird_win' ~ 'Waterbird distributions',
      # METRIC_SUBTYPE == 'avian conservation contribution' ~ 'Avian Conservation Score',
      TRUE ~ METRIC_CATEGORY),
    METRIC_CATEGORY = factor(
      METRIC_CATEGORY,
      levels = c('Agricultural Livelihoods',
                 'Water Quality',
                 'Climate Change Resilience',
                 'Riparian landbird distributions',
                 'Waterbird distributions'
                 # 'Avian Conservation Score'
                 )),
    # METRIC = recode(METRIC,
    #                 'Breeding Landbirds: Grassland' = 'Grassland landbirds (breeding)',
    #                 'Breeding Landbirds: Oak Savannah' = 'Oak Savannah landbirds (breeding)',
    #                 'Breeding Landbirds: Riparian' = 'Riparian landbirds (breeding)',
    #                 'Breeding Waterbirds: Waterfowl' = 'Waterfowl (breeding)',
    #                 'Breeding Waterbirds: Shorebirds' = 'Shorebirds (breeding)',
    #                 'Breeding Waterbirds: Other Waterbirds' = 'Other waterbirds (breeding)',
    #                 'Wintering Waterbirds: Waterfowl' = 'Waterfowl (wintering)',
    #                 'Wintering Waterbirds: Shorebirds' = 'Shorebirds (wintering)',
    #                 'Wintering Waterbirds: Other Waterbirds' = 'Other waterbirds (wintering)'),
    METRIC = factor(
      METRIC,
      levels = c(metriclist[c(1:3, 19:25)], #28:35)],
                 "Nuttall's Woodpecker",
                 'Ash-throated Flycatcher',
                 'Black-headed Grosbeak',
                 'Lazuli Bunting',
                 'Common Yellowthroat',
                 'Yellow Warbler',
                 'Spotted Towhee',
                 'Song Sparrow',
                 'Yellow-breasted Chat',
                 'Geese (fall)', 'Geese (winter)',
                 'Dabbling Ducks (fall)', 'Dabbling Ducks (winter)',
                 'Diving Ducks (winter)',
                 'Cranes (fall)', 'Cranes (winter)',
                 'Shorebirds (fall)', 'Shorebirds (winter)',
                 'Herons/Egrets (fall)', 'Herons/Egrets (winter)'
                 # 'Grassland landbirds (breeding)',
                 # 'Oak Savannah landbirds (breeding)',
                 # 'Riparian landbirds (breeding)',
                 # 'Waterfowl (breeding)',
                 # 'Waterfowl (wintering)',
                 # 'Shorebirds (breeding)',
                 # 'Shorebirds (wintering)',
                 # 'Other waterbirds (breeding)',
                 # 'Other waterbirds (wintering)'
                 ) %>% rev()),
    bin = if_else(change_pct > 0, 'benefit', 'trade-off'),
    change_pct_se = if_else(METRIC == 'Salinity', NA_real_, change_pct_se))

## horizontal bar--------
# bar chart with error bars for reporting and manuscript

### percent change----------
h1 = net_change_format %>%
  filter(!grepl('Avian', METRIC_CATEGORY) &
           scenario == 'scenario1_restoration') %>%
  mutate() %>%
  ggplot(aes(change_pct, METRIC)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(aes(fill = bin)) +
  geom_errorbar(aes(xmin = change_pct - change_pct_se,
                    xmax = change_pct + change_pct_se), width = 0.5) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_vline(xintercept = 0, color = 'gray30') +
  labs(x = '% change', y = NULL, title = 'A') +
  xlim(-35, 55) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text = element_text(family = 'sourcesans', size = 9),
        axis.title = element_text(family = 'sourcesans', size = 10),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 10),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')

h2 = net_change_format %>%
  filter(!grepl('Avian', METRIC_CATEGORY) &
           scenario == 'scenario2_perennialexpand') %>%
  mutate(bin = if_else(change_pct > 0, 'benefit', 'trade-off')) %>%
  ggplot(aes(change_pct, METRIC)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(aes(fill = bin)) +
  geom_errorbar(aes(xmin = change_pct - change_pct_se,
                    xmax = change_pct + change_pct_se), width = 0.5) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_vline(xintercept = 0, color = 'gray30') +
  labs(x = '% change', y = NULL, title = 'B') +
  xlim(-35, 55) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text.x = element_text(family = 'sourcesans', size = 9),
        axis.text.y = element_blank(),
        axis.title = element_text(family = 'sourcesans', size = 10),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 10),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0, color = 'white'),
        strip.background = element_blank(),
        legend.position = 'none')


showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
h1 + h2 + plot_layout(widths = c(1, 1))
ggsave('fig/netchange_barchart_all.png', height = 7.5, width = 6.5, units = 'in')
showtext_auto(FALSE)

### raw change--------
# maybe preferable?
h1_raw = net_change_format %>%
  filter(scenario == 'scenario1_restoration') %>%
  mutate(across(c(net_change, net_change_se),
                ~.x/1000)) %>%
  mutate(across(c(net_change, net_change_se),
                ~if_else(grepl('distributions', METRIC_CATEGORY),
                         .x/10, .x))) %>%
  ggplot(aes(net_change, METRIC)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(aes(fill = bin)) +
  geom_errorbar(aes(xmin = net_change - net_change_se,
                    xmax = net_change + net_change_se), width = 0.5) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_vline(xintercept = 0, color = 'gray30') +
  labs(x = 'net change', y = NULL, title = 'A') +
  xlim(-100, 200) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text = element_text(family = 'sourcesans', size = 9),
        axis.title = element_text(family = 'sourcesans', size = 10),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 10),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')

h2_raw = net_change_format %>%
  filter(scenario != 'scenario1_restoration') %>%
  mutate(across(c(net_change, net_change_se),
                ~.x/1000)) %>%
  mutate(across(c(net_change, net_change_se),
                ~if_else(grepl('distributions', METRIC_CATEGORY),
                         .x/10, .x))) %>%
  ggplot(aes(net_change, METRIC)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(aes(fill = bin)) +
  geom_errorbar(aes(xmin = net_change - net_change_se,
                    xmax = net_change + net_change_se), width = 0.5) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_vline(xintercept = 0, color = 'gray30') +
  labs(x = 'net change', y = NULL, title = 'B') +
  xlim(-100, 200) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text.x = element_text(family = 'sourcesans', size = 9),
        axis.text.y = element_blank(),
        axis.title = element_text(family = 'sourcesans', size = 10),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 10),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0, color = 'white'),
        strip.background = element_blank(),
        legend.position = 'none')

h1_raw + h2_raw + plot_layout(widths = c(1, 1))
ggsave('fig/netchange_barchart_all_raw.png', height = 7.5, width = 6.5, units = 'in')

### raw without biodiversity------
h1a = net_change_format %>%
  filter(!grepl('bird', METRIC_CATEGORY) &
           scenario == 'scenario1_restoration') %>%
  mutate(across(c(net_change, net_change_se),
                ~.x/1000)) %>%
  ggplot(aes(net_change, METRIC)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(aes(fill = bin)) +
  geom_errorbar(aes(xmin = net_change - net_change_se,
                    xmax = net_change + net_change_se), width = 0.5) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_vline(xintercept = 0, color = 'gray30') +
  labs(x = 'net change', y = NULL, title = 'A') +
  xlim(-100, 200) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text = element_text(family = 'sourcesans', size = 9),
        axis.title = element_text(family = 'sourcesans', size = 10),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 10),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')

h2a = net_change_format %>%
  filter(!grepl('bird', METRIC_CATEGORY) &
           scenario == 'scenario2_perennialexpand') %>%
  mutate(across(c(net_change, net_change_se), ~.x/1000)) %>%
  mutate(bin = if_else(change_pct > 0, 'benefit', 'trade-off')) %>%
  ggplot(aes(net_change, METRIC)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(aes(fill = bin)) +
  geom_errorbar(aes(xmin = net_change - net_change_se,
                    xmax = net_change + net_change_se), width = 0.5) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_vline(xintercept = 0, color = 'gray30') +
  labs(x = 'net change', y = NULL, title = 'B') +
  xlim(-100, 200) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text.x = element_text(family = 'sourcesans', size = 9),
        axis.text.y = element_blank(),
        axis.title = element_text(family = 'sourcesans', size = 10),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 10),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0, color = 'white'),
        strip.background = element_blank(),
        legend.position = 'none')
h1a + h2a + plot_layout(widths = c(1, 1))
ggsave('fig/netchange_barchart_rawA.png', height = 4, width = 8, units = 'in')

### raw biodiversity-----
h1b = net_change_format %>%
  filter(grepl('bird', METRIC_CATEGORY) &
           scenario == 'scenario1_restoration') %>%
  mutate(across(c(net_change, net_change_se), ~.x/1000)) %>%
  ggplot(aes(net_change, METRIC)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(aes(fill = bin)) +
  geom_errorbar(aes(xmin = net_change - net_change_se,
                    xmax = net_change + net_change_se),
                width = 0.5) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_vline(xintercept = 0, color = 'gray30') +
  labs(x = 'net change', y = NULL, title = 'A') +
  xlim(-500, 2000) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text = element_text(family = 'sourcesans', size = 9),
        axis.title = element_text(family = 'sourcesans', size = 10),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 10),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')

h2b = net_change_format %>%
  filter(grepl('bird|Avian', METRIC_CATEGORY) &
           grepl('distribution', METRIC_SUBTYPE) &
           scenario == 'scenario2_perennialexpand') %>%
  mutate(across(c(net_change, net_change_se), ~.x/1000)) %>%
  ggplot(aes(net_change, METRIC)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(aes(fill = bin)) +
  geom_errorbar(aes(xmin = net_change - net_change_se,
                    xmax = net_change + net_change_se),
                width = 0.5) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_vline(xintercept = 0, color = 'gray30') +
  labs(x = 'net change', y = NULL,
       title = 'B') +
  xlim(-500, 2000) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text.x = element_text(family = 'sourcesans', size = 9),
        axis.text.y = element_blank(),
        axis.title = element_text(family = 'sourcesans', size = 10),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 10),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0, color = 'white'),
        strip.background = element_blank(),
        legend.position = 'none')

h1b + h2b + plot_layout(widths = c(1, 1))
ggsave('fig/netchange_barchart_rawB.png', height = 4, width = 8, units = 'in')
showtext_auto(F)


### raw by scenario----------

h1a + labs(title = NULL) +
  theme(axis.text.x = element_text(hjust = 1)) +
  h1b + labs(title = NULL) +
  plot_layout(widths = c(1, 1))
ggsave('fig/netchange_barchart_scenario1.png', height = 4, width = 8, units = 'in')

h2a + labs(title = NULL) +
  theme(axis.text.y = element_text(family = 'sourcesans', size = 9, hjust = 1),
        axis.text = element_text(family = 'sourcesans', size = 9),
        strip.text = element_text(color = 'black')) +
  h2b + labs(title = NULL) +
  theme(axis.text.y = element_text(family = 'sourcesans', size = 9, hjust = 1),
        axis.text = element_text(family = 'sourcesans', size = 9),
        strip.text = element_text(color = 'black')) +
  plot_layout(widths = c(1, 1))
ggsave('fig/netchange_barchart_scenario2.png', height = 4, width = 8, units = 'in')


## lollipop chart---------
# simple version for presentations without error bars

### all---------
# this is really too much for one figure!
l1 = net_change_format %>% filter(scenario == 'scenario1_restoration') %>%
  filter(METRIC != 'Overall') %>%
  mutate(bin = if_else(change_pct > 0, 'benefit', 'trade-off')) %>%
  ggplot(aes(change_pct, METRIC, fill = bin, color = bin)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(width = .25) +
  scale_color_manual(values = pointblue.palette[c(1,3)]) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_point(size = 10) +
  geom_text(aes(label = round(change_pct, digits = 0)),
            color = 'black', size = 5) +
  geom_vline(xintercept = 0) +
  labs(x = '% change', y = NULL,
       title = 'Scenario 1.\nHabitat Restoration') + xlim(-20, 20) +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20),
        strip.text = element_text(size = 16, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')

l2 = net_change_format %>% filter(scenario != 'scenario1_restoration') %>%
  filter(METRIC != 'Overall') %>%
  mutate(bin = if_else(change_pct > 0, 'benefit', 'trade-off')) %>%
  ggplot(aes(change_pct, METRIC, fill = bin, color = bin)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(width = .25) +
  scale_color_manual(values = pointblue.palette[c(1,3)]) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_point(size = 10) +
  geom_text(aes(label = round(change_pct, digits = 0)),
            color = 'black', size = 5) +
  geom_vline(xintercept = 0) +
  labs(x = '% change', y = NULL,
       title = 'Scenario 2.\nPerennial Crop Expansion') + xlim(-20, 20) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20),
        strip.text = element_text(size = 16, color = 'white'),
        strip.background = element_blank(),
        legend.position = 'none')
l1 + l2 + plot_layout(widths = c(1, 1))
ggsave('fig/netchange_summary.png', height = 15, width = 12, units = 'in')

### split------
# separate biodiversity metrics into a separate figure
showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
l1a = net_change_format %>%
  filter(!grepl('Biodiversity', METRIC_CATEGORY) &
           scenario == 'scenario1_restoration') %>%
  mutate(bin = if_else(change_pct > 0, 'benefit', 'trade-off')) %>%
  ggplot(aes(change_pct, METRIC, fill = bin, color = bin)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(width = .25) +
  scale_color_manual(values = pointblue.palette[c(1,3)]) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_point(size = 10) +
  geom_text(aes(label = round(change_pct, digits = 0)),
            color = 'black', size = 5) +
  geom_vline(xintercept = 0) +
  labs(x = '% change', y = NULL,
       title = 'Scenario 1.\nHabitat Restoration') + xlim(-20, 20) +
  theme_minimal() +
  theme(axis.text = element_text(family = 'sourcesans', size = 16),
        axis.title = element_text(family = 'sourcesans', size = 16),
        plot.title = element_text(family = 'sourcesans', size = 20),
        strip.text = element_text(family = 'sourcesans', size = 16, hjust = 0),
        strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = 'none')

l2a = net_change_format %>%
  filter(!grepl('Biodiversity', METRIC_CATEGORY) &
           scenario != 'scenario1_restoration') %>%
  mutate(bin = if_else(change_pct > 0, 'benefit', 'trade-off')) %>%
  ggplot(aes(change_pct, METRIC, fill = bin, color = bin)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(width = .25) +
  scale_color_manual(values = pointblue.palette[c(1,3)]) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_point(size = 10) +
  geom_text(aes(label = round(change_pct, digits = 0)),
            color = 'black', size = 5) +
  geom_vline(xintercept = 0) +
  labs(x = '% change', y = NULL,
       title = 'Scenario 2.\nPerennial Crop Expansion') + xlim(-20, 20) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_blank(),
        axis.title = element_text(family = 'sourcesans', size = 16),
        plot.title = element_text(family = 'sourcesans', size = 20),
        strip.text = element_text(family = 'sourcesans', size = 16, color = 'white'),
        strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = 'none')
l1a + l2a + plot_layout(widths = c(1, 1))
ggsave('fig/netchange_lollipopA.png', height = 7, width = 11, units = 'in')

l1b = net_change_format %>%
  filter(grepl('Biodiversity', METRIC_CATEGORY) &
           scenario == 'scenario1_restoration') %>%
  mutate(bin = if_else(change_pct > 0, 'benefit', 'trade-off')) %>%
  ggplot(aes(change_pct, METRIC, fill = bin, color = bin)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(width = .25) +
  scale_color_manual(values = pointblue.palette[c(1,3)]) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_point(size = 10) +
  geom_text(aes(label = round(change_pct, digits = 0)),
            color = 'black', size = 5) +
  geom_vline(xintercept = 0) +
  labs(x = '% change', y = NULL,
       title = 'Scenario 1.\nHabitat Restoration') + xlim(-25, 65) +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20),
        strip.text = element_text(size = 16, hjust = 0),
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        legend.position = 'none')

l2b = net_change_format %>%
  filter(grepl('Biodiversity', METRIC_CATEGORY) &
           scenario != 'scenario1_restoration') %>%
  mutate(bin = if_else(change_pct > 0, 'benefit', 'trade-off')) %>%
  ggplot(aes(change_pct, METRIC, fill = bin, color = bin)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(width = .25) +
  scale_color_manual(values = pointblue.palette[c(1,3)]) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_point(size = 10) +
  geom_text(aes(label = round(change_pct, digits = 0)),
            color = 'black', size = 5) +
  geom_vline(xintercept = 0) +
  labs(x = '% change', y = NULL,
       title = 'Scenario 2.\nPerennial Crop Expansion') + xlim(-25, 65) +
  theme_minimal() +
  theme(axis.text.x = element_text(family = 'sourcesans', size = 16),
        axis.text.y = element_blank(),
        axis.title = element_text(family = 'sourcesans', size = 16),
        plot.title = element_text(family = 'sourcesans', size = 20),
        strip.text = element_text(family = 'sourcesans', size = 16, color = 'white'),
        strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = 'none')
l1b + l2b + plot_layout(widths = c(1, 1))
ggsave('fig/netchange_lollipopB.png', height = 12, width = 11, units = 'in')


## radial bar---------
net_change_final %>%
  mutate(METRIC_CATEGORY = factor(METRIC_CATEGORY,
                                  levels = c('Agricultural Livelihoods',
                                             'Biodiversity Support',
                                             'Climate Change Resilience',
                                             'Water Quality')),
         METRIC = factor(METRIC, levels = net_change_final$METRIC[1:13])) %>%
  ggplot(aes(METRIC, change_pct_ha * 10000, fill = METRIC_CATEGORY)) +
  facet_wrap(~scenario) +
  geom_col(width = .9) +
  # geom_label(aes(label = round(change_pct, digits = 0)),
  #            label.r = unit(0.5, 'lines'),
  #            label.padding = unit(0.3, 'lines')) +
  geom_hline(yintercept = 0) +
  coord_polar() +
  labs(x = NULL, y = 'ha (millions)') +
  theme_minimal() +
  theme(legend.position = 'none')

## stellar------
net_change_final %>%
  mutate(METRIC_CATEGORY = factor(METRIC_CATEGORY,
                                  levels = c('Agricultural Livelihoods',
                                             'Biodiversity Support',
                                             'Climate Change Resilience',
                                             'Water Quality')),
         METRIC = factor(METRIC, levels = net_change_final$METRIC[1:16]),
         series1 = as.numeric(METRIC),
         series2 = LETTERS[as.numeric(METRIC)]) %>%
  # pivot_wider(names_from = series2, values_from = change_pct,
  #             values_fill = 0) %>%
  ggplot(aes(series1, change_pct, group = METRIC_CATEGORY)) +
  facet_wrap(~scenario) + ylim(-45, 45) +
  ggh4x::stat_difference(aes(ymin = 0, ymax = change_pct)) + #coord_polar() +
  geom_point()
  ggh4x::stat_difference(aes(ymin = 0, ymax = B)) +
  ggh4x::stat_difference(aes(ymin = 0, ymax = C)) +
  ggh4x::stat_difference(aes(ymin = 0, ymax = D)) +
  ggh4x::stat_difference(aes(ymin = 0, ymax = E))
  geom_polygon(aes(y = B)) +
  geom_polygon(aes(y = C)) +

  # geom_label(aes(label = round(change_pct, digits = 0)),
  #            label.r = unit(0.5, 'lines'),
  #            label.padding = unit(0.3, 'lines')) +
  geom_hline(yintercept = 0) +

  labs(x = NULL, y = 'ha (millions)') + ylim(-45, 45) +
  theme_minimal() +
  theme(legend.position = 'none')

net_change %>%
  filter(METRIC_CATEGORY == 'Biodiversity Support') %>%
  arrange(METRIC, scenario) %>%
  print(width = Inf)

# EXAMPLE METRICS FOR PRESENTATION--------
# ag livelihoods
a = metrics %>% filter(METRIC_CATEGORY == 'economy' & LABEL == 'Vineyard') %>%
  mutate(SCORE = case_when(METRIC == 'Gross Production Value' ~ SCORE / 1000,
                           TRUE ~ SCORE),
         METRIC = recode(METRIC,
                         'Gross Production Value' = 'Gross Production Value\n($ Millions/ha)',
                         'Annual Wages' = 'Annual Wages\n($ Thousands/FTE)',
                         'Agricultural Jobs' = 'Agricultural Jobs\n(Monthly per 1000ha)')) %>%
  ggplot(aes(SCORE, METRIC)) + geom_col(fill = pointblue.palette[4], na.rm = TRUE) +
  labs(x = '', y = NULL, title = 'Agricultural Livelihoods') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 35)) +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18, hjust = 0),
        plot.title = element_text(size = 20),
        strip.background = element_blank(),
        legend.position = 'none')
ggsave('fig/presentation_economy_vineyard.png', height = 3.5, width = 4.5, units = 'in')

# pesticides
b = metrics %>% filter(METRIC_CATEGORY == 'water quality' & LABEL == 'Vineyard') %>%
  mutate(LABEL = factor(LABEL, levels = rev(label.order)),
         METRIC = case_when(
           METRIC %in% c('Critical Pesticides', 'Groundwater Contaminant') ~
             gsub(' ', '\n', METRIC),
           TRUE ~ 'Risk To Aquatic\nOrganisms')) %>%
  select(LABEL, CODE_NAME, METRIC, SCORE) %>%
  ggplot(aes(SCORE, METRIC)) + geom_col(fill = pointblue.palette[3]) +
  # scale_fill_gradient(low = palette[5], high = palette[7]) +
  labs(x = 'Pesticide application\n(lbs/ha/yr)', y = NULL, title = 'Water Quality') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2.75)) +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18, hjust = 0),
        strip.background = element_blank(),
        plot.title = element_text(size = 20),
        legend.position = 'none')

ggsave('fig/presentation_pesticides_vineyard.png', height = 3.5, width = 4.5)


# climate change resilience
c = metrics %>% filter(METRIC_CATEGORY == 'climate' & LABEL == 'Vineyard' & METRIC != 'Overall') %>%
  # filter(!LABEL %in% c('Riparian Forest', 'Riparian Scrub', 'Wheat')) %>%
  # filter(!grepl('Riparian - ', LABEL)) %>%
  # mutate(LABEL = factor(LABEL, levels = rev(label.order))) %>%
  ggplot(aes(SCORE, METRIC)) + geom_col(fill = 'seagreen', na.rm = TRUE) +
  labs(x = 'Relative score', y = NULL, title = 'Climate change resilience') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 3.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18, hjust = 0),
        plot.title = element_text(size = 20),
        strip.background = element_blank(),
        legend.position = 'none')
ggsave('fig/presentation_climate_vineyard.png', height = 3.5, width = 4.5, units = 'in')

a + b + c + plot_layout(nrow = 2, byrow = TRUE)
ggsave('fig/presentation_metrics_overview.png', height = 8, width = 12, units = 'in')

# REGIONAL METRICS----
county_change = bind_rows(
  read_csv('output/change_riparian_county.csv') %>% mutate(METRIC = 'Riparian Landbirds'),
  read_csv('output/change_waterbird_fall_county.csv') %>% mutate(METRIC = 'Waterbirds (fall)'),
  read_csv('output/change_waterbird_win_county.csv') %>% mutate(METRIC = 'Waterbirds (winter)')
) %>%
  group_by(METRIC, zone, scenario) %>%
  summarize(change_pct = median(change_pct)) %>%
  mutate(METRIC_CATEGORY = 'Biodiversity Support',
         METRIC_SUBTYPE = 'distribution model',
         scenario = recode(scenario,
                           scenario1 = 'scenario_restoration',
                           scenario2 = 'scenario_perennial_expansion')) %>%
  bind_rows(read_csv('output/change_nonspatial_county.csv')) %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, scenario, zone, change_pct)

county_change_waterbirds = bind_rows(
  county_change %>%
    filter(grepl('Waterbirds', METRIC) & METRIC_SUBTYPE == 'distribution model'),
  county_change %>% filter(grepl('Breeding', METRIC)) %>%
    group_by(METRIC_CATEGORY, METRIC_SUBTYPE, scenario, zone) %>%
    summarize(METRIC = 'Waterbirds (breeding)',
              change_pct = mean(change_pct),
              .groups = 'drop')) %>%
  group_by(METRIC_CATEGORY, scenario, zone) %>%
  summarize(change_pct = mean(change_pct),
            .groups = 'drop') %>%
  mutate(METRIC = 'Waterbirds')

county_change_landbirds = county_change %>%
  filter(grepl('Oak|Grassland', METRIC)) %>%
  group_by(METRIC_CATEGORY, scenario, zone) %>%
  summarize(change_pct = mean(change_pct),
            .groups = 'drop') %>%
  mutate(METRIC = 'Other Landbirds')

county_change_birds = bind_rows(
  county_change %>%
    filter(METRIC_CATEGORY == 'Biodiversity Support' &
             grepl('Riparian', METRIC) &
             METRIC_SUBTYPE != 'avian conservation contribution'),
  county_change_waterbirds,
  county_change_landbirds
)

county_change_final = bind_rows(
  county_change %>% filter(METRIC_CATEGORY != 'Biodiversity Support') %>%
    filter(METRIC != 'Overall'),
  county_change_birds
) %>%
  arrange(scenario, METRIC_CATEGORY, METRIC, zone) %>%
  mutate(zone = factor(zone, levels = unique(zone)))

## lollipop---------
cl1 = county_change_final %>% filter(scenario == 'scenario_restoration') %>%
  filter(zone != 'Contra Costa & Alameda') %>%
  # filter(METRIC_CATEGORY %in% c('Agricultural Livelihoods', 'Biodiversity Support')) %>%
  group_by(METRIC_CATEGORY, scenario, zone) %>%
  summarize(change_pct = mean(change_pct),
            .groups = 'drop') %>%
  mutate(METRIC_CATEGORY = factor(METRIC_CATEGORY,
                                  levels = c('Agricultural Livelihoods',
                                             'Water Quality',
                                             'Biodiversity Support',
                                             'Climate Change Resilience')),
         # METRIC = factor(METRIC, levels = unique(county_change_final$METRIC)),
         bin = if_else(change_pct > 0, 'benefit', 'trade-off')) %>%
  ggplot(aes(change_pct, zone, fill = bin, color = bin)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(width = 0.25) +
  scale_color_manual(values = pointblue.palette[c(1,3)]) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_point(size = 10) +
  geom_text(aes(label = round(change_pct, digits = 0)),
            color = 'black', size = 5) +
  geom_vline(xintercept = 0) +
  labs(x = '% change', y = NULL,
       title = 'Scenario 1.\nHabitat Restoration') + xlim(-18, 45) +
  theme_minimal() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 20),
        strip.text = element_text(size = 18, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')

cl2 = county_change_final %>% filter(scenario == 'scenario_perennial_expansion') %>%
  filter(zone != 'Contra Costa & Alameda') %>%
  # filter(METRIC_CATEGORY %in% c('Agricultural Livelihoods', 'Biodiversity Support')) %>%
  group_by(METRIC_CATEGORY, scenario, zone) %>%
  summarize(change_pct = mean(change_pct),
            .groups = 'drop') %>%
  mutate(METRIC_CATEGORY = factor(METRIC_CATEGORY,
                                  levels = c('Agricultural Livelihoods',
                                             'Water Quality',
                                             'Biodiversity Support',
                                             'Climate Change Resilience')),
         # METRIC = factor(METRIC, levels = unique(county_change_final$METRIC)),
         bin = if_else(change_pct > 0, 'benefit', 'trade-off')) %>%
  ggplot(aes(change_pct, zone, fill = bin, color = bin)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(width = .25) +
  scale_color_manual(values = pointblue.palette[c(1,3)]) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_point(size = 10) +
  geom_text(aes(label = round(change_pct, digits = 0)),
            color = 'black', size = 5) +
  geom_vline(xintercept = 0) +
  labs(x = '% change', y = NULL,
       title = 'Scenario 2.\nPerennial Crop Expansion') + xlim(-18, 45) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 20),
        strip.text = element_text(size = 18, color = 'white'),
        strip.background = element_blank(),
        legend.position = 'none')
library(patchwork)
cl1 + cl2 + plot_layout(widths = c(1, 1))
ggsave('fig/netchange_summary_county.png', height = 8, width = 10, units = 'in')


# ## bar plot--------
# # scenario1:
# a = change_scores %>%
#   filter(scenario == 'scenario1_restoration') %>%
#   mutate(bin = if_else(change_pct > 0, 'benefit', 'trade-off')) %>%
#   ggplot(aes(change_pct, METRIC, fill = bin)) +
#   geom_col() +
#   geom_errorbar(aes(xmin = change_pct - change_pct_se,
#                     xmax = change_pct + change_pct_se), width = 0.25) +
#   geom_vline(xintercept = 0, size = 0.2) +
#   # facet_wrap(~METRIC_CATEGORY, scales = 'free') +
#   ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
#   scale_fill_manual(values = pointblue.palette[c(4,3)]) +
#   # scale_fill_manual(values = c('blue', 'orange')) +
#   # labs(x = bquote(' ' *Delta~ 'SCORE'), y = NULL) +
#   labs(x = '% change', y = NULL, title = 'Scenario 1.\nHabitat Restoration') +
#   xlim(-35, 35) +
#   theme_bw() +
#   theme(axis.text = element_text(size = 10),
#         axis.title = element_text(size = 12),
#         plot.title = element_text(size = 12),
#         strip.text = element_text(size = 10.5, hjust = 0),
#         strip.background = element_blank(),
#         legend.position = 'none')
#
# # ggsave('fig/change_scenario1_restoration_all.png', height = 8.5, width = 6)
#
# # scenario2
# b = change_scores %>%
#   filter(scenario == 'scenario2_perennialexpand') %>%
#   mutate(bin = if_else(change_pct > 0, 'benefit', 'trade-off')) %>%
#   ggplot(aes(change_pct, METRIC, fill = bin)) +
#   geom_col() +
#   geom_errorbar(aes(xmin = change_pct - change_pct_se,
#                     xmax = change_pct + change_pct_se), width = 0.25) +
#   geom_vline(xintercept = 0, size = 0.2) +
#   # facet_wrap(~METRIC_CATEGORY, scales = 'free') +
#   ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
#   scale_fill_manual(values = pointblue.palette[c(4,3)]) +
#   # scale_fill_manual(values = c('blue', 'orange')) +
#   # labs(x = bquote(' ' *Delta~ 'total area (ha, thousands)'), y = NULL) +
#   labs(x = '% change', y = NULL, title = 'Scenario 2.\nPerennial Crop Expansion') +
#   xlim(-35, 35) +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_blank(),
#         axis.title = element_text(size = 12),
#         plot.title = element_text(size = 12),
#         strip.text = element_text(size = 10.5, color = 'white'),
#         strip.background = element_blank(),
#         legend.position = 'none')
# # library(patchwork)
# a + b + plot_layout(widths = c(1, 1))
# ggsave('fig/netchange_metrics.png', height = 6, width = 6)
#
# ## lollipop----------
# l1 = change_scores %>% filter(scenario == 'scenario1_restoration') %>%
#   filter(METRIC_SUBTYPE != 'avian conservation contribution') %>%
#   mutate(METRIC_CATEGORY = factor(METRIC_CATEGORY,
#                                   levels = c('Agricultural Livelihoods',
#                                              'Water Quality',
#                                              'Biodiversity Support',
#                                              'Climate Change Resilience')),
#          METRIC = factor(METRIC, levels = change_scores$METRIC[1:13]),
#          bin = if_else(change_pct > 0, 'benefit', 'trade-off')) %>%
#   ggplot(aes(change_pct, METRIC, fill = bin, color = bin)) +
#   ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
#   geom_col(width = .25) +
#   scale_color_manual(values = pointblue.palette[c(1,3)]) +
#   scale_fill_manual(values = pointblue.palette[c(1,3)]) +
#   geom_point(size = 10) +
#   geom_text(aes(label = round(change_pct, digits = 0)),
#             color = 'black', size = 5) +
#   geom_vline(xintercept = 0) +
#   labs(x = '% change', y = NULL,
#        title = 'Scenario 1.\nHabitat Restoration') + xlim(-18, 45) +
#   theme_minimal() +
#   theme(axis.text = element_text(size = 18),
#         axis.title = element_text(size = 18),
#         plot.title = element_text(size = 20),
#         strip.text = element_text(size = 18, hjust = 0),
#         strip.background = element_blank(),
#         legend.position = 'none')
#
# l2 = change_scores %>% filter(scenario != 'scenario1_restoration') %>%
#   filter(METRIC_SUBTYPE != 'avian conservation contribution') %>%
#   mutate(METRIC_CATEGORY = factor(METRIC_CATEGORY,
#                                   levels = c('Agricultural Livelihoods',
#                                              'Water Quality',
#                                              'Biodiversity Support',
#                                              'Climate Change Resilience')),
#          METRIC = factor(METRIC, levels = change_scores$METRIC[1:13]),
#          bin = if_else(change_pct > 0, 'benefit', 'trade-off')) %>%
#   ggplot(aes(change_pct, METRIC, fill = bin, color = bin)) +
#   ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
#   geom_col(width = .25) +
#   scale_color_manual(values = pointblue.palette[c(1,3)]) +
#   scale_fill_manual(values = pointblue.palette[c(1,3)]) +
#   geom_point(size = 10) +
#   geom_text(aes(label = round(change_pct, digits = 0)),
#             color = 'black', size = 5) +
#   geom_vline(xintercept = 0) +
#   labs(x = '% change', y = NULL,
#        title = 'Scenario 2.\nPerennial Crop Expansion') + xlim(-18, 45) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(size = 18),
#         axis.text.y = element_blank(),
#         axis.title = element_text(size = 18),
#         plot.title = element_text(size = 20),
#         strip.text = element_text(size = 18, color = 'white'),
#         strip.background = element_blank(),
#         legend.position = 'none')
# l1 + l2 + plot_layout(widths = c(1, 1))
# ggsave('fig/netchange_summary.png', height = 8, width = 11, units = 'in')
#
#
# county:
#
# # scenario1:
# c = change_scores_county %>%
#   # rename(class = METRIC) %>%
#   filter(scenario == 'scenario_restoration') %>%
#   ggplot(aes(SCORE_PCT, METRIC, fill = zone)) + geom_col(position = position_dodge()) +
#   # facet_wrap(~METRIC_CATEGORY, scales = 'free') +
#   ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
#   # scale_fill_manual(values = c('blue' = 'blue', 'orange' = 'orange')) +
#   # labs(x = bquote(' ' *Delta~ 'SCORE'), y = NULL) +
#   labs(x = '% change', y = NULL, title = 'Scenario 1.\nHabitat Restoration') + xlim(-0.45, 0.45) +
#   theme_bw() +
#   theme(axis.text = element_text(size = 10),
#         axis.title = element_text(size = 12),
#         plot.title = element_text(size = 12),
#         strip.text = element_text(size = 10.5, hjust = 0),
#         strip.background = element_blank(),
#         legend.position = 'none')
#
# # scenario2
# d = change_scores_county %>%
#   # rename(class = METRIC) %>%
#   filter(scenario == 'scenario_perennial_expansion') %>%
#   ggplot(aes(SCORE_PCT, METRIC, fill = zone)) + geom_col(position = position_dodge()) +
#   # facet_wrap(~METRIC_CATEGORY, scales = 'free') +
#   ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
#   # scale_fill_manual(values = c('blue' = 'blue', 'orange' = 'orange')) +
#   # labs(x = bquote(' ' *Delta~ 'total area (ha, thousands)'), y = NULL) +
#   labs(x = '% change', y = NULL, title = 'Scenario 2.\nPerennial Crop Expansion') + xlim(-0.45, 0.45) +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_blank(),
#         axis.title = element_text(size = 12),
#         plot.title = element_text(size = 12),
#         strip.text = element_text(size = 10.5, color = 'white'),
#         strip.background = element_blank(),
#         legend.position = 'none')
# # library(patchwork)
# c + d + plot_layout(widths = c(1, 1))

# APPLY TO METRICS---------




# # TRY VISUALIZATION
# change_score %>%
#   # filter(METRIC_CATEGORY == 'biodiversity') %>%
#   ggplot(aes(change_perc, METRIC, fill = change_perc)) +
#   geom_col(color = 'black') + scale_fill_gradient2() +
#   facet_wrap(~METRIC_CATEGORY + scenario, nrow = 4, scales = 'free_y') + xlim(-1, 1.25)
# change_score %>%
#   filter(METRIC_CATEGORY == 'economy') %>%
#   ggplot(aes(change_perc, METRIC, fill = change_perc)) +
#   geom_col(color = 'black') + scale_fill_gradient2() +
#   facet_wrap(~scenario) + xlim(-1, 1.25)
# change_score %>%
#   filter(METRIC_CATEGORY == 'climate') %>%
#   ggplot(aes(change_perc, METRIC, fill = change_perc)) +
#   geom_col(color = 'black') + scale_fill_gradient2() +
#   facet_wrap(~scenario)
#
# # HIGH-LEVEL SUMMARY-------
# change_summary = bind_rows(
#   change_scores %>%
#     filter(METRIC %in% c('gross production value',
#                          'habitat for breeding waterbirds',
#                          'habitat for grassland/oak savannah landbirds',
#                          'climate change resilience', 'total pesticides',
#                          'aquatic contaminant')),
#   change_scores %>%
#     filter(METRIC_CATEGORY == 'economy' & METRIC_SUBTYPE == 'livelihoods') %>%
#     select(scenario:METRIC, base_score:scen_score) %>%
#     pivot_longer(base_score:scen_score) %>%
#     pivot_wider(names_from = METRIC, values_from = value) %>%
#     mutate(value = JOBS * WAGES) %>%
#     select(-JOBS, -WAGES) %>%
#     pivot_wider() %>%
#     mutate(METRIC = 'wages from agricultural jobs',
#            change_score = scen_score - base_score,
#            change_perc = change_score / base_score)
#   ) %>%
#   mutate(METRIC = recode(METRIC,
#                          'total pesticides' = 'pesticide application rate',
#                          'gross production value' = 'gross crop production value'))
#
# # visualize
# change_summary %>%
#   mutate(METRIC_CATEGORY = toupper(METRIC_CATEGORY)) %>%
#   unite(label, METRIC_CATEGORY, METRIC, sep = ': ') %>%
#   ggplot(aes(change_perc, label, fill = change_perc)) +
#   geom_col(color = 'black') + scale_fill_gradient2() +
#   facet_wrap(~scenario, nrow = 4) +
#   labs(x = '% change from baseline')
#
# # --> need to figure out how to show/label "disservices" (i.e. pesticide
# # application rate, aquatic contaminants) so that a decrease is obviously a good
# # thing
#
# totrip %>% arrange(scenario, desc(area.ha)) %>%
#   mutate(name = factor(name, levels = name[1:9])) %>%
#   ggplot(aes(name, area.ha/1000000, color = scenario, group = scenario)) +
#   geom_polygon(aes(fill = scenario), alpha = 0.1) +
#   coord_polar() +
#   labs(x = NULL, y = 'ha (millions)') +
#   theme_minimal()
#
# totrip %>% arrange(scenario, desc(area.ha)) %>%
#   mutate(name = factor(name, levels = name[1:9])) %>%
#   ggplot(aes(name, area.ha/1000000, color = scenario, fill = scenario)) +
#   geom_col(position = 'dodge') +
#   coord_polar() +
#   labs(x = NULL, y = 'ha (millions)') +
#   theme_minimal()
