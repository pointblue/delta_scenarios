# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')

library(patchwork)
library(showtext)
font_add_google('Source Sans Pro', 'sourcesans')


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

library(showtext)
font_add_google('Source Sans Pro', 'sourcesans')

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




# METRICS CHANGE------
# illustrate net changes in metrics across categories

# define metric category order
categorylist = c('Agricultural Livelihoods',
                 'Water Quality',
                 'Climate Change Resilience',
                 'Biodiversity Support')

# define metric order
metriclist = c(
  "Agricultural Jobs", "Annual Wages", "Gross Production Value",
  "Critical Pesticides", "Groundwater Contaminant", "Risk to Aquatic Organisms",
  "Drought", "Flood", "Heat", #"Salinity",
  'Total','Total (fall)', 'Total (winter)')

# define species order
spplist = c(
  "Nuttall's Woodpecker", 'Ash-throated Flycatcher', 'Black-headed Grosbeak',
  'Lazuli Bunting', 'Common Yellowthroat', 'Yellow Warbler', 'Spotted Towhee',
  'Song Sparrow', 'Yellow-breasted Chat',
  'Geese (fall)', 'Geese (winter)',
  'Dabbling Ducks (fall)', 'Dabbling Ducks (winter)',
  'Diving Ducks (winter)',
  'Cranes (fall)', 'Cranes (winter)',
  'Shorebirds (fall)', 'Shorebirds (winter)',
  'Herons/Egrets (fall)', 'Herons/Egrets (winter)'
)

netchange = read_csv('output/netchange_scores.csv', col_types = cols()) %>%
  filter(METRIC %in% metriclist & !grepl('distributions', METRIC_SUBTYPE)) %>%
  mutate(
    # rescale metrics for readability
    across(c(BASELINE, BASELINE_SE, SCENARIO, SCENARIO_SE, net_change, net_change_se),
           ~case_when(
             METRIC_CATEGORY == 'Water Quality' ~ ./1000, #kg to MT
             # METRIC == 'Annual Wages' ~ .*1000, #thousands to dollars
             METRIC == 'Gross Production Value' ~ ./1000/1000, #USD to thousands to millions
             TRUE ~ .)),
    METRIC_CATEGORY = factor(METRIC_CATEGORY, levels = categorylist),
    METRIC = factor(METRIC, levels = rev(metriclist)),
    # clarify metric labels
    METRIC = recode(
      METRIC,
      'Agricultural Jobs' = 'Agricultural Jobs (FTE/yr)',
      'Annual Wages' = 'Annual Wages (USD/FTE)',
      'Gross Production Value' = 'Gross Production Value (USD/yr, millions)',
      'Critical Pesticides' = 'Critical Pesticides (MT/yr)',
      'Groundwater Contaminant' = 'Groundwater Contaminants (MT/yr)',
      'Risk to Aquatic Organisms' = 'Risk to Aquatic Organisms (MT/yr)',
      Drought = 'Drought (mean score)',
      Flood = 'Flood (mean score)',
      Heat = 'Heat (mean score)',
      Total = 'Riparian landbird habitat (ha)',
      `Total (fall)` = 'Waterbird habitat, fall (ha)',
      `Total (winter)` = 'Waterbird habitat, winter (ha)'),
    # invert water quality scores
    net_change = if_else(METRIC_CATEGORY == 'Water Quality',
                         -1 * net_change, net_change),
    # define benefits and trade-offs for color coding
    bin = if_else(net_change > 0, 'benefit', 'trade-off'))

## barchart overview-----
# with error bars, for manuscript

part1 = netchange %>%
  filter(METRIC_CATEGORY == 'Agricultural Livelihoods') %>%
  ggplot(aes(net_change, METRIC)) +
  facet_wrap(~scenario, ncol = 2) +
  # ggforce::facet_col(~METRIC_CATEGORY, scales = 'free', space = 'free') +
  geom_col(aes(fill = bin)) +
  geom_errorbar(aes(xmin = net_change - net_change_se,
                    xmax = net_change + net_change_se), width = 0.5) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_vline(xintercept = 0, color = 'gray30') +
  labs(x = NULL, y = NULL, title = 'A') +
  xlim(-1100, 1100) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text = element_text(family = 'sourcesans', size = 9),
        axis.title = element_text(family = 'sourcesans', size = 10),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 10),
        strip.placement = 'outside',
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = 'none')

part2 = netchange %>%
  filter(METRIC_CATEGORY == 'Water Quality') %>%
  ggplot(aes(net_change, METRIC)) +
  facet_wrap(~scenario, ncol = 2) +
  # ggforce::facet_col(~METRIC_CATEGORY, scales = 'free', space = 'free') +
  geom_col(aes(fill = bin)) +
  geom_errorbar(aes(xmin = net_change - net_change_se,
                    xmax = net_change + net_change_se), width = 0.5) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_vline(xintercept = 0, color = 'gray30') +
  labs(x = NULL, y = NULL, title = 'B') +
  xlim(-75, 75) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text = element_text(family = 'sourcesans', size = 9),
        axis.title = element_text(family = 'sourcesans', size = 10),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 10),
        strip.placement = 'outside',
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = 'none')

part3 = netchange %>%
  filter(METRIC_CATEGORY == 'Climate Change Resilience') %>%
  ggplot(aes(net_change, METRIC)) +
  facet_wrap(~scenario, ncol = 2) +
  # ggforce::facet_col(~METRIC_CATEGORY, scales = 'free', space = 'free') +
  geom_col(aes(fill = bin)) +
  geom_errorbar(aes(xmin = net_change - net_change_se,
                    xmax = net_change + net_change_se), width = 0.5) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_vline(xintercept = 0, color = 'gray30') +
  labs(x = NULL, y = NULL, title = 'C') +
  xlim(-0.4, 0.4) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text = element_text(family = 'sourcesans', size = 9),
        axis.title = element_text(family = 'sourcesans', size = 10),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 10),
        strip.placement = 'outside',
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = 'none')

part4 = netchange %>%
  filter(METRIC_CATEGORY == 'Biodiversity Support') %>%
  mutate(scenario = factor(scenario,
                           levels = c('scenario1_restoration',
                                      'scenario2_perennialexpand'),
                           labels = c('Scenario 1', 'Scenario 2'))) %>%
  ggplot(aes(net_change, METRIC)) +
  facet_wrap(~scenario, ncol = 2, strip.position = 'bottom') +
  # ggforce::facet_col(~METRIC_CATEGORY, scales = 'free', space = 'free') +
  geom_col(aes(fill = bin)) +
  geom_errorbar(aes(xmin = net_change - net_change_se,
                    xmax = net_change + net_change_se), width = 0.5) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_vline(xintercept = 0, color = 'gray30') +
  labs(x = NULL, y = NULL, title = 'D') +
  xlim(-7000, 7000) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text = element_text(family = 'sourcesans', size = 9),
        axis.title = element_text(family = 'sourcesans', size = 10),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 10),
        strip.placement = 'outside',
        strip.text = element_text(family = 'sourcesans', size = 10),
        # strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = 'none')

showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
part1/part2/part3/part4
ggsave('fig/netchange_barchart_all.png', height = 5, width = 5.5, units = 'in')
showtext_auto(FALSE)

## lollipop chart overview---------
# simple version for presentations with larger fonts and without error bars

part1 = netchange %>%
  filter(METRIC_CATEGORY == 'Agricultural Livelihoods') %>%
  ggplot(aes(net_change, METRIC, fill = bin, color = bin)) +
  facet_wrap(~scenario, ncol = 2) +
  geom_vline(xintercept = 0, color = 'gray30') +
  geom_col(width = 0.25) +
  geom_point(size = 10) +
  geom_text(aes(label = round(net_change, digits = 0)),
            color = 'black', size = 4) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  scale_color_manual(values = pointblue.palette[c(1,3)]) +
  labs(x = NULL, y = NULL, title = 'Agricultural Livelihoods') +
  xlim(-600, 600) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text = element_text(family = 'sourcesans', size = 16),
        axis.title = element_text(family = 'sourcesans', size = 16),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 18),
        strip.placement = 'outside',
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = 'none')

part2 = netchange %>%
  filter(METRIC_CATEGORY == 'Water Quality') %>%
  ggplot(aes(net_change, METRIC, fill = bin, color = bin)) +
  facet_wrap(~scenario, ncol = 2) +
  geom_vline(xintercept = 0, color = 'gray30') +
  geom_col(width = 0.25) +
  geom_point(size = 10) +
  geom_text(aes(label = round(net_change, digits = 0)),
            color = 'black', size = 4) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  scale_color_manual(values = pointblue.palette[c(1,3)]) +
  labs(x = NULL, y = NULL, title = 'Water Quality') +
  xlim(-75, 75) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text = element_text(family = 'sourcesans', size = 16),
        axis.title = element_text(family = 'sourcesans', size = 16),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 18),
        strip.placement = 'outside',
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = 'none')

part3 = netchange %>%
  filter(METRIC_CATEGORY == 'Climate Change Resilience') %>%
  ggplot(aes(net_change, METRIC, fill = bin, color = bin)) +
  facet_wrap(~scenario, ncol = 2) +
  geom_vline(xintercept = 0, color = 'gray30') +
  geom_col(width = 0.25) +
  geom_point(size = 10) +
  geom_text(aes(label = round(net_change, digits = 2)),
            color = 'black', size = 4) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  scale_color_manual(values = pointblue.palette[c(1,3)]) +
  labs(x = NULL, y = NULL, title = 'Climate Change Resilience') +
  xlim(-0.3, 0.3) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text = element_text(family = 'sourcesans', size = 16),
        axis.title = element_text(family = 'sourcesans', size = 16),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 18),
        strip.placement = 'outside',
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = 'none')

part4 = netchange %>%
  filter(METRIC_CATEGORY == 'Biodiversity Support') %>%
  mutate(scenario = factor(scenario,
                           levels = c('scenario1_restoration',
                                      'scenario2_perennialexpand'),
                           labels = c('Scenario 1', 'Scenario 2'))) %>%
  ggplot(aes(net_change, METRIC, fill = bin, color = bin)) +
  facet_wrap(~scenario, ncol = 2, strip.position = 'bottom') +
  geom_vline(xintercept = 0, color = 'gray30') +
  geom_col(width = 0.25) +
  geom_point(size = 11) +
  geom_text(aes(label = round(net_change, digits = 0)),
            color = 'black', size = 4) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  scale_color_manual(values = pointblue.palette[c(1,3)]) +
  labs(x = NULL, y = NULL, title = 'Biodiversity Support') +
  xlim(-7000, 7000) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text = element_text(family = 'sourcesans', size = 16),
        axis.title = element_text(family = 'sourcesans', size = 16),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 18),
        strip.placement = 'outside',
        strip.text = element_text(family = 'sourcesans', size = 18),
        strip.background = element_blank(),
        legend.position = 'none')


showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
part1/part2/part3/part4
ggsave('fig/netchange_lollipop_all.png', height = 7.5, width = 11, units = 'in')
showtext_auto(FALSE)

## barchart - scenario 1---------
# doesn't work well without rescaling all metrics to be closer to a common scale

netchange %>%
  filter(scenario == 'scenario1_restoration') %>%
  ggplot(aes(net_change, METRIC)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free', space = 'free') +
  geom_col(aes(fill = bin)) +
  geom_errorbar(aes(xmin = net_change - net_change_se,
                    xmax = net_change + net_change_se), width = 0.5) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_vline(xintercept = 0, color = 'gray30') +
  labs(x = NULL, y = NULL, title = 'A') +
  # xlim(-100, 200) +
  theme_minimal() +
  theme(axis.line.x = element_line(color = 'gray30'),
        axis.text = element_text(family = 'sourcesans', size = 9),
        axis.title = element_text(family = 'sourcesans', size = 10),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 10),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')

## barchart - scenario 2---------
netchange %>%
  filter(scenario != 'scenario1_restoration') %>%
  ggplot(aes(net_change, METRIC)) +
  ggforce::facet_col(~METRIC_CATEGORY, scales = 'free_y', space = 'free') +
  geom_col(aes(fill = bin)) +
  geom_errorbar(aes(xmin = net_change - net_change_se,
                    xmax = net_change + net_change_se), width = 0.5) +
  scale_fill_manual(values = pointblue.palette[c(1,3)]) +
  geom_vline(xintercept = 0, color = 'gray30') +
  labs(x = NULL, y = NULL, title = 'B') +
  # xlim(-100, 200) +
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
