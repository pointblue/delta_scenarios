# ANALYZE NET CHANGE IN MULTIPLE METRICS---------
# Calculate the total "landscape score" for each metric and landscape, and then
# the net change between each scenario and the baseline

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')

# REFERENCE DATA
key = readxl::read_excel('GIS/VEG_key.xlsx')
metrics = read_csv('output/metrics.csv')
habitat = read_csv('output/scenario_habitat.csv')
habitat_binary = read_csv('output/scenario_habitat_binary.csv')


# METRICS CHANGE--------

## total landscape scores-----------
# combine the landscape-specific estimates of the total area of each land cover
# class with the per-unit-area metrics for each land cover class
# - for most metrics, this is the sum over all hectares
# - for Annual Wages, calculate the new weighted average wage across all
#    agricultural ha (i.e., those with a wage value)
# - for climate change resilience, calculate the new overall average

# exclude riparian and managed wetland subclasses (to not double-count), and
# exclude tidal wetlands and water since not addressing them in these scenarios
# (and they shouldn't change)
# --> exclude winter scenarios/landscapes
# --> combine with habitat estimates from above

# check units:
metrics %>% select(METRIC_CATEGORY, METRIC, UNIT) %>%
  distinct() %>% print(width = Inf)

# land cover totals
landcover = read_csv('output/landcover_totals.csv')

scores = DeltaMultipleBenefits::sum_metrics(
  metricdat = metrics %>%
    filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))),
  areadat = landcover %>%
    filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))) %>%
    filter(!grepl('win', scenario))) %>%
  bind_rows(habitat, habitat_binary) %>%
  mutate(scenario = gsub('_win', '', scenario)) %>% #rename in habitat metrics
  arrange(scenario, METRIC_CATEGORY, METRIC) %>%
  mutate(UNIT = gsub('/ha', '', UNIT)) # now scores are not per ha
write_csv(scores, 'output/scenario_scores.csv')

# check updated units:
scores %>% select(METRIC_CATEGORY, METRIC_SUBTYPE, UNIT) %>% distinct()

## table--------
scores_table = scores %>% filter(is.na(METRIC_SUBTYPE) | grepl('habitat', METRIC_SUBTYPE)) %>%
  filter(METRIC_CATEGORY != 'Biodiversity Support' | grepl('Total', METRIC)) %>%
  mutate(across(c(SCORE_TOTAL, SCORE_TOTAL_SE),
                ~case_when(METRIC == 'Gross Production Value' ~ ./1000,
                           METRIC_CATEGORY == 'Water Quality' ~ ./1000,
                           TRUE ~ .)),
         UNIT = case_when(METRIC == 'Gross Production Value' ~ 'USD/yr (thousands)',
                          METRIC_CATEGORY == 'Water Quality' ~ 'MT/yr',
                          TRUE ~ UNIT),
         across(c(SCORE_TOTAL, SCORE_TOTAL_SE),
                ~case_when(METRIC_CATEGORY %in%
                             c('Water Quality', 'Climate Change Resilience') ~
                             round(., digits = 2) %>% format(nsmall = 2),
                           TRUE ~ round(., digits = 0) %>% format(nsmall = 0)))) %>%
  select(scenario, METRIC_CATEGORY, METRIC, UNIT, SCORE_TOTAL, SCORE_TOTAL_SE) %>%
  pivot_wider(names_from = scenario,
              values_from = c(SCORE_TOTAL, SCORE_TOTAL_SE)) %>%
  select(METRIC_CATEGORY, METRIC, UNIT, ends_with('baseline'),
         ends_with('restoration'), ends_with('expand'), ends_with('combo'))
write_csv(scores_table, 'output/TABLE_scores_summary.csv')

## net change-------
# compare each scenario to baseline
scores_change = DeltaMultipleBenefits::sum_change(scores)
write_csv(scores_change, 'output/netchange_scores.csv')

# PLOT CHANGE----------
library(patchwork)
library(showtext)
font_add_google('Source Sans Pro', 'sourcesans')

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
      'Agricultural Jobs' = 'Agricultural Jobs\n(FTE/yr)',
      'Annual Wages' = 'Annual Wages\n(USD/FTE)',
      'Gross Production Value' = 'Gross Production Value\n(USD/yr, millions)',
      'Critical Pesticides' = 'Critical Pesticides\n(MT/yr)',
      'Groundwater Contaminant' = 'Groundwater Contaminants\n(MT/yr)',
      'Risk to Aquatic Organisms' = 'Risk to Aquatic Organisms\n(MT/yr)',
      Drought = 'Drought\n(mean score)',
      Flood = 'Flood\n(mean score)',
      Heat = 'Heat\n(mean score)',
      Total = 'Riparian landbird\nhabitat (ha)',
      `Total (fall)` = 'Waterbird habitat,\nfall (ha)',
      `Total (winter)` = 'Waterbird habitat,\nwinter (ha)'),
    # invert water quality scores
    net_change = if_else(METRIC_CATEGORY == 'Water Quality',
                         -1 * net_change, net_change),
    # define benefits and trade-offs for color coding
    bin = if_else(net_change > 0, 'benefit', 'trade-off')) %>%
  mutate(scenario = factor(scenario,
                           levels = c('scenario1_restoration',
                                      'scenario2_perennialexpand',
                                      'scenario3_combo'),
                           labels = c('Scenario 1:\nHabitat restoration',
                                      'Scenario 2:\nPerennial crop\nexpansion',
                                      'Scenario 3:\nCombination')))

## for manuscript-----
# barchart with error bars

plot_change = function(dat) {
  ggplot(dat, aes(net_change, METRIC)) +
    facet_wrap(~scenario, ncol = 3) +
    # ggforce::facet_col(~METRIC_CATEGORY, scales = 'free', space = 'free') +
    geom_col(aes(fill = bin)) +
    geom_errorbar(aes(xmin = net_change - net_change_se,
                      xmax = net_change + net_change_se), width = 0.25) +
    scale_fill_manual(values = pointblue.palette[c(1,3)]) +
    geom_vline(xintercept = 0, color = 'gray30') +
    theme_minimal() +
    theme(axis.line.x = element_line(color = 'gray30'),
          axis.text = element_text(family = 'sourcesans', size = 8.5),
          axis.title = element_text(family = 'sourcesans', size = 10),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(family = 'sourcesans', size = 10),
          strip.placement = 'outside',
          # strip.text = element_text(family = 'sourcesans', size = 10, vjust = 1),
          strip.text = element_blank(),
          strip.background = element_blank(),
          legend.position = 'none',
          panel.spacing = unit(1, 'lines'))
}


part1 = netchange %>%
  filter(METRIC_CATEGORY == 'Agricultural Livelihoods') %>%
  plot_change() +
  labs(x = NULL, y = NULL, title = 'A') +
  xlim(-1100, 1100)

part2 = netchange %>%
  filter(METRIC_CATEGORY == 'Water Quality') %>%
  plot_change() +
  labs(x = NULL, y = NULL, title = 'B') +
  xlim(-75, 75)

part3 = netchange %>%
  filter(METRIC_CATEGORY == 'Climate Change Resilience') %>%
  plot_change() +
  labs(x = NULL, y = NULL, title = 'C') +
  xlim(-0.4, 0.4)

part4 = netchange %>%
  filter(METRIC_CATEGORY == 'Biodiversity Support') %>%
  plot_change() +
  labs(x = NULL, y = NULL, title = 'D') +
  xlim(-7000, 7000) +
  facet_wrap(~scenario, ncol = 3, strip.position = 'bottom') +
  theme(strip.text = element_text(family = 'sourcesans', size = 10, vjust = 1))

showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
part1/part2/part3/part4
ggsave('fig/netchange_barchart_all.png', height = 6.5, width = 6.5, units = 'in')
showtext_auto(FALSE)

## lollipop chart overview---------
# simple version for presentations with larger fonts and without error bars

plot_lollipop = function(dat) {
  dat %>% mutate(METRIC = gsub('\n', ' ', METRIC)) %>%
  ggplot(aes(net_change, METRIC, fill = bin, color = bin)) +
    facet_wrap(~scenario, ncol = 3) +
    geom_vline(xintercept = 0, color = 'gray30') +
    geom_col(width = 0.25) +
    geom_point(size = 10) +
    geom_text(aes(label = round(net_change, digits = 0)),
              color = 'black', size = 4) +
    scale_fill_manual(values = pointblue.palette[c(1,3)]) +
    scale_color_manual(values = pointblue.palette[c(1,3)]) +
    theme_minimal() +
    theme(axis.line.x = element_line(color = 'gray30'),
          axis.text = element_text(family = 'sourcesans', size = 14, lineheight = 0.8),
          axis.title = element_text(family = 'sourcesans', size = 16),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(family = 'sourcesans', size = 16),
          strip.placement = 'outside',
          strip.text = element_blank(),
          strip.background = element_blank(),
          legend.position = 'none',
          panel.spacing = unit(1, 'lines'))
}

part1 = netchange %>%
  filter(METRIC_CATEGORY == 'Agricultural Livelihoods') %>%
  plot_lollipop() +
  labs(x = NULL, y = NULL, title = 'Agricultural Livelihoods') +
  xlim(-600, 600)

part2 = netchange %>%
  filter(METRIC_CATEGORY == 'Water Quality') %>%
  plot_lollipop() +
  labs(x = NULL, y = NULL, title = 'Water Quality') +
  xlim(-75, 75)

part3 = netchange %>%
  filter(METRIC_CATEGORY == 'Climate Change Resilience') %>%
  plot_lollipop() +
  labs(x = NULL, y = NULL, title = 'Climate Change Resilience') +
  xlim(-0.3, 0.3)

part4 = netchange %>%
  filter(METRIC_CATEGORY == 'Biodiversity Support') %>%
  plot_lollipop() +
  facet_wrap(~scenario, ncol = 3, strip.position = 'bottom') +
  labs(x = NULL, y = NULL, title = 'Biodiversity Support') +
  xlim(-7000, 7000) +
  theme(strip.text = element_text(family = 'sourcesans', size = 16, vjust = 1))


showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
part1/part2/part3/part4
ggsave('fig/netchange_lollipop_all3.png', height = 7.5, width = 11, units = 'in')
showtext_auto(FALSE)


# COUNTY-SPECIFIC ESTIMATES---------
#
## habitat-------
# habitat_county = DeltaMultipleBenefits::sum_habitat(
#   pathin = 'GIS/prediction_rasters',
#   zonepath = 'GIS/landscape_rasters/boundaries/counties.tif',
#   subtype = 'distributions',
#   rollup = TRUE,
#   keypath = 'output/TABLE_species_key.csv')
# write_csv(habitat_county, 'output/scenario_habitat_county.csv')
#
# habitat_binary_county = DeltaMultipleBenefits::sum_habitat(
#   pathin = 'GIS/prediction_rasters_threshold',
#   zonepath = 'GIS/landscape_rasters/boundaries/counties.tif',
#   subtype = 'habitat',
#   rollup = TRUE,
#   keypath = 'output/TABLE_species_key.csv')
# write_csv(habitat_binary_county, 'output/scenario_habitat_binary_county.csv')
#
## land cover totals-------
# landcover_county = DeltaMultipleBenefits::sum_landcover(
#   pathin = 'GIS/scenario_rasters',
#   zonepath = 'GIS/landscape_rasters/boundaries/counties.tif',
#   maskpath = 'GIS/boundaries/delta.tif',
#   pixel_area = 0.09,
#   rollup = TRUE) %>%
#   # add LABEL fields
#   left_join(key %>% select(CODE_NAME, LABEL), by = 'CODE_NAME') %>%
#   select(scenario, ZONE, CODE_NAME, LABEL, area) %>%
#   arrange(scenario, ZONE, CODE_NAME)
# write_csv(landcover_county, 'output/landcover_totals_county.csv')
#
## scores------
# scores_county = DeltaMultipleBenefits::sum_metrics(
#   metricdat = metrics %>%
#     filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))),
#   areadat = landcover_county %>%
#     filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))) %>%
#     filter(!grepl('win', scenario))) %>%
#   bind_rows(habitat_county, habitat_binary_county) %>%
#   mutate(scenario = gsub('_win', '', scenario)) %>% #rename in habitat metrics
#   arrange(scenario, ZONE, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC)
# # check updated units:
# scores_county %>% select(METRIC_CATEGORY, METRIC_SUBTYPE, UNIT) %>% distinct()
#
# write_csv(scores_county, 'output/scenario_scores_county.csv')
#
## net change--------
# landcover_change_county = landcover_county %>% filter(!grepl('win', scenario)) %>%
#   rename(SCORE_TOTAL = area) %>%
#   mutate(SCORE_TOTAL_SE = 0) %>%
#   DeltaMultipleBenefits::sum_change(scoredat = .) %>%
#   select(-ends_with('SE', ignore.case = TRUE))
# write_csv(landcover_change_county, 'output/netchange_landcover_county.csv')
#
# scores_change_county = DeltaMultipleBenefits::sum_change(scores_county)
# write_csv(scores_change_county, 'output/netchange_scores_county.csv')
