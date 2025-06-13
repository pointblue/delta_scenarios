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
habitat_binary_ci = read_csv('output/scenario_habitat_binary_ci.csv')
bootstrap = read_csv('output/scenario_habitat_binary_bootstrap.csv')
landcover = read_csv('output/landcover_totals.csv') # land cover totals

# METRICS CHANGE--------

## landscape diversity----------

# exclude overall RIPARIAN and WETLAND classes, since subclasses also present
# (to not double-count)
divmat = landcover |>
  filter(!CODE_NAME %in% c('RIPARIAN', 'WETLAND_MANAGED')) |>
  filter(!grepl('win', scenario)) |>
  select(-LABEL) |>
  #mutate(area = area/1000) |> #1000s of ha
  pivot_wider(names_from = CODE_NAME, values_from = area) |>
  tibble::column_to_rownames(var = 'scenario')

divdf = landcover |> select(scenario) |> filter(!grepl('win', scenario)) |> distinct() |>
  mutate(METRIC_CATEGORY = 'Climate Change Resilience',
         METRIC = 'Landscape diversity',
         UNIT = 'Shannon index',
         SCORE_TOTAL = vegan::diversity(divmat, index = 'shannon'))

ecolTest::Hutcheson_t_test(divmat['scenario1_restoration',], divmat['baseline',]) # p = 0.03
ecolTest::Hutcheson_t_test(divmat['scenario2_perennialexpand_alt',], divmat['baseline',]) #p < 0.001
ecolTest::Hutcheson_t_test(divmat['scenario3_combo_alt',], divmat['baseline',]) #p < 0.001


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

scores = DeltaMultipleBenefits::sum_metrics(
  metricdat = metrics %>%
    filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))),
  areadat = landcover %>%
    filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))) %>%
    filter(!grepl('win', scenario)))  %>%
  mutate(SCORE_TOTAL_SE = if_else(METRIC == 'N loading', NA_real_, SCORE_TOTAL_SE)) |>
  #mutate(UNIT = gsub('/ha', '', UNIT)) %>% # now scores are not per ha
  bind_rows(habitat_binary_ci %>%
              select(scenario, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, UNIT,
                     SCORE_TOTAL, SCORE_TOTAL_SE = se)) %>%
  mutate(scenario = gsub('_win', '', scenario)) %>% #rename in habitat metrics
  arrange(scenario, METRIC_CATEGORY, METRIC) |>
  # add diversity index
  bind_rows(divdf)
write_csv(scores, 'output/scenario_scores.csv')

# check updated units:
scores %>% select(METRIC_CATEGORY, METRIC_SUBTYPE, UNIT) %>% distinct()

## table--------
scores_table = scores %>% filter(is.na(METRIC_SUBTYPE) | grepl('habitat', METRIC_SUBTYPE)) %>%
  filter(METRIC_CATEGORY != 'Biodiversity Support' | grepl('Total', METRIC)) %>%
  mutate(across(c(SCORE_TOTAL, SCORE_TOTAL_SE),
                ~case_when(METRIC == 'Gross Production Value' ~ ./1000,
                           METRIC == 'N loading' ~ ./100000,
                           METRIC_CATEGORY == 'Water Quality' ~ ./1000,
                           TRUE ~ .)),
         UNIT = case_when(METRIC == 'Gross Production Value' ~ 'USD/yr (thousands)',
                          METRIC == 'N loading' ~ '100 MT/yr',
                          METRIC_CATEGORY == 'Water Quality' ~ 'MT/yr',
                          TRUE ~ UNIT),
         across(c(SCORE_TOTAL, SCORE_TOTAL_SE),
                ~case_when(METRIC_CATEGORY %in%
                             c('Water Quality', 'Climate Change Resilience') ~
                             round(., digits = 2) %>% format(nsmall = 2),
                           TRUE ~ round(., digits = 0) %>% format(nsmall = 0)))) %>%
  select(scenario, METRIC_CATEGORY, METRIC, UNIT, SCORE_TOTAL, SCORE_TOTAL_SE)

# define metric category order
categorylist = c('Agricultural Livelihoods',
                 'Water Quality',
                 'Climate Change Resilience',
                 'Biodiversity Support')

scores_table_wide = scores_table %>%
  filter(grepl('baseline|scenario1|_alt', scenario)) %>%
  pivot_wider(names_from = scenario,
              values_from = c(SCORE_TOTAL, SCORE_TOTAL_SE)) %>%
  select(METRIC_CATEGORY, METRIC, UNIT, ends_with('baseline'),
         ends_with('restoration'), ends_with('expand_alt'), ends_with('combo_alt')) %>%
  mutate(METRIC_CATEGORY = factor(METRIC_CATEGORY, levels = categorylist)) %>%
  arrange(METRIC_CATEGORY)
write_csv(scores_table_wide, 'output/TABLE_scores_summary.csv')

## net change-------
# compare each scenario to baseline
scores_change = scores |>
  # transform shannon indices
  mutate(SCORE_TOTAL = if_else(METRIC == 'Landscape diversity', exp(SCORE_TOTAL), SCORE_TOTAL)) |>
  DeltaMultipleBenefits::sum_change() %>%
  mutate(prop_change = net_change / BASELINE_VALUE)
# --> but use bootstrap CI for biodiversity support instead

scores_change_biodiversity = left_join(
  bootstrap %>% mutate(scenario = gsub('_win', '', scenario)) %>%
    filter(!grepl('baseline', scenario)) %>%
    select(scenario:n, SCENARIO_VALUE = value),
  bootstrap %>% mutate(scenario = gsub('_win', '', scenario)) %>%
    filter(grepl('baseline', scenario)) %>%
    select(spp:n, BASELINE_VALUE = value),
  by = c('spp', 'METRIC_SUBTYPE', 'METRIC', 'n')) %>%
  mutate(METRIC_CATEGORY = 'Biodiversity Support',
         diff = (SCENARIO_VALUE - BASELINE_VALUE) * 0.09) %>%  #convert to ha
  group_by(scenario, spp, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC) %>%
  summarize(lcl = quantile(diff, 0.025),
            ucl = quantile(diff, 0.975),
            .groups = 'drop') %>%
  # add original net_change estimate
  left_join(scores_change %>% select(scenario:METRIC, net_change, prop_change),
            by = c('scenario', 'METRIC_CATEGORY', 'METRIC'))

# combine:
scores_change_all = bind_rows(
  scores_change %>%
    filter(METRIC_CATEGORY != 'Biodiversity Support') %>%
    select(scenario, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, UNIT, net_change,
           lcl, ucl, prop_change),
  scores_change_biodiversity %>%
    select(-spp))
write_csv(scores_change_all, 'output/netchange_scores.csv')

# scores_change_toshare = scores_change_all %>%
#   select(scenario:net_change, lower = lcl, upper = ucl, prop_change) %>%
#   mutate(UNIT = if_else(METRIC_CATEGORY == 'Biodiversity Support', 'ha', UNIT))

# PLOT CHANGE----------
library(patchwork)
library(showtext)
library(ggtext)
font_add_google('Source Sans Pro', 'sourcesans')

# define metric category order
categorylist = c('Agricultural Livelihoods',
                 'Water Quality',
                 'Climate Change Resilience',
                 'Biodiversity Support')

# define metric order
metriclist = c(
  "Agricultural Jobs", "Annual Wages", "Gross Production Value",
  "Critical Pesticides", "Groundwater Contaminant", "Risk to Aquatic Organisms", 'N loading',
  "Landscape diversity", "Drought", "Flood", "Heat",  #"Salinity",
  'Total','Total (fall)', 'Total (winter)')



netchange = read_csv('output/netchange_scores.csv', col_types = cols()) %>%
  # filter(METRIC %in% metriclist & !grepl('distributions', METRIC_SUBTYPE)) %>%
  filter(grepl('scenario1|_alt', scenario)) %>%
  mutate(
    # rescale metrics for readability
    across(c(net_change, lcl, ucl),
           ~case_when(
             METRIC == 'N loading' ~ ./100000,
             METRIC_CATEGORY == 'Water Quality' ~ ./1000, #kg to MT per yr
             METRIC == 'Annual Wages' ~ ./100, #USD to hundreds per FTE
             METRIC == 'Gross Production Value' ~ ./1000000, #USD to hundred-thousands per yr
             METRIC_CATEGORY == 'Biodiversity Support' ~ ./1000, #ha to thousands of ha
             TRUE ~ .)),
    METRIC_CATEGORY = factor(METRIC_CATEGORY, levels = categorylist),
    METRIC = factor(METRIC, levels = rev(metriclist)),
    # clarify metric labels
    METRIC = recode(
      METRIC,
      'Agricultural Jobs' = 'Agricultural Jobs\n(FTE per yr)',
      'Annual Wages' = 'Annual Wages\n(USD per FTE, hundreds)',
      'Gross Production Value' = 'Gross Production Value\n(USD per yr, millions)',
      'Critical Pesticides' = 'Critical Pesticides<br>(MT yr<sup>-1</sup>)',
      'Groundwater Contaminant' = 'Groundwater Contaminants<br>(MT yr<sup>-1</sup>)',
      'Risk to Aquatic Organisms' = 'Risk to Aquatic Organisms<br>(MT yr<sup>-1</sup>)',
      'N loading' = 'Nitrogen loading<br>(MT yr<sup>-1</sup>, hundreds)',
      Drought = 'Drought\n(mean score)',
      Flood = 'Flood\n(mean score)',
      Heat = 'Heat\n(mean score)',
      'Landscape diversity' = 'Landscape diversity\n(Shannon index)',
      Total = 'Riparian landbird habitat\n(ha, thousands)',
      `Total (fall)` = 'Waterbird habitat, fall\n(ha, thousands)',
      `Total (winter)` = 'Waterbird habitat, winter\n(ha, thousands)'),
    # invert water quality scores
    across(c(net_change, lcl, ucl, prop_change),
           ~if_else(METRIC_CATEGORY == 'Water Quality', -1 * ., .)),
    # define benefits and trade-offs for color coding
    bin = if_else(net_change > 0, 'benefit', 'trade-off'),
    sig = case_when(grepl('diversity', METRIC) ~ TRUE,
                    bin == 'trade-off' & ucl < 0 & lcl < 0 ~ TRUE,
                    bin == 'benefit' & lcl > 0 & ucl > 0 ~ TRUE,
                    TRUE ~ FALSE)) %>%
  mutate(scenario = factor(scenario,
                           levels = c('scenario1_restoration',
                                      'scenario2_perennialexpand_alt',
                                      'scenario3_combo_alt'),
                           labels = c('A',
                                      'B',
                                      'C')))
  # mutate(scenario = factor(scenario,
  #                          levels = c('scenario1_restoration',
  #                                     'scenario2_perennialexpand_alt',
  #                                     'scenario3_combo_alt'),
  #                          labels = c('Scenario 1:\nHabitat restoration',
  #                                     'Scenario 2:\nPerennial crop\nexpansion',
  #                                     'Scenario 3:\nCombination')))

## species results
# define species order
spplist = c(
  "Nuttall's Woodpecker", 'Ash-throated Flycatcher', 'Black-headed Grosbeak',
  'Lazuli Bunting', 'Common Yellowthroat', 'Yellow Warbler', 'Spotted Towhee',
  'Song Sparrow', 'Yellow-breasted Chat',
  'Geese', 'Dabbling Ducks', 'Diving Ducks', 'Cranes', 'Shorebirds',
  'Herons/Egrets'
)
netchange_spp = read_csv('output/netchange_scores.csv', col_types = cols()) %>%
  # filter(METRIC %in% metriclist & !grepl('distributions', METRIC_SUBTYPE)) %>%
  filter(grepl('scenario1|_alt', scenario) &
           METRIC_CATEGORY == 'Biodiversity Support' &
           !grepl('Total', METRIC)) %>%
  mutate(
    METRIC_SUBTYPE = case_when(
      grepl('fall', METRIC) ~ 'FALL',
      grepl('winter', METRIC) ~ 'WINTER',
      TRUE ~ 'RIPARIAN'),
    METRIC = gsub(' \\(fall\\)| \\(winter\\)', '', METRIC),
    METRIC = factor(METRIC, levels = rev(spplist)),
    # rescale metrics for readability
    across(c(net_change, lcl, ucl), ~ ./1000),
    bin = if_else(net_change > 0, 'benefit', 'trade-off'),
    sig = case_when(bin == 'trade-off' & ucl < 0 & lcl < 0 ~ TRUE,
                    bin == 'benefit' & lcl > 0 & ucl > 0 ~ TRUE,
                    TRUE ~ FALSE),
    scenario = factor(scenario,
                      levels = c('scenario1_restoration',
                                 'scenario2_perennialexpand_alt',
                                 'scenario3_combo_alt'),
                      labels = c('A',
                                 'B',
                                 'C'))) %>%
  arrange(scenario, METRIC_SUBTYPE, METRIC)



## for manuscript-----
# barchart with error bars
part1 = netchange %>%
  filter(METRIC_CATEGORY == 'Agricultural Livelihoods') %>%
  plot_change_bar(plus = 0.05, nudge_plus = 120, nudge_star = 40, star = TRUE) +
  labs(x = NULL, y = NULL, title = 'Agricultural Livelihoods') +
  facet_wrap(~scenario, ncol = 3, strip.position = 'top') +
  theme(strip.text = element_text(family = 'sourcesans', size = 9, vjust = 0, hjust = 0, face = 'bold'),
        strip.placement = 'outside')
  # xlim(-1000, 1000)

part2 = netchange %>%
  filter(METRIC_CATEGORY == 'Water Quality') %>%
  plot_change_bar(plus = 0.05, nudge_plus = 10, nudge_star = 5, star = TRUE) +
  labs(x = NULL, y = NULL, title = 'Water Quality') +
  theme(axis.text.y = ggtext::element_markdown())
  # xlim(-100, 100)

part3 = netchange %>%
  filter(METRIC_CATEGORY == 'Climate Change Resilience') %>%
  plot_change_bar(plus = 0.05, nudge_plus = 0.06, nudge_star = 0.03, star = TRUE) +
  labs(x = NULL, y = NULL, title = 'Climate Change Resilience') +
  xlim(-1, 1)

part4 = netchange %>%
  filter(METRIC_CATEGORY == 'Biodiversity Support' & !is.na(METRIC)) %>%
  plot_change_bar(plus = 0.05, nudge_plus = 3, nudge_star = 1, star = TRUE) +
  labs(x = NULL, y = NULL, title = 'Biodiversity Support')
  # xlim(-25, 25) +
  # facet_wrap(~scenario, ncol = 3, strip.position = 'bottom') +
  # theme(strip.text = element_text(family = 'sourcesans', size = 10, vjust = 1))

showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
part1/part2/part3/part4 + plot_layout(heights = c(3, 4, 4, 3))
ggsave('fig/netchange_barchart_perc.png', height = 6.5, width = 6.5, units = 'in')
showtext_auto(FALSE)

rip = netchange_spp %>% filter(METRIC_SUBTYPE == 'RIPARIAN') %>%
  plot_change_bar(plus = 0.05, nudge_plus = 3, star = TRUE, nudge_star = 1) +
  labs(x = NULL, y = NULL, title = 'Riparian landbirds') +
  facet_wrap(~scenario, ncol = 3, strip.position = 'top') +
  theme(strip.text = element_text(family = 'sourcesans', size = 9, vjust = 0, hjust = 0, face = 'bold'),
        strip.placement = 'outside')
fall = netchange_spp %>% filter(METRIC_SUBTYPE == 'FALL') %>%
  # mutate(METRIC = gsub(' (fall)', '', METRIC)) %>%
  plot_change_bar(plus = 0.05, nudge_plus = 0.75, star = TRUE, nudge_star = 0.25) +
  labs(x = NULL, y = NULL, title = 'Waterbird groups (fall)')
win = netchange_spp %>% filter(METRIC_SUBTYPE == 'WINTER') %>%
  # mutate(METRIC = gsub(' (winter)', '', METRIC)) %>%
  plot_change_bar(plus = 0.05, nudge_plus = 0.75, star = TRUE, nudge_star = 0.25) +
  labs(x = NULL, y = NULL, title = 'Waterbird groups (winter)')

showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
rip/fall/win + plot_layout(heights = c(.9, .5, .6))
ggsave('fig/netchange_barchart_spp_perc.png', height = 6.5, width = 6.5, units = 'in')
showtext_auto(FALSE)


# ## for presentation
# # results by scenario with species details
#
# netchange2 = read_csv('output/netchange_scores.csv', col_types = cols()) %>%
#   filter(!METRIC %in% metriclist & !grepl('distributions', METRIC_SUBTYPE)) %>%
#   filter(grepl('scenario1|_alt', scenario)) %>%
#   # define benefits and trade-offs for color coding
#   mutate(bin = if_else(net_change > 0, 'benefit', 'trade-off'),
#          METRIC = factor(METRIC, levels = rev(spplist)))
#
# part1a = netchange %>%
#   filter(grepl(1, scenario)) %>%
#   plot_change_bar() +
#   facet_wrap(~METRIC_CATEGORY, ncol = 1, drop = TRUE, scales = 'free_y') +
#   labs(x = NULL, y = NULL) +
#   xlim(-2100, 2100) +
#   theme(strip.text = element_text(family = 'sourcesans', size = 14, hjust = 0, face = 'bold'))
# part1b = netchange2 %>%
#   filter(grepl(1, scenario)) %>%
#   plot_change_bar() +
#   facet_wrap(~METRIC_SUBTYPE, ncol = 1, drop = TRUE, scales = 'free_y') +
#   labs(x = NULL, y = NULL) +
#   xlim(-500, 1500) +
#   theme(strip.text = element_text(family = 'sourcesans', size = 14, hjust = 0, face = 'bold'))
# part1a|part1b
# ggsave('fig/presentations/netchange_barchart_scenario1.png', height = 5, width = 10, units = 'in')

## for presentation---------

### lollipop chart------
# simple version for presentations with larger fonts and without error bars

part1 = netchange %>%
  filter(METRIC_CATEGORY == 'Agricultural Livelihoods') %>%
  plot_change_lollipop(digits = 0, wrapy = TRUE, star = 0.05, nudge = 50) +
  labs(x = NULL, y = NULL, title = 'Agricultural Livelihoods') +
  xlim(-600, 600)

part2 = netchange %>%
  filter(METRIC_CATEGORY == 'Water Quality') %>%
  plot_change_lollipop(digits = 0, wrapy = TRUE) +
  labs(x = NULL, y = NULL, title = 'Water Quality') +
  xlim(-75, 75)

part3 = netchange %>%
  filter(METRIC_CATEGORY == 'Climate Change Resilience') %>%
  plot_change_lollipop(digits = 1, wrapy = TRUE) +
  labs(x = NULL, y = NULL, title = 'Climate Change Resilience') +
  xlim(-0.3, 0.3)

part4 = netchange %>%
  filter(METRIC_CATEGORY == 'Biodiversity Support') %>%
  plot_change_lollipop(digits = 0, wrapy = TRUE) +
  facet_wrap(~scenario, ncol = 3, strip.position = 'bottom') +
  labs(x = NULL, y = NULL, title = 'Biodiversity Support') +
  xlim(-7000, 7000)
# +
  # theme(strip.text = element_text(family = 'sourcesans', size = 16, vjust = 1))


showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
part1/part2/part3/part4
ggsave('fig/presentations/netchange_lollipop_all.png',
       height = 8, width = 10, units = 'in')
showtext_auto(FALSE)

### bar chart-------
# same as for manuscript but with larger fonts and separate panels for each category


part1 = netchange %>%
  filter(METRIC_CATEGORY == 'Agricultural Livelihoods') %>%
  plot_change_bar(star = 0.05, nudge = 80, textsize = 8, linewidth = 1,
                  errorwidth = 0.3, axistext = 20) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_text(lineheight = 0.8),
        plot.title = element_blank())
part2 = netchange %>%
  filter(METRIC_CATEGORY == 'Water Quality') %>%
  plot_change_bar(star = 0.05, nudge = 20, textsize = 8, linewidth = 1,
                  errorwidth = 0.3, axistext = 20) +
  labs(x = NULL, y = NULL) +
  # xlim(-75, 75) +
  theme(axis.text = element_text(lineheight = 0.8),
        plot.title = element_blank())
part3 = netchange %>%
  filter(METRIC_CATEGORY == 'Climate Change Resilience') %>%
  plot_change_bar(star = 0.05, nudge = 0.01, textsize = 8, linewidth = 1,
                  errorwidth = 0.3, axistext = 20) +
  labs(x = NULL, y = NULL) +
  xlim(-1, 1) +
  theme(axis.text = element_text(lineheight = 0.8),
        plot.title = element_blank())
part4 = netchange %>%
  filter(METRIC_CATEGORY == 'Biodiversity Support' & !is.na(METRIC)) %>%
  plot_change_bar(star = 0.05, nudge = 2, textsize = 8, linewidth = 1,
                  errorwidth = 0.3, axistext = 20) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_text(lineheight = 0.8),
        plot.title = element_blank())

## plot as separate panels:
showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
part1
ggsave('fig/presentations/netchange_aglivelihoods.png',
       height = 4.5, width = 12, units = 'in')
part2
ggsave('fig/presentations/netchange_waterquality.png',
       height = 4.5, width = 12, units = 'in')
part3
ggsave('fig/presentations/netchange_climatechange.png',
       height = 4.5, width = 12, units = 'in')
part4
ggsave('fig/presentations/netchange_biodiversity.png',
       height = 4.5, width = 12, units = 'in')

# plot combined (drop units)
part1 = netchange %>%
  filter(METRIC_CATEGORY == 'Agricultural Livelihoods') %>%
  mutate(METRIC = gsub('\\\n.*$', '', METRIC)) %>%
  plot_change_bar(star = 0.05, nudge = 80, textsize = 8, linewidth = 1,
                  errorwidth = 0.3, axistext = 20) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_text(lineheight = 0.8),
        plot.title = element_blank())
part2 = netchange %>%
  filter(METRIC_CATEGORY == 'Water Quality') %>%
  mutate(METRIC = gsub('\\\n.*$', '', METRIC)) %>%
  plot_change_bar(star = 0.05, nudge = 20, textsize = 8, linewidth = 1,
                  errorwidth = 0.3, axistext = 20) +
  labs(x = NULL, y = NULL) +
  # xlim(-75, 75) +
  theme(axis.text = element_text(lineheight = 0.8),
        plot.title = element_blank())
part3 = netchange %>%
  filter(METRIC_CATEGORY == 'Climate Change Resilience') %>%
  mutate(METRIC = gsub('\\\n.*$', '', METRIC)) %>%
  plot_change_bar(star = 0.05, nudge = 0.01, textsize = 8, linewidth = 1,
                  errorwidth = 0.3, axistext = 20) +
  labs(x = NULL, y = NULL) +
  xlim(-1, 1) +
  theme(axis.text = element_text(lineheight = 0.8),
        plot.title = element_blank())
part4 = netchange %>%
  filter(METRIC_CATEGORY == 'Biodiversity Support' & !is.na(METRIC)) %>%
  mutate(METRIC = gsub('\\\n.*$', '', METRIC)) %>%
  plot_change_bar(star = 0.05, nudge = 2, textsize = 8, linewidth = 1,
                  errorwidth = 0.3, axistext = 20) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_text(lineheight = 0.8),
        plot.title = element_blank())

part1/part2/part3/part4
ggsave('fig/presentations/netchange_barchart.png',
       height = 6.5, width = 13, units = 'in')

# option for separate panels for each category:
netchange %>%
  filter(METRIC_CATEGORY == 'Agricultural Livelihoods') %>%
  plot_change_lollipop(digits = 0, wrapy = FALSE) +
  labs(x = NULL, y = NULL) +
  xlim(-600, 600) +
  theme(strip.text = element_text(family = 'sourcesans', size = 18, hjust = 0, face = 'bold'),
        axis.text = element_text(family = 'sourcesans', size = 16))
ggsave('fig/presentations/netchange_lollipop_aglivelihoods.png',
       height = 4, width = 10, units = 'in')

netchange %>%
  filter(METRIC_CATEGORY == 'Water Quality') %>%
  plot_change_lollipop(digits = 0, wrapy = FALSE) +
  labs(x = NULL, y = NULL) +
  xlim(-75, 75) +
  theme(strip.text = element_text(family = 'sourcesans', size = 18, hjust = 0, face = 'bold'),
        axis.text = element_text(family = 'sourcesans', size = 16))
ggsave('fig/presentations/netchange_lollipop_waterquality.png',
       height = 4, width = 10, units = 'in')

netchange %>%
  filter(METRIC_CATEGORY == 'Climate Change Resilience') %>%
  plot_change_lollipop(digits = 0, wrapy = FALSE) +
  labs(x = NULL, y = NULL) +
  xlim(-0.3, 0.3) +
  theme(strip.text = element_text(family = 'sourcesans', size = 18, hjust = 0, face = 'bold'),
        axis.text = element_text(family = 'sourcesans', size = 16))
ggsave('fig/presentations/netchange_lollipop_climatechange.png',
       height = 4, width = 10, units = 'in')

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
