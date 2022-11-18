# PLOTS OF INPUT DATA

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')
key = readxl::read_excel('GIS/VEG_key.xlsx')

# standardize formatting
label.order = key %>% select(CODE_NAME, LABEL) %>% distinct() %>%
  filter(!CODE_NAME %in% c(
    'PERENNIAL_CROPS', 'ANNUAL_CROPS', 'GRAIN&HAY', 'FIELD',
    'GRASSLAND&PASTURE', 'WOODLAND&SCRUB', 'WATER', 'WETLAND',
    'WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL')) %>%
  filter(!grepl('RIPARIAN_', CODE_NAME) & !is.na(CODE_NAME)) %>%
  pull(LABEL) %>% unique()
label.order = label.order[c(1:9, 11:13, 10, 14:22)]

# COMPILE ALL METRICS----------
waterquality = read_csv('data/pesticide_exposure.csv', col_types = cols()) %>%
  filter(METRIC %in%
           c('Risk to Aquatic Organisms', 'Critical Pesticides',
             'Groundwater Contaminant'))

economy = bind_rows(
  read_csv('data/crop_production_value.csv', col_types = cols()),
  read_csv('data/livelihoods.csv', col_types = cols())) %>%
  #change jobs metrics back to jobs/ha for easier landscape calculations
  mutate(across(c(SCORE_MEAN, SCORE_SE, SCORE_MIN, SCORE_MAX),
                ~if_else(METRIC == 'Agricultural Jobs', .x/100, .x)),
         UNIT = recode(UNIT, 'number of employees per 100ha' = 'employees per ha'))

ccs = read_csv('data/climate_change_resilience.csv', col_types = cols())

# acs = read_csv('data/avian_conservation_score.csv', col_types = cols())

metrics = bind_rows(economy, waterquality, ccs) %>%
  left_join(key %>% select(LABEL, CODE_NAME), by = 'CODE_NAME') %>%
  mutate(CODE_NAME = factor(CODE_NAME,
                            levels = key$CODE_NAME %>% na.omit()),
         METRIC_CATEGORY = factor(METRIC_CATEGORY,
                                  levels = c('economy', 'water quality',
                                             'climate', 'biodiversity')),
         METRIC_CATEGORY = recode(METRIC_CATEGORY,
                                  economy = 'Agricultural Livelihoods',
                                  `water quality` = 'Water Quality',
                                  climate = 'Climate Change Resilience',
                                  biodiversity = 'Biodiversity Support'),
         METRIC = gsub('_', ': ', METRIC)) %>%
  mutate_at(vars(METRIC_SUBTYPE:METRIC), factor) %>%
  arrange(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, CODE_NAME)
write_csv(metrics, 'output/metrics_final.csv')

# SUMMARY TABLE---------
# for supplemental info in manuscript
metrics_table = metrics %>%
  filter(LABEL %in% label.order) %>%
  mutate(LABEL = factor(LABEL, levels = label.order)) %>%
  arrange(LABEL) %>%
  # for the purposes of this table, convert to jobs/100ha
  mutate(across(c(SCORE_MEAN, SCORE_SE),
                ~if_else(METRIC == 'Agricultural Jobs',
                         .x*100, .x))) %>%
  mutate(SCORE_MEAN = paste0("'",round(SCORE_MEAN, digits = 2) %>% format(nsmall = 2)),
         SCORE_SE = paste0("'(", round(SCORE_SE, digits = 2) %>% format(nsmall = 2), ')')
         # SCORE = paste0(SCORE_MEAN, ' (', SCORE_SE, ')'),
         # SCORE = gsub(' \\(  NA\\)', '', SCORE)
  ) %>%
  select(LABEL, METRIC_CATEGORY, METRIC, SCORE_MEAN, SCORE_SE) %>%
  pivot_wider(names_from = c(METRIC_CATEGORY, METRIC), values_from = c(SCORE_MEAN, SCORE_SE)) %>%
  select(LABEL,
         contains('Jobs'), contains('Wages'), contains('Gross'),
         contains('Critical'), contains('Groundwater'), contains('Aquatic'),
         contains('Drought'), contains('Flood'), contains('Heat'),
         contains('Salinity'))
write_csv(metrics_table, 'output/TABLE_metrics_summary.csv')

# PLOTS BY METRIC--------
library(showtext)
font_add_google('Source Sans Pro', 'sourcesans')
showtext_auto()
showtext_opts(dpi = 300)

## ag livelihoods------
metrics %>%
  filter(METRIC_CATEGORY=='Agricultural Livelihoods' & LABEL %in% label.order) %>%
  mutate(LABEL = factor(LABEL, levels = rev(label.order))) %>%
  select(LABEL, CODE_NAME, METRIC, SCORE_MEAN, SCORE_SE) %>%
  # for plotting purposes, convert to ag jobs/100 ha
  mutate(across(c(SCORE_MEAN, SCORE_SE),
                ~if_else(METRIC == 'Agricultural Jobs', .x*100, .x))) %>%
  ggplot(aes(SCORE_MEAN, LABEL)) +
  geom_col(fill = pointblue.palette[4]) +
  geom_errorbar(aes(xmin = SCORE_MEAN - SCORE_SE,
                    xmax = SCORE_MEAN + SCORE_SE),
                width = 0.5) +
  facet_wrap(~METRIC, ncol = 3, scales = 'free_x',
             labeller = labeller(
               METRIC = c(`Agricultural Jobs` = 'Agricultural Jobs\n(FTE / 100 ha)',
                          `Annual Wages` = 'Annual Wages\n(USD, thousands)',
                          `Gross Production Value` = 'Gross Crop Production Value\n(USD / ha, millions)'))) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(family = 'sourcesans', size = 10),
        axis.title = element_text(family = 'sourcesans', size = 12),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0, vjust = 1),
        strip.background = element_blank(),
        legend.position = 'none')
ggsave('fig/metrics_aglivelihoods.png', height = 4, width = 8)

## water quality-----
metrics %>%
  filter(METRIC_CATEGORY == 'Water Quality' & LABEL %in% label.order) %>%
  mutate(LABEL = factor(LABEL, levels = rev(label.order))) %>%
  select(LABEL, CODE_NAME, METRIC, SCORE_MEAN, SCORE_SE) %>%
  ggplot(aes(SCORE_MEAN, LABEL)) + geom_col(fill = pointblue.palette[3]) +
  geom_errorbar(aes(xmin = SCORE_MEAN - SCORE_SE,
                    xmax = SCORE_MEAN + SCORE_SE),
                width = 0.5) +
  facet_wrap(~METRIC, scale = 'free_x') +
  # scale_fill_gradient(low = palette[5], high = palette[7]) +
  labs(x = 'Pesticide application rate (lbs/ha/yr)', y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(family = 'sourcesans', size = 10),
        axis.title = element_text(family = 'sourcesans', size = 11),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')
ggsave('fig/metrics_waterquality.png', height = 4, width = 8)

## climate change resilience---------
metrics %>%
  filter(METRIC_CATEGORY=='Climate Change Resilience' & LABEL %in% label.order) %>%
  mutate(LABEL = factor(LABEL, levels = rev(label.order)),
         METRIC = factor(METRIC, levels = c('Drought', 'Flood', 'Heat', 'Salinity', 'Overall'))) %>%
  select(LABEL, CODE_NAME, METRIC, SCORE_MEAN, SCORE_SE) %>%
  ggplot(aes(SCORE_MEAN, LABEL)) + geom_col(fill = 'seagreen') +
  geom_errorbar(aes(xmin = SCORE_MEAN - SCORE_SE,
                    xmax = SCORE_MEAN + SCORE_SE),
                width = 0.5) +
  facet_wrap(~METRIC, ncol = 5, scale = 'free_x') +
  labs(x = 'Climate change resilience score', y = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(family = 'sourcesans', size = 9),
        axis.text.y = element_text(family = 'sourcesans', size = 10),
        axis.title = element_text(family = 'sourcesans', size = 11),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')
ggsave('fig/metrics_climate.png', height = 4, width = 8)
