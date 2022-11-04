# PLOTS OF INPUT DATA

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')

# key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')
key = readxl::read_excel('GIS/VEG_key.xlsx')

label.order = key %>% select(CODE_NAME, LABEL) %>% distinct() %>%
  filter(!CODE_NAME %in% c(
    'PERENNIAL_CROPS', 'ANNUAL_CROPS', 'GRAIN&HAY', 'FIELD',
    'GRASSLAND&PASTURE', 'WOODLAND&SCRUB', 'WATER', 'WETLAND',
    'WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL')) %>%
  filter(!grepl('RIPARIAN_', CODE_NAME) & !is.na(CODE_NAME)) %>%
  pull(LABEL) %>% unique()
label.order = label.order[c(1:9, 11:13, 10, 14:22)]

library(showtext)
font_add_google('Source Sans Pro', 'sourcesans')

# WATER QUALITY / PESTICIDES-------------
waterquality = read_csv('data/pesticide_exposure.csv', col_types = cols()) %>%
  filter(METRIC %in%
           c('Risk to Aquatic Organisms', 'Critical Pesticides',
             'Groundwater Contaminant'))

showtext_auto()
showtext_opts(dpi = 300)
waterquality %>%
  left_join(key %>% select(LABEL, CODE_NAME), by = 'CODE_NAME') %>%
  filter(LABEL %in% label.order) %>%
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
ggsave('fig/metrics_pesticides.png', height = 4, width = 8)
showtext_auto(F)

# ECONOMY-----------
economy = bind_rows(
  read_csv('data/crop_production_value.csv', col_types = cols()),
  read_csv('data/livelihoods.csv', col_types = cols()))

showtext_auto()
showtext_opts(dpi = 300)
economy %>%
  left_join(key %>% select(LABEL, CODE_NAME), by = 'CODE_NAME') %>%
  filter(LABEL %in% label.order) %>%
  mutate(LABEL = factor(LABEL, levels = rev(label.order))) %>%
  select(LABEL, CODE_NAME, METRIC, SCORE_MEAN, SCORE_SE) %>%
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
ggsave('fig/metrics_economy.png', height = 4, width = 8)
showtext_auto(F)

# CLIMATE CHANGE RESILIENCE SCORE---------
ccs = read_csv('data/climate_change_resilience.csv', col_types = cols())

showtext_auto()
showtext_opts(dpi = 300)
ccs %>%
  # filter(METRIC != 'Overall') %>%
  left_join(key %>% select(LABEL, CODE_NAME), by = 'CODE_NAME') %>%
  filter(LABEL %in% label.order) %>%
  mutate(LABEL = factor(LABEL, levels = rev(label.order)),
         METRIC = factor(METRIC, levels = c('Heat', 'Drought', 'Flood', 'Salinity', 'Overall'))) %>%
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
showtext_auto(F)

# # AVIAN CONSERVATION SCORE----------
# acs = read_csv('data/avian_conservation_score.csv', col_types = cols())
#
# showtext_auto()
# showtext_opts(dpi = 300)
# acs %>%
#   left_join(key %>% select(LABEL, CODE_NAME), by = 'CODE_NAME') %>%
#   filter(LABEL %in% label.order & !grepl('all', METRIC, ignore.case = TRUE)) %>%
#   mutate(LABEL = factor(LABEL, levels = rev(label.order))) %>%
#   separate(METRIC, into = c('group', 'METRIC'), sep = '_') %>%
#   mutate(METRIC = factor(METRIC,
#                          levels = c('Riparian', 'Oak Savannah', 'Grassland',
#                                     'Shorebirds', 'Waterfowl',
#                                     'Other Waterbirds'))) %>%
#   select(group, LABEL, CODE_NAME, METRIC, SCORE_MEAN, SCORE_SE) %>%
#   ggplot(aes(SCORE_MEAN, LABEL)) +
#   geom_col(aes(fill = METRIC), width = 0.75, position = 'dodge') +
#   facet_wrap(~group, ncol = 3) +
#   scale_fill_manual(values = pointblue.palette[c(3, 1, 2, 4:6)]) +
#   labs(x = 'Avian Conservation Score', y = NULL, fill = NULL) +
#   theme_bw() +
#   theme(axis.text = element_text(family = 'sourcesans', size = 10),
#         axis.title = element_text(family = 'sourcesans', size = 10),
#         strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
#         strip.background = element_blank(),
#         legend.background = element_rect(fill = NA),
#         legend.position = 'right',
#         legend.text = element_text(family = 'sourcesans', size = 10))
# ggsave('fig/metrics_acs.png', height = 4, width = 8)
# showtext_auto(F)

# old 9-panel version
# acs %>%
#   # filter(!METRIC %in% c('Breeding Landbirds', 'Breeding Waterbirds', 'Wintering Waterbirds')) %>%
#   # filter(LABEL != 'Wheat') %>%
#   mutate(LABEL = factor(LABEL, levels = rev(label.order))) %>%
#   select(LABEL, CODE_NAME, METRIC, SCORE_MEAN, SCORE_SE) %>%
#   ggplot(aes(SCORE_MEAN, LABEL)) +
#   geom_col(fill = pointblue.palette[1]) +
#   geom_errorbar(aes(xmin = SCORE_MEAN - SCORE_SE,
#                     xmax = SCORE_MEAN + SCORE_SE),
#                 width = 0.5) +
#   geom_text(aes(label = SCORE_MEAN), hjust = 0, nudge_x = 0.05, size = 3) +
#   facet_wrap(~METRIC, ncol = 3) +
#   # scale_fill_distiller(palette = 'Purples', direction = 1) +
#   labs(x = 'Avian conservation score', y = NULL) +
#   theme_bw() +
#   theme(axis.text = element_text(size = 10),
#         axis.title = element_text(size = 11),
#         strip.text = element_text(size = 10, hjust = 0),
#         strip.background = element_blank(),
#         legend.position = 'none')
# ggsave('fig/metrics_acs.png', height = 9, width = 8)

# TABLE---------
metrics = bind_rows(economy, waterquality, ccs) %>%
  left_join(key %>% select(LABEL, CODE_NAME), by = 'CODE_NAME') %>%
  filter(LABEL %in% label.order) %>%
  mutate(LABEL = factor(LABEL, levels = label.order),
         METRIC_CATEGORY = factor(METRIC_CATEGORY,
                                  levels = c('economy', 'water quality',
                                             'climate', 'biodiversity'))) %>%
  arrange(LABEL)

metrics_table = metrics %>%
  mutate(SCORE_MEAN = round(SCORE_MEAN, digits = 2) %>% format(nsmall = 2),
         SCORE_SE = paste0('(', round(SCORE_SE, digits = 2) %>% format(nsmall = 2), ')')
         # SCORE = paste0(SCORE_MEAN, ' (', SCORE_SE, ')'),
         # SCORE = gsub(' \\(  NA\\)', '', SCORE)
         ) %>%
  select(LABEL, METRIC_CATEGORY, METRIC, SCORE_MEAN, SCORE_SE) %>%
  pivot_wider(names_from = c(METRIC_CATEGORY, METRIC), values_from = c(SCORE_MEAN, SCORE_SE)) %>%
  select(LABEL, contains('economy_Ag'), contains('economy_An'),
         contains('economy_Gross'), contains('quality_Crit'),
         contains('quality_Gr'), contains('quality_Risk'),
         contains('climate_Dr'), contains('climate_Fl'), contains('climate_He'),
         contains('climate_Sal'))
write_csv(metrics_table, 'output/TABLE_metrics_summary.csv')
