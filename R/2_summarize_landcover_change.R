# README
# compile land cover totals for the baseline and each scenario; calculate the
# net impact of each scenario on the coverage of each land cover class, and plot
# the results

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

# PLOT-------
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
    mutate(

      bin = if_else(net_change > 0, 'increase', 'decrease'),
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
                           'scenario1_restoration' = 'Scenario 1',
                           'scenario2_perennialexpand' = 'Scenario 2',
                           'scenario3_combo' = 'Scenario 3'))

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
       # x = expression(paste(Delta, ' total area (ha, thousands)')),
  ) +
  xlim(-7, 20) +
  geom_vline(xintercept = 0, size = 0.2) +
  theme_bw() +
  theme(strip.placement = 'outside',
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 10),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = 'sourcesans', size = 8, hjust = 1),
        axis.title = element_text(family = 'sourcesans', size = 8),
        legend.position = 'none')

part2 = change_summary %>% filter(group != 'Major classes') %>%
  ggplot(aes(net_change/1000, LABEL)) +
  facet_wrap(~scenario, ncol = 3, strip.position = 'bottom') +
  geom_col(fill = 'gray60') +
  geom_text(data = change_summary %>% filter(group != 'Major classes' & bin == 'increase'),
            aes(x = net_change/1000 + 2,
                label = paste0(round(change_pct, digits = 0), '%')), size = 2) +
  geom_text(data = change_summary %>% filter(group != 'Major classes' & bin == 'decrease'),
            aes(x = net_change/1000 - 2,
                label = paste0(round(change_pct, digits = 0), '%')), size = 2) +
  labs(x = expression(paste(Delta, ' total area (ha, thousands)')),
       y = NULL,
       title = 'B') +
  xlim(-7, 20) +
  geom_vline(xintercept = 0, size = 0.2) +
  theme_bw() +
  theme(strip.placement = 'outside',
        strip.text = element_text(family = 'sourcesans', size = 10),
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
ggsave('fig/netchange_land_cover.jpg',
       height = 5, width = 6, units = 'in', dpi = 300)
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

