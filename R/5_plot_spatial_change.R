# README---------
# Script to calculate the net change in spatial distribution predictions


# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')

# reference data:
delta = rast('GIS/boundaries/delta.tif')
county_raster = rast('GIS/landscape_rasters/boundaries/counties.tif')
delta_shp = read_sf('GIS/boundaries/Legal_Delta_Boundary.shp') %>%
  st_transform(crs = crs(delta))

palette = c("#2b83ba", "#80bfab", "#c7e8ad", "#ffffbf", "#fdc980", "#f07c4a",
            "#d7191c")
palette2 = c("darkblue", "white", "darkred")

spp_key = read_csv('output/TABLE_species_key.csv')

library(showtext)
font_add_google('Source Sans Pro', 'sourcesans')

# PLOT CHANGE MAPS---------

## riparian-------
change1_rip = list.files('GIS/change_rasters/riparian/scenario1_restoration/',
                         '.tif$', full.names = TRUE) %>% rast() %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'spp') %>%
  filter(value != 0) %>%
  left_join(spp_key, by = 'spp') %>%
  mutate(value = factor(value, levels = c(-1, 1),
                        labels = c('loss', 'gain')),
         label = factor(label, levels = spp_key$label))

showtext_auto()
showtext_opts(dpi = 300)
ggplot(change1_rip) + facet_wrap(~label, ncol = 5) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_manual(values = c('gain' = palette[1], 'loss' = palette[7])) +
  # scale_fill_gradientn(colors = rev(c(palette[1], 'white', palette[7])),
  #                      na.value = 'transparent',
  #                      limits = c(-1, 1)) +
  labs(x = NULL, y = NULL, fill = 'Change in\npredicted\npresence') +
  geom_sf(data = delta_shp, fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 10),
        legend.text = element_text(family = 'sourcesans', size = 9),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        # legend.margin = margin(0, 0, 0, 5.5, unit = 'pt'),
        plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
        panel.spacing = unit(3, 'pt'))
ggsave('fig/changemap_scenario1_riparian.png', height = 7, width = 10)
showtext_auto(F)

change2_rip = list.files('GIS/change_rasters/riparian/scenario2_perennialexpand/',
                         '.tif$', full.names = TRUE) %>% rast() %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'spp') %>%
  filter(value != 0) %>%
  left_join(spp_key, by = 'spp') %>%
  mutate(value = factor(value, levels = c(-1, 1),
                        labels = c('loss', 'gain')),
         label = factor(label, levels = spp_key$label))

showtext_auto()
showtext_opts(dpi = 300)
ggplot(change2_rip) + facet_wrap(~label, ncol = 5) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_manual(values = c('gain' = palette[1], 'loss' = palette[7])) +
  # scale_fill_gradientn(colors = rev(c(palette[1], 'white', palette[7])),
  #                      na.value = 'transparent',
  #                      limits = c(-1, 1)) +
  labs(x = NULL, y = NULL, fill = 'Change in\npredicted\npresence') +
  geom_sf(data = delta_shp, fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 10),
        legend.text = element_text(family = 'sourcesans', size = 9),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        # legend.margin = margin(0, 0, 0, 5.5, unit = 'pt'),
        plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
        panel.spacing = unit(3, 'pt'))
ggsave('fig/changemap_scenario2_riparian.png', height = 7, width = 10)
showtext_auto(F)

## waterbird_fall-----------

change1_fall = list.files('GIS/change_rasters/waterbird_fall/scenario1_restoration/',
                          '.tif$', full.names = TRUE) %>% rast() %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'spp') %>%
  filter(value != 0) %>%
  left_join(spp_key, by = 'spp') %>%
  mutate(value = factor(value, levels = c(-1, 1),
                        labels = c('loss', 'gain')),
         label = factor(label, levels = spp_key$label))

showtext_auto()
showtext_opts(dpi = 300)
ggplot(change1_fall) + facet_wrap(~label, ncol = 3) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_manual(values = c('gain' = palette[1], 'loss' = palette[7])) +
  # scale_fill_gradientn(colors = rev(c(palette[1], 'white', palette[7])),
  #                      na.value = 'transparent',
  #                      limits = c(-1, 1)) +
  labs(x = NULL, y = NULL, fill = 'Change in\npredicted\npresence') +
  geom_sf(data = delta_shp, fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 10),
        legend.text = element_text(family = 'sourcesans', size = 9),
        legend.position = 'right',
        # legend.justification = c(1, 0),
        # legend.margin = margin(0, 0, 0, 5.5, unit = 'pt'),
        plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
        panel.spacing = unit(3, 'pt'))
ggsave('fig/changemap_scenario1_waterbirds_fall.png', height = 7, width = 7)
showtext_auto(F)

change2_fall = list.files('GIS/change_rasters/waterbird_fall/scenario2_perennialexpand/',
                          '.tif$', full.names = TRUE) %>% rast() %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'spp') %>%
  filter(value != 0) %>%
  left_join(spp_key, by = 'spp') %>%
  mutate(value = factor(value, levels = c(-1, 1),
                        labels = c('loss', 'gain')),
         label = factor(label, levels = spp_key$label))

showtext_auto()
showtext_opts(dpi = 300)
ggplot(change2_fall) + facet_wrap(~label, ncol = 3) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_manual(values = c('gain' = palette[1], 'loss' = palette[7])) +
  # scale_fill_gradientn(colors = rev(c(palette[1], 'white', palette[7])),
  #                      na.value = 'transparent',
  #                      limits = c(-1, 1)) +
  labs(x = NULL, y = NULL, fill = 'Change in\npredicted\npresence') +
  geom_sf(data = delta_shp, fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 10),
        legend.text = element_text(family = 'sourcesans', size = 9),
        legend.position = 'right',
        # legend.justification = c(1, 0),
        # legend.margin = margin(0, 0, 0, 5.5, unit = 'pt'),
        plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
        panel.spacing = unit(3, 'pt'))
ggsave('fig/changemap_scenario2_waterbirds_fall.png', height = 7, width = 7)
showtext_auto(F)

## waterbird_winter-----------

change1_win = list.files('GIS/change_rasters/waterbird_win/scenario1_restoration/',
                         '.tif$', full.names = TRUE) %>% rast() %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'spp') %>%
  filter(value != 0) %>%
  left_join(spp_key, by = 'spp') %>%
  mutate(value = factor(value, levels = c(-1, 1),
                        labels = c('loss', 'gain')),
         label = factor(label, levels = spp_key$label))

showtext_auto()
showtext_opts(dpi = 300)
ggplot(change1_win) + facet_wrap(~label, ncol = 3) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_manual(values = c('gain' = palette[1], 'loss' = palette[7])) +
  # scale_fill_gradientn(colors = rev(c(palette[1], 'white', palette[7])),
  #                      na.value = 'transparent',
  #                      limits = c(-1, 1)) +
  labs(x = NULL, y = NULL, fill = 'Change in\npredicted\npresence') +
  geom_sf(data = delta_shp, fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 10),
        legend.text = element_text(family = 'sourcesans', size = 9),
        legend.position = 'right',
        # legend.justification = c(1, 0),
        # legend.margin = margin(0, 0, 0, 5.5, unit = 'pt'),
        plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
        panel.spacing = unit(3, 'pt'))
ggsave('fig/changemap_scenario1_waterbirds_winter.png', height = 7, width = 7)
showtext_auto(F)

change2_win = list.files('GIS/change_rasters/waterbird_win/scenario2_perennialexpand/',
                          '.tif$', full.names = TRUE) %>% rast() %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'spp') %>%
  filter(value != 0) %>%
  left_join(spp_key, by = 'spp') %>%
  mutate(value = factor(value, levels = c(-1, 1),
                        labels = c('loss', 'gain')),
         label = factor(label, levels = spp_key$label))

showtext_auto()
showtext_opts(dpi = 300)
ggplot(change2_win) + facet_wrap(~label, ncol = 3) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_manual(values = c('gain' = palette[1], 'loss' = palette[7])) +
  # scale_fill_gradientn(colors = rev(c(palette[1], 'white', palette[7])),
  #                      na.value = 'transparent',
  #                      limits = c(-1, 1)) +
  labs(x = NULL, y = NULL, fill = 'Change in\npredicted\npresence') +
  geom_sf(data = delta_shp, fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 10),
        legend.text = element_text(family = 'sourcesans', size = 9),
        legend.position = 'right',
        # legend.justification = c(1, 0),
        # legend.margin = margin(0, 0, 0, 5.5, unit = 'pt'),
        plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
        panel.spacing = unit(3, 'pt'))
ggsave('fig/changemap_scenario2_waterbirds_winter.png', height = 7, width = 7)
showtext_auto(F)
