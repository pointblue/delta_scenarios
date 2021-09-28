# README---------
# From baseline land cover layer, develop scenario of perennial crop expansion
# based on Central Valley Futures project (Wilson et al. 2021), using the BBAU
# ("bad business as usual") scenario for the year 2050, including historically
# high rates of perennial crop expansion and no restoration

# PACKAGES & FUNCTIONS
source('R/packages.R')

# reference data:
delta = rast('GIS/boundaries/delta.tif')
delta_buff10k = read_sf('GIS/boundaries/Legal_Delta_boundary_buff10k.shp') %>%
  st_transform(crs = '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')
template = rasterize(vect(delta_buff10k), extend(delta, delta_buff10k))

baseline = rast('GIS/landscape_rasters/veg_baseline_fall.tif')
wkey = read_csv('data/landcover_key_waterbirds.csv', col_types = cols())

# source data---------
skey = read_csv('GIS/original_source_data/State Class Rasters/key.csv',
                show_col_types = FALSE)

bbau = rast('GIS/original_source_data/State Class Rasters/scn421.sc.it1.ts2050.tif') %>%
  project(template, method = 'near') %>%
  mask(template) %>%
  # keep only the footprint of expanded perennial crops (code=20) - reclassify
  # to waterbird code for orch (9); all others convert to NA
  classify(rcl = matrix(c(20, 9), byrow = TRUE, ncol = 2), othersNA = TRUE)
# note: missing southwest corner of Delta

# compare to baseline
tab = crosstab(c(baseline, bbau), useNA = TRUE, long = TRUE)
tab %>% rlang::set_names(c('baseline', 'bbau', 'n')) %>%
  left_join(wkey %>% select(baseline = WATERBIRD_CODE, from = label)) %>%
  left_join(wkey %>% select(bbau = WATERBIRD_CODE, to = label)) %>%
  filter(!is.na(to)) %>%
  arrange(desc(n))
# would convert to orch from all other types, including wetland, managed wetland,
# tidal marsh, riparian, and urban

# overlay----------
# overlay perennial crop footprint on baseline

# 1. erase areas from bbau that are unsuitable for conversion: exclude rice
# because assume soils suitable for rice are unsuitable for orchards
bbau_limited = lapp(c(bbau, baseline),
                    function(x, y) {
                      ifelse(y %in% c('other wetland', 'urban', 'water', 'rice',
                                      'forest/shrub', 'riparian',
                                      'managed wetland', 'tidal marsh') &
                               !is.na(x), NA, x)
                    })

# 2. overlay
scenario_perex = cover(bbau_limited, baseline)
levels(scenario_perex) <- wkey %>% select(id = WATERBIRD_CODE, label) %>%
  as.data.frame()
coltab(scenario_perex) <- wkey %>% select(WATERBIRD_CODE, col) %>%
  complete(WATERBIRD_CODE = c(0:255)) %>% pull(col)
names(scenario_perex) = 'scenario_perennial_expansion'
plot(c(baseline, scenario_perex))

writeRaster(scenario_perex,
            'GIS/scenario_rasters/scenario2_perennialexpand.tif',
            overwrite = TRUE)

# calculate change-------
delta_perex = DeltaMultipleBenefits::calculate_change(
  baseline = baseline %>% mask(delta),
  scenario = scenario_perex %>% mask(delta))
delta_perex %>% rename(label = label.base) %>%
  group_by(label) %>%
  summarize(change = sum(change), .groups = 'drop') %>%
  arrange(change) %>%
  DeltaMultipleBenefits::plot_change(scale = 1000000) +
  labs(x = NULL, y = bquote(' ' *Delta~ 'total area ('~km^2*')')) +
  theme_bw() + coord_flip() + ylim(-50, 150) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))
ggsave('fig/change_scenario2_perennialexpand.png', height = 7.5, width = 6)
# large increase in orchard cover, at the expense of: field/row, fallow, corn,
# grain, alfalfa, pasture, grassland
