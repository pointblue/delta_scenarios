# README---------
# From baseline land cover layer, develop scenario of rising flood risk, based
# on Delta Adapts analyses, leading to pre-emptive conversion to managed
# wetlands where ag will not be compatible and conversion to annual crops where
# perennial crops will not be compatible

# PACKAGES & FUNCTIONS
source('R/packages.R')

# reference data:
delta = rast('GIS/boundaries/delta.tif')
delta_buff10k = read_sf('GIS/boundaries/Legal_Delta_boundary_buff10k.shp') %>%
  st_transform(crs = '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')
template = rasterize(vect(delta_buff10k), extend(delta, delta_buff10k))

baseline = rast('GIS/landscape_rasters/veg_baseline_fall.tif')
baseline_win = rast('GIS/landscape_rasters/veg_baseline_winter.tif')
wkey = read_csv('data/landcover_key_waterbirds.csv', col_types = cols())

# scenario assumption: all "high" and "very high" flood risk areas become
# managed wetlands, and all perennial crops move out of "medium" risk areas
# --> except where already water, riparian, urban, tidal marsh, rice


# source data---------
# combine separate layers representing flood risk for 2050 sea level rise
# scenario (M6)
filelist = c('DeltaAdapts_basepolygons_201001', 'M6_200yrpoly', 'M6_100yrpoly',
             'M6_50yrpoly', 'M6_10yrpoly')
tmp = purrr::map(c(1:5),
                 function(x) {
                   read_sf(dsn = 'GIS/original_source_data/210115_DeltaAdapts_FloodExposure_External.gdb',
                         layer = filelist[x]) %>%
                     mutate(risk = x) %>%
                     vect() %>%
                     rasterize(template, field = 'risk')}
                 )

fkey = data.frame(id = c(1:5),
                  label = c('very low', 'low', 'medium', 'high', 'very high'),
                  risk.annual = c('<0.5%', '0.5-1%', '1-2%', '2-10%', '>10%'),
                  col = c('lightskyblue', 'dodgerblue', 'royalblue', 'blue3', 'midnightblue'))


floodrisk = cover(tmp[[5]], tmp[[4]]) %>%
  cover(tmp[[3]]) %>%
  cover(tmp[[2]]) %>%
  cover(tmp[[1]]) %>%
  crop(template) %>%
  mask(template)
levels(floodrisk) <- fkey
coltab(floodrisk) <- fkey %>% select(id, col) %>%
  complete(id = c(0:255)) %>% pull(col)
plot(floodrisk)

writeRaster(floodrisk, 'GIS/scenario_inputs/floodrisk2050.tif')

# add managed wetlands-------

# 1. erase areas unsuitable for conversion to managed wetland
highrisk = classify(floodrisk,
                    rcl = matrix(c(4, 18,
                                   5, 18), byrow = TRUE, ncol = 2),
                    othersNA = TRUE)
highrisk_limited = lapp(c(highrisk, baseline),
                        function(x, y) {
                          ifelse(y %in% c('water', 'riparian', 'urban',
                                          'tidal marsh', 'rice') &
                                   !is.na(x),
                                 NA, x)
                        })

# 2. overlay
added_wetlands = cover(highrisk_limited, baseline)
added_wetlands_win = cover(highrisk_limited, baseline_win)

# replace perennial crops------
# make replacement annual crops proportional to the area of these crops lost to
# managed wetlands in the high risk areas (calculated separately for fall and
# winter)

# calculate change in area of each land cover (separate for fall and winter)
change_fall = DeltaMultipleBenefits::calculate_change(
  baseline = baseline,
  scenario = added_wetlands)
# lots of corn, fallow, field/row, alfalfa, orch/vin; some "other wetland"
# converted to managed

change_win = DeltaMultipleBenefits::calculate_change(
  baseline = baseline_win,
  scenario = added_wetlands_win)
# similar - lots of fallow, alfalfa, corn, field/row, orch/vin)

# identify contiguous patches of orchard pixels in the medium flood risk areas
medrisk = subst(floodrisk, c(1:2, 4:5), NA)
med_orch_patches = mask(baseline, medrisk) %>%
  subst(c(0:8,10:99), NA) %>%
  patches()
plot(med_orch_patches)

# map each contiguous patch of orchard to a new randomly selected crop, weighted
# by the proportion lost to wetlands in the "very high" risk islands:
prop_sample = change_fall %>%
  select(value, label, change) %>%
  filter(change < 0 &
           label %in% c('corn', 'field/row', 'alfalfa', 'grassland', 'grain',
           'pasture')) %>%
  left_join(wkey %>% select(value = WATERBIRD_CODE, shortlab)) %>%
  mutate(change = abs(change),
         prop = change / sum(change))
# 32% corn, ~20% each row and alfalfa

new_crops = tibble(patchID = c(1:minmax(med_orch_patches)[2]),
                   newID = sample(prop_sample$value,
                                  minmax(med_orch_patches)[2],
                                  replace = TRUE,
                                  prob = prop_sample$prop))
replace_orchard = classify(med_orch_patches,
                           rcl = as.matrix(new_crops))

# repeat for winter baseline
prop_sample_win = change_win %>%
  select(value, label, change) %>%
  filter(change < 0 &
           label %in% c('corn', 'field/row', 'alfalfa', 'grassland', 'grain',
                        'pasture')) %>%
  left_join(wkey %>% select(value = WATERBIRD_CODE, shortlab)) %>%
  mutate(change = abs(change),
         prop = change / sum(change))
# 20% alfalfa

new_crops_win = tibble(patchID = c(1:minmax(med_orch_patches)[2]),
                       newID = sample(prop_sample_win$value,
                                      minmax(med_orch_patches)[2],
                                      replace = TRUE,
                                      prob = prop_sample_win$prop))
replace_orchard_win = classify(med_orch_patches,
                               rcl = as.matrix(new_crops_win))


# overlay on baseline------
scenario_floodrisk = cover(replace_orchard, added_wetlands)
levels(scenario_floodrisk) <- wkey %>% select(WATERBIRD_CODE, label) %>%
  as.data.frame()
coltab(scenario_floodrisk) <- wkey %>% select(WATERBIRD_CODE, col) %>%
  complete(WATERBIRD_CODE = c(0:255)) %>% pull(col)
names(scenario_floodrisk) = 'scenario_floodrisk'
plot(c(baseline, scenario_floodrisk))
writeRaster(scenario_floodrisk,
            'GIS/scenario_rasters/scenario3_floodrisk_fall.tif',
            overwrite = TRUE)

scenario_floodrisk_win = cover(replace_orchard_win, added_wetlands_win)
levels(scenario_floodrisk_win) <- wkey %>% select(WATERBIRD_CODE, label) %>%
  as.data.frame()
coltab(scenario_floodrisk_win) <- wkey %>% select(WATERBIRD_CODE, col) %>%
  complete(WATERBIRD_CODE = c(0:255)) %>% pull(col)
names(scenario_floodrisk_win) = 'scenario_floodrisk'
plot(c(baseline_win, scenario_floodrisk_win))
writeRaster(scenario_floodrisk_win,
            'GIS/scenario_rasters/scenario3_floodrisk_winter.tif',
            overwrite = TRUE)

# calculate change------

delta_floodrisk = DeltaMultipleBenefits::calculate_change(
  baseline = baseline %>% mask(delta),
  scenario = scenario_floodrisk %>% mask(delta))
delta_floodrisk %>%
  group_by(label.base) %>%
  summarize(change = sum(change), .groups = 'drop') %>%
  arrange(change) %>%
  rename(label = label.base) %>%
  DeltaMultipleBenefits::plot_change(scale = 1000000) +
  labs(x = NULL, y = bquote(' ' *Delta~ 'total area ('~km^2*')')) +
  theme_bw() + coord_flip() + ylim(-100, 410) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))
ggsave('fig/change_scenario3_floodrisk.png', height = 7.5, width = 6)
# large increase in wetland cover, at the expense of especially orch/vin, corn,
# fallow, field/row, alf

