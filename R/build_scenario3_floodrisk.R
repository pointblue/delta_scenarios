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
key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')

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

writeRaster(floodrisk, 'GIS/scenario_inputs/floodrisk2050.tif', overwrite = TRUE)

# add managed wetlands-------

# 1. recode all high risk wetlands as perennial managed wetlands
highrisk = classify(floodrisk,
                    rcl = matrix(c(4, 82,
                                   5, 82), byrow = TRUE, ncol = 2),
                    othersNA = TRUE)
# --> but mask out areas unsuitable for conversion to managed wetland
# (urban, riparian, water, tidal marsh, rice)
highrisk_limited = lapp(c(highrisk, baseline),
                        function(x, y) {
                          ifelse(y %in% c(60, 70:77, 86, 90, 30) &
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

# identify contiguous patches of orch & vin pixels in the medium flood risk areas
medrisk = subst(floodrisk, c(1:2, 4:5), NA)
med_orch_patches = mask(baseline, medrisk) %>%
  subst(c(20:130), NA) %>%
  patches()
plot(med_orch_patches)

# map each contiguous patch of orchard to a new randomly selected crop, weighted
# by the proportion lost to wetlands in the "very high" risk islands:
prop_sample = change_fall %>%
  select(value, change) %>%
  left_join(key %>% select(value = CODE_BASELINE, label = CODE_NAME)) %>%
  filter(change < 0 &
           label %in% c('FIELD_CORN', 'FIELD', 'ROW', 'PASTURE_ALFALFA',
                        'PASTURE', 'GRAIN&HAY_WHEAT', 'GRAIN&HAY')) %>%
  mutate(change = abs(change),
         prop = change / sum(change))
# 35% corn, ~20% each row and alfalfa

new_crops = tibble(patchID = c(1:minmax(med_orch_patches)[2]),
                   newID = sample(prop_sample$value,
                                  minmax(med_orch_patches)[2],
                                  replace = TRUE,
                                  prob = prop_sample$prop))
replace_orchard = classify(med_orch_patches,
                           rcl = as.matrix(new_crops))

# repeat for winter baseline
prop_sample_win = change_win %>%
  select(value, change) %>%
  left_join(key %>% select(value = CODE_BASELINE, label = CODE_NAME)) %>%
  filter(change < 0 &
           label %in% c('FIELD_CORN', 'FIELD', 'ROW', 'PASTURE_ALFALFA',
                        'PASTURE', 'GRAIN&HAY_WHEAT', 'GRAIN&HAY')) %>%
  mutate(change = abs(change),
         prop = change / sum(change))
# 22% alfalfa

new_crops_win = tibble(patchID = c(1:minmax(med_orch_patches)[2]),
                       newID = sample(prop_sample_win$value,
                                      minmax(med_orch_patches)[2],
                                      replace = TRUE,
                                      prob = prop_sample_win$prop))
replace_orchard_win = classify(med_orch_patches,
                               rcl = as.matrix(new_crops_win))


# overlay on baseline------
scenario_floodrisk = cover(replace_orchard, added_wetlands)
levels(scenario_floodrisk) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
coltab(scenario_floodrisk) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
names(scenario_floodrisk) = 'scenario_floodrisk'
writeRaster(scenario_floodrisk,
            'GIS/scenario_rasters/scenario3_floodrisk.tif',
            overwrite = TRUE)

levels(baseline) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
coltab(baseline) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
names(baseline) = 'baseline'
plot(c(baseline, scenario_floodrisk))


scenario_floodrisk_win = cover(replace_orchard_win, added_wetlands_win)
levels(scenario_floodrisk_win) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
coltab(scenario_floodrisk_win) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
names(scenario_floodrisk_win) = 'scenario_floodrisk_win'
plot(c(baseline_win, scenario_floodrisk_win))
writeRaster(scenario_floodrisk_win,
            'GIS/scenario_rasters/scenario3_floodrisk_win.tif',
            overwrite = TRUE)

# calculate change------

delta_floodrisk = DeltaMultipleBenefits::calculate_change(
  baseline = baseline %>% mask(delta),
  scenario = scenario_floodrisk %>% mask(delta))
delta_floodrisk %>%
  mutate(label = case_when(label.base == 'FIELD_CORN' ~ 'CORN',
                           label.base == 'PASTURE_ALFALFA' ~ 'ALFALFA',
                           label.base == 'GRAIN&HAY' ~ 'GRAIN',
                           label.base == 'GRAIN&HAY_WHEAT' ~ 'GRAIN',
                           label.base %in% c('ROW', 'FIELD') ~ 'ROW & FIELD CROPS',
                           label.base == 'WOODLAND&SCRUB' ~ 'WOODLAND & SCRUB',
                           grepl('RIPARIAN', label.base) ~ 'RIPARIAN',
                           grepl('WETLAND', label.base) ~ 'WETLANDS',
                           grepl('ORCHARD', label.base) ~ 'ORCHARD',
                           TRUE ~ label.base)) %>%
  group_by(label) %>%
  summarize(change = sum(change), .groups = 'drop') %>%
  arrange(change) %>%
  DeltaMultipleBenefits::plot_change(scale = 1000000) +
  labs(x = NULL, y = bquote(' ' *Delta~ 'total area ('~km^2*')')) +
  theme_bw() + coord_flip() + ylim(-100, 410) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))
ggsave('fig/change_scenario3_floodrisk.png', height = 7.5, width = 6)
# large increase in wetland cover, at the expense of especially idle, corn,
# row&field, orch, vin, alf, grain, grassland, pasture

