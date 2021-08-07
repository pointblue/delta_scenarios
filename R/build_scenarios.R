# Build & evaluate scenarios of landscape change in the Delta

# PACKAGES & FUNCTIONS
source('R/packages.R')

# reference data:
delta = rast('GIS/delta.tif')

# baseline layers:
baseline_rip = rast('data/landcover_riparian/baseline_riparian.tif')
baseline_ripdetail = rast('data/landcover_riparian/baseline_riparian_detail.tif')
baseline_ripperm = rast('data/landcover_riparian/baseline_riparian_permwetland.tif')
baseline_wat_fall = rast('data/landcover_waterbirds_fall/baseline_waterbird_fall.tif')
baseline_wat_win = rast('data/landcover_waterbirds_winter/baseline_waterbird_winter.tif')
baseline_mb = rast('data/landcover_multiplebenefits/baseline_mb.tif')

# keys:
rkey = read_csv('data/landcover_key_riparian.csv')
rkey2 = read_csv('data/landcover_key_riparian_detail.csv')
rkey3 = read_csv('data/landcover_key_riparian_permwetland.csv')
wkey = read_csv('data/landcover_key_waterbirds.csv', col_types = cols())
mkey = read_csv('data/landcover_key_multiplebenefits.csv')


# SCENARIO 1. Restoration--------

## Restoring habitat to meet (some of) the proposed objectives in the draft
## amendment to Chapter 4 of the Delta Plan, specifically for riparian habitat
## and seasonal wetlands. The objectives are less than the total area of
## potential habitat, so distribute potential restored acres by:
##
## a) including only pixels within the Legal Delta boundary that are identified
## as potential habitat by the "restoration opportunities" analysis, and are not
## already wetland or riparian habitat in the baseline veg layer
##
## b) prioritizing pixels in priority restoration areas (as defined by the Delta
## Stewardship Council, i.e. excluding areas with a high risk of becoming
## flooded by sea level rise)
##
## c) within priority restoration area, prioritizing pixels on protected land,
## easements, public/quasi-public land, or open space, and excluding areas
## designated for development and areas that are already urban in the baseline
## veg layer

## restoration objectives------
# from proposed performance measures to the Delta Plan
obj = tibble(type = c('seasonal wetland, wet meadow, nontidal wetland',
                      'willow riparian scrub/shrub, valley foothill riparian, willow thicket'),
             target_acres = c(19000, 16300),
             total_acres = c(24100, 30500)) %>%
  mutate(target_ha = target_acres / 2.47105,
         total_ha = total_acres / 2.47105)


# current totals (from baseline layer)
base = baseline_wat_fall %>%
  mask(delta) %>%
  freq() %>%
  filter(label %in% c('woodw', 'duwet')) %>%
  mutate(current_ha = count * 30 * 30 / 10000)
# wetlands = 6,919.56 ha (consider nontidal wetlands only)
# riparian = 8,216.73 ha

# how much more needed to meet proposed 2050 targets?
targets = full_join(
  obj %>% mutate(label = c('duwet', 'woodw')) %>% select(label, total_ha),
  base %>% select(label, current_ha),
  by = 'label') %>%
  mutate(add_ha = total_ha - current_ha)
# riparian: add 4,126 ha
# wetlands: add 2,833 ha

## planned restoration---------
# - ecorestore (already planned/under way)
# but exclude areas that are already urban or water, or already managed wetlands
# - assume base layer misalignment/double-counting)

ecorestore = rast('GIS/DSLPT_Ecorestore.tif')
# includes addition of tidal wetland (8), seasonal/nontidal wetland (18),
#  riparian (15), water (14), and grassland (15)

# evaluate riparian and nontidal wetland restoration ONLY
# - don't allow conversion to wetland from existing wetland or urban (but note
#    that there may be some conversion from open water or unspecified wetland to
#    managed wetland)
# - don't allow conversion to riparian from existing riparian or water (but note
#    that there may be some conversion of riparian to wetland?)

ecorestore_subset = ecorestore %>%
  subst(from = c(8, 14, 16), to = NA) %>%
  mask(baseline_wat_fall %>% subst(from = c(12, 18), to = NA))
ecorestore_subset2 = lapp(c(ecorestore_subset, baseline_wat_fall),
                          fun = function(x, y) {
                           ifelse(x == 15 & y %in% c('water', 'woodw'), NA, x)
                            })

# check alignment:
tab = crosstab(c(ecorestore_subset2,
                 baseline_wat_fall %>% mask(ecorestore)),
               useNA = TRUE, long = TRUE)
tab %>% rlang::set_names(c('ecorestore', 'baseline', 'n')) %>%
  left_join(wkey %>% select(code, label), by = c('baseline' = 'code')) %>%
  arrange(ecorestore, desc(n))
# Note: some of the new managed wetland is coming from other wetland (?)
#  also pasture, rice, corn, alf, row/field, orch/vin, grassland, grain, fallow
# --> and riparian! (but this seems correct)

base_plus_ecorestore = cover(ecorestore_subset2, baseline_wat_fall)
base_plus = base_plus_ecorestore %>%
  mask(delta) %>%
  freq() %>% as_tibble() %>%
  filter(value %in% c(15, 18)) %>%
  mutate(label = recode(value,
                        '15' = 'woodw', '18' = 'duwet')) %>%
  mutate(ecorestore_ha = count * 30 * 30 / 10000)
targets_plus = full_join(
  targets %>% select(label, total_ha, current_ha),
  base_plus %>% select(label, ecorestore_ha),
  by = 'label') %>%
  mutate(add_ha = total_ha - ecorestore_ha)
# riparian: add 4,016 ha (decreased a bit)
# wetlands: add 1,224 ha (decreased a lot)



## restoration potential--------
# (excluding areas that are already wetland or riparian; also already urban or
# water (assume permanent)
exclude = subst(base_plus_ecorestore, c(8, 12, 14, 15, 18, 19, 20), NA)

restoration_stack = c(rast('GIS/habpotential.tif'), #7100 = valley foothill riparian, 7600 = willow shrub/scrub, 8000 = wetland
                      rast('GIS/restorationpriority.tif'), # 100 = priority
                      rast('GIS/zoning.tif'), #10 = open space/recreation; 20 = public/quasi=public; 90 = designated for development
                      rast('GIS/protectedareas.tif') # 1 = protected, 2 = easement
                      ) %>%
  mask(delta) %>%
  mask(exclude)

# simplify riparian coding
restoration_stack$habpotential = classify(restoration_stack$habpotential,
                                          rcl = matrix(c(7100, 7000,
                                                         7600, 7000),
                                                       byrow = TRUE, ncol = 2))

# sum scores to classify each pixel with multiple pieces of data
restoration_targets = sum(restoration_stack, na.rm = TRUE)

# rank priority restoration locations:
# - prioritize "priority restoration area" and protected land within potential

# group pixels by priority for pixels classified as potential riparian & wetland
restoration_targets_rip = classify(
  restoration_targets,
  rcl = matrix(c(0, 7000, NA,
                 7101, 7103, 1, #priority, protected
                 7111, 7113, 1, #priority, protected, open space
                 7100, 7101, 2, #priority, unprotected
                 7110, 7111, 2, #priority, unprotected, open space
                 7190, 7191, 9, #priority, unprotected, designated for development
                 7001, 7003, 3, #not priority, protected
                 7011, 7013, 3, #not priority, protected, open space
                 7010, 7011, 4, #not priority, unprotected, open space
                 7020, 7021, 4, #not priority, unprotected, public/quasi-public
                 7000, 7001, 5, #not priority, unprotected, not open space
                 7090, 7092, 9, #not priority, designated for development
                 7191, Inf, NA),
               byrow = TRUE, ncol = 3))
crosstab(c(restoration_targets_rip, restoration_stack$habpotential),
         useNA = TRUE, long = TRUE)
# all potential riparian pixels assigned a priority level 1-5, or 9
writeRaster(restoration_targets_rip, 'GIS/restoration_targets_riparian.tif',
            overwrite = TRUE)


restoration_targets_wet = classify(
  restoration_targets,
  rcl = matrix(c(0, 8000, NA,
                 8101, 8103, 1, #priority, protected
                 8111, 8113, 1, #priority, protected, open space
                 8100, 8101, 2, #priority, unprotected
                 8110, 8111, 2, #priority, unprotected, open space
                 8190, 8191, 9, #priority, unprotected, designated for development
                 8001, 8003, 3, #not priority, protected
                 8011, 8013, 3, #not priority, protected, open space
                 8021, 8023, 3, #not priority, protected, public/quasi-public
                 8010, 8011, 4, #not priority, unprotected, open space
                 8020, 8021, 4, #not priority, unprotected, public/quasi-public
                 8000, 8001, 5, #not priority, unprotected
                 8090, 8092, 9, #not priority, designated for development
                 8191, Inf, NA),
               byrow = TRUE, ncol = 3))
crosstab(c(restoration_targets_wet, restoration_stack$habpotential),
         useNA = TRUE, long = TRUE)
# all potential wetland pixels assigned a priority level 1-5, or 9
writeRaster(restoration_targets_wet, 'GIS/restoration_targets_wetlands.tif',
            overwrite = TRUE)

### add riparian restoration-----
# approach: select pixels by priority rankings as above
# - exclude very small patches of potential riparian (minimum 1 acre?)
# - compare remaining total area of each priority level vs. objective
# - when not all of a priority level is needed, randomly select a subset that
#     will meet the restoration objective
# objective: 4,016 ha added to ecorestore for a total of 4126 added to baseline

targets_area_rip = freq(restoration_targets_rip) %>% as_tibble() %>%
  mutate(area.ha = count * 30 * 30 / 10000,
         tot.ha = cumsum(area.ha))
# --> to reach target, will need priority levels 1-4 plus some of level 5

# find patches in priority levels 1-4 of at least 1 acre (0.4 ha):
# (exclude areas designated for development & priority level 5 for now)
rip_targets1 = subst(restoration_targets_rip, c(5, 9), NA) %>%
  # separate by value so patches reflect separate priority levels
  segregate(keep = TRUE, other = NA) %>%
  patches(directions = 8)
rip_targets1_keepID = freq(rip_targets1) %>% as_tibble() %>%
  mutate(area.ha = count * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4) %>%
  pull(value) #303 patches
# classify all included patch IDs as new riparian; rest to NA
rip_targets1_keep = classify(rip_targets1,
                             rcl = tibble(from = rip_targets1_keepID,
                                          to = 15) %>% as.matrix(),
                             othersNA = TRUE) %>%
  sum(na.rm = TRUE) #combine into one layer again

# how much additional area needed from priority level 5?
rip_need = targets_plus %>% filter(label == 'woodw') %>% pull(add_ha) -
  freq(rip_targets1_keep) %>% as_tibble() %>%
  mutate(area.ha = count * 30 * 30 / 10000) %>%
  pull(area.ha) %>% sum()
# 2373.001 ha

rip_targets2 = subst(restoration_targets_rip, c(0:4,9), NA) %>%
  patches(directions = 8)
rip_targets2_consider = freq(rip_targets2) %>% as_tibble() %>%
  mutate(area.ha = count * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4) %>%
  arrange(desc(area.ha)) %>%
  mutate(weight = area.ha / sum(area.ha))
# take random sample of priority level 5 patches > 1 acre
rip_targets2_sampleorder = sample(rip_targets2_consider %>% pull(value),
                                  # weight by size? (leave out for now)
                                  # prob = rip_targets2_consider %>% pull(weight),
                                  nrow(rip_targets2_consider), replace = FALSE)
rip_targets2_considerorder = rip_targets2_consider %>%
  mutate(value = factor(value, levels =  rip_targets2_sampleorder)) %>%
  arrange(value) %>%
  mutate(tot.ha = cumsum(area.ha))
# keep all up until cumulative sum reaches/exceeds target
rip_targets2_keepID = rip_targets2_considerorder %>%
  filter(tot.ha <=
           rip_targets2_considerorder %>% filter(tot.ha > rip_need) %>%
           slice(1) %>% pull(tot.ha)) %>%
  pull(value)
# classify all included patch IDs as riparian; rest to NA
rip_targets2_keep = classify(rip_targets2,
                             rcl = tibble(from = rip_targets2_keepID %>%
                                            as.character() %>% as.numeric(),
                                          to = 15) %>% as.matrix(),
                             othersNA = TRUE)

# COMBINE PLANNED & PROPOSED RIPARIAN RESTORATION
# (convert all other patch IDs to NA)
rip_targets_final = ecorestore_subset2 %>% subst(from = 18, to = NA) %>%
  cover(rip_targets1_keep) %>%
  cover(rip_targets2_keep)
names(rip_targets_final) = 'riparian_restoration'
freq(rip_targets_final) %>% as_tibble() %>% pull(count) * 30 * 30 / 10000
# adding 4161.2 ha (target = 4126)
writeRaster(rip_targets_final,
            'GIS/restoration_targets_riparian_objectives.tif',
            overwrite = TRUE)

### add wetland restoration---------
# same approach as for riparian
# objective: 1224 ha added to ecorestore for a total of 2833 added to baseline

targets_area_wet = freq(restoration_targets_wet) %>% as_tibble() %>%
  mutate(area.ha = count * 30 * 30 / 10000,
         tot.ha = cumsum(area.ha))
# --> to reach target, only need part of priority level 1
wet_need = targets_plus %>% filter(label == 'duwet') %>% pull(add_ha)

# find patches in priority level 1 of at least 1 acre (0.4 ha) and less than the
# total target
wet_targets1 = subst(restoration_targets_wet, c(2:9), NA) %>%
  patches(directions = 8)
wet_targets1_consider = freq(wet_targets1) %>% as_tibble() %>%
  mutate(area.ha = count * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4 & area.ha < 1.25 * wet_need)
# take random sample of patches > 1 acre
wet_targets1_sampleorder = sample(wet_targets1_consider %>% pull(value),
                                 nrow(wet_targets1_consider), replace = FALSE)
wet_targets1_considerorder = wet_targets1_consider %>%
  mutate(value = factor(value, levels =  wet_targets1_sampleorder)) %>%
  arrange(value) %>%
  mutate(tot.ha = cumsum(area.ha))
# keep all up until cumulative sum reaches/exceeds target
wet_targets1_keepID = wet_targets1_considerorder %>%
  filter(tot.ha <= wet_targets1_considerorder %>% filter(tot.ha > wet_need) %>%
           slice(1) %>% pull(tot.ha)
         ) %>%
  pull(value)
# classify all included patch IDs as wetland; rest to NA
wet_targets1_keep = classify(wet_targets1,
                             rcl = tibble(from = wet_targets1_keepID %>%
                                            as.character() %>% as.numeric(),
                                          to = 18) %>% as.matrix(),
                             othersNA = TRUE)

# sufficient?
targets_plus %>% filter(label == 'duwet') %>% pull(add_ha) -
  freq(wet_targets1_keep) %>% as_tibble() %>%
  mutate(area.ha = count * 30 * 30 / 10000) %>%
  pull(area.ha) %>% sum()
# -23 ha

# wet_targets2 = subst(restoration_targets_wet, c(1, 3:9), NA) %>%
#   patches(directions = 8)
# wet_targets2_consider = freq(wet_targets2) %>% as_tibble() %>%
#   mutate(area.ha = count * 30 * 30 / 10000) %>%
#   filter(area.ha >= 0.4 & area.ha < 1.25 * wet_need)
# # take random sample of patches > 1 acre
# wet_targets2_sampleorder = sample(wet_targets2_consider %>% pull(value),
#                                   nrow(wet_targets2_consider), replace = FALSE)
# wet_targets2_considerorder = wet_targets2_consider %>%
#   mutate(value = factor(value, levels =  wet_targets2_sampleorder)) %>%
#   arrange(value) %>%
#   mutate(tot.ha = cumsum(area.ha))
# # keep all up until cumulative sum reaches/exceeds target
# wet_targets2_keepID = wet_targets2_considerorder %>%
#   filter(tot.ha <= wet_targets2_considerorder %>% filter(tot.ha > wet_need) %>%
#            slice(1) %>% pull(tot.ha)
#          ) %>%
#   pull(value)
# # classify all included patch IDs as wetland; rest to NA
# wet_targets2_keep = classify(wet_targets2,
#                              rcl = tibble(from = wet_targets2_keepID %>%
#                                             as.character() %>% as.numeric(),
#                                           to = 18) %>% as.matrix(),
#                              othersNA = TRUE)

# COMBINE PLANNED & POTENTIAL WETLAND RESTORATION
# (convert all other patch IDs to NA)
wet_targets_final = ecorestore_subset2 %>% subst(from = 15, to = NA) %>%
  cover(wet_targets1_keep)
names(wet_targets_final) = 'wetland_restoration'
freq(wet_targets_final) %>% as_tibble() %>% pull(count) * 30 * 30 / 10000
# adding 2856.33 ha (compared to 2833 target)
plot(wet_targets_final)
writeRaster(wet_targets_final,
            'GIS/restoration_targets_wetland_objectives.tif',
            overwrite = TRUE)


## overlay on baseline layers------------

### waterbirds-fall----------
scenario_restoration = cover(rip_targets_final, wet_targets_final) %>%
  cover(baseline_wat_fall)
levels(scenario_restoration) <- wkey %>% select(id = code, shortlab) %>%
  as.data.frame()
coltab(scenario_restoration) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_restoration) = 'scenario_restoration'
plot(c(baseline_wat_fall, scenario_restoration))
writeRaster(scenario_restoration,
            'data/landcover_waterbirds_fall/scenario1_restoration_waterbird_fall.tif',
            overwrite = TRUE)

# calculate change in area of each land cover
delta_restoration = DeltaMultipleBenefits::calculate_change(
  baseline = baseline_wat_fall %>% mask(delta),
  scenario = scenario_restoration %>% mask(delta))
delta_restoration %>%
  left_join(wkey %>% select(label.base = shortlab, label), by = 'label.base') %>%
  group_by(label) %>%
  summarize(change = sum(change), .groups = 'drop') %>%
  arrange(change) %>%
  DeltaMultipleBenefits::plot_change(scale = 1000000) +
  labs(x = NULL, y = bquote(' ' *Delta~ 'total area ('~km^2*')')) +
  theme_bw() + coord_flip() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))
ggsave('fig/delta_restoration.png', height = 7.5, width = 6)
# large increase in riparian and wetland cover, mostly at the expense of
# orchard/vineyard

# compare/check conversions between layers
tab = crosstab(c(baseline_wat_fall %>% mask(delta),
                 scenario_restoration %>% mask(delta)),
               useNA = TRUE, long = TRUE)

tab %>% rlang::set_names(c('baseline', 'scenario', 'n')) %>%
  mutate_at(vars(baseline:scenario), as.numeric) %>%
  left_join(wkey %>% dplyr::select(code, shortlab), by = c('baseline' = 'code')) %>%
  left_join(wkey %>% dplyr::select(code, shortlab), by = c('scenario' = 'code')) %>%
  arrange(desc(n)) %>%
  group_by(shortlab.y) %>%
  mutate(tot = sum(n),
         prop = n/tot) %>%
  ungroup() %>%
  filter(shortlab.y == 'duwet')
  filter(shortlab.y == 'woodw')

# - riparian: 66% existing riparian; most conversion from orch (17%);
#    no conversion from rice, duwet, dev, water
# - wetland: 71% existing duwet; most conversion from ip (10%), rice (5%);
#    no conversion from dev, forest, barren

### waterbirds-winter--------
scenario_restoration_win = cover(rip_targets_final, wet_targets_final) %>%
  cover(baseline_wat_win)
levels(scenario_restoration_win) <- wkey %>% select(id = code, shortlab) %>%
  as.data.frame()
coltab(scenario_restoration_win) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_restoration_win) = 'scenario_restoration'
plot(c(baseline_wat_win, scenario_restoration_win))
writeRaster(scenario_restoration_win,
            'data/landcover_waterbirds_winter/scenario1_restoration_waterbird_winter.tif')

### riparian----------
scenario_restoration_rip <- DeltaMultipleBenefits::reclassify_ripmodels(scenario_restoration)
levels(scenario_restoration_rip) <- rkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(scenario_restoration_rip) <- rkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_restoration_rip) = 'scenario_restoration'
plot(c(baseline_rip, scenario_restoration_rip))
writeRaster(scenario_restoration_rip,
            'data/landcover_riparian/scenario1_restoration_riparian.tif')

# compare/check conversions between layers
tab = crosstab(c(baseline_rip %>% mask(delta),
                 scenario_restoration_rip %>% mask(delta)),
               useNA = TRUE, long = TRUE)

tab %>% set_names(c('baseline', 'scenario', 'n')) %>%
  mutate_at(vars(baseline:scenario), as.numeric) %>%
  left_join(rkey %>% dplyr::select(code, group), by = c('baseline' = 'code')) %>%
  left_join(rkey %>% dplyr::select(code, group), by = c('scenario' = 'code')) %>%
  arrange(desc(n)) %>%
  group_by(group.y) %>%
  mutate(tot = sum(n),
         prop = n/tot) %>%
  ungroup() %>%
  # filter(group.y == 'WETLAND')
  filter(group.y == 'RIPARIAN')

# - riparian: 66% existing riparian; most conversion from ORCHVIN (16%);
#    no conversion from RICE, URBAN, or WATER
# - wetland: 91% existing WETLAND; most conversion from GRASSPAS (6%);
#    no conversion from URBAN, OAKWOODLAND, BARREN

### riparian detail---------
# use veg types from restoration opportunities analysis to inform riparian
# sub-types; pixels not in restoration opportunities analysis likely from
# ecorestore = 'valley foothill riparian'

rip_targets_type = lapp(c(rast('GIS/habpotential.tif') %>% mask(rip_targets_final),
                          rip_targets_final),
                        fun = function(x, y) {
                          ifelse(y == 15 & is.na(x), 7100, x)
                        })
# 7100 = undefined "valley foothill riparian"
# 7600 = SALIXSHRUB ("willow shrub/scrub")
plot(rip_targets_type)
crosstab(c(rip_targets_final, rip_targets_type), useNA = TRUE, long = TRUE)

# ASSIGN FOREST TYPES
# keep only pixels that could be forest
rip_targets_forestpts = rip_targets_type %>% subst(from = 7600, to = NA) %>%
  as.points()
# 41,979 points

# assign the most frequent ripforest value from surrounding area in baseline
# landscape
rip_targets_forest_buff1000 = buffer(rip_targets_forestpts, width = 1000)
baseline_ripforest = subst(baseline_ripdetail, from = c(75:78), to = NA)
ripforest_assignments = extract(baseline_ripforest, rip_targets_forest_buff1000,
                                method = 'simple', fun = modal, na.rm = TRUE,
                                ties = 'random')
values(rip_targets_forestpts) <- ripforest_assignments$baseline_ripdetail
ripforest = rasterize(rip_targets_forestpts, scenario_restoration_rip,
                      field = 'value')
plot(ripforest)
#817 cells still unassigned (not bad out of 41979)

# repeat for remaining unassigned pixels
rip_targets_forestpts2 = rip_targets_type %>% subst(from = 7600, to = NA) %>%
  mask(ripforest, inverse = TRUE) %>% as.points() #remaining 817 points
rip_targets_forest_buff2000 = rip_targets_forestpts2 %>%
  buffer(width = 2000)
ripforest_assignments2 = extract(baseline_ripforest, rip_targets_forest_buff2000,
                                method = 'simple', fun = modal, na.rm = TRUE,
                                ties = 'random')
values(rip_targets_forestpts2) <- ripforest_assignments2$baseline_ripdetail
ripforest2 = rasterize(rip_targets_forestpts2, scenario_restoration_rip,
                       field = 'value')

# ASSIGN SHRUBS
rip_targets_shrubpts = rip_targets_type %>% subst(from = 7100, to = NA) %>%
  as.points()
# 4,381 points
# assign all to 76 (SALIXSHRUB) - by far the most frequent ripshrub
values(rip_targets_shrubpts) <- 76
ripshrub = rasterize(rip_targets_shrubpts, scenario_restoration_rip,
                      field = 'value')
plot(ripshrub)

# COMBINE
scenario_restoration_ripdetail = cover(ripforest, ripforest2) %>%
  cover(ripshrub) %>%
  cover(baseline_ripdetail) # original unchanged riparian detail
levels(scenario_restoration_ripdetail) <- rkey2 %>% select(ID = code, label) %>%
  as.data.frame()
coltab(scenario_restoration_ripdetail) <- rkey2 %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
plot(scenario_restoration_ripdetail)
writeRaster(scenario_restoration_ripdetail,
            'data/landcover_riparian/scenario1_restoration_ripdetail.tif')

# all restoration targets assigned a rip detail
crosstab(c(rip_targets_final, scenario_restoration_ripdetail),
         useNA = TRUE, long = TRUE)

# all riparian pixels assigned a ripdetail
crosstab(c(scenario_restoration_rip, scenario_restoration_ripdetail),
         useNA = TRUE, long = TRUE)


### riparian perm wetlands--------
# assume all restored wetlands are seasonal?
plot(baseline_ripperm)
crosstab(c(baseline_ripperm, wet_targets_final), useNA = TRUE, long = TRUE)
# nothing in common - no changes needed


### multiple benefits--------
scenario_restoration_mb <- DeltaMultipleBenefits::reclassify_multiplebenefits(scenario_restoration)
levels(scenario_restoration_mb) <- mkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(scenario_restoration_mb) <- mkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_restoration_mb) = 'scenario_restoration'
writeRaster(scenario_restoration_mb,
            'data/landcover_multiplebenefits/scenario1_restoration_mb.tif')


# SCENARIO 2. Perennial crop expansion---------
# based on the Wilson et al. (2021) BBAU ("bad business as usual") scenario for
# the year 2100, including historically high rates of perennial crop expansion;
# no restoration

skey = readr::read_csv('GIS/State Class Rasters/key.csv', show_col_types = FALSE)

bbau = terra::rast('GIS/State Class Rasters/scn421.sc.it1.ts2100.tif') %>%
  terra::project(baseline_wat_fall, method = 'near') %>%
  terra::mask(baseline_wat_fall) %>%
  # keep only the footprint of expanded perennial crops (code=20) - reclassify
  # to waterbird code for orch (9); all others convert to NA
  terra::classify(rcl = matrix(c(20, 9), byrow = TRUE, ncol = 2),
                  othersNA = TRUE)
# note: missing southwest corner of Delta

# allow perennial crop footprint from bbau scenario to cover baseline footprint,
# except where baseline is a land cover that shouldn't change to orchard:
#   dev (12), water (14), woodw (15), wetland (8), duwet (18)
# (all else unchanged)

### waterbirds-fall--------------
scenario_perex = baseline_wat_fall %>%
  subst(from = c(1:7, 9:11, 13, 16:17, 99), to = NA) %>% #create layer of "permanent" land covers
  cover(cover(bbau, baseline_wat_fall)) # fill in with an overlay of perennial expansion on top of full baseline
levels(scenario_perex) <- wkey %>% select(id = code, shortlab) %>%
  as.data.frame()
coltab(scenario_perex) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_perex) = 'scenario_perennial_expansion'
plot(c(baseline_wat_fall, scenario_perex))
writeRaster(scenario_perex,
            'data/landcover_waterbirds_fall/scenario2_perennialexpand_waterbird_fall.tif',
            overwrite = TRUE)

# calculate change in area of each land cover
delta_perex = DeltaMultipleBenefits::calculate_change(
  baseline = baseline_wat_fall %>% mask(delta),
  scenario = scenario_perex %>% mask(delta))
delta_perex %>%
  left_join(wkey %>% select(label.base = shortlab, label), by = 'label.base') %>%
  group_by(label) %>%
  summarize(change = sum(change), .groups = 'drop') %>%
  arrange(change) %>%
  DeltaMultipleBenefits::plot_change(scale = 1000000) +
  labs(x = NULL, y = bquote(' ' *Delta~ 'total area ('~km^2*')')) +
  theme_bw() + coord_flip() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))
ggsave('fig/delta_perennialexpand.png', height = 7.5, width = 6)
# large increase in orchard cover, at the expense of most others, especially
# row, alfalfa, corn, fallow

# compare
tab = crosstab(c(baseline_wat_fall, scenario_perex), useNA = TRUE, long = TRUE)
tab %>% rlang::set_names(c('baseline', 'bbau', 'n')) %>%
  mutate_at(vars(baseline:bbau), as.numeric) %>%
  left_join(wkey %>% dplyr::select(code, baseline_landuse = label),
            by = c('baseline' = 'code')) %>%
  left_join(wkey %>% dplyr::select(code, bbau_landuse = label),
            by = c('bbau' = 'code')) %>%
  group_by(bbau_landuse) %>%
  mutate(tot = sum(n),
         prop = n/tot) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  filter(baseline_landuse != bbau_landuse)
# all changes are conversions to orchard/vineyard - as expected!

### waterbirds-winter-------------
scenario_perex_win = baseline_wat_win %>%
  subst(from = c(1:7, 9:11, 13, 16:17, 99), to = NA) %>% #create layer of "permanent" land covers
  cover(cover(bbau, baseline_wat_win))
levels(scenario_perex_win) <- wkey %>% select(id = code, shortlab) %>%
  as.data.frame()
coltab(scenario_perex_win) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_perex_win) = 'scenario_perennial_expansion'
plot(c(baseline_wat_win, scenario_perex_win))
writeRaster(scenario_perex_win,
            'data/landcover_waterbirds_winter/scenario2_perennialexpand_waterbird_winter.tif')


### riparian-------
scenario_perex_rip <- DeltaMultipleBenefits::reclassify_ripmodels(scenario_perex)
levels(scenario_perex_rip) <- rkey %>% select(id = code, label = group) %>%
  as.data.frame()
coltab(scenario_perex_rip) <- rkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_perex_rip) = 'scenario_perennial_expansion'
plot(c(baseline_rip, scenario_perex_rip))
writeRaster(scenario_perex_rip,
            'data/landcover_riparian/scenario2_perennialexpand_riparian.tif')

### riparian detail------
# should be no change
crosstab(c(scenario_perex_rip %>% subst(from = c(20:130), to = NA),
           baseline_ripdetail),
         useNA = TRUE, long = TRUE)
# no pixels in common - no change from baseline

crosstab(c(scenario_perex_rip %>% subst(from = c(20:130), to = NA),
           baseline_ripperm),
         useNA = TRUE, long = TRUE)
# no pixels in common - no change from baseline

### multiple benefits-------
scenario_perex_mb <- DeltaMultipleBenefits::reclassify_multiplebenefits(scenario_perex)
levels(scenario_perex_mb) <- mkey %>% select(id = code, label = group) %>%
  as.data.frame()
coltab(scenario_perex_mb) <- mkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_perex_mb) = 'scenario_perennial_expansion'
plot(c(baseline_mb, scenario_perex_mb))
writeRaster(scenario_perex_mb,
            'data/landcover_multiplebenefits/scenario2_perennialexpand_mb.tif')


# SCENARIO 3. Flood risk-----------
# scenario assumption: all "very high" flood risk areas become wetlands, and
# all perennial crops move out of "high" and "medium" risk areas

maxfloodrisk = rast('GIS/floodrisk2085.tif')
plot(maxfloodrisk)

## very high risk------
veryhigh = classify(maxfloodrisk,
                    rcl = matrix(c(4, 18), byrow = TRUE, ncol = 2),
                    othersNA = TRUE) %>%
  mask(delta) #exclude some extra areas in Suisun
plot(veryhigh)

# except pixels that are already water, riparian, urban, tidal wetland
exclude = classify(baseline_wat_fall,
                   rcl = matrix(c(12, 12,
                                  14, 14,
                                  15, 15,
                                  19, 19,
                                  20, 20),
                                byrow = TRUE, ncol = 2),
                   othersNA = TRUE)

# overlay on baseline
veryhigh_wetlands = cover(exclude,
                          cover(veryhigh, baseline_wat_fall))
veryhigh_wetlands_win = cover(exclude,
                             cover(veryhigh, baseline_wat_win))

# calculate change in area of each land cover (separate for fall and winter)
delta_veryhigh = DeltaMultipleBenefits::calculate_change(
  baseline = baseline_wat_fall,
  scenario = veryhigh_wetlands)
# lots of corn, alf, row lost (plus orch); zero wet, dev, water, woodw

delta_veryhigh_win = DeltaMultipleBenefits::calculate_change(
  baseline = baseline_wat_win,
  scenario = veryhigh_wetlands)
# lots of grain, wheat, corn lost (plus orch); zero wet, dev, water, woodw


## high & medium flood risk-----

# identify contiguous patches of orchard pixels in the high and medium flood
# risk areas
highmed = subst(maxfloodrisk, c(0:1,4), NA)
highmed_orch_patches = mask(baseline_wat_fall, highmed) %>%
  subst(c(0:8,10:99), NA) %>%
  patches()
plot(highmed_orch_patches)

# map each contiguous patch of orchard to a new randomly selected crop, weighted
# by the proportion lost to wetlands in the "very high" risk islands:
prop_sample = delta_veryhigh %>%
  select(value, label, change) %>%
  filter(change < 0 & !label %in% c('orch', 'forest', 'fal', 'barren', 'rice')) %>%
  mutate(change = abs(change),
         prop = change / sum(change))
# mostly corn
# --> a tiny proportion was rice; exclude because requires specific soil
# properties

new_crops = tibble(patchID = c(1:minmax(highmed_orch_patches)[2]),
                   newID = sample(prop_sample$value,
                                  minmax(highmed_orch_patches)[2],
                                  replace = TRUE,
                                  prob = prop_sample$prop))
replace_orchard = classify(highmed_orch_patches,
                           rcl = as.matrix(new_crops))
#Note: a lot of the orchard stringers (along roads?) get converted to corn,
#which is maybe a bit odd...

# repeat for winter baseline
prop_sample_win = delta_veryhigh_win %>%
  select(value, label, change) %>%
  filter(change < 0 & !label %in% c('orch', 'forest', 'fal', 'barren', 'rice')) %>%
  mutate(change = abs(change),
         prop = change / sum(change))
# mostly grain

new_crops_win = tibble(patchID = c(1:minmax(highmed_orch_patches)[2]),
                       newID = sample(prop_sample_win$value,
                                      minmax(highmed_orch_patches)[2],
                                      replace = TRUE,
                                      prob = prop_sample_win$prop))
replace_orchard_win = classify(highmed_orch_patches,
                               rcl = as.matrix(new_crops_win))


## overlay on baseline------

### waterbirds-fall---------
scenario_floodrisk = cover(replace_orchard, veryhigh_wetlands)
levels(scenario_floodrisk) <- wkey %>% select(id = code, shortlab) %>%
  as.data.frame()
coltab(scenario_floodrisk) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_floodrisk) = 'scenario_floodrisk'
plot(c(baseline_wat_fall, scenario_floodrisk))
writeRaster(scenario_floodrisk,
            'data/landcover_waterbirds_fall/scenario3_floodrisk_waterbird_fall.tif',
            overwrite = TRUE)

# calculate change in area of each land cover
delta_floodrisk = DeltaMultipleBenefits::calculate_change(
  baseline = baseline_wat_fall %>% mask(delta),
  scenario = scenario_floodrisk %>% mask(delta))
delta_floodrisk %>%
  left_join(wkey %>% select(label.base = shortlab, label), by = 'label.base') %>%
  group_by(label) %>%
  summarize(change = sum(change), .groups = 'drop') %>%
  arrange(change) %>%
  DeltaMultipleBenefits::plot_change(scale = 1000000) +
  labs(x = NULL, y = bquote(' ' *Delta~ 'total area ('~km^2*')')) +
  theme_bw() + coord_flip() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))
ggsave('fig/delta_floodrisk.png', height = 7.5, width = 6)
# large increase in wetland cover, at the expense of especially corn, orch, alf;
# no change in water, urban, riparian

### waterbirds-winter---------
scenario_floodrisk_win = cover(replace_orchard_win, veryhigh_wetlands)
levels(scenario_floodrisk_win) <- wkey %>% select(id = code, shortlab) %>%
  as.data.frame()
coltab(scenario_floodrisk_win) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_floodrisk_win) = 'scenario_floodrisk'
plot(c(baseline_wat_win, scenario_floodrisk_win))
writeRaster(scenario_floodrisk_win,
            'data/landcover_waterbirds_winter/scenario3_floodrisk_waterbird_winter.tif')

# compare to fall
tab = crosstab(c(scenario_floodrisk, scenario_floodrisk_win))
tab %>% as_tibble() %>%
  set_names(c('fall', 'winter', 'n')) %>%
  mutate_at(vars(fall:winter), as.numeric) %>%
  left_join(wkey %>% dplyr::select(code, fall_lc = label),
            by = c('fall' = 'code')) %>%
  left_join(wkey %>% dplyr::select(code, winter_lc = label),
            by = c('winter' = 'code')) %>%
  group_by(winter_lc) %>%
  mutate(tot = sum(n),
         prop = n/tot) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  filter(fall_lc != winter_lc) %>%
  ggplot(aes(fall_lc, winter_lc, fill = n)) + geom_tile()
# conversions mostly from corn to grain, ip, row/field
#   from dryp to ip, grain
#   from alf to corn, grain

### riparian------
scenario_floodrisk_riparian <- DeltaMultipleBenefits::reclassify_ripmodels(scenario_floodrisk)
levels(scenario_floodrisk_riparian) <- rkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(scenario_floodrisk_riparian) <- rkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_floodrisk_riparian) = 'scenario_floodrisk'
plot(c(baseline_rip, scenario_floodrisk_riparian))
writeRaster(scenario_floodrisk_riparian,
            'data/landcover_riparian/scenario3_floodrisk_riparian.tif')

### riparIan detail------
# should be no change
crosstab(c(scenario_floodrisk_riparian %>% subst(from = c(10:70,90:130), to = NA),
           baseline_ripdetail),
         useNA = TRUE, long = TRUE)
# no pixels in common - no change from baseline

crosstab(c(scenario_floodrisk_riparian %>% subst(from = c(10:70,90:130), to = NA),
           baseline_ripperm),
         useNA = TRUE, long = TRUE)
# assume all new wetlands are seasonal wetlands - no change from baseline

### multiple benefits---------
scenario_floodrisk_mb <- DeltaMultipleBenefits::reclassify_multiplebenefits(scenario_floodrisk)
levels(scenario_floodrisk_mb) <- mkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(scenario_floodrisk_mb) <- mkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_floodrisk_mb) = 'scenario_floodrisk'
plot(c(baseline_mb, scenario_floodrisk_mb))
writeRaster(scenario_floodrisk_mb,
            'data/landcover_multiplebenefits/scenario3_floodrisk_mb.tif')


# INTERACTIVE MAP-----------

mapstack = paste0('data/landcover_waterbirds_fall/',
                  list.files('data/landcover_waterbirds_fall/', '.tif$')) %>%
  rast()

# # consider resampling rasters to prevent the file size from being too enormous (this is
# # still fairly hi-res)
# scenarios_sampled_hi = pred_cv %>%
#   terra::spatSample(size = 5000000, as.raster = TRUE)

cl <- colors()
cl[grep('blue', cl)]
scales::show_col(cl[grep('blue', cl)])

wkey2 = wkey %>%
  mutate(label = factor(label, levels = c('alfalfa', 'corn', 'rice', 'grain',
                                          'row/field crop','orchard/vineyard',
                                          'pasture', 'grassland', 'water',
                                          'wetland', 'managed wetland',
                                          'riparian', 'forest/shrub',
                                          'fallow', 'barren', 'urban')))
scales::show_col(wkey2$col)

testmap = DeltaMultipleBenefits::map_scenarios(
  rast = mapstack,
  key = wkey %>% select(value = code, col, label))

## add delta boundary
delta_poly = read_sf('V:/Data/geopolitical/california/Legal_Delta_Boundary/Legal_Delta_Boundary.shp') %>%
  st_transform(crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
testmap <- testmap %>%
  leaflet::addPolygons(data = delta_poly, fill = FALSE, color = 'black',
              weight = 2, opacity = 1)

## save the interactive map as an html widget
# --> you may want to change this to "selfcontained = TRUE", which would allow
# you to email the html file to anyone to view -- the file size just might be super large!

htmlwidgets::saveWidget(testmap,
                        'docs/draft_scenarios.html',
                        selfcontained = FALSE, libdir = 'lib',
                        title = 'Draft scenarios of landcover change in the Delta')

# # try alternative:
# m1 = DeltaMultipleBenefits::map_scenarios(
#   rast = mapstack$baseline,
#   key = wkey %>% select(value = code, col, label)) %>%
#   leaflet::addPolygons(data = delta_poly, fill = FALSE, color = 'black',
#                        weight = 2, opacity = 1)
# m2 = DeltaMultipleBenefits::map_scenarios(
#   rast = mapstack[[2:4]],
#   key = wkey %>% select(value = code, col, label)) %>%
#   leaflet::addPolygons(data = delta_poly, fill = FALSE, color = 'black',
#                        weight = 2, opacity = 1)
# testsyncmap = leafsync::sync(m1, m2, ncol = 2)
#
# htmlwidgets::saveWidget(testsyncmap,
#                         'docs/draft_scenarios.html',
#                         selfcontained = FALSE, libdir = 'lib',
#                         title = 'Draft scenarios of landcover change in the Delta')
