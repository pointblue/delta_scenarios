# Build & evaluate scenarios of landscape change in the Delta

# PACKAGES & FUNCTIONS
source('R/packages.R')

# reference data:
delta = rast('GIS/delta.tif')

# baseline layers:
baseline_rip = rast('data/landcover_riparian/baseline_riparian.tif')
baseline_wat_fall = rast('data/landcover_waterbirds_fall/baseline_waterbird_fall.tif')
baseline_wat_win = rast('data/landcover_waterbirds_winter/baseline_waterbird_winter.tif')
baseline_mb = rast('data/landcover_multiplebenefits/baseline_mb.tif')

# keys:
rkey = read_csv('data/landcover_key_riparian.csv')
wkey = read_csv('data/landcover_key_waterbirds.csv')
mkey = read_csv('data/landcover_key_multiplebenefits.csv')


# SCENARIO 1. Restoration--------

## Restoring habitat to meet (some of) the proposed objectives in the draft
## amendment to Chapter 4 of the Delta Plan, specifically for riparian habitat
## and seasonal wetlands. The objectives are less than the total area of
## potential habitat, so distribute potential restored acres by prioritizing:
## a) those identified as potential habitat by the "restoration opportunities"
##    analysis
## b) those on public land/easements and/or in priority restoration areas (as
##    defined by the Delta Stewardship Council, i.e. excluding areas with a high
##    risk of becoming flooded by sea level rise), and
## c) avoiding areas designated for development
## d) within Legal Delta boundary?
## e) not already wetland or riparian habitat

# Additional ideas/feedback:
# - prioritize restoration near Zonation hotspots
# - incorporate specific plans/projects underway for Sherman & Twichell Island
# --> incorporate EcoRestore into restoration scenario (if not already shown)
# - consider alternative restoration scenario that is more focused on subsided
# islands (i.e. what we can get, short-term value)

## restoration objectives------
# from proposed performance measures to the Delta Plan
obj = tibble(type = c('seasonal wetland, wet meadow, nontidal wetland',
                      'willow riparian scrub/shrub, valley foothill riparian, willow thicket'),
             target_acres = c(19000, 16300),
             total_acres = c(24100, 30500)) %>%
  mutate(target_ha = target_acres / 2.47105,
         total_ha = total_acres / 2.47105)

# (Note: DWR conservation plan goals for entire Lower Sac River and Lower San
# Joaquin River conservation planning areas combined are less than the goals for
# just the riparian and wetland habitat in the Delta)

# current totals (from baseline layer)
base = veg_baseline_waterbird_fall %>%
  mask(delta) %>%
  freq() %>%
  filter(label %in% c('woodw', 'duwet')) %>%
  mutate(current_ha = count * 30 * 30 / 10000)
# wetlands = 7,756 ha (consider managed wetlands only)
# riparian = 8,019 ha

targets = full_join(
  obj %>% mutate(label = c('duwet', 'woodw')) %>% select(label, total_ha),
  base %>% select(label, current_ha),
  by = 'label') %>%
  mutate(add_ha = total_ha - current_ha)
# riparian: 4,587 ha
# wetlands: 1,733 ha

## planned restoration---------
# - ecorestore (already planned/under way)
# but exclude areas that are already urban or water, or already managed wetlands
# - assume base layer misalignment/double-counting)

exclude = subst(veg_baseline_waterbird_fall, c(12, 14, 18), NA)
ecorestore = rast('GIS/DSLPT_Ecorestore.tif') %>% mask(exclude)
tab = crosstab(c(ecorestore,
                 veg_baseline_waterbird_fall %>% mask(ecorestore)))
tab %>% as_tibble() %>% arrange(DSLPT_Ecorestore) %>%
  filter(DSLPT_Ecorestore == 18)
# Note: some of the new managed wetland is coming from other wetland (?)

base_plus_ecorestore = cover(ecorestore, veg_baseline_waterbird_fall)
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
# riparian: 4,616 ha (increased)
# wetlands: 158 ha (decreased)



## restoration potential--------
# (excluding areas that are already wetland or riparian; also already urban or
# water (assume permanent)
exclude2 = subst(base_plus_ecorestore, c(8, 12, 14, 15, 18), NA)

restoration_stack = c(rast('GIS/habpotential.tif'), #7000 = riparian, 8000 = wetland
                      rast('GIS/restorationpriority.tif'), # 100 = priority
                      rast('GIS/zoning.tif'), #10 = open space/recreation; 20 = public/quasi=public; 90 = designated for development
                      rast('GIS/protectedareas.tif') # 1 = protected, 2 = easement
                      ) %>%
  mask(delta) %>%
  mask(exclude2)

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
                 7001, 7003, 3, #not priority, protected
                 7011, 7013, 3, #not priority, protected, open space
                 7010, 7011, 4, #not priority, unprotected, open space
                 7020, 7021, 4, #not priority, unprotected, public/quasi-public
                 7000, 7001, 5, #not priority, unprotected, not open space
                 7090, 7092, 9, #not priority, designated for development
                 7190, Inf, NA),
               byrow = TRUE, ncol = 3))
writeRaster(restoration_targets_rip, 'GIS/restoration_targets_riparian.tif')

restoration_targets_wet = classify(
  restoration_targets,
  rcl = matrix(c(0, 8000, NA,
                 8101, 8103, 1, #priority, protected
                 8111, 8113, 1, #priority, protected, open space
                 8100, 8101, 2, #priority, unprotected
                 8110, 8111, 2, #priority, unprotected, open space
                 8001, 8003, 3, #not priority, protected
                 8011, 8013, 3, #not priority, protected, open space
                 8021, 8023, 3, #not priority, protected, public/quasi-public
                 8010, 8011, 4, #not priority, unprotected, open space
                 8020, 8021, 4, #not priority, unprotected, public/quasi-public
                 8000, 8001, 5, #not priority, unprotected
                 8090, 8092, 9, #not priority, designated for development
                 8190, Inf, NA),
               byrow = TRUE, ncol = 3))
writeRaster(restoration_targets_wet, 'GIS/restoration_targets_wetlands.tif')

## ASSUMPTIONS/QUESTIONS:
## - minimum size of restoration to consider (is 1 acre reasonable?)
## - feasibility of restoration on protected land, easements, public land?
##   (and does it matter for this hypothetical?)
## - more of a priority to focus restoration in "priority restoration areas" or
##    protected land? [relevant mainly to wetlands]

### add riparian restoration-----
# approach: select pixels by priority rankings as above
# - exclude very small patches of potential riparian (minimum 1 acre?)
# - compare remaining total area of each priority level vs. objective
# - when not all of a priority level is needed, randomly select a subset that
#     will meet the restoration objective
# objective: 4616 ha added

targets_area_rip = freq(restoration_targets_rip) %>% as_tibble() %>%
  mutate(area.ha = count * 30 * 30 / 10000,
         tot.ha = cumsum(area.ha))
# --> to reach target, will need priority levels 1-4 plus some of level 5

# find patches in priority levels 1-4 of at least 1 acre (0.4 ha):
# (exclude areas designated for development & priority level 5 for now)
rip_targets1 = subst(restoration_targets_rip, c(5, 9), NA) %>%
  patches(directions = 8)
rip_targets1_keepID = freq(rip_targets1) %>% as_tibble() %>%
  mutate(area.ha = count * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4) %>%
  pull(value)
# classify all included patch IDs as riparian; rest to NA
rip_targets1_keep = classify(rip_targets1,
                             rcl = tibble(from = rip_targets1_keepID,
                                          to = 15) %>% as.matrix(),
                             othersNA = TRUE)

# how much additional area needed from priority level 5?
rip_need = targets_plus %>% filter(label == 'woodw') %>% pull(add_ha) -
  freq(rip_targets1_keep) %>% as_tibble() %>%
  mutate(area.ha = count * 30 * 30 / 10000) %>%
  pull(area.ha) %>% sum()
# 2990.131 ha

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
rip_targets_final = ecorestore %>% subst(c(0:14,16:18), NA) %>%
  cover(rip_targets1_keep) %>%
  cover(rip_targets2_keep)
names(rip_targets_final) = 'riparian_restoration'
freq(rip_targets_final) %>% as_tibble() %>% pull(count) * 30 * 30 / 10000
# adding 4824.36 ha (target = 4616)
writeRaster(rip_targets_final,
            'GIS/restoration_targets_riparian_objectives.tif')

### add wetland restoration---------
# same approach as for riparian
# objective: 158 ha added

targets_area_wet = freq(restoration_targets_wet) %>% as_tibble() %>%
  mutate(area.ha = count * 30 * 30 / 10000,
         tot.ha = cumsum(area.ha))
# --> to reach target, only need part of priority level 1

# find patches in priority level 1 of at least 1 acre (0.4 ha) and less than the
# total target
wet_targets1 = subst(restoration_targets_wet, c(2:9), NA) %>%
  patches(directions = 8)
wet_targets1_consider = freq(wet_targets1) %>% as_tibble() %>%
  mutate(area.ha = count * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4 &
           area.ha < targets_plus %>% filter(label == 'duwet') %>% pull(add_ha))
# take random sample of patches > 1 acre
wet_targets1_sampleorder = sample(wet_targets1_consider %>% pull(value),
                                  nrow(wet_targets1_consider), replace = FALSE)
wet_targets1_considerorder = wet_targets1_consider %>%
  mutate(value = factor(value, levels =  wet_targets1_sampleorder)) %>%
  arrange(value) %>%
  mutate(tot.ha = cumsum(area.ha))
# keep all up until cumulative sum reaches/exceeds target
wet_targets1_keepID = wet_targets1_considerorder %>%
  filter(tot.ha <= wet_targets1_considerorder %>%
           filter(tot.ha >
                    targets_plus %>% filter(label == 'duwet') %>% pull(add_ha)) %>%
           slice(1) %>% pull(tot.ha)) %>%
  pull(value)
# classify all included patch IDs as wetland; rest to NA
wet_targets1_keep = classify(wet_targets1,
                             rcl = tibble(from = wet_targets1_keepID %>%
                                            as.character() %>% as.numeric(),
                                          to = 18) %>% as.matrix(),
                             othersNA = TRUE)

# COMBINE PLANNED & POTENTIAL WETLAND RESTORATION
# (convert all other patch IDs to NA)
wet_targets_final = ecorestore %>% subst(c(0:17), NA) %>%
  cover(wet_targets1_keep)
names(wet_targets_final) = 'wetland_restoration'
freq(wet_targets_final) %>% as_tibble() %>% pull(count) * 30 * 30 / 10000
# adding 1754.1 ha (compared to 1733 target)
writeRaster(wet_targets_final,
            'GIS/restoration_targets_wetland_objectives.tif')


## overlay on baseline layers------------

### waterbirds-fall----------
scenario_restoration = cover(rip_targets_final, wet_targets_final) %>%
  cover(baseline_wat_fall)
levels(scenario_restoration) <- wkey %>% select(id = code, shortlab) %>%
  as.data.frame()
coltab(scenario_restoration) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_restoration) = 'scenario_restoration'
writeRaster(scenario_restoration,
            'data/landcover_waterbirds_fall/scenario1_restoration_waterbird_fall.tif')

# calculate change in area of each land cover
delta_restoration = DeltaMultipleBenefits::calculate_change(
  baseline = baseline_wat_fall,
  scenario = scenario_restoration)
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
# orchard/vineyard, dry & irrigated pastures, alfalfa

# compare/check conversions between layers
tab = crosstab(c(baseline_wat_fall %>% mask(delta),
                 scenario_restoration %>% mask(delta)))

tab %>% as_tibble() %>%
  set_names(c('baseline', 'scenario', 'n')) %>%
  mutate_at(vars(baseline:scenario), as.numeric) %>%
  left_join(wkey %>% dplyr::select(code, shortlab), by = c('baseline' = 'code')) %>%
  left_join(wkey %>% dplyr::select(code, shortlab), by = c('scenario' = 'code')) %>%
  arrange(desc(n)) %>%
  group_by(shortlab.y) %>%
  mutate(tot = sum(n),
         prop = n/tot) %>%
  ungroup() %>%
  filter(shortlab.y == 'duwet')
  # filter(shortlab.y == 'woodw')

# - riparian: 62% existing riparian; most conversion from orch (18%);
#    no conversion from rice, duwet, dev, water
# - wetland: 82% existing duwet; most conversion from ip (6%), wet (4%);
#    no conversion from dev, forest, water, barren

### waterbirds-winter--------
scenario_restoration_win = cover(rip_targets_final, wet_targets_final) %>%
  cover(baseline_wat_win)
levels(scenario_restoration_win) <- wkey %>% select(id = code, shortlab) %>%
  as.data.frame()
coltab(scenario_restoration_win) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_restoration_win) = 'scenario_restoration'
writeRaster(scenario_restoration_win,
            'data/landcover_waterbirds_winter/scenario1_restoration_waterbird_winter.tif')

### riparian----------
scenario_restoration_rip <- DeltaMultipleBenefits::reclassify_ripmodels(scenario_restoration)
levels(scenario_restoration_rip) <- rkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(scenario_restoration_rip) <- rkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_restoration_rip) = 'scenario_restoration'
writeRaster(scenario_restoration_rip,
            'data/landcover_riparian/scenario1_restoration_riparian.tif')

# compare/check conversions between layers
tab = crosstab(c(baseline_rip %>% mask(delta),
                 scenario_restoration_rip %>% mask(delta)))

tab %>% as_tibble() %>%
  set_names(c('baseline', 'scenario', 'n')) %>%
  mutate_at(vars(baseline:scenario), as.numeric) %>%
  left_join(rkey %>% dplyr::select(code, group), by = c('baseline' = 'code')) %>%
  left_join(rkey %>% dplyr::select(code, group), by = c('scenario' = 'code')) %>%
  arrange(desc(n)) %>%
  group_by(group.y) %>%
  mutate(tot = sum(n),
         prop = n/tot) %>%
  ungroup() %>%
  filter(group.y == 'WETLAND')
  # filter(group.y == 'RIPARIAN')

# - riparian: 62% existing riparian; most conversion from ORCHVIN (18%);
#    no conversion from RICE, URBAN, or WATER
# - wetland: 94% existing WETLAND; most conversion from GRASSPAS (4%), AG (1%);
#    no conversion from URBAN, WATER, OAKWOODLAND, BARREN


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

skey = readr::read_csv('GIS/State Class Rasters/key.csv')

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

exclude = terra::classify(baseline_wat_fall,
                          rcl = matrix(c(8, 8,
                                         12, 12,
                                         14, 14,
                                         15, 15,
                                         18, 18),
                                       byrow = TRUE, ncol = 2),
                          othersNA = TRUE)

## overlay on baseline layers------

### waterbirds-fall--------------
scenario_perex = terra::cover(exclude,
                              terra::cover(bbau, baseline_wat_fall))
levels(scenario_perex) <- wkey %>% select(id = code, shortlab) %>%
  as.data.frame()
coltab(scenario_perex) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_perex) = 'scenario_perennial_expansion'
terra::writeRaster(scenario_perex,
                   'data/landcover_waterbirds_fall/scenario2_perennialexpand_waterbird_fall.tif')

# calculate change in area of each land cover
delta_perex = DeltaMultipleBenefits::calculate_change(
  baseline = baseline_wat_fall,
  scenario = scenario_perex)
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
# row, alfalfa, corn, fallow, but also water and wet

# compare
tab = crosstab(c(baseline_wat_fall, scenario_perex))
tab %>% as_tibble() %>%
  set_names(c('baseline', 'bbau', 'n')) %>%
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
  # filter(bbau_landuse == 'dev')
  # filter(bbau_landuse == 'orch')
  filter(bbau_landuse == 'wet')

### waterbirds-winter-------------
scenario_perex_win = terra::cover(exclude,
                                  terra::cover(bbau, baseline_wat_win))
levels(scenario_perex_win) <- wkey %>% select(id = code, shortlab) %>%
  as.data.frame()
coltab(scenario_perex_win) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_perex_win) = 'scenario_perennial_expansion'
terra::writeRaster(scenario_perex_win,
                   'data/landcover_waterbirds_winter/scenario2_perennialexpand_waterbird_winter.tif')


### riparian-------
scenario_perex_rip <- DeltaMultipleBenefits::reclassify_ripmodels(scenario_perex)
levels(scenario_perex_rip) <- rkey %>% select(id = code, label = group) %>%
  as.data.frame()
coltab(scenario_perex_rip) <- rkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_perex_rip) = 'scenario_perennial_expansion'
plot(scenario_perex_rip)
writeRaster(scenario_perex_rip,
            'data/landcover_riparian/scenario2_perennialexpand_riparian.tif')

### multiple benefits-------
scenario_perex_mb <- DeltaMultipleBenefits::reclassify_multiplebenefits(scenario_perex)
levels(scenario_perex_mb) <- mkey %>% select(id = code, label = group) %>%
  as.data.frame()
coltab(scenario_perex_mb) <- mkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_perex_mb) = 'scenario_perennial_expansion'
plot(scenario_perex_mb)
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

# except pixels that are already water, riparian, urban, wet
exclude = classify(baseline_wat_fall,
                   rcl = matrix(c(8, 8,
                                  12, 12,
                                  14, 14,
                                  15, 15),
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
writeRaster(scenario_floodrisk,
            'data/landcover_waterbirds_fall/scenario3_floodrisk_waterbird_fall.tif')

plot(scenario_floodrisk)

# calculate change in area of each land cover
delta_floodrisk = DeltaMultipleBenefits::calculate_change(
  baseline = veg_baseline_waterbird_fall,
  scenario = scenario_floodrisk)
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
writeRaster(scenario_floodrisk_win,
            'data/landcover_waterbirds_winter/scenario3_floodrisk_waterbird_winter.tif')

plot(scenario_floodrisk_winter)

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
plot(scenario_floodrisk_riparian)
writeRaster(scenario_floodrisk_riparian,
            'data/landcover_riparian/scenario3_floodrisk_riparian.tif')

### multiple benefits---------
scenario_floodrisk_mb <- DeltaMultipleBenefits::reclassify_multiplebenefits(scenario_floodrisk)
levels(scenario_floodrisk_mb) <- mkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(scenario_floodrisk_mb) <- mkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_floodrisk_mb) = 'scenario_floodrisk'
plot(scenario_floodrisk_mb)
writeRaster(scenario_floodrisk_mb,
            'data/landcover_multiplebenefits/scenario3_floodrisk_mb.tif')

# ADDITIONAL THOUGHTS/IDEAS/FEEDBACK--------
# - SFEI carbon/subsidence reversal scenarios (but not waiting on this)
# - prioritize restoration near Zonation hotspots
# - incorporate specific plans/projects underway for Sherman & Twichell Island
# --> incorporate EcoRestore into restoration scenario (if not already shown)
# - consider alternative restoration scenario that is more focused on subsided
# islands (i.e. what we can get, short-term value)
# - shorter-term sea level rise scenario?
# - consider rice in sea level rise scenario? but some think not much rice
# feasible except on public lands/where subsidized


# INTERACTIVE MAP-----------

# best to set the names of the raster layers (will transfer to the selector)
mapstack = c(veg_baseline_waterbird_fall,
             scenario_restoration,
             scenario_orchard_refine,
             scenario_floodrisk)
names(mapstack) = c('baseline', 'scenario_restoration',
                    'scenario_orchard_expansion', 'scenario_flood_risk')

# # consider resampling rasters to prevent the file size from being too enormous (this is
# # still fairly hi-res)
# scenarios_sampled_hi = pred_cv %>%
#   terra::spatSample(size = 5000000, as.raster = TRUE)

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

