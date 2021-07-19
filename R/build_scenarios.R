# Build & evaluate scenarios of landscape change in the Delta

# PACKAGES & FUNCTIONS
source('R/packages.R')

# reference data:
delta = rast('GIS/delta.tif')

# BASELINE--------
# make a local copy & add code details
gisdir = 'V:/Project/Terrestrial/cv_riparian/distribution_modeling/'

## waterbirds---------
wkey = read_csv('V:/Project/wetland/Delta/landscape_models/predrasters_baseline/baseline/VegCAMP_crosswalk.csv',
                col_types = cols()) %>%
  dplyr::select(group = WATERBIRD, code) %>%
  distinct() %>%
  bind_rows(tibble(group = 'DUwetland',
                   code = 18)) %>%
  arrange(code) %>%
  mutate(shortlab = case_when(group == 'Irrigated pasture' ~ 'ip',
                              group == 'Dryland pasture' ~ 'dryp',
                              group == 'wetland' ~ 'wet',
                              group == 'orchard' ~ 'orch',
                              group == 'alfalfa' ~ 'alf',
                              group == 'woody wetland' ~ 'woodw',
                              group == 'developed' ~ 'dev',
                              group == 'fallow' ~ 'fal',
                              group == 'DUwetland' ~ 'duwet',
                              TRUE ~ group),
         label = recode(shortlab,
                        'ip' = 'irrigated pasture',
                        'wheat' = 'grain',
                        'row' = 'row/field crop',
                        'field' = 'row/field crop',
                        'wet' = 'wetland',
                        'DUwetland' = 'wetland',
                        'orch' = 'orchard/vineyard',
                        'fal' = 'fallow',
                        'dev' = 'urban',
                        'woodw' = 'riparian',
                        'dryp' = 'dryland pasture',
                        'alf' = 'alfalfa',
                        'duwet' = 'wetland'),
         col = c('#FFFF00', '#FF1493', '#2E8B57', '#FFA54F', '#FA8072',
                 '#FA8072', '#00008B', '#9400D3', '#FFA54F', '#CCCCCC',
                 '#4D4D4D', '#8B4513', '#87CEFA', '#FF0000', '#FFE4C4',
                 '#32CD32', '#00008B', '#FFFFFF'))
write_csv(wkey, 'data/landcover_key_waterbirds.csv')

veg_baseline_waterbird_fall = rast(paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_waterbirds_fall.tif'))
levels(veg_baseline_waterbird_fall) <- wkey %>% select(id = code, shortlab) %>%
  as.data.frame()
coltab(veg_baseline_waterbird_fall) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(veg_baseline_waterbird_fall) = 'baseline'
writeRaster(veg_baseline_waterbird_fall, 'data/veg_baseline_waterbird_fall.tif')

veg_baseline_waterbird_winter = rast(paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_waterbirds_winter.tif'))
levels(veg_baseline_waterbird_winter) <- wkey %>%
  select(id = code, shortlab) %>% as.data.frame()
coltab(veg_baseline_waterbird_winter) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(veg_baseline_waterbird_winter) = 'baseline'
writeRaster(veg_baseline_waterbird_winter, 'data/veg_baseline_waterbird_winter.tif')


## riparian-------
rkey = read_csv(paste0(gisdir, 'GIS/landscape_rasters/key.csv'),
                col_types = cols()) %>%
  filter(type == 'veg' & !code %in% c(110, 120, 999)) %>%
  select(-type) %>%
  mutate(label = recode(group,
                        'ORCHVIN' = 'orchard/vineyard',
                        'RICE' = 'rice',
                        'AG' = 'other crops',
                        'IDLE' = 'fallow',
                        'GRASSPAS' = 'grassland/pasture',
                        'URBAN' = 'urban',
                        'RIPARIAN' = 'riparian',
                        'WETLAND' = 'wetland',
                        'WATER' = 'water',
                        'OAKWOODLAND' = 'forest',
                        'BARREN' = 'barren'),
         col = c('#9400D3', '#32CD32', '#FF1493', '#CCCCCC', '#FFE4C4',
                 '#4D4D4D', '#FF0000', '#00008B', '#87CEFA', '#8B4513',
                 '#FFFFFF'))
write_csv(rkey, 'data/landcover_key_riparian.csv')

veg_baseline_riparian = rast(paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_riparian.tif'))
levels(veg_baseline_riparian) <- rkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(veg_baseline_riparian) <- rkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(veg_baseline_riparian) = 'baseline'
writeRaster(veg_baseline_riparian, 'data/veg_baseline_riparian.tif')

## multiple benefits-------
mkey = tibble(group = c('orchard/vineyard', 'citrus', 'orchard', 'vineyard',
                        'row/field', 'corn', 'grains', 'cotton',
                        'tomato', 'rice', 'grassland/pasture', 'alfalfa',
                        'riparian', 'wetland', 'other'),
              code = c(10, 11, 12, 13,
                       21, 22, 23, 24,
                       25, 30, 50, 51,
                       70, 80, 90),
              col = c('#9400D3', NA, NA, NA,
                      '#FA8072', '#FFFF00', '#FFA54F', NA,
                      NA, '#FF1493', '#FFE4C4', '#32CD32',
                      '#FF0000', '#00008B', '#4D4D4D'))

veg_baseline_mb = DeltaMultipleBenefits::reclassify_multiplebenefits(veg_baseline_waterbird_fall)
levels(veg_baseline_mb) <- mkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(veg_baseline_mb) <- mkey %>% select(code, col) %>% filter(!is.na(col)) %>%
  complete(code = c(0:255)) %>% pull(col)
names(veg_baseline_mb) = 'baseline'
plot(veg_baseline_mb)
writeRaster(veg_baseline_mb, 'data/veg_baseline_multiplebenefits.tif')

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


## overlay on baseline layer------------
scenario_restoration = cover(rip_targets_final, wet_targets_final) %>%
  cover(veg_baseline_waterbird_fall)
levels(scenario_restoration) <- wkey %>% select(id = code, shortlab) %>%
  as.data.frame()
coltab(scenario_restoration) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_restoration) = 'scenario_restoration'
writeRaster(scenario_restoration,
            'data/proposed_scenarios/waterbirds/DeltaPlan_restoration_objectives.tif')

# calculate change in area of each land cover
delta_restoration = DeltaMultipleBenefits::calculate_change(
  baseline = veg_baseline_waterbird_fall,
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
tab = crosstab(c(veg_baseline_waterbird_fall %>% mask(delta),
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

# convert encoding for use with riparian models
scenario_restoration_riparian <- DeltaMultipleBenefits::reclassify_ripmodels(scenario_restoration)
levels(scenario_restoration_riparian) <- rkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(scenario_restoration_riparian) <- rkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_restoration_riparian) = 'scenario_restoration'
writeRaster(scenario_restoration_riparian,
            'data/proposed_scenarios/riparian/DeltaPlan_restoration_objectives.tif')

# compare/check conversions between layers
tab = crosstab(c(veg_baseline_riparian %>% mask(delta),
                 scenario_restoration_riparian %>% mask(delta)))

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

# convert encoding for use with multiple benefits models
scenario_restoration_mb <- DeltaMultipleBenefits::reclassify_multiplebenefits(scenario_restoration)
levels(scenario_restoration_mb) <- mkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(scenario_restoration_mb) <- mkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_restoration_mb) = 'scenario_restoration'
writeRaster(scenario_restoration_mb,
            'data/proposed_scenarios/multiplebenefits/DeltaPlan_restoration_objectives.tif')

# compare/check conversions between layers
tab = crosstab(c(veg_baseline_mb %>% mask(delta),
                 scenario_restoration_mb %>% mask(delta)))

tab %>% as_tibble() %>%
  set_names(c('baseline', 'scenario', 'n')) %>%
  mutate_at(vars(baseline:scenario), as.numeric) %>%
  left_join(mkey %>% dplyr::select(code, group), by = c('baseline' = 'code')) %>%
  left_join(mkey %>% dplyr::select(code, group), by = c('scenario' = 'code')) %>%
  arrange(desc(n)) %>%
  group_by(group.y) %>%
  mutate(tot = sum(n),
         prop = n/tot) %>%
  ungroup() %>%
  filter(group.y == 'wetland')
# filter(group.y == 'riparian')

# - riparian: 62% existing riparian; most conversion from orchard/vineyard (18%);
#    no conversion from rice
# - wetland: 94% existing WETLAND; most conversion from grassland/pasture (3%)


# SCENARIO 2. Perennial crop expansion---------
# based on the Wilson et al. (2021) BBAU ("bad business as usual") scenario for
# the year 2100, including historically high rates of perennial crop expansion;
# no restoration

skey = readr::read_csv('GIS/State Class Rasters/key.csv')

bbau = terra::rast('GIS/State Class Rasters/scn421.sc.it1.ts2100.tif') %>%
  terra::project(veg_baseline_waterbird_fall, method = 'near') %>%
  terra::mask(veg_baseline_waterbird_fall) %>%
  # keep only the footprint of expanded perennial crops (code=20) - reclassify
  # to waterbird code for orch (9); all others convert to NA
  terra::classify(rcl = matrix(c(20, 9), byrow = TRUE, ncol = 2),
                  othersNA = TRUE)
# note: missing southwest corner of Delta

# allow orchard footprint from bbau scenario to cover baseline footprint, except
#  where baseline is a land cover that shouldn't change to orchard
#   dev (12), water (14), woodw (15), wetland (8), duwet (18)
# (all else unchanged)

exclude = terra::classify(veg_baseline_waterbird_fall,
                          rcl = matrix(c(8, 8,
                                         12, 12,
                                         14, 14,
                                         15, 15,
                                         18, 18),
                                       byrow = TRUE, ncol = 2),
                          othersNA = TRUE)

scenario_orchard = terra::cover(bbau, veg_baseline_waterbird_fall)
scenario_orchard_refine = terra::cover(exclude, scenario_orchard)
levels(scenario_orchard_refine) <- wkey %>%
  dplyr::select(id = code, shortlab) %>%
  as.data.frame()
coltab(scenario_orchard_refine) <- wkey %>%
  dplyr::select(code, col) %>%
  tidyr::complete(code = c(0:255)) %>%
  dplyr::pull(col)
names(scenario_orchard_refine) = 'scenario_orchard_expansion'
terra::writeRaster(
  scenario_orchard_refine,
  'data/proposed_scenarios/waterbirds/orchard_expansion.tif')
# Note: levels may not be saved, but colors are?


# calculate change in area of each land cover
delta_orchard = DeltaMultipleBenefits::calculate_change(
  baseline = veg_baseline_waterbird_fall,
  scenario = scenario_orchard_refine)
delta_orchard %>%
  left_join(wkey %>% select(label.base = shortlab, label), by = 'label.base') %>%
  group_by(label) %>%
  summarize(change = sum(change), .groups = 'drop') %>%
  arrange(change) %>%
  DeltaMultipleBenefits::plot_change(scale = 1000000) +
  labs(x = NULL, y = bquote(' ' *Delta~ 'total area ('~km^2*')')) +
  theme_bw() + coord_flip() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))
ggsave('fig/delta_orchard.png', height = 7.5, width = 6)
# large increase in orchard cover, at the expense of most others, especially
# row, alfalfa, corn, fallow, but also water and wet


# compare
tab = crosstab(c(veg_baseline_waterbird_fall, scenario_orchard_refine))
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

# convert to riparian
scenario_orchard_riparian <- DeltaMultipleBenefits::reclassify_ripmodels(scenario_orchard_refine)
levels(scenario_orchard_riparian) <- rkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(scenario_orchard_riparian) <- rkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_orchard_riparian) = 'scenario_orchard_expansion'
plot(scenario_orchard_riparian)
writeRaster(scenario_orchard_riparian,
            'data/proposed_scenarios/riparian/orchard_expansion.tif')

# convert to multiple benefits
scenario_orchard_mb <- DeltaMultipleBenefits::reclassify_multiplebenefits(scenario_orchard_refine)
levels(scenario_orchard_mb) <- mkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(scenario_orchard_mb) <- mkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_orchard_mb) = 'scenario_orchard_expansion'
plot(scenario_orchard_mb)
writeRaster(scenario_orchard_mb,
            'data/proposed_scenarios/multiplebenefits/orchard_expansion.tif')


# SCENARIO 3. Flood risk-----------
veg_baseline_rast = raster::raster('data/veg_baseline_waterbird_fall.tif')

# first combine separate layers representing flood risk
filelist = paste0('GIS/DeltaAdapts/',
                  list.files('GIS/DeltaAdapts',
                             'baseline_.*shp$|floodrisk2085_.*shp$'))

tmp = purrr::map(filelist,
                 ~st_read(.) %>%
                   fasterize::fasterize(veg_baseline_rast,
                                        field = 'flood_risk')) %>%
  raster::stack()
maxfloodrisk = max(tmp, na.rm = TRUE)
plot(maxfloodrisk, col = rev(hcl.colors(5)))

key = data.frame(id = c(0:4),
                 label = c('very low', 'low', 'medium', 'high', 'very high'),
                 risk.annual = c('<0.5%', '0.5-1%', '1-2%', '2-10%', '>10%'),
                 col = c('lightskyblue', 'dodgerblue', 'royalblue', 'blue3', 'midnightblue'))

maxfloodrisk = terra::rast(maxfloodrisk)
levels(maxfloodrisk) <- key
coltab(maxfloodrisk) <- key %>% select(id, col) %>%
  complete(id = c(0:255)) %>% pull(col)
plot(maxfloodrisk)

writeRaster(maxfloodrisk, 'GIS/floodrisk2085.tif')

# scenario assumption: all "very high" flood risk areas become wetlands, and
# all perennial crops move out of "high" and "medium" risk areas

## very high risk------
veryhigh = classify(maxfloodrisk,
                    rcl = matrix(c(4, 18), byrow = TRUE, ncol = 2),
                    othersNA = TRUE)
plot(veryhigh)

# except pixels that are already water, riparian, urban, wet
exclude = classify(veg_baseline_waterbird_fall,
                   rcl = matrix(c(8, 8,
                                  12, 12,
                                  14, 14,
                                  15, 15),
                                byrow = TRUE, ncol = 2),
                   othersNA = TRUE)

# overlay on baseline
veryhigh_wetlands = cover(veryhigh, veg_baseline_waterbird_fall) %>%
  mask(veg_baseline_waterbird_fall) #exclude some extra areas in Suisun
veryhigh_wetlands_refine = cover(exclude, veryhigh_wetlands)
plot(veryhigh_wetlands_refine)

# calculate change in area of each land cover
delta_veryhigh = DeltaMultipleBenefits::calculate_change(
  baseline = veg_baseline_waterbird_fall,
  scenario = veryhigh_wetlands_refine)
# lots of corn, alf, row lost

## high & medium flood risk-----

# identify contiguous patches of orchard pixels in the high and medium flood
# risk areas
highmed = subst(maxfloodrisk, c(0:1,4), NA)
highmed_orch_patches = mask(veg_baseline_waterbird_fall, highmed) %>%
  subst(c(0:8,10:99), NA) %>%
  patches()
plot(highmed_orch_patches)

# map each contiguous patch of orchard to a new randomly selected crop, weighted
# by the proportion lost to wetlands in the "very high" risk islands:
prop_sample = delta_veryhigh %>%
  select(value, label, change) %>%
  filter(change<0 & !label %in% c('orch', 'forest', 'fal', 'barren', 'rice')) %>%
  mutate(change = abs(change),
         prop = change / sum(change))
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

## overlay on baseline------
scenario_floodrisk = cover(replace_orchard, veryhigh_wetlands_refine)
levels(scenario_floodrisk) <- wkey %>% select(id = code, shortlab) %>%
  as.data.frame()
coltab(scenario_floodrisk) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_floodrisk) = 'scenario_floodrisk'
writeRaster(scenario_floodrisk,
            'data/proposed_scenarios/waterbirds/DeltaAdapts_floodrisk.tif')

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


# convert to riparian
scenario_floodrisk_riparian <- DeltaMultipleBenefits::reclassify_ripmodels(scenario_floodrisk)
levels(scenario_floodrisk_riparian) <- rkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(scenario_floodrisk_riparian) <- rkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_floodrisk_riparian) = 'scenario_floodrisk'
plot(scenario_floodrisk_riparian)
writeRaster(scenario_floodrisk_riparian,
            'data/proposed_scenarios/riparian/DeltaAdapts_floodrisk.tif')

# convert to multiple benefits
scenario_floodrisk_mb <- DeltaMultipleBenefits::reclassify_multiplebenefits(scenario_floodrisk)
levels(scenario_floodrisk_mb) <- mkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(scenario_floodrisk_mb) <- mkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(scenario_floodrisk_mb) = 'scenario_floodrisk'
plot(scenario_floodrisk_mb)
writeRaster(scenario_floodrisk_mb,
            'data/proposed_scenarios/multiplebenefits/DeltaAdapts_floodrisk.tif')

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

