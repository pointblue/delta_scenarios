# Build & evaluate scenarios of landscape change in the Delta

# PACKAGES & FUNCTIONS
source('R/packages.R')

# reference data:
delta = rast('GIS/delta.tif')

gisdir = 'V:/Project/Terrestrial/cv_riparian/distribution_modeling/'
veg_baseline_waterbird_fall = rast(paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_waterbirds_fall.tif'))
veg_baseline_riparian = rast(paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_riparian.tif'))

rkey = read_csv(paste0(gisdir, 'GIS/landscape_rasters/key.csv'),
                col_types = cols())
wkey = read_csv('V:/Project/wetland/Delta/landscape_models/predrasters_baseline/baseline/VegCAMP_crosswalk.csv',
                col_types = cols()) %>%
  dplyr::select(group = WATERBIRD, code) %>%
  distinct() %>%
  mutate(label = case_when(group == 'Irrigated pasture' ~ 'ip',
                           group == 'Dryland pasture' ~ 'dryp',
                           group == 'wetland' ~ 'wet',
                           group == 'orchard' ~ 'orch',
                           group == 'alfalfa' ~ 'alf',
                           group == 'woody wetland' ~ 'woodw',
                           group == 'developed' ~ 'dev',
                           group == 'fallow' ~ 'fal',
                           TRUE ~ group)) %>%
  bind_rows(tibble(group = 'DUwetland',
                   code = 18,
                   label = 'duwet'))

levels(veg_baseline_waterbird_fall) <- wkey %>% select(id = code, label) %>% as.data.frame()
coltab(veg_baseline_waterbird_fall) <- wkey %>% select(code) %>%
  mutate(col = c('#FFFF00', '#FF1493', '#2E8B57', '#FFA54F', '#FA8072',
                 '#FA8072', '#00008B', '#9400D3', '#FFA54F', '#CCCCCC',
                 '#4D4D4D', '#8B4513', '#87CEFA', '#FF0000', '#FFE4C4',
                 '#32CD32', '#FFFFFF', '#00008B')) %>%
  complete(code = c(0:255)) %>% pull(col)

# #combined key
# key = wkey %>% select(id = code, label_wat = label) %>%
#   mutate(id_rip = c(20, 30, 50, 20, 20, 20, 80, 10, 20, 40, 60, 110, 90, 70, 50,
#                     50, 130, 80),
#          label_rip = c('AG', 'RICE', 'GRASSPAS', 'AG', 'AG', 'AG', 'WETLAND',
#                        'ORCHVIN', 'AG', 'IDLE', 'URBAN', 'WOODLAND', 'WATER',
#                        'RIPARIAN', 'GRASSPAS', 'GRASSPAS', 'BARREN', 'WETLAND')) %>%
#   arrange(id)


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

## restoration objectives------
obj = read_csv('data/SFEI_DSLPT/delta_targets.csv', col_types = cols())

obj %>% dplyr::select(TYPE, TARGET_HECTARES) %>%
  slice(1:2) %>% mutate(code = c(80, 70)) %>%
  left_join(potential_area, by = 'code') %>%
  mutate(prop = TARGET_HECTARES / area.ha)
# 7,689 ha of wet meadow/seasonal wetland
# 6,596 ha of riparian

# objectives are to add this amount to the baseline amount from the 2007 VegCAMP
# layer; so question is how much has already been added since 2007?
# alternatively, aim for a total amount of 9,753 ha of wetlands and 12,343 ha of
# riparian

# (Note: DWR conservation plan goals for entire Lower Sac River and Lower San
# Joaquin River conservation planning areas combined are less than the goals for
# just the riparian and wetland habitat in the Delta)

# current totals (from baseline layer)
veg_baseline_waterbird_fall %>%
  mask(delta) %>%
  values(dataframe = TRUE) %>%
  table() %>% as_tibble() %>%
  set_names(c('code', 'n')) %>%
  filter(code %in% c(15, 18)) %>%
  mutate(area.ha = n * 30 * 30 / 10000,
         code = as.numeric(code))
# wetlands = 7,756 ha (consider 'du wetlands' only)
# riparian = 8,019 ha

targets = tibble(habitat = c('riparian', 'wetlands'),
                 add.ha = c(12343 - 8019, 9753 - 7756))
# riparian: 4,324 ha
# wetlands: 1,997 ha


## rank priority restoration locations-----
## ASSUMPTIONS/QUESTIONS:
## - minimum size of restoration to consider (is 1 acre reasonable?)
## - feasibility of restoration on protected land, easements, public land?
##   (and does it matter for this hypothetical?)
## - more of a priority to focus restoration in "priority restoration areas" or
##    protected land? [relevant mainly to wetlands]

# exclude areas that are already wetland or riparian; also already urban or
# water (assume permanent)
exclude = classify(veg_baseline_waterbird_fall,
                   rcl = matrix(c(8, NA, #wetland
                                  12, NA, #dev
                                  14, NA, # water
                                  15, NA, # woodw
                                  18, NA), #duwet
                                byrow = TRUE, ncol = 2))

# input data
restoration_stack = c(rast('GIS/habpotential.tif'), #7000 = riparian, 8000 = wetland
                      rast('GIS/restorationpriority.tif'), # 100 = priority
                      rast('GIS/zoning.tif'), #10 = open space/recreation; 20 = public/quasi=public; 90 = designated for development
                      rast('GIS/protectedareas.tif') # 1 = protected, 2 = easement
                      ) %>%
  mask(delta) %>%
  mask(exclude)

# sum scores to classify each pixel with multiple pieces of data
restoration_targets = sum(restoration_stack, na.rm = TRUE)

# group pixels by priority for pixels classified as potential riparian & wetland
restoration_targets_rip = classify(
  restoration_targets,
  rcl = matrix(c(0, 7000, NA,
                 7000, 7001, 5, #potential but not priority, open space, or protected
                 7001, 7003, 3, #potential but not priority; not open space but protected
                 7010, 7011, 4, #potential but not priority; open space, unprotected
                 7011, 7013, 3, #potential but not priority; open space, protected
                 7020, 7021, 4, #potential but not priority; public/quasi-public, unprotected
                 7090, 7092, 9, #potential but not priority, designed for development
                 7100, 7101, 2, #priority, unprotected
                 7101, 7103, 1, #priority, protected
                 7110, 7111, 2, #priority, open space, unprotected
                 7111, 7113, 1, #priority, open space, protected
                 7190, Inf, NA),
               byrow = TRUE, ncol = 3)
)
writeRaster(restoration_targets_rip, 'GIS/restoration_targets_riparian.tif')

restoration_targets_wet = classify(
  restoration_targets,
  rcl = matrix(c(0, 8000, NA,
                 8000, 8001, 5, #potential but not priority, open space, or protected
                 8001, 8003, 3, #potential but not priority; not open space but protected
                 8010, 8011, 4, #potential but not priority; open space, unprotected
                 8011, 8013, 3, #potential but not priority; open space, protected
                 8020, 8021, 4, #potential but not priority; public/quasi-public, unprotected
                 8021, 8023, 3, #potential but not priority; public/quasi-public, protected
                 8090, 8092, 9, #potential but not priority, designed for development
                 8100, 8101, 2, #priority, unprotected
                 8101, 8103, 1, #priority, protected
                 8110, 8111, 2, #priority, open space, unprotected
                 8111, 8113, 1, #priority, open space, protected
                 8190, Inf, NA),
               byrow = TRUE, ncol = 3)
)
writeRaster(restoration_targets_wet, 'GIS/restoration_targets_wetlands.tif')


## riparian restoration-----
# approach: select pixels by priority, but exclude very small patches of
# potential riparian compare area of each priority level vs. objective; when not
# all of a priority level is needed, randomly select a subset that will meet the
# restoration objective
#
# objective: 4324 ha added

targets_area_rip = values(restoration_targets_rip) %>%
  table() %>% as_tibble() %>%
  set_names('code', 'n') %>%
  mutate(area.ha = n * 30 * 30 / 10000,
         code = as.numeric(code),
         tot.ha = cumsum(area.ha))
#  code     n area.ha tot.ha
# <dbl> <int>   <dbl>  <dbl>
#     1   616    55.4   55.4 # priority & protected
#     2 12400  1116   1171.  # priority & unprotected
#     3  2243   202.  1373.  # not priority but protected
#     4  4342   391.  1764.  # not priority or protected but open space/quasi-public
#     5 43319  3899.  5663.  # other potential riparian habitat
#     9  7206   649.  6311.  # designated for development

# --> to reach target of 4324 ha, will need most/all of priority levels 1-4 plus
# some of priority level 5

# PRIORITY LEVELS 1-4:
# (exclude areas designated for development & priority level 5 for now)
rip_targets1 = classify(restoration_targets_rip,
                        rcl = matrix(c(5, NA,
                                       9, NA),
                                     byrow = TRUE, ncol = 2))

# find patches & exclude those < 1 acre (0.4 ha) in size
rip_targets1_patches = patches(rip_targets1, directions = 8)

rip_targets1_keepID = values(rip_targets1_patches) %>%
  table() %>% as_tibble() %>%
  set_names(c('patchID', 'n')) %>%
  mutate(area.ha = n * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4) %>%
  pull(patchID)

keep = function(x, y) {
  ifelse(x %in% y, 1, NA)
}

rip_targets1_keep = lapp(rip_targets1_patches,
                         y = rip_targets1_keepID,
                         fun = keep)

# EVALUATE PRIORITY LEVEL 5:
# how much additional area needed from priority level 5?
rip_need2 = targets %>% filter(habitat == 'riparian') %>% pull(add.ha) - values(rip_targets1_patches) %>%
  table() %>% as_tibble() %>%
  set_names(c('patchID', 'n')) %>%
  mutate(area.ha = n * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4) %>%
  pull(area.ha) %>% sum()
# 2691.4 ha

rip_targets2 = classify(restoration_targets_rip,
                        rcl = matrix(c(0, 5, NA,
                                       6, Inf, NA),
                                     byrow = TRUE, ncol = 3))

rip_targets2_patches = patches(rip_targets2, directions = 8)

rip_targets2_consider = values(rip_targets2_patches) %>%
  table() %>% as_tibble() %>%
  set_names(c('patchID', 'n')) %>%
  mutate(area.ha = n * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4) %>%
  arrange(desc(area.ha)) %>%
  mutate(weight = area.ha / sum(area.ha))

# random sample of priority level 5 patches > 1 acre, weighted by size
rip_targets2_sampleorder = sample(rip_targets2_consider %>% pull(patchID),
                                  # prob = rip_targets2_consider %>% pull(weight),
                                  nrow(rip_targets2_consider), replace = FALSE)

rip_targets2_considerorder = rip_targets2_consider %>%
  mutate(patchID = factor(patchID, levels =  rip_targets2_sampleorder)) %>%
  arrange(patchID) %>%
  mutate(tot.ha = cumsum(area.ha))

# stop when cumulative sum reaches/exceeds target
rip_targets2_keepID = rip_targets2_considerorder %>%
  filter(tot.ha <= rip_targets2_considerorder %>% filter(tot.ha > rip_need2) %>%
           slice(1) %>% pull(tot.ha)) %>%
  pull(patchID)

rip_targets2_keep = lapp(rip_targets2_patches,
                         y = rip_targets2_keepID,
                         fun = keep)

# COMBINE PROPOSED RIPARIAN RESTORATION
# (convert all other patch IDs to NA)
rip_targets_final = cover(rip_targets1_keep, rip_targets2_keep)
writeRaster(rip_targets_final,
            'GIS/restoration_targets_riparian_objectives.tif')

## wetland restoration---------
# same approach as for riparian
#
# objective: 1,997 ha added

targets_area_wet = values(restoration_targets_wet) %>%
  table() %>% as_tibble() %>%
  set_names('code', 'n') %>%
  mutate(area.ha = n * 30 * 30 / 10000,
         code = as.numeric(code),
         tot.ha = cumsum(area.ha))
#  code     n area.ha tot.ha
# <dbl> <int>   <dbl>  <dbl>
#     1 14827   1334.  1334. # priority & protected
#     2 65751   5918.  7252. # priority & unprotected
#     3  7881    709.  7961. # not priority but protected
#     4  1217    110.  8071. # not priority or protected but open space/quasi-public
#     5 61983   5578. 13649. # other potential wetland habitat
#     9  2865    258. 13907. # designated for development

# --> to reach target of 1997 ha added, need most/all of priority level 1 plus
# some of priority level 2 (or is 3 a higher priority?)

# PRIORITY LEVEL 1:
# (exclude areas designated for development & priority levels 2-5 for now)

wet_targets1 = classify(restoration_targets_wet,
                        rcl = matrix(c(2, Inf, NA),
                                     byrow = TRUE, ncol = 3))

# find patches & exclude those < 1 acre (0.4 ha) in size
wet_targets1_patches = patches(wet_targets1, directions = 8)

wet_targets1_keepID = values(wet_targets1_patches) %>%
  table() %>% as_tibble() %>%
  set_names(c('patchID', 'n')) %>%
  mutate(area.ha = n * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4) %>%
  pull(patchID)

wet_targets1_keep = lapp(wet_targets1_patches,
                         y = wet_targets1_keepID,
                         fun = keep)

# EVALUATE PRIORITY LEVEL 2:
# how much additional area needed from priority level 2?
wet_need2 = targets %>% filter(habitat == 'wetlands') %>% pull(add.ha) - values(wet_targets1_patches) %>%
  table() %>% as_tibble() %>%
  set_names(c('patchID', 'n')) %>%
  mutate(area.ha = n * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4) %>%
  pull(area.ha) %>% sum()
# 676.79 ha

wet_targets2 = classify(restoration_targets_wet,
                        rcl = matrix(c(0, 2, NA,
                                       3, Inf, NA),
                                     byrow = TRUE, ncol = 3))

wet_targets2_patches = patches(wet_targets2, directions = 8)

wet_targets2_consider = values(wet_targets2_patches) %>%
  table() %>% as_tibble() %>%
  set_names(c('patchID', 'n')) %>%
  mutate(area.ha = n * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4) %>%
  arrange(desc(area.ha)) %>%
  mutate(weight = area.ha / sum(area.ha))

# random sample of priority level 2 patches > 1 acre, weighted by size
# --> CONSIDER STRATIFYING BY REGION?
wet_targets2_sampleorder = sample(wet_targets2_consider %>% pull(patchID),
                                  # prob = wet_targets2_consider %>% pull(weight),
                                  nrow(wet_targets2_consider), replace = FALSE)

wet_targets2_considerorder = wet_targets2_consider %>%
  mutate(patchID = factor(patchID, levels =  wet_targets2_sampleorder)) %>%
  arrange(patchID) %>%
  mutate(tot.ha = cumsum(area.ha))

# stop when cumulative sum reaches/exceeds target (only need first one!)
wet_targets2_keepID = wet_targets2_considerorder %>%
  filter(tot.ha <= wet_targets2_considerorder %>% filter(tot.ha > wet_need2) %>%
           slice(1) %>% pull(tot.ha)) %>%
  pull(patchID)

wet_targets2_keep = lapp(wet_targets2_patches,
                         y = wet_targets2_keepID,
                         fun = keep)

# COMBINE PROPOSED WETLAND RESTORATION
# (convert all other patch IDs to NA)
wet_targets_final = cover(wet_targets1_keep, wet_targets2_keep)
writeRaster(wet_targets_final,
            'GIS/restoration_targets_wetland_objectives.tif')


## overlay on baseline layer------------

# combine restoration targets into one layer, coded according to waterbird key
rip_targets_final[rip_targets_final == 1] = 15
wet_targets_final[wet_targets_final == 1] = 18
targets_final = cover(rip_targets_final, wet_targets_final)

# overlay on baseline layer
scenario_restoration <- lapp(
  x = c(veg_baseline_waterbird_fall, targets_final),
  fun = function(x, y) {
    ifelse(!is.na(y), y, x)
  })

writeRaster(scenario_restoration,
            'data/proposed_scenarios/waterbirds/DeltaPlan_restoration_objectives.tif')

# compare/check conversions between layers
tab = crosstab(c(veg_baseline_waterbird_fall %>% mask(delta),
                 scenario_restoration))

tab %>% as_tibble() %>%
  set_names(c('baseline', 'scenario', 'n')) %>%
  mutate_at(vars(baseline:scenario), as.numeric) %>%
  left_join(wkey %>% dplyr::select(code, label), by = c('baseline' = 'code')) %>%
  left_join(wkey %>% dplyr::select(code, label), by = c('scenario' = 'code')) %>%
  arrange(desc(n)) %>%
  group_by(label.y) %>%
  mutate(tot = sum(n),
         prop = n/tot) %>%
  ungroup() %>%
  # filter(label.y == 'duwet')
  filter(label.y == 'woodw')

# - riparian: 64% existing riparian; most conversion from orch (20%);
#    no conversion from rice, wet, duwet, dev, or water
# - wetland: 75% existing duwet; most conversion from ip (6%), rice (5%), dryp (4%);
#    no conversion from wet, dev, forest, water, woodw, or barren

# convert encoding for use with riparian models
scenario_restoration_riparian <- classify(scenario_restoration,
                                     rcl = matrix(c(2, 20, #corn = AG
                                                    3, 30, #rice = RICE
                                                    4, 50, #ip = GRASSPAS
                                                    5, 20, #wheat = AG
                                                    6, 20, #row = AG
                                                    7, 20, #field = AG
                                                    8, 80, #wet = WETLAND
                                                    9, 10, #orch = ORCHVIN
                                                    10, 20, #grain = AG
                                                    11, 40, #fallow = IDLE
                                                    12, 60, #dev = URBAN
                                                    13, 100, #forest = OAKWOODLAND/WOODLAND/SCRUB
                                                    14, 90, #water = WATER
                                                    15, 70, #woodw = RIPARIAN
                                                    16, 50, #dryp = GRASSPAS
                                                    17, 50, #alfalfa = GRASSPAS
                                                    18, 80, #duwet = WETLAND
                                                    99, 130), #barren = BARREN
                                                  byrow = TRUE, ncol = 2))

writeRaster(scenario_restoration_riparian,
            'data/proposed_scenarios/riparian/DeltaPlan_restoration_objectives.tif')

# calculate change in area of each land cover
delta_restoration = calculate_change(baseline = veg_baseline_waterbird_fall,
                                 scenario = scenario_restoration)
delta_restoration %>%
  mutate(label = recode(label,
                         'ip' = 'irrigated pasture',
                         'wheat' = 'grain',
                         'row' = 'row/field crop',
                         'field' = 'row/field crop',
                         'wet' = 'wetland',
                         'orch' = 'orchard/vineyard',
                         'fal' = 'fallow',
                         'dev' = 'urban',
                         'woodw' = 'riparian',
                         'dryp' = 'dryland pasture',
                         'alf' = 'alfalfa',
                         'duwet' = 'wetland')) %>%
  group_by(label) %>%
  summarize(change = sum(change), .groups = 'drop') %>%
  arrange(change) %>%
  plot_change(scale = 1000000) +
  labs(x = NULL, y = bquote(' ' *Delta~ 'total area ('~km^2*')')) +
  theme_bw() + coord_flip() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))
ggsave('fig/delta_restoration.png', height = 7.5, width = 6)
# large increase in riparian and wetland cover, mostly at the expense of
# orchard/vineyard, but also alfalfa, pasture

# compare/check conversions between layers
tab = crosstab(c(veg_baseline_riparian %>% mask(delta),
                 scenario_restoration_riparian))

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
  # filter(group.y == 'WETLAND')
  filter(group.y == 'RIPARIAN')

# - riparian: 64% existing riparian; most conversion from ORCHVIN (20%);
#    no conversion from RICE, URBAN, WETLAND, or WATER
# - wetland: 89% existing WETLAND; most conversion from GRASSPAS (6%), rice (2%);
#    no conversion from URBAN, RIPARIAN, WATER, OAKWOODLAND, BARREN

# SCENARIO 2. Perennial crop expansion---------
# based on the Wilson et al. (2021) BBAU ("bad business as usual") scenario for
# the year 2100, including historically high rates of perennial crop expansion;
# no restoration

skey = read_csv('GIS/State Class Rasters/key.csv')

bbau = rast('GIS/State Class Rasters/scn421.sc.it1.ts2100.tif') %>%
  project(veg_baseline_waterbird_fall, method = 'near') %>%
  mask(veg_baseline_waterbird_fall) %>%
  # keep only the footprint of expanded perennial crops (code=20) - reclassify
  # to waterbird code for orch (9); all others convert to NA
  classify(rcl = matrix(c(20, 9), byrow = TRUE, ncol = 2),
           othersNA = TRUE)
# note: missing southwest corner of Delta

# allow orchard footprint from bbau scenario to cover baseline footprint, except
#  where baseline is a land cover that shouldn't change to orchard
#   dev (12), water (14), woodw (15), wetland (8), duwet (18)
# (all else unchanged)

exclude = classify(veg_baseline_waterbird_fall,
                   rcl = matrix(c(8, 8,
                                  12, 12,
                                  14, 14,
                                  15, 15,
                                  18, 18), byrow = TRUE, ncol = 2),
                   othersNA = TRUE)

scenario_orchard = cover(bbau, veg_baseline_waterbird_fall)
scenario_orchard_refine = cover(exclude, scenario_orchard)

# but allow certain baseline pixels to over-rule a change to orchard:
scenario_orchard_refine <- lapp(
    x = c(scenario_orchard, veg_baseline_waterbird_fall),
    fun = function(x, y) {
      ifelse (!is.na(x) & y %in% c(8, 12, 14, 15, 18), y, x)}
    )


# calculate change in area of each land cover
delta_orchard = calculate_change(baseline = veg_baseline_waterbird_fall,
                                 scenario = scenario_orchard_refine)
plot_change(delta_orchard, scale = 1000000) +
  labs(x = NULL, y = 'km^2') + theme_bw()
# large increase in orchard cover, at the expense of most others, especially
# row, alfalfa, corn, fallow, but also water and wet
delta_orchard %>%
  mutate(label = recode(label,
                        'ip' = 'irrigated pasture',
                        'wheat' = 'grain',
                        'row' = 'row/field crop',
                        'field' = 'row/field crop',
                        'wet' = 'wetland',
                        'orch' = 'orchard/vineyard',
                        'fal' = 'fallow',
                        'dev' = 'urban',
                        'woodw' = 'riparian',
                        'dryp' = 'dryland pasture',
                        'alf' = 'alfalfa',
                        'duwet' = 'wetland')) %>%
  group_by(label) %>%
  summarize(change = sum(change), .groups = 'drop') %>%
  arrange(change) %>%
  plot_change(scale = 1000000) +
  labs(x = NULL, y = bquote(' ' *Delta~ 'total area ('~km^2*')')) +
  theme_bw() + coord_flip() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))
ggsave('fig/delta_orchard.png', height = 7.5, width = 6)

levels(scenario_orchard_refine) <- wkey %>% select(id = code, label) %>%
  as.data.frame()
coltab(scenario_orchard_refine) <- wkey %>% select(code) %>%
  mutate(col = c('#FFFF00', '#FF1493', '#2E8B57', '#FFA54F', '#FA8072',
                 '#FA8072', '#00008B', '#9400D3', '#FFA54F', '#CCCCCC',
                 '#4D4D4D', '#8B4513', '#87CEFA', '#FF0000', '#FFE4C4',
                 '#32CD32', '#FFFFFF', '#00008B')) %>%
  complete(code = c(0:255)) %>% pull(col)

plot(c(veg_baseline_waterbird_fall, scenario_orchard_refine))
writeRaster(scenario_orchard_refine,
            'data/proposed_scenarios/waterbirds/orchard_expansion.tif')
# Note: levels may not be saved, but colors are?


# compare
tab = crosstab(c(veg_baseline_waterbird_fall, scenario_bbau_fill))
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



# SCENARIO X. Projected land use------
# under BBAU ("bad business as usual") scenario for the year 2100: historical
# regional land conversion rates continue (orchard expansion, urban devel); no
# restoration

skey = read_csv('GIS/State Class Rasters/key.csv')
ncell(veg_baseline_waterbird_fall[!is.na(veg_baseline_waterbird_fall)]) #7257416

scenario_stack = rast(
  paste0('GIS/State Class Rasters/',
         list.files('GIS/State Class Rasters/', 'tif$'))
  )
names(scenario_stack) = c('DUST', 'DREAM', 'HBAU', 'BBAU', 'EEM')
# in a different projection, and with a different resolution than our baseline landscape


scenario_stack_proj = scenario_stack %>%

  # first crop in the original projection to make the file smaller
  crop(veg_baseline_riparian %>% project(scenario_stack)) %>%

  # disaggregate to 30m pixels (staying in original projection)
  disaggregate(fact = 270/30) %>%

  # project to baseline crs; use "near" so we keep classifications intact (no
  # bilinear interpolation)
  project(veg_baseline_riparian, method = 'near') %>%

  # exclude areas outside the 10km buffer
  mask(veg_baseline_riparian)


writeRaster(scenario_stack_proj,
            filename = paste0('GIS/projected_landuse_2100_',
                              names(scenario_stack_proj),
                              '.tif'))

plot(scenario_stack_proj, col = viridis::inferno(32)) #differences appear subtle
ncell(scenario_stack$DUST[!is.na(scenario_stack$DUST)]) #5771648

# compare baseline vs. BBAU: (mask out missing SW corner of Delta)
tab = crosstab(c(veg_baseline_waterbird_fall %>% mask(scenario_stack_proj$BBAU),
                 scenario_stack_proj$BBAU))
tab_bbau = tab %>% as_tibble() %>%
  set_names(c('baseline', 'bbau', 'n')) %>%
  mutate_at(vars(baseline:bbau), as.numeric) %>%
  left_join(wkey %>% dplyr::select(code, baseline_landuse = label),
            by = c('baseline' = 'code')) %>%
  left_join(skey %>% dplyr::select(code, bbau_landuse = label),
            by = c('bbau' = 'code')) %>%
  group_by(bbau_landuse) %>%
  mutate(tot = sum(n),
         prop = n/tot) %>%
  ungroup() %>%
  arrange(desc(n))

tab_bbau %>%
  filter(!(bbau_landuse == baseline_landuse) &
           !(bbau_landuse == 'grass/shrub' & baseline_landuse == 'dryp')) %>%
  # filter(!is.na(hbau_landuse)) %>% #area not included by the scenario
  ggplot(aes(x = bbau_landuse, y = baseline_landuse, fill = n)) +
  geom_tile() +
  scale_fill_gradientn(name = '', colors = topo.colors(10))

tab_bbau %>%
  filter(!is.na(bbau_landuse) & bbau_landuse != baseline_landuse &
           !(bbau_landuse == 'grass/shrub' & baseline_landuse == 'dryp') &
           !(bbau_landuse == 'pasture' & baseline_landuse == 'ip') &
           !(bbau_landuse == 'seas' & baseline_landuse == 'duwet')) %>%
  # filter(bbau_landuse == 'orch') %>%
  # filter(bbau_landuse == 'dev') %>%
  arrange(desc(n))
# - orch: 52% from existing orchard; most conversion from row (9%), alf (7%),
#     corn (7%), fal (5%); some conversion from woodw, water, wet, rice, duwet
# - dev: 61% from existing dev; most conversion from orch (10%), dryp (6%), fal
#    (4%); includes conversion from woodw, wet, forest, rice, duwet

# --> some inconsistencies introduced because conversions based on progressing
# from 2007 VegCAMP layer




# # # compare to each other and to new riparian/waterbird baseline:
# # tab1 = crosstab(scenario_stack$DUST, scenario_stack$DREAM)
# # tab_extremes = tab1 %>% as_tibble() %>%
# #   mutate_at(vars(DUST:DREAM), as.numeric) %>%
# #   left_join(skey %>% dplyr::select(code, dream_landuse = label),
# #             by = c('DREAM' = 'code')) %>%
# #   left_join(skey %>% dplyr::select(code, dust_landuse = label),
# #             by = c('DUST' = 'code')) %>%
# #   mutate(perc = n / 5771648 * 100) %>%
# #   arrange(desc(n))
# #
# # tab_extremes %>%
# #   filter(!(dream_landuse == dust_landuse)) %>%
# #   ggplot(aes(x = dream_landuse, y = dust_landuse, fill = n)) +
# #   geom_tile() +
# #   scale_fill_gradientn(name = '', colors = topo.colors(10))
# #
# # tab_extremes %>%
# #   filter(!(dream_landuse == dust_landuse)) %>%
# #   arrange(desc(n))
# # # largest differences are: (DREAM vs DUST)
# # # - dev vs. corn;  corn vs. dev
# # # - dev vs. orch; orch vs. dev
# # # - dev vs. grass/shrub; grass/shrub vs. dev
# # # - dev vs. seas
# # # --> but all differences relatively small: < 1%
# #
# # tab2 = crosstab(scenario_stack$BBAU, scenario_stack$DREAM)
# # tab_extremes2 = tab2 %>% as_tibble() %>%
# #   mutate_at(vars(BBAU:DREAM), as.numeric) %>%
# #   left_join(skey %>% dplyr::select(code, dream_landuse = label),
# #             by = c('DREAM' = 'code')) %>%
# #   left_join(skey %>% dplyr::select(code, bbau_landuse = label),
# #             by = c('BBAU' = 'code')) %>%
# #   mutate(perc = n / 5793303 * 100) %>%
# #   arrange(desc(n))
# #
# # tab_extremes2 %>%
# #   filter(!(dream_landuse == bbau_landuse)) %>%
# #   ggplot(aes(x = dream_landuse, y = bbau_landuse, fill = n)) +
# #   geom_tile() +
# #   scale_fill_gradientn(name = '', colors = topo.colors(10))
# #
# # tab_extremes2 %>%
# #   filter(!(dream_landuse == bbau_landuse)) %>%
# #   arrange(desc(n))
# # # largest differences are (DREAM vs BBAU)
# # # - dev vs. corn; corn vs. dev (equal amts though)
# # # - dev vs. orchard > orch vs. dev
# # # - grass/shrub vs. dev > dev vs. grass/shrub
# # # --> but all differences relatively small: < 1%
#
#
#
#
# # total acres? but remember that scenarios exclude southwest corner
# veg_baseline_mask = mask(veg_baseline_waterbird_fall, scenario_stack$BBAU)
#
# btot = getValues(veg_baseline_mask) %>% as.factor() %>% summary() %>%
#   as_tibble(rownames = 'code') %>%
#   mutate(code = as.numeric(code),
#          area.ha = value * 30 * 30 / 10000) %>%
#   filter(!is.na(code))
#
# bbautot = getValues(scenario_stack$BBAU) %>% as.factor() %>% summary() %>%
#   as_tibble(rownames = 'code') %>%
#   mutate(code = as.numeric(code),
#          area.ha = value * 30 * 30 / 10000) %>%
#   filter(!is.na(code))
#
# dusttot = getValues(scenario_stack$DUST) %>% as.factor() %>% summary() %>%
#   as_tibble(rownames = 'code') %>%
#   mutate(code = as.numeric(code),
#          area.ha = value * 30 * 30 / 10000) %>%
#   filter(!is.na(code))
#
# dreamtot = getValues(scenario_stack$DREAM) %>% as.factor() %>% summary() %>%
#   as_tibble(rownames = 'code') %>%
#   mutate(code = as.numeric(code),
#          area.ha = value * 30 * 30 / 10000) %>%
#   filter(!is.na(code))

# bind_rows(
#   btot %>%
#     left_join(wkey) %>%
#     mutate(label = case_when(label %in% c('dryp', 'woodw') ~ 'dryp & woodw',
#                              label %in% c('fal', 'row', 'field') ~ 'fal & row & field',
#                              label %in% c('grain', 'wheat') ~ 'grain & wheat',
#                              TRUE ~ label)) %>%
#     group_by(label) %>%
#     summarize(area.ha = sum(area.ha)) %>%
#     mutate(source = 'baseline'),
#   bbautot %>%
#     left_join(skey) %>%
#     mutate(label = case_when(label %in% c('semiperm', 'seas') ~ 'duwet',
#                              label == 'wetother' ~ 'wet',
#                              label == 'grass/shrub' ~ 'dryp & woodw',
#                              label == 'row' ~ 'fal & row & field',
#                              label == 'pasture' ~ 'ip',
#                              label == 'grain' ~ 'grain & wheat',
#                              TRUE ~ label)) %>%
#     group_by(label) %>%
#     summarize(area.ha = sum(area.ha)) %>%
#     mutate(source = 'BBAU'),
#   dusttot %>%
#     left_join(skey) %>%
#     mutate(label = case_when(label %in% c('semiperm', 'seas') ~ 'duwet',
#                              label == 'wetother' ~ 'wet',
#                              label == 'grass/shrub' ~ 'dryp & woodw',
#                              label == 'row' ~ 'fal & row & field',
#                              label == 'pasture' ~ 'ip',
#                              label == 'grain' ~ 'grain & wheat',
#                              TRUE ~ label)) %>%
#     group_by(label) %>%
#     summarize(area.ha = sum(area.ha)) %>%
#     mutate(source = 'DUST'),
#   dreamtot %>%
#     left_join(skey) %>%
#     mutate(label = case_when(label %in% c('semiperm', 'seas') ~ 'duwet',
#                              label == 'wetother' ~ 'wet',
#                              label == 'grass/shrub' ~ 'dryp & woodw',
#                              label == 'row' ~ 'fal & row & field',
#                              label == 'pasture' ~ 'ip',
#                              label == 'grain' ~ 'grain & wheat',
#                              TRUE ~ label)) %>%
#     group_by(label) %>%
#     summarize(area.ha = sum(area.ha)) %>%
#     mutate(source = 'DREAM')
# ) %>%
#   ggplot(aes(label, area.ha/1000, fill = source)) +
#   geom_col(position = position_dodge())

# biggest changes from baseline:
# ALL SCENARIOS: more dev, less dryp & woodw, less ip, less water, more wet
# BBAU: more orch; less alf, corn, fal/row/field, grain/wheat, rice
# DREAM: more orch (but less of an increase), duwet; less alf, corn,
#    fal/row/field, grain/wheat, rice
# DUST: more alf, corn; less orch; less reduction in dryp/woodw, grain/wheat, ip, rice

#--> note that riparian is lost completely at this resolution, and/or lumped in
# with grass/shrub; propose to overlay baseline riparian, and possibly
# dev, orchard, du wetlands, water just to make sure these don't change.

#--> other issue: missing southwest corner and 10km buffer. A lot of it is
# developed and then grasslands in the hills; can just fill in and assume no
# change?

#--> plus: field/row crops combined; grain/winter wheat combined and no way to
# pull out separate winter map

scenario_bbau <- classify(scenario_stack_proj$BBAU,
                          rcl = matrix(
                            c(1, 14, #water
                              2, 12, #dev
                              3, 99, #barren
                              4, 13, #forest
                              5, 16, #grass/shrub/riparian - dryp
                              10, 6, #field/row - row
                              11, 4, #pasture - ip
                              12, 17, #alf
                              13, 10, #grain
                              14, 2, #corn
                              15, 3, #rice
                              20, 9, #orch/vin
                              30, 18, #semiperm - DUwet
                              31, 18, #seas - DUwet
                              32, 8 #other/general wetland
                            ),
                            byrow = TRUE, ncol = 2)
)

# allow certain baseline pixels to over-rule scenario pixels:
#   orch (9), dev (12), water (14), woodw (15), wetland (8), duwet (18)
scenario_bbau_overrule <- lapp(
  x = c(scenario_bbau, veg_baseline_waterbird_fall),
  fun = function(x, y) {
    ifelse (!is.na(x) & x == 12 & y %in% c(9, 15),
            # allow orch (9) and woodw (15) get converted to dev (12)
            x,
            # otherwise, don't let new water or wetlands overwrite baseline land cover;
            #   let baseline pixels overrule scenario pixels for
            #   wetland (8), orch (9), dev (12), water(14), woodw(15), duwet(18)
            #   (i.e., no conversion from these land cover types)
            ifelse(!is.na(x) &
                     ((x == 14 & y != 14) |
                        (x %in% c(8, 18) & !(y %in% c(8, 18))) |
                        y %in% c(8, 9, 12, 14, 15, 18)),
                   y,
                   # for all others, use scenario pixel value
                   x))
  })

# then fill in baseline values where missing (primarily in southwest corner)
scenario_bbau_fill = cover(scenario_bbau_overrule, veg_baseline_waterbird_fall)
levels(scenario_bbau_fill) <- wkey %>% select(id = code, label) %>% as.data.frame()
coltab(scenario_bbau_fill) <- wkey %>% select(code) %>%
  mutate(col = c('#FFFF00', '#FF1493', '#2E8B57', '#FFA54F', '#FA8072',
                 '#FA8072', '#00008B', '#9400D3', '#FFA54F', '#CCCCCC',
                 '#4D4D4D', '#8B4513', '#87CEFA', '#FF0000', '#FFE4C4',
                 '#32CD32', '#FFFFFF', '#00008B')) %>%
  complete(code = c(0:255)) %>% pull(col)
plot(scenario_bbau_fill)
writeRaster(scenario_bbau_fill,
            'data/proposed_scenarios/waterbirds/projected_landuse_2100_BBAU_labeled.tif')
# Note: levels may not be saved, but colors are?

# compare
tab = crosstab(c(veg_baseline_waterbird_fall, scenario_bbau_fill))
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

# - dev: 78% from existing dev; most conversion from orch (8%), dryp (5%);
#     none from water, woodw, duwet
# - orch: 67% from existing orchard; most conversion from row (7%), alf (6%),
#     corn (5%); none from dev, water, woodw, duwet

# change in acreage by land cover type

calculate_area = function(rast) {
  terra::freq(rast) %>% as_tibble() %>%
    mutate(area = count * prod(terra::res(rast)))
}

calculate_change = function(baseline, scenario, key = NULL, type = 'area', sort = TRUE) {
  if (type == 'area') {
    base = calculate_area(baseline)
    scen = calculate_area(scenario)
    res = full_join(base, scen, by = c('layer', 'value'),
              suffix = c('.base', '.scen')) %>%
      mutate(change = area.scen - area.base)
    if (!is.null(key)) {
      res = left_join(res, key, by = 'value')
    }
  }
  if (sort) {
    res = res %>% arrange(change)
  }
  return(res)
}

plot_change = function(df, scale = 1, col = c('orange', 'blue')) {
  if (!'label' %in% names(df)) {
    df = df %>% mutate(label = as.factor(value))
  } else {
    df = df %>% mutate(label = factor(label, levels = df$label))
  }
  if (!is.null(col)) {
    df = df %>% mutate(bin = if_else(change < 0, 'decrease', 'increase'),
                       bin = as.factor(bin))
    ggplot(df, aes(label, change/scale, fill = bin)) + geom_col() +
      scale_fill_manual(values = col) + guides(fill = 'none')
  } else {
    ggplot(df, aes(label, change/scale)) + geom_col()
  }

}
delta = calculate_change(baseline = veg_baseline_waterbird_fall,
                         scenario = scenario_bbau_fill,
                         key = wkey %>% rename(value = code))
plot_change(delta, scale = 1000000) + labs(x = NULL, y = 'km^2') +
  theme_bw()
# no change in water, wet, duwet
# increase primarily in: dev, orch; also grain
# decrease primarily in: alf, corn, dryp, fal, ip, wheat; also woodw, row/field, rice, forest

scenario_bbau_riparian <- classify(scenario_bbau_fill,
                                   rcl = matrix(c(2, 20, #corn = AG
                                                  3, 30, #rice = RICE
                                                  4, 50, #ip = GRASSPAS
                                                  5, 20, #wheat = AG
                                                  6, 20, #row = AG
                                                  7, 20, #field = AG
                                                  8, 80, #wet = WETLAND
                                                  9, 10, #orch = ORCHVIN
                                                  10, 20, #grain = AG
                                                  11, 40, #fallow = IDLE
                                                  12, 60, #dev = URBAN
                                                  13, 100, #forest = OAKWOODLAND/WOODLAND/SCRUB
                                                  14, 90, #water = WATER
                                                  15, 70, #woodw = RIPARIAN
                                                  16, 50, #dryp = GRASSPAS
                                                  17, 50, #alfalfa = GRASSPAS
                                                  18, 80, #duwet = WETLAND
                                                  99, 130), #barren = BARREN
                                                byrow = TRUE, ncol = 2)
)
plot(scenario_bbau_riparian)
writeRaster(scenario_bbau_riparian,
            'data/proposed_scenarios/riparian/projected_landuse_2100_BBAU.tif')

# change in acreage by riparian land cover type
area_baseline_rip = values(veg_baseline_riparian) %>% as.factor() %>% summary() %>%
  as_tibble(rownames = 'code') %>%
  mutate(code = as.numeric(code),
         area.ha = value * 30 * 30 / 10000) %>%
  filter(!is.na(code)) %>%
  left_join(rkey %>% dplyr::select(code, baseline = group),
            by = 'code')

area_bbau_rip = values(scenario_bbau_riparian) %>% as.factor() %>% summary() %>%
  as_tibble(rownames = 'code') %>%
  mutate(code = as.numeric(code),
         area.ha = value * 30 * 30 / 10000) %>%
  filter(!is.na(code)) %>%
  left_join(rkey %>% dplyr::select(code, bbau = group),
            by = 'code')

full_join(area_baseline_rip, area_bbau_rip, by = c('baseline' = 'bbau')) %>%
  mutate(diff = area.ha.y - area.ha.x) %>%
  ggplot(aes(baseline, diff)) + geom_col()
# no change in water, wetland
# increase primarily in: URBAN, ORCHVIN
# decrease primarily in: GRASSPAS, IDLE, AG; also RIPARIAN, RICE, OAKWOODLAND, BARREN

# SCENARIO 3. Flood risk?-----------
# first combine separate layers representing flood risk
filelist = paste0('GIS/DeltaAdapts/',
                  list.files('GIS/DeltaAdapts',
                             'baseline_.*shp$|floodrisk2085_.*shp$'))

tmp = purrr::map(filelist,
                 ~st_read(.) %>%
                   fasterize::fasterize(veg_baseline_riparian,
                                        field = 'flood_risk')) %>%
  stack()
maxfloodrisk = max(tmp, na.rm = TRUE)
plot(tmp)
plot(maxfloodrisk)

writeRaster(maxfloodrisk, 'GIS/floodrisk2085', format = 'GTiff')

# suitable veg per elevation:
elev = read_csv('data/SFEI_DSLPT/elevation_notifications_v5.csv')

elev %>%
  dplyr::select(Habitat_type, Elevation_band, Flag_type) %>%
  filter(Habitat_type != 'Stabilized interior dune vegetation') %>%
  pivot_wider(values_from = Flag_type, names_from = Elevation_band) %>%
  dplyr::select(Habitat_type, `-2`, `-1`, `0`, `1`, `2`)

elev %>%
  dplyr::select(Habitat_type, Elevation_zone_DeltaPlan, Flag_type) %>%
  filter(Habitat_type != 'Stabilized interior dune vegetation') %>%
  pivot_wider(values_from = Flag_type, names_from = Elevation_zone_DeltaPlan) %>%
  dplyr::select(Habitat_type, 'Deeply subsided (more than 8 ft below MLLW)',
                'Shallowly subsided (up to 8 feet below MLLW)',
                'Intertidal (between MLLW and MHHW)',
                'Sea level rise projection (0 to +10 ft MHHW)',
                'Floodplain (more than 10 feet MHHW)')


elev %>%
  dplyr::select(Elevation_zone_SFEI, Elevation_zone_DeltaPlan) %>%
  table()
# SFEI: Terrestrial; DeltaPlan: Deeply subsided?
# SFEI: Terrestrial, DeltaPlan: Intertidal


# INTERACTIVE MAP-----------
library(leaflet)

# combined scenario rasters
scenarios = c(veg_baseline_waterbird_fall,
              scenario_restoration,
              scenario_orchard_refine) %>%
  project(y = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", method = 'near')
scenarios = raster::stack(scenarios) #convert to raster stack
names(scenarios) = c('baseline', 'restoration scenario', 'orchard expansion scenario')

# # resample rasters to prevent the file size from being too enormous (this is
# # still fairly hi-res)
# scenarios_sampled_hi = pred_cv %>%
#   terra::spatSample(size = 5000000, as.raster = TRUE)


# color palette
key <- wkey %>%
  mutate(color = c('#FFFF00', '#FF1493', '#2E8B57', '#FFA54F', '#FA8072',
                   '#FA8072', '#00008B', '#9400D3', '#FFA54F', '#CCCCCC',
                   '#4D4D4D', '#8B4513', '#87CEFA', '#FF0000', '#FFE4C4',
                   '#32CD32', '#FFFFFF', '#00008B'),
         label2 = recode(label,
                        'ip' = 'irrigated pasture',
                        'wheat' = 'grain',
                        'row' = 'row/field crop',
                        'field' = 'row/field crop',
                        'wet' = 'wetland',
                        'orch' = 'orchard/vineyard',
                        'fal' = 'fallow',
                        'dev' = 'urban',
                        'woodw' = 'riparian',
                        'dryp' = 'dryland pasture',
                        'alf' = 'alfalfa',
                        'duwet' = 'wetland'),
         label2 = factor(label2,
                        levels = c('wetland',
                                   'riparian',
                                   'corn',
                                   'rice',
                                   'alfalfa',
                                   'irrigated pasture',
                                   'dryland pasture',
                                   'grain',
                                   'row/field crop',
                                   'orchard/vineyard',
                                   'forest',
                                   'fallow',
                                   'urban',
                                   'barren',
                                   'water'))
         ) %>%
  arrange(code)

key_simple = key %>% select(label2, color) %>%
  distinct() %>%
  arrange(label2)

# for waterbird landcover classifications
pal <- leaflet::colorFactor(
  palette = key$color,
  domain = key$code,
  alpha = TRUE,
  na.color = 'transparent')


## base map with legends

delta_poly = read_sf('V:/Data/geopolitical/california/Legal_Delta_Boundary/Legal_Delta_Boundary.shp') %>%
  st_transform(crs(scenarios))

m <- leaflet(delta_poly) %>%

  # background terrain
  addProviderTiles("Stamen.TonerLite") %>%

  # add layers control
  addLayersControl(position = 'bottomleft',
                   options = layersControlOptions(collapsed = F),
                   baseGroups = names(scenarios)) %>%
  # add legend
  addLegend("topright",
            labels = key_simple$label2,
            colors = key_simple$color,
            title = "Land cover class",
            opacity = 1)

## add rasters
for (i in c(1:raster::nlayers(scenarios))) {
  m <- m %>%
    addRasterImage(x = scenarios[[i]],
                   group = names(scenarios)[i],
                   colors = pal, opacity = 0.7,
                   project = FALSE)
}

## add delta boundary
m <- m %>% addPolygons(fill = FALSE, color = 'black', weight = 2, opacity = 1)

## save the interactive map as an html widget
# --> you may want to change this to "selfcontained = TRUE", which would allow
# you to email the html file to anyone to view -- the file size just might be super large!

htmlwidgets::saveWidget(m,
                        'docs/draft_scenarios.html',
                        selfcontained = FALSE, libdir = 'lib',
                        title = 'Draft scenarios of landcover change in the Delta')

