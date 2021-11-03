# README---------
# From baseline land cover layer, develop scenario of meeting Delta Plan
# restoration objectives for riparian vegetation and managed/seasonal wetlands.

# PACKAGES & FUNCTIONS
source('R/packages.R')

# reference data:
delta = rast('GIS/boundaries/delta.tif')
baseline = rast('GIS/landscape_rasters/veg_baseline_fall.tif')
wkey = read_csv('data/landcover_key_waterbirds.csv', col_types = cols())
rkey = read_csv('data/landcover_key_riparian.csv', col_types = cols())

# Scenario development approach:
#
# Restoring habitat to meet (some of) the proposed objectives in the draft
# amendment to Chapter 4 of the Delta Plan, specifically for riparian habitat
# and seasonal wetlands. The objectives are less than the total area of
# potential habitat, so distribute potential restored acres by:
#
# a) including only pixels within the Legal Delta boundary that are identified
# as potential habitat by the "restoration opportunities" analysis, and are not
# already wetland or riparian habitat in the baseline veg layer
#
# b) prioritizing pixels in priority restoration areas (as defined by the Delta
# Stewardship Council, i.e. excluding areas with a high risk of becoming
# flooded by sea level rise)
#
# c) within priority restoration area, prioritizing pixels on protected land,
# easements, public/quasi-public land, or open space, and excluding areas
# designated for development and areas that are already urban in the baseline
# veg layer
#--> considering also excluding areas that are already orchard/vineyard
#--> consider excluding easements -- some have to stay in ag?

# restoration objectives------
# from proposed performance measures to the Delta Plan
obj = tibble(type = c('seasonal wetland, wet meadow, nontidal wetland',
                      'willow riparian scrub/shrub, valley foothill riparian, willow thicket'),
             target_acres = c(19000, 16300),
             total_acres = c(24100, 30500)) %>%
  mutate(target_ha = target_acres / 2.47105,
         total_ha = total_acres / 2.47105)


# current totals (from baseline layer)
base = baseline %>%
  mask(delta) %>%
  freq() %>%
  filter(label %in% c('riparian', 'managed wetland')) %>%
  mutate(current_ha = count * 30 * 30 / 10000)
# riparian = 8,218 ha
# wetlands = 5,709 ha

# how much more needed to meet proposed 2050 targets?
targets = full_join(
  obj %>% mutate(label = c('managed wetland', 'riparian')) %>%
    select(label, total_ha),
  base %>% select(label, current_ha),
  by = 'label') %>%
  mutate(add_ha = total_ha - current_ha)
# wetlands: add 4,044 ha
# riparian: add 4,125 ha


# source data---------
## restoration plans
plans = rast('GIS/scenario_inputs/restoration_plans.tif')

## restoration potential
restoration_stack = rast(
  c(#potential: 7100 = valley foothill riparian, 7600 = willow shrub/scrub, 8000 = wetland
    'GIS/scenario_inputs/restoration_potential.tif',
    #priority areas: 100 = priority
    'GIS/scenario_inputs/restoration_priority_areas.tif',
    #zoning: 10 = open space/recreation; 20 = public/quasi=public; 90 = designated for development
    'GIS/scenario_inputs/zoning.tif',
    #protected areas: 1 = protected, 2 = easement
    'GIS/scenario_inputs/protected_areas.tif')
  ) %>%
  mask(delta)

# simplify riparian coding
restoration_stack[[1]] = classify(restoration_stack[[1]],
                                  rcl = matrix(c(7100, 7000,
                                                 7600, 7000),
                                               byrow = TRUE, ncol = 2))
# don't distinguish between public space & open space
restoration_stack[[3]] = classify(restoration_stack[[3]],
                                  rcl = matrix(c(20, 10), byrow = TRUE, ncol = 2))
# # don't distinguish between protected vs. easement
# restoration_stack[[4]] = classify(restoration_stack[[4]],
#                                   rcl = matrix(c(2, 1), byrow = TRUE, ncol = 2))

restoration_sum = sum(restoration_stack, na.rm = TRUE) %>%
  # eliminate those not in restoration potential areas
  subst(from = c(0:200), to = NA)
freq(restoration_sum)

# 1. overlay restoration plans on baseline-------
# check conversions:
tab = crosstab(c(baseline, plans), useNA = TRUE, long = TRUE)
tab %>% rlang::set_names(c('baseline', 'plans', 'n')) %>%
  left_join(wkey %>% select(baseline = WATERBIRD_CODE, from = label)) %>%
  left_join(wkey %>% select(plans = WATERBIRD_CODE, to = label)) %>%
  filter(!is.na(plans)) %>%
  arrange(plans, desc(n))
# existing restoration plans will convert:
# - to riparian from: fallow, grassland, existing riparian; some pixels from
# water, other wetland, urban
# - to wetland from: pasture, other wetland, corn, rice, fallow, grassland...;
# some pixels from riparian, existing managed wetland

# --> allow conversion from urban to riparian for urban restoration projects

base_plus_plans = cover(plans, baseline)

# * checkpoint: calculate how much still needed----
change_plans = base_plus_plans %>% mask(delta) %>%
  freq() %>% as_tibble() %>%
  filter(value %in% c(15, 18)) %>%
  mutate(label = recode(value,
                        '15' = 'riparian', '18' = 'managed wetland')) %>%
  mutate(plans_ha = count * 30 * 30 / 10000)

targets_plus = full_join(targets %>% select(label, total_ha),
                         change_plans %>% select(label, plans_ha),
                         by = 'label') %>%
  mutate(add_ha = total_ha - plans_ha)
# wetlands: still need 2337 ha
# riparian: still need 3708 ha

# 2. prioritize potential restoration areas-------

## group pixels into priority levels--------
# 7000 = potential riparian; 8000 = potential seasonal wetland
# 100 = priority restoration area
# 10 = open space/recreation/public/quasi-public; 90 = designated for development
# 1 = protected; 2 = easement

restoration_targets = classify(
  restoration_sum,
  rcl = matrix(
    c(# potential riparian pixels:
      7111, 1, #priority area, public space, protected

      7110, 2, #priority area, public/open space, but not protected
      7101, 2, #priority area, not public/open, but protected

      # remaining priority area pixels (including easements):
      7112, 3, #priority area, public space, easement
      7100, 3, #priority area, but not public/open or protected/easement
      7102, 3, #priority area, not public/open, but easement

      # if needed:
      7011, 4, #not priority area, but public/open space and protected
      7012, 4, #not priority area, but public/open space and easement

      # avoid unless necessary:
      7010, 5, #not priority, but public/open space; but not protected/easement
      7001, 5, #not priority, not public/open space, but protected
      7002, 5, #not priority, not public/open space, but easement

      7000, 6, #not priority, not open/public, not protected

      # exclude:
      7190, 9, #priority, but designated for development (only 4 pixels)
      7090, 9, #not priority, designated for development (the majority of pixels designated for development)
      7091, 9, #not priority, designated for development, but also protected? (551 pixels)

      # potential wetland pixels: (same coding structure as above, but plus 10)
      # --> code easements separately, since they could be limited
      8111, 11,

      8110, 12,
      8101, 12,

      8112, 13,
      8100, 13,
      8102, 13,

      8011, 14,

      8010, 15,
      8001, 15,
      8002, 15,

      8000, 16,

      8090, 19,
      8091, 19), byrow = TRUE, ncol = 2))

# compare to baseline plus restoration plans
tab = crosstab(c(base_plus_plans, restoration_targets),
               useNA = TRUE, long = TRUE)
tab %>% rlang::set_names(c('base', 'new', 'n')) %>%
  left_join(wkey %>% select(base = WATERBIRD_CODE, from = label)) %>%
  mutate(to = case_when(new %in% c(1:4) ~ 'riparian',
                        new %in% c(5:6) ~ 'riparian2',
                        new %in% c(9, 19) ~ 'development',
                        new %in% c(11:14) ~ 'wetland',
                        new %in% c(15:16) ~ 'wetland2')) %>%
  group_by(to, from) %>%
  summarize(n = sum(n, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(to)) %>%
  arrange(to, desc(n)) %>% print(n = 100)
# to riparian largely from grassland, fallow, orch/vin; also corn, field/row,
#   grain, alfalfa...
# to riparian2 largely from orch/vin

# to wetland largely from pasture, existing managed wetland, fallow
# to wetland2 largely from pasture, orch/vin

# don't allow "restoration" of existing managed wetland, tidal marsh, water, or
# riparian; also exclude existing forest/shrub and urban; deprioritize existing orch/vin
restoration_targets_limited = lapp(c(restoration_targets, base_plus_plans),
                                   function(x, y) {
                                     ifelse(!is.na(x) & y %in% c(12:15, 18:19),
                                            NA,
                                            ifelse(!is.na(x) & y == 9,
                                                   x + 20,
                                                   x))
                                   })

# restoration_targets_limited = lapp(c(restoration_targets, base_plus_plans),
#                                    function(x, y) {
#                                      ifelse(!is.na(x) & y %in% c(12:15, 18:19),
#                                             NA,
#                                             ifelse(x %in% c(1:7) & y == 9,
#                                                    8,
#                                                    ifelse(x %in% c(11:17) & y == 9,
#                                                           18,
#                                                           x)))
#                                    })
tab = crosstab(c(base_plus_plans, restoration_targets_limited),
               useNA = TRUE, long = TRUE)
tab %>% rlang::set_names(c('base', 'new', 'n')) %>%
  left_join(wkey %>% select(base = WATERBIRD_CODE, from = label)) %>%
  mutate(to = case_when(new %in% c(1:4) ~ 'riparian',
                        new %in% c(5:7) ~ 'riparian2',
                        new %in% c(21:27) ~ 'riparian3',
                        new %in% c(11:14) ~ 'wetland',
                        new %in% c(15:17) ~ 'wetland2',
                        new %in% c(32:37) ~ 'wetland3',
                        new %in% c(9, 19, 29, 39) ~ 'development')) %>%
  group_by(to, from) %>%
  summarize(n = sum(n, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(to)) %>%
  arrange(to, desc(n)) %>% print(n = 51)
# to riparian primarily from grassland, fallow; not wetland, urban, or orch/vin
# to riparian2 primarily from grassland, fallow, field/row, grain
# to riparian3 entirely from existing orch/vin
# to wetland primarily from pasture
# to wetland2 primarily from pasture
# to wetland3 entirely from existing orch/vin

# all potential riparian pixels assigned a priority level 1-6, 9, or 22-29 and
#  potential wetland pixels assigned 11-16, 19, or 32-39
writeRaster(restoration_targets_limited,
            'GIS/scenario_inputs/restoration_targets.tif',
            overwrite = TRUE)

# how much potential is this relative to restoration objectives?
freq(restoration_targets_limited) %>% as_tibble() %>%
  mutate(label = case_when(value %in% c(1:9, 21:29) ~ 'riparian',
                           value %in% c(11:19, 31:39) ~ 'managed wetland'),
         area.ha = count * 30 * 30 / 10000,
         priority = case_when(value %in% c(9, 29, 19, 39) ~ 50,
                              TRUE ~ value)) %>%
  arrange(label, priority) %>%
  group_by(label) %>%
  mutate(tot.ha = cumsum(area.ha)) %>%
  print(n = 50)
# to reach wetland target of 2337 additional ha, need all of priority
# levels 1-2 and some of 3
# to reach riparian target of 3708 additional ha (while excluding existing
# orchard and vineyard), need all of priority levels 1-6 plus 22-25 and some of
# 26!


## top priority restoration-------
# start with priority levels for which entire level is needed (as above), but
# exclude very small patches of potential restoration (minimum 1 acre)

# find all pixels in top priority levels:
targets1 = restoration_targets_limited %>%
  subst(c(26, 9, 29, 13:19, 32:39), NA) %>%
  subst(c(1:6, 22:25), 15) %>%
  subst(c(11:12), 18) %>%
  segregate(keep = TRUE, other = NA)

# find all patches of at least 1 acre (0.4 ha): base on original land cover
# class --> hopefully identifying fields as potential restoration units (instead
# of giant blocks)

# rip patches
targets1_rip = mask(baseline, targets1[[1]]) %>%
  segregate(keep = TRUE, other = NA) %>%
  patches(directions = 8)

# wetland patches
targets1_wet = mask(baseline, targets1[[2]]) %>%
  segregate(keep = TRUE, other = NA) %>%
  patches(directions = 8)

# find the size of each patch and filter out those < 1 acre (0.4 ha)
targets1_keepID = bind_rows(
  freq(targets1_rip) %>% as_tibble() %>% mutate(label = 'riparian', code = 15),
  freq(targets1_wet) %>% as_tibble() %>% mutate(label = 'managed wetland', code = 18)
  ) %>%
  mutate(area.ha = count * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4)
targets1_keepID %>% group_by(label) %>% count()
# 1022 patches: 917 riparian; 105 wetland

# classify all included patch IDs as new riparian or wetlands; rest to NA
# --> have to classify each layer/sublayer separately, since patch names are
# duplicated across values

# riparian:
rlist = targets1_keepID %>% filter(label == 'riparian') %>%
  left_join(names(targets1_rip) %>% as_tibble(rownames = 'layer') %>%
              rename(baseline = value) %>% mutate(layer = as.numeric(layer))) %>%
  select(layer, baseline, value, code)
# keep only the baseline layers included:
targets1_rip_subset = subset(targets1_rip, subset = unique(rlist$baseline))
rlist_split = rlist %>% split(.$layer)

# wetlands:
tlist = targets1_keepID %>% filter(label == 'managed wetland') %>%
  left_join(names(targets1_wet) %>% as_tibble(rownames = 'layer') %>%
              rename(baseline = value) %>% mutate(layer = as.numeric(layer))) %>%
  select(layer, baseline, value, code)
# keep only the baseline layers included:
targets1_wet_subset = subset(targets1_wet, subset = unique(tlist$baseline))
tlist_split = tlist %>% split(.$layer)

# reclassify included patches as riparian/managed wetlands:
targets1_keep = c(classify(targets1_rip_subset[[1]],
                           rcl = rlist_split[[1]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_rip_subset[[2]],
                           rcl = rlist_split[[2]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_rip_subset[[3]],
                           rcl = rlist_split[[3]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_rip_subset[[4]],
                           rcl = rlist_split[[4]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_rip_subset[[5]],
                           rcl = rlist_split[[6]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_rip_subset[[6]],
                           rcl = rlist_split[[6]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_rip_subset[[7]],
                           rcl = rlist_split[[7]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_rip_subset[[8]],
                           rcl = rlist_split[[8]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_rip_subset[[9]],
                           rcl = rlist_split[[9]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_rip_subset[[10]],
                           rcl = rlist_split[[10]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_rip_subset[[11]],
                           rcl = rlist_split[[11]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_rip_subset[[12]],
                           rcl = rlist_split[[12]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_wet_subset[[1]],
                           rcl = tlist_split[[1]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_wet_subset[[2]],
                           rcl = tlist_split[[2]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_wet_subset[[3]],
                           rcl = tlist_split[[3]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_wet_subset[[4]],
                           rcl = tlist_split[[4]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_wet_subset[[5]],
                           rcl = tlist_split[[5]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_wet_subset[[6]],
                           rcl = tlist_split[[6]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets1_wet_subset[[7]],
                           rcl = tlist_split[[7]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE)) %>%
  #combine into one layer again
  sum(na.rm = TRUE)
plot(targets1_keep)

## * checkpoint: how much still needed-------

base_plus_targets1 = cover(targets1_keep, base_plus_plans)

change_targets1 = base_plus_targets1 %>% mask(delta) %>%
  freq() %>% as_tibble() %>%
  filter(value %in% c(15, 18)) %>%
  mutate(label = recode(value,
                        '15' = 'riparian', '18' = 'managed wetland')) %>%
  mutate(targets1_ha = count * 30 * 30 / 10000)

targets_plus2 = full_join(targets %>% select(label, total_ha),
                          change_targets1 %>% select(label, targets1_ha),
                          by = 'label') %>%
  mutate(add_ha = total_ha - targets1_ha)
# wetlands: still need 948 ha
# riparian: still need 1103 ha

## close the gap-------
# randomly distribute additional pixels to a subset of the next priority level
subst(c(26, 9, 29, 13:19, 32:39), NA) %>%
  subst(c(1:6, 22:25), 15) %>%
  subst(c(11:12), 18)

# identify candidate patches in the next priority level: 26 for riparian and 13
# for wetland
targets2 = restoration_targets_limited %>%
  subst(c(1:6, 9, 22:25, 29, 11:12, 14:19, 32:39), NA) %>%
  subst(26, 15) %>%
  subst(13, 18) %>%
  segregate(keep = TRUE, other = NA)

# find all patches of at least 1 acre (0.4 ha): base on original land cover
# class --> hopefully identifying fields as potential restoration units (instead
# of giant blocks)

# rip patches
targets2_rip = mask(baseline, targets2[[1]]) %>%
  segregate(keep = TRUE, other = NA) %>%
  patches(directions = 8)

# wetland patches
targets2_wet = mask(baseline, targets2[[2]]) %>%
  segregate(keep = TRUE, other = NA) %>%
  patches(directions = 8)

# find the size of each patch and filter out those < 1 acre (0.4 ha)
targets2_consider = bind_rows(
  freq(targets2_rip) %>% as_tibble() %>% mutate(label = 'riparian', code = 15),
  freq(targets2_wet) %>% as_tibble() %>% mutate(label = 'managed wetland', code = 18)
) %>%
  mutate(area.ha = count * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4)
targets2_consider %>% group_by(label) %>% count()
#519 patches: 163 riparian; 356 wetland

# take random sample of candidate patches by putting them in a random order
targets2_sample = sample(c(1:nrow(targets2_consider)),
                         nrow(targets2_consider), replace = FALSE)
targets2_order = targets2_consider[targets2_sample,] %>%
  mutate(order = c(1:length(targets2_sample))) %>%
  arrange(label, order) %>%
  group_by(label, code) %>%
  # calculate cumulative area of all preceding patches
  mutate(tot.ha = cumsum(area.ha)) %>%
  ungroup() %>%
  left_join(targets_plus2 %>% select(label, add_ha))

# for each habitat type, find the first patch where tot.ha exceeds targets2_need:
targets2_keepID = targets2_order %>%
  left_join(targets2_order %>% filter(tot.ha > add_ha) %>%
              group_by(label, code) %>% slice_min(order_by = tot.ha) %>% ungroup() %>%
              select(label, max = tot.ha)) %>%
  filter(tot.ha <= max)

# how much did we overshoot the objectives?
# (can try random sampling again to reduce)
targets2_keepID %>%
  group_by(label, code) %>% slice_max(order_by = tot.ha) %>% ungroup() %>%
  mutate(diff.ha = tot.ha - add_ha) %>% select(label, diff.ha)

# classify all included patch IDs as new riparian or wetlands; rest to NA
# --> have to classify each layer/sublayer separately, since patch names are
# duplicated across values

# riparian:
rlist2 = targets2_keepID %>% filter(label == 'riparian') %>%
  left_join(names(targets2_rip) %>% as_tibble(rownames = 'layer') %>%
              rename(baseline = value) %>%
              mutate(layer = as.numeric(layer),
                     baseline = as.numeric(baseline))) %>%
  select(layer, baseline, value, code) %>%
  arrange(baseline)
# keep only the baseline layers included:
targets2_rip_subset = subset(targets2_rip,
                             subset = as.character(unique(rlist2$baseline)))
rlist2_split = rlist2 %>% split(.$layer)
#--> only one layer (orch/vin)


# wetlands:
tlist2 = targets2_keepID %>% filter(label == 'managed wetland') %>%
  left_join(names(targets2_wet) %>% as_tibble(rownames = 'layer') %>%
              rename(baseline = value) %>%
              mutate(layer = as.numeric(layer),
                     baseline = as.numeric(baseline))) %>%
  select(layer, baseline, value, code) %>%
  arrange(baseline)
# keep only the baseline layers included:
targets2_wet_subset = subset(targets2_wet,
                             subset = as.character(unique(tlist2$baseline)))
tlist2_split = tlist2 %>% split(.$layer)

# reclassify included patches as riparian/managed wetlands:
targets2_keep = c(classify(targets2_rip_subset[[1]],
                           rcl = rlist2_split[[1]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets2_wet_subset[[1]],
                           rcl = tlist2_split[[1]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets2_wet_subset[[2]],
                           rcl = tlist2_split[[2]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets2_wet_subset[[3]],
                           rcl = tlist2_split[[3]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets2_wet_subset[[4]],
                           rcl = tlist2_split[[4]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets2_wet_subset[[5]],
                           rcl = tlist2_split[[5]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets2_wet_subset[[6]],
                           rcl = tlist2_split[[6]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets2_wet_subset[[7]],
                           rcl = tlist2_split[[7]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets2_wet_subset[[8]],
                           rcl = tlist2_split[[8]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE),
                  classify(targets2_wet_subset[[9]],
                           rcl = tlist2_split[[9]] %>%
                             select(from = value, to = code) %>%
                             as.matrix(),
                           othersNA = TRUE)) %>%
  #combine into one layer again
  sum(na.rm = TRUE)
plot(targets2_keep)

## * checkpoint: did we meet objectives?---------

base_plus_targets2 = cover(targets2_keep, base_plus_targets1)

change_targets2 = base_plus_targets2 %>% mask(delta) %>%
  freq() %>% as_tibble() %>%
  filter(value %in% c(15, 18)) %>%
  mutate(label = recode(value,
                        '15' = 'riparian', '18' = 'managed wetland')) %>%
  mutate(targets2_ha = count * 30 * 30 / 10000)

targets_plus2 = full_join(targets %>% select(label, total_ha),
                          change_targets2 %>% select(label, targets2_ha),
                          by = 'label') %>%
  mutate(add_ha = total_ha - targets2_ha)
# wetlands: overshot by 6.75 ha
# riparian: overshot by 13.0 ha


# 3. combine new restoration into one layer--------
restore_all = c(plans, targets1_keep, targets2_keep) %>% sum(na.rm = TRUE)
writeRaster(restore_all,
            'GIS/scenario_inputs/restoration_added.tif',
            overwrite = TRUE)

## overlay on baseline----------
scenario_restoration = cover(restore_all, baseline)
levels(scenario_restoration) <- wkey %>% select(WATERBIRD_CODE, label) %>%
  as.data.frame()
coltab(scenario_restoration) <- wkey %>% select(WATERBIRD_CODE, col) %>%
  complete(WATERBIRD_CODE = c(0:255)) %>% pull(col)
names(scenario_restoration) = 'scenario_restoration'
plot(c(baseline, scenario_restoration))
writeRaster(scenario_restoration,
            'GIS/scenario_rasters/scenario1_restoration.tif',
            overwrite = TRUE)

## calculate change------
delta_restoration = DeltaMultipleBenefits::calculate_change(
  baseline = baseline %>% mask(delta),
  scenario = scenario_restoration %>% mask(delta))
delta_restoration %>%
  group_by(label.base) %>%
  summarize(change = sum(change), .groups = 'drop') %>%
  arrange(change) %>%
  rename(label = label.base) %>%
  DeltaMultipleBenefits::plot_change(scale = 1000000) +
  labs(x = NULL, y = bquote(' ' *Delta~ 'total area ('~km^2*')')) +
  theme_bw() + coord_flip() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))
ggsave('fig/change_scenario1_restoration.png', height = 7.5, width = 6)
# large increase in riparian and wetland cover, mostly at the expense of
# pasture, orchard/vineyard, grassland [minimized reduction of orchard/vineyard
# to only that necessary to meet objectives]

# 4. specify riparian subtypes---------
# restored riparian veg needs to be assigned subtypes for use with riparian
# distribution models; use veg types from restoration opportunities analysis to
# inform riparian sub-types: valley foothill riparian (7100) vs. willow riparian
# scrub/shrub (7600)
# --> pixels not in restoration opportunities analysis likely from restoration
# plans; assume 'valley foothill riparian'


rip_targets_type = lapp(
  c(rast('GIS/scenario_inputs/restoration_potential.tif') %>%
      subst(8000, NA) %>%
      mask(restore_all %>% subst(18, NA)),
    restore_all),
  fun = function(x, y) {
    ifelse(y == 15 & is.na(x), 7100, x)
  })
plot(rip_targets_type)
freq(rip_targets_type)
# 7100: 45415 (undefined "valley foothill riparian")
# 7600: 3840 (SALIXSHRUB/"willow shrub/scrub")


# ASSIGN FOREST TYPES
# keep only pixels that could be forest (valley foothill riparian), and find
# distinct patches
rip_targets_vri = rip_targets_type %>% subst(7600, NA) %>%
  patches(directions = 8) #values up to 1737
patchlist = freq(rip_targets_vri) %>% as_tibble() %>%
  mutate(ID = c(1:nrow(freq(rip_targets_vri))))

# convert to polygons and find centroids
rip_targets_vri_ctr = as.polygons(rip_targets_vri) %>% centroids()
# find 2km buffer
rip_targets_vri_ctr_buff2k = buffer(rip_targets_vri_ctr,
                                   width = 2000)

# for each buffer, find frequency of riparian subtypes in surrounding area
# from baseline
baseline_ripdetail = rast('GIS/landscape_rasters/veg_baseline_ripdetail.tif')
freq(baseline_ripdetail)
# exclude introscrub as an option
baseline_ripdetail_limited = classify(baseline_ripdetail,
                                      rcl = matrix(c(75, NA),
                                                   ncol = 2, byrow = TRUE))
buff_values = extract(baseline_ripdetail_limited, rip_targets_vri_ctr_buff2k)
buff_values_prop = buff_values %>% filter(!is.na(veg_baseline_ripdetail)) %>%
  group_by(ID, veg_baseline_ripdetail) %>% count() %>%
  group_by(ID) %>% mutate(total = sum(n)) %>% ungroup() %>%
  mutate(prop = n/total) %>%
  left_join(patchlist %>% select(ID, patchID = value, ncells = count)) %>%
  split(.$ID)

# for each value in ID/patchID, use list of riparian subtype codes and the
# relative proportion of each to randomly assign the total number of cells in
# each patch to one of the subtypes
new = purrr::map(buff_values_prop,
                 ~sample(x = .x %>% pull(veg_baseline_ripdetail),
                         size = .$ncells[1],
                         prob = .$prop,
                         replace = TRUE))
names(new) = patchlist$value

# now transfer these random assignments back to corresponding patch IDs
ripforest = rip_targets_vri
for(i in c(1:length(new))) {
  target = names(new)[i] %>% as.numeric()
  cell_id = cells(ripforest, target)[[1]]
  ripforest[cell_id] <- new[[i]]
}
freq(ripforest)


# ASSIGN SHRUBS
ripshrub = rip_targets_type %>% subst(7100, NA) %>% subst(7600, 76)
# assign all to 76 (SALIXSHRUB) - by far the most frequent ripshrub

# COMBINE
scenario_restoration_ripdetail = cover(ripforest, ripshrub) %>%
  cover(baseline_ripdetail)
# original unchanged riparian detail
levels(scenario_restoration_ripdetail) <- rkey %>%
  filter(RIPARIAN_CODE %in% c(71:78)) %>% select(RIPARIAN_CODE, label) %>%
  as.data.frame()
coltab(scenario_restoration_ripdetail) <- rkey %>%
  filter(RIPARIAN_CODE %in% c(71:78)) %>% select(RIPARIAN_CODE, col) %>%
  complete(RIPARIAN_CODE = c(0:255)) %>% pull(col)
plot(scenario_restoration_ripdetail)
writeRaster(scenario_restoration_ripdetail,
            'GIS/scenario_rasters/scenario1_restoration_ripdetail.tif',
            overwrite = TRUE)
