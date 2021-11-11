# README---------
# From baseline land cover layer, develop scenario of meeting Delta Plan
# restoration objectives for riparian vegetation and managed/seasonal wetlands.

# PACKAGES & FUNCTIONS
source('R/packages.R')

# reference data:
delta = rast('GIS/boundaries/delta.tif')
baseline = rast('GIS/landscape_rasters/veg_baseline_fall.tif')
key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')

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
  as_tibble() %>%
  filter(value %in% c(70:77, 89, 81:82)) %>% #exclude tidal and "other" wetlands
  mutate(label = if_else(value %in% c(70:77), 'RIPARIAN', 'WETLAND')) %>%
  group_by(label) %>%
  summarize(count = sum(count), .groups = 'drop') %>%
  mutate(current_ha = count * 30 * 30 / 10000)
# riparian = 8,218 ha
# wetlands = 6,009 ha

# how much more needed to meet proposed 2050 targets?
targets = full_join(
  obj %>% mutate(label = c('WETLAND', 'RIPARIAN')) %>%
    select(label, total_ha),
  base %>% select(label, current_ha),
  by = 'label') %>%
  mutate(add_ha = total_ha - current_ha)
# wetlands: add 3,744 ha
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

# EXISTING PLANS------
# overlay existing restoration plans on baseline
# check conversions:
tab = crosstab(c(baseline, plans), useNA = TRUE, long = TRUE)
tab %>% rlang::set_names(c('baseline', 'plans', 'n')) %>%
  left_join(key %>% select(baseline = CODE_BASELINE, from = CODE_NAME)) %>%
  left_join(key %>% select(plans = CODE_BASELINE, to = CODE_NAME)) %>%
  filter(!is.na(plans)) %>%
  arrange(plans, desc(n))
# existing restoration plans will convert:
# - to riparian from: fallow, grassland, existing riparian; some pixels from
# water, other wetland, urban
# - to wetland from: pasture, other wetland, corn, rice, fallow, grassland...;
# some pixels from riparian, existing managed wetland

# --> allow conversion from urban to riparian for urban restoration projects;
# don't allow conversion from riparian to wetlands (assume it will stay as
# riparian)

base_plus_plans = lapp(c(plans, baseline),
                       function(x, y) {
                         ifelse(!is.na(x) & !y %in% c(71:79, 81:82), x, y)
                       })

# * checkpoint: calculate how much still needed----
change_plans = base_plus_plans %>% mask(delta) %>%
  freq() %>% as_tibble() %>%
  filter(value %in% c(70:82,89)) %>%
  mutate(label = if_else(value %in% c(70:77), 'RIPARIAN', 'WETLAND')) %>%
  group_by(label) %>%
  summarize(count = sum(count), .groups = 'drop') %>%
  mutate(plans_ha = count * 30 * 30 / 10000)

targets_plus = full_join(targets %>% select(label, total_ha),
                         change_plans %>% select(label, plans_ha),
                         by = 'label') %>%
  mutate(add_ha = total_ha - plans_ha)
# wetlands: still need 2342 ha
# riparian: still need 3690 ha

# ADDITIONAL PRIORITIES-----------
# prioritize potential restoration areas

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
  left_join(key %>% select(base = CODE_BASELINE, from = CODE_NAME)) %>%
  mutate(to = case_when(new %in% c(1:4) ~ 'riparian',
                        new %in% c(5:6) ~ 'riparian2',
                        new %in% c(9, 19) ~ 'development',
                        new %in% c(11:14) ~ 'wetland',
                        new %in% c(15:16) ~ 'wetland2')) %>%
  group_by(to, from) %>%
  summarize(n = sum(n, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(to)) %>%
  arrange(to, desc(n)) %>% print(n = 100)
# to riparian largely from grassland, fallow, orch, wheat, row, alfalfa
# to riparian2 largely from vineyard, fallow, orchard

# to wetland largely from pasture, existing managed wetland, fallow
# to wetland2 largely from pasture

# don't allow "restoration" of existing managed wetland, tidal marsh, water, or
# riparian; also exclude existing urban; deprioritize existing orch/vin
restoration_targets_limited = lapp(c(restoration_targets, base_plus_plans),
                                   function(x, y) {
                                     ifelse(!is.na(x) &
                                              y %in% c(60, 70:77, 81:82, 86, 89, 90),
                                            NA,
                                            ifelse(!is.na(x) & y %in% c(10:19),
                                                   x + 20,
                                                   x))
                                   })

tab = crosstab(c(base_plus_plans, restoration_targets_limited),
               useNA = TRUE, long = TRUE)
tab %>% rlang::set_names(c('base', 'new', 'n')) %>%
  left_join(key %>% select(base = CODE_BASELINE, from = CODE_NAME)) %>%
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
  arrange(to, desc(n)) %>% print(n = 100)
# to riparian primarily from grassland, fallow; not wetland, urban, or orch/vin
# to riparian2 primarily from grassland, fallow, alfalfa, row
# to riparian3 entirely from existing vin, orch
# to wetland primarily from pasture, fallow
# to wetland2 primarily from pasture
# to wetland3 entirely from existing vin, orch

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
# to reach wetland target of 2342 additional ha, need all of priority
# levels 1-2 and some of 3
# to reach riparian target of 3708 additional ha (while excluding existing
# orchard and vineyard), need all of priority levels 1-6 plus 22-25 and some of
# 26!


## ROUND 1: top priority restoration-------
# start with priority levels for which entire level is needed (as above), but
# exclude very small patches of potential restoration (minimum 1 acre)

# find all pixels in top priority levels:
targets1 = restoration_targets_limited %>%
  subst(c(26, 9, 29, 13:19, 32:39), NA) %>%
  subst(c(1:6, 22:25), 70) %>%
  subst(c(11:12), 89) %>%
  segregate(keep = TRUE, other = NA)

# find all patches of at least 1 acre (0.4 ha): base on original land cover
# class --> hopefully identifying fields as potential restoration units (instead
# of giant blocks)

# rip patches
targets1_rip = mask(baseline, targets1[[1]]) %>%
  # lump perennial crops, annual crops, and natural lands
  classify(rcl = matrix(c(9.5, 19.5, 10,
                          19.5, 28.5, 20,
                          49.5, 56.5, 50,
                          #lump other wetland, wood/scrub, barren with grassland
                          86.5, 87.5, 50,
                          99.5, 100.5, 50,
                          129.5, 130.5, 50), byrow = TRUE, ncol = 3)) %>%
  segregate(keep = TRUE, other = NA) %>%
  patches(directions = 8)

# wetland patches
targets1_wet = mask(baseline, targets1[[2]]) %>%
  # lump annual crops, and other wetlands with grassland; keep pasture and alfalfa separate
  classify(rcl = matrix(c(19.5, 28.5, 20,
                          51.5, 52.5, 51, #combine alfalfa with other pasture
                          55.5, 87.5, 56 #combine other wetland with grassland
                          ), byrow = TRUE, ncol = 3)) %>%
  segregate(keep = TRUE, other = NA) %>%
  patches(directions = 8)

# find the size of each patch and filter out those < 1 acre (0.4 ha)
targets1_keepID = bind_rows(
  freq(targets1_rip) %>% as_tibble() %>% mutate(label = 'RIPARIAN', code = 70),
  freq(targets1_wet) %>% as_tibble() %>% mutate(label = 'WETLAND', code = 89)
  ) %>%
  mutate(area.ha = count * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4)
targets1_keepID %>% group_by(label) %>% count()
# 1064 patches: 895 riparian; 81 wetland

# classify all included patch IDs as new riparian or wetlands; rest to NA
# --> have to classify each layer/sublayer separately, since patch names are
# duplicated across values

# riparian:
rlist = targets1_keepID %>% filter(label == 'RIPARIAN') %>%
  left_join(names(targets1_rip) %>% as_tibble(rownames = 'layer') %>%
              rename(baseline = value) %>% mutate(layer = as.numeric(layer))) %>%
  select(layer, baseline, value, code)
# keep only the baseline layers included:
targets1_rip_subset = subset(targets1_rip, subset = unique(rlist$baseline))
rlist_split = rlist %>% split(.$layer)
# reclassify included patches:
targets1_rip_keep = targets1_rip_subset
for (i in c(1:length(rlist_split))) {
  targets1_rip_keep[[i]] = classify(targets1_rip_keep[[i]],
                                    rcl = rlist_split[[i]] %>%
                                      select(from = value, to = code) %>%
                                      as.matrix(),
                                    othersNA = TRUE)
}
plot(sum(targets1_rip_keep, na.rm = TRUE))

# wetlands:
tlist = targets1_keepID %>% filter(label == 'WETLAND') %>%
  left_join(names(targets1_wet) %>% as_tibble(rownames = 'layer') %>%
              rename(baseline = value) %>% mutate(layer = as.numeric(layer))) %>%
  select(layer, baseline, value, code)
# keep only the baseline layers included:
targets1_wet_subset = subset(targets1_wet, subset = unique(tlist$baseline))
tlist_split = tlist %>% split(.$layer)
# reclassify included patches:
targets1_wet_keep = targets1_wet_subset
for (i in c(1:length(tlist_split))) {
  targets1_wet_keep[[i]] = classify(targets1_wet_keep[[i]],
                                    rcl = tlist_split[[i]] %>%
                                      select(from = value, to = code) %>%
                                      as.matrix(),
                                    othersNA = TRUE)
}
plot(sum(targets1_wet_keep, na.rm = TRUE))

targets1_keep = c(targets1_rip_keep, targets1_wet_keep) %>%
  #combine into one layer again
  sum(na.rm = TRUE)
plot(targets1_keep)

## * checkpoint: how much still needed-------

base_plus_targets1 = cover(targets1_keep, base_plus_plans)

change_targets1 = base_plus_targets1 %>% mask(delta) %>%
  freq() %>% as_tibble() %>%
  filter(value %in% c(70:77, 81:82, 89)) %>%
  mutate(label = if_else(value %in% c(70:77), 'RIPARIAN', 'WETLAND')) %>%
  group_by(label) %>%
  summarize(count = sum(count), .groups = 'drop') %>%
  mutate(targets1_ha = count * 30 * 30 / 10000)

targets_plus2 = full_join(targets %>% select(label, total_ha),
                          change_targets1 %>% select(label, targets1_ha),
                          by = 'label') %>%
  mutate(add_ha = total_ha - targets1_ha)
# wetlands: still need 947 ha
# riparian: still need 847 ha

## ROUND 2: close the gap-------
# randomly distribute additional pixels to a subset of the next priority level

# - identify candidate patches in the next priority level: 26 for riparian and
# 13 for wetland
targets2 = restoration_targets_limited %>%
  subst(c(1:6, 9, 22:25, 29, 11:12, 14:19, 32:39), NA) %>%
  subst(26, 70) %>%
  subst(13, 89) %>%
  segregate(keep = TRUE, other = NA)

# find all patches of at least 1 acre (0.4 ha): base on original land cover
# class --> hopefully identifying fields as potential restoration units (instead
# of giant blocks)

# rip patches
targets2_rip = mask(baseline, targets2[[1]]) %>%
  # lump annual crops, and other wetlands with grassland; keep pasture and alfalfa separate
  classify(rcl = matrix(c(19.5, 28.5, 20,
                          51.5, 52.5, 51, #combine alfalfa with other pasture
                          55.5, 87.5, 56 #combine other wetland with grassland
  ), byrow = TRUE, ncol = 3)) %>%
  segregate(keep = TRUE, other = NA) %>%
  patches(directions = 8)

# wetland patches
targets2_wet = mask(baseline, targets2[[2]]) %>%
  # lump annual crops, and other wetlands with grassland; keep pasture and alfalfa separate
  classify(rcl = matrix(c(19.5, 28.5, 20,
                          51.5, 52.5, 51, #combine alfalfa with other pasture
                          55.5, 87.5, 56 #combine other wetland with grassland
  ), byrow = TRUE, ncol = 3)) %>%
  segregate(keep = TRUE, other = NA) %>%
  patches(directions = 8)

# find the size of each patch and filter out those < 1 acre (0.4 ha)
targets2_consider = bind_rows(
  freq(targets2_rip) %>% as_tibble() %>% mutate(label = 'RIPARIAN', code = 70),
  freq(targets2_wet) %>% as_tibble() %>% mutate(label = 'WETLAND', code = 89)
  ) %>%
  mutate(area.ha = count * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4)
targets2_consider %>% group_by(label) %>% count()
#528 patches: 187 riparian; 341 wetland

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
rlist2 = targets2_keepID %>% filter(label == 'RIPARIAN') %>%
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
# reclassify included patches:
targets2_rip_keep = targets2_rip_subset
for (i in c(1:length(rlist2_split))) {
  targets2_rip_keep[[i]] = classify(targets2_rip_keep[[i]],
                                    rcl = rlist2_split[[i]] %>%
                                      select(from = value, to = code) %>%
                                      as.matrix(),
                                    othersNA = TRUE)
}
plot(sum(targets2_rip_keep, na.rm = TRUE))

# wetlands:
tlist2 = targets2_keepID %>% filter(label == 'WETLAND') %>%
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
# reclassify included patches:
targets2_wet_keep = targets2_wet_subset
for (i in c(1:length(tlist2_split))) {
  targets2_wet_keep[[i]] = classify(targets2_wet_keep[[i]],
                                    rcl = tlist2_split[[i]] %>%
                                      select(from = value, to = code) %>%
                                      as.matrix(),
                                    othersNA = TRUE)
}
plot(sum(targets2_wet_keep, na.rm = TRUE))

# reclassify included patches as riparian/managed wetlands:
targets2_keep = c(targets2_rip_keep, targets2_wet_keep) %>%
  #combine into one layer again
  sum(na.rm = TRUE)
plot(targets2_keep)

## * checkpoint: did we meet objectives?---------

base_plus_targets2 = cover(targets2_keep, base_plus_targets1)

change_targets2 = base_plus_targets2 %>% mask(delta) %>%
  freq() %>% as_tibble() %>%
  filter(value %in% c(70:77, 81:82, 89)) %>%
  mutate(label = if_else(value %in% c(70:77), 'RIPARIAN', 'WETLAND')) %>%
  group_by(label) %>%
  summarize(count = sum(count), .groups = 'drop') %>%
  mutate(targets2_ha = count * 30 * 30 / 10000)

targets_plus2 = full_join(targets %>% select(label, total_ha),
                          change_targets2 %>% select(label, targets2_ha),
                          by = 'label') %>%
  mutate(add_ha = total_ha - targets2_ha)
# wetlands: overshot by 3.96 ha
# riparian: overshot by 6.42 ha


# COMBINED RESTORATION--------
# combine new restoration into one layer

# -->assume additional new wetlands are seasonal (except those otherwise
# designated in existing restoration plans)

restore_all = c(plans,
                targets1_keep %>% subst(89, 82),
                targets2_keep %>% subst(89, 82)) %>% sum(na.rm = TRUE)
writeRaster(restore_all,
            'GIS/scenario_inputs/restoration_added.tif',
            overwrite = TRUE)

## assign riparian subclasses--------
# restored riparian veg needs to be assigned subclasses for use with riparian
# distribution models; use details from restoration opportunities analysis to
# inform riparian subclasses: valley foothill riparian (7100) vs. willow
# riparian scrub/shrub (7600)
# --> pixels not in restoration opportunities analysis likely from restoration
# plans; assume 'valley foothill riparian'


rip_targets_type = lapp(
  c(rast('GIS/scenario_inputs/restoration_potential.tif') %>%
      subst(8000, NA) %>%
      mask(restore_all %>% subst(81, NA) %>% subst(82, NA)),
    restore_all),
  fun = function(x, y) {
    ifelse(y==70 & is.na(x), 7100, x)
  })
plot(rip_targets_type)
freq(rip_targets_type)
# 7100: 44991 (undefined "valley foothill riparian")
# 7600:  4037 (SALIXSHRUB/"willow shrub/scrub")


## * riparian forest----
# keep only pixels that could be forest (valley foothill riparian), and find
# distinct patches
rip_targets_vri = rip_targets_type %>% subst(7600, NA) %>%
  patches(directions = 8) #values up to 1737
patchlist = freq(rip_targets_vri) %>% as_tibble() %>%
  mutate(ID = c(1:nrow(freq(rip_targets_vri))))

# convert to polygons, find centroids, and 2km buffers
rip_targets_vri_buff = as.polygons(rip_targets_vri) %>% centroids() %>%
  buffer(width = 2000)

# for each buffer, find frequency of riparian subclasses in surrounding area
# from baseline (excluding INTROSCRUB and generic RIPARIAN)
baseline_ripdetail = baseline %>% subst(c(1:69, 70, 75, 80:130), NA)
freq(baseline_ripdetail) #should only 71-74, 76-77
buff_values = extract(baseline_ripdetail, rip_targets_vri_buff)
buff_values_prop = buff_values %>% filter(!is.na(lyr1)) %>%
  rename(polygon_num = ID, class = lyr1) %>%
  group_by(polygon_num, class) %>% count() %>%
  group_by(polygon_num) %>% mutate(total = sum(n)) %>% ungroup() %>%
  mutate(prop = n/total) %>%
  # eliminate tiny proportions & recalculate prop
  filter(prop >= 0.05) %>%
  group_by(polygon_num) %>%
  mutate(total = sum(n)) %>%
  # how many subclasses per patch?
  add_count(name = 'n_subclasses') %>%
  ungroup() %>%
  mutate(prop = n/total) %>%
  left_join(patchlist %>% select(polygon_num = ID, patchID = value, ncells = count))

# SINGLES: for patches with only a single option for a subclass, make the
# assignments straight-forward (can't fold into sampling procedure below because
# sample function doesn't work as expected with only one option)
singles = classify(
  rip_targets_vri,
  rcl = buff_values_prop %>% filter(n_subclasses == 1) %>%
    select(from = patchID, to = class) %>% as.matrix(),
  othersNA = TRUE)
freq(singles)

# DOUBLES & TRIPLES: for patches with more than one possible subclass,
# randomly assign the total number of cells in each patch to one of the
# subclasses
# - first find the polygon numbers that are relevant:
buff_doubles = buff_values_prop %>% filter(n_subclasses > 1) %>%
  select(polygon_num, patchID) %>% distinct()
#503 patches

# - remove any patches not included in this set:
doubles = classify(
  rip_targets_vri,
  rcl = buff_doubles %>% select(from = patchID, to = patchID) %>% as.matrix(),
  othersNA = TRUE)

# - SEMI-SLOW STEP: find cell numbers for each of these unique patches
cellnum = buff_doubles %>% split(.$patchID) %>%
  purrr::map_df(~cells(doubles, .x$patchID),
                .id = 'patchID') %>%
  rename(cellnum = lyr1) %>%
  mutate(patchID = as.numeric(patchID)) %>%
  left_join(buff_doubles, by = 'patchID')

# - randomly assign the total number of cells in each patch to one of the
# subclasses according to the proportion in the surrounding buffer
new = buff_values_prop %>% filter(n_subclasses > 1) %>% split(.$patchID) %>%
  purrr::map_df(~sample(x = .x %>% pull(class), # possible values
                        size = .$ncells[1],
                        prob = .x %>% pull(prop),
                        replace = TRUE) %>% as_tibble(),
                .id = 'patchID') %>%
  mutate(patchID = as.numeric(patchID))

# - match the new random values to the cell numbers
assignments = bind_cols(cellnum, new)
assignments %>% filter(patchID...1 != patchID...4)  # should be none!
# 44745 cells

# transfer the random assignments to the corresponding patch IDs
doubles[assignments$cellnum] <- assignments$value

# combine:
ripforest = cover(singles, doubles)
plot(ripforest)
plot(cover(ripforest, baseline_ripdetail))


## * riparian scrub--------
# labeled as willow scrub in restoration opportunities analysis, so just assign
# all to SALIXSHRUB

ripshrub = rip_targets_type %>% subst(7100, NA) %>% subst(7600, 76)


## final restoration layer-------
restore_all_ripdetail = cover(ripforest, ripshrub) %>% cover(restore_all)
freq(restore_all_ripdetail) #no generic 70 riparian left
plot(restore_all_ripdetail)
writeRaster(restore_all_ripdetail,
            'GIS/scenario_inputs/restoration_added_ripdetail.tif',
            overwrite = TRUE)

# OVERLAY ON BASELINE------------
scenario_restoration = cover(restore_all_ripdetail, baseline)
levels(scenario_restoration) <- key %>% select(CODE_BASELINE, CODE_NAME) %>%
  drop_na() %>% as.data.frame()
coltab(scenario_restoration) <- key %>% select(CODE_BASELINE, CODE_NAME, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
names(scenario_restoration) = 'scenario_restoration'

levels(baseline) <- key %>% select(CODE_BASELINE, CODE_NAME) %>%
  drop_na() %>% as.data.frame()
coltab(baseline) <- key %>% select(CODE_BASELINE, CODE_NAME, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
plot(c(baseline, scenario_restoration))

writeRaster(scenario_restoration,
            'GIS/scenario_rasters/scenario1_restoration.tif',
            overwrite = TRUE)

## calculate change------
delta_restoration = DeltaMultipleBenefits::calculate_change(
  baseline = baseline %>% mask(delta),
  scenario = scenario_restoration %>% mask(delta))
delta_restoration %>%
  # mutate(label = label.base) %>%
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
  theme_bw() + coord_flip() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))
ggsave('fig/change_scenario1_restoration.png', height = 7.5, width = 6)
# large increase in riparian and wetland cover, mostly at the expense of
# pasture, grassland, idle, row&field crops, corn, grain, rice, alfalfa,
# --> impacts to orchard/vineyard [minimized to only that necessary to meet
# riparian objectives

