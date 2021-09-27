# README---------
# From baseline land cover layer, develop scenario of meeting Delta Plan
# restoration objectives for riparian vegetation and managed/seasonal wetlands.

# PACKAGES & FUNCTIONS
source('R/packages.R')

# reference data:
delta = rast('GIS/boundaries/delta.tif')
delta_buff10k = read_sf('GIS/boundaries/Legal_Delta_boundary_buff10k.shp') %>%
  st_transform(crs = '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')
template = rasterize(vect(delta_buff10k), extend(delta, delta_buff10k))

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
# wetlands = 5,643 ha

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

## restoration plans------
# existing planned/in-progress restoration projects: based on EcoAtlas, with
# additional info/edits from DSLPT EcoRestore and examining project plans
plans = read_sf('GIS/scenario_inputs/habitatprojects_Deltabuff10k.shp') %>%
  st_transform(crs = st_crs(delta_buff10k)) %>%
  # exclude baseline (already completed), unknown status, and "proposed" (early stages)
  filter(status %in% c('In-progress', 'Permitting', 'Planning')) %>%
  # exclude non-target habitat types and enhancement projects
  filter(habitat_ty %in%
           c('valley foothill riparian',
             'non-tidal freshwater emergent wetland',
             'managed non-tidal wetland',
             'wet meadow/seasonal wetland',
             'managed non-tidal wetland semi-perm')) %>%
  # exclude Delta Wetlands Project Compensatory Mitigation (because not showing
  # corresponding losses from other areas)
  filter(name != 'Delta Wetlands Project Compensatory Mitigation') %>%
  mutate(code = if_else(habitat_ty == 'valley foothill riparian', 15, 18)) %>%
  vect() %>%
  rasterize(template, field = 'code')
writeRaster(plans, 'GIS/scenario_inputs/restoration_plans.tif',
            overwrite = TRUE)

## restoration potential------

### restoration opportunities-------
# to add to these plans and meet restoration objectives, select from restoration
# opportunities analysis: combine separate layers representing riparian and
# wetland restoration opportunities (excluding tidal marsh for now)
filelist = paste0('GIS/original_source_data/DSLPT/',
                  list.files('GIS/original_source_data/DSLPT', 'potential.*shp$'))
habpotential = do.call(rbind,
              lapply(filelist[1:2],
                     function(x) st_read(x) %>% dplyr::select(Habitat_Ty))) %>%
  filter(Habitat_Ty %in% c('valley foothill riparian',
                           'willow riparian scrub/shrub',
                           'wet meadow/seasonal wetland')) %>%
  mutate(code = case_when(Habitat_Ty == 'wet meadow/seasonal wetland' ~ 8000,
                          Habitat_Ty == 'valley foothill riparian' ~ 7100,
                          Habitat_Ty == 'willow riparian scrub/shrub' ~ 7600)) %>%
  st_transform(crs = st_crs(delta_buff10k)) %>%
  vect() %>%
  rasterize(template, field = 'code') %>%
  mask(template)
writeRaster(habpotential, 'GIS/scenario_inputs/restoration_potential.tif')

plot(habpotential)
# Note: coding "valley foothill riparian" as POFR, but should be ~22% QULO, 21%
# POFR, 12% SALIX, 7% MIXEDFOREST; also, SALIXSHRUB_2000 left out of main models
# due to CV-wide correlation with total RIPARIAN_2000, but Delta baseline is
# ~25% SALIXSHRUB [was SALIXSHRUB_50 important to any individual spp?]

### zoning-----------
# avoid areas already designated for development, and prioritize restoration
# in open space/public areas over private lands

zoning = st_read('GIS/original_source_data/FINAL_City_County_LU_merged/FINAL_City_County_LU_merged.shp') %>%
  filter(GIN_CLASS %in% c('Areas Designated for Development',
                          'Open Space/Recreation',
                          'Public/Quasi-Public')) %>%
  mutate(code = case_when(GIN_CLASS == 'Areas Designated for Development' ~ 90,
                          GIN_CLASS == 'Open Space/Recreation' ~ 10,
                          GIN_CLASS == 'Public/Quasi-Public' ~ 20)) %>%
  st_transform(crs = st_crs(delta_buff10k)) %>%
  vect() %>%
  rasterize(template, field = 'code') %>%
  mask(template)
writeRaster(zoning, 'GIS/scenario_inputs/zoning.tif')
# 10 = open space/recreation; 20 = public/quasi=public; 90 = designated for development

### priority areas---------
# further prioritize restoration in the Delta Stewardship Council's "priority
# restoration areas"
priority = st_read('GIS/original_source_data/ER_P3/ER_P3.shp') %>%
  mutate(code = 100) %>%
  vect() %>%
  rasterize(template, field = 'code')
writeRaster(priority, 'GIS/scenario_inputs/restoration_priority_areas.tif')
# priority = 100

### protected areas-------------
# further prioritize restoration within existing protected areas

filelist = paste0('GIS/original_source_data/DSLPT/',
                  list.files('GIS/original_source_data/DSLPT', 'protected.*shp$'))
protected = rbind(st_read(filelist[1]) %>% st_zm() %>% mutate(code = 2) %>%
                    dplyr::select(code),
                  st_read(filelist[2]) %>% mutate(code = 1) %>%
                    dplyr::select(code)
) %>%
  st_transform(crs = st_crs(delta_buff10k)) %>%
  vect() %>%
  rasterize(template, field = 'code') %>%
  mask(template)
writeRaster(protected, 'GIS/scenario_inputs/protected_areas.tif')
#  1 = protected; 2 = easement

### combined restoration potential-----
restoration_stack = c(habpotential, #7100 = valley foothill riparian, 7600 = willow shrub/scrub, 8000 = wetland
                      priority, # 100 = priority
                      zoning, #10 = open space/recreation; 20 = public/quasi=public; 90 = designated for development
                      protected # 1 = protected, 2 = easement
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
# don't distinguish between protected vs. easement
restoration_stack[[4]] = classify(restoration_stack[[4]],
                                  rcl = matrix(c(2, 1), byrow = TRUE, ncol = 2))



# 1. overlay restoration plans on baseline-------
# check conversions:
tab = crosstab(c(baseline, plans), useNA = TRUE, long = TRUE)
tab %>% rlang::set_names(c('baseline', 'plans', 'n')) %>%
  left_join(wkey %>% select(baseline = WATERBIRD_CODE, from = label)) %>%
  left_join(wkey %>% select(plans = WATERBIRD_CODE, to = label)) %>%
  filter(!is.na(plans)) %>%
  arrange(plans, desc(n))
# restoration plans will convert:
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

# first sum scores to classify each pixel with multiple pieces of data
restoration_targets = sum(restoration_stack, na.rm = TRUE) %>%
  # eliminate those not in restoration potential areas
  subst(from = c(0:200), to = NA) %>%
  # then reclassify
  classify(rcl = matrix(
    c(7111, 1, #priority, public space, protected
      7101, 2, #priority, protected, but not public/open
      7110, 2, #priority, public/open space, but not protected
      7100, 3, #priority, but not public/open or protected
      7011, 4, #not priority, but open space and protected
      7001, 5, #not priority, protected, but not public/open
      7010, 5, #not priority, public/open space, but not protected
      7000, 6, #not priority, not open/public, not protected
      7090, 9, #not priority, designated for development
      7091, 9, #not priority, designated for development (but also protected?)
      7190, 9, #priority, designed for development
      8111, 11,
      8101, 12,
      8110, 12,
      8100, 13,
      8011, 14,
      8001, 15,
      8010, 15,
      8000, 16,
      8090, 19,
      8091, 19,
      8190, 19), byrow = TRUE, ncol = 2))

# compare to baseline plus restoration plans
tab = crosstab(c(base_plus_plans, restoration_targets),
               useNA = TRUE, long = TRUE)
tab %>% rlang::set_names(c('base', 'new', 'n')) %>%
  left_join(wkey %>% select(base = WATERBIRD_CODE, from = label)) %>%
  mutate(to = case_when(new %in% c(1:9) ~ 'riparian',
                        new %in% c(11:19) ~ 'wetland')) %>%
  group_by(to, from) %>%
  summarize(n = sum(n, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(to)) %>%
  arrange(to, desc(n)) %>% print(n = 51)
# to riparian largely from orch/vin; also grassland, fallow, urban; also some
# existing/planned riparian, other wetland, managed wetland, tidal marsh, water

# to wetland largely from pasture, fallow; also existing managed wetland,
# grassland, orch/vin; also includes some urban, riparian, water, tidal marsh

# don't allow "restoration" of existing managed wetland, tidal marsh, water, or
# riparian; also exclude existing forest/shrub and urban
restoration_targets_limited = lapp(c(restoration_targets, base_plus_plans),
                                   function(x, y) {
                                     ifelse(!is.na(x) &
                                              y %in% c(12:15, 18:19),
                                            NA, x)
                                   })
tab = crosstab(c(base_plus_plans, restoration_targets_limited),
               useNA = TRUE, long = TRUE)
tab %>% rlang::set_names(c('base', 'new', 'n')) %>%
  left_join(wkey %>% select(base = WATERBIRD_CODE, from = label)) %>%
  mutate(to = case_when(new %in% c(1:9) ~ 'riparian',
                        new %in% c(11:19) ~ 'wetland')) %>%
  group_by(to, from) %>%
  summarize(n = sum(n, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(to)) %>%
  arrange(to, desc(n)) %>% print(n = 51)

# all potential riparian pixels assigned a priority level 1-4, or 9 and
#  potential wetland pixels assigned 11-14 or 19
writeRaster(restoration_targets_limited,
            'GIS/scenario_inputs/restoration_targets.tif',
            overwrite = TRUE)

# how much potential is this relative to restoration objectives?
freq(restoration_targets_limited) %>% as_tibble() %>%
  mutate(label = case_when(value %in% c(1:9) ~ 'riparian',
                           value %in% c(11:19) ~ 'managed wetland'),
         area.ha = count * 30 * 30 / 10000,
         tot.ha = cumsum(area.ha))
# to reach riparian target of 3708 additional ha, need all of priority levels
# 1-5 and some of 6; to reach wetland target of 2337 additional ha, need all of
# priority levels 1-2 and some of 3

## top priority restoration-------
# start with priority levels needed for each habitat type (as above), but
# exclude very small patches of potential restoration (minimum 1 acre)

# find all patches in top priority levels of at least 1 acre (0.4 ha):
targets1 = restoration_targets_limited %>%
  subst(c(6, 9, 13:19), NA) %>%
  subst(c(1:5), 15) %>%
  subst(c(11:12), 18) %>%
  # separate by value so patches reflect separate priority levels
  segregate(keep = TRUE, other = NA)

# rip patches
targets1_rip = targets1[[1]] %>% patches(directions = 8)

# wetland patches: base on original land cover class --> hopefully identifying
# fields as potential restoration units (instead of giant blocks)
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
#447 patches: 286 riparian; 161 wetland

# classify all included patch IDs as new riparian or wetlands; rest to NA
# --> have to classify each layer/sublayer separately, since patch names are
# duplicated across values

# riparian:
rlist = targets1_keepID %>% filter(label == 'riparian')
# wetlands:
tlist = targets1_keepID %>% filter(label == 'managed wetland') %>%
  left_join(names(targets1_wet) %>% as_tibble(rownames = 'layer') %>%
              rename(baseline = value) %>% mutate(layer = as.numeric(layer))) %>%
  select(layer, baseline, value, code)
# keep only the baseline layers included:
targets1_wet_subset = subset(targets1_wet, subset = unique(tlist$baseline))
tlist_split = tlist %>% split(.$layer)


# reclassify included patches as riparian/managed wetlands:
targets1_keep = c(classify(targets1_rip,
                           rcl = rlist %>%
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
                           othersNA = TRUE),
                  classify(targets1_wet_subset[[8]],
                           rcl = tlist_split[[8]] %>%
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
# wetlands: still need 593 ha
# riparian: still need 2075 ha

## close the gap-------
# randomly distribute additional pixels to a subset of the next priority level

# identify candidate patches in the next priority level:
targets2 = restoration_targets_limited %>%
  subst(c(1:5, 9, 11:12, 14:19), NA) %>%
  subst(6, 15) %>%
  subst(13, 18) %>%
  segregate(keep = TRUE, other = NA)

# rip patches based on restoration priority
targets2_rip = targets2[[1]] %>% patches(directions = 8)

# wetland patches based on both restoration priority rank & original land cover
# class --> hopefully identifying fields as potential restoration units (instead
# of giant blocks)
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
#584 patches: 257 riparian; 327 wetland

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

# riparian: (only 1 layer)
rlist2 = targets2_keepID %>% filter(label == 'riparian') %>%
  select(layer, value, code)
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
targets2_keep = c(classify(targets2_rip,
                           rcl = rlist2 %>%
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
# wetlands: overshot by 6.3 ha
# riparian: overshot by 3.8 ha


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
# orchard/vineyard AND pasture

# 4. specify riparian subtypes---------
# restored riparian veg needs to be assigned subtypes for use with riparian
# distribution models; use veg types from restoration opportunities analysis to
# inform riparian sub-types: valley foothill riparian (7100) vs. willow riparian
# scrub/shrub (7600)
# --> pixels not in restoration opportunities analysis likely from restoration
# plans; assume 'valley foothill riparian'


rip_targets_type = lapp(c(habpotential %>% subst(8000, NA) %>%
                            mask(restore_all %>% subst(18, NA)),
                          restore_all),
                        fun = function(x, y) {
                          ifelse(y == 15 & is.na(x), 7100, x)
                          })
# 7100 = undefined "valley foothill riparian" (most)
# 7600 = SALIXSHRUB ("willow shrub/scrub")
plot(rip_targets_type)

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
            'GIS/scenario_rasters/scenario1_restoration_ripdetail.tif')
