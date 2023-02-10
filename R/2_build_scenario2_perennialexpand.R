# README---------
# From baseline land cover layer, develop scenario of perennial crop expansion
# based on Central Valley Futures project (Wilson et al. 2021), using the BBAU
# ("bad business as usual") scenario for the year 2050, including historically
# high rates of perennial crop expansion and no restoration.
#
# Assumptions:
# - conversions will not happen within the existing footprint for
# certain land cover classes (urban, woodland/scrub, riparian, wetland, water,
# rice) - except where riparian/wetland are not protected
# - conversions will not happen on patches < 1 acre in size (to eliminate
# patches with only a very few pixels).
# - the specific subclasses of new perennial crops will be proportional to the
# subclasses in the baseline from the surrounding region (~5km buffer). This
# should not affect bird distribution models, but may help account for variation
# in some of the other metrics among subclasses, so this level of detail doesn't
# need to be spatial at this time, but it may help display and compare scenario
# maps.

# PACKAGES & FUNCTIONS
source('R/0_packages.R')

# reference data:
delta = rast('GIS/boundaries/delta.tif')
delta_buff10k = read_sf('GIS/boundaries/Legal_Delta_boundary_buff10k.shp') %>%
  st_transform(crs = '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')
template = rasterize(vect(delta_buff10k), extend(delta, delta_buff10k))

baseline = c(rast('GIS/landscape_rasters/veg_baseline.tif'),
             rast('GIS/landscape_rasters/veg_baseline_winter.tif'))
levels(baseline) <- NULL
# key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')
key = readxl::read_excel('GIS/VEG_key.xlsx')

# source data---------
skey = read_csv('GIS/original_source_data/State Class Rasters/key.csv')

bbau = rast('GIS/original_source_data/State Class Rasters/scn421.sc.it1.ts2050.tif') %>%
  project(template, method = 'near') %>%
  mask(template) %>%
  # keep only the footprint of expanded perennial crops (code=20) - reclassify
  # to baseline code for general perennial crops (10); all others convert to NA
  subst(from = 20, to = 10, others = NA)
# note: missing southwest corner of Delta

# # compare to baseline
# tab = crosstab(c(baseline, bbau), useNA = TRUE, long = TRUE)
# tab %>% rlang::set_names(c('baseline', 'bbau', 'n')) %>%
#   left_join(key %>% select(baseline = CODE_BASELINE, from = CODE_NAME)) %>%
#   left_join(key %>% select(bbau = CODE_BASELINE, to = CODE_NAME)) %>%
#   filter(!is.na(to)) %>%
#   arrange(desc(n))
# # would convert to orch from all other types, including wetlands, water,
# # riparian, and urban

# CLEAN UP PROJECTIONS---------
# erase areas from bbau projections that are unsuitable for conversion: urban,
# water, forest/shrub, wetland & riparian (unless unprotected), and don't allow
# rice (assume soils unsuitable for orchards)
# --> also, don't replace existing perennial crops with more detailed subtypes

# first eliminate urban, water, forest/shrub, rice, & existing perennial as
# candidates for conversion to perennial crops:
bbau_limited1 = c(lapp(c(bbau, baseline$baseline),
                    function(x, y) {
                      ifelse(y %in% c(11:19, 30, 60, 90, 100), NA, x)
                    }),
                  lapp(c(bbau, baseline$baseline_win),
                       function(x, y) {
                         ifelse(y %in% c(11:19, 30, 60, 90, 100), NA, x)
                       }))
# freq(bbau); freq(bbau_limited1)
# dropped a lot (but a lot of existing perennial crops)


# now eliminate riparian/wetland that is in a protected area/easement as
# candidates for conversion:
protected = rast('GIS/scenario_inputs/protected_areas.tif')
#from restoration scenario; 1 = protected, 2 = easement

protected_ripwet = baseline %>%
  subst(from = c(1:69, 90:130), to = NA) %>% # keep only riparian & wetland
  mask(protected)

bbau_limited2 = lapp(c(bbau_limited1, protected_ripwet),
                     function(x, y) {
                       ifelse(y %in% c(71:87), NA, x)
                     })
# freq(bbau); freq(bbau_limited1); freq(bbau_limited2)
# small further decrease



# of the remaining projected perennial crops, examine range of patch sizes;
# reasonable to assume perennial expansion on small patches of ~ 1 pixel?
bbau_patches = bbau_limited2 %>% patches(directions = 8)
patchlist = freq(bbau_patches) %>% as_tibble() %>%
  mutate(ID = c(1:nrow(freq(bbau_patches))),
         area.ha = count * 30 * 30 / 10000)
# 8052 patches, ranging from 0.09 ha (1 pixel) to 579 ha

# patchlist = patchlist %>% filter(area.ha > 0.4)
# # classify all included patch IDs as new perennial cropland; rest to NA
# bbau_clean = classify(bbau_patches,
#                       rcl = patchlist %>% select(from = value) %>%
#                         mutate(to = 10) %>% as.matrix(),
#                       othersNA = TRUE)
# freq(bbau); freq(bbau_limited1); freq(bbau_limited2); freq(bbau_clean)
# # relatively small further reduction

# keep all patches, no matter the size
bbau_clean = bbau_limited2
writeRaster(bbau_clean,
            'GIS/scenario_inputs/perex_added.tif',
            overwrite = TRUE)

# ORIGINAL VERSION-------
## ASSIGN SUBCLASSES-------
# similar process as for assigning subclasses/details to riparian restoration
# pixels, but use larger buffer size to ensure all have some perennial crops
# within the buffer

### find proportions---------
# from patchlist above, convert to polygons, find centroids, and buffer by 10km
bbau_patches1_buff = as.polygons(bbau_patches) %>% centroids() %>%
  buffer(width = 10000) #8052 polygons
names(bbau_patches1_buff) = 'buffers'

# for each buffer, find frequency of perennial crop types in surrounding 10km
# from baseline
baseline_perennials = classify(baseline,
                               rcl = matrix(c(11, 11,
                                              15, 15,
                                              19, 19), byrow = TRUE, ncol = 2),
                               othersNA = TRUE)
names(baseline_perennials) = 'baseline'
plot(cover(baseline_perennials, bbau_clean))

# SLOW STEP: extract any baseline perennial cell values for all cells in each buffer
memory.limit(size = 48000)

# 1-2000
buff_values1 = extract(baseline_perennials, bbau_patches1_buff[1:2000])
buff_values1  = buff_values1 %>% filter(!is.na(lyr1))
buff_values1 %>% pull(ID) %>% unique() %>% length() #1997 (3 not covered by 10km buffer)
# for each buffer, calculate the proportions in each subclass
buff_values1_prop = buff_values1 %>%
  select(polygon_num = ID, class = lyr1) %>%
  group_by(polygon_num, class) %>% count() %>%
  group_by(polygon_num) %>% mutate(total = sum(n)) %>% ungroup() %>%
  mutate(prop = n/total) %>%
  # eliminate tiny proportions and recalculate prop
  filter(prop >= 0.05) %>%
  group_by(polygon_num) %>%
  mutate(total = sum(n)) %>%
  # how many subclasses per patch?
  add_count(name = 'n_subclasses') %>%
  ungroup() %>%
  mutate(prop = n/total) %>%
  left_join(patchlist[1:2000,] %>%
              select(patch_name = value, ncells = count) %>%
              mutate(polygon_num = c(1:2000),
                     buffer_num = c(1:2000)), by = 'polygon_num')
write_csv(buff_values1_prop, 'GIS/scenario_inputs/perennial_props1.csv')
rm(buff_values1)

# 2001-4000
buff_values2 = extract(baseline_perennials, bbau_patches1_buff[2001:4000])
buff_values2 = buff_values2 %>% filter(!is.na(lyr1))
buff_values2 %>% pull(ID) %>% unique() %>% length() #2000 (all covered by 10km buffer)
buff_values2_prop = buff_values2 %>%
  select(polygon_num = ID, class = lyr1) %>%
  group_by(polygon_num, class) %>% count() %>%
  group_by(polygon_num) %>% mutate(total = sum(n)) %>% ungroup() %>%
  mutate(prop = n/total) %>%
  # eliminate tiny proportions and recalculate prop
  filter(prop >= 0.05) %>%
  group_by(polygon_num) %>%
  mutate(total = sum(n)) %>%
  # how many subclasses per patch?
  add_count(name = 'n_subclasses') %>%
  ungroup() %>%
  mutate(prop = n/total) %>%
  left_join(patchlist[2001:4000,] %>%
              select(patch_name = value, ncells = count) %>%
              mutate(polygon_num = c(1:2000),
                     buffer_num = c(2001:4000)), by = 'polygon_num')
write_csv(buff_values2_prop, 'GIS/scenario_inputs/perennial_props2.csv')
rm(buff_values2)

# 4001-6000
buff_values3 = extract(baseline_perennials, bbau_patches1_buff[4001:6000])
buff_values3 = buff_values3 %>% filter(!is.na(lyr1))
buff_values3 %>% pull(ID) %>% unique() %>% length() #2000 (all covered by 10km buffer)
buff_values3_prop = buff_values3 %>%
  select(polygon_num = ID, class = lyr1) %>%
  group_by(polygon_num, class) %>% count() %>%
  group_by(polygon_num) %>% mutate(total = sum(n)) %>% ungroup() %>%
  mutate(prop = n/total) %>%
  # eliminate tiny proportions and recalculate prop
  filter(prop >= 0.05) %>%
  group_by(polygon_num) %>%
  mutate(total = sum(n)) %>%
  # how many subclasses per patch?
  add_count(name = 'n_subclasses') %>%
  ungroup() %>%
  mutate(prop = n/total) %>%
  left_join(patchlist[4001:6000,] %>%
              select(patch_name = value, ncells = count) %>%
              mutate(polygon_num = c(1:2000),
                     buffer_num = c(4001:6000)), by = 'polygon_num')
write_csv(buff_values3_prop, 'GIS/scenario_inputs/perennial_props3.csv')
rm(buff_values3)

# 6001-8052
buff_values4 = extract(baseline_perennials, bbau_patches1_buff[6001:8052])
buff_values4 = buff_values4 %>% filter(!is.na(baseline))
buff_values4 %>% pull(ID) %>% unique() %>% length() #2052 (all covered by 10km buffer)
# NOTE: ID is now just the order of the subset of buffers examined, not a unique
# number
buff_values4_prop = buff_values4 %>%
  select(polygon_num = ID, class = baseline) %>%
  group_by(polygon_num, class) %>% count() %>%
  group_by(polygon_num) %>% mutate(total = sum(n)) %>% ungroup() %>%
  mutate(prop = n/total) %>%
  # eliminate tiny proportions and recalculate prop
  filter(prop >= 0.05) %>%
  group_by(polygon_num) %>%
  mutate(total = sum(n)) %>%
  # how many subclasses per patch?
  add_count(name = 'n_subclasses') %>%
  ungroup() %>%
  mutate(prop = n/total) %>%
  left_join(patchlist[6001:8052,] %>%
              select(patch_name = value, ncells = count) %>%
              mutate(polygon_num = c(1:2052),
                     buffer_num = c(6001:8052)), by = 'polygon_num')
write_csv(buff_values4_prop, 'GIS/scenario_inputs/perennial_props4.csv')
rm(buff_values4)

buff_values_prop = bind_rows(buff_values1_prop,
                             buff_values2_prop,
                             buff_values3_prop,
                             buff_values4_prop)
buff_values_prop %>% pull(patch_name) %>% unique() %>% length() #8049 (all but 3)
buff_values_prop %>% select(patch_name, n_subclasses) %>% distinct() %>%
  pull(n_subclasses) %>% as.factor() %>% summary()
# most have 2 possible subclass assignments, ranging 1-3

### ROUND 1-------
# SINGLES: for patches with only a single option for a subclass (1664), make the
# assignments straight-forward (can't fold into sampling procedure below because
# sample function doesn't work as expected with only one option)
bbau_singles = classify(
  bbau_patches,
  rcl = buff_values_prop %>% filter(n_subclasses == 1) %>%
    select(from = patch_name, to = class) %>% as.matrix(),
  othersNA = TRUE)
c(bbau_singles, baseline_perennials) %>% plot(colNA = 'black')
c(bbau_singles, cover(bbau_singles, baseline_perennials)) %>% plot(colNA = 'black')

# DOUBLES & TRIPLES: for patches with more than one possible subclass, randomly
# assign entire patch to one of the subclasses, weighted by proportion
buff_doubles = buff_values_prop %>% filter(n_subclasses > 1) %>%
  select(patch_name, class, prop) %>%
  split(.$patch_name) %>%
  purrr::map_dfr(function(x) {
    tibble(patch_name = x$patch_name[1],
           class = sample(x$class, size = 1, prob = x$prop))
  })

# assign new class values to patches
bbau_doubles = classify(
  bbau_patches,
  rcl = buff_doubles %>% select(from = patch_name, to = class) %>% as.matrix(),
  othersNA = TRUE)

c(bbau_doubles, baseline_perennials) %>% plot(colNA = 'black')
c(cover(bbau_singles, bbau_doubles), baseline_perennials) %>% plot(colNA = 'black')

# remaining 3 patches?
cover(bbau_singles, bbau_doubles) %>% cover(bbau_patches) %>%
  plot(col = viridis::viridis(5), colNA = 'gray80')

# OVERLAY round 1 results on baseline
bbau_round1 = cover(bbau_singles, bbau_doubles) %>% cover(baseline)
plot(bbau_round1)

### ROUND 2-------
# use the newly added perennial crop subclasses to inform the remaining
# undefined pixels

# extract only the perennial crop subclasses in the updated round 1 raster
baseline_perennials2 = classify(
  bbau_round1,
  rcl = matrix(c(11, 11,
                 15, 15,
                 19, 19), byrow = TRUE, ncol = 2),
  othersNA = TRUE)

# erase patches already accounted for
bbau_patches2 = mask(bbau_patches, baseline_perennials2, inverse = TRUE)
plot(bbau_patches2)
patchlist2 = freq(bbau_patches2) %>% as_tibble() %>%
  mutate(order = c(1:nrow(freq(bbau_patches2)))) %>%
  select(order, patch_name = value, count)
#3 patches left

# convert to polygons and buffer
bbau_patches2_buff = as.polygons(bbau_patches2) %>% buffer(width = 10000)

# extract baseline cell values for all cells in each buffer
buff_values_round2 = extract(baseline_perennials2, bbau_patches2_buff)
buff_values_round2 %>% filter(!is.na(lyr1)) %>% pull(ID) %>% unique() %>% length()
# all 3 accounted for

# for each buffer, calculate the proportions in each subclass
buff_values_prop2 = buff_values_round2 %>%
  filter(!is.na(lyr1)) %>%
  select(polygon_num = ID, class = lyr1) %>%
  group_by(polygon_num, class) %>% count() %>%
  group_by(polygon_num) %>% mutate(total = sum(n)) %>% ungroup() %>%
  mutate(prop = n/total) %>%
  # eliminate small proportions and recalculate proportions
  filter(prop >= 0.05) %>%
  group_by(polygon_num) %>%
  mutate(total = sum(n)) %>%
  # how many subclasses per patch?
  add_count(name = 'n_subclasses') %>%
  ungroup() %>%
  mutate(prop = n/total) %>%
  left_join(patchlist2 %>%
              select(polygon_num = order, patch_name, ncells = count))

# SINGLES: all only had a single option
bbau_singles2 = classify(
  bbau_patches2,
  rcl = buff_values_prop2 %>% select(from = patch_name, to = class) %>%
    as.matrix(),
  othersNA = TRUE)

# OVERLAY round 2 results on baseline
bbau_round2 = cover(bbau_singles2, bbau_round1)
plot(bbau_round2)

writeRaster(bbau_round2,
            'GIS/scenario_inputs/perex_added_detail.tif',
            overwrite = TRUE)

## CROSSTAB-------
crosstab(c(bbau_clean, bbau_round2), useNA = TRUE, long = TRUE)
#should align perfectly

crosstab(c(baseline, bbau_round2), useNA = TRUE, long = TRUE)
#should not have conversions from unsuitable types


## FINALIZE-----------
# overlay perennial crop footprint details on baseline
levels(baseline$baseline) <- NULL
levels(baseline$baseline_win) <- NULL
scenario_perex = cover(bbau_round2, baseline)

levels(scenario_perex[[1]]) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
levels(scenario_perex[[2]]) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
coltab(scenario_perex[[1]]) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
coltab(scenario_perex[[2]]) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
names(scenario_perex) = c('scenario2_perennialexpand',
                          'scenario2_perennialexpand_win')
plot(scenario_perex)
writeRaster(scenario_perex,
            paste0('GIS/scenario_rasters/', names(scenario_perex), '.tif'),
            overwrite = TRUE)

# side-by-side comparison to baseline
baseline = c(rast('GIS/landscape_rasters/veg_baseline.tif'),
             rast('GIS/landscape_rasters/veg_baseline_winter.tif'))
plot(c(baseline[[1]], scenario_perex[[1]]))


# ALTERNATIVE VERSION---------
# keep it simple -- just don't let bbau replace water, urban, existing perennial
bbau_limited1_alt = lapp(c(bbau, baseline$baseline),
                         function(x, y) {
                           ifelse(y %in% c(11:19, 60, 90), NA, x)
                         })
# freq(bbau); freq(bbau_limited1); freq(bbau_limited1_alt)

# and mask out any protected areas (not limited to riparian/wetland)
bbau_limited2_alt = c(lapp(c(bbau_limited1_alt, protected),
                           function(x, y) {
                             ifelse(y %in% c(1:2), NA, x)
                           }))

# keep all patches, no matter the size
writeRaster(bbau_limited2_alt,
            'GIS/scenario_inputs/perex_added_alt.tif',
            overwrite = TRUE)

# tmp = cover(bbau_limited2_alt, baseline$baseline)
# levels(tmp) <- key %>%
#   select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
#   as.data.frame()
# levels(baseline$baseline) <- key %>%
#   select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
#   as.data.frame()
# sum_landcover(landscapes = c(baseline$baseline, tmp), mask = delta)

## assign subclasses----------

## find unique patches:
# of the remaining projected perennial crops, examine range of patch sizes;
# reasonable to assume perennial expansion on small patches of ~ 1 pixel?
bbau_patches_alt = bbau_limited2_alt %>% patches(directions = 8) #values 1-7831
patchlist = freq(bbau_patches_alt) %>% as_tibble() %>%
  mutate(ID = c(1:nrow(freq(bbau_patches_alt))),
         area.ha = count * 0.09)
# 6817 patches, ranging from 0.09 ha (1 pixel) to 570 ha
# value = unique value assigned in bbau_patches_alt (not sequential?)
# ID = sequential value 1-6817

## find 10km buffers around each patch:
# convert raster patches to polygons, find centroids, and buffer by 10km
bbau_patches_alt_buff = as.polygons(bbau_patches_alt) %>% centroids() %>%
  buffer(width = 10000) #6817 polygons corresponding to # of patches
names(bbau_patches_alt_buff) = 'buffers'
# values in polygons correspond to values in bbau_patches_alt (not sequential),
# but their indexing/order should match ID in patchlist

## reference data:
# from baseline, extract just the perennial land cover classes
baseline_perennials = subst(baseline$baseline, from = c(20:150), NA)
names(baseline_perennials) = 'baseline'
plot(c(baseline_perennials, cover(baseline_perennials, bbau_limited2_alt)))

### find proportions:
# rasterize each buffer polygon use as a mask on baseline_perennials,
# to find frequency of perennial crop types in each buffer
patch_freq = purrr::map_df(
  c(1:length(bbau_patches_alt_buff)),
  function(polynum) {
    mask(baseline_perennials,
         rasterize(bbau_patches_alt_buff[polynum], baseline$baseline)) %>%
      freq() %>% as_tibble() %>% mutate(ID = polynum)})

patch_prop = patch_freq %>%
  group_by(ID) %>%
  mutate(total = sum(count)) %>%
  # how many subclasses per patch?
  add_count(name = 'n_subclasses') %>%
  ungroup() %>%
  mutate(prop = count/total) %>%
  select(ID, n_subclasses, CODE_BASELINE = value, count, prop) %>%
  left_join(patchlist %>%
              select(ID, patch_name = value, ncells = count),
            by = 'ID')
write_csv(patch_prop, 'GIS/scenario_inputs/perennial_props_alt.csv')

patch_prop %>% select(patch_name, n_subclasses) %>% distinct() %>%
  pull(n_subclasses) %>% as.factor() %>% summary()
# most have 3 possible subclass assignments, ranging 2-3

### ROUND 1-------
# SINGLES: none?

# DOUBLES & TRIPLES: for patches with more than one possible subclass, randomly
# assign entire patch to one of the subclasses, weighted by proportion
buff_prop = patch_prop %>%
  select(patch_name, CODE_BASELINE, prop) %>%
  split(.$patch_name) %>%
  purrr::map_dfr(function(x) {
    tibble(patch_name = x$patch_name[1],
           CODE_BASELINE = sample(x$CODE_BASELINE, size = 1, prob = x$prop))
  })

# assign new class values to patches
bbau_assignments = classify(
  bbau_patches_alt,
  rcl = buff_prop %>% select(from = patch_name, to = CODE_BASELINE) %>% as.matrix(),
  others = NA)
plot(c(bbau_patches_alt, bbau_assignments))
freq(c(bbau_patches_alt, bbau_assignments)) %>% as_tibble() %>%
  group_by(layer) %>% summarize(count = sum(count)) # a few pixels not yet assigned

# remaining 3 patches?
patchlist %>% filter(!value %in% buff_prop$patch_name) #127, 133, 136
patchlist[patchlist$value %in% c(127, 133, 136),] #ID 109, 114, 116
plot(delta, col = 'blue')
plot(cover(bbau_assignments, baseline_perennials), add = TRUE,
     col = c('purple', 'orange', 'red'))
bbau_patches_alt_buff[c(109, 114, 116)] %>% plot(add = TRUE)
# --> nearly identical patches outside the core delta, largest is ~17 acres


### ROUND 2-------
# use the newly added perennial crop subclasses to inform the remaining
# undefined pixels

# extract only the perennial crop subclasses in the updated round 1 raster
updated_perennials = cover(bbau_assignments, baseline_perennials)
plot(c(baseline_perennials, updated_perennials))

# for each remaining buffer, find frequency of perennial crop types in surrounding 10km
# from baseline
patch_freq2 = purrr::map_df(
  c(109, 114, 116),
  function(polynum) {
    mask(updated_perennials,
         rasterize(bbau_patches_alt_buff[polynum], baseline$baseline)) %>%
      freq() %>% as_tibble() %>% mutate(ID = polynum)})
# they each only have one possible value (19)

bbau_assignments2 = subst(bbau_patches_alt,
                          from = c(127, 133, 136), to = 19, others = NA)
bbau_assignments_all = cover(bbau_assignments, bbau_assignments2)
freq(c(bbau_patches_alt, bbau_assignments_all)) %>% as_tibble() %>%
  group_by(layer) %>% summarize(count = sum(count)) # identical

c(bbau_assignments_all, cover(bbau_assignments_all, baseline_perennials)) %>%
  plot(colNA = 'black')

writeRaster(bbau_assignments_all,
            'GIS/scenario_inputs/perex_added_detail_alt.tif',
            overwrite = TRUE)

## CROSSTAB-------
crosstab(c(bbau_limited2_alt, bbau_assignments_all), useNA = TRUE, long = TRUE)
#should align perfectly-yes

crosstab(c(baseline$baseline, bbau_assignments_all), useNA = TRUE, long = TRUE)
#should not have conversions from unsuitable types: urban (60), water (90)

## FINALIZE-----------
# overlay perennial crop footprint details on baseline
levels(baseline$baseline) <- NULL
levels(baseline$baseline_win) <- NULL
scenario_perex_alt = cover(bbau_assignments_all, baseline)

levels(scenario_perex_alt[[1]]) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
levels(scenario_perex_alt[[2]]) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
coltab(scenario_perex_alt[[1]]) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
coltab(scenario_perex_alt[[2]]) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
names(scenario_perex_alt) = c('scenario2_perennialexpand_alt',
                          'scenario2_perennialexpand_alt_win')
plot(scenario_perex_alt)
writeRaster(scenario_perex_alt,
            paste0('GIS/scenario_rasters/', names(scenario_perex_alt), '.tif'),
            overwrite = TRUE)
