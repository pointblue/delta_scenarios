# README---------
# From baseline land cover layer, develop scenario of perennial crop expansion
# based on Central Valley Futures project (Wilson et al. 2021), using the BBAU
# ("bad business as usual") scenario for the year 2050, including historically
# high rates of perennial crop expansion and no restoration.
#
# Assumptions:
# - conversions will not happen within the existing footprint for
# certain land cover classes (urban, woodland/scrub, riparian, wetland, water,
# rice)
# - conversions will not happen on patches < 1 acre in size (to eliminate
# patches with only a very few pixels).
# - the specific subclasses of new perennial crops will be proportional to the
# subclasses in the baseline from the surrounding region (~5km buffer). This
# should not affect bird distribution models, but may help account for variation
# in some of the other metrics among subclasses, so this level of detail doesn't
# need to be spatial at this time, but it may help display and compare scenario
# maps.

# PACKAGES & FUNCTIONS
source('R/packages.R')

# reference data:
delta = rast('GIS/boundaries/delta.tif')
delta_buff10k = read_sf('GIS/boundaries/Legal_Delta_boundary_buff10k.shp') %>%
  st_transform(crs = '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')
template = rasterize(vect(delta_buff10k), extend(delta, delta_buff10k))

baseline = rast('GIS/landscape_rasters/veg_baseline.tif')
baseline_win = rast('GIS/landscape_rasters/veg_baseline_winter.tif')
key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')

# source data---------
skey = read_csv('GIS/original_source_data/State Class Rasters/key.csv',
                show_col_types = FALSE)

bbau = rast('GIS/original_source_data/State Class Rasters/scn421.sc.it1.ts2050.tif') %>%
  project(template, method = 'near') %>%
  mask(template) %>%
  # keep only the footprint of expanded perennial crops (code=20) - reclassify
  # to baseline code for general perennial crops (10); all others convert to NA
  classify(rcl = matrix(c(20, 10), byrow = TRUE, ncol = 2), othersNA = TRUE)
# note: missing southwest corner of Delta

# compare to baseline
tab = crosstab(c(baseline, bbau), useNA = TRUE, long = TRUE)
tab %>% rlang::set_names(c('baseline', 'bbau', 'n')) %>%
  left_join(key %>% select(baseline = CODE_BASELINE, from = CODE_NAME)) %>%
  left_join(key %>% select(bbau = CODE_BASELINE, to = CODE_NAME)) %>%
  filter(!is.na(to)) %>%
  arrange(desc(n))
# would convert to orch from all other types, including wetlands, water,
# riparian, and urban

# CLEAN UP PROJECTIONS---------
# erase areas from bbau projections that are unsuitable for conversion:
# urban, water, forest/shrub, wetland, riparian, and don't allow rice (assume
# soils unsuitable for orchards)
# --> also, don't replace existing perennial crops with more detailed subtypes

bbau_limited = lapp(c(bbau, baseline),
                    function(x, y) {
                      ifelse(y %in% c(11:19, 30, 60, 70:100), NA, x)
                    })
freq(bbau); freq(bbau_limited)
#dropped a lot (but a lot of existing perennial crops)

# of the remaining projected perennial crops, keep only patches > 1 acre
# (eliminate many single pixel additions)
bbau_patches = bbau_limited %>% patches(directions = 8) #values up to 15599
patchlist = freq(bbau_patches) %>% as_tibble() %>%
  mutate(ID = c(1:nrow(freq(bbau_patches))),
         area.ha = count * 30 * 30 / 10000) %>%
  filter(area.ha >= 0.4)
# 3392 patches remaining

# classify all included patch IDs as new perennial cropland; rest to NA
bbau_clean = classify(bbau_patches,
                      rcl = patchlist %>% select(from = value) %>%
                        mutate(to = 10) %>% as.matrix(),
                      othersNA = TRUE)
freq(bbau); freq(bbau_limited); freq(bbau_clean)
# relatively small further reduction


# ASSIGN SUBCLASSES-------
# similar process as for assigning subclasses/details to riparian restoration
# pixels; consider doing in rounds because not all patches have existing
# perennial crops within 2km or 5km

## ROUND 1---------
# initial assignments from existing perennial crops within 5km

# extract only the perennial crop subclasses in the baseline raster
baseline_perennials = classify(baseline,
                               rcl = matrix(c(11, 11,
                                              15, 15,
                                              19, 19), byrow = TRUE, ncol = 2),
                               othersNA = TRUE)
plot(cover(baseline_perennials, bbau_clean))

# again find distinct patches:
bbau_patches1 = bbau_clean %>% patches(directions = 8)
patchlist1 = freq(bbau_patches1) %>% as_tibble() %>%
  mutate(order = c(1:nrow(freq(bbau_patches1)))) %>%
  select(order, patch_name = value, count)
#3392 distinct patches

# convert to polygons, find centroids and buffer
bbau_patches1_buff = as.polygons(bbau_patches1) %>% centroids() %>%
  buffer(width = 5000)

# SLOW STEP: extract baseline cell values for all cells in each buffer
buff_values1 = extract(baseline_perennials, bbau_patches1_buff)

# check for missing buffers (with all cells within 5km NA)
buff_values1 %>% filter(!is.na(lyr1)) %>%
  pull(ID) %>% unique() %>% length()
#3383 (which means 9 not yet covered)

# for each buffer, calculate the proportions in each subclass
buff_values1_prop = buff_values1 %>%
  filter(!is.na(lyr1)) %>%
  select(polygon_num = ID, class = lyr1) %>%
  group_by(polygon_num, class) %>% count() %>%
  group_by(polygon_num) %>% mutate(total = sum(n)) %>% ungroup() %>%
  mutate(prop = n/total) %>%
  # pull(prop) %>% hist()
  # eliminate small proportions and recalculate proportions
  filter(prop >= 0.05) %>%
  group_by(polygon_num) %>% mutate(total = sum(n)) %>% ungroup() %>%
  mutate(prop = n/total) %>%
  # how many subclasses per patch?
  group_by(polygon_num) %>% add_count(name = 'n_subclasses') %>% ungroup() %>%
  left_join(patchlist1 %>%
              select(polygon_num = order, patch_name, ncells = count))
buff_values1_prop %>% select(patch_name, n_subclasses) %>% distinct() %>%
  pull(n_subclasses) %>% as.factor() %>% summary()
# most have 2 possible subclass assignments, ranging 1-3

# SINGLES: for patches with only a single option for a subclass (1079), make the
# assignments straight-forward (can't fold into sampling procedure below because
# sample function doesn't work as expected with only one option)
bbau_round1_singles = classify(
  bbau_patches1,
  rcl = buff_values1_prop %>% filter(n_subclasses == 1) %>%
    select(from = patch_name, to = class) %>% as.matrix(),
  othersNA = TRUE)
plot(c(baseline_perennials, bbau_clean, bbau_round1_singles))

# DOUBLES & TRIPLES: for patches with more than one possible subclass (2326),
# randomly assign the total number of cells in each patch to one of the
# subclasses
# - first find the polygon numbers that are relevant:
buff_doubles1 = buff_values1_prop %>% filter(n_subclasses > 1) %>%
  select(polygon_num, patch_name) %>% distinct() %>%
  mutate(ordernum = c(1:length(polygon_num))) #2326

# - remove any patches not included in this set:
bbau_round1_doubles = classify(
  bbau_patches1,
  rcl = buff_doubles1 %>% select(from = patch_name, to = patch_name) %>%
    as.matrix(),
  othersNA = TRUE)

# - SEMI-SLOW STEP: find cell numbers for each of these unique patches
cellnum1 = buff_doubles1 %>% split(.$patch_name) %>%
  purrr::map_df(~cells(bbau_round1_doubles, .x$patch_name),
                .id = 'patch_name') %>%
  rename(cellnum = lyr1) %>%
  mutate(patch_name = as.numeric(patch_name)) %>%
  left_join(buff_doubles1, by = 'patch_name')

# - randomly assign the total number of cells in each patch to one of the
# subclasses according to the proportion in the surrounding buffer
new1 = buff_values1_prop %>% filter(n_subclasses > 1) %>% split(.$patch_name) %>%
  purrr::map_df(~sample(x = .x %>% pull(class), # possible values
                        size = .$ncells[1],
                        prob = .x %>% pull(prop),
                        replace = TRUE) %>% as_tibble(),
                .id = 'patch_name') %>%
  mutate(patch_name = as.numeric(patch_name))

# - match the new random values to the cell numbers
assignments1 = bind_cols(cellnum1, new1)
assignments1 %>% filter(patch_name...1 != patch_name...5)  # should be none!

# transfer the random assignments to the corresponding patch IDs
bbau_round1_doubles[assignments1$cellnum] <- assignments1$value

freq(bbau_round1_doubles) #should only be subclasses 11, 15, or 19
cover(bbau_round1_singles, bbau_round1_doubles) %>% plot()
cover(bbau_round1_singles, bbau_round1_doubles) %>% cover(bbau_patches1) %>% plot()

# OVERLAY round 1 results on baseline
bbau_round1 = cover(bbau_round1_singles, bbau_round1_doubles) %>% cover(baseline)
plot(bbau_round1)

## ROUND 2-------
# use the newly added perennial crop subclasses to inform the remaining
# undefined pixels

# extract only the perennial crop subclasses in the updated round 1 raster
baseline_perennials2 = classify(
  bbau_round1,
  rcl = matrix(c(11, 11,
                 15, 15,
                 19, 19), byrow = TRUE, ncol = 2),
  othersNA = TRUE)
plot(cover(baseline_perennials2, bbau_clean))

# erase patches already accounted for
bbau_patches2 = mask(bbau_patches1, baseline_perennials2, inverse = TRUE)
plot(bbau_patches2)
patchlist2 = freq(bbau_patches2) %>% as_tibble() %>%
  mutate(order = c(1:nrow(freq(bbau_patches2)))) %>%
  select(order, patch_name = value, count)
#9 distinct patches left

# convert to polygons, find centroids and buffer
bbau_patches2_buff = as.polygons(bbau_patches2) %>% centroids() %>%
  buffer(width = 20000)

# extract baseline cell values for all cells in each buffer
buff_values2 = extract(baseline_perennials2, bbau_patches2_buff)

# check for missing buffers (with all cells within 5km NA)
buff_values2 %>% filter(!is.na(lyr1)) %>%
  pull(ID) %>% unique() %>% length()
#97 (all are covered)

# for each buffer, calculate the proportions in each subclass
buff_values2_prop = buff_values2 %>%
  filter(!is.na(lyr1)) %>%
  select(polygon_num = ID, class = lyr1) %>%
  group_by(polygon_num, class) %>% count() %>%
  group_by(polygon_num) %>% mutate(total = sum(n)) %>% ungroup() %>%
  mutate(prop = n/total) %>%
  # eliminate small proportions and recalculate proportions
  filter(prop >= 0.05) %>%
  group_by(polygon_num) %>% mutate(total = sum(n)) %>% ungroup() %>%
  mutate(prop = n/total) %>%
  # how many subclasses per patch?
  group_by(polygon_num) %>% add_count(name = 'n_subclasses') %>% ungroup() %>%
  left_join(patchlist2 %>%
              select(polygon_num = order, patch_name, ncells = count))

buff_values2_prop %>% select(patch_name, n_subclasses) %>% distinct() %>%
  pull(n_subclasses) %>% as.factor() %>% summary()

# SINGLES: there are none in this round!


# DOUBLES: for patches with more than one possible subclass (14), randomly
# assign the total number of cells in each patch to one of the subclasses
# - first find the polygon numbers that are relevant:
buff_doubles2 = buff_values2_prop %>% filter(n_subclasses > 1) %>%
  select(polygon_num, patch_name) %>% distinct() %>%
  mutate(ordernum = c(1:length(polygon_num)))

# - remove any patches not included in this set:
bbau_round2_doubles = classify(
  bbau_patches2,
  rcl = buff_doubles2 %>% select(from = patch_name, to = patch_name) %>%
    as.matrix(),
  othersNA = TRUE)

# - find cell numbers for each of these unique patches
cellnum2 = buff_doubles2 %>% split(.$patch_name) %>%
  purrr::map_df(~cells(bbau_round2_doubles, .x$patch_name),
                .id = 'patch_name') %>%
  rename(cellnum = lyr1) %>%
  mutate(patch_name = as.numeric(patch_name)) %>%
  left_join(buff_doubles2, by = 'patch_name')

# - randomly assign the total number of cells in each patch to one of the
# subclasses according to the proportion in the surrounding buffer
new2 = buff_values2_prop %>% filter(n_subclasses > 1) %>% split(.$patch_name) %>%
  purrr::map_df(~sample(x = .x %>% pull(class), # possible values
                        size = .$ncells[1],
                        prob = .x %>% pull(prop),
                        replace = TRUE) %>% as_tibble(),
                .id = 'patch_name') %>%
  mutate(patch_name = as.numeric(patch_name))

# match the new random values to the cell numbers
assignments2 = bind_cols(cellnum2, new2)
assignments2 %>% filter(patch_name...1 != patch_name...5)  # should be none!

# transfer the random assignments to the corresponding patch IDs
bbau_round2_doubles[assignments2$cellnum] <- assignments2$value

freq(bbau_round2_doubles) #should only be subclasses 11, 15, or 19
plot(cover(bbau_round2_doubles, bbau_patches2))

# OVERLAY round 1 results on baseline
bbau_round2 = cover(bbau_round2_doubles, bbau_round1)
plot(as.factor(bbau_round2))

## CROSSTAB-------
crosstab(c(bbau_clean,
           cover(bbau_round1_singles, bbau_round1_doubles) %>%
             cover(bbau_round2_doubles)),
         useNA = TRUE, long = TRUE) #should align perfectly

crosstab(c(baseline, bbau_round2),
         useNA = TRUE, long = TRUE)
#should not have conversions from unsuitable types

# OVERLAY-----------
# overlay perennial crop footprint details on baseline

scenario_perex = bbau_round2
levels(scenario_perex) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
coltab(scenario_perex) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
names(scenario_perex) = 'scenario_perennial_expansion'

writeRaster(scenario_perex,
            'GIS/scenario_rasters/scenario2_perennialexpand.tif',
            overwrite = TRUE)

scenario_perex_win = cover(bbau_round1_singles, bbau_round1_doubles) %>%
  cover(bbau_round2_doubles) %>%
  cover(baseline_win)
levels(scenario_perex_win) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
coltab(scenario_perex_win) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
names(scenario_perex_win) = 'scenario_perennial_expansion_win'

writeRaster(scenario_perex_win,
            'GIS/scenario_rasters/scenario2_perennialexpand_win.tif',
            overwrite = TRUE)


levels(baseline) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
coltab(baseline) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
names(baseline) = 'baseline'
plot(c(baseline, scenario_perex, scenario_perex_win))

# calculate change-------
delta_perex = DeltaMultipleBenefits::calculate_change(
  baseline = baseline %>% mask(delta),
  scenario = scenario_perex %>% mask(delta))
delta_perex %>%
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
  theme_bw() + coord_flip() + ylim(-50, 100) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))
ggsave('fig/change_scenario2_perennialexpand.png', height = 7.5, width = 6)
# large increase in orchard cover, at the expense of: idle, row, corn, alfalfa,
# wheat, pasture, grassland, field, grain
