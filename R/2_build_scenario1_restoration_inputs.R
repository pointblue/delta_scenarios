# README---------
# From baseline land cover layer, develop scenario of meeting Delta Plan
# restoration objectives for riparian vegetation and managed/seasonal wetlands.
# --> This script develops several of the input layers needed to actually build
# the restoration scenario.

# PACKAGES & FUNCTIONS
source('R/0_packages.R')

# # reference data:
delta = rast('GIS/boundaries/delta.tif')
delta_buff10k = read_sf('GIS/boundaries/Legal_Delta_boundary_buff10k.shp') %>%
  st_transform(crs = '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')
delta_boundary = read_sf('GIS/boundaries/Legal_Delta_Boundary.shp') %>%
  st_transform(st_crs(delta_buff10k))
template = rasterize(vect(delta_buff10k), extend(delta, delta_buff10k))

# restoration plans------
# existing planned/in-progress restoration projects: based on EcoAtlas, with
# additional info/edits from DSLPT EcoRestore and examining project plans

# EcoAtlas data access: https://www.ecoatlas.org/regions/ecoregion/statewide
# (see "tools" button to right, for an option to download .shp or .csv)
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
  filter(!name %in% c('Delta Wetlands Project Compensatory Mitigation', 'Holland Tract')) %>%
  mutate(
    code = case_when(
      name %in% c('Sherman Island - Belly Wetland Restoration',
                  'Twitchell Island - West End Wetlands') ~ 81, #perennial
      habitat_ty == 'valley foothill riparian' ~ 70,
      #assume other wetlands are seasonal
      TRUE ~ 82)) %>%
  vect() %>%
  rasterize(template, field = 'code')
writeRaster(plans, 'GIS/scenario_inputs/restoration_plans.tif',
            overwrite = TRUE)
plot(plans)
freq(plans)
# 70: 7909
# 81: 13128
# 82: 2042

# restoration potential------

## restoration opportunities-------
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
writeRaster(habpotential, 'GIS/scenario_inputs/restoration_potential.tif',
            overwrite = TRUE)

freq(habpotential)
# 7100: 88246
# 7600: 10024
# 8000: 258486

plot(habpotential)
# Note: coding "valley foothill riparian" as POFR, but should be ~22% QULO, 21%
# POFR, 12% SALIX, 7% MIXEDFOREST; also, SALIXSHRUB_2000 left out of main models
# due to CV-wide correlation with total RIPARIAN_2000, but Delta baseline is
# ~25% SALIXSHRUB [was SALIXSHRUB_50 important to any individual spp?]

## zoning-----------
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
writeRaster(zoning, 'GIS/scenario_inputs/zoning.tif',
            overwrite = TRUE)
# 10 = open space/recreation; 20 = public/quasi=public; 90 = designated for development

freq(zoning)
# 10: 318995
# 20:  36717
# 90: 832965

## priority areas---------
# further prioritize restoration in the Delta Stewardship Council's "priority
# restoration areas"
priority = st_read('GIS/original_source_data/ER_P3/ER_P3.shp') %>%
  mutate(code = 100) %>%
  vect() %>%
  rasterize(template, field = 'code')
writeRaster(priority, 'GIS/scenario_inputs/restoration_priority_areas.tif',
            overwrite = TRUE)
# priority = 100

freq(priority)
# 100: 994649

## protected areas-------------
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
writeRaster(protected, 'GIS/scenario_inputs/protected_areas.tif',
            overwrite = TRUE)
#  1 = protected; 2 = easement

freq(protected)
# 1: 376136
# 2: 157654
