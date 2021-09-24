# README---------
# Produce interactive map to allow exploration of baseline vs. scenario maps

# PACKAGES & FUNCTIONS
source('R/packages.R')

mapstack = c(rast('GIS/landscape_rasters/veg_baseline_fall.tif'),
             rast('GIS/scenario_rasters/scenario1_restoration.tif'),
             rast('GIS/scenario_rasters/scenario2_perennialexpand.tif'),
             rast('GIS/scenario_rasters/scenario3_floodrisk_fall.tif'))

wkey = read_csv('data/landcover_key_waterbirds.csv', col_types = cols())

key_simple <- wkey %>% select(label, col) %>% distinct() %>%
  mutate(label = factor(label,
                        levels = c('water', 'tidal marsh', 'managed wetland',
                                   'other wetland', 'riparian', 'corn',
                                   'alfalfa', 'grain', 'field/row', 'rice',
                                   'orchard/vineyard', 'fallow', 'pasture',
                                   'grassland', 'forest/shrub', 'barren',
                                   'urban'))) %>%
  arrange(label)


# this does take a minute or two and might give error messages - safe to ignore?
m = DeltaMultipleBenefits::map_scenarios(
  rast = mapstack,
  key = wkey %>% select(value = WATERBIRD_CODE, col, label),
  key_alt = key_simple,
  rast_opacity = 0.7, leg_opacity = 0.7)

# add delta boundary
delta_poly = read_sf('GIS/boundaries/Legal_Delta_Boundary.shp') %>%
  st_transform(crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
m <- m %>%
  leaflet::addPolygons(data = delta_poly, fill = FALSE, color = 'black',
                       weight = 2, opacity = 1)

## save the interactive map as an html widget
# *Note: you may want to change this to "selfcontained = TRUE", which would
# allow you to email the html file to anyone to view -- the file size just might
# be super large!

htmlwidgets::saveWidget(m,
                        'docs/draft_scenarios.html',
                        selfcontained = FALSE, libdir = 'lib',
                        title = 'Draft scenarios of landcover change in the Delta')

# old:
# %>%
#   project(y = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext+no_defs",
#           method = 'near')
# names(mapstack)[1] = c('baseline')
#
# scenarios = raster::stack(mapstack) #can't use terra for this

# pal <- leaflet::colorFactor(palette = wkey$col, domain = wkey$WATERBIRD_CODE,
#                             alpha = TRUE, na.color = "transparent")
#


# m <- leaflet::leaflet(scenarios[[1]]) %>%
#   leaflet::addProviderTiles("Stamen.TonerLite") %>%
#   leaflet::addLayersControl(
#     position = "bottomleft",
#     options = leaflet::layersControlOptions(collapsed = F),
#     baseGroups = names(scenarios)) %>%
#   leaflet::addLegend("topright",
#                      labels = key_simple$label,
#                      colors = key_simple$col,
#                      title = 'Land Cover Class',
#                      opacity = 1)
#
# for (i in c(1:raster::nlayers(scenarios))) {
#   m <- m %>% leaflet::addRasterImage(x = scenarios[[i]],
#                                      group = names(scenarios)[i], colors = pal,
#                                      opacity = 0.7,
#                                      project = FALSE)
# }

# # consider resampling rasters to prevent the file size from being too enormous (this is
# # still fairly hi-res)
# scenarios_sampled_hi = pred_cv %>%
#   terra::spatSample(size = 5000000, as.raster = TRUE)

# cl <- colors()
# cl[grep('blue', cl)]
# scales::show_col(cl[grep('blue', cl)])
#
# wkey2 = wkey %>%
#   mutate(label = factor(label, levels = c('alfalfa', 'corn', 'rice', 'grain',
#                                           'row/field crop','orchard/vineyard',
#                                           'pasture', 'grassland', 'water',
#                                           'wetland', 'managed wetland',
#                                           'riparian', 'forest/shrub',
#                                           'fallow', 'barren', 'urban')))
# scales::show_col(wkey2$col)


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
