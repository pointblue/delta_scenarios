# README---------
# Produce interactive map to allow exploration of baseline vs. scenario maps

# PACKAGES & FUNCTIONS
source('R/packages.R')

delta_poly = read_sf('GIS/boundaries/Legal_Delta_Boundary.shp') %>%
  st_transform(crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')

key_simple <- key %>%
  select(CODE_BASELINE, CLASS, SUBCLASS, DETAIL, CODE_NAME, COLOR) %>%
  filter(CODE_NAME %in%
           c('ORCHARD_DECIDUOUS', 'VINEYARD', 'GRAIN&HAY', 'FIELD',
             'FIELD_CORN', 'RICE', 'IDLE', 'PASTURE', 'PASTURE_ALFALFA',
             'GRASSLAND', 'URBAN', 'RIPARIAN', 'WETLAND_MANAGED',
             'WETLAND_TIDAL', 'WETLAND_OTHER', 'WATER', 'WOODLAND&SCRUB',
             'BARREN')) %>%
  mutate(label = if_else(!is.na(CLASS), CLASS,
                         if_else(!is.na(SUBCLASS), SUBCLASS, DETAIL)),
         label = capwords(tolower(label)),
         label = recode(label, 'Deciduous Fruits & Nuts' = 'Orchard',
                        'Grain & Hay' = 'Grains',
                        'Field Crops' = 'Row & Field Crops')) %>%
  select(code = CODE_BASELINE, color = COLOR, label) %>%
  arrange(code)


mapstack = c(rast('GIS/landscape_rasters/veg_baseline_fall.tif'),
             rast('GIS/scenario_rasters/scenario1_restoration.tif'),
             rast('GIS/scenario_rasters/scenario2_perennialexpand.tif'),
             rast('GIS/scenario_rasters/scenario3_floodrisk.tif'))
names(mapstack)[1] = 'baseline'

# consolidate land cover classifications for mapping purposes
mapstack = classify(mapstack,
                    rcl = matrix(
                      c(15, 11, #generic orchard (but keep vineyard separate)
                        22, 21, #wheat lumped with grains)
                        28, 26, #row lumped with field
                        71, 70, #combine riparian subclasses
                        72, 70,
                        73, 70,
                        74, 70,
                        75, 70,
                        76, 70,
                        77, 70,
                        81, 89, #combine perennial and seasonal managed wetlands
                        82, 89),
                      byrow = TRUE, ncol = 2))
freq(mapstack)

# MANUALLY-------

# consider resampling rasters to prevent the file size from being too enormous
# (this is still fairly hi-res)
# scenarios_sampled_hi = mapstack %>% spatSample(size = 5000000, as.raster = TRUE)

# change projection and switch to raster package (can't use terra with leaflet)
scenarios = terra::project(
  mapstack,
  y = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext+no_defs",
  method = 'near') %>%
  raster::stack()

# color palette
pal <- leaflet::colorFactor(palette = key_simple$color,
                            domain = key_simple$code,
                            alpha = TRUE,
                            na.color = "transparent")

# base map plus legend
m <- leaflet::leaflet(scenarios[[1]]) %>%
  leaflet::addProviderTiles("Stamen.TonerLite") %>%
  leaflet::addLayersControl(
    position = "bottomleft",
    options = leaflet::layersControlOptions(collapsed = F),
    baseGroups = names(scenarios)) %>%
  leaflet::addLegend("topright",
                     labels = key_simple$label,
                     colors = key_simple$color,
                     title = 'Land Cover Class',
                     opacity = 1)

# add scenario rasters
for (i in c(1:raster::nlayers(scenarios))) {
  m <- m %>% leaflet::addRasterImage(x = scenarios[[i]],
                                     group = names(scenarios)[i],
                                     colors = pal,
                                     opacity = 0.8,
                                     project = FALSE)
}

# add delta boundary
m <- m %>%
  leaflet::addPolygons(data = delta_poly, fill = FALSE, color = 'black',
                       weight = 2, opacity = 1)


# SAVE-------
# save the interactive map as an html widget
# *Note: you may want to change this to "selfcontained = TRUE", which would
# allow you to email the html file to anyone to view -- the file size just might
# be super large!

htmlwidgets::saveWidget(m,
                        'docs/widget/scenario_map.html',
                        selfcontained = FALSE, libdir = 'lib',
                        title = 'Draft scenarios of landcover change in the Delta')


# TEST FUNCTION---------
# # this does take a minute or two and might give error messages - safe to ignore?
# m = DeltaMultipleBenefits::map_scenarios(
#   rast = mapstack,
#   key = wkey %>% select(value = WATERBIRD_CODE, col, label),
#   key_alt = key_simple,
#   rast_opacity = 0.7, leg_opacity = 0.7)
#
# # separately add delta boundary
# delta_poly = read_sf('GIS/boundaries/Legal_Delta_Boundary.shp') %>%
#   st_transform(crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# m <- m %>%
#   leaflet::addPolygons(data = delta_poly, fill = FALSE, color = 'black',
#                        weight = 2, opacity = 1)
#
