# README---------
# Produce interactive map to allow exploration of baseline vs. scenario maps

# PACKAGES & REFERENCE DATA----------
source('R/0_packages.R')
library(leaflet)

delta_poly = read_sf('GIS/boundaries/Legal_Delta_Boundary.shp') %>%
  st_transform(crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')

# SCENARIOS----------
mapstack = c(rast('GIS/landscape_rasters/veg_baseline.tif'),
             rast('GIS/scenario_rasters/scenario1_restoration.tif'),
             rast('GIS/scenario_rasters/scenario2_perennialexpand.tif'),
             rast('GIS/scenario_inputs/restoration_added_ripdetail.tif'),
             rast('GIS/scenario_inputs/perex_added_detail.tif'))
names(mapstack) = c('Baseline', 'Scenario1', 'Scenario2',
                    'Scenario1_Overlay', 'Scenario2_Overlay')

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
                        77, 70),
                      byrow = TRUE, ncol = 2))
freq(mapstack)

# change projection and switch to raster package (can't use terra with leaflet)
scenarios = terra::project(mapstack, y = 'EPSG:3857', method = 'near') %>%
  raster::stack()

# consider resampling rasters to prevent the file size from being too enormous
# (this is still fairly hi-res)
# scenarios_sampled_hi = mapstack %>% spatSample(size = 5000000, as.raster = TRUE)


# LEAFLET PARAMETERS-------

key_simple <- key %>%
  select(LABEL, CODE_BASELINE, CLASS, SUBCLASS, DETAIL, CODE_NAME, COLOR) %>%
  filter(CODE_NAME %in%
           c('ORCHARD_DECIDUOUS', 'VINEYARD', 'GRAIN&HAY', 'FIELD',
             'FIELD_CORN', 'RICE', 'IDLE', 'PASTURE', 'PASTURE_ALFALFA',
             'GRASSLAND', 'URBAN', 'RIPARIAN', 'WETLAND_MANAGED_PERENNIAL',
             'WETLAND_MANAGED_SEASONAL', 'WETLAND_TIDAL', 'WETLAND_OTHER',
             'WATER', 'WOODLAND&SCRUB', 'BARREN')) %>%
  mutate(LABEL = recode(LABEL,
                        'Orchard - Deciduous Fruits & Nuts' = 'Orchard',
                        'Grain & Hay' = 'Grains',
                        'Field Crops' = 'Row & Field Crops')) %>%
  select(code = CODE_BASELINE, color = COLOR, label = LABEL) %>%
  arrange(code)

scales::show_col(key_simple$color)

# color palette
pal <- colorFactor(palette = key_simple$color,
                   domain = key_simple$code,
                   alpha = TRUE,
                   na.color = "transparent")

# LEAFLET MAP-------

# base map plus legend
m <- leaflet(scenarios[[1]]) %>%
  addProviderTiles(provider = "Stamen.TonerLite") %>%
  addLayersControl(position = "bottomleft",
                   options = layersControlOptions(collapsed = F),
                   baseGroups = names(scenarios)) %>%
  addLegend("topright",
            labels = key_simple$label,
            colors = key_simple$color,
            title = 'Land Cover Class',
            opacity = 1)

# add scenario rasters
for (i in c(1:raster::nlayers(scenarios))) {
  m <- m %>% addRasterImage(x = scenarios[[i]],
                            group = names(scenarios)[i],
                            colors = pal,
                            opacity = 0.8,
                            project = FALSE)
}

# add delta boundary
m <- m %>%
  addPolygons(data = delta_poly, fill = FALSE, color = 'black',
              weight = 2, opacity = 1)

# save the interactive map as an html widget
# *Note: you may want to change this to "selfcontained = TRUE", which would
# allow you to email the html file to anyone to view -- the file size just might
# be super large!

htmlwidgets::saveWidget(m,
                        'docs/widget/scenario_map.html',
                        selfcontained = FALSE, libdir = 'lib',
                        title = 'Scenarios of landcover change in the Delta')


# TEST FUNCTION---------
m2 = DeltaMultipleBenefits::map_scenarios(
  rast = mapstack, poly = delta_poly,
  key = key_simple,
  rast_opacity = 0.8, leg_opacity = 1)

