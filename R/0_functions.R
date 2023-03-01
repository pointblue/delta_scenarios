palette = c("#2b83ba", "#80bfab", "#c7e8ad", "#ffffbf", "#fdc980", "#f07c4a",
            "#d7191c")

pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666', '#456d28',
                       #add a few more complementary colors
                       '#b74374', '#5e2a84', '#d2c921')

cleanup_landcovers = function(sf) {
  # when group1 = PASTURE: use Crp2014 to distinguish Alfalfa vs. irrigated pasture
  # when group1 = TRUCK AND FIELD, use Crp2014 to distinguish Corn from others
  # when group1 = GRAIN AND HAY, use Crp2014 to distinguish Wheat from others
  # when group1 = ORCHARD & comment = 'STRINGER' (and Crp2014 is NA), treat as
  #  idle --> VegCAMP layer defaulted to ORCHARD when no data from Land IQ and
  #  otherwise not identified; often includes field edges and road edges
  sf %>%
    mutate(CLASS = case_when(group1 %in% c('ORCHARD', 'IDLE') &
                               comment == 'STRINGER' ~ 'IDLE', #usually field edges/roads
                             group1 == 'ORCHARD' & Crp2014 %in%
                               c('Citrus', 'Olives') ~
                               'ORCHARD_CITRUS&SUBTROPICAL',
                             group1 == 'ORCHARD' & Crp2014 %in%
                               c('Almonds', 'Apples', 'Cherries', 'Kiwis',
                                 'Miscellaneous Deciduous',
                                 'Peaches/Nectarines', 'Peaches and Nectarines',
                                 'Pears', 'Pistachios',
                                 'Plums, Prunes and Apricots', 'Pomegranates',
                                 'Walnuts', 'Young Perennials',
                                 # miscategorized Crp2014 that gets changed to
                                 # "Young Perennials" below:
                                 'Wheat') ~
                               'ORCHARD_DECIDUOUS',
                             group1 == 'VINEYARD' & Crp2014 == 'Kiwis' ~
                               'ORCHARD_CITRUS&SUBTROPICAL',
                             group1 == 'idle' ~ 'IDLE',
                             group1 %in% c('WETLAND', 'WETWATER') &
                               comment == 'SEASONAL' ~
                               'WETLAND_MANAGED_SEASONAL',
                             group1 %in% c('WETLAND', 'WETWATER') &
                               comment %in% c('SEMIPERM', 'SUBSIDENCE REVERSAL') ~
                               'WETLAND_MANAGED_PERENNIAL',
                             group1 %in% c('WETLAND', 'WETWATER') &
                               comment %in% c('CHANNEL', 'UNMANAGED') ~
                               'WETLAND_OTHER',
                             group1 %in% c('WETLAND', 'WETWATER') &
                               comment == 'TIDAL MARSH (FRESH)' ~
                               'WETLAND_TIDAL_FRESH',
                             group1 %in% c('WETLAND', 'WETWATER') &
                               comment == 'TIDAL MARSH (SALT)' ~
                               'WETLAND_TIDAL_SALT',
                             TRUE ~ group1),
           # clean up additional edits: (e.g. River Islands development)
           Crp2014 = case_when(group1 == 'WATER' & !is.na(Crp2014) ~ NA_character_,
                               group1 == 'RICE' & Crp2014 == 'Idle' ~ NA_character_,
                               group1 == 'GRASSLAND' & !is.na(Crp2014) ~ NA_character_,
                               group1 == 'WETLAND' & !is.na(Crp2014) ~ NA_character_,
                               group1 == 'VINEYARD' & Crp2014 == 'Miscellaneous Grain and Hay' ~ NA_character_,
                               group1 == 'URBAN' & !is.na(Crp2014) & Crp2014 != 'Urban' ~ NA_character_,
                               group1 == 'ORCHARD' & Crp2014 == 'Wheat' ~ 'Young Perennials',
                               CLASS == 'IDLE' & !is.na(Crp2014) & Crp2014 != 'Idle' ~ NA_character_,
                               CLASS == 'WOODLAND' & !is.na(Crp2014) ~ NA_character_,
                               TRUE ~ Crp2014))
}

codify_baseline = function(sf, codekey) {
  sf %>%
    mutate(
      CLASS = case_when(
        # pull out more specific crops needed for waterbird models:
        CLASS == 'GRAIN AND HAY' & CROP == 'Wheat' ~ 'GRAIN&HAY_WHEAT',
        CLASS == 'GRAIN AND HAY' ~ 'GRAIN&HAY_OTHER',
        CLASS == 'TRUCK AND FIELD' & CROP == 'Corn, Sorghum and Sudan' ~ 'FIELD_CORN',
        CLASS == 'PASTURE' & CROP == 'Alfalfa and Alfalfa Mixtures' ~ 'PASTURE_ALFALFA',
        CLASS == 'PASTURE' &
          (CROP %in% c('Mixed Pasture', 'Miscellaneous Grasses') | is.na(CROP)) ~ 'PASTURE_OTHER',
        # lump other crops?
        CLASS == 'TRUCK AND FIELD' &
          CROP %in% c('Beans (Dry)', 'Cotton', 'Sunflowers', 'Safflower',
                         'Miscellaneous Field Crops') ~ 'FIELD_OTHER',
        CLASS == 'TRUCK AND FIELD' &
          CROP %in% c(
            'Bush Berries', 'Carrots', 'Cole Crops',
            'Flowers, Nursery and Christmas Tree Farms',
            'Lettuce/Leafy Greens',
            'Melons, Squash and Cucumbers',
            'Miscellaneous Truck',
            'Miscellaneous Truck Crops',
            'Onions and Garlic',
            'Peppers',
            'Potatoes and Sweet Potatoes',
            'Strawberries',
            'Tomatoes') ~ 'ROW',
        # lump other classes:
        CLASS %in% c('WOODLAND', 'OAKWOODLAND') ~ 'WOODLAND',
        CLASS %in% c('WETLAND_TIDAL_FRESH', 'WETLAND_TIDAL_SALT') ~ 'WETLAND_TIDAL',
        # pull out riparian subclasses needed for riparian models
        CLASS == 'RIPARIAN' & NVCS_Nm == 'Populus fremontii' ~ 'RIPARIAN_FOREST_POFR',
        CLASS == 'RIPARIAN' & NVCS_Nm %in%
          c('Quercus lobata', 'Quercus agrifolia', 'Quercus wislizeni (tree)',
            'Californian broadleaf forest and woodland') ~ 'RIPARIAN_FOREST_QULO',
        CLASS == 'RIPARIAN' &
          NVCS_Nm %in% c('Salix gooddingii', 'Salix laevigata') ~ 'RIPARIAN_FOREST_SALIX',
        CLASS == 'RIPARIAN' &
          NVCS_Nm %in% c('Acer negundo',
                         'Aesculus californica',
                         'Fraxinus latifolia',
                         'Juglans hindsii and Hybrids',
                         'Southwestern North American riparian evergreen and deciduous woodland',
                         'Vancouverian riparian deciduous forest',
                         'Alnus rhombifolia',
                         'Platanus racemosa') ~ 'RIPARIAN_FOREST_MIXED',
        CLASS == 'RIPARIAN' &
          NVCS_Nm %in% c('Tamarix spp.',
                         'Phragmites australis - Arundo donax',
                         'Rubus armeniacus',
                         'Rubus armeniacus - Sesbania punicea - Ficus carica',
                         'Sesbania punicea',
                         'Southwestern North American introduced riparian scrub'
                         ) ~ 'RIPARIAN_SCRUB_INTRO',
        CLASS == 'RIPARIAN' &
          NVCS_Nm %in% c('Salix exigua',
                         'Salix lasiolepis',
                         'Salix lucida') ~ 'RIPARIAN_SCRUB_SALIX',
        CLASS == 'RIPARIAN' &
          NVCS_Nm %in% c('Cephalanthus occidentalis',
                         'Cornus sericea',
                         'Rosa californica',
                         'Sambucus nigra',
                         'Southwestern North American riparian/wash scrub',
                         'Vitis californica') ~ 'RIPARIAN_SCRUB_MIXED',
        TRUE ~ CLASS),
      CLASS = factor(
        CLASS,
        levels = c('PERENNIAL_CROPS', 'ORCHARD_DECIDUOUS',
                   'ORCHARD_CITRUS&SUBTROPICAL', 'VINEYARD', 'ANNUAL_CROPS',
                   'GRAIN&HAY', 'GRAIN&HAY_WHEAT', 'GRAIN&HAY_OTHER',
                   'FIELD', 'FIELD_CORN', 'FIELD_OTHER', 'ROW',
                   'RICE', 'IDLE', 'GRASSLAND&PASTURE', 'PASTURE',
                   'PASTURE_ALFALFA', 'PASTURE_OTHER', 'GRASSLAND', 'URBAN',
                   'RIPARIAN', 'RIPARIAN_FOREST', 'RIPARIAN_FOREST_POFR',
                   'RIPARIAN_FOREST_QULO', 'RIPARIAN_FOREST_SALIX',
                   'RIPARIAN_FOREST_MIXED', 'RIPARIAN_SCRUB',
                   'RIPARIAN_SCRUB_INTRO', 'RIPARIAN_SCRUB_SALIX',
                   'RIPARIAN_SCRUB_MIXED', 'WETLAND', 'WETLAND_MANAGED',
                   'WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL',
                   'WETLAND_TIDAL', 'WETLAND_OTHER', 'WATER', 'WOODLAND&SCRUB',
                   'WOODLAND', 'SCRUB', 'BARREN'))) %>%
    left_join(codekey %>% select(CLASS = CODE_NAME, CODE_BASELINE),
              by = 'CLASS')
}

codify_NASS = function(NASSraster, season = 'fall', codekey) {
  nasskey = cats(NASSraster)[[1]] %>% select(VALUE, CLASS_NAME) %>% drop_na()
  activeCat(NASSraster) <- 'CLASS_NAME' #so freq will return classes

  # build classification matrix
  mat = freq(NASSraster) %>% select(label_orig = value) %>%
    mutate(
      CLASS = case_when(
        label_orig %in% c('Corn', 'Sorghum', 'Pop or Orn Corn', 'Sweet Corn') ~
          'FIELD_CORN',
        label_orig == 'Rice' ~ 'RICE',
        label_orig %in%
          c('Spring Wheat', 'Winter Wheat', 'Durum Wheat', 'Triticale') ~
          'GRAIN&HAY_WHEAT',
        label_orig %in% c('Barley', 'Rye', 'Oats', 'Speltz', 'Other Small Grains') ~
          'GRAIN&HAY_OTHER',
        label_orig %in%
          c('Cotton', 'Sunflower', 'Safflower', 'Dry Beans', 'Vetch', 'Millet',
            'Canola', 'Flaxseed', 'Rape Seed', 'Mustard', 'Camelina', 'Buckwheat',
            'Other Crops') ~ 'FIELD_OTHER',
        label_orig %in%
          c('Mint', 'Sugarbeets', 'Potatoes', 'Misc Vegs & Fruits', 'Watermelons',
            'Onions', 'Cucumbers', 'Peas', 'Tomatoes', 'Herbs', 'Carrots',
            'Garlic', 'Cantaloupes', 'Honeydew Melons', 'Peppers', 'Greens',
            'Strawberries', 'Squash', 'Pumpkins', 'Blueberries', 'Soybeans',
            'Peanuts', 'Tobacco', 'Cucumbers', 'Chick Peas', 'Lentils',
            'Caneberries', 'Hops', 'Christmas Trees', 'Asparagus', 'Broccoli',
            'Lettuce', 'Cabbage', 'Cauliflower', 'Celery', 'Radishes', 'Turnips',
            'Eggplants', 'Gourds', 'Cranberries', 'Sweet Potatoes') ~
          'ROW',
        label_orig == 'Alfalfa' ~ 'PASTURE_ALFALFA',
        label_orig %in%
          c('Other Hay/Non Alfalfa', 'Clover/Wildflowers', 'Sod/Grass Seed',
            'Switchgrass') ~
          'PASTURE_OTHER',
        label_orig == 'Fallow/Idle Cropland' ~ 'IDLE',
        label_orig %in% c('Cherries', 'Peaches', 'Apples', 'Pecans', 'Almonds',
                          'Walnuts', 'Pears', 'Pistachios', 'Pomegranates',
                          'Plums', 'Prunes', 'Nectarines', 'Apricots',
                          'Other Tree Crops') ~
          'ORCHARD_DECIDUOUS',
        label_orig %in% c('Citrus', 'Olives', 'Oranges', 'Avocados') ~
          'ORCHARD_CITRUS&SUBTROPICAL',
        label_orig == 'Grapes' ~ 'VINEYARD',
        label_orig %in% c('Water', 'Open Water', 'Aquaculture') ~ 'WATER',
        label_orig %in%
          c('Developed', 'Developed/Open Space', 'Developed/Low Intensity',
            'Developed/Med Intensity', 'Developed/High Intensity') ~ 'URBAN',
        label_orig %in% c('Barren', 'Perennial Ice/Snow') ~ 'BARREN',
        label_orig %in%
          c('Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 'Shrubland',
            'Forest') ~ 'WOODLAND',
        label_orig == 'Shrubland' ~ 'SCRUB',
        label_orig == 'Grassland/Pasture' ~ 'GRASSLAND',
        label_orig == 'Woody Wetlands' ~ 'RIPARIAN',
        label_orig %in% c('Wetlands', 'Herbaceous Wetlands') ~ 'WETLAND',
        season == 'fall' & label_orig %in%
          c('Dbl Crop WinWht/Corn', 'Dbl Crop Oats/Corn',
            'Dbl Crop Triticale/Corn',
            'Dbl Crop WinWht/Sorghum', 'Dbl Crop Barley/Corn',
            'Dbl Crop Corn/Soybeans', 'Dbl Crop Durum Wht/Sorghum') ~ 'FIELD_CORN',
        season == 'fall' & label_orig %in%
          c('Dbl Crop WinWht/Soybeans', 'Dbl Crop Barley/Soybeans',
            'Dbl Crop Lettuce/Durum Wht', 'Dbl Crop Soybeans/Oats',
            'Dbl Crop Lettuce/Barley') ~ 'ROW',
        season == 'fall' & label_orig %in%
          c('Dbl Crop WinWht/Cotton') ~ 'FIELD_OTHER',
        season == 'winter' & label_orig %in%
          c('Dbl Crop WinWht/Corn', 'Dbl Crop WinWht/Sorghum',
            'Dbl Crop WinWht/Soybeans', 'Dbl Crop Triticale/Corn',
            'Dbl Crop Lettuce/Durum Wht', 'Dbl Crop Durum Wht/Sorghum',
            'Dbl Crop WinWht/Cotton') ~ 'GRAIN&HAY_WHEAT',
        season == 'winter' & label_orig %in%
          c('Dbl Crop Oats/Corn', 'Dbl Crop Barley/Corn',
            'Dbl Crop Barley/Soybeans', 'Dbl Crop Soybeans/Oats',
            'Dbl Crop Lettuce/Barley') ~ 'GRAIN&HAY_OTHER',
        season == 'fall' & label_orig %in%
          c('Dbl Corn/Soybeans') ~ 'ROW',
        label_orig %in% c('Background', 'Clouds/No Data', 'Nonag/Undefined') ~ NA_character_)) %>%
    left_join(nasskey, by = c('label_orig' = 'CLASS_NAME')) %>%
    left_join(codekey %>% select(CLASS = CODE_NAME, CODE_BASELINE),
            by = 'CLASS')

  terra::classify(NASSraster,
                  rcl = mat %>% select(from = VALUE, to = CODE_BASELINE) %>%
                    as.matrix(),
                  others = NA)
}

codify_waterbird = function(df) {
  df %>%
    mutate(
      WATERBIRD_CLASS = case_when(
        CLASS %in% c('WATER', 'RICE', 'BARREN', 'CORN', 'WHEAT', 'ALFALFA') ~ tolower(group1),
        CLASS %in% c('ORCHARD', 'VINEYARD') ~ 'orchard',
        CLASS %in% c('WETLAND_SEASONAL', 'WETLAND_PERENNIAL') ~ 'DU wetland',
        CLASS == 'WETLAND_OTHER' ~ 'wetland',
        CLASS == 'RIPARIAN' ~ 'woody wetland',
        CLASS == 'URBAN' ~ 'developed',
        CLASS == 'GRASSLAND' ~ 'Dryland pasture',
        CLASS == 'PASTURE' & Crp2014 == 'Alfalfa and Alfalfa Mixtures' ~
          'ALFALFA',
        CLASS == 'PASTURE' ~ 'Irrigated pasture',
        CLASS == 'TRUCK AND FIELD' & Crp2014 == 'Corn, Sorghum and Sudan' ~
          'CORN',
        group1 == 'GRAIN AND HAY' & Crp2014 == 'Wheat' ~ 'WHEAT',
        CLASS == 'GRAIN AND HAY' ~ 'grain',
        CLASS %in% c('WOODLAND', 'OAKWOODLAND', 'SCRUB') ~ 'forest',
        CLASS == 'IDLE' ~ 'fallow',
        CLASS == 'TRUCK AND FIELD' &
          Crp2014 %in% c(
            'Sunflowers',
            'Safflower',
            'Flowers, Nursery and Christmas Tree Farms',
            'Miscellaneous Field Crops') ~
          'field',
        CLASS == 'TRUCK AND FIELD' &
          Crp2014 %in% c(
            'Beans (Dry)',
            'Potatoes and Sweet Potatoes',
            'Tomatoes',
            'Melons, Squash and Cucumbers',
            'Carrots',
            'Cole Crops',
            'Onions and Garlic',
            'Lettuce/Leafy Greens',
            'Peppers',
            'Strawberries',
            'Bush Berries',
            'Cotton',
            'Miscellaneous Truck Crops',
            'Miscellaneous Truck') ~
          'row'),
      WATERBIRD_CLASS = factor(WATERBIRD_CLASS,
                               levels = c('corn', 'rice', 'Irrigated pasture',
                                          'wheat', 'row', 'field', 'wetland',
                                          'orchard', 'grain', 'fallow',
                                          'developed', 'forest', 'water',
                                          'woody wetland', 'Dryland pasture',
                                          'alfalfa', 'DU wetland', 'tidal marsh',
                                          'barren')),
      WATERBIRD_CODE = if_else(WATERBIRD_CLASS == 'barren', 99,
                               as.numeric(WATERBIRD_CLASS) + 1))
}

codify_landiq = function(df, meta, codekey) {
  df = df %>%
    left_join(meta %>%
                select(CROPTYP,
                       CLASS_NAME1 = CLASS_NAME,
                       SUBCLASS_NAME1 = SUBCLASS_NAME),
              by = c('CROPTYP1' = 'CROPTYP')) %>%
    left_join(meta %>%
                select(CROPTYP,
                       CLASS_NAME2 = CLASS_NAME,
                       SUBCLASS_NAME2 = SUBCLASS_NAME),
              by = c('CROPTYP2' = 'CROPTYP')) %>%
    left_join(meta %>%
                select(CROPTYP,
                       CLASS_NAME3 = CLASS_NAME,
                       SUBCLASS_NAME3 = SUBCLASS_NAME),
              by = c('CROPTYP3' = 'CROPTYP')) %>%
    select(SYMB_CLASS, MULTIUSE,
           CROPTYP1, CLASS1, CLASS_NAME1, SUBCLASS_NAME1, ADOY1,
           CROPTYP2, CLASS2, CLASS_NAME2, SUBCLASS_NAME2, ADOY2,
           CROPTYP3, CLASS3, CLASS_NAME3, SUBCLASS_NAME3, ADOY3) %>%

    # - SYMB_CLASS = MAIN SEASON (SUMMER) SYMBOLOGY
    # - MULTIUSE = MULTIPLE LAND USES: S = SINGLE-CROPPED, D = DOUBLE, T =
    #    TRIPLE, Q = QUADRUPLE; I = INTERCROPPED, M = MIXED USE
    # - ALL MAIN SEASON SUMMER CROPS START WITH CLASS2, AND ONLY DISTINCT EARLY
    #    CROPS (MULTIUSE = D OR M) WILL HAVE A CLASS1

    mutate(CLASS_MAIN = case_when(MULTIUSE %in% c('S', 'D', 'T') &
                                    SYMB_CLASS == CLASS2 ~ CLASS_NAME2,
                                  MULTIUSE %in% c('M', 'I') & CLASS1 == '**' &
                                    SYMB_CLASS == CLASS2 ~ CLASS_NAME2,
                                  TRUE ~ 'UNCLEAR'),
           SUBCLASS_MAIN = case_when(MULTIUSE %in% c('S', 'D', 'T') &
                                       SYMB_CLASS == CLASS2 ~ SUBCLASS_NAME2,
                                     MULTIUSE %in% c('M', 'I') & CLASS1 == '**' &
                                       SYMB_CLASS == CLASS2 ~ SUBCLASS_NAME2,
                                     TRUE ~ 'UNCLEAR'),
           CLASS_WINTER = case_when(CROPTYP1 != '****' &
                                      MULTIUSE %in% c('D', 'T') &
                                      SUBCLASS_NAME1 != SUBCLASS_NAME2 ~ CLASS_NAME1,
                                    TRUE ~ NA_character_),
           SUBCLASS_WINTER = case_when(CROPTYP1 != '****' &
                                         MULTIUSE %in% c('D', 'T') &
                                         SUBCLASS_NAME1 != SUBCLASS_NAME2 ~ SUBCLASS_NAME1,
                                       TRUE ~ NA_character_),
           CLASS_FALL = case_when(CROPTYP3 != '****' &
                                    MULTIUSE %in% c('D', 'T') &
                                    SUBCLASS_NAME3 != SUBCLASS_NAME2 ~ CLASS_NAME3,
                                  TRUE ~ NA_character_),
           SUBCLASS_FALL = case_when(CROPTYP3 != '****' &
                                       MULTIUSE %in% c('D', 'T') &
                                       SUBCLASS_NAME3 != SUBCLASS_NAME2 ~ SUBCLASS_NAME3,
                                     TRUE ~ NA_character_)) %>%
    mutate(
      CLASS = case_when(
        CLASS_MAIN %in% c('DECIDUOUS FRUITS AND NUTS', 'YOUNG PERENNIAL') ~
          'ORCHARD_DECIDUOUS',
        CLASS_MAIN == 'CITRUS AND SUBTROPICAL' ~ 'ORCHARD_CITRUS&SUBTROPICAL',
        CLASS_MAIN == 'VINEYARDS' ~ 'VINEYARD',
        CLASS_MAIN == 'GRAIN AND HAY' & SUBCLASS_MAIN == 'WHEAT' ~ 'GRAIN&HAY_WHEAT',
        CLASS_MAIN == 'GRAIN AND HAY' ~ 'GRAIN&HAY_OTHER', #ALL OTHER GRAIN&HAY
        CLASS_MAIN == 'FIELD CROPS' & SUBCLASS_MAIN == 'CORN, SORGHUM, OR SUDAN' ~
          'FIELD_CORN',
        CLASS_MAIN == 'FIELD CROPS' ~ 'FIELD_OTHER', #ALL OTHER FIELD CROPS
        CLASS_MAIN == 'TRUCK, NURSERY, AND BERRY CROPS' ~ 'ROW',
        CLASS_MAIN == 'RICE' ~ 'RICE',
        CLASS_MAIN == 'UNCLASSIFIED FALLOW - STATUS NOT IDENTIFIED' ~ 'IDLE',
        CLASS_MAIN == 'PASTURE' & SUBCLASS_MAIN == 'ALFALFA & ALFALFA MIXTURES' ~
          'PASTURE_ALFALFA',
        CLASS_MAIN == 'PASTURE' ~ 'PASTURE_OTHER', #ALL OTHER PASTURE
        CLASS_MAIN == 'URBAN - RESIDENTIAL, COMMERCIAL, AND INDUSTRIAL, UNSEGREGATED' ~
          'URBAN',
        TRUE ~ 'UNCLEAR'),
      CLASS_WINTER = case_when(
        CLASS_WINTER == 'GRAIN AND HAY' & SUBCLASS_WINTER == 'WHEAT' ~ 'GRAIN&HAY_WHEAT',
        CLASS_WINTER == 'GRAIN AND HAY' ~ 'GRAIN&HAY_OTHER', #ALL OTHER GRAIN&HAY
        CLASS_WINTER == 'FIELD CROPS' ~ 'FIELD_OTHER',
        CLASS_WINTER == 'TRUCK, NURSERY, AND BERRY CROPS' ~ 'ROW',
        CLASS_WINTER == 'PASTURE' &
          SUBCLASS_WINTER == 'ALFALFA & ALFALFA MIXTURES' ~ 'PASTURE_ALFALFA',
        CLASS_WINTER == 'PASTURE' ~ 'PASTURE_OTHER', #ALL OTHER PASTURE
        TRUE ~ CLASS)
      ) %>%
    left_join(codekey %>%
                select(CLASS = CODE_NAME, CODE_BASELINE),
              by = 'CLASS')  %>%
    left_join(codekey %>%
                select(CLASS_WINTER = CODE_NAME,
                       CODE_BASELINE_WINTER = CODE_BASELINE),
              by = 'CLASS_WINTER')
}

codify_ripdetail = function(df) {
  df %>% filter(WATERBIRD_CLASS == 'woody wetland') %>%
    mutate(RIPDETAIL = case_when(NVCS_Nm == 'Populus fremontii' ~ 'POFR',
                                 NVCS_Nm == 'Quercus lobata' ~ 'QULO',
                                 NVCS_Nm %in% c('Salix gooddingii',
                                                'Salix laevigata') ~ 'SALIX',
                                 NVCS_Nm %in% c(
                                   'Acer negundo',
                                   'Fraxinus latifolia',
                                   'Juglans hindsii and Hybrids',
                                   'Southwestern North American riparian evergreen and deciduous woodland',
                                   'Vancouverian riparian deciduous forest',
                                   'Alnus rhombifolia',
                                   'Platanus racemosa'
                                 ) ~ 'MIXEDFOREST',
                                 NVCS_Nm %in% c(
                                   'Tamarix spp.',
                                   'Phragmites australis - Arundo donax',
                                   'Rubus armeniacus',
                                   'Rubus armeniacus - Sesbania punicea - Ficus carica',
                                   'Sesbania punicea',
                                   'Southwestern North American introduced riparian scrub'
                                 ) ~ 'INTROSCRUB',
                                 NVCS_Nm %in% c(
                                   'Salix exigua',
                                   'Salix lasiolepis',
                                   'Salix lucida'
                                 ) ~ 'SALIXSHRUB',
                                 NVCS_Nm %in% c(
                                   'Cephalanthus occidentalis',
                                   'Cornus sericea',
                                   'Rosa californica',
                                   'Sambucus nigra',
                                   'Southwestern North American riparian/wash scrub',
                                   'Vitis californica'
                                 ) ~ 'MIXEDSHRUB',
                                 WATERBIRD_CLASS == 'riparian' ~ 'OTHER'),
           RIPDETAIL_CODE = case_when(RIPDETAIL == 'POFR' ~ 71,
                                      RIPDETAIL == 'QULO' ~ 72,
                                      RIPDETAIL == 'SALIX' ~ 73,
                                      RIPDETAIL == 'MIXEDFOREST' ~ 74,
                                      RIPDETAIL == 'INTROSCRUB' ~ 75,
                                      RIPDETAIL == 'SALIXSHRUB' ~ 76,
                                      RIPDETAIL == 'MIXEDSHRUB' ~ 77,
                                      RIPDETAIL == 'OTHER' ~ 78)) %>%
    filter(!is.na(RIPDETAIL_CODE))
}

codify_agstats <- function(df) {
  df %>%
    mutate(
      crop = as.character(crop),
      CODE_NAME = case_when(
        grepl('almond|apple|apricot|cherr|fruits & nuts|nectarines|orchard',
              crop, ignore.case = TRUE) ~ 'ORCHARD_DECIDUOUS',
        grepl('orchard|peaches|pears|plums|walnuts|pistachios',
              crop, ignore.case = TRUE) ~ 'ORCHARD_DECIDUOUS',
        grepl('olive|carob', crop, ignore.case = TRUE) ~ 'ORCHARD_CITRUS&SUBTROPICAL',
        grepl('grape', crop, ignore.case = TRUE) ~ 'VINEYARD',
        grepl('corn|sorghum|sudan', crop, ignore.case = TRUE) ~ 'FIELD_CORN',
        grepl('alfalfa', crop, ignore.case = TRUE) ~ 'PASTURE_ALFALFA',
        grepl('rice', crop, ignore.case = TRUE) ~ 'RICE',
        grepl('wheat', crop, ignore.case = TRUE) ~ 'GRAIN&HAY_WHEAT',
        grepl('hay other|oat|grain|triticale|silage',
              crop, ignore.case = TRUE) ~ 'GRAIN&HAY_OTHER', #not wheat
        grepl('pasture irrigated|ryegrass|seed grass', crop, ignore.case = TRUE) ~ 'PASTURE_OTHER', #not alfalfa
        grepl('pasture range|hay wild', crop, ignore.case = TRUE) ~ 'GRASSLAND',
        grepl('safflower|sunflower|beans dry|field crops',
              crop, ignore.case = TRUE) ~ 'FIELD_OTHER', #not corn
        grepl('asparagus|beans|berries|carrot|cucumber|garlic|melon|onion|pepper',
              crop, ignore.case = TRUE) ~ 'ROW',
        grepl('potato|pumpkin|squash|tomato|vegetable|seed',
              crop, ignore.case = TRUE) ~ 'ROW',
        TRUE ~ 'UNKNOWN'),
      SUBCLASS = case_when(
        grepl('ALMOND', crop) ~ 'ALMONDS',
        crop == 'APPLES ALL' ~ 'APPLES',
        grepl('PEAR', crop) ~ 'PEARS',
        grepl('APRICOTS|NECTARINES|PEACHES|PLUMS|CHERRIES', crop) ~ 'STONE FRUIT',
        crop == 'WALNUTS ENGLISH' ~ 'WALNUTS',
        CODE_NAME == 'ORCHARD_DECIDUOUS' ~ 'MISCELLANEOUS DECIDUOUS',
        crop == 'OLIVES' ~ 'OLIVES',
        crop == 'CAROBS' ~ 'CAROBS',
        CODE_NAME == 'PASTURE_ALFALFA' ~ 'ALFALFA & ALFALFA MIXTURES',
        CODE_NAME == 'FIELD_CORN' ~ 'CORN, SORGHUM, OR SUDAN',
        CODE_NAME == 'GRAIN&HAY_WHEAT' ~ 'WHEAT',
        CODE_NAME == 'GRAIN&HAY_OTHER' ~ 'MISCELLANEOUS GRAIN AND HAY',
        grepl('SUNFLOWER', crop) ~ 'SUNFLOWERS',
        grepl('BEANS DRY', crop) ~ 'BEANS (DRY)',
        crop == 'SAFFLOWER' ~ 'SAFFLOWER',
        CODE_NAME == 'FIELD_OTHER' ~ 'MISCELLANEOUS FIELD',
        CODE_NAME == 'RICE' ~ 'RICE (ALL TYPES)',
        CODE_NAME %in% c('VINEYARD', 'GRASSLAND') ~ CODE_NAME,
        crop == 'PASTURE IRRIGATED' ~ 'MIXED PASTURE',
        crop %in% c('RYEGRASS PERENNIAL ALL', 'SEED GRASS UNSPECIFIED') ~ 'MISCELLANEOUS GRASSES',
        grepl('BLUEBERRIES', crop) ~ 'BUSH BERRIES',
        grepl('CARROTS', crop) ~ 'CARROTS',
        grepl('CUCUMBER|MELON|PUMPKIN|SQUASH', crop) ~
          'MELONS, SQUASH, AND CUCUMBERS (ALL TYPES)',
        grepl('GARLIC|ONION', crop) ~ 'ONIONS & GARLIC',
        grepl('PEPPERS', crop) ~ 'PEPPERS (CHILI, BELL, ETC)',
        grepl('POTATO', crop) ~ 'POTATO OR SWEET POTATO',
        grepl('STRAWBERRIES', crop) ~ 'STRAWBERRIES',
        crop == 'TOMATOES PROCESSING' ~ 'TOMATOES (PROCESSING)',
        grepl('ASPARAGUS|BEANS|TOMATO|VEGETABLE|SEED', crop) ~ 'MISCELLANEOUS TRUCK'
      ))
}

codify_edd <- function(df) {
  df %>%
    mutate(
      CODE_NAME = case_when(
        NAICS_code == 111140 ~ 'GRAIN&HAY_WHEAT', #wheat
        NAICS_code == 111150 ~ 'FIELD_CORN', #corn
        NAICS_code == 111160 ~ 'RICE', #rice
        NAICS_code == 111120 ~ 'FIELD_OTHER', #oilseed (SUNFLOWER, SAFFLOWER OIL SEEDS)
        NAICS_code == 111940 ~ 'PASTURE', #hay farming (alfalfa, clover, grass, hay, mixed hay)
        NAICS_code %in% c(111191, 111199) ~ 'GRAIN&HAY_OTHER', #oilseed and grain combo; all other grain farming
        NAICS_code %in% c(111331, 111335, 111336, 111339) ~ 'ORCHARD_DECIDUOUS', #apple, tree nut, fruit/treenut combo, and "other noncitrus fruit"
        NAICS_code == 111332 ~ 'VINEYARD',
        NAICS_code == 111219 ~ 'ROW', #other vegetable and melon
        NAICS_code == 111998 ~ 'OTHER' # all other misc
      ),
      SUBCLASS = case_when(
        NAICS_code == 111331 ~ 'APPLES',
        NAICS_code == 111335 ~ 'TREE NUTS',
        NAICS_code == 111336 ~ 'ORCHARD_DECIDUOUS', #no subclass really applies for fruit/tree combo
        NAICS_code == 111339 ~ 'MISCELLANEOUS DECIDUOUS', #"other noncitrus fruit"
        NAICS_code == 111120 ~ 'OILSEEDS',
        NAICS_code == 111150 ~ 'CORN, SORGHUM, OR SUDAN',
        NAICS_code == 111160 ~ 'RICE (ALL TYPES)',
        NAICS_code == 111140 ~ 'WHEAT',
        NAICS_code %in% c(111191, 111199) ~ 'MISCELLANEOUS GRAIN AND HAY',
        NAICS_code == 111940 ~ 'PASTURE (ALL TYPES)',
        NAICS_code == 111219 ~ 'ROW (ALL TYPES)',
        TRUE ~ CODE_NAME
      ))
}

codify_pur = function(df) {
  df %>%
    mutate(
      CODE_NAME = case_when(
        site_name %in% c('ALMOND', 'APPLE', 'APRICOT', 'CHERRY', 'FIG', 'NECTARINE',
                         'PEACH', 'PEAR', 'PLUM', 'POMEGRANATE', 'POME FRUIT',
                         'STONE FRUIT', 'WALNUT') ~
          'ORCHARD_DECIDUOUS',
        site_name %in% c('CITRUS', 'OLIVE') ~
          'ORCHARD_CITRUS&SUBTROPICAL',
        site_name %in% c('GRAPE', 'GRAPE, WINE') ~
          'VINEYARD',
        site_name %in% c('RICE', 'RICE, WILD') ~
          'RICE',
        site_name %in% c('CORN (FORAGE - FODDER)', 'CORN, GRAIN',
                         'CORN, HUMAN CONSUMPTION', 'SORGHUM/MILO',
                         'SUDANGRASS') ~
          'FIELD_CORN',
        site_name %in% c('SAFFLOWER', 'SUNFLOWER', 'BEAN, DRIED') ~
          'FIELD_OTHER',
        site_name %in% c('WHEAT', 'WHEAT (FORAGE - FODDER)') ~
          'GRAIN&HAY_WHEAT',
        site_name %in% c('BARLEY', 'OAT', 'OAT (FORAGE - FODDER)', 'RYE',
                         'TRITICALE', 'FORAGE HAY/SILAGE') ~
          'GRAIN&HAY_OTHER',
        site_name %in% c('BASIL, SWEET', 'BEAN, SUCCULENT',
                         'BEAN, UNSPECIFIED', 'BOK CHOY', 'CABBAGE', 'CARROT',
                         'CHINESE CABBAGE (NAPPA)', 'CILANTRO', 'COLLARD',
                         'CUCUMBER', 'DAIKON', 'DANDELION GREEN', 'DILL',
                         'ENDIVE (ESCAROLE)', 'FENNEL', 'GARBANZO BEAN',
                         'GARLIC', 'KALE', 'KOHLRABI', 'LETTUCE, LEAF', 'MUSTARD',
                         'MUSTARD GREENS', 'ONION, DRY', 'PARSLEY',
                         'PEPPER, FRUITING', 'POTATO', 'PUMPKIN', 'RADICCHIO',
                         'SMALL FRUITS/BERRY', 'SPINACH', 'SQUASH', 'STRAWBERRY',
                         'SWISS CHARD', 'TOMATO', 'TOMATO, PROCESSING', 'TURNIP',
                         'VEGETABLE', 'WATERMELON') ~
          'ROW',
        site_name == 'ALFALFA' ~
          'PASTURE_ALFALFA',
        site_name %in% c('PASTURELAND', 'RYEGRASS', 'TURF/SOD', 'ORCHARDGRASS') ~
          'PASTURE_OTHER',
        site_name %in% c('RANGELAND') ~
          'GRASSLAND',
        site_name == 'UNCULTIVATED AG' ~
          'IDLE',
        site_name %in% c('UNCULTIVATED NON-AG', 'LANDSCAPE MAINTENANCE') ~ 'URBAN',
        site_name == 'WATER AREA' ~ 'WATER',
        TRUE ~ 'UNKNOWN'),
      SUBCLASS = case_when(
        site_name %in%
          c('ALMOND', 'APPLE', 'PEAR', 'WALNUT', 'POMEGRANATE',
            'SUNFLOWER', 'CARROT', 'OLIVE') ~
          paste0(site_name, 'S'),
        site_name %in% c('CHERRY', 'PEACH', 'NECTARINE', 'PLUM',
                         'APRICOT', 'STONE FRUIT') ~ 'STONE FRUIT',
        site_name %in% c('FIG', 'POME FRUIT') ~ 'MISCELLANEOUS DECIDUOUS',
        CODE_NAME == 'ORCHARD_CITRUS&SUBTROPICAL' &
          (is.na(site_name) | site_name == 'CITRUS') ~
          'MISCELLANEOUS SUBTROPICAL FRUIT',
        site_name == 'BEAN, DRIED' ~ 'BEANS (DRY)',
        CODE_NAME == 'FIELD_CORN' ~ 'CORN, SORGHUM, OR SUDAN',
        site_name == 'SMALL FRUITS/BERRY' ~ 'BUSH BERRIES',
        site_name %in%
          c('BOK CHOY', 'CABBAGE', 'CHINESE CABBAGE (NAPPA)',
            'COLLARD', 'DAIKON', 'KALE', 'KOHLRABI', 'MUSTARD',
            'MUSTARD GREENS', 'TURNIP') ~
          'COLE CROPS (MIXTURE OF 22-25)',
        site_name %in%
          c('CUCUMBER', 'PUMPKIN', 'SQUASH', 'WATERMELON') ~
          'MELONS, SQUASH, AND CUCUMBERS (ALL TYPES)',
        site_name %in%
          c('GARLIC', 'ONION, DRY') ~
          'ONIONS & GARLIC',
        site_name %in% c('PEPPER, FRUITING') ~
          'PEPPERS (CHILI, BELL, ETC)',
        site_name %in% c('POTATO') ~ 'POTATO OR SWEET POTATO',
        site_name == 'STRAWBERRY' ~ 'STRAWBERRIES',
        site_name == 'TOMATO, PROCESSING' ~ 'TOMATOES (PROCESSING)',
        site_name %in% c('WHEAT', 'WHEAT (FORAGE - FODDER)') ~
          'WHEAT',
        site_name %in%
          c('ORCHARDGRASS', 'RYEGRASS', 'TURF/SOD') ~
          'MISCELLANEOUS GRASSES',
        site_name == 'RICE, WILD' ~ 'WILD RICE',
        site_name == 'ALFALFA' ~ 'ALFALFA & ALFALFA MIXTURES',
        # EVERYTHING ELSE:
        site_name == 'SAFFLOWER' ~ site_name,
        CODE_NAME == 'ROW' ~ 'MISCELLANEOUS TRUCK',
        CODE_NAME == 'GRAIN&HAY_OTHER' ~ 'MISCELLANEOUS GRAIN AND HAY',
        CODE_NAME == 'PASTURE_OTHER' ~ 'MIXED PASTURE',
        CODE_NAME %in%
          c('GRASSLAND', 'IDLE', 'RICE', 'VINEYARD', 'URBAN', 'WATER') ~
          CODE_NAME))
}

codify_ccv = function(df) {
  df %>%
    mutate(CODE_NAME = case_when(
      LANDCOVER == 'orchard' ~ 'ORCHARD_DECIDUOUS',
      LANDCOVER %in% c('citrus', 'olives') ~ 'ORCHARD_CITRUS&SUBTROPICAL',
      LANDCOVER == 'vineyard' ~ 'VINEYARD',
      LANDCOVER %in% c('cereal', 'grain') ~ 'GRAIN&HAY_OTHER',
      LANDCOVER %in% c('cotton','field crops') ~ 'FIELD_OTHER', #is cotton relevant to Delta?
      LANDCOVER %in% c('corn', 'cotn') ~ 'FIELD_CORN', # assume cotn is a typo for corn?
      LANDCOVER %in% c('tomato', 'truck crops') ~ 'ROW',
      LANDCOVER == 'rice' ~ 'RICE',
      LANDCOVER %in% c('pasture', 'irrigated pasture') ~ 'PASTURE_OTHER',
      LANDCOVER == 'alfalfa' ~ 'PASTURE_ALFALFA',
      LANDCOVER %in% c('grass', 'grassland', 'dryland pasture') ~ 'GRASSLAND',
      LANDCOVER == 'riparian' ~ 'RIPARIAN',
      LANDCOVER %in% c('wetl', 'wetland') ~ 'WETLAND_MANAGED'))
}

# capitalize every individual word in a string
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

summarize_vegdat <- function(pathin, buffer, type = 'circle',
                             pathout, ...) {
  ras = rast(list.files(pathin, '.tif', full.names = TRUE))
  # codevec = codevec %>% rlang::set_names(labelvec)
  wt = focalMat(ras, d = buffer, type = type)

  for (i in c(1:nlyr(ras))) {
    focal(ras[[i]], w = wt, fun = 'sum', na.rm = TRUE,
          filename = paste0(pathout, '/', names(ras)[i], '_', buffer, '.tif'),
          ...)
  }
  # purrr::map(codevec,
  #            function(c) {
  #              lab = names(codevec[codevec == c])
  #              r = ras == c
  #              focal(r, w = wt, fun = 'sum', na.rm = TRUE,
  #                    filename = paste0(path, '/', lab, '_', buffer, '.tif'),
  #                    ...)
  #            })
  # names(sdat) = paste(labelvec, buffer, sep = '_')
  # stack(sdat)
}

summarize_fragdat <- function(r, target = 70, buffer = 2000,
                              stats = c('lsm_c_shape_mn'),
                              ...) {

  n = round((buffer * 2) / res(r)[1], digits = 0)
  wt = matrix(rep(rep(1, n), n), nrow = n, ncol = n)

  # reclassify raster so anything but target value is NA (assume max value of 999)
  r = reclassify(r,
                 rcl = matrix(c(0, target, NA,
                                target, target + 1, target,
                                target + 1, 1000, NA),
                              ncol = 3, byrow = TRUE),
                 right = FALSE)

  # for each stat, compute values in focal areas (moving windows) throughout r
  sdat = landscapemetrics::window_lsm(r, window = wt,
                                      what = 'lsm_l_shape_mn',
                                      ...)
  stack(sdat)
}

## DONE - MOVED TO R PACKAGE:---------

# create_directory = function(path) {
#   if (!dir.exists(path)) {
#     cat('Creating directory:', path, '\n')
#     dir.create(path, recursive = TRUE)
#   } else {
#     cat('Writing to directory:', path, '\n')
#   }
# }
#
# python_prep = function(landscape, SDM, pathout, scenario_name,
#                        suffix = NULL, mask = NULL, pixel_value = NULL,
#                        overwrite = FALSE) {
#
#   if (!is.null(mask) & is.null(suffix)) {
#     stop('Provide two suffix values to distinguish unmasked and masked results (e.g., _area and _pfld)')
#   }
#
#   # split layer by land cover classes to represent presence/absence
#   layernames = freq(landscape) %>% pull(label)
#   presence = segregate(landscape, other = 0) %>% setNames(layernames)
#
#   # reclassify according to riparian and waterbird model inputs
#   presence_reclass = reclassify_landcover(presence, type = SDM)
#
#   create_directory(file.path(pathout, scenario_name, SDM))
#
#   # optional: if mask is provided (e.g. pfld data), generate layers
#   # reflecting the value of the mask layer wherever each land cover is present
#   # --> expect two values provided for "suffix" to distinguish them (e.g., _area
#   # and _pfld)
#   if (!is.null(mask)) {
#     # where presence_reclass is 0 (land cover not present), change maskpath to
#     # NA (allowing values in mask path to be summarized only for that specific
#     # land cover)
#     newstack_mask = mask(mask, presence_reclass,
#                          maskvalue = 0, updatevalue = NA)
#     names(newstack_mask) = paste0(names(presence_reclass), suffix[2])
#     writeRaster(newstack_mask,
#                 filename = file.path(pathout, scenario_name, SDM,
#                                      paste0(names(newstack_mask), '.tif')),
#                 overwrite = overwrite)
#   }
#
#   # finalize original unmasked values:
#   # optional: replace presence (1) with another value (e.g., pixel area)
#   if (!is.null(pixel_value)) {
#     presence_reclass = subst(presence_reclass, from = 1, to = pixel_value)
#   }
#
#   # optional: add suffix
#   if (!is.null(suffix)) {
#     names(presence_reclass) = paste0(names(presence_reclass), suffix[1])
#   }
#
#   writeRaster(presence_reclass,
#               filename = file.path(pathout, scenario_name, SDM,
#                                    paste0(names(presence_reclass), '.tif')),
#               overwrite = overwrite)
# }

# reclassify_landcover = function(stack, type) {
#   if (type == 'riparian') {
#     c(# sum of all riparian subclasses
#       subset(stack, names(stack)[grepl('RIPARIAN', names(stack))]) %>%
#         sum(na.rm = TRUE) %>% setNames('RIPARIAN'),
#       # sum of all wetland subclasses
#       subset(stack, names(stack)[grepl('WETLAND', names(stack))]) %>%
#         sum(na.rm = TRUE) %>% setNames('WETLAND'),
#       # rename riparian and wetland subclasses
#       subset(stack, c('WETLAND_MANAGED_PERENNIAL',
#                     'RIPARIAN_FOREST_POFR',
#                     'RIPARIAN_FOREST_QULO',
#                     'RIPARIAN_FOREST_SALIX',
#                     'RIPARIAN_FOREST_MIXED',
#                     'RIPARIAN_SCRUB_MIXED',
#                     'RIPARIAN_SCRUB_SALIX',
#                     'RIPARIAN_SCRUB_INTRO')) %>%
#         setNames(c('PERM', 'POFR', 'QULO','SALIX', 'MIXEDFOREST',
#                    'MIXEDSHRUB', 'SALIXSHRUB', 'INTROSCRUB')),
#       # sum of perennial crops, grassland/pasture, and other ag
#       subset(stack, names(stack)[grepl('ORCH|VINE', names(stack))]) %>%
#         sum(na.rm = TRUE) %>% setNames('ORCHVIN'),
#       subset(stack, names(stack)[grepl('PASTURE|GRASS', names(stack))]) %>%
#         sum(na.rm = TRUE) %>% setNames('GRASSPAS'),
#       subset(stack, names(stack)[grepl('FIELD|GRAIN|ROW', names(stack))]) %>%
#         sum(na.rm = TRUE) %>% setNames('AG'),
#       # keep rice, idle, urban, and water as-is
#       subset(stack, c('RICE', 'IDLE', 'URBAN', 'WATER'))
#     )
#
#   } else if (type == 'waterbird_fall') {
#     c(
#       # combine all riparian, perennial crops, managed wetlands,
#       # other wetlands, and woodland/scrub
#       subset(stack, names(stack)[grepl('RIPARIAN', names(stack))]) %>%
#         sum(na.rm = TRUE) %>% setNames('woodw'),
#       subset(stack, names(stack)[grepl('ORCHARD|VINEYARD', names(stack))]) %>%
#         sum(na.rm = TRUE) %>% setNames('orch'),
#       subset(stack, c('WETLAND_TIDAL', 'WETLAND_OTHER')) %>%
#         sum(na.rm = TRUE) %>% setNames('wet'),
#       subset(stack, names(stack)[grepl('WETLAND_MANAGED', names(stack))]) %>%
#         sum(na.rm = TRUE) %>% setNames('duwet'),
#       subset(stack, c('WOODLAND', 'SCRUB')) %>% sum(na.rm = TRUE) %>%
#         setNames('for'),
#       # for fall, combine all grains
#       subset(stack, names(stack)[grepl('GRAIN', names(stack))]) %>%
#         sum(na.rm = TRUE) %>% setNames('grain'),
#       # keep others as-is and rename to match model inputs
#       subset(stack,
#              c('PASTURE_ALFALFA', 'PASTURE_OTHER', 'GRASSLAND',
#                'IDLE', 'RICE', 'FIELD_CORN', 'ROW', 'FIELD_OTHER',
#                'WATER', 'URBAN', 'BARREN')) %>%
#         setNames(c('alf', 'ip', 'dryp', 'fal', 'rice', 'corn',
#                    'row', 'field', 'water', 'dev', 'barren'))
#     )
#   } else if (type == 'waterbird_win') {
#     c(
#       # combine all riparian, perennial crops, managed wetlands,
#       # other wetlands, and woodland/scrub
#       subset(stack, names(stack)[grepl('RIPARIAN', names(stack))]) %>%
#         sum(na.rm = TRUE) %>% setNames('woodw'),
#       subset(stack, names(stack)[grepl('ORCHARD|VINEYARD', names(stack))]) %>%
#         sum(na.rm = TRUE) %>% setNames('orch'),
#       subset(stack, c('WETLAND_TIDAL', 'WETLAND_OTHER')) %>%
#         sum(na.rm = TRUE) %>% setNames('wet'),
#       subset(stack, names(stack)[grepl('WETLAND_MANAGED', names(stack))]) %>%
#         sum(na.rm = TRUE) %>% setNames('duwet'),
#       subset(stack, c('WOODLAND', 'SCRUB')) %>% sum(na.rm = TRUE) %>%
#         setNames('for'),
#       # for winter, keep wheat separate from other grains
#       subset(stack, c('GRAIN&HAY_OTHER', 'GRAIN&HAY_WHEAT')) %>%
#         setNames(c('grain', 'ww')),
#       # keep others as-is and rename to match model inputs
#       subset(stack,
#              c('PASTURE_ALFALFA', 'PASTURE_OTHER', 'GRASSLAND',
#                'IDLE', 'RICE', 'FIELD_CORN', 'ROW', 'FIELD_OTHER',
#                'WATER', 'URBAN', 'BARREN')) %>%
#         setNames(c('alf', 'ip', 'dryp', 'fal', 'rice', 'corn',
#                    'row', 'field', 'water', 'dev', 'barren'))
#     )
#   }
# }
#
# python_run = function(scenario, type = 'waterbird', scale,
#                       pathin = 'GIS/landscape_rasters/cover', regex = NULL,
#                       pathout = 'GIS/landscape_rasters/focal_stats',
#                       fun = 'SUM' # or "MEAN"
#                       ) {
#
#   # create necessary directories
#   create_directory(file.path(pathout, scenario, type, scale))
#
#   # run focal stats
#   focal_stats(
#     pathin = file.path(pathin, scenario, type),
#     pathout = file.path(pathout, scenario, type, scale),
#     buffer = scale, fun = fun, regex = regex)
#
# }
#
# python_dist = function(pathin, scenario_name, copyto = NULL, pathout,
#                        maskpath = NULL, overwrite = FALSE) {
#
#   # calculate distance to roosts and put in same pathin directory
#   dist_stats(filename = 'roosts.tif',
#              fullpathin = file.path(pathin, scenario_name) %>% normalizePath(),
#              fullpathout =  file.path(pathin, scenario_name) %>%
#                normalizePath() %>% paste0('\\droost_km.tif'))
#
#   r = file.path(pathin, scenario_name, 'droost_km.tif') %>% rast()
#
#   if (!is.null(maskpath)) {
#     r = mask(r, rast(maskpath))
#   }
#   create_directory(file.path(pathout[1], scenario_name))
#   writeRaster(r, file.path(pathout[1], scenario_name, 'droost_km.tif'),
#               wopt = list(names = 'droost_km'), overwrite = overwrite)
#
#   if (!is.null(copyto)) {
#     # copy from scenario_name/pathout[1] to copyto/pathout[2]
#     create_directory(file.path(pathout[2], copyto))
#     writeRaster(r, file.path(pathout[2], copyto, 'droost_km.tif'),
#                 wopt = list(names = 'droost_km'), overwrite = overwrite)
#   }
# }
#
# generate_covertype = function(landscape, pathout, type, maskpath=NULL) {
#   create_directory(pathout)
#
#   if (type == 'waterbird_fall') {
#     key = freq(landscape) %>%
#       mutate(
#         covertype = case_when(
#           label == 'RICE' ~ 'Rice',
#           label == 'PASTURE_OTHER' ~ 'Irrigated pasture',
#           label == 'PASTURE_ALFALFA' ~ 'Alfalfa',
#           label %in%
#             c('WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL') ~ 'Wetland',
#           TRUE ~ NA_character_),
#         covertype_code = case_when(
#           covertype == 'Alfalfa' ~ 1,
#           covertype == 'Irrigated pasture' ~ 2,
#           covertype == 'Rice' ~ 3,
#           covertype == 'Wetland' ~ 4)
#         )
#
#   } else if (type == 'waterbird_win') {
#     key = freq(landscape) %>%
#       mutate(
#         covertype = case_when(
#           label == 'PASTURE_ALFALFA' ~ 'Alfalfa',
#           label == 'FIELD_CORN' ~ 'Corn',
#           label == 'PASTURE_OTHER' ~ 'Irrigated pasture',
#           label == 'RICE' ~ 'Rice',
#           label %in%
#             c('WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL') ~ 'Wetland',
#           label == 'GRAIN&HAY_WHEAT' ~ 'Winter wheat',
#           TRUE ~ NA_character_),
#         covertype_code = case_when(
#           covertype == 'Alfalfa' ~ 1,
#           covertype == 'Corn' ~ 2,
#           covertype == 'Irrigated pasture' ~ 3,
#           covertype == 'Rice' ~ 4,
#           covertype == 'Wetland' ~ 5,
#           covertype == 'Winter wheat' ~ 6)
#       )
#   }
#
#   if (!is.null(maskpath)) {
#     landscape = mask(landscape, rast(maskpath))
#   }
#
#   covertype = classify(landscape,
#                        rcl = key %>%
#                          select(from = value, to = covertype_code),
#                        othersNA = TRUE)
#   levels(covertype) = key %>% select(covertype_code, covertype) %>%
#     distinct() %>% drop_na() %>% arrange(covertype_code) %>% as.data.frame()
#   writeRaster(covertype, paste0(pathout, '/covertype.tif'))
# }

# generate_pwater = function(waterdatpath,
#                            baseline_landscape,
#                            scenario_landscape = NULL,
#                            landscape_name,
#                            floor = TRUE,
#                            maskpath, pathout,
#                            overwrite = FALSE) {
#
#   pwater = rast(waterdatpath)
#
#   if (!is.null(scenario_landscape)) {
#     #generate new pwater values for scenario
#     # calculate mean baseline pwater by land cover class (detailed) - including
#     # within the 10km buffer
#     mwater = zonal(pwater, baseline_landscape, fun = mean, na.rm = TRUE) %>%
#       setNames(c('label', 'pwater')) %>% drop_na() %>%
#       left_join(freq(baseline_landscape), by = 'label')
#
#     # assign mean baseline pwater values to changed pixels in each scenario
#     changes = c(scenario_landscape, baseline_landscape) %>% diff() %>%
#       subst(from = 0, to = NA) %>% #no change = NA
#       classify(rcl = matrix(c(-Inf, Inf, 1), nrow = 1)) # all others = 1
#
#     pwater_new = scenario_landscape %>% mask(changes) %>%
#       classify(rcl = mwater %>% select(value, pwater) %>% as.matrix(),
#                othersNA = TRUE)
#
#     if (floor) {
#       # current mean pwater is the "floor"; only allow pwater to increase for
#       # changed pixels (e.g. restoration scenario)
#       pwater_scenario = lapp(c(pwater_new, pwater),
#                              function(x, y) {
#                                ifelse(!is.na(x) & x > y, x, y)
#                              })
#     } else {
#       pwater_scenario = cover(pwater_new, pwater)
#     }
#   } else {
#     # just write final baseline pwater to appropriate directory
#     pwater_scenario = pwater
#   }
#
#   # write unmasked version for focal stats
#   create_directory(file.path(pathout[1], landscape_name))
#   writeRaster(pwater_scenario,
#               file.path(pathout[1], landscape_name, 'pwater.tif'),
#               wopt = list(names = 'pwater'), overwrite = overwrite)
#
#   # write masked version as a predictor
#   pwater_scenario_mask = mask(pwater_scenario, rast(maskpath))
#   create_directory(file.path(pathout[2], landscape_name))
#   writeRaster(pwater_scenario_mask,
#               file.path(pathout[2], landscape_name, 'pwater.tif'),
#               wopt = list(names = 'pwater'), overwrite = overwrite)
#
# }
#
# update_roosts = function(roostpath = 'GIS/original_source_data/Ivey/Select_recent_roosts_Ivey_utm.shp',
#                          landscape, pathout, scenario_name,
#                          overwrite = FALSE) {
#   create_directory(file.path(pathout, scenario_name))
#   # check how much traditional roosts overlap with incompatible land covers:
#   # orchard, vineyard, riparian, woodland, scrub, urban
#   roost_overlay = landscape %>%
#     subst(from = c(11:19, 60, 70:79, 100:120), to = 1) %>%
#     subst(from = c(2:130), to = 0) %>% #everything else
#     terra::extract(vect(roostpath))
#
#   # identify polygons to exclude with >20% incompatible landcover
#   incompatible = roost_overlay %>% setNames(c('ID', 'landscape')) %>%
#     group_by(ID, landscape) %>% count() %>% ungroup() %>%
#     group_by(ID) %>% mutate(ncell = sum(n), prop = n/ncell) %>% ungroup() %>%
#     filter(landscape == 1 & prop > 0.2) %>% arrange(desc(prop))
#
#   read_sf(roostpath) %>%
#     filter(!Roost_ID %in% incompatible$ID) %>%
#     vect() %>% rasterize(., landscape) %>%
#     writeRaster(file.path(pathout, scenario_name, 'roosts.tif'),
#                 wopt = list(names = scenario_name), overwrite = overwrite)
# }
#
# finalize_SDM_predictors = function(pathin, pathout, SDM, scenario_name, scale,
#                                    maskpath = NULL, cover = FALSE,
#                                    overwrite = FALSE) {
#
#   # troubleshooting
#   if (cover & is.null(maskpath)) {
#     stop('cover=TRUE but maskpath not provided')
#   }
#
#   create_directory(file.path(pathout, scenario_name))
#   dat = list.files(file.path(pathin, scenario_name, SDM, scale),
#                    pattern = '.tif$', full.names = TRUE) %>% rast()
#
#   if (!is.null(mask)) {
#     mask = rast(maskpath)
#     dat = mask(dat, mask)
#
#     if (cover) { # fill NAs within mask boundary with zero (e.g. pfld)
#       dat = cover(dat, mask %>% subst(from = 1, to = 0))
#     }
#   }
#
#   if (SDM == 'riparian') {
#
#     names(dat) = paste0(names(dat), '_', scale)
#     # convert to proportion
#     if (scale == '50') {dat = dat/13} else if (scale == '2000') {dat = dat/14073}
#     writeRaster(dat,
#                 paste0(file.path(pathout, scenario_name), '/', names(dat), '.tif'),
#                 overwrite = overwrite)
#
#   } else if (SDM %in% c('waterbird_fall', 'waterbird_win')) { # scale-specific focal stats:
#
#     names(dat) = paste0(names(dat), '_', as.numeric(scale)/1000, 'k')
#     writeRaster(dat,
#                 paste0(file.path(pathout, scenario_name), '/', names(dat), '.tif'),
#                 overwrite = overwrite)
#   }
# }
#
# fit_SDMs = function(modlist, scenario_name, pathin, pathout,
#                     constants = NULL, factors = NULL, overwrite = FALSE,
#                     landscape = NULL, unsuitable = NULL) {
#
#   if (is.null(landscape) & !is.null(unsuitable)) {
#     stop('Landscape provided but unsuitable cover types not specified')
#   }
#   if (!is.null(landscape) & is.null(unsuitable)) {
#     stop('Unsuitable cover types specified but landscape not provided')
#   }
#
#   create_directory(file.path(pathout, scenario_name))
#
#   # scenario-independent predictors (in pathin) and scenario-specific predictors
#   predictors = c(list.files(pathin, pattern = '.tif$', full.names = TRUE),
#                  list.files(file.path(pathin, scenario_name), pattern = '.tif$',
#                             full.names = TRUE)) %>%
#     rast()
#
#   # NOTE: This step is slow:
#   if (is.null(unsuitable)) {
#     #predict and write results raster directly
#     purrr::map(names(modlist),
#                ~terra::predict(
#                  model = modlist[[.x]],
#                  object = subset(predictors,
#                                  modlist[[.x]]$contributions %>%
#                                    filter(!var %in% names(constants)) %>%
#                                    pull(var)),
#                  n.trees = modlist[[.x]]$n.trees,
#                  na.rm = TRUE,
#                  type = 'response',
#                  const = constants,
#                  factors = factors,
#                  filename = paste0(file.path(pathout, scenario_name), '/',
#                                    .x, '.tif'),
#                  overwrite = overwrite,
#                  wopt = list(names = .x)
#                ))
#   } else {
#     # predict results, but fill in unsuitable land covers with zero before
#     # writing raster
#     mask = landscape %>% subst(from = unsuitable, to = 0) %>%
#       subst(c(1:999), NA)
#
#     purrr::map(names(modlist),
#                ~terra::predict(
#                  model = modlist[[.x]],
#                  object = subset(predictors,
#                                  modlist[[.x]]$contributions %>%
#                                    filter(!var %in% names(constants)) %>%
#                                    pull(var)),
#                  n.trees = modlist[[.x]]$n.trees,
#                  na.rm = TRUE,
#                  type = 'response',
#                  const = constants,
#                  factors = factors) %>%
#                  cover(mask,
#                        filename = paste0(file.path(pathout, scenario_name), '/',
#                                          .x, '.tif'),
#                        overwrite = overwrite,
#                        wopt = list(names = .x))
#               )
#   }
#
# }

# # clean up results
# process_focal_stats(
#   pathin = paste0(pathout, scenario, '/buffer', scale, '/raw'),
#   pathout = paste0(pathout, scenario, '/buffer', scale, '/'),
#   suffix = paste0('_', scale), zero_ras = zero_ras, mask = mask,
#   overwrite = overwrite)

# process_focal_stats = function(pathin, pathout, zero_ras = NULL, mask = NULL,
#                                suffix = NULL, overwrite = FALSE) {
#
#   ras = list.files(pathin, '.tif$', full.names = TRUE) %>% rast()
#
#   if (!is.null(zero_ras)) {
#     ras = cover(ras, zero_ras)
#   }
#   if (!is.null(mask)) {
#     ras = mask(ras, mask)
#   }
#   if (!is.null(suffix)) {
#     names(ras) = paste0(names(ras), suffix)
#   }
#
#   writeRaster(ras,
#               filename = paste0(pathout, names(ras), '.tif'),
#               overwrite = overwrite)
# }
#
# transform_SDM = function(pathin, landscape_name, modist, stat, pathout,
#                          overwrite = FALSE) {
#
#   predictions = list.files(file.path(pathin, landscape_name),
#                            pattern = '.tif$', full.names = TRUE) %>%
#     terra::rast()
#
#   threshold_list = purrr::map(names(modlist) %>% setNames(names(modlist)),
#                               function(x) {
#                                 obs = modlist[[x]]$gbm.call$dataframe[modlist[[x]]$gbm.call$gbm.y]
#                                 presence = modlist[[x]]$fitted[obs == 1]
#                                 absence = modlist[[x]]$fitted[obs == 0]
#                                 e = dismo::evaluate(presence, absence)
#                                 dismo::threshold(e, stat)
#                               })
#
#   create_directory(file.path(pathout, landscape_name))
#
#   # reclassify using model-specific thresholds and write to file
#   purrr::map(names(modlist),
#              ~terra::classify(
#                predictions[[.x]],
#                rcl = matrix(c(-Inf, threshold_list[[.x]], 0,
#                               threshold_list[[.x]], Inf, 1),
#                             nrow = 2, byrow = TRUE),
#                filename = paste0(file.path(pathout, landscape_name, .x), '.tif'),
#                overwrite = overwrite,
#                wopt = list(names = .x))
#   )
# }

# transform_SDM = function(modlist, pathin, landscape_name, stat, pathout,
#                          overwrite = FALSE) {
#
#   predictions = list.files(file.path(pathin, landscape_name),
#                            pattern = '.tif$', full.names = TRUE) %>%
#     terra::rast()
#
#   create_directory(file.path(pathout, landscape_name))
#
#   purrr::map(names(modlist),
#              ~find_threshold(mod = modlist[[.x]],
#                              name = .x,
#                              pred = predictions[[.x]],
#                              stat = stat,
#                              pathout = pathout,
#                              landscape_name = landscape_name,
#                              overwrite = overwrite))
# }
#
# find_threshold = function(mod, name, pred, stat, pathout, landscape_name, overwrite) {
#   obs = mod$gbm.call$dataframe[mod$gbm.call$gbm.y]
#   presence = mod$fitted[obs == 1]
#   absence = mod$fitted[obs == 0]
#   e = dismo::evaluate(presence, absence)
#   t = dismo::threshold(e, stat)
#
#   terra::classify(
#     pred,
#     rcl = matrix(c(-Inf, t, 0,
#                    t, Inf, 1), nrow = 2, byrow = TRUE),
#     filename = paste0(file.path(pathout, landscape_name), '/', name, '.tif'),
#     overwrite = overwrite,
#     wopt = list(names = name))
#
# }
#
# sum_habitat = function(pathin, zones = NULL, subtype = 'distributions',
#                        keypath = NULL) {
#   fl = list.files(pathin, '.tif$', recursive = TRUE, full.names = TRUE) %>%
#     set_names()
#
#   if (is.null(zones)) {
#     # sum total
#     res = purrr::map_df(fl,
#                         ~terra::rast(.x) %>% terra::values() %>% sum(na.rm = TRUE) %>%
#                           dplyr::as_tibble(),
#                         .id = 'pathin')
#   } else {
#     # zonal total
#     res = purrr::map_df(fl,
#                         ~terra::rast(.x) %>%
#                           terra::zonal(zones, 'sum', na.rm = TRUE) %>%
#                           setNames(c('ZONE', 'value')),
#                         .id = 'pathin')
#   }
#   res = res %>%
#     mutate(pathin = gsub(!!pathin, '', pathin),
#            pathin = gsub('^\\/|.tif$', '', pathin)) %>%
#     separate(pathin, sep = '/', into = c('SDM', 'scenario', 'spp')) %>%
#     mutate(METRIC_CATEGORY = 'Biodiversity Support',
#            METRIC_SUBTYPE = case_when(
#              SDM == 'riparian' ~ paste('Riparian landbird', subtype),
#              SDM %in% c('waterbird_fall', 'waterbird_win') ~
#                paste('Waterbird', subtype)),
#            SCORE_TOTAL = value / .09, # convert to ha
#            UNIT = 'ha')
#
#   if (!is.null(keypath)) {
#     res = left_join(res, read_csv(keypath, col_types = cols()) %>%
#                       select(spp, METRIC = label), by = 'spp')
#   } else {
#     res = rename(res, METRIC = spp)
#   }
#
#   res %>%
#     mutate(METRIC = case_when(
#       SDM == 'waterbird_fall' ~ paste0(METRIC, ' (fall)'),
#       SDM == 'waterbird_win' ~ paste0(METRIC, ' (winter)'),
#       TRUE ~ METRIC
#     )) %>%
#     select(scenario, any_of('ZONE'), METRIC_CATEGORY, METRIC_SUBTYPE, METRIC,
#            UNIT, SCORE_TOTAL)
# }
#
# sum_landcover = function(pathin, maskpath, pixel_area, zones = NULL) {
#   fl = list.files(pathin, '.tif$', full.names = TRUE) %>% set_names()
#
#   if (is.null(zones)) {
#     if (is.null(maskpath)) {
#       res = purrr::map_df(fl,
#                           ~rast(.x) %>% freq() %>%
#                             mutate(area = count * pixel_area),
#                           .id = 'scenario')
#     } else {
#       res = purrr::map_df(fl,
#                           ~rast(.x) %>% mask(rast(maskpath)) %>% freq() %>%
#                             mutate(area = count * pixel_area),
#                           .id = 'scenario')
#     }
#     res = res %>%
#       mutate(scenario = gsub(!!pathin, '', scenario),
#              scenario = gsub('^\\/|.tif$', '', scenario)) %>%
#       select(scenario, CODE_NAME = label, area)
#
#     # add roll-up of riparian & managed wetland subtypes
#     res = bind_rows(
#       res,
#       res %>% dplyr::filter(grepl('RIPARIAN_|WETLAND_MANAGED', CODE_NAME)) %>%
#         dplyr::mutate(CODE_NAME = dplyr::case_when(
#           grepl('RIPARIAN_', CODE_NAME) ~ 'RIPARIAN',
#           grepl('WETLAND_MANAGED', CODE_NAME) ~ 'WETLAND_MANAGED',
#           TRUE ~ CODE_NAME)) %>%
#         dplyr::group_by(scenario, CODE_NAME) %>%
#         dplyr::summarize(area = sum(area), .groups = 'drop'))
#
#   } else {
#     znames = levels(zones)[[1]]
#     zseg = segregate(zones, other = NA)
#
#     if (is.null(maskpath)) {
#
#       res = purrr::map_df(fl,
#                           ~zonal(zseg, rast(.x), 'sum', na.rm = TRUE) %>%
#                             set_names(c('CODE_NAME', znames)),
#                           .id = 'scenario')
#     } else {
#       res = purrr::map_df(fl,
#                           ~zonal(zseg, mask(rast(.x), rast(maskpath)), 'sum',
#                                  na.rm = TRUE) %>%
#                             set_names(c('CODE_NAME', znames)),
#                           .id = 'scenario')
#     }
#
#     res = res %>%
#       pivot_longer(znames, names_to = 'ZONE', values_to = 'count') %>%
#       mutate(area = count * pixel_area) %>%
#       mutate(scenario = gsub(!!pathin, '', scenario),
#              scenario = gsub('^\\/|.tif$', '', scenario)) %>%
#       select(scenario, ZONE, CODE_NAME, area)
#
#     # add roll-up of riparian & managed wetland subtypes
#     res = bind_rows(
#       res,
#       res %>% dplyr::filter(grepl('RIPARIAN_|WETLAND_MANAGED', CODE_NAME)) %>%
#         dplyr::mutate(CODE_NAME = dplyr::case_when(
#           grepl('RIPARIAN_', CODE_NAME) ~ 'RIPARIAN',
#           grepl('WETLAND_MANAGED', CODE_NAME) ~ 'WETLAND_MANAGED',
#           TRUE ~ CODE_NAME)) %>%
#         dplyr::group_by(scenario, ZONE, CODE_NAME) %>%
#         dplyr::summarize(area = sum(area), .groups = 'drop'))
#   }
#   return(res)
# }
#
# sum_metrics = function(metricdat, areadat) {
#   dat_join = full_join(areadat, metricdat) %>%
#     mutate(
#       # retain mean value for Annual Wages for now, otherwise multiply by area
#       # for total score
#       SCORE_TOTAL = if_else(METRIC == 'Annual Wages',
#                             SCORE_MEAN, area * SCORE_MEAN),
#       # propagate error: multiplication by a constant
#       SCORE_TOTAL_SE = if_else(METRIC == 'Annual Wages',
#                                SCORE_SE, area * SCORE_SE)) %>%
#     replace_na(list(SCORE_TOTAL_SE = 0))
#   # -->the only NA values are for land covers and metrics where value is
#   # presumed zero and scores for climate change resilience to salinity
#
#   bind_rows(
#     # for all but annual wages, sum over all land cover classes:
#     dat_join %>% filter(METRIC != 'Annual Wages') %>%
#       group_by(
#         across(
#           any_of(
#             c('scenario', 'ZONE', 'METRIC_CATEGORY', 'METRIC_SUBTYPE',
#               'METRIC', 'UNIT')))) %>%
#       summarize(SCORE_TOTAL = sum(SCORE_TOTAL),
#                 SCORE_TOTAL_SE = sqrt(sum(SCORE_TOTAL_SE^2)),
#                 .groups = 'drop'),
#     # for annual wages: multiply the average wage per-landcover by the
#     # proportion of the total ag landscape made up by that land cover
#     dat_join %>% filter(METRIC == 'Annual Wages' & SCORE_TOTAL > 0) %>%
#       group_by(
#         across(
#           any_of(
#             c('scenario', 'ZONE', 'METRIC_CATEGORY', 'METRIC_SUBTYPE',
#               'METRIC', 'UNIT')))) %>%
#       mutate(area_ag_total = sum(area),
#              area_prop = area / area_ag_total) %>%
#       summarize(SCORE_TOTAL = sum(SCORE_TOTAL * area_prop),
#                 SCORE_TOTAL_SE = sqrt(sum((SCORE_TOTAL_SE * area_prop)^2)),
#                 .groups = 'drop')
#   )
# }
#
# sum_change = function(scoredat) {
#   left_join(
#     scoredat %>% filter(scenario != 'baseline') %>%
#       rename(SCENARIO = SCORE_TOTAL, SCENARIO_SE = SCORE_TOTAL_SE),
#     scoredat %>% filter(scenario == 'baseline') %>%
#       rename(BASELINE = SCORE_TOTAL, BASELINE_SE = SCORE_TOTAL_SE) %>%
#       select(-scenario)
#     # by = any_of(c('METRIC_CATEGORY', 'ZONE', 'METRIC_SUBTYPE', 'METRIC', 'UNIT'))
#   ) %>%
#     mutate(net_change = SCENARIO - BASELINE,
#            net_change_se = sqrt(SCENARIO_SE^2 + BASELINE_SE^2),
#            change_prop = net_change/BASELINE,
#            change_prop_se = abs(change_prop) * sqrt((net_change_se/net_change)^2 + (BASELINE_SE/BASELINE)^2),
#            change_pct = change_prop * 100,
#            change_pct_se = change_prop_se * 100)
# }

## MOVE TO R PACKAGE:---------

map_predictions <- function(df,
                            palette = c("#2b83ba", "#80bfab", "#c7e8ad",
                                        "#ffffbf", "#fdc980", "#f07c4a",
                                        "#d7191c"),
                            facet = TRUE, ncol = 3, title = NULL,
                            boundary_polygon = NULL,
                            range = c(0, 1),
                            fill_lab = 'Probability\nof presence',
                            ...) {
  p = ggplot(df) +
    geom_raster(aes(x, y, fill = value)) +
    scale_fill_gradientn(colors = palette,
                         na.value = 'transparent',
                         limits = range) +
    labs(x = NULL, y = NULL, fill = fill_lab) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(size = 8, hjust = 0),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          legend.margin = margin(0, 0, 0, 5.5, unit = 'pt'),
          plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
          panel.spacing = unit(3, 'pt'),
          ...)
  if (!is.null(boundary_polygon)) {
    p = p + geom_sf(data = boundary_polygon, fill = NA)
  }
  if (facet) {
    p = p + facet_wrap(~name, ncol = ncol)
  } else {
    p = p + ggtitle(label = title)
  }
  return(p)
}

plot_change_map = function(pathin, SDM, landscape_name, key,
                       studyarea, watermask,
                       palette = c('loss' = 'red',
                                   'maintain' = '#74b743',
                                   'gain' = '#005baa',
                                   'open water' = 'white'),
                       ncol,
                       legend.title = 'Change in\npredicted\npresence',
                       legend.position = 'right',
                       legend.justification = c(1, 0.5),
                       pathout = NULL,
                       ...) {

  df = list.files(file.path(pathin, SDM, landscape_name),
                  '.tif$', full.names = TRUE) %>%
    rast() %>%
    cover(y = watermask) %>%
    crop(vect(studyarea))

  layernames = names(df)

  for (i in c(1:nlyr(df))) { # doesn't work with purrr for some reason
    levels(df)[[i]] = data.frame(ID = c(-1, 0, 1, 90),
                                 label = names(palette))
  }
  names(df) = key$label[match(layernames, key$spp)] #reassign with labels from key
  df = subset(df, subset = key$label[key$label %in% names(df)]) #reorder

  p = ggplot() +
    geom_sf(data = studyarea, fill = 'gray80', color = 'black', size = 0.2) +
    geom_spatraster(data = df) +
    geom_sf(data = studyarea, fill = NA, color = 'black', size = 0.2) +
    facet_wrap(~lyr, ncol = ncol) +
    scale_fill_manual(values = palette, na.value = 'transparent',
                      breaks = names(palette)[1:3]) +
    labs(x = NULL, y = NULL, fill = legend.title) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
          legend.title = element_text(family = 'sourcesans', size = 10),
          legend.text = element_text(family = 'sourcesans', size = 9),
          legend.position = legend.position,
          legend.justification = legend.justification,
          plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
          panel.spacing = unit(0, 'pt'))

  ggsave(filename = pathout, plot = p, ...)
}

plot_change_bar = function(dat, errorbar = TRUE, label = TRUE) {
  p = ggplot(dat, aes(net_change, METRIC)) +
    facet_wrap(~scenario, ncol = 3) +
    # ggforce::facet_col(~METRIC_CATEGORY, scales = 'free', space = 'free') +
    geom_col(aes(fill = bin)) +
    scale_fill_manual(values = pointblue.palette[c(1,3)]) +
    geom_vline(xintercept = 0, color = 'gray30') +
    theme_minimal() +
    theme(axis.line.x = element_line(color = 'gray30'),
          axis.text = element_text(family = 'sourcesans', size = 8.5),
          axis.title = element_text(family = 'sourcesans', size = 10),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(family = 'sourcesans', size = 10),
          strip.placement = 'outside',
          # strip.text = element_text(family = 'sourcesans', size = 10, vjust = 1),
          strip.text = element_blank(),
          strip.background = element_blank(),
          legend.position = 'none',
          panel.spacing = unit(1, 'lines'))

  if (errorbar) {
    p = p +
      geom_errorbar(aes(xmin = lcl,
                        xmax = ucl), width = 0.25)
    if (label) {
      p = p +
        geom_text(data = dat %>% filter(bin == 'benefit'),
                  aes(x = ucl + 3,
                      label = paste0(round(change_pct, digits = 0), '%')), size = 2.5) +
        geom_text(data = dat %>% filter(bin == 'trade-off'),
                  aes(x = lcl - 3,
                      label = paste0(round(change_pct, digits = 0), '%')), size = 2.5)
    }
  }

  print(p)
}

plot_change_lollipop = function(dat, digits, wrapy = FALSE) {

  if (!wrapy) {
    dat = dat %>% mutate(METRIC = gsub('\n', ' ', METRIC))
  }
  dat %>%
    ggplot(aes(net_change, METRIC, fill = bin, color = bin)) +
    facet_wrap(~scenario, ncol = 3) +
    geom_vline(xintercept = 0, color = 'gray30') +
    geom_col(width = 0.25) +
    geom_point(size = 10) +
    geom_text(aes(label = round(net_change, digits = digits)),
              color = 'black', size = 4) +
    scale_fill_manual(values = pointblue.palette[c(1,3)]) +
    scale_color_manual(values = pointblue.palette[c(1,3)]) +
    theme_minimal() +
    theme(axis.line.x = element_line(color = 'gray30'),
          axis.text = element_text(family = 'sourcesans', size = 14, lineheight = 0.8),
          axis.title = element_text(family = 'sourcesans', size = 16),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(family = 'sourcesans', size = 16),
          strip.placement = 'outside',
          strip.text = element_blank(),
          strip.background = element_blank(),
          legend.position = 'none',
          panel.spacing = unit(1, 'lines'))
}
# analyze_metrics = function(metrics_df, baseline, scenario = NULL) {
#   if (!'METRIC' %in% names(metrics_df) | !"SCORE" %in% names(metrics_df)) {
#     stop('metrics_df must contain at least the fields "METRIC" and "SCORE"')
#   }
#   if (class(baseline) == 'SpatRaster') {
#     # calculate the area of each land cover classification
#
#     if (length(levels(baseline)[[1]]) == 1) {
#       # no levels/labels defined
#       warning('No levels defined for baseline raster; using numeric values only.')
#     }
#     area_baseline = calculate_area(baseline, unit = 'ha')
#
#   } else if ('data.frame' %in% class(baseline)) {
#     # assume area already calculated
#     if (!'area' %in% names(baseline) |
#         (!'label' %in% names(baseline) & !'value' %in% names(baseline))) {
#       # check that landcover1 contains the necessary fields
#       stop('baseline must contain fields "area" and either "label" or "value", or provide a SpatRaster')
#     } else {
#       # proceed below
#       area_baseline = baseline
#     }
#   } else {
#     # not a spatraster or a tibble/dataframe
#     stop('baseline must be a SpatRaster (from package terra), or a tibble or data.frame.')
#   }
#
#   metrics_base = full_join(area1, df) %>%  #join by value and/or label fields
#     # calculate total score
#     mutate(SCORE_BASELINE = SCORE * area) %>%
#     select(-area, -SCORE) %>%
#     group_by(across(c(-label, -value, -SCORE_BASELINE))) %>%
#     summarize(SCORE_BASELINE = sum(SCORE_BASELINE, na.rm = TRUE),
#               .groups = 'drop') %>%
#     drop_na()
#
#   if (!is.null(scenario)) {
#     # calculate change as well as total metrics
#     if (class(scenario) == 'SpatRaster') {
#       if (length(levels(scenario)[[1]]) == 1) {
#         # no levels/labels defined
#         warning('No levels defined for scenario raster; using numeric values only.')
#       }
#       area_scenario = calculate_area(scenario)
#     } else if ('data.frame' %in% class(scenario)) {
#       # assume area already calculated
#       if (!'area' %in% names(scenario) |
#           (!'label' %in% names(scenario) & !'value' %in% names(scenario))) {
#         # check that landcover2 contains the necessary fields
#         stop('scenario must contain fields "area" and either "label" or "value", or provide a SpatRaster')
#       } else {
#         # proceed below
#         area_scenario = scenario
#       }
#     } else {
#       # not a spatraster or a tibble/dataframe
#       stop('scenario must be a SpatRaster (from package terra), or a tibble or data.frame.')
#     }
#
#     metrics_scenario = full_join(area_scenario, df) %>%  #join by value and/or label fields
#       # calculate total score
#       mutate(SCORE_SCENARIO = SCORE * area) %>%
#       select(-area, -SCORE) %>%
#       group_by(across(c(-label, -value, -SCORE_SCENARIO))) %>%
#       summarize(SCORE_SCENARIO = sum(SCORE_SCENARIO, na.rm = TRUE),
#                 .groups = 'drop') %>%
#       drop_na()
#
#     metrics_change = full_join(metrics_base, metrics_scenario) %>%
#       mutate(SCORE_CHANGE = SCORE_SCENARIO - SCORE_BASELINE)
#
#     return(metrics_change)
#   } else {
#     return(metrics_base)
#   }
#
# }
