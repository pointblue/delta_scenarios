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
        CLASS == 'GRAIN AND HAY' ~ 'GRAIN&HAY',
        CLASS == 'TRUCK AND FIELD' & CROP == 'Corn, Sorghum and Sudan' ~ 'FIELD_CORN',
        CLASS == 'PASTURE' & CROP == 'Alfalfa and Alfalfa Mixtures' ~ 'PASTURE_ALFALFA',
        # lump other crops?
        CLASS == 'TRUCK AND FIELD' &
          CROP %in% c('Beans (Dry)', 'Cotton', 'Sunflowers', 'Safflower',
                         'Miscellaneous Field Crops') ~ 'FIELD',
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
        CLASS %in% c('WOODLAND', 'OAKWOODLAND', 'SCRUB') ~ 'WOODLAND&SCRUB',
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
                   'GRAIN&HAY', 'GRAIN&HAY_WHEAT', 'FIELD', 'FIELD_CORN', 'ROW',
                   'RICE', 'IDLE', 'GRASSLAND&PASTURE', 'PASTURE',
                   'PASTURE_ALFALFA', 'GRASSLAND', 'URBAN', 'RIPARIAN',
                   'RIPARIAN_FOREST', 'RIPARIAN_FOREST_POFR',
                   'RIPARIAN_FOREST_QULO', 'RIPARIAN_FOREST_SALIX',
                   'RIPARIAN_FOREST_MIXED', 'RIPARIAN_SCRUB',
                   'RIPARIAN_SCRUB_INTRO', 'RIPARIAN_SCRUB_SALIX',
                   'RIPARIAN_SCRUB_MIXED', 'WETLAND', 'WETLAND_MANAGED',
                   'WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL',
                   'WETLAND_TIDAL', 'WETLAND_OTHER', 'WATER', 'WOODLAND&SCRUB',
                   'BARREN'))) %>%
    left_join(codekey %>% select(CLASS = CODE_NAME, CODE_BASELINE),
              by = 'CLASS')
}

codify_NASS = function(NASSraster, season = 'fall', codekey) {
  # build classification matrix
  mat = freq(NASSraster) %>% select(code_orig = value, label_orig = label) %>%
    mutate(code_orig = as.numeric(code_orig),
           CLASS = case_when(
      label_orig %in% c('Corn', 'Sorghum', 'Pop or Orn Corn', 'Sweet Corn') ~
        'FIELD_CORN',
      label_orig == 'Rice' ~ 'RICE',
      label_orig %in%
        c('Spring Wheat', 'Winter Wheat', 'Durum Wheat', 'Triticale') ~
        'GRAIN&HAY_WHEAT',
      label_orig %in% c('Barley', 'Rye', 'Oats', 'Speltz', 'Other Small Grains') ~
        'GRAIN&HAY',
      label_orig %in%
        c('Cotton', 'Sunflower', 'Safflower', 'Dry Beans', 'Vetch', 'Millet',
          'Canola', 'Flaxseed', 'Rape Seed', 'Mustard', 'Camelina', 'Buckwheat',
          'Other Crops') ~ 'FIELD',
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
        'PASTURE',
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
          'Forest') ~
        'WOODLAND&SCRUB',
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
        c('Dbl Crop WinWht/Cotton') ~ 'FIELD',
      season == 'winter' & label_orig %in%
        c('Dbl Crop WinWht/Corn', 'Dbl Crop WinWht/Sorghum',
          'Dbl Crop WinWht/Soybeans', 'Dbl Crop Triticale/Corn',
          'Dbl Crop Lettuce/Durum Wht', 'Dbl Crop Durum Wht/Sorghum',
          'Dbl Crop WinWht/Cotton') ~ 'GRAIN&HAY_WHEAT',
      season == 'winter' & label_orig %in%
        c('Dbl Crop Oats/Corn', 'Dbl Crop Barley/Corn',
          'Dbl Crop Barley/Soybeans', 'Dbl Crop Soybeans/Oats',
          'Dbl Crop Lettuce/Barley') ~ 'GRAIN&HAY',
      season == 'fall' & label_orig %in%
        c('Dbl Corn/Soybeans') ~ 'ROW',
      label_orig %in% c('Background', 'Clouds/No Data', 'Nonag/Undefined') ~ NA_character_)) %>%
  left_join(codekey %>% select(CLASS = CODE_NAME, CODE_BASELINE),
            by = 'CLASS')

  terra::classify(NASSraster,
                  rcl = mat %>% select(from = code_orig, to = CODE_BASELINE) %>%
                    as.matrix(),
                  othersNA = TRUE)
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
        CLASS_MAIN == 'GRAIN AND HAY' ~ 'GRAIN&HAY', #ALL OTHER GRAIN&HAY
        CLASS_MAIN == 'FIELD CROPS' & SUBCLASS_MAIN == 'CORN, SORGHUM, OR SUDAN' ~
          'FIELD_CORN',
        CLASS_MAIN == 'FIELD CROPS' ~ 'FIELD', #ALL OTHER FIELD CROPS
        CLASS_MAIN == 'TRUCK, NURSERY, AND BERRY CROPS' ~ 'ROW',
        CLASS_MAIN == 'RICE' ~ 'RICE',
        CLASS_MAIN == 'UNCLASSIFIED FALLOW - STATUS NOT IDENTIFIED' ~ 'IDLE',
        CLASS_MAIN == 'PASTURE' & SUBCLASS_MAIN == 'ALFALFA & ALFALFA MIXTURES' ~
          'PASTURE_ALFALFA',
        CLASS_MAIN == 'PASTURE' ~ 'PASTURE', #ALL OTHER PASTURE
        CLASS_MAIN == 'URBAN - RESIDENTIAL, COMMERCIAL, AND INDUSTRIAL, UNSEGREGATED' ~
          'URBAN',
        TRUE ~ 'UNCLEAR'),
      CLASS_WINTER = case_when(
        CLASS_WINTER == 'GRAIN AND HAY' & SUBCLASS_WINTER == 'WHEAT' ~ 'GRAIN&HAY_WHEAT',
        CLASS_WINTER == 'GRAIN AND HAY' ~ 'GRAIN&HAY', #ALL OTHER GRAIN&HAY
        CLASS_WINTER == 'FIELD CROPS' ~ 'FIELD',
        CLASS_WINTER == 'TRUCK, NURSERY, AND BERRY CROPS' ~ 'ROW',
        CLASS_WINTER == 'PASTURE' &
          SUBCLASS_WINTER == 'ALFALFA & ALFALFA MIXTURES' ~ 'PASTURE_ALFALFA',
        CLASS_WINTER == 'PASTURE' ~ 'PASTURE', #ALL OTHER PASTURE
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

#---> move to R package!
python_prep = function(stack, masklayers = NULL, pixel_value = NULL,
                       path, subdir = NULL, layernames, suffix,
                       overwrite = FALSE) {
  nlayer = nlyr(stack)
  # for each layer (scenario):
  purrr::map(c(1:nlayer),
             function(x) {
               # split by land cover classes
               newstack = segregate(stack[[x]], other = 0)
               names(newstack) = paste0(layernames, suffix[1])

               if (!is.null(masklayers)) {# mask by presence of each land cover type
                 newstack_mask = mask(masklayers[[x]], newstack, maskvalue = 0)
                 names(newstack_mask) = paste0(layernames, suffix[2])
                 writeRaster(newstack_mask,
                             filename = paste0(path, '/', names(stack)[x], '/',
                                               names(newstack_mask), '.tif'),
                             overwrite = overwrite)
               }

               if (!is.null(pixel_value)) {
                 # replace presence (1) with another value (e.g., pixel area)
                 newstack = subst(newstack, from = 1, to = pixel_value)
               }

               if (is.null(subdir)) {
                 writeRaster(newstack,
                             filename = paste0(path, '/', names(stack)[x], '/',
                                               names(newstack), '.tif'),
                             overwrite = overwrite)
               } else {
                 writeRaster(newstack,
                             filename = paste0(path, '/', names(stack)[x], '/',
                                               subdir, '/',
                                               names(newstack), '.tif'),
                             overwrite = overwrite)
               }

              }
  )}

python_run = function(scenario, scale, type = 'waterbird',
                      pathin, pathout,
                      zero_ras = NULL, mask = NULL, suffix = NULL,
                      overwrite = FALSE) {

  if (type == 'waterbird') {

    if (scale == '2k') {
      bufflabel = '2000'
    } else if (scale == '5k') {
      bufflabel = '5000'
    } else if (scale == '10k') {
      bufflabel = '10000'
    }

    # calculate sum of total area:
    focal_stats(
      pathin = paste0(pathin, scenario),
      pathout = paste0(pathout, scenario, '/buffer', scale, '/raw'),
      buffer = bufflabel, fun = 'SUM', regex = '*_area.tif')

    # calculate mean probability of open water
    focal_stats(
      pathin = paste0(pathin, scenario),
      pathout = paste0(pathout, scenario, '/buffer', scale, '/raw'),
      buffer = bufflabel, fun = 'MEAN', regex = '*_pfld.tif')

    # clean up results
    process_focal_stats(
      pathin = paste0(pathout, scenario, '/buffer', scale, '/raw'),
      pathout = paste0(pathout, scenario, '/buffer', scale, '/'),
      suffix = paste0('_', scale), zero_ras = zero_ras, mask = mask,
      overwrite = overwrite)

  } else if (type == 'riparian') {

    # calculate sum of pixels:
    focal_stats(
      pathin = paste0(pathin, scenario, '/buffer', scale),
      pathout = paste0(pathout, scenario, '/buffer', scale),
      buffer = scale, fun = 'SUM',
      # regex added after code was actually run - should allow shape_raw.tif to
      # go in the same directory, but needs testing
      regex = paste0('*_', scale, '.tif'))

    # clean up results
    process_focal_stats(
      pathin = paste0(pathout, scenario, '/buffer', scale),
      pathout = paste0(pathout, scenario, '/'),
      suffix = NULL, zero_ras = zero_ras, mask = mask,
      overwrite = overwrite)
  }
}

process_focal_stats = function(pathin, pathout, zero_ras = NULL, mask = NULL,
                               suffix = NULL, overwrite = FALSE) {

  ras = list.files(pathin, '.tif$', full.names = TRUE) %>% rast()

  if (!is.null(zero_ras)) {
    ras = cover(ras, zero_ras)
  }
  if (!is.null(mask)) {
    ras = mask(ras, mask)
  }
  if (!is.null(suffix)) {
    names(ras) = paste0(names(ras), suffix)
  }

  writeRaster(ras,
              filename = paste0(pathout, names(ras), '.tif'),
              overwrite = overwrite)
}

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


