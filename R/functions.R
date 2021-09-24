codify_landcovers = function(df) {
  # when group1 = PASTURE: use Crp2014 to distinguish Alfalfa vs. irrigated pasture
  # when group1 = TRUCK AND FIELD, use Crp2014 to distinguish Corn from others
  # when group1 = GRAIN AND HAY, use Crp2014 to distinguish Wheat from others
  # when group1 = ORCHARD & comment = 'STRINGER' (and Crp2014 is NA), treat as
  #  idle --> VegCAMP layer defaulted to ORCHARD when no data from Land IQ and
  #  otherwise not identified; often includes field edges and road edges
  df %>%
    mutate(WATERBIRD_CLASS = case_when(group1 %in%
                                         c('WATER', 'RICE', 'BARREN') ~
                                         tolower(group1),
                                       group1 == 'ORCHARD' &
                                         is.na(comment) ~ 'orchard',
                                       #usually field edges or unspecified ag
                                       group1 == 'ORCHARD' &
                                         comment == 'STRINGER' ~ 'fallow',
                                       group1 == 'VINEYARD' ~ 'orchard',
                                       group1 == 'RIPARIAN' ~ 'woody wetland',
                                       group1 == 'URBAN' ~ 'developed',
                                       group1 == 'GRASSLAND' ~ 'Dryland pasture',
                                       group1 %in% c('IDLE', 'idle') ~ 'fallow',
                                       group1 %in% c('WETLAND', 'WETWATER') &
                                         comment %in% c('SEASONAL', 'SEMIPERM') ~
                                         'DU wetland',
                                       group1 %in% c('WETLAND', 'WETWATER') &
                                         comment %in% c('CHANNEL', 'UNMANAGED') ~
                                         'wetland',
                                       group1 %in% c('WETLAND', 'WETWATER') &
                                         comment %in%
                                         c('TIDAL MARSH (FRESH)',
                                           'TIDAL MARSH (SALT)') ~
                                         'tidal marsh',
                                       group1 %in%
                                         c('WOODLAND', 'OAKWOODLAND', 'SCRUB') ~
                                         'forest',
                                       group1 == 'TRUCK AND FIELD' &
                                         Crp2014 == 'Corn, Sorghum and Sudan' ~
                                         'corn',
                                       group1 == 'TRUCK AND FIELD' &
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
                                         'row',
                                       group1 == 'TRUCK AND FIELD' &
                                         Crp2014 %in% c(
                                           'Sunflowers',
                                           'Safflower',
                                           'Miscellaneous Field Crops') ~
                                         'field',
                                       group1 == 'GRAIN AND HAY' &
                                         Crp2014 == 'Wheat' ~ 'wheat',
                                       group1 == 'GRAIN AND HAY' ~ 'grain',
                                       group1 == 'PASTURE' &
                                         Crp2014 == 'Alfalfa and Alfalfa Mixtures' ~
                                         'alfalfa',
                                       group1 == 'PASTURE' & is.na(Crp2014) &
                                         NVCS_Nm == 'Cynodon dactylon' ~ 'Dryland pasture',
                                       group1 == 'PASTURE' &
                                         Crp2014 %in%
                                         c('Mixed Pasture',
                                           'Miscellaneous Grasses') ~
                                         'Irrigated pasture',
                                       Crp2014 == 'Flowers, Nursery and Christmas Tree Farms' ~
                                         'orchard'),
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

codify_landiq = function(df, meta) {
  df = df %>%
    left_join(metadat_2018 %>%
                select(CROPTYP,
                       CLASS_NAME1 = CLASS_NAME,
                       SUBCLASS_NAME1 = SUBCLASS_NAME),
              by = c('CROPTYP1' = 'CROPTYP')) %>%
    left_join(metadat_2018 %>%
                select(CROPTYP,
                       CLASS_NAME2 = CLASS_NAME,
                       SUBCLASS_NAME2 = SUBCLASS_NAME),
              by = c('CROPTYP2' = 'CROPTYP')) %>%
    left_join(metadat_2018 %>%
                select(CROPTYP,
                       CLASS_NAME3 = CLASS_NAME,
                       SUBCLASS_NAME3 = SUBCLASS_NAME),
              by = c('CROPTYP3' = 'CROPTYP')) %>%
    select(SYMB_CLASS, MULTIUSE,
           CROPTYP1, CLASS1, CLASS_NAME1, SUBCLASS_NAME1, ADOY1,
           CROPTYP2, CLASS2, CLASS_NAME2, SUBCLASS_NAME2, ADOY2,
           CROPTYP3, CLASS3, CLASS_NAME3, SUBCLASS_NAME3, ADOY3) %>%
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
      waterbird_fall = case_when(
        CLASS_MAIN == 'RICE' ~ 'rice',
        CLASS_MAIN %in%
          c('DECIDUOUS FRUITS AND NUTS',
            'CITRUS AND SUBTROPICAL',
            'VINEYARDS',
            'YOUNG PERENNIAL') ~ 'orchard',
        SUBCLASS_MAIN == 'FLOWERS, NURSERY, AND CHRISTMAS TREE FARMS' ~ 'orchard',
        CLASS_MAIN %in%
          c('IDLE',
            'UNCLASSIFIED FALLOW - STATUS NOT IDENTIFIED') ~ 'fallow',
        CLASS_MAIN %in%
          c('URBAN - RESIDENTIAL, COMMERCIAL, AND INDUSTRIAL, UNSEGREGATED') ~ 'developed',
        SUBCLASS_MAIN == 'ALFALFA & ALFALFA MIXTURES' ~ 'alfalfa',
        SUBCLASS_MAIN == 'CORN, SORGHUM, OR SUDAN' ~ 'corn',
        SUBCLASS_MAIN %in%
          c('MIXED PASTURE',
            'MISCELLANEOUS GRASSES') ~ 'Irrigated pasture',
        SUBCLASS_MAIN == 'WHEAT' ~ 'wheat',
        SUBCLASS_MAIN %in%
          c('MELONS, SQUASH, AND CUCUMBERS (ALL TYPES)',
            'TOMATOES (PROCESSING)',
            'BEANS (DRY)',
            'ONIONS & GARLIC',
            'PEPPERS (CHILI, BELL, ETC)',
            'BUSH BERRIES',
            'MISCELLANEOUS TRUCK',
            'STRAWBERRIES',
            'COLE CROPS (MIXTURE OF 22-25)',
            'CARROTS',
            'POTATO OR SWEET POTATO',
            'POTATOES',
            'LETTUCE OR LEAFY GREENS',
            'GREENHOUSE') ~ 'row',
        CLASS_MAIN == 'TRUCK, NURSERY, AND BERRY CROPS' &
          is.na(SUBCLASS_MAIN) ~ 'row',
        CLASS_MAIN == 'GRAIN AND HAY' &
          is.na(SUBCLASS_MAIN) ~ 'grain',
        SUBCLASS_MAIN %in%
          c('MISCELLANEOUS GRAIN AND HAY') ~ 'grain',
        SUBCLASS_MAIN %in%
          c('SAFFLOWER',
            'SUNFLOWERS',
            'COTTON',
            'MISCELLANEOUS FIELD') ~ 'field',
        TRUE ~ 'UNCLEAR'),
      waterbird_winter = case_when(
        !is.na(CLASS_WINTER) & SUBCLASS_WINTER == 'WHEAT' ~ 'wheat',
        !is.na(CLASS_WINTER) &
          SUBCLASS_WINTER == 'MISCELLANEOUS GRAIN AND HAY' ~ 'grain',
        !is.na(CLASS_WINTER) &
          CLASS_WINTER == 'TRUCK, NURSERY, AND BERRY CROPS' ~ 'row',
        !is.na(CLASS_WINTER) &
          CLASS_WINTER == 'FIELD CROPS' ~ 'field',
        !is.na(CLASS_WINTER) &
          SUBCLASS_WINTER == 'ALFALFA & ALFALFA MIXTURES' ~ 'alfalfa',
        !is.na(CLASS_WINTER) &
          SUBCLASS_WINTER == 'MISCELLANEOUS GRASSES' ~ 'Irrigated pasture',
        CLASS_MAIN == 'RICE' ~ 'rice',
        CLASS_MAIN %in%
          c('DECIDUOUS FRUITS AND NUTS',
            'CITRUS AND SUBTROPICAL',
            'VINEYARDS',
            'YOUNG PERENNIAL') ~ 'orchard',
        SUBCLASS_MAIN == 'FLOWERS, NURSERY, AND CHRISTMAS TREE FARMS' ~ 'orchard',
        CLASS_MAIN %in%
          c('IDLE',
            'UNCLASSIFIED FALLOW - STATUS NOT IDENTIFIED') ~ 'fallow',
        CLASS_MAIN %in%
          c('URBAN - RESIDENTIAL, COMMERCIAL, AND INDUSTRIAL, UNSEGREGATED') ~
          'developed',
        SUBCLASS_MAIN == 'ALFALFA & ALFALFA MIXTURES' ~ 'alfalfa',
        SUBCLASS_MAIN == 'CORN, SORGHUM, OR SUDAN' ~ 'corn',
        SUBCLASS_MAIN %in%
          c('MIXED PASTURE',
            'MISCELLANEOUS GRASSES') ~ 'Irrigated pasture',
        SUBCLASS_MAIN == 'WHEAT' ~ 'wheat',
        SUBCLASS_MAIN %in%
          c('MELONS, SQUASH, AND CUCUMBERS (ALL TYPES)',
            'TOMATOES (PROCESSING)',
            'BEANS (DRY)',
            'ONIONS & GARLIC',
            'PEPPERS (CHILI, BELL, ETC)',
            'BUSH BERRIES',
            'MISCELLANEOUS TRUCK',
            'STRAWBERRIES',
            'COLE CROPS (MIXTURE OF 22-25)',
            'CARROTS',
            'POTATO OR SWEET POTATO',
            'POTATOES',
            'LETTUCE OR LEAFY GREENS',
            'GREENHOUSE') ~ 'row',
        CLASS_MAIN == 'TRUCK, NURSERY, AND BERRY CROPS' &
          is.na(SUBCLASS_MAIN) ~ 'row',
        CLASS_MAIN == 'GRAIN AND HAY' &
          is.na(SUBCLASS_MAIN) ~ 'grain',
        SUBCLASS_MAIN %in%
          c('MISCELLANEOUS GRAIN AND HAY') ~ 'grain',
        SUBCLASS_MAIN %in%
          c('SAFFLOWER',
            'SUNFLOWERS',
            'COTTON',
            'MISCELLANEOUS FIELD') ~ 'field',
        TRUE ~ 'UNCLEAR')
      ) %>%
      mutate_at(vars(CLASS_MAIN:waterbird_winter), as.factor)
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
