# PESTICIDE METRICS
# part of HEALTHY ENVIRONMENT metrics
#
# concerns for both human exposure and aquatic environment/biodiversity,
# although note that many pesticides enter the Delta from farther upstream
#
# GOAL: metric representing the average/range of total inputs of critical
# pesticides per acre as an indicator of risk to drinking water supplies and
# ecosystem health

# PACKAGES & FUNCTIONS
source('R/packages.R')
library(tabulizer)

# reference data:
baseline = rast('GIS/landscape_rasters/veg_baseline_fall.tif')
key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')
levels(baseline) = key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
delta = rast('GIS/boundaries/delta.tif')

# NOTES & OBJECTIVES---------
# Delta Plan performance measures
# (https://www.deltacouncil.ca.gov/pdf/delta-plan/2018-04-26-amended-appendix-e-performance-measures.pdf)
# specifically mention: - the number of waterbody-pesticide combinations on the
# 303(d) list (State Water Resources Control Board Integrated Report) - baseline
# from 2010 report, target = 0 by 2034 - completing TMDLs and Basin Plan
# Amendments for diazinon and chlorpyrifos by Jan 1 2013; Central Valley
# Pesticide TMDL by Jan 1 2016;
#
# "An overall reduction of critical pesticides in the waters and sediments of
# the Delta and Suisun Marsh will be beneficial for ecosystem health and
# drinking water supplies. The Central Valley Regional Water Quality Control
# Board (CVRWQCB) Basin Plan sets objectives to control pesticide runoff into
# Delta waters. Although pesticides have many uses for agriculture, urban, and
# residential areas, they also have the potential to contaminate drinking water
# supplies. Aquatic ecosystems contain species related to the target organisms
# of pesticides, and undesirable side effects may occur to native and desired
# species in relation to high pesticide exposure. The California Department of
# Pesticide Regulation reports that 13,084 pesticide formulations are registered
# in the state and are of concern to fish and zooplankton health."
#
# Additional sources of pesticide info:
# https://www.epa.gov/ingredients-used-pesticide-products/brief-overviews-about-individual-pesticides
# PubChem notes: https://pubchem.ncbi.nlm.nih.gov/ Pyrethroids:
# https://www.epa.gov/ingredients-used-pesticide-products/registration-review-pyrethrins-and-pyrethroids


# PESTICIDE RISK LISTS---------
# based on PUR summary reports

## reproductive toxicity-------
# pesticides that are on the State’s Proposition 65 list of chemicals that are
# “known to cause reproductive toxicity.”
repro = tabulizer::extract_tables('data_orig/pesticides/pur2018/ex_sum_18.pdf',
                                  pages = c(31:32), output = 'data.frame') %>%
  bind_rows() %>% select(chemname = AI, X2018)

# manually fix entries with wrapped chemnames
i = which(repro$X2018 == '')
repro$chemname[24] = paste(repro$chemname[24], repro$chemname[25])
repro$chemname[30] = paste(repro$chemname[30], repro$chemname[31])
repro$chemname[49] = paste(repro$chemname[49], repro$chemname[50])
repro$chemname[53] = paste(repro$chemname[53], repro$chemname[54])
repro$chemname[61] = paste(repro$chemname[61], repro$chemname[62])
repro = repro %>% filter(X2018 != '' & chemname != 'TOTAL')

## carcinogens----
# pesticides that are listed by U.S. EPA as A or B carcinogens or on the State’s
# Proposition 65 list of chemicals that are “known to cause cancer.”

carcinogens = tabulizer::extract_tables('data_orig/pesticides/pur2018/ex_sum_18.pdf',
                                        pages = c(36:37), output = 'data.frame') %>%
  bind_rows() %>% select(chemname = AI, X2018)

# manually fix entries with wrapped chemnames
i = which(carcinogens$X2018 == '')
carcinogens$chemname[1] = paste(carcinogens$chemname[1], carcinogens$chemname[2])
carcinogens$chemname[1] = paste(carcinogens$chemname[1], carcinogens$chemname[3])
carcinogens$chemname[42] = paste(carcinogens$chemname[42], carcinogens$chemname[43])
carcinogens$chemname[44] = paste(carcinogens$chemname[44], carcinogens$chemname[45])
carcinogens$chemname[65] = paste(carcinogens$chemname[65], carcinogens$chemname[66])
carcinogens$chemname[77] = paste(carcinogens$chemname[76], carcinogens$chemname[77])
carcinogens = carcinogens %>% filter(X2018 != '' & chemname != 'TOTAL')

## organophosphorus--------
# pesticides that are organophosphorus or carbamate cholinesterase-inhibiting
# pesticides

organop = tabulizer::extract_tables('data_orig/pesticides/pur2018/ex_sum_18.pdf',
                                    pages = c(43:44), output = 'data.frame') %>%
  bind_rows() %>% select(chemname = AI, X2018)

# manually fix entries with wrapped chemnames
i = which(organop$X2018 == '')
organop$chemname[40] = paste(organop$chemname[40], organop$chemname[41])
organop$chemname[57] = paste(organop$chemname[57], organop$chemname[58])
organop$chemname[63] = paste(organop$chemname[63], organop$chemname[64])
organop$chemname[65] = paste(organop$chemname[65], organop$chemname[66])
organop = organop %>% filter(X2018 != '' & chemname != 'TOTAL')

## groundwater concern-------
# pesticides that are on the “a” part of DPR’s groundwater protection list

# tabulizer having issues with this smaller table; easier to manually input
groundwater = tibble(chemname = c('ATRAZINE',
                                  'ATRAZINE, OTHER RELATED',
                                  'BENTAZON, SODIUM SALT',
                                  'BROMACIL',
                                  'BROMACIL, LITHIUM SALT',
                                  'DIURON',
                                  'NORFLURAZON',
                                  'PROMETON',
                                  'SIMAZINE'))

## toxic air-------
# pesticides used that are on DPR’s toxic air contaminants list applied in
# California
air = tabulizer::extract_tables('data_orig/pesticides/pur2018/ex_sum_18.pdf',
                                pages = c(51:52), output = 'data.frame') %>%
  bind_rows() %>% select(chemname = AI, X2018) %>%
  # add one more entry from 3rd page (tough for tabulizer to deal with)
  bind_rows(tibble(chemname = c('ZINC PHOSPHIDE'),
                   X2018 = '4,328'))

# manually fix entries with wrapped chemnames
i = which(air$X2018 == '')
air$chemname[4] = paste(air$chemname[4], air$chemname[5], air$chemname[6])
air$chemname[51] = paste(air$chemname[51], air$chemname[52])
air$chemname[64] = paste(air$chemname[64], air$chemname[65])
air$chemname[69] = paste(air$chemname[69], air$chemname[70])
air = air %>% filter(X2018 != '')

## aquatic risk------------
# water board report (2009)
# https://www.waterboards.ca.gov/centralvalley/water_issues/tmdl/central_valley_projects/central_valley_pesticides/risk_evaluation/rre_stff_rpt_feb2009_final.pdf

high_risk = tabulizer::extract_tables('data_orig/pesticides/rre_stff_rpt_feb2009_final.pdf',
                                      pages = 24)[[1]] %>% as_tibble()
names(high_risk) <- high_risk %>% slice(1) %>% unlist() %>% gsub('\r', ' ', .)
high_risk <- high_risk %>% slice(-1) %>% mutate(aquatic_risk = 'high')

mod_risk = tabulizer::extract_tables('data_orig/pesticides/rre_stff_rpt_feb2009_final.pdf',
                                     pages = 25)[[1]] %>% as_tibble() %>%
  separate(V3, into = c('V3', 'V4'), sep = ' ')
names(mod_risk) <- mod_risk %>% slice(1:3) %>%
  # mutate(rownum = c(1:3)) %>%
  pivot_longer(V1:V7) %>%
  group_by(name) %>% summarize(title = paste(value, collapse = ' ')) %>%
  mutate(title = stringr::str_trim(title),
         title = gsub('NA ', '', title),
         title = recode(title, 'Rank Toxicity' = 'Rank of Toxicity')) %>%
  pull(title)
mod_risk <- mod_risk %>% slice(-(1:3)) %>%
  mutate(aquatic_risk = 'moderate')

aquatic_risk = bind_rows(high_risk, mod_risk) %>%
  select(chemname = ChemName)
write_csv(aquatic_risk, 'data_orig/pesticides/pesticide_risk_aquatic.csv')

## Delta Plan critical pesticides---------
# Performance website for Delta Plan lists "critical pesticides" by water body
# that are in the pesticide use data:
# https://viewperformance.deltacouncil.ca.gov/pm/critical-pesticides

# Unclear: what all is in the list of "pyrethroids"
# known pyrethroids: https://biomonitoring.ca.gov/chemicals/pyrethroid-pesticides
pyrethroids = tibble(chemname = c('ALLETHRIN', 'D-ALLETHRIN',
                                  'BIFENTHRIN', #*
                                  'CYFLUTHRIN', 'BETA-CLYFLUTHRIN', #*
                                  'CYHALOTHRIN', 'GAMMA-CYHALOTHRIN', 'LAMBDA-CYHALOTHRIN', #*
                                  'CYPERMETHRIN', 'ALPHA-CYPERMETHRIN',
                                  '(S)-CYPERMETHRIN', #*
                                  'ZETA-CYPERMETHRIN',
                                  'CYPHENOTHRIN', 'DELTAMETHRIN',
                                  'ESFENVALERATE', 'ETOFENPROX',
                                  'FENPROPATHRIN', 'FLUMETHRIN',
                                  'IMIPROTHRIN', 'MOMFLUORTHRIN',
                                  'PERMETHRIN', #*
                                  'D-PHENOTHRIN', 'PRALLETHRIN',
                                  'PYRETHRINS', #*
                                  'RESMETHRIN',
                                  'TAU-FLUVALINATE', #*
                                  'TEFLUTHRIN',
                                  'TETRAMETHRIN', 'TRALOMETHRIN',
                                  '3-PHENOXYBENZOIC ACID (3-PBA)',
                                  '4-FLUORO-3-PHENOXYBENZOIC ACID (4F-3-PBA)',
                                  'TRANS-DCCA'))

critical_pesticides = read_csv('data_orig/pesticides/Critical Pesticides  Delta Stewardship Council.csv') %>%
  select(POLLUTANT) %>% distinct() %>% mutate(chemname = toupper(POLLUTANT)) %>%
  select(chemname) %>%
  # Assumptions: "Group A pesticides" refers to groundwater pesticides from
  # above, and 'organophosphorous pesticides' refers to those from above
  filter(!chemname %in% c('GROUP A PESTICIDES', 'ORGANOPHOSPHORUS PESTICIDES',
                          'PYRETHROIDS')) %>%
  bind_rows(organop) %>%
  bind_rows(groundwater) %>%
  bind_rows(pyrethroids)

# Note: several of these no longer in use, including CHLORDANE, DDT, DDE (since
# it's a breakdown product of DDT), TOXAPHENE; plus some being phased out
# (CHLORPYRIFOS) or in declining use (DISULFOTON)



# PESTICIDE USE DATA--------------
# original data from: ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives
# (accessible only using Internet Explorer)

# 2016, 2017, and 2018 (most recent available as of Nov 2021)

## compile raw data--------
pur_raw =
  bind_rows(
    purrr::map_df(paste0('data_orig/pesticides/pur2018/',
                         list.files('data_orig/pesticides/pur2018/',
                                    pattern = 'udc18_01|udc18_07|udc18_34|udc18_29|udc18_48|udc18_50|udc18_57')),
                  read_csv,
                  col_types = cols(county_cd = 'c',
                                   township = 'c',
                                   grower_id = 'c',
                                   license_no = 'c')) %>%
      mutate(year = 2018),
    purrr::map_df(paste0('data_orig/pesticides/pur2017/',
                         list.files('data_orig/pesticides/pur2017/',
                                    pattern = 'udc17_01|udc17_07|udc17_34|udc17_29|udc17_48|udc17_50|udc17_57')),
                  read_csv,
                  col_types = cols(county_cd = 'c',
                                   township = 'c',
                                   grower_id = 'c',
                                   license_no = 'c')) %>%
      mutate(year = 2017),
    purrr::map_df(paste0('data_orig/pesticides/pur2016/',
                         list.files('data_orig/pesticides/pur2016/',
                                    pattern = 'udc16_01|udc16_07|udc16_34|udc16_29|udc16_48|udc16_50|udc16_57')),
                  read_csv,
                  col_types = cols(county_cd = 'c',
                                   township = 'c',
                                   grower_id = 'c',
                                   license_no = 'c')) %>%
      mutate(year = 2016)
  ) %>%
  # add land cover details
  left_join(
    bind_rows(
      read_csv('data_orig/pesticides/pur2018/site.txt', col_types = cols()) %>%
        mutate(year = 2018),
      read_csv('data_orig/pesticides/pur2017/site.txt', col_types = cols()) %>%
        mutate(year = 2017),
      read_csv('data_orig/pesticides/pur2016/site.txt', col_types = cols()) %>%
        mutate(year = 2016)
    ), by = c('site_code', 'year')) %>%
  # add pesticide product info
  left_join(
    bind_rows(
      read_csv('data_orig/pesticides/pur2018/product.txt',
               col_types = cols(condreg_sw = 'c', agriccom_sw = 'c')) %>%
        mutate(year = 2018),
      read_csv('data_orig/pesticides/pur2017/product.txt',
               col_types = cols(condreg_sw = 'c', agriccom_sw = 'c')) %>%
        mutate(year = 2017),
      read_csv('data_orig/pesticides/pur2016/product.txt',
               col_types = cols(condreg_sw = 'c', agriccom_sw = 'c')) %>%
        mutate(year = 2016)
    ) %>% select(prodno, product_name, gen_pest_ind, signlwrd_ind, year),
    by = c('prodno', 'year')) %>%
  # add chemical name info
  left_join(
    bind_rows(
      read_csv('data_orig/pesticides/pur2018/chemical.txt', col_types = cols()) %>%
        mutate(year = 2018),
      read_csv('data_orig/pesticides/pur2017/chemical.txt', col_types = cols()) %>%
        mutate(year = 2017),
      read_csv('data_orig/pesticides/pur2016/chemical.txt', col_types = cols()) %>%
        mutate(year = 2016)
    ), by = c('chem_code', 'year')) %>%
  # format
  mutate(gen_pest_ind = recode(gen_pest_ind,
                               'C' = 'Chemical',
                               'M' = 'Microbial',
                               'K' = 'Both') %>%
           factor(levels = c('Chemical', 'Microbial', 'Both')),
         signlwrd_ind = recode(signlwrd_ind,
                               '1' = 'Danger (Poison)',
                               '2' = 'Danger (Only)',
                               '3' = 'Warning',
                               '4' = 'Caution',
                               '5' = 'None',
                               '0' = 'Unknown (0)') %>%
           factor(levels = c('Danger (Poison)', 'Danger (Only)', 'Warning',
                             'Caution', 'None', 'Unknown (0)')),
         aer_gnd_ind = recode(aer_gnd_ind,
                              'G' = 'Ground applied',
                              'A' = 'Aerially applied',
                              'O' = 'Other application',
                              'F' = 'Unknown (F)') %>%
           factor(levels = c('Aerially applied', 'Ground applied',
                             'Other application', 'Unknown (F)')))
write_csv(pur_raw, 'data_orig/pesticides/pesticide_use_raw.csv')

## address error codes-----
# error list
errors = bind_rows(
  full_join(read_csv('data_orig/pesticides/pur2018/errors2018.txt'),
            read_csv('data_orig/pesticides/pur2018/changes2018.txt'),
            by = c('use_no', 'error_id')) %>%
    mutate(year = 2018),
  full_join(read_csv('data_orig/pesticides/pur2017/errors2017.txt'),
            read_csv('data_orig/pesticides/pur2017/changes2017.txt'),
            by = c('use_no', 'error_id')) %>%
    mutate(year = 2017),
  full_join(read_csv('data_orig/pesticides/pur2016/errors2016.txt'),
            read_csv('data_orig/pesticides/pur2016/changes2016.txt'),
            by = c('use_no', 'error_id')) %>%
    mutate(year = 2016)
)
duplist = errors %>% filter(error_code == 80) %>% select(year, use_no) %>%
  mutate(use_id = paste(year, use_no, sep = '_'))
# ignore error_code 14: invalid site code

# error_code 47: area treated > area planted (update area planted with new value)
# error_code 60: area planted > 110% of section area
arealist = errors %>% filter(error_code %in% c(47, 60)) %>%
  select(year, use_no, new_value) %>%
  mutate(use_id = paste(year, use_no, sep = '_'))

# error_code 23: area treated > 110% of section area
trtlist = errors %>% filter(error_code == 23) %>%
  select(year, use_no, new_value) %>%
  mutate(use_id = paste(year, use_no, sep = '_'))

# error_code 76: outliers in pounds per area treated

pur_clean = pur_raw %>%
  mutate(use_id = paste(year, use_no, sep = '_')) %>%
  filter(!use_id %in% duplist$use_id) %>%  # remove duplicates (42,004)
  left_join(arealist %>% select(use_id, new_value), by = 'use_id') %>% #fix acre planted data
  mutate(acre_planted = if_else(!is.na(new_value), as.numeric(new_value), acre_planted)) %>%
  select(-new_value) %>%
  left_join(trtlist %>% select(use_id, new_value), by = 'use_id') %>%
  mutate(acre_treated = if_else(!is.na(new_value), as.numeric(new_value), acre_treated)) %>%
  select(-new_value)

## filter to Delta--------
# shapefiles for the townships and sections of California from:
# https://www.cdpr.ca.gov/docs/emon/grndwtr/plss_shapefiles.htm
# (clipped to Legal Delta boundary in ArcMap)
plss = sf::read_sf('GIS/boundaries/PLSS_Delta.shp')

pur_clean_delta = pur_clean %>% filter(comtrs %in% plss$CO_MTRS) %>%
  # exclude those without active ingredients?
  filter(!is.na(chemname))
# 114188 obs of 43 variables

## add risk info----------

pur_delta_risks = pur_clean_delta %>%
  left_join(repro %>% select(chemname) %>% mutate(repro = TRUE)) %>%
  left_join(carcinogens %>% select(chemname) %>% mutate(carcinogen = TRUE)) %>%
  left_join(organop %>% select(chemname) %>% mutate(organop = TRUE)) %>%
  left_join(groundwater %>% select(chemname) %>% mutate(groundwater = TRUE)) %>%
  left_join(air %>% select(chemname) %>% mutate(air = TRUE)) %>%
  left_join(aquatic_risk %>% select(chemname) %>% mutate(aquatic = TRUE)) %>%
  left_join(critical_pesticides %>% select(chemname) %>% mutate(critical = TRUE)) %>%
  select(year, site_name, county_cd, product_name, chemname, signlwrd_ind,
         repro, carcinogen, organop, groundwater, air, aquatic, critical,
         gen_pest_ind, aer_gnd_ind, lbs_chm_used, acre_treated, acre_planted) %>%
  filter(!(signlwrd_ind == 'None' & is.na(repro) & is.na(carcinogen) &
             is.na(organop) & is.na(groundwater) & is.na(air) & is.na(aquatic) &
             is.na(critical)))
write_csv(pur_delta_risks, 'data_orig/pesticides/pesticide_use_compiled.csv')

# gen_pest_in: general pesticide type; C = chemical, M = microbial, K = both
# rodent_sw: X = product registered as a pesticide
# signlwrd_ind: signal word indicator; 1 = Danger (Poison), 2 = Danger (Only),
#   3 = Warning, 4 = Caution, 5 = None, 0 = ???
#   NOTE: signalwrd goes with the product - each chemname can be in products
#     with different signlwrds! (whereas aquatic risk is specific to the chemname)
# aer_gnd_ind: A = Aerially applied; G = Ground (ground-based equipment)
#   applied; O = Other application methods (e.g. paint, ear tag, dip, injection,
#   chemigation, etc); says "must be A, G, or O", but data includes F


# SUMMARIZE BY LAND COVER---------
# match land cover classes as in baseline layer, but retain subclasses to allow
# identifying which specific crop types are driving the results

## classify-------
# and summarize annual use data by land cover class, subclass, chemical name, and year:
pur_simplify = pur_delta_risks %>%
  # assign to classes & subclasses to match Land IQ classifications
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
        'FIELD',
      site_name %in% c('WHEAT', 'WHEAT (FORAGE - FODDER)') ~
        'GRAIN&HAY_WHEAT',
      site_name %in% c('BARLEY', 'OAT', 'OAT (FORAGE - FODDER)', 'RYE',
                       'TRITICALE', 'FORAGE HAY/SILAGE') ~
        'GRAIN&HAY',
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
        'PASTURE',
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
      CODE_NAME == 'GRAIN&HAY' ~ 'MISCELLANEOUS GRAIN AND HAY',
      CODE_NAME == 'PASTURE' ~ 'MIXED PASTURE',
      CODE_NAME %in%
        c('GRASSLAND', 'IDLE', 'RICE', 'VINEYARD', 'URBAN', 'WATER') ~
        CODE_NAME)) %>%
  filter(CODE_NAME != 'UNKNOWN') %>% #outdoor plants in containers, greenhouse plants, rights of way, research commodity
  # summarize by all pesticide groups
  group_by(year, CODE_NAME, SUBCLASS, site_name, chemname, repro, carcinogen,
           organop, groundwater, air, aquatic, critical) %>%
  summarize(across(c(lbs_chm_used, acre_treated, acre_planted), sum, na.rm = TRUE),
            .groups = 'drop')
write_csv(pur_simplify, 'data_orig/pesticides/pesticide_use_annual_summary.csv')

## calculate annual use rates------
# lbs of chemical used per ha of each land cover class and subclass

# for total area of each class, start with baseline layer
baseline_ha = baseline %>% mask(delta) %>% freq() %>% as_tibble() %>%
  mutate(label = case_when(grepl('RIPARIAN', label) ~ 'RIPARIAN',
                           grepl('WETLAND_MANAGED', label) ~ 'WETLAND_MANAGED',
                           TRUE ~ label)) %>%
  group_by(label) %>%
  summarize(count = sum(count), .groups = 'drop') %>%
  mutate(area_ha = count * 30 * 30 / 10000)

# for details on specific crops, use ag_details from Land IQ layer:
ag_details = read_csv('data/landiq2018_area_detail.csv') %>%
  select(CLASS, SUBCLASS = SUBCLASS_MAIN, area_ha, total_area, prop) %>%
  mutate(SUBCLASS = case_when(
    SUBCLASS %in%
      c('PEACHES AND NECTARINES', 'PLUMS, PRUNES OR APRICOTS', 'CHERRIES') ~
      'STONE FRUIT',
    SUBCLASS == 'PISTACHIOS' ~ 'MISCELLANEOUS DECIDUOUS',
    SUBCLASS == 'KIWIS' ~ 'MISCELLANEOUS SUBTROPICAL FRUIT',
    CLASS == 'ORCHARD_DECIDUOUS' & is.na(SUBCLASS) ~ 'MISCELLANEOUS DECIDUOUS',
    CLASS == 'ORCHARD_CITRUS&SUBTROPICAL' & is.na(SUBCLASS) ~ 'MISCELLANEOUS SUBTROPICAL FRUIT',
    CLASS == 'GRAIN&HAY' & is.na(SUBCLASS) ~ 'MISCELLANEOUS GRAIN AND HAY',
    SUBCLASS %in% c('FLOWERS, NURSERY, AND CHRISTMAS TREE FARMS') ~ 'MISCELLANEOUS TRUCK',
    CLASS == 'ROW' & is.na(SUBCLASS) ~ 'MISCELLANEOUS TRUCK',
    CLASS == 'IDLE' ~ 'IDLE',
    CLASS %in% c('URBAN', 'VINEYARD', 'WATER') ~ CLASS,
    TRUE ~ SUBCLASS)) %>%
  group_by(CLASS, SUBCLASS, total_area) %>%
  summarize(area_ha = sum(area_ha), .groups = 'drop') %>%
  full_join(baseline_ha %>% select(CLASS = label, baseline_total = area_ha)) %>%
  mutate(total_area = case_when(CLASS %in% c('IDLE', 'URBAN') ~ baseline_total,
                                is.na(total_area) ~ baseline_total,
                                TRUE ~ total_area),
         area_ha = case_when(CLASS %in% c('IDLE', 'URBAN') ~ baseline_total,
                             is.na(area_ha) ~ baseline_total,
                             TRUE ~ area_ha),
         prop = area_ha/total_area,
         SUBCLASS = if_else(is.na(SUBCLASS), CLASS, SUBCLASS)) %>%
  select(CLASS, CLASS_AREA = total_area, SUBCLASS, SUBCLASS_AREA = area_ha,
         SUBCLASS_PROP = prop)
write_csv(ag_details, 'data/baseline_area.csv')

pur_details = pur_simplify %>%
  mutate(total = TRUE) %>% select(year:critical, total, everything()) %>%
  pivot_longer(repro:total, names_to = 'CONCERN') %>%
  filter(!is.na(value)) %>%
  group_by(CONCERN, CODE_NAME, SUBCLASS, year) %>%
  summarize(lbs_chm_used = sum(lbs_chm_used), .groups = 'drop') %>%
  bind_rows(
    tibble(CONCERN = 'total',
           CODE_NAME = c('RIPARIAN', 'WETLAND_MANAGED', 'WETLAND_TIDAL',
                         'WETLAND_OTHER', 'WOODLAND&SCRUB', 'BARREN', 'FIELD'),
           SUBCLASS = c('RIPARIAN', 'WETLAND_MANAGED', 'WETLAND_TIDAL',
                         'WETLAND_OTHER', 'WOODLAND&SCRUB', 'BARREN',
                        'MISCELLANEOUS FIELD'),
           year = 2016,
           lbs_chm_used = 0)
  ) %>%
  mutate(CODE_NAME = factor(
           CODE_NAME,
           levels = c('ORCHARD_DECIDUOUS', 'ORCHARD_CITRUS&SUBTROPICAL',
                      'VINEYARD', 'GRAIN&HAY', 'GRAIN&HAY_WHEAT', 'FIELD',
                      'FIELD_CORN', 'ROW', 'RICE', 'IDLE', 'PASTURE',
                      'PASTURE_ALFALFA', 'GRASSLAND', 'URBAN', 'RIPARIAN',
                      'WETLAND_MANAGED', 'WETLAND_TIDAL', 'WETLAND_OTHER',
                      'WATER', 'WOODLAND&SCRUB', 'BARREN')),
         SUBCLASS = factor(
           SUBCLASS,
           levels = c(unique(pur_simplify$SUBCLASS),
                      'RIPARIAN', 'WETLAND_MANAGED', 'WETLAND_TIDAL',
                      'WETLAND_OTHER', 'WOODLAND&SCRUB', 'BARREN',
                      'MISCELLANEOUS FIELD'))) %>%
  complete(nesting(CODE_NAME, SUBCLASS), CONCERN, year,
           fill = list(lbs_chm_used = 0, rate = 0)) %>%
  # total acreage of each crop type and class
  full_join(ag_details %>%
              select(CODE_NAME = CLASS, CLASS_AREA = total_area,
                     SUBCLASS, SUBCLASS_AREA = area_ha, SUBCLASS_PROP = prop),
            by = c("CODE_NAME", "SUBCLASS")) %>%
  mutate(rate = lbs_chm_used / SUBCLASS_AREA)
write_csv(pur_details, 'data_orig/pesticides/pesticide_use_annual_bysubclass.csv')

pur_details %>% filter(CONCERN == 'total') %>%
  ggplot(aes(rate, SUBCLASS, fill = as.factor(year))) +
  geom_col(position = 'dodge')
# rates for total lbs/ha are highest for cole crops, then pears, vineyard, and
# applies
pur_details %>% filter(CONCERN != 'total') %>%
  ggplot(aes(rate, SUBCLASS, fill = as.factor(year))) +
  geom_col(position = 'dodge') + facet_wrap(~CONCERN)
# BUT relative rates vary depending on the concern: pears stand out for air,
# aquatic, carcinogen; cole crops for critical pesticides, organop

## calculate average rates---------
## over all 3 years

## by subclass:
pur_details_subclass = pur_details %>%
  group_by(CODE_NAME, CONCERN, SUBCLASS) %>%
  summarize(rate = mean(rate), .groups = 'drop') %>%
  mutate(
    METRIC_CATEGORY = if_else(
      CONCERN == 'aquatic', 'biodiversity', 'healthy environment'),
    METRIC_SUBTYPE = if_else(
      CONCERN == 'aquatic', 'aquatic contaminant', 'pesticide exposure risk'),
    METRIC = 'pesticide application rate',
    UNIT = 'pounds per ha') %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, CONCERN, CODE_NAME, SUBCLASS, SCORE = rate, UNIT)
write_csv(pur_details_subclass, 'data/multiplebenefits/pesticide_exposure_subclass.csv')

## by class:
pur_details_class = pur_details %>%
  group_by(CODE_NAME, CONCERN, CLASS_AREA, year) %>%
  summarize(lbs_chm_used = sum(lbs_chm_used), .groups = 'drop') %>%
  mutate(rate = lbs_chm_used / CLASS_AREA) %>%
  group_by(CODE_NAME, CONCERN) %>%
  summarize(rate = mean(rate), .groups = 'drop') %>%
  mutate(
    METRIC_CATEGORY = if_else(
      CONCERN == 'aquatic', 'biodiversity', 'healthy environment'),
    METRIC_SUBTYPE = if_else(
      CONCERN == 'aquatic', 'aquatic contaminant', 'pesticide exposure risk'),
    METRIC = 'pesticide application rate',
    UNIT = 'pounds per ha') %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, CONCERN, CODE_NAME, SCORE = rate, UNIT)

write_csv(pur_details_class, 'data/multiplebenefits/pesticide_exposure.csv')

pur_details_class %>% filter(CONCERN != 'total') %>%
  ggplot(aes(SCORE, CODE_NAME, fill = CONCERN)) + geom_col(position = 'dodge')
# highest use rates in ORCHARD_DECIDUOUS and VINEYARD, depending on the concern
pur_details_subclass %>%
  filter(CONCERN != 'total' & CODE_NAME %in% c('ORCHARD_DECIDUOUS', 'VINEYARD')) %>%
  ggplot(aes(SCORE, SUBCLASS, fill = CONCERN)) + geom_col(position = 'dodge')
# within these classes, highest rates are for pears


plot_metric(pur2018_sum %>% filter(CONCERN == 'total'),
            xlab = "Pesticide Application Rate (lbs/ha planted, 2018)",
            range = FALSE)
