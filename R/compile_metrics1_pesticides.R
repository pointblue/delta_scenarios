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
baseline = rast('GIS/landscape_rasters/veg_baseline.tif')
baseline_win = rast('GIS/landscape_rasters/veg_baseline_winter.tif')

key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')
delta = rast('GIS/boundaries/delta.tif')

ag_details = read_csv('data/baseline_area.csv')
winter_change = read_csv('data/baseline_area_winterchange.csv')

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

## all---------
pesticide_table = bind_rows(repro %>% select(chemname) %>% mutate(group = 'repro'),
          carcinogens %>% select(chemname) %>% mutate(group = 'carcinogen'),
          organop %>% select(chemname) %>% mutate(group = 'organop'),
          groundwater %>% select(chemname) %>% mutate(group = 'groundwater'),
          air %>% select(chemname) %>% mutate(group = 'air'),
          aquatic_risk %>% select(chemname) %>% mutate(group = 'aquatic'),
          critical_pesticides %>% select(chemname) %>% mutate(group = 'critical')) %>%
  distinct() %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = group, values_from = value) %>%
  arrange(chemname)
write_csv(pesticide_table, 'data_orig/pesticides/pesticide_risk_groups.csv')


# PESTICIDE USE DATA--------------
# original data from: ftp://transfer.cdpr.ca.gov/pub/outgoing/pur_archives
# (accessible only using Internet Explorer)

# 2016, 2017, and 2018 (most recent available as of Nov 2021)

## compile raw data--------
# include only the 5 counties in the Delta
# 07 = Contra Costa
# 34 = Sacramento
# 39 = San Joaquin
# 48 = Solano
# 57 = Yolo

years = c(2014:2018)

pur_raw = purrr::map_df(years %>% setNames(years),
                        function(x) {
                          fl = list.files(
                            paste0('data_orig/pesticides/pur', x, '/'),
                            pattern = '_07|_34|_39|_48|_57',
                            full.names = TRUE)
                          purrr::map_df(fl, read_csv,
                                        col_types = cols(county_cd = 'c',
                                                         township = 'c',
                                                         grower_id = 'c',
                                                         license_no = 'c',
                                                         site_loc_id = 'c',
                                                         aer_gnd_ind = 'c',
                                                         acre_planted = 'n',
                                                         unit_planted = 'c',
                                                         error_flag = 'c',
                                                         applic_time = 'c',
                                                         base_ln_mer = 'c',
                                                         tship_dir = 'c',
                                                         range = 'c',
                                                         range_dir = 'c',
                                                         section = 'n',
                                                         comtrs = 'c'))
                        }, .id = 'year')

# compile land cover details
landcover = purrr::map_dfr(years %>% setNames(years),
                           ~read_csv(
                             paste0('data_orig/pesticides/pur', .x, '/site.txt'),
                             col_types = cols()),
                          .id = 'year')

# pesticide product info
product = purrr::map_dfr(years %>% setNames(years),
                         ~read_csv(
                           paste0('data_orig/pesticides/pur', .x, '/product.txt'),
                           col_types = cols(condreg_sw = 'c',
                                            agriccom_sw = 'c')) %>%
                           select(prodno, product_name, gen_pest_ind,
                                  signlwrd_ind),
                         .id = 'year')

# chemical name info
chemname = purrr::map_dfr(years %>% setNames(years),
                          ~read_csv(
                            paste0('data_orig/pesticides/pur', .x, '/chemical.txt'),
                            col_types = cols()),
                          .id = 'year')

pur_raw = pur_raw %>%
  left_join(landcover, by = c('site_code', 'year'))  %>%
  left_join(product, by = c('prodno', 'year')) %>%
  left_join(chemname, by = c('chem_code', 'year')) %>%
  # format:
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
errors = purrr::map_dfr(years %>% setNames(years),
                        function(x) {
                          full_join(
                            read_csv(
                              paste0('data_orig/pesticides/pur', x, '/errors',
                                     x, '.txt'),
                              col_types = cols()),
                            read_csv(
                              paste0('data_orig/pesticides/pur', x, '/changes',
                                     x, '.txt'),
                              col_types = cols()),
                            by = c('use_no', 'error_id')
                          )
                        }, .id = 'year')

# error_code 80: duplicates
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
  # remove duplicates (42,004)
  filter(!use_id %in% duplist$use_id) %>%
  #fix acre planted data
  left_join(arealist %>% select(use_id, new_value), by = 'use_id') %>%
  mutate(acre_planted = if_else(!is.na(new_value), as.numeric(new_value), acre_planted)) %>%
  select(-new_value) %>%
  # fix acre treated data
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
# 298597 obs of 43 variables

## add risk info----------

pur_delta_risks = pur_clean_delta %>%
  left_join(pesticide_table, by = 'chemname') %>%
  select(year, site_name, county_cd, product_name, chemname, signlwrd_ind,
         repro, carcinogen, groundwater, air, aquatic, critical,
         gen_pest_ind, aer_gnd_ind, lbs_chm_used, acre_treated, acre_planted) %>%
  filter(!(is.na(repro) & is.na(carcinogen) & is.na(groundwater) & is.na(air) &
             is.na(aquatic) & is.na(critical)))
write_csv(pur_delta_risks, 'data_orig/pesticides/pesticide_use_compiled.csv')
# --> including only those pesticides listed in one of the risk groups (excluding the organop group)

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
           groundwater, air, aquatic, critical) %>%
  summarize(across(c(lbs_chm_used, acre_treated, acre_planted), sum, na.rm = TRUE),
            .groups = 'drop')
write_csv(pur_simplify, 'data_orig/pesticides/pesticide_use_annual_summary.csv')

## calculate annual use rates------
# lbs of chemical used per ha of each land cover class and subclass by concern group and overall


pur_details = pur_simplify %>%
  mutate(total = TRUE) %>% select(year:critical, total, everything()) %>%
  pivot_longer(repro:total, names_to = 'CONCERN') %>%
  filter(!is.na(value)) %>%
  group_by(CONCERN, CODE_NAME, SUBCLASS, year) %>%
  summarize(lbs_chm_used = sum(lbs_chm_used), .groups = 'drop') %>%
  # total acreage of each crop type and class
  left_join(ag_details %>%
              select(CODE_NAME = CLASS, CLASS_AREA,
                     SUBCLASS, SUBCLASS_AREA, SUBCLASS_PROP),
            by = c("CODE_NAME", "SUBCLASS")) %>%
  mutate(rate = lbs_chm_used / SUBCLASS_AREA)
write_csv(pur_details, 'data_orig/pesticides/pesticide_use_annual_bysubclass.csv')

pur_details %>% filter(CONCERN == 'total') %>%
  ggplot(aes(rate, CODE_NAME, fill = as.factor(year))) +
  geom_col(position = 'dodge')
# rates for total lbs/ha are highest for row, but highly variable by year

## calculate average rates---------
## over all 5 years

## by subclass:
pur_details_subclass = pur_details %>%
  group_by(CODE_NAME, CONCERN, SUBCLASS) %>%
  summarize(SCORE = mean(rate),
            SCORE_MIN = min(rate),
            SCORE_MAX = max(rate),
            .groups = 'drop') %>%
  mutate(
    METRIC_CATEGORY = if_else(
      CONCERN == 'aquatic', 'biodiversity', 'healthy environment'),
    METRIC_SUBTYPE = 'pesticide application rate',
    METRIC = case_when(
      CONCERN == 'aquatic' ~ 'aquatic contaminant',
      CONCERN == 'air' ~ 'air pollution',
      CONCERN == 'carcinogen' ~ 'carcinogen',
      CONCERN == 'groundwater' ~ 'groundwater contaminant',
      CONCERN == 'organop' ~ 'organophosphorus',
      CONCERN == 'repro' ~ 'reproductive toxicity',
      CONCERN == 'critical' ~ 'critical pesticides',
      CONCERN == 'total' ~ 'total pesticides'),
    UNIT = 'pounds per ha') %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, CODE_NAME, SUBCLASS,
         SCORE, SCORE_MIN, SCORE_MAX, UNIT)
write_csv(pur_details_subclass, 'data/pesticide_exposure_subclass.csv')

## by class:
pur_details_class = pur_details %>%
  group_by(CODE_NAME, CONCERN, CLASS_AREA, year) %>%
  summarize(lbs_chm_used = sum(lbs_chm_used), .groups = 'drop') %>%
  mutate(rate = lbs_chm_used / CLASS_AREA) %>%
  group_by(CODE_NAME, CONCERN) %>%
  summarize(SCORE = mean(rate),
            SCORE_MIN = min(rate),
            SCORE_MAX = max(rate),
            .groups = 'drop') %>%
  mutate(
    METRIC_CATEGORY = if_else(
      CONCERN == 'aquatic', 'biodiversity', 'healthy environment'),
    METRIC_SUBTYPE = 'pesticide application rate',
    METRIC = case_when(
      CONCERN == 'aquatic' ~ 'aquatic contaminant',
      CONCERN == 'air' ~ 'air pollution',
      CONCERN == 'carcinogen' ~ 'carcinogen',
      CONCERN == 'groundwater' ~ 'groundwater contaminant',
      CONCERN == 'repro' ~ 'reproductive toxicity',
      CONCERN == 'critical' ~ 'critical pesticides',
      CONCERN == 'total' ~ 'total pesticides'),
    UNIT = 'pounds per ha') %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, CODE_NAME, SCORE, SCORE_MIN,
         SCORE_MAX, UNIT)

# check for missing:
pur_details_class %>% select(METRIC, CODE_NAME, SCORE) %>%
  pivot_wider(names_from = 'METRIC', values_from = 'SCORE') %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = key %>% pull(CODE_NAME))) %>%
  complete(CODE_NAME) %>% print(n = 40)

# fill in missing:
# assume none for grassland, riparian, wetland, woodland&scrub, barren
pur_details_fill = pur_details_class %>%
  bind_rows(
    pur_details_class %>% filter(CODE_NAME == 'URBAN') %>%
      mutate(CODE_NAME = 'RIPARIAN', SCORE = 0, SCORE_MIN = NA, SCORE_MAX = NA),
    pur_details_class %>% filter(CODE_NAME == 'URBAN') %>%
      mutate(CODE_NAME = 'WETLAND_MANAGED', SCORE = 0, SCORE_MIN = NA, SCORE_MAX = NA),
    pur_details_class %>% filter(CODE_NAME == 'URBAN') %>%
      mutate(CODE_NAME = 'WETLAND_OTHER', SCORE = 0, SCORE_MIN = NA, SCORE_MAX = NA),
    pur_details_class %>% filter(CODE_NAME == 'URBAN') %>%
      mutate(CODE_NAME = 'GRASSLAND', SCORE = 0, SCORE_MIN = NA, SCORE_MAX = NA),
    pur_details_class %>% filter(CODE_NAME == 'URBAN') %>%
      mutate(CODE_NAME = 'WOODLAND&SCRUB', SCORE = 0, SCORE_MIN = NA, SCORE_MAX = NA),
    pur_details_class %>% filter(CODE_NAME == 'URBAN') %>%
      mutate(CODE_NAME = 'BARREN', SCORE = 0, SCORE_MIN = NA, SCORE_MAX = NA)
  )
# complete other missing values (if NA, then none)
pur_details_fill = pur_details_fill %>%
  mutate(CODE_NAME = factor(CODE_NAME,
                            levels = pur_details_fill %>% pull(CODE_NAME) %>% unique())) %>%
  complete(CODE_NAME, nesting(METRIC, METRIC_CATEGORY, METRIC_SUBTYPE, UNIT),
           fill = list(SCORE = 0, SCORE_MIN = NA, SCORE_MAX = NA))

pur_details_fill %>% select(METRIC, CODE_NAME, SCORE) %>%
  pivot_wider(names_from = 'METRIC', values_from = 'SCORE') %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = key %>% pull(CODE_NAME))) %>%
  complete(CODE_NAME) %>% print(n = 40)

write_csv(pur_details_fill, 'data/pesticide_exposure.csv')


pur_details_fill %>% filter(METRIC == 'total pesticides') %>%
  arrange(desc(SCORE)) %>%
  ggplot(aes(SCORE, CODE_NAME)) + geom_col(position = 'dodge')

pur_details_fill %>% filter(METRIC != 'total pesticides') %>%
  ggplot(aes(SCORE, CODE_NAME, fill = METRIC)) + geom_col(position = 'dodge')
# by concern, highest use rates among ROW, ORCHARD_DECIDUOUS, RICE, VINEYARD

pur_details_subclass %>%
  filter(METRIC != 'total pesticides' &
           CODE_NAME %in% c('ORCHARD_DECIDUOUS', 'VINEYARD', 'ORCHARD_CITRUS&SUBTROPICAL', 'ROW')) %>%
  arrange(desc(SCORE)) %>%
  ggplot(aes(SCORE, SUBCLASS, fill = METRIC)) +
  geom_col(position = 'dodge')
# within these classes, highest rates are for potato/sweet potato, pears, apples, vineyard

