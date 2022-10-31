# AVIAN CONSERVATION SCORE

# including expert opinion rankings for relative contribution to habitat for
# different taxonomic groups (especially to allow addressing those groups not
# covered by spatial distribution models)

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')

key = readxl::read_excel('GIS/VEG_key.xlsx')

# compile raw data--------
acs_raw <- read_csv('data_orig/Peterson_et_al_2020/Avian_conservation_scores.csv',
                        col_types = cols()) %>%
  pivot_longer(Grassland_landbirds:Breeding_waterbirds,
               names_to = 'TAXA', values_to = 'value')

# classify landcovers------
acs_classify = acs_raw %>%
  select(LANDCOVER, TAXA, value) %>%
  mutate(
    CODE_NAME = case_when(
      LANDCOVER == 'orchard crop' ~ 'ORCHARD_DECIDUOUS',
      LANDCOVER == 'citrus' ~ 'ORCHARD_CITRUS&SUBTROPICAL',
      LANDCOVER == 'cotton' ~ 'FIELD_OTHER', #not very relevant to Delta?
      LANDCOVER == 'corn' ~ 'FIELD_CORN',
      LANDCOVER == 'tomato' ~ 'ROW',
      LANDCOVER == 'cereal' ~ 'GRAIN&HAY', #including wheat
      LANDCOVER == 'pasture' ~ 'PASTURE_OTHER',
      LANDCOVER == 'alfalfa' ~ 'PASTURE_ALFALFA',
      LANDCOVER == 'wetland' ~ 'WETLAND_MANAGED',
      TRUE ~ toupper(LANDCOVER))
    )

# check for missing:
acs_classify %>% select(TAXA, CODE_NAME, value) %>%
  pivot_wider(names_from = TAXA, values_from = value) %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = key %>% pull(CODE_NAME))) %>%
  complete(CODE_NAME) %>% print(n = 40)

# fill in missing------
# treat grain&hay as applying to wheat and other grain&hay
# treat wetland_other as having same value as grassland
# assume idle, barren, urban have no value for any
# treat woodland&scrub as having some value for oak woodland, riparian, and grassland birds but not waterbirds
# treat riparian & wetland subclasses and equivalent to overall scores except
#  for RIPARIAN_SCRUB_INTRO

acs_ripsubclass = acs_classify %>% filter(CODE_NAME == 'RIPARIAN') %>% select(-CODE_NAME) %>%
  expand_grid(tibble(CODE_NAME = key$CODE_NAME[grepl('RIPARIAN_', key$CODE_NAME)])) %>%
  mutate(value = case_when(
    grepl('SCRUB', CODE_NAME) &
      TAXA %in% c('Oaksavannah_landbirds', 'Wintering_waterfowl',
                  'Wintering_waterbirds', 'Breeding_waterbirds') ~ 1,
    TAXA == 'Riparian_landbirds' & grepl('SCRUB_INTRO', CODE_NAME) ~ 1,
    TRUE ~ value)) %>%
  filter(TAXA != 'all')
ggplot(acs_ripsubclass, aes(value, CODE_NAME)) + geom_col() + facet_wrap(~TAXA)

acs_woodland_scrub = expand_grid(
  CODE_NAME = c('WOODLAND', 'SCRUB'),
  TAXA = unique(acs_classify$TAXA)) %>%
  filter(TAXA != 'all') %>%
  mutate(value = c(1, 3, 2, 0, 0, 0, 0, 0, 0,
                   1, 1, 1, 0, 0, 0, 0, 0, 0))

acs_fill = acs_classify %>%
  filter(CODE_NAME != 'GRAIN&HAY') %>%
  bind_rows(
    acs_classify %>% filter(CODE_NAME == 'GRAIN&HAY') %>% select(-CODE_NAME) %>%
      expand_grid(tibble(CODE_NAME = c('GRAIN&HAY_WHEAT', 'GRAIN&HAY_OTHER'))),
    acs_classify %>% filter(CODE_NAME == 'WETLAND_MANAGED') %>% select(-CODE_NAME) %>%
      expand_grid(tibble(
        CODE_NAME = c('WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL'))),
    acs_classify %>% filter(CODE_NAME == 'PASTURE_OTHER') %>% select(-LANDCOVER) %>%
      mutate(CODE_NAME = 'WETLAND_OTHER'),
    acs_classify %>% filter(CODE_NAME == 'GRASSLAND') %>%
      select(-CODE_NAME, -LANDCOVER) %>% mutate(value = 0) %>%
      expand_grid(tibble(CODE_NAME = c('BARREN', 'IDLE', 'URBAN'))),
    acs_woodland_scrub,
    acs_ripsubclass)

# check for missing:
acs_fill %>% select(TAXA, CODE_NAME, value) %>%
  pivot_wider(names_from = TAXA, values_from = value) %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = key %>% pull(CODE_NAME))) %>%
  complete(CODE_NAME) %>% print(n = 40)

# explore plot
ggplot(acs_fill, aes(value, CODE_NAME)) + geom_col() + facet_wrap(~TAXA)
ggplot(acs_fill, aes(value, TAXA)) + geom_col() + facet_wrap(~CODE_NAME)
# Note: scores for "all" taxa are only for the taxa considered here (so don't
# incorporate, e.g. raptors, tidal marsh spp, etc.)

# add combined scores-----
#
# # add total score-----
# # from original effort, including all taxa, rescaled 0-1
# acs_raw %>% select(LANDCOVER, value = Rescaled) %>% distinct()
#
# acs_sum = bind_rows(
#   acs_fill %>% select(TAXA, CODE_NAME, SCORE_MEAN = value) %>%
#     mutate(SCORE_SE = NA, SCORE_MIN = SCORE_MEAN, SCORE_MAX = SCORE_MEAN),
#   # overall
#   acs_fill %>%
#     mutate(group = if_else(grepl('_landbirds', TAXA), 'double', 'single'),
#            value = if_else(group == 'double', value * 2, value)) %>%
#     group_by(CODE_NAME) %>%
#     summarize(SCORE_MEAN = mean(value),
#               SCORE_SE = sd(value)/sqrt(length(!is.na(value))),
#               SCORE_MIN = min(value),
#               SCORE_MAX = max(value),
#               .groups = 'drop') %>%
#     mutate(TAXA = 'all'),
#   # larger subsets of taxa
#   acs_fill %>%
#     mutate(group = case_when(
#       TAXA %in% c('Breeding_shorebirds', 'Breeding_waterbirds',
#                   'Breeding_waterfowl') ~ 'breeding waterbirds (all)',
#       TAXA %in% c('Grassland_landbirds', 'Oaksavannah_landbirds',
#                   'Riparian_landbirds') ~ 'breeding landbirds (all)',
#       TAXA %in% c('Wintering_shorebirds', 'Wintering_waterbirds',
#                   'Wintering_waterfowl') ~ 'wintering waterbirds (all)')) %>%
#     group_by(group, CODE_NAME) %>%
#     summarize(SCORE_MEAN = mean(value),
#               SCORE_SE = sd(value)/sqrt(length(!is.na(value))),
#               SCORE_MIN = min(value),
#               SCORE_MAX = max(value),
#               .groups = 'drop') %>%
#     rename(TAXA = group)
#   )
#
# # explore plot
# acs_sum %>%
#   filter(TAXA %in% c('all', 'breeding waterbirds (all)',
#                      'breeding landbirds (all)', 'wintering waterbirds (all)')) %>%
#   arrange(TAXA, desc(SCORE_MEAN)) %>%
#   mutate(CODE_NAME = factor(CODE_NAME, levels = .$CODE_NAME[1:30])) %>%
#   ggplot(aes(SCORE_MEAN, CODE_NAME, xmin = SCORE_MEAN - SCORE_SE,
#              xmax = SCORE_MEAN + SCORE_SE)) +
#   geom_col(position = 'dodge') + geom_errorbar() +
#   facet_wrap(~TAXA)


# finalize--------

acs_final = acs_fill %>%
  select(TAXA, CODE_NAME, SCORE_MEAN = value) %>%
  # convert to 0-10 scale (to better distinguish between primary and marginal
  # habitat)
  mutate(SCORE_MEAN = case_when(SCORE_MEAN == 3 ~ 10,
                                SCORE_MEAN == 2 ~ 5,
                                TRUE ~ SCORE_MEAN),
         SCORE_SE = NA, SCORE_MIN = NA, SCORE_MAX = NA,
         METRIC_CATEGORY = 'biodiversity',
         METRIC_SUBTYPE = 'avian conservation contribution',
         UNIT = 'ranking') %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC = TAXA, CODE_NAME,
         SCORE_MEAN, SCORE_SE, SCORE_MIN, SCORE_MAX, UNIT) %>%
  # update labels to reflect larger groupings and taxonomic group names (can be
  # easily split at the _)
  mutate(METRIC = recode(METRIC,
                         Grassland_landbirds = 'Breeding Landbirds_Grassland',
                         Oaksavannah_landbirds = 'Breeding Landbirds_Oak Savannah',
                         Riparian_landbirds = 'Breeding Landbirds_Riparian',
                         Wintering_waterfowl = 'Wintering Waterbirds_Waterfowl',
                         Wintering_shorebirds = 'Wintering Waterbirds_Shorebirds',
                         Wintering_waterbirds = 'Wintering Waterbirds_Other Waterbirds',
                         Breeding_waterfowl = 'Breeding Waterbirds_Waterfowl',
                         Breeding_shorebirds = 'Breeding Waterbirds_Shorebirds',
                         Breeding_waterbirds = 'Breeding Waterbirds_Other Waterbirds'))

write_csv(acs_final, 'data/avian_conservation_score.csv')
