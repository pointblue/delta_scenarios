# AVIAN CONSERVATION SCORE

# including expert opinion rankings for relative contribution to habitat for
# different taxonomic groups (especially to allow addressing those groups not
# covered by spatial distribution models)

# PACKAGES & FUNCTIONS
source('R/packages.R')

# compile raw data--------
acs_raw <- read_csv('data_orig/Peterson_et_al_2020/Avian_conservation_scores.csv',
                        col_types = cols()) %>%
  pivot_longer(Grassland_landbirds:Breeding_waterbirds,
               names_to = 'TAXA', values_to = 'value')

# # add total score (from original effort, including all taxa, rescaled 0-1)
# acs_overall = acs_raw %>% select(LANDCOVER, value = Rescaled) %>% distinct() %>%
#   mutate(TAXA = 'all')
#
# acs_compiled = bind_rows(acs_raw, acs_overall)

# classify landcovers------
acs_classify = acs_raw %>%
  select(LANDCOVER, TAXA, value) %>%
  mutate(
    CODE_NAME = case_when(
      LANDCOVER == 'orchard crop' ~ 'ORCHARD_DECIDUOUS',
      LANDCOVER == 'citrus' ~ 'ORCHARD_CITRUS&SUBTROPICAL',
      LANDCOVER == 'cotton' ~ 'FIELD', #not very relevant to Delta?
      LANDCOVER == 'corn' ~ 'FIELD_CORN',
      LANDCOVER == 'tomato' ~ 'ROW',
      LANDCOVER == 'cereal' ~ 'GRAIN&HAY',
      LANDCOVER == 'pasture' ~ 'PASTURE',
      LANDCOVER == 'alfalfa' ~ 'PASTURE_ALFALFA',
      LANDCOVER == 'wetland' ~ 'WETLAND_MANAGED',
      TRUE ~ toupper(LANDCOVER))
    )

# fill in missing------
# treat missing wheat as equivalent to other grain&hay
# treat woodland/scrub as having value for oak woodland, riparian, and grassland birds
# treat wetland_other as having same value as grassland
# assume idle, barren, urban have no value for any?

acs_fill = acs_classify %>%
  bind_rows(
    acs_classify %>%
      filter(CODE_NAME == 'GRAIN&HAY') %>%
      mutate(CODE_NAME = recode(CODE_NAME,
                                'GRAIN&HAY' = 'GRAIN&HAY_WHEAT')),
    acs_classify %>%
      filter(CODE_NAME == 'GRASSLAND') %>%
      mutate(LANDCOVER = 'woodland', CODE_NAME = 'WOODLAND&SCRUB',
             value = case_when(TAXA == 'Oaksavannah_landbirds' ~ 3,
                               TAXA == 'Grassland_landbirds' ~ 1,
                               TAXA == 'Riparian_landbirds' ~ 2,
                               TRUE ~ 0)),
    acs_classify %>%
      filter(CODE_NAME == 'GRASSLAND') %>%
      mutate(LANDCOVER = 'wetland_other', CODE_NAME = 'WETLAND_OTHER'),
    acs_classify %>%
      filter(CODE_NAME == 'GRASSLAND') %>%
      mutate(LANDCOVER = 'fallow', CODE_NAME = 'IDLE', value = 0),
    acs_classify %>%
      filter(CODE_NAME == 'GRASSLAND') %>%
      mutate(LANDCOVER = 'barren', CODE_NAME = 'BARREN', value = 0),
    acs_classify %>%
      filter(CODE_NAME == 'GRASSLAND') %>%
      mutate(LANDCOVER = 'urban', CODE_NAME = 'URBAN', value = 0)
  )

# explore plot
acs_fill %>%
  ggplot(aes(value, CODE_NAME)) +
  geom_col(aes(fill = as.factor(value)), color = 'black') +
  facet_wrap(~TAXA)


# add combined scores-----
acs_sum = bind_rows(
  acs_fill,
  # breeding waterbirds
  acs_fill %>%
    filter(TAXA %in%
             c('Breeding_shorebirds', 'Breeding_waterbirds',
               'Breeding_waterfowl')) %>%
    group_by(CODE_NAME, LANDCOVER) %>%
    summarize(value = sum(value), .groups = 'drop') %>%
    mutate(TAXA = 'breeding waterbirds',
           value = value / (9/3)),
  # breeding landbirds
  acs_fill %>%
    filter(TAXA %in% c('Grassland_landbirds', 'Oaksavannah_landbirds', 'Riparian_landbirds')) %>%
    group_by(CODE_NAME, LANDCOVER) %>%
    summarize(value = sum(value), .groups = 'drop') %>%
    mutate(TAXA = 'breeding landbirds',
           value = value / (9/3)),
  # wintering waterbirds
  acs_fill %>%
    filter(TAXA %in%
             c('Wintering_shorebirds', 'Wintering_waterbirds',
               'Wintering_waterfowl')) %>%
    group_by(CODE_NAME, LANDCOVER) %>%
    summarize(value = sum(value), .groups = 'drop') %>%
    mutate(TAXA = 'wintering waterbirds',
           value = value / (9/3)),
  )

# explore plot
acs_sum %>%
  filter(TAXA %in% c('breeding waterbirds', 'breeding landbirds', 'wintering waterbirds')) %>%
  ggplot(aes(value, CODE_NAME)) +
  geom_col(aes(fill = value), color = 'black') +
  facet_wrap(~TAXA, )

# finalize--------
acs_final = acs_sum %>%
  mutate(METRIC_CATEGORY = 'biodiversity',
         METRIC_SUBTYPE = 'avian conservation contribution',
         UNIT = 'ranking') %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC = TAXA, CODE_NAME,
         SCORE = value, UNIT)

write_csv(acs_final, 'data/avian_conservation_score.csv')

