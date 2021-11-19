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

# add total score (from original effort, including all taxa, rescaled 0-1)
acs_overall = acs_raw %>% select(LANDCOVER, value = Rescaled) %>% distinct() %>%
  mutate(TAXA = 'all')

acs_compiled = bind_rows(acs_raw, acs_overall)

# classify landcovers------
acs_classify = acs_compiled %>%
  select(LANDCOVER, TAXA, value) %>%
  mutate(
    CODE_NAME = case_when(
      LANDCOVER == 'orchard crop' ~ 'ORCHARD_DECIDUOUS',
      LANDCOVER == 'citrus' ~ 'ORCHARD_CITRUS&SUBTROPICAL',
      LANDCOVER == 'cotton' ~ 'FIELD', #not very relevant to Delta?
      LANDCOVER == 'corn' ~ 'FIELD_CORN',
      LANDCOVER == 'tomato' ~ 'ROW',
      LANDCOVER == 'cereal' ~ 'GRAIN',
      LANDCOVER == 'pasture' ~ 'PASTURE',
      LANDCOVER == 'alfalfa' ~ 'PASTURE_ALFALFA',
      TRUE ~ toupper(LANDCOVER))
    )

# filter-------
# include only breeding shorebirds, waterbirds, and waterfowl (not included in
# waterbird spatial distribution modeling), plus oak savannah and grassland
# landbirds

acs_filter = acs_classify %>%
  filter(TAXA %in%
           c('Breeding_shorebirds', 'Breeding_waterbirds', 'Breeding_waterfowl',
             'Grassland_landbirds', 'Oaksavannah_landbirds'))

# explore plot
acs_filter %>%
  ggplot(aes(value, CODE_NAME)) +
  geom_col(aes(fill = value), color = 'black') +
  facet_wrap(~TAXA)

# add combined scores-----
acs_sum = bind_rows(
  acs_filter,
  # all included taxa
  acs_filter %>%
    group_by(CODE_NAME, LANDCOVER) %>%
    summarize(value = sum(value), .groups = 'drop') %>%
    mutate(TAXA = 'ALL TAXA',
           #rescale from max possible score (15) to max of 3
           value = value / (15/3)),
  # breeding waterbirds
  acs_filter %>%
    filter(TAXA %in%
             c('Breeding_shorebirds', 'Breeding_waterbirds',
               'Breeding_waterfowl')) %>%
    group_by(CODE_NAME, LANDCOVER) %>%
    summarize(value = sum(value), .groups = 'drop') %>%
    mutate(TAXA = 'All breeding waterbirds',
           value = value / (9/3)),
  # breeding landbirds
  acs_filter %>%
    filter(TAXA %in% c('Grassland_landbirds', 'Oaksavannah_landbirds')) %>%
    group_by(CODE_NAME, LANDCOVER) %>%
    summarize(value = sum(value), .groups = 'drop') %>%
    mutate(TAXA = 'All breeding landbirds',
           value = value / (6/3))
  )

# finalize--------
acs_final = acs_sum %>%
  mutate(METRIC_CATEGORY = 'biodiversity',
         METRIC_SUBTYPE = 'avian conservation contribution',
         UNIT = 'ranking') %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC = TAXA, CODE_NAME,
         SCORE = value, UNIT)

write_csv(acs_final, 'data/multiplebenefits/avian_conservation_score.csv')

# explore plot
acs_final %>%
  mutate(CODE_NAME = factor(CODE_NAME,
                            levels = acs_final %>%
                              filter(METRIC == 'ALL TAXA') %>%
                              arrange(SCORE) %>% pull(CODE_NAME))) %>%
  ggplot(aes(SCORE, CODE_NAME)) +
  geom_col(aes(fill = SCORE), color = 'black') +
  facet_wrap(~METRIC)
