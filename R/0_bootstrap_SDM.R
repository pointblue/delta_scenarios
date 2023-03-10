bootstrap_SDM = function(modlist, n, predictors,
                         constants = NULL, factors = NULL, unsuitable = NULL,
                         stat = NULL, rollup = TRUE) {
  # for each sample n: estimate total amount of habitat predicted on the landscape for each scenario
  purrr::map_df(
    n %>% setNames(n),
    function(n) {
      print(paste0('sample = ', n))

      # generate new dataset & model for each species
      newmods = purrr::map(
        names(modlist) %>% setNames(names(modlist)),
        function(x) {
          # create list of resamples
          df = modlist[[x]]$gbm.call$dataframe
          s = sample(c(1:nrow(df)), size = nrow(df), replace = TRUE)
          dismo::gbm.fixed(
            data = df[s,],
            gbm.x = modlist[[x]]$gbm.call$gbm.x,
            gbm.y = modlist[[x]]$gbm.call$gbm.y,
            tree.complexity = modlist[[x]]$interaction.depth,
            site.weights = modlist[[x]]$weights[s],
            learning.rate = modlist[[x]]$shrinkage,
            n.trees = modlist[[x]]$n.trees,
            bag.fraction = modlist[[x]]$bag.fraction,
            family = 'bernoulli')
        })

      if (!is.null(stat)) {
        # find thresholds for each new model
        thresholds = purrr::map(
          names(newmods) %>% setNames(names(newmods)),
          function(x) {
            obs = newmods[[x]]$gbm.call$data[newmods[[x]]$gbm.call$gbm.y]
            presence = newmods[[x]]$fitted[obs == 1]
            absence = newmods[[x]]$fitted[obs == 0]
            e = dismo::evaluate(presence, absence)
            t = dismo::threshold(e, stat)
          })}

      # for each landscape scenario:
      purrr::map_df(
        names(predictors) %>% setNames(names(predictors)),
        function(s) {
          print(paste0("predicting for: ", s))
          # make predictions from each new species model
          newrast = purrr::map(
            names(newmods) %>% setNames(names(newmods)),
            ~terra::predict(
              model = newmods[[.x]],
              object = terra::subset(
                x = predictors[[s]],
                subset = newmods[[.x]]$contributions %>%
                  dplyr::filter(!var %in% names(constants)) %>%
                  dplyr::pull(var)),
              n.trees = newmods[[.x]]$n.trees,
              na.rm = TRUE,
              type = 'response',
              const = constants,
              factors = factors)) %>% terra::rast()
          if (!is.null(unsuitable)) {
            newrast = terra::cover(x = unsuitable, y = newrast)
            names(newrast) = names(newmods)
          }
          # find thresholds and convert to presence/absence
          if (!is.null(stat)) {
            newrast = purrr::map(
              names(newmods) %>% setNames(names(newmods)),
              ~terra::classify(
                newrast[[.x]],
                rcl = matrix(c(-Inf, thresholds[[.x]], 0,
                               thresholds[[.x]], Inf, 1),
                             nrow = 2, byrow = TRUE))) %>%
              terra::rast()
          }
          # sum habitat
          totals = purrr::map_df(
            names(newrast) %>% setNames(names(newrast)),
            ~terra::values(newrast[[.x]]) %>%
              sum(na.rm = TRUE) %>%
              dplyr::as_tibble(),
            .id = 'spp')

          if (rollup) {
            # first find max value across all rasters, then sum
            totals = dplyr::bind_rows(
              totals,
              max(newrast, na.rm = TRUE) %>%
                terra::values() %>% sum(na.rm = TRUE) %>%
                dplyr::as_tibble() %>%
                dplyr::mutate(spp = 'TOTAL'))
          }
          return(totals)
        },
        .id = 'scenario')
    },
    .id = 'n')
}

bootstrap_SDM2 = function(modlist, n, predictors,
                         constants = NULL, factors = NULL, unsuitable = NULL,
                         stat = NULL, rollup = TRUE) {
  # for each sample n: estimate total amount of habitat predicted on the landscape for each scenario
  purrr::map_df(
    n %>% setNames(n),
    function(n) {
      print(paste0('sample = ', n))

      # generate new dataset & model for each species
      newmods = purrr::map(
        names(modlist) %>% setNames(names(modlist)),
        function(x) {
          # create list of resamples
          df = modlist[[x]]$gbm.call$dataframe
          s = sample(c(1:nrow(df)), size = nrow(df), replace = TRUE)
          dismo::gbm.fixed(
            data = df[s,],
            gbm.x = modlist[[x]]$gbm.call$gbm.x,
            gbm.y = modlist[[x]]$gbm.call$gbm.y,
            tree.complexity = modlist[[x]]$interaction.depth,
            site.weights = modlist[[x]]$weights[s],
            learning.rate = modlist[[x]]$shrinkage,
            n.trees = modlist[[x]]$n.trees,
            bag.fraction = modlist[[x]]$bag.fraction,
            family = 'bernoulli')
        })

      if (!is.null(stat)) {
        # find thresholds for each new model
        thresholds = purrr::map(
          names(newmods) %>% setNames(names(newmods)),
          function(x) {
            obs = newmods[[x]]$gbm.call$data[newmods[[x]]$gbm.call$gbm.y]
            presence = newmods[[x]]$fitted[obs == 1]
            absence = newmods[[x]]$fitted[obs == 0]
            e = dismo::evaluate(presence, absence)
            t = dismo::threshold(e, stat)
          })}

      # for each landscape scenario:
      purrr::map_df(
        names(predictors) %>% setNames(names(predictors)),
        function(s) {
          print(paste0("predicting for: ", s))
          # make predictions from each new species model
          newrast = purrr::map(
            names(newmods) %>% setNames(names(newmods)),
            ~terra::predict(
              model = newmods[[.x]],
              object = terra::subset(
                x = predictors[[s]],
                subset = newmods[[.x]]$contributions %>%
                  dplyr::filter(!var %in% names(constants[[1]])) %>%
                  dplyr::pull(var)),
              n.trees = newmods[[.x]]$n.trees,
              na.rm = TRUE,
              type = 'response',
              const = constants[[.x]],
              factors = factors)) %>% terra::rast()
          if (!is.null(unsuitable)) {
            newrast = terra::cover(x = unsuitable, y = newrast)
            names(newrast) = names(newmods)
          }
          # find thresholds and convert to presence/absence
          if (!is.null(stat)) {
            newrast = purrr::map(
              names(newmods) %>% setNames(names(newmods)),
              ~terra::classify(
                newrast[[.x]],
                rcl = matrix(c(-Inf, thresholds[[.x]], 0,
                               thresholds[[.x]], Inf, 1),
                             nrow = 2, byrow = TRUE))) %>%
              terra::rast()
          }
          # sum habitat
          totals = purrr::map_df(
            names(newrast) %>% setNames(names(newrast)),
            ~terra::values(newrast[[.x]]) %>%
              sum(na.rm = TRUE) %>%
              dplyr::as_tibble(),
            .id = 'spp')

          if (rollup) {
            # first find max value across all rasters, then sum
            totals = dplyr::bind_rows(
              totals,
              max(newrast, na.rm = TRUE) %>%
                terra::values() %>% sum(na.rm = TRUE) %>%
                dplyr::as_tibble() %>%
                dplyr::mutate(spp = 'TOTAL'))
          }
          return(totals)
        },
        .id = 'scenario')
    },
    .id = 'n')
}
