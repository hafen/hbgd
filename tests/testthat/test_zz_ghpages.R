# nolint start

(function(){
  # init
  # only cache per day
  if (file.exists("test_cache")) {
    creation_date <- format.Date(file.info("test_cache")$ctime, "%Y-%m-%d")
    if (!identical(creation_date, Sys.Date())) {
      unlink("test_cache", recursive = TRUE)
    }
  }

  dir.create("test_cache", recursive = TRUE, showWarnings = FALSE)
})()

get_cached_data <- function(name, fn) {
  file_name <- file.path("test_cache", paste(name, ".Rda", sep = ""))
  if (!file.exists(file_name)) {
    result <- fn()
    save(result, file = file_name)
  }
  env <- environment()
  load(file_name, envir = env)
  get("result", env)
}

# delete_cached_data <- function(name) {
#   unlink(file.path("test_cache", paste(name, ".Rda", sep = "")))
# }


facefit_fn <- function() {
  get_cached_data("facefit_obj", function() {
    smc <- get_smocc_data()[1:200, ]

    get_fit(smc, y_var = "haz", method = "face")
  })
}

smc_tr_fn <- function() {
  get_cached_data("smc_tr", function() {
    smc <- get_smocc_data()[1:200, ]
    facefit <- facefit_fn() # nolint
    fit_all_trajectories(smc, facefit)
  })
}

expect_class <- function(x, class) {
  expect_true(inherits(x, class)) # nolint
}

expect_data_frame <- function(x, colnames) {
  expect_class(x, "data.frame") # nolint
  expect_equivalent(colnames(x), colnames) # nolint
}

expect_silent_plot <- function(p) {
  pdf(file = NULL)
  expect_silent({
    print(p)
  })
  dev.off()
}
expect_warning_plot <- function(p, warning) {
  pdf(file = NULL)
  expect_warning({
    print(p)
  }, warning) # nolint
  dev.off()
}

expect_fit_plot <- function(p) {
  expect_rbokeh(p, c(
    "Patches", "Patches", "Patches", "Line", "Circle",
    "Line", "Circle", "CircleX"
  ))
}
expect_fit_zplot <- function(p) {
  expect_rbokeh(p, c(
    "Patches", "Patches", "Patches", "Patches", "Line",
    "Circle", "Line", "Circle", "CircleX"
  ))
}

expect_rbokeh <- function(p, layer_types) {

  p$x$spec$layers %>%
    lapply("[[", "glyph_ids") %>%
    lapply("[[", 1) %>%
    unlist() %>% unname() %>%
    p$x$spec$model[.] %>%
    lapply(function(item) {
      item$attributes$glyph$type
    }) %>%
    unlist() %>% unname() ->
  actual_layer_types

  expect_equivalent(layer_types, actual_layer_types) # nolint

  # json_output <- capture.output({
  #   print_model_json(p)
  # })
  # expect_class(json_output, "character") # nolint
  # expect_equivalent(json_output[4], "    \"title\": \"Bokeh Figure\",") # nolint
  # expect_true(length(json_output) > 500) # nolint
}

expect_rbokeh_plot_matrix <- function(pm, type_list, ncol, check_data = NULL) {

  last <- function(x) {
    x[[length(x)]]
  }

  expect_class(pm$x$spec, "BokehGridPlot") # nolint

  if (!is.null(check_data)) {
    check_data_cols <- colnames(check_data)
    num_check_data_cols <- length(check_data_cols)

    expect_equivalent(length(pm$x$spec$plot_refs), ceiling(num_check_data_cols / ncol)) # nolint

    expect_true(
      ! is.null(
        last(pm$x$spec$plot_refs)[
          last(rep(1:ncol, length.out = num_check_data_cols))
        ]
      )
    )
  }


  pm$x$spec$plot_refs %>%
    unlist(recursive = FALSE) %>%
    lapply("[[", "id") %>%
    unlist() %>% unname() ->
  plot_ids

  seq_along(plot_ids) %>%
    lapply(function(i) {
      p <- pm$x$spec$figs[[plot_ids[i]]]
      expect_rbokeh(p, type_list[[i]]) # nolint

      if (!is.null(check_data)) {
        expect_equivalent(p$x$spec$model$title$attributes$text, check_data_cols[i]) # nolint
      }
    })
}




context("working with data")

test_that("data", {
  expect_data_frame(cpp, c(
    "subjid", "agedays", "wtkg", "htcm", "lencm", "bmi",
    "waz", "haz", "whz", "baz", "siteid", "sexn",
    "sex", "feedingn", "feeding", "gagebrth", "birthwt", "birthlen",
    "apgar1", "apgar5", "mage", "mracen", "mrace", "mmaritn",
    "mmarit", "meducyrs", "sesn", "ses", "parity", "gravida",
    "smoked", "mcignum", "preeclmp", "comprisk", "geniq", "sysbp",
    "diabp"
  ))

  vv <- view_variables()
  expect_class(vv, "datatables")
  expect_class(vv, "htmlwidget")
  expect_equivalent(attr(vv, "package"), "DT")
})

test_that("dealing with longitudinal data", {
  sub_dt <- get_subject_data(cpp)
  expect_data_frame(sub_dt, c(
    "subjid", "siteid", "sexn", "sex", "feedingn", "feeding",
    "gagebrth", "birthwt", "birthlen", "apgar1", "apgar5", "mage",
    "mracen", "mrace", "mmaritn", "mmarit", "meducyrs", "sesn",
    "ses", "parity", "gravida", "smoked", "mcignum", "preeclmp",
    "comprisk"
  ))
  expect_equivalent(nrow(sub_dt), 500)
  expect_class(attr(sub_dt, "hbgd"), "list")

  time_dt <- get_time_data(cpp)

  expect_data_frame(time_dt, c(
    "agedays", "wtkg", "htcm", "lencm", "bmi", "waz", "haz",
    "whz", "baz", "geniq", "sysbp", "diabp"
  ))
  expect_equivalent(nrow(time_dt), 1912)
  expect_class(attr(time_dt, "hbgd"), "list")
})


test_that("meta data", {
  cpp2 <- get_data_attributes(cpp)

  sub_dt <- get_subject_data(cpp)
  time_dt <- get_time_data(cpp)


  expect_equivalent(cpp, cpp2[, 1:ncol(cpp)])

  cpp2_hbgd <- attr(cpp2, "hbgd")
  expect_class(cpp2_hbgd, "list")
  expect_equivalent(names(cpp2_hbgd), c(
    "labels", "subjectlevel_vars", "longi_vars",
    "time_vars", "var_summ", "subj_count",
    "n_subj", "ad_tab"
  ))
  expect_equivalent(names(cpp2_hbgd$labels), colnames(cpp))
  expect_equivalent(c("subjid", cpp2_hbgd$subjectlevel_vars), colnames(sub_dt))
  expect_equivalent(c(cpp2_hbgd$time_vars, cpp2_hbgd$longi_vars), colnames(time_dt))
  expect_equivalent(cpp2_hbgd$time_vars, "agedays")
  expect_data_frame(cpp2_hbgd$var_summ, c("variable", "label", "type", "vtype", "n_unique"))
  expect_equivalent(nrow(cpp2_hbgd$var_summ), ncol(cpp))
  expect_data_frame(cpp2_hbgd$subj_count, c("subjid", "n"))
  expect_equivalent(
    nrow(cpp2_hbgd$subj_count),
    subset(cpp2_hbgd$var_summ, type == "subject id", n_unique)[[1]]
  )
  expect_equivalent(nrow(cpp2_hbgd$subj_count), cpp2_hbgd$n_subj)
  expect_data_frame(cpp2_hbgd$ad_tab, c("agedays", "n"))
  expect_true(inherits(cpp2_hbgd$ad_tab, "ad_tab"))
  expect_equivalent(nrow(cpp2_hbgd$ad_tab), 5)


  cpp3 <- fix_height(cpp)
})


context("Simple Exploration")

test_that("Data distributions", {

  expect_rbokeh_plot_matrix(
    plot_univar(cpp, subject = TRUE, width = 250, height = 250, ncol = 3),
    rep("Quad", 25),
    3,
    get_subject_data(cpp)
  )
  expect_rbokeh_plot_matrix(
    plot_univar(cpp, subject = FALSE, width = 250, height = 250, ncol = 4),
    rep("Quad", 12),
    4,
    get_time_data(cpp)
  )
})


test_that("Missing data", {
  p <- plot_missing(cpp, subject = TRUE)
  expect_rbokeh(p, "Quad")

  p <- plot_missing(cpp, subject = FALSE)
  expect_rbokeh(p, "Quad")

  p <- plot_complete_pairs(cpp, subject = TRUE)
  expect_rbokeh(p, "Rect")

  p <- plot_complete_pairs(cpp, subject = FALSE)
  expect_rbokeh(p, "Rect")
})

test_that("special plots", {
  expect_rbokeh_plot_matrix(
    plot_visit_distn(cpp, width = 350, height = 350),
    c("Quad", "Circle"),
    2
  )

  expect_rbokeh_plot_matrix(
    plot_first_visit_age(cpp, width = 350, height = 350),
    c("Quad", "Circle"),
    2
  )


  agefreq <- get_agefreq(cpp)
  p <- plot_agefreq(agefreq)
  expect_rbokeh(p, "Line")

})


context("Modeling Methods")

test_that("fitting a model to a data set", {
  smc <- get_smocc_data()[1:200, ]

  for (meth in get_avail_methods()) {
    y_var <- switch(meth,
      "sitar" = "htcm",
      "haz"
      )

    fit_obj <- get_cached_data(
      paste("fit", meth, sep = "_"),
      function() {
        get_fit(smc, y_var = y_var, method = meth)
      }
    )

    expect_output(
      print(fit_obj),
      paste("Object obtained from get_fit\\(\\) using method '", meth, "'", sep = "")
    )
  }

  facefit <- facefit_fn()

  fit <- get_cached_data("fit_obj", function() {
    fit_trajectory(subset(smc, subjid == 10001), facefit)
  })

  expect_fit_plot(plot(fit, x_range = c(0, 755), hover = c("agedays", "htcm")))
  expect_fit_plot(plot(fit, x_range = c(0, 755), hover = c("agedays", "htcm"), center = TRUE))

  expect_fit_zplot(plot_z(fit, hover = c("agedays", "wtkg")))
  expect_rbokeh(plot_velocity(fit), c("Line"))
  expect_rbokeh(plot_zvelocity(fit), c("Line"))
})

test_that("fit all trajectories", {
  facefit <- facefit_fn()
  smc_tr <- smc_tr_fn()

  expect_fit_plot(
    plot(smc_tr$fit[[1]], hover = c("agedays", "htcm"))
  )

  expect_fit_zplot(
    plot_z(smc_tr$fit[smc_tr$subjid == 10002][[1]], hover = c("agedays", "htcm"))
  )
})

test_that("Assessing fits with holdouts", {
  smc <- get_smocc_data()[1:200, ]

  set.seed(1234)
  smc2 <- add_holdout_ind(smc)

  mse_ans <- list(
    "brokenstick" = 0.21692451,
    "face" = 0.191857,
    "fda" = NA,
    "gam" = 0.24876602,
    "loess" = NA,
    "lwmod" = 0.18871031,
    "rlm" = 0.23332911,
    "sitar" = NA,
    "smooth.spline" = 0.31762098,
    "wand" = 0.17557084
  )

  for (meth in get_avail_methods()) {
    if (meth %in% c("fda", "loess", "sitar")) {
      next
    }
    y_var <- switch(meth,
      "sitar" = "htcm",
      "haz"
    )

    smc_tr_mse <- get_cached_data(
      paste("holdout", meth, sep = "_"),
      function() {
        fit_hold <- get_fit(
          smc2,
          y_var = y_var,
          method = meth,
          holdout = TRUE
        )
        smc_tr_hold <- fit_all_trajectories(smc2, fit_hold)
        get_fit_holdout_mse(smc_tr_hold)
      }
    )

    expect_equal(
      smc_tr_mse,
      mse_ans[[meth]],
      tolerance = 0.00002
    )
  }
})

context("Trelliscope")

test_that("built-in methods", {

  library(dplyr)
  library(trelliscopejs)

  smc_tr <- smc_tr_fn() %>%
    add_all_cogs()

  smc_tr %>%
    add_trajectory_plot(center = TRUE) %>%
    trelliscope(name = "test1", thumb = FALSE)

  smc_tr %>%
    add_trajectory_plot(z = TRUE) %>%
    trelliscope(name = "test1", thumb = FALSE)

  smc_tr %>%
    add_velocity_plot() %>%
    trelliscope(name = "test1", thumb = FALSE)

  smc_tr %>%
    add_velocity_plot(z = TRUE) %>%
    trelliscope(name = "test1", thumb = FALSE)

  expect_true(TRUE)
  # trscope_trajectories(smc_tr)
})

context("Division Methods")

context("Multi-Dataset Vis")

test_that("multi dataset vis", {
  dat_list <- list(cpp = cpp, smc = get_smocc_data()[1:2000, ])

  expect_rbokeh(plot_var_matrix(dat_list), c("Rect"))

  expect_rbokeh_plot_matrix(
    plot_time_count_grid(dat_list),
    c("Quad", "Quad"),
    1
  )

  expect_rbokeh(
    plot_multi_subj_boxplot(dat_list),
    c("Rect", "Segment", "Circle", "Rect", "Segment", "Circle")
  )
})

# nolint end
