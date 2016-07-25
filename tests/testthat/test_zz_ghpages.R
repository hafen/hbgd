

get_cached_data <- function(name, fn) {
  file_name <- file.path("test_cache", paste(name, ".Rda", sep = ""))
  if (!file.exists(file_name)) {
    dir.create("test_cache", showWarnings = FALSE)
    result <- fn()
    save(result, file = file_name)
  }
  env <- environment()
  load(file_name, envir = env)
  get("result", env)
}

facefit_fn <- function() {
  get_cached_data("facefit_obj", function() {
    smc <- get_smocc_data()[1:2000, ]

    get_fit(smc, y_var = "haz", method = "face")
  })
}

smc_tr_fn <- function() {
  get_cached_data("smc_tr", function() {
    smc <- get_smocc_data()[1:2000, ]
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
        expect_equivalent(p$x$spec$model$plot$attributes$title, check_data_cols[i]) # nolint
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
    "labels", "subjectlevel_vars", "timevarying_vars",
    "time_vars", "var_summ", "subj_count",
    "n_subj", "ad_tab"
  ))
  expect_equivalent(names(cpp2_hbgd$labels), colnames(cpp))
  expect_equivalent(c("subjid", cpp2_hbgd$subjectlevel_vars), colnames(sub_dt))
  expect_equivalent(c(cpp2_hbgd$time_vars, cpp2_hbgd$timevarying_vars), colnames(time_dt))
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


context("growth standards")

expect_zscore_centile_fn <- function(
  fn_to_zscore,
  fn_to_centile,
  zscore_to_fn,
  centile_to_fn,
  time_male,
  time_female,
  name,
  standard_name
) {

  # print(str_c(standard_name, "_", type))
  expect_true(!is.null(time)) # nolint

  if (standard_name == "igfet") {
    time <- time_male
    for (random_centile in runif(5, min = 0.1, max = 0.9)) {

      maybe_random_centile <- fn_to_centile(time, centile_to_fn(time, random_centile))
      expect_equal(
        random_centile,
        unique(round(maybe_random_centile, 6)),
        tolerance = 0.00002
      )

      random_zvalue <- qnorm(random_centile)
      maybe_random_zvalue <- fn_to_zscore(time, zscore_to_fn(time, random_zvalue))
      expect_equal(
        random_zvalue,
        unique(round(maybe_random_zvalue, 6)),
        tolerance = 0.00002
      )
    }
    return()
  }


  for (sex in c("Female", "Male")) {
    time <- ifelse(sex == "Male", time_male, time_female)
    for (random_centile in runif(5, min = 0.1, max = 0.9)) {

      maybe_random_centile <- fn_to_centile(
        time,
        centile_to_fn(time, random_centile, sex = sex),
        sex = sex
      )

      expect_equal(
        random_centile,
        unique(round(maybe_random_centile, 6)),
        tolerance = 0.00002
      )

      random_zvalue <- qnorm(random_centile)

      maybe_random_zvalue <- fn_to_zscore(
        time,
        zscore_to_fn(time, random_zvalue, sex = sex),
        sex = sex
      )
      expect_equal(
        random_zvalue,
        unique(round(maybe_random_zvalue, 6)),
        tolerance = 0.00002
      )
    }
  }

}

expect_standard <- function(standard_name, types, coef_data, time) {
  str_c <- function(...) {
    paste(..., sep = "")
  }

  for (type in types) {
    fn_to_centile_name <- str_c(standard_name, "_", type, "2centile")
    fn_to_zscore_name <- str_c(standard_name, "_", type, "2zscore")
    centile_to_fn_name <- str_c(standard_name, "_", "centile2", type)
    zscore_to_fn_name <- str_c(standard_name, "_", "zscore2", type)

    fns <- mget(
      c(fn_to_centile_name, fn_to_zscore_name, centile_to_fn_name, zscore_to_fn_name),
      mode = "function",
      envir = loadNamespace("hbgd") # nolint
    )

    time_val_male <- switch(standard_name,
      "who" = who_coefs[[str_c(type, "_agedays")]][["Male"]]$data$x,
      "igb" = ig_coefs[[type]][["Male"]]$ga,
      "igfet" = seq(from = 14 * 7, to = 40 * 7, by = 1)
    )
    time_val_female <- switch(standard_name,
      "who" = who_coefs[[str_c(type, "_agedays")]][["Female"]]$data$x,
      "igb" = ig_coefs[[type]][["Female"]]$ga,
      "igfet" = seq(from = 14 * 7, to = 40 * 7, by = 1)
    )
    expect_zscore_centile_fn(
      fn_to_centile = fns[[fn_to_centile_name]],
      fn_to_zscore = fns[[fn_to_zscore_name]],
      centile_to_fn = fns[[centile_to_fn_name]],
      zscore_to_fn = fns[[zscore_to_fn_name]],
      time_male = time_val_male,
      time_female = time_val_female,
      name = str_c(standard_name, "_", type),
      standard_name = standard_name
    )

  }


}

test_that("WHO growth standards", {
  expect_standard(
    "who",
    c("bmi", "htcm", "hcircm", "muaccm", "ss", "tsftmm", "wtkg")
  )

  expect_standard(
    "igb",
    c("hcircm", "lencm", "wtkg")
  )
  expect_standard(
    "igfet",
    c("accm", "bpdcm", "flcm", "hccm", "ofdcm")
  )

})

context("standard-who-special")
test_that("who data", {
  for (sex_val in c("Male", "Female")) {
    dat <- data.frame(
      time = who_coefs[["bmi_agedays"]][[sex_val]]$data$x,
      sex = sex_val
    )
    dat$p_val <- runif(nrow(dat))

    dat$cen2value <- who_centile2value(
      x = time,
      p = p_val,
      x_var = "agedays",
      y_var = "bmi",
      sex = sex,
      data = dat
    )

    dat$value2centile <- who_value2centile(
      x = time,
      y = cen2value,
      x_var = "agedays",
      y_var = "bmi",
      sex = sex,
      data = dat
    )
    expect_equivalent(dat$p_val, dat$value2centile)

    dat$val2zscore <- who_value2zscore(
      x = time,
      y = cen2value,
      x_var = "agedays",
      y_var = "bmi",
      sex = sex,
      data = dat
    )
    dat$zscore2value <- who_zscore2value(
      x = time,
      z = val2zscore,
      x_var = "agedays",
      y_var = "bmi",
      sex = sex,
      data = dat
    )
    expect_equivalent(dat$cen2value, dat$zscore2value)
  }
})
test_that("who sex", {
  expect_error({
    who_centile2value(
      x = who_coefs[["bmi_agedays"]][["Male"]]$data$x,
      p = runif(1),
      x_var = "agedays",
      y_var = "bmi",
      sex = "Not human"
    )
  }, "sex must be 'Male' or 'Female'") # nolint
  expect_error({
    who_value2zscore(
      x = who_coefs[["bmi_agedays"]][["Male"]]$data$x,
      y = who_centile2value(
        x = who_coefs[["bmi_agedays"]][["Male"]]$data$x,
        p = runif(1),
        x_var = "agedays",
        y_var = "bmi",
        sex = "Male"
      ),
      x_var = "agedays",
      y_var = "bmi",
      sex = "Not human"
    )
  }, "sex must be 'Male' or 'Female'") # nolint
})


context("plot utilities")

test_that("ggplot2", {
  library(ggplot2)

  expect_layer_type <- function(p, layer_pos, layer_type) {
    expect_true(
      inherits(
        p$layers[[layer_pos]]$geom,
        layer_type
      )
    )
  }

  p <- ggplot(data = subset(cpp, subjid == 8), aes(x = agedays, y = htcm))
  who_p <- geom_who(p, x = seq(0, 2600, by = 10)) + geom_point()

  expect_layer_type(who_p, 1, "GeomPolygon")
  expect_layer_type(who_p, 2, "GeomPath")
  expect_layer_type(who_p, 3, "GeomPolygon")
  expect_layer_type(who_p, 4, "GeomPath")
  expect_layer_type(who_p, 5, "GeomPolygon")
  expect_layer_type(who_p, 6, "GeomPath")
  expect_layer_type(who_p, 7, "GeomPath")
  expect_layer_type(who_p, 8, "GeomPoint")

  expect_warning_plot(who_p, "Removed 1 rows containing missing values")

})


test_that("lattice", {
  library(lattice)

  lat_p <- xyplot(
    wtkg ~ agedays,
    data = subset(cpp, subjid == 8),
    panel = function(x, y, ...) {
      panel.who(
        x = seq(0, 2600, by = 10),
        sex = "Male",
        y_var = "wtkg",
        p = pnorm(-3:0) * 100
      )
      panel.xyplot(x, y, ...)
    },
    ylim = c(0, 39),
    col = "black"
  )

  expect_silent_plot(lat_p)

})


test_that("rbokeh", {
  library(rbokeh)
  library(magrittr)

  figure(xlab = "Age (years)", ylab = "wtkg") %>%
    ly_who(x = seq(0, 2600, by = 10), y_var = "wtkg",
      sex = "Male", x_trans = days2years) %>%
    ly_points(days2years(agedays), wtkg,
      data = subset(cpp, subjid == 8), col = "black",
      hover = c(agedays, wtkg, lencm, htcm, bmi, geniq, sysbp, diabp)) ->
  p

  expect_rbokeh(p, c("Patches", "Patches", "Patches", "Line", "Circle"))


  cppsubj <- get_subject_data(cpp)
  figure(
    xlab = "Gestational Age at Birth (days)",
    ylab = "Birth Length (cm)"
  ) %>%
    ly_igb(gagebrth = 250:310, var = "lencm", sex = "Male") %>%
    ly_points(
      jitter(gagebrth), birthlen,
      data = subset(cppsubj, sex == "Male"),
      color = "black"
    ) ->
  p

  expect_rbokeh(p, c("Patches", "Patches", "Patches", "Line", "Circle"))


  figure(
    xlab = "Gestational Age (days)",
    ylab = "Head Circumference (cm)"
  ) %>%
    ly_igfet(gagedays = 98:280, var = "hccm", p = pnorm(-3:0) * 100) ->
  p

  expect_rbokeh(p, c("Patches", "Patches", "Patches", "Line"))

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
  smc <- get_smocc_data()[1:2000, ]

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
    plot(smc_tr[[1]]$value, hover = c("agedays", "htcm"))
  )

  expect_fit_zplot(
    plot_z(smc_tr[["subjid=10002"]]$value, hover = c("agedays", "htcm"))
  )
})

test_that("Assessing fits with holdouts", {
  smc <- get_smocc_data()[1:2000, ]

  set.seed(1234)
  smc2 <- add_holdout_ind(smc)

  mse_ans <- list(
    "brokenstick" = 0.1798087,
    "face" = 0.1170519,
    "fda" = NA,
    "gam" = 0.3668065,
    "loess" = NA,
    "lwmod" = 0.3159714,
    "rlm" = 0.4613727,
    "sitar" = NA,
    "smooth.spline" = 0.432525,
    "wand" = 0.2637936
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

  smc_tr <- smc_tr_fn()

  vdb_path <- "ghap_vdb"

  test_vdb_conn <- trelliscope::vdbConn(name = "ghap_vdb", path = vdb_path, autoYes = TRUE)

  trscope_trajectories(smc_tr, center = TRUE, vdb_conn = test_vdb_conn)
  trscope_trajectories(smc_tr, z = TRUE, vdb_conn = test_vdb_conn)
  trscope_velocities(smc_tr, vdb_conn = test_vdb_conn)
  trscope_velocities(smc_tr, z = TRUE, vdb_conn = test_vdb_conn)

  unlink(vdb_path, recursive = TRUE)

  expect_true(TRUE)
  # trscope_trajectories(smc_tr)
})


context("Division Methods")

test_that("By Subject", {
  smc_tr <- smc_tr_fn()

  smc_cp <- by_trajectory_checkpoints(smc_tr)

  expect_equivalent(length(datadr::getKeys(smc_cp)), 1)
})


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


context("Misc")

test_that("size conversion", {
  expect_equal(
    who_htcm2zscore(1670, in2cm(44)),
    1.117365,
    tolerance = 0.00002
  )

  expect_equal(
    who_wtkg2zscore(1670, lb2kg(48)),
    1.527048,
    tolerance = 0.00002
  )
})
