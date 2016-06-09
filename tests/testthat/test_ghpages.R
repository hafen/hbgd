

context("working with data")


expect_class <- function(x, class) {
  expect_true(inherits(x, class)) # nolint
}

expect_data_frame <- function(x, colnames) {
  expect_class(x, "data.frame") # nolint
  expect_equivalent(colnames(x), colnames)
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

  json_output <- capture.output({
    print_model_json(p)
  })
  expect_class(json_output, "character") # nolint
  expect_equivalent(json_output[4], "    \"title\": \"Bokeh Figure\",") # nolint
  expect_true(length(json_output) > 500) # nolint
}


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


  cpp3 <- fix_height(cpp, target = "new_col")
  expect_true("new_col" %in% colnames(cpp3))

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
  expect_true(!is.null(time))

  if (standard_name == "igpre") {
    time <- time_male
    for (random_centile in runif(5, min = 0.1, max = 0.9)) {

      maybe_random_centile <- fn_to_centile(time, centile_to_fn(time, random_centile))
      expect_equivalent(round(random_centile, 6), unique(round(maybe_random_centile, 6)))

      random_zvalue <- qnorm(random_centile)
      maybe_random_zvalue <- fn_to_zscore(time, zscore_to_fn(time, random_zvalue))
      expect_equivalent(round(random_zvalue, 6), unique(round(maybe_random_zvalue, 6)))
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

      expect_equivalent(
        round(random_centile, 6),
        unique(round(maybe_random_centile, 6))
      )

      random_zvalue <- qnorm(random_centile)

      maybe_random_zvalue <- fn_to_zscore(
        time,
        zscore_to_fn(time, random_zvalue, sex = sex),
        sex = sex
      )
      expect_equivalent(
        round(random_zvalue, 6),
        unique(round(maybe_random_zvalue, 6))
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
      "igpre" = seq(from = 14 * 7, to = 40 * 7, by = 1)
    )
    time_val_female <- switch(standard_name,
      "who" = who_coefs[[str_c(type, "_agedays")]][["Female"]]$data$x,
      "igb" = ig_coefs[[type]][["Female"]]$ga,
      "igpre" = seq(from = 14 * 7, to = 40 * 7, by = 1)
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
    "igpre",
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

  expect_warning({
    print(who_p)
  }, "Removed 1 rows containing missing values") # nolint

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

  expect_silent({
    print(lat_p)
  })

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
    ly_igpre(gagedays = 98:280, var = "hccm", p = pnorm(-3:0) * 100) ->
  p

  expect_rbokeh(p, c("Patches", "Patches", "Patches", "Line"))

})


context("Simple Exploration")

test_that("plot univar", {

  expect_rbokeh_plot_matrix <- function(pm, ncol, check_data) {

    last <- function(x) {
      x[[length(x)]]
    }

    expect_class(pm$x$spec, "BokehGridPlot")

    subject_cpp <- get_subject_data(cpp)

    check_data_cols <- colnames(check_data)
    num_check_data_cols <- length(check_data_cols)

    expect_equivalent(length(pm$x$spec$plot_refs), ceiling(num_check_data_cols / ncol))

    expect_true(
      ! is.null(
        last(pm$x$spec$plot_refs)[
          last(rep(1:ncol, length.out = num_check_data_cols))
        ]
      )
    )

    pm$x$spec$plot_refs %>%
      unlist(recursive = FALSE) %>%
      lapply("[[", "id") %>%
      unlist() %>% unname() ->
    plot_ids

    pm$x$spec$figs[plot_ids] %>%
      lapply(expect_rbokeh, c("Quad"))

    seq_along(plot_ids) %>%
      lapply(function(i) {
        p <- pm$x$spec$figs[[plot_ids[i]]]
        expect_equivalent(p$x$spec$model$plot$attributes$title, check_data_cols[i])
      })
  }

  expect_rbokeh_plot_matrix(
    plot_univar(cpp, subject = TRUE, width = 250, height = 250, ncol = 3),
    3,
    get_subject_data(cpp)
  )
  expect_rbokeh_plot_matrix(
    plot_univar(cpp, subject = FALSE, width = 250, height = 250, ncol = 4),
    4,
    get_time_data(cpp)
  )

})
