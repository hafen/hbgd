

context("working with data")

expect_class <- function(x, class) {
  expect_true(inherits(x, class)) # nolint
}

expect_data_frame <- function(x, colnames) {
  expect_class(x, "data.frame") # nolint
  expect_equivalent(colnames(x), colnames)
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
    time = time_male
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

  # expect_zscore_centile_fn(
  #   fn_to_centile = who_htcm2centile,
  #   fn_to_zscore = who_htcm2zscore,
  #   centile_to_fn = who_centile2htcm,
  #   zscore_to_fn = who_zscore2htcm,
  #   time = seq(0, 365, by = 7)
  # )

})
