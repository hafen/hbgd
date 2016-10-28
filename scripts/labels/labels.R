library(dplyr)
library(readr)

## read in variable / label list
##---------------------------------------------------------
vars <- read_csv("scripts/labels/kikm_variables.csv") %>%
  select(-kikm_uri) %>%
  mutate(variable = tolower(variable))

## look at type consistency across variables
##---------------------------------------------------------
type_fn <- function(study_id, type, misfit = FALSE) {
  tptbl <- table(type)
  ind <- ifelse(misfit, which.min(tptbl), which.max(tptbl))
  paste0(names(tptbl[ind]), " (", tptbl[ind], ")")
}

misfit_studies_fn <- function(study_id, type) {
  tptbl <- table(type)
  nm <- names(which.min(tptbl))
  paste(study_id[type == nm], collapse = ", ")
}

types <- vars %>%
  group_by(variable) %>%
  summarise(
    n_studies = n(),
    nu_type = n_distinct(type),
    prevalent_type = type_fn(study_id, type),
    misfit_type = type_fn(study_id, type, misfit = TRUE),
    misfits = misfit_studies_fn(study_id, type)) %>%
  filter(nu_type > 1 & variable != "studyid") %>%
  select(-nu_type)

types

## get a table of unique variable / name combinations
## if names are not unique for a variable, choose the most common
## if there are ties for most common, choose the shortest one
##---------------------------------------------------------------
most_label_fn <- function(labels) {
  lbltab <- table(labels)
  ind <- which.max(lbltab)
  lbls <- names(lbltab[lbltab == lbltab[ind]])
  ind <- which.min(nchar(lbls, allowNA = TRUE))
  if (length(ind) == 0)
    ind <- 1
  lbls[ind]
}

labels <- vars %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    nu_label = n_distinct(variable_label),
    most = max(table(variable_label)),
    most_label = most_label_fn(variable_label)) %>%
  # filter(nu_label > 1) %>%
  # arrange(-nu_label)
  # arrange(-most)
  arrange(variable)

hbgd_labels <- as.list(labels$most_label)
names(hbgd_labels) <- labels$variable
save(hbgd_labels, file = "data/hbgd_labels.rda")

hbgd_labels_df <- labels %>%
  select(variable, most_label) %>%
  rename(var = variable, label = most_label) %>%
  data.frame()

save(hbgd_labels_df, file = "data/hbgd_labels_df.rda")
