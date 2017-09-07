## ----init, message=FALSE, echo=FALSE-------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

library(hbgd)

## ----gen-trajectory, echo=FALSE, results='hide'--------------------------
smc <- get_smocc_data()[1:500,]
smc_haz_wandfit <- get_fit(smc, y_var = "haz", method = "wand")
smc_tr <- fit_all_trajectories(smc, smc_haz_wandfit)

## ----gen-velocity--------------------------------------------------------
traj_velocity <- get_traj_velocity(all_traj = smc_tr)

## ----inspect-velocity----------------------------------------------------
print(traj_velocity)

