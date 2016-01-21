Version 0.2
----------------------------------------------------------------------

- Fix `plot_time_count_grid()` dimension issues (0.2.5)
- Update `plot_var_matrix()` to have a `head` argument (0.2.5)
- Update `plot_time_count_grid()` to have a `min_border_left` argument (0.2.5)
- Major speedup in `get_subject_data()` (0.2.4)
- Change default transformation to identity (0.2.3)
- Handle empty subjects more robustly in fitting all trajectories (0.2.3)
- Add nadir cognostics (0.2.3)
- Fix issue with inversion large z-scores (0.2.3)
- Add fda, face, smooth.spline fitting methods  (0.2.3)
- Add trelliscope velocity plots (0.2.3)
- Clean up `fit_trajectory()` to get more consistent output in all cases (0.2.2)
- Fix in `fit_trajectory()` to deal with z-scores that are so large that they break `pnorm()` - truncate z-scores whose absolute value exceeds 8 (0.2.2)
- Add ability to plot nadir guide to `plot_z()` (0.2.2)
- Add nadir to cognostics (0.2.2)
- Add "fda" and "face" fitting methods (0.2.1)
- Add z-scale, velocity, and z-velocity Trelliscope methods (0.2.1)
- Make some of the cognostics methods generic (0.2.1)
- Add Intergrowth birth standard (0.2.0)
- Add Intergrowth prenatal standard (0.2.0)
- Update all standards to have measurement-specific methods (e.g. `who_bmi2zscore()`) (0.2.0)
- Add rbokeh/lattice/ggplot2 utilities for plotting all growth standards (e.g. `ly_igb()`) (0.2.0)
- Add z band and velocity plot methods (0.2.0)
- Several documentation updates (0.2.0)
- Change "quantile" to "centile" in growth standard methods (0.2.0)
- Add `grid_deriv()` function for estimating derivatives given a grid of points (0.2.0)
- Revamp trajectory fitting to work with fitting on both z-score and measurement scale (0.2.0)
- Add growth velocity and z-score velocity to results or trajectory fits (0.2.0)
- Add `get_subject_data()` and `get_time_data()` for dealing with subject-level / time-varying aspects of longitudinal data (0.2.0)

Version 0.1
----------------------------------------------------------------------

- Initial release
