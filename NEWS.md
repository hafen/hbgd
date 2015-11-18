Version 0.2
----------------------------------------------------------------------

- Add Intergrowth birth standard
- Add Intergrowth prenatal standard
- Update all standards to have measurement-specific methods (e.g. `who_bmi2zscore()`)
- Add rbokeh/lattice/ggplot2 utilities for plotting all growth standards (e.g. `ly_igb()`)
- Change "quantile" to "centile" in growth standard methods
- Add `grid_deriv()` function for estimating derivatives given a grid of points
- Revamp trajectory fitting to work with fitting on both z-score and measurement scale
- Add growth velocity and z-score velocity to results or trajectory fits
- Add `get_subject_data()` and `get_time_data()` for dealing with subject-level / time-varying aspects of longitudinal data

Version 0.1
----------------------------------------------------------------------

- Initial release
