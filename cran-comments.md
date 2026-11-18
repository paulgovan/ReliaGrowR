## Submission summary

This is a minor release (0.7 -> 0.8). Changes:

* Removed `grwr_api()` and the bundled Plumber REST API; `plumber` is no
  longer in Imports.
* Fixed `weibull_to_rga()` so its cumulative time is the population total
  time on test.
* Added an example for `rga_mcp_server()` and updated the RGF vignette.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

We checked all 3 reverse dependencies (ReliaLearnR, ReliaPlotR, ReliaShiny)
with R CMD check against this version. All three returned Status: OK.
None of them call the removed `grwr_api()` or `weibull_to_rga()`.

 * We saw 0 new problems
 * We failed to check 0 packages
