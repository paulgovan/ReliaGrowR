# ReliaGrowR: Reliability Growth Analysis

Modeling and plotting functions for Reliability Growth Analysis (RGA).
The package implements three families of models and provides a REST API
interface via plumber.

## Reliability Growth Models

**Crow-AMSAA (NHPP power-law)**

The core model fits a Non-Homogeneous Poisson Process (NHPP) with a
Weibull intensity function to cumulative failure data. Parameters are
estimated by least-squares log-log regression (default) or maximum
likelihood. A growth rate \> 0 indicates reliability improvement.

**Piecewise NHPP**

Extends Crow-AMSAA by fitting separate NHPP segments separated by change
points. Change points can be detected automatically via the
[segmented](https://rdrr.io/pkg/segmented/man/segmented.html) package or
supplied by the user.

**Duane**

Log-log regression of cumulative MTBF versus cumulative time, providing
a graphical and analytical representation of reliability growth.

## Main Functions

|                                                                                          |                                                  |
|------------------------------------------------------------------------------------------|--------------------------------------------------|
| Function                                                                                 | Description                                      |
| [`rga()`](https://paulgovan.github.io/ReliaGrowR/reference/rga.md)                       | Fit Crow-AMSAA or Piecewise NHPP model           |
| [`predict_rga()`](https://paulgovan.github.io/ReliaGrowR/reference/predict_rga.md)       | Forecast cumulative failures from a fitted model |
| [`duane()`](https://paulgovan.github.io/ReliaGrowR/reference/duane.md)                   | Fit Duane model                                  |
| [`rdt()`](https://paulgovan.github.io/ReliaGrowR/reference/rdt.md)                       | Reliability Demonstration Test plan calculator   |
| [`weibull_to_rga()`](https://paulgovan.github.io/ReliaGrowR/reference/weibull_to_rga.md) | Convert Weibull data to RGA format               |
| [`sim_failures()`](https://paulgovan.github.io/ReliaGrowR/reference/sim_failures.md)     | Simulate failures via PPS sampling               |
| [`qqplot.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/qqplot.rga.md)         | Q-Q goodness-of-fit plot for an `rga` object     |
| [`ppplot.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/ppplot.rga.md)         | P-P goodness-of-fit plot for an `rga` object     |
| [`grwr_api()`](https://paulgovan.github.io/ReliaGrowR/reference/grwr_api.md)             | Launch the plumber REST API                      |

## S3 Classes and Methods

|               |                                                                                                                                                                                              |
|---------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Class         | Methods                                                                                                                                                                                      |
| `rga`         | [`print.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/print.rga.md), [`plot.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.rga.md)                                 |
| `rga_predict` | [`print.rga_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/print.rga_predict.md), [`plot.rga_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.rga_predict.md) |
| `duane`       | [`print.duane()`](https://paulgovan.github.io/ReliaGrowR/reference/print.duane.md), [`plot.duane()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.duane.md)                         |
| `rdt`         | [`print.rdt()`](https://paulgovan.github.io/ReliaGrowR/reference/print.rdt.md)                                                                                                               |

## References

Crow, L. H. (1975). *Reliability Analysis for Complex Repairable
Systems.* AMSAA Technical Report No. 138. US Army Materiel Systems
Analysis Activity.

Duane, J. T. (1964). Learning curve approach to reliability monitoring.
*IEEE Transactions on Aerospace*, 2(2), 563–566.
[doi:10.1109/TA.1964.4319640](https://doi.org/10.1109/TA.1964.4319640)

Guo, H., Mettas, A., Sarakakis, G., & Niu, P. (2010). Piecewise NHPP
models with maximum likelihood estimation for repairable systems. In
*Proceedings of the 2010 Annual Reliability and Maintainability
Symposium* (pp. 1–6). IEEE.
[doi:10.1109/RAMS.2010.5448029](https://doi.org/10.1109/RAMS.2010.5448029)

Muggeo, V. M. R. (2024). *segmented: Regression Models with Break-Points
/ Change-Points Estimation.* R package.
<https://cran.r-project.org/package=segmented>

## See also

Useful links:

- <https://paulgovan.github.io/ReliaGrowR/>

- <https://github.com/paulgovan/ReliaGrowR>

- Report bugs at <https://github.com/paulgovan/ReliaGrowR/issues>

## Author

**Maintainer**: Paul Govan <paul.govan2@gmail.com>
([ORCID](https://orcid.org/0000-0002-1821-8492)) \[copyright holder\]
