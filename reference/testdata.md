# Reliability Test Data

A dataset containing example reliability test data from the military
report "Reliability Growth Prediction" (1986) by The Analytical Sciences
Corporation. This dataset includes cumulative ETI, failure counts,
cumulative MTBF, report numbers, flags, and causes for two different
LRUs (G1 and G2).

## Usage

``` r
testdata
```

## Format

@format \## `testdata` A data frame with 25 rows and 6 variables:

- LRU:

  The Line Replaceable Unit identifier (G1 or G2).

- Cum_ETI:

  Cumulative Equivalent Test Hours (ETI).

- Failure_Count:

  Cumulative number of failures observed.

- Cum_MTBF:

  Cumulative Mean Time Between Failures (MTBF).

- Report_No:

  Report number associated with the failure.

- Flag:

  A flag indicating special conditions or notes.

- Cause:

  Cause of the failure (e.g., D for Design, M for Manufacturing, R for
  Random, NR for No Report).

@usage data(testdata)

## Examples

``` r
data(testdata)
#> Warning: data set ‘testdata’ not found
head(testdata)
#>   LRU Cum_ETI Failure_Count Cum_MTBF Report_No Flag Cause
#> 1  G1     4.9             1      4.9        23    1     D
#> 2  G1    17.8             2      8.9         4    1     D
#> 3  G1    33.4             3     11.1         6    1     D
#> 4  G1    75.3             4     18.8        12    1     D
#> 5  G1    84.0             5     16.8        13    1     D
#> 6  G1   215.0             6     35.6        25    1     D
summary(testdata)
#>      LRU               Cum_ETI       Failure_Count    Cum_MTBF    
#>  Length:25          Min.   :   4.9   Min.   : 1    Min.   :  4.9  
#>  Class :character   1st Qu.: 215.0   1st Qu.: 4    1st Qu.: 31.3  
#>  Mode  :character   Median : 349.0   Median : 7    Median : 70.4  
#>                     Mean   : 780.3   Mean   : 7    Mean   : 89.7  
#>                     3rd Qu.:1503.0   3rd Qu.:10    3rd Qu.:143.4  
#>                     Max.   :2502.0   Max.   :15    Max.   :235.5  
#>                                                                   
#>    Report_No           Flag           Cause          
#>  Min.   :  2.00   Min.   : 0.000   Length:25         
#>  1st Qu.: 21.00   1st Qu.: 1.000   Class :character  
#>  Median : 36.00   Median : 1.000   Mode  :character  
#>  Mean   : 54.36   Mean   : 7.609                     
#>  3rd Qu.: 98.00   3rd Qu.:14.500                     
#>  Max.   :125.00   Max.   :28.000                     
#>                   NA's   :2                          
str(testdata)
#> 'data.frame':    25 obs. of  7 variables:
#>  $ LRU          : chr  "G1" "G1" "G1" "G1" ...
#>  $ Cum_ETI      : num  4.9 17.8 33.4 75.3 84 ...
#>  $ Failure_Count: int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ Cum_MTBF     : num  4.9 8.9 11.1 18.8 16.8 35.6 31.3 32.8 34.6 53.4 ...
#>  $ Report_No    : num  23 4 6 12 13 25 28 36 41 57 ...
#>  $ Flag         : num  1 1 1 1 1 1 1 NA 21 0 ...
#>  $ Cause        : chr  "D" "D" "D" "D" ...
```
