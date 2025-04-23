# downhillr <img src='man/figures/logo.png' align="right" height="150" /></a>

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

## Overview
An R package that scrapes and analyses downhill mountain bike race results.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

```r
pak::pak("betweenthetape/downhillr")
```

## Accessing Data

To date, the package includes the following data sets:
- `world_cup_24_elite_men_results`: Elite Men's results from the each round of the 2024 UCI Mountain Bike World Cup
- `world_cup_24_elite_men_timed_training`: Elite Men's timed training results from each round of the 2024 Mountain Bike World Cup
- `world_cup_24_elite_men_points_scale`: Elite Men's points scale documenting the available points available for each position in each type of round.
- `world_cup_24_elite_men_simulated`: This dataset takes the fastest section times available (from timed training, qualifying, semi-finals, and finals) for each rider across at each event and combines them together to simulate what could have been their fastest race run. These fastest runs are then ranked to determine a new simulated set of race results.

```r
> downhillr::world_cup_24_elite_men_results
# A tibble: 1,690 × 28
    rank protected    nr name  uci_team  uci_id nat   yob   speed split_1 split_2 split_3 split_4  time time_from_leader
   <dbl> <lgl>     <dbl> <chr> <chr>      <dbl> <chr> <chr> <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <dbl>            <dbl>
 1     1 TRUE          1 BRUN… SPECIAL… 1.00e10 FRA   1994   55.6    46.6    155.    190.    220.  244.             0   
 2     2 TRUE          8 BROS… CANYON … 1.00e10 AUS   1993   56.0    47.2    158.    192.    222.  246.             1.84
 3     3 TRUE          4 ILES… SPECIAL… 1.01e10 CAN   1999   55.3    47.2    157.    192.    222.  246.             1.99
 4     4 TRUE          9 NORT… MONDRAK… 1.00e10 USA   1992   54.7    47.6    158.    193.    223.  247.             3.09
 5     5 TRUE         10 SHAW… CANYON … 1.00e10 USA   1996   54.9    46.8    159.    194.    224.  248.             3.68
 6     6 TRUE          7 COUL… DORVAL … 1.00e10 FRA   1994   55.4    47.3    160.    195.    225.  248.             3.87
 7     7 FALSE        38 WILL… MADISON… 1.00e10 GBR   1992   56.4    47.9    159.    195.    225.  249.             4.81
 8     8 TRUE          3 VERG… TREK FA… 1.00e10 FRA   1996   55.2    48.1    160.    195.    225.  249.             5.10
 9     9 TRUE          5 KOLB… CONTINE… 1.00e10 AUT   1996   53.8    48.1    160.    195.    225.  249.             5.17
10    10 FALSE        18 WALK… MADISON… 1.00e10 GBR   1999   53.5    47.8    160.    195.    226.  250.             5.85
# ℹ 1,680 more rows
# ℹ 13 more variables: dnf <lgl>, dsq <lgl>, dns <lgl>, points <dbl>, event_name <chr>, event_type <chr>,
#   event_year <chr>, round_type <chr>, round_category <chr>, metadata_weather <chr>, metadata_temp_deg_c <dbl>,
#   metadata_distance_km <dbl>, metadata_average_speed_kmh <dbl>
# ℹ Use `print(n = ...)` to see more rows
```