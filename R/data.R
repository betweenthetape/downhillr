#' Elite Results - Fort William World Cup 2024
#'
#' This dataset contains the results of the Men's and Women's Elite category
#' from the Fort William round of the 2024 Mountain Bike World Cup. It includes
#' detailed information about each rider's performance, team affiliation,
#' nationality, splits, and other metadata.
#'
#' @format A data frame with 301 rows and 28 variables:
#' \describe{
#'   \item{rank}{The finishing rank of the rider.}
#'   \item{protected}{Whether the rider was in the protected category (TRUE or FALSE).}
#'   \item{nr}{Rider's race number.}
#'   \item{name}{Rider's name (e.g., "BRUNI Loic").}
#'   \item{uci_team}{Rider's UCI team (e.g., "SPECIALIZED GRAVITY").}
#'   \item{uci_id}{Rider's unique UCI ID.}
#'   \item{nat}{Rider's nationality code (e.g., "FRA" for France).}
#'   \item{yob}{Rider's year of birth.}
#'   \item{speed}{Rider's average speed in km/h.}
#'   \item{split_1}{Rider's time at the first split (in seconds).}
#'   \item{split_2}{Rider's time at the second split (in seconds).}
#'   \item{split_3}{Rider's time at the third split (in seconds).}
#'   \item{split_4}{Rider's time at the fourth split (in seconds).}
#'   \item{time}{Rider's total time to complete the course (in seconds).}
#'   \item{time_from_leader}{Time difference from the race leader (in seconds).}
#'   \item{dnf}{Whether the rider did not finish (TRUE or FALSE).}
#'   \item{dsq}{Whether the rider was disqualified (TRUE or FALSE).}
#'   \item{dns}{Whether the rider did not start (TRUE or FALSE).}
#'   \item{points}{UCI points awarded for the race.}
#'   \item{event_name}{Name of the event (e.g., "Fort William").}
#'   \item{event_type}{Type of the event (e.g., "World Cup").}
#'   \item{event_year}{Year of the event (e.g., "2024").}
#'   \item{round_type}{Round type (e.g., "Final").}
#'   \item{round_category}{Category of the round (e.g., "Men Elite").}
#'   \item{metadata_weather}{Weather conditions during the event (e.g., "Light rain").}
#'   \item{metadata_temp_deg_c}{Temperature during the event (in degrees Celsius).}
#'   \item{metadata_distance_km}{Total race distance (in kilometers).}
#'   \item{metadata_average_speed_kmh}{Average speed across all riders (in km/h).}
#' }
#'
#' @source <https://prod.chronorace.be/angular/results.html#/uci/event/20240503_mtb/DHI/CG1>
"world_cup_24_fort_william_elites_results"

#' Elite Timed Training - Fort William World Cup 2024
#'
#' This dataset contains the results of the Men's and Women's Elite Timed
#' Training session from the Fort William round of the 2024 Mountain Bike World
#' Cup. It includes detailed information about each rider's performance across
#' multiple runs, as well as event metadata.
#'
#' @format A data frame with 169 rows and 30 variables:
#' \describe{
#'   \item{rank}{The finishing rank of the rider based on their best time.}
#'   \item{nr}{Rider's race number.}
#'   \item{name}{Rider's name (e.g., "NORTON Dakotah").}
#'   \item{uci_team}{Rider's UCI team (e.g., "MONDRAKER FACTORY RACING").}
#'   \item{nat}{Rider's nationality code (e.g., "USA" for United States).}
#'   \item{run_1_speed}{Rider's average speed in km/h during Run 1.}
#'   \item{run_1_split_1}{Time at the first split during Run 1 (in seconds).}
#'   \item{run_1_split_2}{Time at the second split during Run 1 (in seconds).}
#'   \item{run_1_split_3}{Time at the third split during Run 1 (in seconds).}
#'   \item{run_1_split_4}{Time at the fourth split during Run 1 (in seconds).}
#'   \item{run_1_time}{Total time for Run 1 (in seconds).}
#'   \item{run_2_speed}{Rider's average speed in km/h during Run 2.}
#'   \item{run_2_split_1}{Time at the first split during Run 2 (in seconds).}
#'   \item{run_2_split_2}{Time at the second split during Run 2 (in seconds).}
#'   \item{run_2_split_3}{Time at the third split during Run 2 (in seconds).}
#'   \item{run_2_split_4}{Time at the fourth split during Run 2 (in seconds).}
#'   \item{run_2_time}{Total time for Run 2 (in seconds).}
#'   \item{run_3_speed}{Rider's average speed in km/h during Run 3.}
#'   \item{run_3_split_1}{Time at the first split during Run 3 (in seconds).}
#'   \item{run_3_split_2}{Time at the second split during Run 3 (in seconds).}
#'   \item{run_3_split_3}{Time at the third split during Run 3 (in seconds).}
#'   \item{run_3_split_4}{Time at the fourth split during Run 3 (in seconds).}
#'   \item{run_3_time}{Total time for Run 3 (in seconds).}
#'   \item{best_time}{Rider's best time across all runs (in seconds).}
#'   \item{best_time_from_leader}{Time difference from the fastest rider (in seconds).}
#'   \item{event_name}{Name of the event (e.g., "Fort William").}
#'   \item{event_type}{Type of the event (e.g., "World Cup").}
#'   \item{event_year}{Year of the event (e.g., "2024").}
#'   \item{round_type}{Round type (e.g., "Timed Training").}
#'   \item{round_category}{Category of the round (e.g., "Men Elite").}
#' }
#'
#' @source <https://prod.chronorace.be/angular/results.html#/uci/event/20240503_mtb/DHI/CG1>
"world_cup_24_fort_william_elites_timed_training"
