#' Elite Men World Cup 2024 Points Scale
#'
#' This dataset contains the Elite Men's points scale documenting the available
#' points available for each position in each type of round.
#'
#' @format A data frame with 110 rows and 3 vairables:
#' \describe{
#'   \item{round_type}{Round type (e.g., "Final").}
#'   \item{position}{The finishing position of the rider.}
#'   \item{points}{UCI points available}
#' }
#'
#' @source <https://assets.ctfassets.net/761l7gh5x5an/19VfFLnKvm2qgeovbR0N4d/5555f8bd603a91ec7f914c88aaf98f48/UCI_MTB_Rule_Changes_-_Part_IV_-_ENG_-_V05.2023.pdf>
"world_cup_24_elite_men_points_scale"

#' Elite Men World Cup 2024 Results
#'
#' This dataset contains the Elite Men's results from the each round of the 2024
#' UCI Mountain Bike World Cup. It includes detailed information about each
#' rider's performance, team affiliation, nationality, splits, and other
#' metadata.
#'
#' @format A data frame with 1,690 rows and 28 variables:
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
"world_cup_24_elite_men_results"

#' Elite Men World Cup 2024 Timed Training Results
#'
#' This dataset contains the Elite Men's timed training results from each round
#' of the 2024 Mountain Bike World Cup. It includes detailed information about
#' each rider's performance across multiple runs, as well as event metadata.
#'
#' @format A data frame with 886 rows and 30 variables:
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
"world_cup_24_elite_men_timed_training"

#' Elite Men World Cup 2024 Simulated Results
#'
#' This dataset takes the fastest section times available (from timed training,
#' qualifying, semi-finals, and finals) for each rider across at each event and
#' combines them together to simulate what could have been their fastest race
#' run. These fastest runs are then ranked to determine a new simulated set of
#' race results.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{name}{Rider's name (e.g., "NORTON Dakotah").}
#'   \item{event_name}{Name of the event (e.g., "Fort William").}
#'   \item{split_1}{Rider's fastest first split time (in seconds).}
#'   \item{split_2}{Rider's fastest second split time (in seconds).}
#'   \item{split_3}{Rider's fastest third split time (in seconds).}
#'   \item{split_4}{Rider's fastest fourth split time (in seconds).}
#'   \item{time}{Rider's fastest possible time to complete the course (in seconds).}
#'   \item{rank}{The finishing rank of the rider based on time.}
#'   \item{time_from_leader}{Time difference from the fastest rider (in seconds).}
#' }
#'
#' @source Simulated from section analysis of 2024 World Cup events
"world_cup_24_elite_men_simulated"

#' Elite Men World Cup 2025 Results
#'
#' This dataset contains the Elite Men's results from the each round of the 2025
#' UCI Mountain Bike World Cup. It includes detailed information about each
#' rider's performance, team affiliation, nationality, splits, and other
#' metadata.
#'
#' @format A data frame with 1,844 rows and 27 variables:
#' \describe{
#'   \item{rank}{The finishing rank of the rider.}
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
#'   \item{event_year}{Year of the event (e.g., "2025").}
#'   \item{round_type}{Round type (e.g., "Final").}
#'   \item{round_category}{Category of the round (e.g., "Men Elite").}
#'   \item{metadata_weather}{Weather conditions during the event (e.g., "Light rain").}
#'   \item{metadata_temp_deg_c}{Temperature during the event (in degrees Celsius).}
#'   \item{metadata_distance_km}{Total race distance (in kilometers).}
#'   \item{metadata_average_speed_kmh}{Average speed across all riders (in km/h).}
#' }
#'
#' @source <https://prod.chronorace.be/angular/results.html#/uci/event/20240503_mtb/DHI/CG1>
"world_cup_25_elite_men_results"

#' Elite Men World Cup 2025 Timed Training Results
#'
#' This dataset contains the Elite Men's timed training results from each round
#' of the 2025 Mountain Bike World Cup. It includes detailed information about
#' each rider's performance across multiple runs, as well as event metadata.
#'
#' @format A data frame with 866 rows and 41 variables:
#' \describe{
#'   \item{rank}{The finishing rank of the rider based on their best time.}
#'   \item{nr}{Rider's race number.}
#'   \item{name}{Rider's name (e.g., "NORTON Dakotah").}
#'   \item{uci_team}{Rider's UCI team (e.g., "MONDRAKER FACTORY RACING").}
#'   \item{run_1_speed_kmh}{Rider's average speed in km/h during Run 1.}
#'   \item{run_1_split_1}{Time at the first split during Run 1 (in seconds).}
#'   \item{run_1_split_2}{Time at the second split during Run 1 (in seconds).}
#'   \item{run_1_split_3}{Time at the third split during Run 1 (in seconds).}
#'   \item{run_1_split_4}{Time at the fourth split during Run 1 (in seconds).}
#'   \item{run_1_time}{Total time for Run 1 (in seconds).}
#'   \item{run_2_speed_kmh}{Rider's average speed in km/h during Run 2.}
#'   \item{run_2_split_1}{Time at the first split during Run 2 (in seconds).}
#'   \item{run_2_split_2}{Time at the second split during Run 2 (in seconds).}
#'   \item{run_2_split_3}{Time at the third split during Run 2 (in seconds).}
#'   \item{run_2_split_4}{Time at the fourth split during Run 2 (in seconds).}
#'   \item{run_2_time}{Total time for Run 2 (in seconds).}
#'   \item{run_3_speed_kmh}{Rider's average speed in km/h during Run 3.}
#'   \item{run_3_split_1}{Time at the first split during Run 3 (in seconds).}
#'   \item{run_3_split_2}{Time at the second split during Run 3 (in seconds).}
#'   \item{run_3_split_3}{Time at the third split during Run 3 (in seconds).}
#'   \item{run_3_split_4}{Time at the fourth split during Run 3 (in seconds).}
#'   \item{run_3_time}{Total time for Run 3 (in seconds).}
#'   \item{run_4_speed_kmh}{Rider's average speed in km/h during Run 4.}
#'   \item{run_4_split_1}{Time at the first split during Run 4 (in seconds).}
#'   \item{run_4_split_2}{Time at the second split during Run 4 (in seconds).}
#'   \item{run_4_split_3}{Time at the third split during Run 4 (in seconds).}
#'   \item{run_4_split_4}{Time at the fourth split during Run 4 (in seconds).}
#'   \item{run_4_time}{Total time for Run 4 (in seconds).}
#'   \item{run_5_speed_kmh}{Rider's average speed in km/h during Run 5.}
#'   \item{run_5_split_1}{Time at the first split during Run 5 (in seconds).}
#'   \item{run_5_split_2}{Time at the second split during Run 5 (in seconds).}
#'   \item{run_5_split_3}{Time at the third split during Run 5 (in seconds).}
#'   \item{run_5_split_4}{Time at the fourth split during Run 5 (in seconds).}
#'   \item{run_5_time}{Total time for Run 5 (in seconds).}
#'   \item{best_time}{Rider's best time across all runs (in seconds).}
#'   \item{best_time_from_leader}{Time difference from the fastest rider (in seconds).}
#'   \item{event_name}{Name of the event (e.g., "Fort William").}
#'   \item{event_type}{Type of the event (e.g., "World Cup").}
#'   \item{event_year}{Year of the event (e.g., "2025").}
#'   \item{round_type}{Round type (e.g., "Timed Training").}
#'   \item{round_category}{Category of the round (e.g., "Men Elite").}
#' }
#'
#' @source <https://prod.chronorace.be/angular/results.html#/uci/event/20240503_mtb/DHI/CG1>
"world_cup_25_elite_men_timed_training"

#' Elite Women World Cup 2025 Results
#'
#' This dataset contains the Elite Women's results from the each round of the
#' 2025 UCI Mountain Bike World Cup. It includes detailed information about each
#' rider's performance, team affiliation, nationality, splits, and other
#' metadata.
#'
#' @format A data frame with 334 rows and 27 variables:
#' \describe{
#'   \item{rank}{The finishing rank of the rider.}
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
#'   \item{event_year}{Year of the event (e.g., "2025").}
#'   \item{round_type}{Round type (e.g., "Final").}
#'   \item{round_category}{Category of the round (e.g., "Men Elite").}
#'   \item{metadata_weather}{Weather conditions during the event (e.g., "Light rain").}
#'   \item{metadata_temp_deg_c}{Temperature during the event (in degrees Celsius).}
#'   \item{metadata_distance_km}{Total race distance (in kilometers).}
#'   \item{metadata_average_speed_kmh}{Average speed across all riders (in km/h).}
#' }
#'
#' @source <https://prod.chronorace.be/angular/results.html#/uci/event/20240503_mtb/DHI/CG1>
"world_cup_25_elite_women_results"

#' Elite Women World Cup 2025 Timed Training Results
#'
#' This dataset contains the Elite Women's timed training results from each
#' round of the 2025 Mountain Bike World Cup. It includes detailed information
#' about each rider's performance across multiple runs, as well as event
#' metadata.
#'
#' @format A data frame with 151 rows and 41 variables:
#' \describe{
#'   \item{rank}{The finishing rank of the rider based on their best time.}
#'   \item{nr}{Rider's race number.}
#'   \item{name}{Rider's name (e.g., "NORTON Dakotah").}
#'   \item{uci_team}{Rider's UCI team (e.g., "MONDRAKER FACTORY RACING").}
#'   \item{run_1_speed_kmh}{Rider's average speed in km/h during Run 1.}
#'   \item{run_1_split_1}{Time at the first split during Run 1 (in seconds).}
#'   \item{run_1_split_2}{Time at the second split during Run 1 (in seconds).}
#'   \item{run_1_split_3}{Time at the third split during Run 1 (in seconds).}
#'   \item{run_1_split_4}{Time at the fourth split during Run 1 (in seconds).}
#'   \item{run_1_time}{Total time for Run 1 (in seconds).}
#'   \item{run_2_speed_kmh}{Rider's average speed in km/h during Run 2.}
#'   \item{run_2_split_1}{Time at the first split during Run 2 (in seconds).}
#'   \item{run_2_split_2}{Time at the second split during Run 2 (in seconds).}
#'   \item{run_2_split_3}{Time at the third split during Run 2 (in seconds).}
#'   \item{run_2_split_4}{Time at the fourth split during Run 2 (in seconds).}
#'   \item{run_2_time}{Total time for Run 2 (in seconds).}
#'   \item{run_3_speed_kmh}{Rider's average speed in km/h during Run 3.}
#'   \item{run_3_split_1}{Time at the first split during Run 3 (in seconds).}
#'   \item{run_3_split_2}{Time at the second split during Run 3 (in seconds).}
#'   \item{run_3_split_3}{Time at the third split during Run 3 (in seconds).}
#'   \item{run_3_split_4}{Time at the fourth split during Run 3 (in seconds).}
#'   \item{run_3_time}{Total time for Run 3 (in seconds).}
#'   \item{run_4_speed_kmh}{Rider's average speed in km/h during Run 4.}
#'   \item{run_4_split_1}{Time at the first split during Run 4 (in seconds).}
#'   \item{run_4_split_2}{Time at the second split during Run 4 (in seconds).}
#'   \item{run_4_split_3}{Time at the third split during Run 4 (in seconds).}
#'   \item{run_4_split_4}{Time at the fourth split during Run 4 (in seconds).}
#'   \item{run_4_time}{Total time for Run 4 (in seconds).}
#'   \item{run_5_speed_kmh}{Rider's average speed in km/h during Run 5.}
#'   \item{run_5_split_1}{Time at the first split during Run 5 (in seconds).}
#'   \item{run_5_split_2}{Time at the second split during Run 5 (in seconds).}
#'   \item{run_5_split_3}{Time at the third split during Run 5 (in seconds).}
#'   \item{run_5_split_4}{Time at the fourth split during Run 5 (in seconds).}
#'   \item{run_5_time}{Total time for Run 5 (in seconds).}
#'   \item{best_time}{Rider's best time across all runs (in seconds).}
#'   \item{best_time_from_leader}{Time difference from the fastest rider (in seconds).}
#'   \item{event_name}{Name of the event (e.g., "Fort William").}
#'   \item{event_type}{Type of the event (e.g., "World Cup").}
#'   \item{event_year}{Year of the event (e.g., "2025").}
#'   \item{round_type}{Round type (e.g., "Timed Training").}
#'   \item{round_category}{Category of the round (e.g., "Men Elite").}
#' }
#'
#' @source <https://prod.chronorace.be/angular/results.html#/uci/event/20240503_mtb/DHI/CG1>
"world_cup_25_elite_women_timed_training"

#' 2025 World Championships Results
#'
#' This dataset contains the results from the 2025 UCI Mountain Bike World
#' Championships held in Champéry. It includes detailed information about each
#' rider's performance across Men Elite, Women Elite, and Junior Men categories,
#' including team affiliation, nationality, splits, and other metadata.
#'
#' @format A data frame with 200 rows and 25 variables:
#' \describe{
#'   \item{rank}{The finishing rank of the rider.}
#'   \item{nr}{Rider's race number.}
#'   \item{name}{Rider's name (e.g., "A'HERN Kye").}
#'   \item{nat}{Rider's nationality code (e.g., "ARG" for Argentina).}
#'   \item{yob}{Rider's year of birth.}
#'   \item{uci_id}{Rider's unique UCI ID.}
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
#'   \item{event_name}{Name of the event (e.g., "Champéry").}
#'   \item{event_type}{Type of the event (e.g., "World Championships").}
#'   \item{event_year}{Year of the event (e.g., "2025").}
#'   \item{round_type}{Round type (e.g., "Final").}
#'   \item{round_category}{Category of the round (e.g., "Men Elite", "Women Elite", "Junior Men").}
#'   \item{metadata_weather}{Weather conditions during the event (e.g., "Sunny").}
#'   \item{metadata_temp_deg_c}{Temperature during the event (in degrees Celsius).}
#'   \item{metadata_distance_km}{Total race distance (in kilometers).}
#'   \item{metadata_average_speed_kmh}{Average speed across all riders (in km/h).}
#' }
#'
#' @source <https://www.valais2025.ch/results>
"world_championships_25"
