% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{world_cup_25_elite_women_results}
\alias{world_cup_25_elite_women_results}
\title{Elite Women World Cup 2025 Results}
\format{
A data frame with 209 rows and 27 variables:
\describe{
\item{rank}{The finishing rank of the rider.}
\item{nr}{Rider's race number.}
\item{name}{Rider's name (e.g., "BRUNI Loic").}
\item{uci_team}{Rider's UCI team (e.g., "SPECIALIZED GRAVITY").}
\item{uci_id}{Rider's unique UCI ID.}
\item{nat}{Rider's nationality code (e.g., "FRA" for France).}
\item{yob}{Rider's year of birth.}
\item{speed}{Rider's average speed in km/h.}
\item{split_1}{Rider's time at the first split (in seconds).}
\item{split_2}{Rider's time at the second split (in seconds).}
\item{split_3}{Rider's time at the third split (in seconds).}
\item{split_4}{Rider's time at the fourth split (in seconds).}
\item{time}{Rider's total time to complete the course (in seconds).}
\item{time_from_leader}{Time difference from the race leader (in seconds).}
\item{dnf}{Whether the rider did not finish (TRUE or FALSE).}
\item{dsq}{Whether the rider was disqualified (TRUE or FALSE).}
\item{dns}{Whether the rider did not start (TRUE or FALSE).}
\item{points}{UCI points awarded for the race.}
\item{event_name}{Name of the event (e.g., "Fort William").}
\item{event_type}{Type of the event (e.g., "World Cup").}
\item{event_year}{Year of the event (e.g., "2025").}
\item{round_type}{Round type (e.g., "Final").}
\item{round_category}{Category of the round (e.g., "Men Elite").}
\item{metadata_weather}{Weather conditions during the event (e.g., "Light rain").}
\item{metadata_temp_deg_c}{Temperature during the event (in degrees Celsius).}
\item{metadata_distance_km}{Total race distance (in kilometers).}
\item{metadata_average_speed_kmh}{Average speed across all riders (in km/h).}
}
}
\source{
\url{https://prod.chronorace.be/angular/results.html#/uci/event/20240503_mtb/DHI/CG1}
}
\usage{
world_cup_25_elite_women_results
}
\description{
This dataset contains the Elite Women's results from the each round of the
2025 UCI Mountain Bike World Cup. It includes detailed information about each
rider's performance, team affiliation, nationality, splits, and other
metadata.
}
\keyword{datasets}
