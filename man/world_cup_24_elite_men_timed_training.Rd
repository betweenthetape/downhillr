% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{world_cup_24_elite_men_timed_training}
\alias{world_cup_24_elite_men_timed_training}
\title{Elite Men World Cup 2024 Timed Training Results}
\format{
A data frame with 886 rows and 30 variables:
\describe{
\item{rank}{The finishing rank of the rider based on their best time.}
\item{nr}{Rider's race number.}
\item{name}{Rider's name (e.g., "NORTON Dakotah").}
\item{uci_team}{Rider's UCI team (e.g., "MONDRAKER FACTORY RACING").}
\item{nat}{Rider's nationality code (e.g., "USA" for United States).}
\item{run_1_speed}{Rider's average speed in km/h during Run 1.}
\item{run_1_split_1}{Time at the first split during Run 1 (in seconds).}
\item{run_1_split_2}{Time at the second split during Run 1 (in seconds).}
\item{run_1_split_3}{Time at the third split during Run 1 (in seconds).}
\item{run_1_split_4}{Time at the fourth split during Run 1 (in seconds).}
\item{run_1_time}{Total time for Run 1 (in seconds).}
\item{run_2_speed}{Rider's average speed in km/h during Run 2.}
\item{run_2_split_1}{Time at the first split during Run 2 (in seconds).}
\item{run_2_split_2}{Time at the second split during Run 2 (in seconds).}
\item{run_2_split_3}{Time at the third split during Run 2 (in seconds).}
\item{run_2_split_4}{Time at the fourth split during Run 2 (in seconds).}
\item{run_2_time}{Total time for Run 2 (in seconds).}
\item{run_3_speed}{Rider's average speed in km/h during Run 3.}
\item{run_3_split_1}{Time at the first split during Run 3 (in seconds).}
\item{run_3_split_2}{Time at the second split during Run 3 (in seconds).}
\item{run_3_split_3}{Time at the third split during Run 3 (in seconds).}
\item{run_3_split_4}{Time at the fourth split during Run 3 (in seconds).}
\item{run_3_time}{Total time for Run 3 (in seconds).}
\item{best_time}{Rider's best time across all runs (in seconds).}
\item{best_time_from_leader}{Time difference from the fastest rider (in seconds).}
\item{event_name}{Name of the event (e.g., "Fort William").}
\item{event_type}{Type of the event (e.g., "World Cup").}
\item{event_year}{Year of the event (e.g., "2024").}
\item{round_type}{Round type (e.g., "Timed Training").}
\item{round_category}{Category of the round (e.g., "Men Elite").}
}
}
\source{
\url{https://prod.chronorace.be/angular/results.html#/uci/event/20240503_mtb/DHI/CG1}
}
\usage{
world_cup_24_elite_men_timed_training
}
\description{
This dataset contains the Elite Men's timed training results from each round
of the 2024 Mountain Bike World Cup. It includes detailed information about
each rider's performance across multiple runs, as well as event metadata.
}
\keyword{datasets}
