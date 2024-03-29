% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_skater_stats_hr.R
\name{get_skater_stats_hr}
\alias{get_skater_stats_hr}
\title{Scrape skater stats}
\usage{
get_skater_stats_hr(season = as.numeric(format(Sys.Date() + 81, "\%Y")))
}
\arguments{
\item{season}{Integer value denoting the end year of the season to scrape}
}
\value{
A tibble containing all skater stats found on hockey-reference.com
for the given season
}
\description{
A function to scrape all skater stats from a single season via
hockey-reference.com
}
\examples{
\dontrun{
get_skater_stats_hr(2022)
}
}
