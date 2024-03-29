% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_team_records.R
\name{get_team_records}
\alias{get_team_records}
\title{Get team records from hockey-reference.com}
\usage{
get_team_records(
  season = as.numeric(format(Sys.Date() + 184, "\%Y")),
  league = "NHL",
  include_records = TRUE
)
}
\arguments{
\item{season}{An integer value denoting the end year of the season(s) to
scrape}

\item{league}{The league stats to scrape, either 'NHL' or 'WHA' or get both
with c('NHL','WHA')}

\item{include_records}{Option to exclude records from the function, used to
gather full team names & abbreviations for every season since 1918}
}
\value{
A tibble containing full team names & win-loss records for teams
in all desired seasons
}
\description{
Get team records from hockey-reference.com
}
\examples{
\dontrun{
get_team_records(2021)
}
}
