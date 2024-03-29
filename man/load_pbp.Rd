% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_pbp.R
\name{load_pbp}
\alias{load_pbp}
\title{Load season play-by-play}
\usage{
load_pbp(
  season = as.numeric(substr(Sys.Date() + 184, 1, 4)),
  shift_events = FALSE
)
}
\arguments{
\item{season}{An integer value or vector of values denoting the end year of the season(s) to scrape.
\code{load_pbp} also accepts character strings with more explicit definitions of the
season to scrape: '2020-2021', '2020-21', '2020_21' are also acceptable.\cr
The default value is the current season, switching to the next year on July 1st when the new league year begins.}

\item{shift_events}{Logical value; when set to \code{FALSE} this function
returns a smaller dataset that excludes specifically shift change events}
}
\value{
A tibble containing all play-by-play data for a given season(s) in
the same format as the output of \code{\link{scrape_game}}
}
\description{
Load season play-by-play
}
\examples{
\dontrun{
pbp <- load_pbp(2021)
}
}
