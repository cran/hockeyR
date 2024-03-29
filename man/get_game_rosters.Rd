% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_game_rosters.R
\name{get_game_rosters}
\alias{get_game_rosters}
\title{Scrape game day rosters for individual game}
\usage{
get_game_rosters(game_id)
}
\arguments{
\item{game_id}{Game ID to scrape (Can be found using get_game_ids function)}
}
\value{
A tibble containing player names, ids, and positions for both team rosters
in a given game.
}
\description{
Scrapes the game-day rosters for both teams in the given game ID
}
