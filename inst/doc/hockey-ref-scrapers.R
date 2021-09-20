## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/vignettes-",
  out.width = "75%",
  dpi = 750,
  fig.width = 7,
  fig.height = 4.67
)

## ----setup--------------------------------------------------------------------
library(hockeyR)

## ----records------------------------------------------------------------------
get_team_records(1967) |>
  dplyr::arrange(-w) |>
  dplyr::select(team_name, team_abbr, season, overall, w, l, otl, st_points)

## ----player stats-------------------------------------------------------------
get_player_stats_hr(player_name = "Wayne Gretzky", season = 1982) |>
  dplyr::select(player, age, season_full, tm, gp, g, a, pts)

## ----jerseys------------------------------------------------------------------
# get every player to wear the desired number
df <- get_jersey_players(98)

# get their statistics from the year they wore that sweater
df2 <- purrr::map2_dfr(
  .x = df$player,
  .y = df$season,
  ~get_player_stats_hr(player_name = .x, season = .y)
  )

# who had the most goals?
dplyr::arrange(df2, dplyr::desc(g)) |>
  dplyr::select(player, tm, season_full, gp, g, a, pts)

## ----plot-example-------------------------------------------------------------
# add colors & logos
df3 <- df2 |>
  dplyr::group_by(player, season_full) |>
  # this part is just to get both of Mete's teams into one row
  dplyr::summarize(
    gp = sum(gp),
    pts = sum(pts),
    pts_gm = pts/gp,
    tm = utils::tail(tm, n=1),
    .groups = "drop"
  ) |>
  dplyr::mutate(player_season = glue::glue("{player}\n{season_full}")) |>
  dplyr::left_join(team_logos_colors, by = c("tm" = "team_abbr"))

# make a bar chart
df3 |>
  ggplot2::ggplot(ggplot2::aes(stats::reorder(player_season, -pts_gm), pts_gm)) +
  ggplot2::geom_col(fill = df3$team_color1, color = df3$team_color2) +
  ggimage::geom_image(
    ggplot2::aes(y = pts_gm + .027, image = team_logo_espn),
    size = .07, asp = 1.5
  ) +
  ggplot2::geom_text(ggplot2::aes(y = 0.01, label = player_season),
            color = "white", angle = 90, hjust = 0) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "black"),
    plot.background = ggplot2::element_rect(fill = "black"),
    panel.grid.major.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(color = "white"),
    title = ggplot2::element_text(color = "white")
  ) +
  ggplot2::labs(x = NULL, y = "Points per game",
       title = "Most productive NHL seasons wearing #98",
       caption = "data pulled from hockey-reference.com using hockeyR")

## ----rosters------------------------------------------------------------------
player_stats <- get_rosters(c("COL","Detroit red wings"), season = 2001, include_stats = TRUE) |>
  dplyr::mutate(
    g_60 = 60 * g / toi,
    a_60 = 60 * a /toi,
    p_60 = 60 * pts / toi
  ) |>
  dplyr::filter(toi >= 300) |>
  dplyr::left_join(team_logos_colors, by = "team_abbr")

top_performers <- dplyr::filter(
      player_stats,
      p_60 >= dplyr::arrange(player_stats, -p_60) |>
        dplyr::slice(10) |>
        dplyr::pull(p_60)
      )

player_stats |>
  ggplot2::ggplot(ggplot2::aes(a_60,g_60)) +
  ggplot2::geom_hline(yintercept = 60 * sum(player_stats$g) / sum(player_stats$toi),
             linetype = "dashed", color = "black") +
  ggplot2::geom_vline(xintercept = 60 * sum(player_stats$a) / sum(player_stats$toi),
             linetype = "dashed", color = "black") +
  #geom_point(aes(size = toi), show.legend = FALSE,
  #           color = player_stats$team_color_alt1, alpha = .8) +
  ggimage::geom_image(ggplot2::aes(image = team_logo_espn),
                      size = 0.07, asp = 1.5) +
  ggrepel::geom_text_repel(
    data = top_performers,
    ggplot2::aes(label = player),
    color = top_performers$team_color_alt1
  ) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "#708090"),
    plot.background = ggplot2::element_rect(fill = "#708090"),
    title = ggplot2::element_text(color = "white")
  ) +
  ggplot2::labs(x = "Assists/60", y = "Goals/60",
       title = "2000-01 Wings v Avs, regular season stats",
       subtitle = "min. 300 minutes",
       caption = "data pulled from hockey-reference.com using hockeyR")

