#' Resample Season
#'
#' @param pbp_df Play-by-play data frame.
#' @param drive_level Indicator whether or not to resample
#' entire drives in the season (default is 0).
#' @examples
#' # Resample the 2017 season:
#' resampled_pbp_17 <- resample_season(get_pbp_data(2017))
#' @export

resample_season <- function(pbp_data, drive_level = 0) {
  # Vector of the teams in the season:
  teams <- na.omit(unique(pbp_data$posteam))

  # Check to see if the drive_level indicator is 1,
  # then resample entire drives, otherwise resample
  # team plays:

  if (drive_level == 1) {
    # Create a column that is the GameID and
    # Drive combined:
    pbp_data <- pbp_data %>%
      dplyr::mutate(Game_Drive = paste(GameID, "-", Drive, sep = ""))

    result <- purrr::map_dfr(teams, function(x) {
      # Sample from the team drives with replacement:
      pbp_data %>%
        dplyr::filter(posteam == x) %>%
        dplyr::group_by(Game_Drive) %>%
        tidyr::nest() %>%
        dplyr::sample_n(size = nrow(.), replace = TRUE) %>%
        tidyr::unnest() %>%
        dplyr::ungroup()

    }) %>%
      dplyr::select(-Game_Drive)

  } else {

    # Now apply to each team a function which first finds the rows
    # that a team belongs to, resamples with replacement those rows,
    # and then returns those rows:

    result <- purrr::map_dfr(teams, function(x) {
      pbp_data %>%
      dplyr::filter(posteam == x) %>%
      dplyr::sample_n(size = nrow(.), replace = TRUE) %>%
      return
    })
  }
}

#' Simulate NFL Season Pipeline
#'
#' @param pbp_df Play-by-play data frame.
#' @param n_sims Number of simulations.
#' @param sim_pipeline Pipeline function to apply to
#' each simulation.
#' @return List of results for each simulation.
#' @examples
#' # Create the pipeline:
#' generate_war_results <- . %>%
#'  resample_season() %>%
#'  prepare_model_data() %>%
#'  add_position_tables() %>%
#'  join_position_statistics() %>%
#'  find_positional_replacement_level(league_replacement_functions) %>%
#'  estimate_player_value_added(wp_model_formula_list, "none") %>%
#'  calculate_above_replacement() %>%
#'  convert_prob_to_wins()
#' sim_results <- bootstrap_results(get_pbp_data(2017), 1000, generate_war_results)
#' @export

simulate_season_statistics <- function(pbp_df, n_sims, sim_pipeline) {

  purrr::rerun(n_sims, pbp_df %>% sim_pipeline) %>%
    return
}


#' Combine the Simulation Results
#' @param sim_list List of simulated results
#' where each member of the list has the following tables:
#' \itemize{
#' \item{"QB_table"} - Table of QBs
#' \item{"RB_table"} - Table of RBs
#' \item{"WR_table"} - Table of WRs
#' \item{"TE_table"} - Table of TEs
#' }
#' @return List of the position tables but now with all
#' the simulation results rbind together.
#' @examples
#' sim_player_tables <- combine_simulations(sim_list)
#' @export

combine_simulations <- function(sim_list) {

  # Go through each position table and turn the
  # bootstrapped tables into one:

  sim_QB_table <- purrr::map_dfr(sim_list,
                                  function(x) x$QB_table)

  sim_RB_table <- purrr::map_dfr(sim_list,
                                  function(x) x$RB_table)

  sim_WR_table <- purrr::map_dfr(sim_list,
                                  function(x) x$WR_table)

  sim_TE_table <- purrr::map_dfr(sim_list,
                                  function(x) x$TE_table)

  return(list("QB_table" = sim_QB_table,
              "RB_table" = sim_RB_table,
              "WR_table" = sim_WR_table,
              "TE_table" = sim_TE_table))
}




