#' Calculate Above Replacement Level
#'
#' @param model_data_list List that must have the following:
#' \itemize{
#' \item{"QB_table"} - Table of QBs
#' \item{"RB_table"} - Table of RBs
#' \item{"WR_table"} - Table of WRs
#' \item{"TE_table"} - Table of TEs
#' }
#' @return The input position tables with iPAR columns for
#' air, yac, rush, and totals along with upper/lower bounds.
#' @examples
#' # Calculate the player's value above replacement level:
#' model_data_list <- model_data_list %>%
#'   calculate_above_replacement(model_data_list)
#' @export

calculate_above_replacement <- function(model_data_list) {

  # Find the individual replacement effects for each position,
  # first for QBs:

  qb_replacement_effects <- model_data_list$QB_table %>%
    dplyr::filter(Player_Model_ID == "Replacement_QB") %>%
    dplyr::select(air_iPA, yac_iPA, rush_iPA) %>%
    dplyr::distinct()

  # RBs:
  rb_rush_replacement_effect <- model_data_list$RB_table %>%
    dplyr::filter(Player_Model_ID_Rush == "Replacement_RB_rush") %>%
    dplyr::select(rush_iPA) %>%
    dplyr::distinct()
  rb_rec_replacement_effects <- model_data_list$RB_table %>%
    dplyr::filter(Player_Model_ID_Rec == "Replacement_RB_rec") %>%
    dplyr::select(air_iPA, yac_iPA) %>%
    dplyr::distinct()
  rb_replacement_effects <- rb_rush_replacement_effect %>%
    dplyr::bind_cols(rb_rec_replacement_effects)

  # WRs:
  wr_rush_replacement_effect <- model_data_list$WR_table %>%
    dplyr::filter(Player_Model_ID_Rush == "Replacement_WR_TE_rush") %>%
    dplyr::select(rush_iPA) %>%
    dplyr::distinct()
  wr_rec_replacement_effects <- model_data_list$WR_table %>%
    dplyr::filter(Player_Model_ID_Rec == "Replacement_WR_rec") %>%
    dplyr::select(air_iPA, yac_iPA) %>%
    dplyr::distinct()
  wr_replacement_effects <- wr_rush_replacement_effect %>%
    dplyr::bind_cols(wr_rec_replacement_effects)

  # TEs:
  te_rush_replacement_effect <- model_data_list$TE_table %>%
    dplyr::filter(Player_Model_ID_Rush == "Replacement_WR_TE_rush") %>%
    dplyr::select(rush_iPA) %>%
    dplyr::distinct()
  te_rec_replacement_effects <- model_data_list$TE_table %>%
    dplyr::filter(Player_Model_ID_Rec == "Replacement_TE_rec") %>%
    dplyr::select(air_iPA, yac_iPA) %>%
    dplyr::distinct()
  te_replacement_effects <- te_rush_replacement_effect %>%
    dplyr::bind_cols(te_rec_replacement_effects)

  # Calculate above replacement for QBs:
  model_data_list$QB_table <- model_data_list$QB_table %>%
    dplyr::mutate(air_iPAR = air_iPAA - (Pass_Attempts * qb_replacement_effects$air_iPA),
                  yac_iPAR = yac_iPAA - (Pass_Attempts * qb_replacement_effects$yac_iPA),
                  rush_iPAR = rush_iPAA - ((Rush_Attempts + Sacks) * qb_replacement_effects$rush_iPA),
  # ,
  #                 Lower_air_iPAR = Lower_air_iPAA - (Pass_Attempts * qb_replacement_effects$air_iPA),
  #                 Upper_air_iPAR = Upper_air_iPAA - (Pass_Attempts * qb_replacement_effects$air_iPA),
  #                 Extreme_Lower_air_iPAR = Lower_air_iPAA - (Pass_Attempts * qb_replacement_effects$Upper_air_iPA),
  #                 Extreme_Upper_air_iPAR = Upper_air_iPAA - (Pass_Attempts * qb_replacement_effects$Lower_air_iPA),
  #                 Lower_yac_iPAR = Lower_yac_iPAA - (Pass_Attempts * qb_replacement_effects$yac_iPA),
  #                 Upper_yac_iPAR = Upper_yac_iPAA - (Pass_Attempts * qb_replacement_effects$yac_iPA),
  #                 Extreme_Lower_yac_iPAR = Lower_yac_iPAA - (Pass_Attempts * qb_replacement_effects$Upper_yac_iPA),
  #                 Extreme_Upper_yac_iPAR = Upper_yac_iPAA - (Pass_Attempts * qb_replacement_effects$Lower_yac_iPA),
  #                 Lower_rush_iPAR = Lower_rush_iPAA - ((Rush_Attempts + Sacks) * qb_replacement_effects$rush_iPA),
  #                 Upper_rush_iPAR = Upper_rush_iPAA - ((Rush_Attempts + Sacks) * qb_replacement_effects$rush_iPA),
  #                 Extreme_Lower_rush_iPAR = Lower_rush_iPAA - ((Rush_Attempts + Sacks) * qb_replacement_effects$Upper_rush_iPA),
  #                 Extreme_Upper_rush_iPAR = Upper_rush_iPAA - ((Rush_Attempts + Sacks) * qb_replacement_effects$Lower_rush_iPA),
                    total_iPAR = air_iPAR + yac_iPAR + rush_iPAR)
  #,
  #                 Lower_total_iPAR = Lower_air_iPAR + Lower_yac_iPAR + Lower_rush_iPAR,
  #                 Extreme_Lower_total_iPAR = Extreme_Lower_air_iPAR + Extreme_Lower_yac_iPAR + Extreme_Lower_rush_iPAR,
  #                 Upper_total_iPAR = Upper_air_iPAR + Upper_yac_iPAR + Upper_rush_iPAR,
  #                 Extreme_Upper_total_iPAR = Extreme_Upper_air_iPAR + Extreme_Upper_yac_iPAR + Extreme_Upper_rush_iPAR)

  # Define a helper function that takes in a position table (RB, WR, or TE) and the replacement effects
  # data frames, then returns the modified table:

  add_above_replacement_helper <- function(position_table, rec_replacement_effects,
                                           rush_replacement_effects) {
    position_table %>%
      dplyr::mutate(air_iPAR = air_iPAA - (Targets * rec_replacement_effects$air_iPA),
                    yac_iPAR = yac_iPAA - (Targets * rec_replacement_effects$yac_iPA),
                    rush_iPAR = rush_iPAA - (Rush_Attempts * rush_replacement_effects$rush_iPA),
    # ,
    #                 Lower_air_iPAR = Lower_air_iPAA - (Targets * rec_replacement_effects$air_iPA),
    #                 Upper_air_iPAR = Upper_air_iPAA - (Targets * rec_replacement_effects$air_iPA),
    #                 Extreme_Lower_air_iPAR = Lower_air_iPAA - (Targets * rec_replacement_effects$Upper_air_iPA),
    #                 Extreme_Upper_air_iPAR = Upper_air_iPAA - (Targets * rec_replacement_effects$Lower_air_iPA),
    #                 Lower_yac_iPAR = Lower_yac_iPAA - (Targets * rec_replacement_effects$yac_iPA),
    #                 Upper_yac_iPAR = Upper_yac_iPAA - (Targets * rec_replacement_effects$yac_iPA),
    #                 Extreme_Lower_yac_iPAR = Lower_yac_iPAA - (Targets * rec_replacement_effects$Upper_yac_iPA),
    #                 Extreme_Upper_yac_iPAR = Upper_yac_iPAA - (Targets * rec_replacement_effects$Lower_yac_iPA),
    #                 Lower_rush_iPAR = Lower_rush_iPAA - (Rush_Attempts * rush_replacement_effects$rush_iPA),
    #                 Upper_rush_iPAR = Upper_rush_iPAA - (Rush_Attempts * rush_replacement_effects$rush_iPA),
    #                 Extreme_Lower_rush_iPAR = Lower_rush_iPAA - (Rush_Attempts * rush_replacement_effects$Upper_rush_iPA),
    #                 Extreme_Upper_rush_iPAR = Upper_rush_iPAA - (Rush_Attempts * rush_replacement_effects$Lower_rush_iPA),
                      total_iPAR = air_iPAR + yac_iPAR + rush_iPAR)
    #,
    #                 Lower_total_iPAR = Lower_air_iPAR + Lower_yac_iPAR + Lower_rush_iPAR,
    #                 Extreme_Lower_total_iPAR = Extreme_Lower_air_iPAR + Extreme_Lower_yac_iPAR + Extreme_Lower_rush_iPAR,
    #                 Upper_total_iPAR = Upper_air_iPAR + Upper_yac_iPAR + Upper_rush_iPAR,
    #                 Extreme_Upper_total_iPAR = Extreme_Upper_air_iPAR + Extreme_Upper_yac_iPAR + Extreme_Upper_rush_iPAR)
  }

  # Modify the RB, WR, and TE tables:
  model_data_list$RB_table <- model_data_list$RB_table %>%
    add_above_replacement_helper(rb_rec_replacement_effects, rb_rush_replacement_effect)

  model_data_list$WR_table <- model_data_list$WR_table %>%
    add_above_replacement_helper(wr_rec_replacement_effects, wr_rush_replacement_effect)

  model_data_list$TE_table <- model_data_list$TE_table %>%
    add_above_replacement_helper(te_rec_replacement_effects, te_rush_replacement_effect)

  # Return the modified_data_list:
  return(model_data_list)
}

#' Convert Probabilities to Wins
#'
#' @param model_data_list List that must have the following:
#' \itemize{
#' \item{"QB_table"} - Table of QBs
#' \item{"RB_table"} - Table of RBs
#' \item{"WR_table"} - Table of WRs
#' \item{"TE_table"} - Table of TEs
#' }
#' @return  The input position tables with WAR columns for
#' air, yac, rush, and totals along with upper/lower bounds.
#' @examples
#' # Calculate the WAR values:
#' model_data_list <- model_data_list %>%
#'   convert_prob_to_wins(model_data_list)
#' @export

convert_prob_to_wins <- function(model_data_list) {

  # Create the pipeline expression to calculate the various WAR
  # components and bounds, using the fact the probability sums
  # already reflect the win values:

  calculate_war <- . %>%
    dplyr::mutate(air_WAR = air_iPAR,
                  #Lower_air_WAR = Lower_air_iPAR,
                  #Upper_air_WAR = Upper_air_iPAR,
                  #Extreme_Lower_air_WAR = Extreme_Lower_air_iPAR,
                  #Extreme_Upper_air_WAR = Extreme_Upper_air_iPAR,
                  yac_WAR = yac_iPAR,
                  #Lower_yac_WAR = Lower_yac_iPAR,
                  #Upper_yac_WAR = Upper_yac_iPAR,
                  #Extreme_Lower_yac_WAR = Extreme_Lower_yac_iPAR,
                  #Extreme_Upper_yac_WAR = Extreme_Upper_yac_iPAR,
                  rush_WAR = rush_iPAR,
                  #Lower_rush_WAR = Lower_rush_iPAR,
                  #Upper_rush_WAR = Upper_rush_iPAR,
                  #Extreme_Lower_rush_WAR = Extreme_Lower_rush_iPAR,
                  #Extreme_Upper_rush_WAR = Extreme_Upper_rush_iPAR,
                  total_WAR = total_iPAR)
                  #,
                  #Lower_total_WAR = Lower_total_iPAR,
                  #Upper_total_WAR = Upper_total_iPAR,
                  #Extreme_Lower_total_WAR = Extreme_Lower_total_iPAR,
                  #Extreme_Upper_total_WAR = Extreme_Upper_total_iPAR)

  # Apply this to each of the position tables:

  model_data_list$QB_table <- model_data_list$QB_table %>% calculate_war
  model_data_list$RB_table <- model_data_list$RB_table %>% calculate_war
  model_data_list$WR_table <- model_data_list$WR_table %>% calculate_war
  model_data_list$TE_table <- model_data_list$TE_table %>% calculate_war

  # Return the modified input:
  return(model_data_list)
}



#' Convert Points to Wins
#'
#' @param model_data_list List that must have the following:
#' \itemize{
#' \item{"QB_table"} - Table of QBs
#' \item{"RB_table"} - Table of RBs
#' \item{"WR_table"} - Table of WRs
#' \item{"TE_table"} - Table of TEs
#' }
#' @param points_per_win Value indicating the number of points
#' per win to use (will usually be the result of a function).
#' @return  The input position tables with WAR columns for
#' air, yac, rush, and totals along with upper/lower bounds.
#' @examples
#' # Calculate the WAR values:
#' model_data_list <- model_data_list %>%
#'   convert_points_to_wins(model_data_list, points_per_win)
#' @export

convert_points_to_wins <- function(model_data_list, points_per_win) {

  # Create the pipeline expression to calculate the various WAR
  # components and bounds:

  calculate_war <- . %>%
    dplyr::mutate(air_WAR = air_iPAR / points_per_win,
                  #Lower_air_WAR = Lower_air_iPAR / points_per_win,
                  #Upper_air_WAR = Upper_air_iPAR / points_per_win,
                  #Extreme_Lower_air_WAR = Extreme_Lower_air_iPAR / points_per_win,
                  #Extreme_Upper_air_WAR = Extreme_Upper_air_iPAR / points_per_win,
                  yac_WAR = yac_iPAR / points_per_win,
                  #Lower_yac_WAR = Lower_yac_iPAR / points_per_win,
                  #Upper_yac_WAR = Upper_yac_iPAR / points_per_win,
                  #Extreme_Lower_yac_WAR = Extreme_Lower_yac_iPAR / points_per_win,
                  #Extreme_Upper_yac_WAR = Extreme_Upper_yac_iPAR / points_per_win,
                  rush_WAR = rush_iPAR / points_per_win,
                  #Lower_rush_WAR = Lower_rush_iPAR / points_per_win,
                  #Upper_rush_WAR = Upper_rush_iPAR / points_per_win,
                  #Extreme_Lower_rush_WAR = Extreme_Lower_rush_iPAR / points_per_win,
                  #Extreme_Upper_rush_WAR = Extreme_Upper_rush_iPAR / points_per_win,
                  total_WAR = total_iPAR / points_per_win)
  #,
                  #Lower_total_WAR = Lower_total_iPAR / points_per_win,
                  #Upper_total_WAR = Upper_total_iPAR / points_per_win,
                  #Extreme_Lower_total_WAR = Extreme_Lower_total_iPAR / points_per_win,
                  #Extreme_Upper_total_WAR = Extreme_Upper_total_iPAR / points_per_win)

  # Apply this to each of the position tables:

  model_data_list$QB_table <- model_data_list$QB_table %>% calculate_war
  model_data_list$RB_table <- model_data_list$RB_table %>% calculate_war
  model_data_list$WR_table <- model_data_list$WR_table %>% calculate_war
  model_data_list$TE_table <- model_data_list$TE_table %>% calculate_war

  # Return the modified input:
  return(model_data_list)
}

#' Calculate the Points per Win for Given Season(s)
#' @param years Single number or vector of years to use in calculating
#' the number of points per win.
#' @return Value indicating the points per win.
#' @examples
#' # Calculate the points per win in 2016:
#' points_per_win <- calculate_points_per_win(2016)
#' @export

calculate_points_per_win <- function(years) {

  # First get the season summaries data frame for the given years:
  season_summary <- get_season_summary(years)

  # Fit the linear regression model of Win on the score differential
  # and then return the reciprocal of the coefficient:
  lm_model <- lm(Wins ~ Total_Score_Diff, data = season_summary)
  return(1/as.numeric(coef(lm_model)[2]))
}
