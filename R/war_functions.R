#' Calculate Above Replacement Level
#'
#' @param model_data_list List that must have the following:
#' \itemize{
#' \item{"QB_table"} - Table of QBs
#' \item{"RB_table"} - Table of RBs
#' \item{"WR_table"} - Table of WRs
#' \item{"TE_table"} - Table of TEs
#' }
#' @return The input position tables with iPAR columns and
#' for air, yac, and rush along with their upper/lower bounds.
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
    dplyr::select(air_iPA, Lower_air_iPA, Upper_air_iPA,
                  yac_iPA, Lower_yac_iPA, Upper_yac_iPA,
                  rush_iPA, Lower_rush_iPA, Upper_rush_iPA) %>%
    dplyr::distinct()

  # RBs:
  rb_rush_replacement_effect <- model_data_list$RB_table %>%
    dplyr::filter(Player_Model_ID_Rush == "Replacement_RB_rush") %>%
    dplyr::select(rush_iPA, Lower_rush_iPA, Upper_rush_iPA) %>%
    dplyr::distinct()
  rb_rec_replacement_effects <- model_data_list$RB_table %>%
    dplyr::filter(Player_Model_ID_Rec == "Replacement_RB_rec") %>%
    dplyr::select(air_iPA, Lower_air_iPA, Upper_air_iPA,
                  yac_iPA, Lower_yac_iPA, Upper_yac_iPA) %>%
    dplyr::distinct()
  rb_replacement_effects <- rb_rush_replacement_effect %>%
    dplyr::bind_cols(rb_rec_replacement_effects)

  # WRs:
  wr_rush_replacement_effect <- model_data_list$WR_table %>%
    dplyr::filter(Player_Model_ID_Rush == "Replacement_WR_TE_rush") %>%
    dplyr::select(rush_iPA, Lower_rush_iPA, Upper_rush_iPA) %>%
    dplyr::distinct()
  wr_rec_replacement_effects <- model_data_list$WR_table %>%
    dplyr::filter(Player_Model_ID_Rec == "Replacement_WR_rec") %>%
    dplyr::select(air_iPA, Lower_air_iPA, Upper_air_iPA,
                  yac_iPA, Lower_yac_iPA, Upper_yac_iPA) %>%
    dplyr::distinct()
  wr_replacement_effects <- wr_rush_replacement_effect %>%
    dplyr::bind_cols(wr_rec_replacement_effects)

  # TEs:
  te_rush_replacement_effect <- model_data_list$TE_table %>%
    dplyr::filter(Player_Model_ID_Rush == "Replacement_WR_TE_rush") %>%
    dplyr::select(rush_iPA, Lower_rush_iPA, Upper_rush_iPA) %>%
    dplyr::distinct()
  te_rec_replacement_effects <- model_data_list$TE_table %>%
    dplyr::filter(Player_Model_ID_Rec == "Replacement_TE_rec") %>%
    dplyr::select(air_iPA, Lower_air_iPA, Upper_air_iPA,
                  yac_iPA, Lower_yac_iPA, Upper_yac_iPA) %>%
    dplyr::distinct()
  te_replacement_effects <- te_rush_replacement_effect %>%
    dplyr::bind_cols(te_rec_replacement_effects)

  # Calculate above replacement for QBs:
  model_data_list$QB_table <- model_data_list$QB_table %>%
    dplyr::mutate(air_iPAR = air_iPAA - (Pass_Attempts * qb_replacement_effects$air_iPA),
                  yac_iPAR = yac_iPAA - (Pass_Attempts * qb_replacement_effects$yac_iPA),
                  rush_iPAR = rush_iPAA - ((Rush_Attempts + Sacks) * qb_replacement_effects$rush_iPA),
                  Lower_air_iPAR = Lower_air_iPAA - (Pass_Attempts * qb_replacement_effects$Lower_air_iPA),
                  Upper_air_iPAR = Upper_air_iPAA + (Pass_Attempts * qb_replacement_effects$Upper_air_iPA),
                  Lower_yac_iPAR = Lower_yac_iPAA - (Pass_Attempts * qb_replacement_effects$Lower_yac_iPA),
                  Upper_yac_iPAR = Upper_yac_iPAA + (Pass_Attempts * qb_replacement_effects$Upper_yac_iPA),
                  Lower_rush_iPAR = Lower_rush_iPAA - ((Rush_Attempts + Sacks) * qb_replacement_effects$Lower_rush_iPA),
                  Upper_rush_iPAR = Upper_rush_iPAA + ((Rush_Attempts + Sacks) * qb_replacement_effects$Upper_rush_iPA))

  # Define a helper function that takes in a position table (RB, WR, or TE) and the replacement effects
  # data frames, then returns the modified table:

  add_above_replacement_helper <- function(position_table, rec_replacement_effects,
                                           rush_replacement_effects) {
    position_table %>%
      dplyr::mutate(air_iPAR = air_iPAA - (Targets * rec_replacement_effects$air_iPA),
                    yac_iPAR = yac_iPAA - (Targets * rec_replacement_effects$yac_iPA),
                    rush_iPAR = rush_iPAA - (Rush_Attempts * rush_replacement_effects$rush_iPA),
                    Lower_air_iPAR = Lower_air_iPAA - (Targets * rec_replacement_effects$Lower_air_iPA),
                    Upper_air_iPAR = Upper_air_iPAA + (Targets * rec_replacement_effects$Upper_air_iPA),
                    Lower_yac_iPAR = Lower_yac_iPAA - (Targets * rec_replacement_effects$Lower_yac_iPA),
                    Upper_yac_iPAR = Upper_yac_iPAA + (Targets * rec_replacement_effects$Upper_yac_iPA),
                    Lower_rush_iPAR = Lower_rush_iPAA - (Rush_Attempts * rush_replacement_effects$Lower_rush_iPA),
                    Upper_rush_iPAR = Upper_rush_iPAA + (Rush_Attempts * rush_replacement_effects$Upper_rush_iPA))
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
