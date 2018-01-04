#' Calculate the 3 Types iPA and iPAA Values for Each Player
#'
#' @param model_data_list List of data frames:
#' \itemize{
#' \item{"pass_model_df"} - Passing play-by-play data
#' \item{"rush_model_df"} - Rushing play-by-play data
#' \item{"QB_table"} - Table of QBs
#' \item{"RB_table"} - Table of RBs
#' \item{"WR_table"} - Table of WRs
#' \item{"TE_table"} - Table of TEs
#' }
#' @param model_formula_list List of the following formulas:
#' \itemize{
#' \item{"air_formula"} - Formula for airEPA/airWPA model.
#' \item{"yac_forumla"} - Formula for yacEPA/yacWPA model.
#' \item{"qb_rush_formula"} - Formula for QB rushing/sacks model.
#' \item{"main_rush_formula"} - Formula for non-QB rushing model.
#' }
#' @param weight_type String indicating which type of observation
#' weighting to use, can either be: (1) "none" or (2) "scorediff".
#' @return The input model_data_list but with each position table
#' modified so that the following columns are included:
#' \itemize{
#' \item{"air_iPA"} - Individual player points/probability added in the air.
#' \item{"yac_iPA"} - Individual player points/probability added after the catch.
#' \item{"rush_iPA"} - Individual player points/probability added from rushing.
#' \item{"air_iPAA"} - Individual player points/probability above average in the air.
#' \item{"yac_iPAA"} - Individual player points/probability above average after the catch.
#' \item{"rush_iPAA"} - Individual player points/probability above average from rushing.
#' }
#' as well as the resulting models:
#' \itemize{
#' \item{"air_model"} - Model fit for air component
#' \item{"yac_model"} - Model fit for yac component
#' \item{"qb_rush_model"} - Model fit for QB rushing/sacks
#' \item{"main_rush_model"} - Model for for all non-QB rushing attempts
#' }
#' @examples
#' # Estimate the player effects and get the iPA and iPAA values:
#' model_data_list <- model_data_list %>%
#'   estimate_player_value_added(model_formula_list, "none")
#' @export

estimate_player_value_added <- function(model_data_list, model_formula_list, weight_type) {

  # ADD ASSERTIONS HERE FOR INPUT

  # Use the estimate_passing_value_added function to estimate the player effects for
  # both air and yac values, returning a list of data frames with the columns to join
  # to the position tables:

  player_passing_value_added <- estimate_passing_value_added(model_data_list$pass_model_df,
                                                             model_formula_list$air_formula,
                                                             model_formula_list$yac_formula,
                                                             weight_type)

  # Use the estimate_rushing_value_added function to estimate the player effects
  # for rushing plays, returning a list of data frames with the columns to join
  # to the position tables:

  player_rushing_value_added <- estimate_rushing_value_added(model_data_list$rush_model_df,
                                                             model_formula_list$qb_rush_formula,
                                                             model_formula_list$main_rush_formula,
                                                             weight_type)

  # Join these individual values to their respective position tables using their associated
  # Model ID fields, then inserting the correct values for replacement level players,
  # and finally calculating the iPAA values using their respective number of attempts:

  # First for QBs:
  model_data_list$QB_table <- model_data_list$QB_table %>%
    dplyr::left_join(player_passing_value_added$QB_table, by = "Player_Model_ID") %>%
    dplyr::left_join(player_rushing_value_added$QB_table, by = "Player_Model_ID") %>%
    dplyr::mutate(air_iPAA = Pass_Attempts * air_iPA,
                  yac_iPAA = Pass_Attempts * yac_iPA,
                  rush_iPAA = (Rush_Attempts + Sacks) * rush_iPA)

  # For RBs:
  model_data_list$RB_table <- model_data_list$RB_table %>%
    dplyr::left_join(player_passing_value_added$Rec_table, by = "Player_Model_ID_Rec") %>%
    dplyr::left_join(player_rushing_value_added$Rush_table, by = "Player_Model_ID_Rush") %>%
    dplyr::mutate(air_iPAA = Targets * air_iPA,
                  yac_iPAA = Targets * yac_iPA,
                  rush_iPAA = Rush_Attempts * rush_iPA)

  # For WRs:
  model_data_list$WR_table <- model_data_list$WR_table %>%
    dplyr::left_join(player_passing_value_added$Rec_table, by = "Player_Model_ID_Rec") %>%
    dplyr::left_join(player_rushing_value_added$Rush_table, by = "Player_Model_ID_Rush") %>%
    dplyr::mutate(air_iPAA = Targets * air_iPA,
                  yac_iPAA = Targets * yac_iPA,
                  rush_iPAA = Rush_Attempts * rush_iPA)

  # For TEs:
  model_data_list$TE_table <- model_data_list$TE_table %>%
    dplyr::left_join(player_passing_value_added$Rec_table, by = "Player_Model_ID_Rec") %>%
    dplyr::left_join(player_rushing_value_added$Rush_table, by = "Player_Model_ID_Rush") %>%
    dplyr::mutate(air_iPAA = Targets * air_iPA,
                  yac_iPAA = Targets * yac_iPA,
                  rush_iPAA = Rush_Attempts * rush_iPA)

  # Return the updated model_data_list:
  return(append(model_data_list,
                list("air_model" = player_passing_value_added$air_model,
                     "yac_model" = player_passing_value_added$yac_model,
                     "qb_rush_model" = player_rushing_value_added$qb_rush_model,
                     "main_rush_model" = player_rushing_value_added$main_rush_model)))
}


#' Calculate Player Effects for Passing Plays
#'
#' @param pass_pbp_df Play-by-play data set of passing plays.
#' @param air_formula Formula for the air based component.
#' @param yac_formula Formula for the yac based component.
#' @param weight_type String indicating which type of observation
#' weighting to use, can either be: (1) "none" or (2) "scorediff".
#' @return List with the following:
#' \itemize{
#' \item{"air_model"} - Model fit for air component
#' \item{"yac_model"} - Model fit for yac component
#' \item{"QB_table"} - Table of QB estimated random effects
#' \item{"Rec_table"} - Table of receiver estimated random effects
#' }
#' @examples
#' passing_effects_list <- estimate_passing_value_added(pass_model_df, air_formula,
#'                                                      yac_formula, "scorediff")
#' @export

estimate_passing_value_added <- function(pass_pbp_df, air_formula, yac_formula, weight_type) {

  # ADD ASSERTIONS TO ENSURE THE FORMULAS CONTAIN THE NECESSARY RANDOM EFFECTS
  # OR CHANGE THE INPUT TO REFLECT THIS

  # Relevel the receiving position factor so that WR is the reference group:
  pass_pbp_df$Receiver_Position <- relevel(as.factor(pass_pbp_df$Receiver_Position), ref="WR")

  # Fit the models based on the weight_type indicator:
  if (weight_type == "scorediff"){

    # Create a weight column based on score differential:
    pass_pbp_df <- pass_pbp_df %>%
      dplyr::mutate(ScoreDiff_W = (max(abs(ScoreDiff)) - abs(ScoreDiff)) / (max(abs(ScoreDiff)) - min(abs(ScoreDiff))))

    air_mm_lmer <- lme4::lmer(air_formula, data = pass_pbp_df,
                              weights = ScoreDiff_W, REML = FALSE)
    yac_mm_lmer <- lme4::lmer(yac_formula, data = pass_pbp_df,
                              weights = ScoreDiff_W, REML = FALSE)

  } else{
    air_mm_lmer <- lme4::lmer(air_formula, data = pass_pbp_df,
                              REML = FALSE)
    yac_mm_lmer <- lme4::lmer(yac_formula, data = pass_pbp_df,
                              REML = FALSE)
  }

  # Get the random effects for each model:
  air_re <- lme4::ranef(air_mm_lmer)
  yac_re <- lme4::ranef(yac_mm_lmer)

  # Create tables with the necessary formatting to store the random effects
  # for both the air and yac iPA estimates, and then joining to one table.

  # First for QBs:
  QB_air_table <- as.data.frame(air_re$Passer_ID_Name) %>%
    dplyr::mutate(Player_Model_ID = rownames(air_re$Passer_ID_Name)) %>%
    dplyr::rename(air_iPA = `(Intercept)`)
  QB_yac_table <- as.data.frame(yac_re$Passer_ID_Name) %>%
    dplyr::mutate(Player_Model_ID = rownames(yac_re$Passer_ID_Name)) %>%
    dplyr::rename(yac_iPA = `(Intercept)`)
  QB_table <- QB_air_table %>%
    dplyr::full_join(QB_yac_table, by = "Player_Model_ID")

  # Next for receiver
  Rec_air_table <- as.data.frame(air_re$Receiver_ID_Name) %>%
    dplyr::mutate(Player_Model_ID_Rec = rownames(air_re$Receiver_ID_Name)) %>%
    dplyr::rename(air_iPA = `(Intercept)`)
  Rec_yac_table <- as.data.frame(yac_re$Receiver_ID_Name) %>%
    dplyr::mutate(Player_Model_ID_Rec = rownames(yac_re$Receiver_ID_Name)) %>%
    dplyr::rename(yac_iPA = `(Intercept)`)
  Rec_table <- Rec_air_table %>%
    dplyr::full_join(Rec_yac_table, by = "Player_Model_ID_Rec")

  # Return the list of the models and the resulting tables:
  return(list("air_model" = air_mm_lmer, "yac_model" = yac_mm_lmer,
              "QB_table" = QB_table, "Rec_table" = Rec_table))
}

#' Calculate Player Effects for Rushing Plays
#'
#' @param rush_pbp_df Play-by-play data set of passing plays.
#' @param qb_rush_formula Formula for QB rushing/sacks.
#' @param main_rush_formula Formula for non-QB rushing.
#' @param weight_type String indicating which type of observation
#' weighting to use, can either be: (1) "none" or (2) "scorediff".
#' @return List with the following:
#' \itemize{
#' \item{"qb_rush_model"} - Model fit for air component
#' \item{"main_rush_model"} - Model fit for yac component
#' \item{"QB_table"} - Table of QB estimated random effects
#' \item{"Rush_table"} - Table of rusher estimated random effects
#' }
#' @examples
#' rushing_effects_list <- estimate_rushing_value_added(rush_model_df, qb_rush_formula,
#'                                                      main_rush_formula, "scorediff")
#' @export

estimate_rushing_value_added <- function(rush_pbp_df, qb_rush_formula, main_rush_formula, weight_type) {

  # ADD ASSERTIONS TO ENSURE THE FORMULAS CONTAIN THE NECESSARY RANDOM EFFECTS
  # OR CHANGE THE INPUT TO REFLECT THIS

  # Create the separate datasets for positions and filter on the year:
  qb_rush_pbp <- rush_pbp_df %>%
    dplyr::filter(Rusher_Position == "QB")
  main_rush_pbp <- rush_pbp_df %>%
    dplyr::filter(Rusher_Position != "QB")

  # Relevel the rushing position factor so that RB is the reference group:
  main_rush_pbp$Rusher_Position <- relevel(as.factor(main_rush_pbp$Rusher_Position), ref = "RB")

  # Fit the models based on the weight_type indicator:
  if (weight_type == "scorediff"){

    # Create a weight column based on score differential:
    # Create a weight column based on score differential:
    qb_rush_pbp <- qb_rush_pbp %>%
      dplyr::mutate(ScoreDiff_W = (max(abs(ScoreDiff)) - abs(ScoreDiff)) / (max(abs(ScoreDiff)) - min(abs(ScoreDiff))))
    main_rush_pbp <- main_rush_pbp %>%
      dplyr::mutate(ScoreDiff_W = (max(abs(ScoreDiff)) - abs(ScoreDiff)) / (max(abs(ScoreDiff)) - min(abs(ScoreDiff))))

    qb_mm_lmer <- lme4::lmer(qb_rush_formula, data = qb_rush_pbp,
                             weights = ScoreDiff_W, REML = FALSE)
    main_rush_mm_lmer <- lme4::lmer(main_rush_formula, data = main_rush_pbp,
                                    weights = ScoreDiff_W, REML = FALSE)

  } else{

    qb_mm_lmer <- lme4::lmer(qb_rush_formula, data = qb_rush_pbp,REML = FALSE)
    main_rush_mm_lmer <- lme4::lmer(main_rush_formula, data = main_rush_pbp,
                                    REML = FALSE)
  }

  # Get the random effects for each model:
  qb_re <- lme4::ranef(qb_mm_lmer)
  rush_re <- lme4::ranef(main_rush_mm_lmer)

  # Create tables with the necessary formatting to store the random effects
  # for both the air and yac iPA estimates, and then joining to one table.

  # First for QBs:
  QB_table <- as.data.frame(qb_re$Rusher_ID_Name) %>%
    dplyr::mutate(Player_Model_ID = rownames(qb_re$Rusher_ID_Name)) %>%
    dplyr::rename(rush_iPA = `(Intercept)`)

  # Next for rushers:
  Rush_table <- as.data.frame(rush_re$Rusher_ID_Name) %>%
    dplyr::mutate(Player_Model_ID_Rush = rownames(rush_re$Rusher_ID_Name)) %>%
    dplyr::rename(rush_iPA = `(Intercept)`)

  # Return the list of the models and the resulting tables:
  return(list("qb_rush_model" = qb_mm_lmer, "main_rush_model" = main_rush_mm_lmer,
              "QB_table" = QB_table, "Rush_table" = Rush_table))
}

