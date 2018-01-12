#' Join Various Statistics to the Position Tables
#'
#' @param model_data_list List that must have the following:
#' \itemize{
#' \item{"pass_model_df"} - Passing play-by-play data
#' \item{"rush_model_df"} - Rushing play-by-play data
#' \item{"QB_table"} - Table of QBs
#' \item{"RB_table"} - Table of RBs
#' \item{"WR_table"} - Table of WRs
#' \item{"TE_table"} - Table of TEs
#' }
#' @return The input position tables with all the different
#' types of statistics included.
#' @examples
#' # Calculate the various types of statistics for each position:
#' model_data_list <- model_data_list %>%
#'   join_position_statistics(model_data_list)
#' @export

join_position_statistics <- function(model_data_list) {

  # Use the split functions to calculate the different
  # types of statistics:
  passing_stats <- calc_passing_splits(model_data_list$pass_model_df)
  receiving_stats <- calc_receiving_splits(model_data_list$pass_model_df)
  rushing_stats <- calculate_rushing_splits(model_data_list$rush_model_df)

  # Drop the redundant columns in each dataset:
  passing_stats <- passing_stats %>%
    dplyr::select(-Pass_Attempts)

  receiving_stats <- receiving_stats %>%
    dplyr::select(-Targets)

  rushing_stats <- rushing_stats %>%
    dplyr::select(-Rushes, -Sacks)

  # Now join the stats to the position tables, and return:
  model_data_list$QB_table <- model_data_list$QB_table %>%
    dplyr::left_join(passing_stats,
                     by = c("Player_ID_Name" = "Passer_ID_Name")) %>%
    dplyr::left_join(rushing_stats,
                     by = c("Player_ID_Name" = "Rusher_ID_Name"))
  model_data_list$QB_table[is.na(model_data_list$QB_table)] <- 0

  # Expression for the non-QB tables:
  join_nonQB_stats <- . %>%
    dplyr::left_join(receiving_stats,
                     by = c("Player_ID_Name" = "Receiver_ID_Name")) %>%
    dplyr::left_join(rushing_stats,
                     by = c("Player_ID_Name" = "Rusher_ID_Name"))

  model_data_list$WR_table <- model_data_list$WR_table %>% join_nonQB_stats
  model_data_list$WR_table[is.na(model_data_list$WR_table)] <- 0

  model_data_list$RB_table <- model_data_list$RB_table %>% join_nonQB_stats
  model_data_list$RB_table[is.na(model_data_list$RB_table)] <- 0

  model_data_list$TE_table <- model_data_list$TE_table %>% join_nonQB_stats
  model_data_list$TE_table[is.na(model_data_list$TE_table)] <- 0


  return(model_data_list)
}

#' Calculate Passing Statistics for Given Splits
#'
#' @param pass_pbp Data frame of passing plays.
#' @param splits String vector of variables to split on, default
#' is "Passer_ID_Name" (splits are levels to calculate the
#' statistics for).
#' @return Data frame of passing statistics evaluated at the
#' given level of splits.
#' @examples
#' # Calculate passing statistics :
#' model_data_list <- calculate_passing_splits(model_data_list$pass_model_df,
#'                                             "Passer_ID_Name")
#' @export

calc_passing_splits <- function(pass_pbp, splits = "Passer_ID_Name") {

  # Turn the splits into the group variables:
  split_groups <- lapply(splits, as.symbol)

  # Create the Game_Drive column:
  pass_pbp %>%
    dplyr::mutate(Game_Drive = paste(as.character(GameID),
                                    as.character(Drive), sep = "-")) %>%
    # Group by the given splits:
    dplyr::group_by_(.dots = split_groups) %>%
    # Calculate the passing statistics at the given splits level:
    dplyr::summarise(Pass_Attempts = n(),
                     Completions = sum(Reception, na.rm = TRUE),
                     Pass_Drives = n_distinct(Game_Drive),
                     Comp_Perc = Completions / Pass_Attempts,
                     Pass_Yards_Gained = sum(Yards.Gained, na.rm = TRUE),
                     Total_AirYards = sum(AirYards, na.rm = TRUE),
                     Total_AirYards_Comp = sum(Reception*AirYards, na.rm = TRUE),
                     Total_YardsAfterCatch = sum(Reception*YardsAfterCatch, na.rm = TRUE),
                     Yards_per_Att = Pass_Yards_Gained / Pass_Attempts,
                     Yards_per_Comp = Pass_Yards_Gained / Completions,
                     Yards_per_Drive = Pass_Yards_Gained / Pass_Drives,
                     AirYards_per_Att = Total_AirYards / Pass_Attempts,
                     AirYards_per_Comp = Total_AirYards / Completions,
                     AirYards_per_Drive = Total_AirYards / Pass_Drives,
                     AirYards_Comp_per_Att = Total_AirYards_Comp / Pass_Attempts,
                     AirYards_Comp_per_Comp = Total_AirYards_Comp / Completions,
                     AirYards_Comp_per_Drive = Total_AirYards_Comp / Pass_Drives,
                     YardsAfterCatch_per_Att = Total_YardsAfterCatch / Pass_Attempts,
                     YardsAfterCatch_per_Comp = Total_YardsAfterCatch / Completions,
                     YardsAfterCatch_per_Drive = Total_YardsAfterCatch / Pass_Drives,
                     PACR = Pass_Yards_Gained / Total_AirYards,
                     Times_Hit = sum(QBHit, na.rm = TRUE),
                     Times_Hit_per_Att = Times_Hit / Pass_Attempts,
                     Times_Hit_per_Comp = Times_Hit / Completions,
                     Times_Hit_per_Drive = Times_Hit / Pass_Drives,
                     INTs = sum(InterceptionThrown, na.rm = TRUE),
                     Pass_TDs = sum(Touchdown, na.rm = TRUE),
                     Air_TDs = sum(as.numeric(YardsAfterCatch == 0) * Touchdown, na.rm = TRUE),
                     Air_TD_Rate = Air_TDs / Pass_TDs,
                     TD_to_INT = Pass_TDs / INTs,
                     TD_per_Att = Air_TDs / Pass_Attempts,
                     Air_TD_per_Att = Air_TDs / Pass_Attempts,
                     TD_per_Comp = Pass_TDs / Completions,
                     Air_TD_per_Comp = Air_TDs / Completions,
                     TD_per_Drive = Pass_TDs / Pass_Drives,
                     Air_TD_per_Drive = Air_TDs / Pass_Drives,
                     INTs_per_Att = INTs / Pass_Attempts,
                     INTs_per_Comp = INTs / Completions,
                     INTs_per_Drive = INTs / Pass_Drives,
                     Pass_EPA = sum(EPA, na.rm = TRUE),
                     Pass_EPA_per_Att = Pass_EPA / Pass_Attempts,
                     Pass_EPA_per_Comp = Pass_EPA / Completions,
                     Pass_EPA_per_Drive = Pass_EPA / Pass_Drives,
                     Pass_Success_Rate = length(which(EPA > 0)) / Pass_Attempts,
                     Comp_EPA = sum(Reception * EPA, na.rm = TRUE),
                     Comp_EPA_per_Att = Comp_EPA / Pass_Attempts,
                     Comp_EPA_per_Comp = Comp_EPA / Completions,
                     Comp_EPA_per_Drive = Comp_EPA / Pass_Drives,
                     EPA_Comp_Perc = Comp_EPA / sum(abs(EPA), na.rm = TRUE),
                     Pass_WPA = sum(WPA, na.rm = TRUE),
                     Pass_WPA_per_Att = Pass_WPA / Pass_Attempts,
                     Pass_WPA_per_Comp = Pass_WPA / Completions,
                     Pass_WPA_per_Drive = Pass_WPA / Pass_Drives,
                     Pass_Win_Rate = length(which(WPA > 0)) / Pass_Attempts,
                     Comp_WPA = sum(Reception * WPA, na.rm = TRUE),
                     Comp_WPA_per_Att = Comp_WPA / Pass_Attempts,
                     Comp_WPA_per_Comp = Comp_WPA / Completions,
                     Comp_WPA_per_Drive = Comp_WPA / Pass_Drives,
                     WPA_Comp_Perc = Comp_WPA / sum(abs(WPA), na.rm = TRUE),
                     Pass_Clutch_EPA = sum(EPA*abs(WPA), na.rm = TRUE),
                     Pass_Clutch_EPA_per_Att = Pass_Clutch_EPA / Pass_Attempts,
                     Pass_Clutch_EPA_per_Comp = Pass_Clutch_EPA / Completions,
                     Pass_Clutch_EPA_per_Drives = Pass_Clutch_EPA / Pass_Drives,
                     Total_airEPA = sum(Reception*airEPA,na.rm=TRUE),
                     airEPA_per_Att = Total_airEPA / Pass_Attempts,
                     airEPA_per_Comp = Total_airEPA / Completions,
                     airEPA_per_Drive = Total_airEPA / Pass_Drives,
                     air_Success_Rate = length(which(airEPA>0)) / Pass_Attempts,
                     air_Comp_Success_Rate = length(which((Reception*airEPA)>0)) / Pass_Attempts,
                     Total_airWPA = sum(Reception*airWPA,na.rm=TRUE),
                     airWPA_per_Att = Total_airWPA / Pass_Attempts,
                     airWPA_per_Comp = Total_airWPA / Completions,
                     airWPA_per_Drive = Total_airWPA / Pass_Drives,
                     air_Win_Rate = length(which(airWPA>0)) / Pass_Attempts,
                     air_Comp_Win_Rate = length(which((Reception*airWPA)>0)) / Pass_Attempts,
                     Total_yacEPA = sum(Reception*yacEPA,na.rm=TRUE),
                     yacEPA_per_Att = Total_yacEPA / Pass_Attempts,
                     yacEPA_per_Comp = Total_yacEPA / Completions,
                     yacEPA_per_Drive = Total_yacEPA / Pass_Drives,
                     yac_Success_Rate = length(which((Reception*yacEPA)>0)) / Pass_Attempts,
                     Total_yacWPA = sum(Reception*yacWPA,na.rm=TRUE),
                     yacWPA_per_Att = Total_yacWPA / Pass_Attempts,
                     yacWPA_per_Comp = Total_yacWPA / Completions,
                     yacWPA_per_Drive = Total_yacWPA / Pass_Drives,
                     yac_Win_Rate = length(which((Reception*yacWPA)>0)) / Pass_Attempts) %>%
    return

}

#' Calculate Receiving Statistics for Given Splits
#'
#' @param pass_pbp Data frame of passing plays.
#' @param splits String vector of variables to split on, default
#' is "Receiver_ID_Name" (splits are levels to calculate the
#' statistics for).
#' @return Data frame of receiving statistics evaluated at the
#' given level of splits.
#' @examples
#' # Calculate receiving statistics :
#' model_data_list <- calculate_receiving_splits(model_data_list$pass_model_df,
#'                                               "Receiver_ID_Name")
#' @export

calc_receiving_splits <- function(pass_pbp, splits = "Receiver_ID_Name") {

  # Turn the splits into the group variables:
  split_groups <- lapply(splits, as.symbol)

  # Create the Game_Drive column:
  pass_pbp %>%
    dplyr::mutate(Game_Drive = paste(as.character(GameID),
                                     as.character(Drive), sep = "-")) %>%
    # Group by the given splits:
    dplyr::group_by_(.dots = split_groups) %>%
    # Calculate the passing statistics at the given splits level:
    dplyr::summarise(Targets = n(),
                     Receptions = sum(Reception, na.rm = TRUE),
                     Rec_Drives = n_distinct(Game_Drive),
                     Rec_Perc = Receptions / Targets,
                     Rec_Yards_Gained = sum(Yards.Gained, na.rm = TRUE),
                     Total_AirYards = sum(AirYards, na.rm = TRUE),
                     Total_AirYards_Rec = sum(Reception*AirYards, na.rm = TRUE),
                     Total_YardsAfterCatch = sum(Reception*YardsAfterCatch, na.rm = TRUE),
                     Yards_per_Tgt = Rec_Yards_Gained / Targets,
                     Yards_per_Rec = Rec_Yards_Gained / Receptions,
                     Yards_per_Drive = Rec_Yards_Gained / Rec_Drives,
                     AirYards_per_Tgt = Total_AirYards / Targets,
                     AirYards_per_Rec = Total_AirYards / Receptions,
                     AirYards_per_Drive = Total_AirYards / Rec_Drives,
                     AirYards_Rec_per_Tgt = Total_AirYards_Rec / Targets,
                     AirYards_Rec_per_Rec = Total_AirYards_Rec / Receptions,
                     AirYards_Rec_per_Drive = Total_AirYards_Rec / Rec_Drives,
                     YardsAfterCatch_per_Tgt = Total_YardsAfterCatch / Targets,
                     YardsAfterCatch_per_Rec = Total_YardsAfterCatch / Receptions,
                     YardsAfterCatch_per_Drive = Total_YardsAfterCatch / Rec_Drives,
                     RACR = Rec_Yards_Gained / Total_AirYards,
                     Rec_INTs = sum(InterceptionThrown, na.rm = TRUE),
                     Rec_TDs = sum(Touchdown, na.rm = TRUE),
                     Air_TDs = sum(as.numeric(YardsAfterCatch == 0) * Touchdown, na.rm = TRUE),
                     Air_TD_Rate = Air_TDs / Rec_TDs,
                     TD_to_INT = Rec_TDs / Rec_INTs,
                     TD_per_Tgt = Rec_TDs / Targets,
                     Air_TD_per_Tgt = Air_TDs / Targets,
                     TD_per_Rec = Rec_TDs / Receptions,
                     Air_TD_per_Rec = Air_TDs / Receptions,
                     TD_per_Drive = Rec_TDs / Rec_Drives,
                     Air_TD_per_Drive = Air_TDs / Rec_Drives,
                     Rec_INTs_per_Tgt = Rec_INTs / Targets,
                     Rec_INTs_per_Rec = Rec_INTs / Receptions,
                     Rec_INTs_per_Drive = Rec_INTs / Rec_Drives,
                     Rec_EPA = sum(EPA, na.rm = TRUE),
                     Rec_EPA_per_Tgt = Rec_EPA / Targets,
                     Rec_EPA_per_Rec = Rec_EPA / Receptions,
                     Rec_EPA_per_Drive = Rec_EPA / Rec_Drives,
                     Rec_Success_Rate = length(which(EPA > 0)) / Targets,
                     Caught_EPA = sum(Reception * EPA, na.rm = TRUE),
                     Caught_EPA_per_Tgt = Caught_EPA / Targets,
                     Caught_EPA_per_Rec = Caught_EPA / Receptions,
                     Caught_EPA_per_Drive = Caught_EPA / Rec_Drives,
                     EPA_Rec_Perc = Caught_EPA / sum(abs(EPA), na.rm = TRUE),
                     Rec_WPA = sum(WPA, na.rm = TRUE),
                     Rec_WPA_per_Tgt = Rec_WPA / Targets,
                     Rec_WPA_per_Rec = Rec_WPA / Receptions,
                     Rec_WPA_per_Drive = Rec_WPA / Rec_Drives,
                     Rec_Win_Rate = length(which(WPA > 0)) / Targets,
                     Caught_WPA = sum(Reception * WPA, na.rm = TRUE),
                     Caught_WPA_per_Tgt = Caught_WPA / Targets,
                     Caught_WPA_per_Rec = Caught_WPA / Receptions,
                     Caught_WPA_per_Drive = Caught_WPA / Rec_Drives,
                     WPA_Rec_Perc = Caught_WPA / sum(abs(WPA), na.rm = TRUE),
                     Rec_Clutch_EPA = sum(EPA*abs(WPA), na.rm = TRUE),
                     Rec_Clutch_EPA_per_Tgt = Rec_Clutch_EPA / Targets,
                     Rec_Clutch_EPA_per_Rec = Rec_Clutch_EPA / Receptions,
                     Rec_Clutch_EPA_per_Drives = Rec_Clutch_EPA / Rec_Drives,
                     Total_airEPA = sum(Reception*airEPA,na.rm=TRUE),
                     airEPA_per_Tgt = Total_airEPA / Targets,
                     airEPA_per_Rec = Total_airEPA / Receptions,
                     airEPA_per_Drive = Total_airEPA / Rec_Drives,
                     air_Success_Rate = length(which(airEPA>0)) / Targets,
                     air_Rec_Success_Rate = length(which((Reception*airEPA)>0)) / Targets,
                     Total_airWPA = sum(Reception*airWPA,na.rm=TRUE),
                     airWPA_per_Tgt = Total_airWPA / Targets,
                     airWPA_per_Rec = Total_airWPA / Receptions,
                     airWPA_per_Drive = Total_airWPA / Rec_Drives,
                     air_Win_Rate = length(which(airWPA>0)) / Targets,
                     air_Rec_Win_Rate = length(which((Reception*airWPA)>0)) / Targets,
                     Total_yacEPA = sum(Reception*yacEPA,na.rm=TRUE),
                     yacEPA_per_Tgt = Total_yacEPA / Targets,
                     yacEPA_per_Rec = Total_yacEPA / Receptions,
                     yacEPA_per_Drive = Total_yacEPA / Rec_Drives,
                     yac_Success_Rate = length(which((Reception*yacEPA)>0)) / Targets,
                     Total_yacWPA = sum(Reception*yacWPA,na.rm=TRUE),
                     yacWPA_per_Tgt = Total_yacWPA / Targets,
                     yacWPA_per_Rec = Total_yacWPA / Receptions,
                     yacWPA_per_Drive = Total_yacWPA / Rec_Drives,
                     yac_Win_Rate = length(which((Reception*yacWPA)>0)) / Targets) %>%
    return

}

#' Calculate Rushing Statistics for Given Splits
#'
#' @param rush_pbp Data frame of rushing plays.
#' @param splits String vector of variables to split on, default
#' is "Rusher_ID_Name" (splits are levels to calculate the
#' statistics for).
#' @return Data frame of rushing statistics evaluated at the
#' given level of splits.
#' @examples
#' # Calculate receiving statistics :
#' model_data_list <- calculate_rushing_splits(model_data_list$pass_model_df,
#'                                               "Rusher_ID_Name")
#' @export

calculate_rushing_splits <- function(rush_pbp, splits = "Rusher_ID_Name") {

  # Turn the splits into the group variables:
  split_groups <- lapply(splits, as.symbol)

  # Create the Game_Drive column:
  rush_pbp %>%
    dplyr::mutate(Game_Drive = paste(as.character(GameID),
                                     as.character(Drive), sep = "-")) %>%
    # Group by the given splits:
    dplyr::group_by_(.dots = split_groups) %>%
    # Calculate the passing statistics at the given splits level:
    dplyr::summarise(Rushes = n(),
                     Sacks = sum(Sack, na.rm = TRUE),
                     Rush_Drives = n_distinct(Game_Drive),
                     Rush_Yards_Gained = sum(Yards.Gained, na.rm = TRUE),
                     Sack_Yards_Lost = sum(Sack * Yards.Gained, na.rm = TRUE),
                     Rush_Yards_per_Rush = Rush_Yards_Gained / Rushes,
                     Rush_Yards_per_Drive = Rush_Yards_Gained / Rush_Drives,
                     Rush_TDs = sum(Touchdown, na.rm = TRUE),
                     Rush_TD_per_Rush = Rush_TDs / Rushes,
                     Rush_TD_per_Drive = Rush_TDs / Rush_Drives,
                     Rush_EPA = sum(EPA, na.rm = TRUE),
                     Rush_EPA_per_Rush = Rush_EPA / Rushes,
                     Rush_EPA_per_Drive = Rush_EPA / Rush_Drives,
                     Rush_Success_Rate = length(which(EPA>0)) / Rushes,
                     Pos_EPA_Ratio = sum(as.numeric(EPA > 0) * EPA, na.rm = TRUE) / sum(abs(EPA), na.rm = TRUE),
                     Rush_WPA = sum(WPA, na.rm = TRUE),
                     Rush_WPA_per_Rush = Rush_WPA / Rushes,
                     Rush_WPA_per_Drive = Rush_WPA / Rush_Drives,
                     Rush_Win_Rate = length(which(WPA>0)) / Rushes,
                     Pos_WPA_Ratio = sum(as.numeric(WPA > 0) * WPA, na.rm = TRUE) / sum(abs(WPA), na.rm = TRUE),
                     Fumbles = sum(Fumble,na.rm = TRUE),
                     Fumbles_per_Rush = Fumbles / Rushes,
                     Fumbles_per_Drive = Fumbles / Rush_Drives,
                     Rush_Clutch_EPA = sum(EPA*abs(WPA),na.rm=TRUE),
                     Rush_Clutch_EPA_per_Rush = Rush_Clutch_EPA / Rushes,
                     Rush_Clutch_EPA_per_Drive = Rush_Clutch_EPA / Rush_Drives) %>%
    return

}


