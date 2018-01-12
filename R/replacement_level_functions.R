#' Creates Player Tables for Each Position
#'
#' @param model_data_list List of two data frames (1) passing
#' play-by-play (pass_model_df) and (2) rushing play-by-play
#' (rush_model_df).
#' @return List of dataframes with the given model_data_list,
#' as well as the tables for each position containing the
#' player names/IDs along with their respective number of
#' attempts for both passing/receiving and rushing/sacks.
#' @examples
#' # Add the player tables to the model_data_list:
#' model_data_list <- model_data_list %>% add_position_tables()
#' @export

add_position_tables <- function(model_data_list) {
  # INSERT ASSERT HERE REGARDING MODEL_DATA_LIST

  # Create the position tables using the fact that model_data_list
  # follows the assumed format with pass_model_df and rush_model_df.

  # QB table:
  qb_table_pass <- model_data_list$pass_model_df %>%
    dplyr::group_by(Passer_ID_Name) %>%
    dplyr::summarise(Pass_Attempts = n())
  qb_table_rush <- dplyr::filter(model_data_list$rush_model_df,
                                 Rusher_Position == "QB") %>%
    dplyr::group_by(Rusher_ID_Name) %>%
    dplyr::summarise(Rush_Attempts = n() - sum(Sack),
                     Sacks = sum(Sack)) %>%
    dplyr::rename(Passer_ID_Name = Rusher_ID_Name)
  qb_table <- qb_table_pass %>%
    dplyr::full_join(qb_table_rush, by = "Passer_ID_Name") %>%
    dplyr::rename(Player_ID_Name = Passer_ID_Name) %>%
    dplyr::mutate(Position = "QB",
                  Pass_Attempts = ifelse(is.na(Pass_Attempts),
                                         0, Pass_Attempts),
                  Rush_Attempts = ifelse(is.na(Rush_Attempts),
                                         0, Rush_Attempts),
                  Sacks = ifelse(is.na(Sacks), 0, Sacks),
                  Total_Plays = Pass_Attempts + Rush_Attempts + Sacks)

  # RB table:
  rb_table_rec <- dplyr::filter(model_data_list$pass_model_df,
                                Receiver_Position %in% c("RB","FB")) %>%
    dplyr::group_by(Receiver_ID_Name) %>%
    dplyr::summarise(Receptions = sum(Reception),
                     Targets = n()) %>%
    dplyr::rename(Rusher_ID_Name = Receiver_ID_Name)
  rb_table_rush <- dplyr::filter(model_data_list$rush_model_df,
                                 Rusher_Position %in% c("RB","FB")) %>%
    dplyr::group_by(Rusher_ID_Name) %>%
    dplyr::summarise(Rush_Attempts = n())
  rb_table <- rb_table_rec %>%
    dplyr::full_join(rb_table_rush, by = "Rusher_ID_Name") %>%
    dplyr::rename(Player_ID_Name = Rusher_ID_Name) %>%
    dplyr::mutate(Position = "RB",
                  Receptions = ifelse(is.na(Receptions),
                                      0, Receptions),
                  Targets = ifelse(is.na(Targets),
                                   0, Targets),
                  Rush_Attempts = ifelse(is.na(Rush_Attempts),
                                         0, Rush_Attempts),
                  Total_Plays = Receptions + Targets + Rush_Attempts)

  # WR table:
  wr_table_rec <- dplyr::filter(model_data_list$pass_model_df,
                                Receiver_Position %in% c("WR")) %>%
    dplyr::group_by(Receiver_ID_Name) %>%
    dplyr::summarise(Receptions = sum(Reception),
                     Targets = n())
  wr_table_rush <- dplyr::filter(model_data_list$rush_model_df,
                                 Rusher_Position %in% c("WR")) %>%
    dplyr::group_by(Rusher_ID_Name) %>%
    dplyr::summarise(Rush_Attempts = n()) %>%
    dplyr::rename(Receiver_ID_Name = Rusher_ID_Name)
  wr_table <- wr_table_rec %>%
    dplyr::full_join(wr_table_rush, by = "Receiver_ID_Name") %>%
    dplyr::rename(Player_ID_Name = Receiver_ID_Name) %>%
    dplyr::mutate(Position = "WR",
                  Receptions = ifelse(is.na(Receptions),
                                      0, Receptions),
                  Targets = ifelse(is.na(Targets),
                                   0, Targets),
                  Rush_Attempts = ifelse(is.na(Rush_Attempts),
                                         0, Rush_Attempts),
                  Total_Plays = Receptions + Targets + Rush_Attempts)

  # TE table:
  te_table_rec <- dplyr::filter(model_data_list$pass_model_df,
                                Receiver_Position %in% c("TE")) %>%
    dplyr::group_by(Receiver_ID_Name) %>%
    dplyr::summarise(Receptions = sum(Reception),
                     Targets = n())
  te_table_rush <- dplyr::filter(model_data_list$rush_model_df,
                                 Rusher_Position %in% c("TE")) %>%
    dplyr::group_by(Rusher_ID_Name) %>%
    dplyr::summarise(Rush_Attempts = n()) %>%
    dplyr::rename(Receiver_ID_Name = Rusher_ID_Name)
  te_table <- te_table_rec %>%
    dplyr::full_join(te_table_rush, by = "Receiver_ID_Name") %>%
    dplyr::rename(Player_ID_Name = Receiver_ID_Name) %>%
    dplyr::mutate(Position = "TE",
                  Receptions = ifelse(is.na(Receptions),
                                      0, Receptions),
                  Targets = ifelse(is.na(Targets),
                                   0, Targets),
                  Rush_Attempts = ifelse(is.na(Rush_Attempts),
                                         0, Rush_Attempts),
                  Total_Plays = Receptions + Targets + Rush_Attempts)


  # Create the final list to return:
  append(model_data_list,
        list("QB_table" = qb_table,
             "RB_table" = rb_table,
             "WR_table" = wr_table,
             "TE_table" = te_table)) %>%
    return
}

#' Find Replacement Level for Each Position
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
#' @param replacement_fns List of functions:
#' \itemize{
#' \item{"find_replacement_QB"} - Function to find replacement
#' level QBs
#' \item{"find_replacement_RB_rush"} - Function to find replacement
#' level RBs for rushing plays
#' \item{"find_replacement_WR_TE_rush"} - Function to find replacement
#' level TEs and WRs for rushing plays
#' \item{"find_replacement_WR_rec"} - Function to find replacement
#' level WRs for receiving plays
#' \item{"find_replacement_RB_rec"} - Function to find replacement
#' level RBs for receiving plays
#' \item{"find_replacement_TE_rec"} - Function to find replacement
#' level TEs for receiving plays
#' }
#' @return The input model_data_list but with replacement-level
#' players' names in the play-by-play data all set to be
#' Replacement_POSITION and a new column for each of the
#' positional tables indicating which players are replacement
#' level.
#' @examples
#' # Find positional replacement level:
#' model_data_list <- model_data_list %>%
#'   find_positional_replacement_level(replacement_functions)
#' @export

find_positional_replacement_level <- function(model_data_list,
                                              replacement_fns) {

  # Use of the corresponding replacement level functions with their
  # associated position tables, creating a vector of Player_ID_Names
  # for each position:
  replacement_QBs <- replacement_fns$find_replacement_QB(model_data_list)
  replacement_RBs_rec <- replacement_fns$find_replacement_RB_rec(model_data_list)
  replacement_WRs_rec <- replacement_fns$find_replacement_WR_rec(model_data_list)
  replacement_TEs_rec <- replacement_fns$find_replacement_TE_rec(model_data_list)
  replacement_RBs_rush <- replacement_fns$find_replacement_RB_rush(model_data_list)
  replacement_WR_TE_rush <- replacement_fns$find_replacement_WR_TE_rush(model_data_list)

  # Update the play-by-play data:
  model_data_list$pass_model_df <- model_data_list$pass_model_df %>%
    dplyr::mutate(Passer_ID_Name = ifelse(Passer_ID_Name %in% replacement_QBs,
                                          "Replacement_QB", Passer_ID_Name),
                  Receiver_ID_Name = ifelse(Receiver_ID_Name %in% replacement_RBs_rec,
                                            "Replacement_RB_rec",
                                            ifelse(Receiver_ID_Name %in% replacement_WRs_rec,
                                                   "Replacement_WR_rec",
                                                   ifelse(Receiver_ID_Name %in% replacement_TEs_rec,
                                                          "Replacement_TE_rec", Receiver_ID_Name))))

  model_data_list$rush_model_df <- model_data_list$rush_model_df %>%
    dplyr::mutate(Rusher_ID_Name = ifelse(Rusher_ID_Name %in% replacement_RBs_rush,
                                            "Replacement_RB_rush",
                                            ifelse(Rusher_ID_Name %in% replacement_WR_TE_rush,
                                                   "Replacement_WR_TE_rush",
                                                   ifelse(Rusher_ID_Name %in% replacement_QBs,
                                                          "Replacement_QB",Rusher_ID_Name))))

  # Create the Replacement_Level indicator columns each in positional table,
  # as well as a column that will be used to join the player effects from the model
  # later on for replacement level (Player_Model_ID):
  model_data_list$QB_table <- model_data_list$QB_table %>%
    dplyr::mutate(Replacement_Level = ifelse(Player_ID_Name %in% replacement_QBs, 1, 0),
                  Player_Model_ID = ifelse(Replacement_Level == 1, "Replacement_QB",
                                           Player_ID_Name))

  model_data_list$RB_table <- model_data_list$RB_table %>%
    dplyr::mutate(Replacement_Level_Rusher = ifelse(Player_ID_Name %in% replacement_RBs_rush, 1, 0),
           Replacement_Level_Receiver = ifelse(Player_ID_Name %in% replacement_RBs_rec, 1, 0),
           Player_Model_ID_Rush = ifelse(Replacement_Level_Rusher == 1, "Replacement_RB_rush",
                                    Player_ID_Name),
           Player_Model_ID_Rec = ifelse(Replacement_Level_Receiver == 1, "Replacement_RB_rec",
                                    Player_ID_Name))

  model_data_list$WR_table <- model_data_list$WR_table %>%
    dplyr::mutate(Replacement_Level_Receiver = ifelse(Player_ID_Name %in% replacement_WRs_rec, 1, 0),
           Replacement_Level_Rusher = ifelse(Player_ID_Name %in% replacement_WR_TE_rush, 1, 0),
           Player_Model_ID_Rec = ifelse(Replacement_Level_Receiver == 1, "Replacement_WR_rec",
                                    Player_ID_Name),
           Player_Model_ID_Rush = ifelse(Replacement_Level_Rusher == 1, "Replacement_WR_TE_rush",
                                         Player_ID_Name))

  model_data_list$TE_table <- model_data_list$TE_table %>%
    dplyr::mutate(Replacement_Level_Receiver = ifelse(Player_ID_Name %in% replacement_TEs_rec, 1, 0),
           Replacement_Level_Rusher = ifelse(Player_ID_Name %in% replacement_WR_TE_rush, 1, 0),
           Player_Model_ID_Rec = ifelse(Replacement_Level_Receiver == 1, "Replacement_TE_rec",
                                        Player_ID_Name),
           Player_Model_ID_Rush = ifelse(Replacement_Level_Rusher == 1, "Replacement_WR_TE_rush",
                                         Player_ID_Name))

  # Return the updated data:
  return(model_data_list)
}

#' Create a Function to Find Replacement Level Based on the Team's Depth
#'
#' @param replacement_depth Number indicating which rank on team's depth chart the
#' replacement level can at most be.
#' @param positions String indicating which position(s) to find the replacement level for,
#' can only be (1) "QB", (2) "RB", (3) "WR", (4) "TE", or (5) "FB".
#' @param pbp_type String indicating which type of performance to use, either use
#' (1) "pass" or (2) "rush".
#' @return Function that returns a vector of Player_ID_Name values indicating the
#' replacement level for a given play-by-play dataset based on the replacement_depth to
#' decide the team depth chart cutoff, as well as the position(s) to search for,
#' and the type of the play-by-play data that will be used.
#' @examples
#' # Create the replacement function for QBs using based on 3rd or higher on the
#' depth chart using passing performance:
#' find_replacement_QB <- create_roster_replacement_fn(3, "QB", "pass")
#' replacement_QBs <- find_replacement_QB(model_data_list)
#' @export

create_roster_replacement_fn <- function(replacement_depth, positions, pbp_type) {
  # ADD ASSERTIONS FOR THE INPUT

  # First case is QBs:
  if ("QB" %in% positions) {
    replacement_function <- function(model_data_list) {
      replacement_qbs <- dplyr::filter(model_data_list$pass_model_df) %>%
        dplyr::group_by(Passer_ID_Name,posteam) %>%
        dplyr::summarise(Attempts=n()) %>%
        dplyr::arrange(posteam,desc(Attempts)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(posteam) %>%
        dplyr::mutate(Team_QB = row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Replacement_Level = ifelse(Team_QB >= replacement_depth,
                                                 1, 0)) %>%
        dplyr::filter(Replacement_Level == 1)

      return(replacement_qbs$Passer_ID_Name)
    }

    # Next for non-QB rushing plays:
  } else if (pbp_type == "rush") {
    replacement_function <- function(model_data_list) {
      replacement_rush <- dplyr::filter(model_data_list$rush_model_df,
                                        Rusher_Position %in% positions) %>%
        dplyr::group_by(Rusher_ID_Name,posteam) %>%
        dplyr::summarise(Rusher_Attempts = n()) %>%
        dplyr::arrange(posteam, desc(Rusher_Attempts)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(posteam) %>%
        dplyr::mutate(Team_Rusher = row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Replacement_Level = ifelse(Team_Rusher >= replacement_depth,
                                                 1, 0)) %>%
        dplyr::filter(Replacement_Level == 1)

      return(replacement_rush$Rusher_ID_Name)
    }

    # Last for non-QB receiving plays:
  } else {
      replacement_function <- function(model_data_list) {
        replacement_rec <- dplyr::filter(model_data_list$pass_model_df,
                                         Receiver_Position %in% positions) %>%
          dplyr::group_by(Receiver_ID_Name,posteam) %>%
          dplyr::summarise(Targets = n()) %>%
          dplyr::arrange(posteam, desc(Targets)) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(posteam) %>%
          dplyr::mutate(Team_Receiver = row_number()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Replacement_Level = ifelse(Team_Receiver >= replacement_depth,
                                                   1, 0)) %>%
          dplyr::filter(Replacement_Level == 1)

        return(replacement_rec$Receiver_ID_Name)
      }
  }
  return(replacement_function)
}

#' Create a Function to Find Replacement Level Based on League Attempts
#'
#' @param replacement_depth Number indicating how many players of that position
#' and performance type each team should have, then every player have that will
#' be considered replacement level.
#' @param positions String indicating which position(s) to find the replacement level for,
#' can only be (1) "QB", (2) "RB" (or "FB"), (3) "WR", or (4) "TE".
#' @param attempt_type String indicating which type of attempts to use from the position
#' tables to sort the players by for finding the league replacement level.
#' @param combine_wrte Indicator for whether or not to combine the TE and WR position tables,
#' which will be primarily used for rushing attempts by WRs and TEs (default is 0).
#' @return Function that returns a vector of Player_ID_Name values indicating the
#' replacement level for the position given the league replacement_depth and attempt_type.
#' @examples
#' # Create the replacement function for RBs by stating every team has 3 RBs on their
#' # team for rushing, and every RB with less rushing attempts than the top 3*32 is
#' # considered replacement level:
#' find_replacement_RB_rush <- create_league_replacement_fn(3, "RB", "Rush_Attempts")
#' replacement_RBs <- find_replacement_RB_rush(model_data_list)
#' @export

create_league_replacement_fn <- function(replacement_depth, positions, attempt_type, combine_wrte = 0) {

  # ADD ASSERT FOR INPUT HERE

  # Paste together desc() with the attempt_type to form the arrange option:
  player_sort <- paste("desc(", attempt_type, ")", sep = "")

  # Calculate the league_depth for finding the replacement players:
  league_depth <- (replacement_depth*32) + 1

  # Create the expression for grabbing the players by sorting:

  grab_replacement_players <- . %>%
    dplyr::arrange_(.dots = as.character(player_sort)) %>%
    dplyr::slice(league_depth:n()) %>%
    dplyr::pull(Player_ID_Name)

  # First case is QBs:
  if ("QB" %in% positions) {
    replacement_function <- function(model_data_list) {

      # Sort the QB table:
      model_data_list$QB_table %>%
        grab_replacement_players %>%
        return

    }
  # Else if RB or FB:
  } else if (positions %in% c("RB", "FB")) {
    replacement_function <- function(model_data_list) {

       # Sort the RB table:
      model_data_list$RB_table %>%
        grab_replacement_players %>%
        return
    }
  # WR but don't combine with TE
  } else if ("WR" %in% positions & combine_wrte == 0) {
    replacement_function <- function(model_data_list) {
      model_data_list$WR_table %>%
        grab_replacement_players %>%
        return
    }
  # TE but don't combine with WR
  } else if ("TE" %in% positions & combine_wrte == 0) {
    replacement_function <- function(model_data_list) {
      model_data_list$TE_table %>%
        grab_replacement_players %>%
        return
    }
  # Combine WR and TE
  } else {
    replacement_function <- function(model_data_list) {
      model_data_list$TE_table %>%
        dplyr::bind_rows(model_data_list$WR_table) %>%
        grab_replacement_players %>%
        return
    }
  }

  # Return the function:
  return(replacement_function)
}


#' Create QB Table Stats with Team Information
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
#' @return A different form of the QB table that is grouped by the player and team
#' level that also includes the player's number and percentage of different attempts with
#' respect to their team.
#' @examples
#' # Create the QB team table:
#' QB_team_table <- create_QB_team_table(model_data_list)
#' @export

create_QB_team_table <- function(model_data_list) {

  # Need to do the similar grouping for the position tables for QBs but now also record
  # the possession team, first for passing then rushing and join:
  qb_table_pass <- model_data_list$pass_model_df %>%
    dplyr::group_by(Passer_ID_Name, posteam) %>%
    dplyr::summarise(Pass_Attempts = n()) %>%
    dplyr::ungroup()

  qb_table_rush <- dplyr::filter(model_data_list$rush_model_df,
                                 Rusher_Position == "QB") %>%
    dplyr::group_by(Rusher_ID_Name, posteam) %>%
    dplyr::summarise(Rush_Attempts = n() - sum(Sack),
                     Sacks = sum(Sack)) %>%
    dplyr::rename(Passer_ID_Name = Rusher_ID_Name) %>%
    dplyr::ungroup()

  qb_table <- qb_table_pass %>%
    dplyr::full_join(qb_table_rush,
                     by = c("Passer_ID_Name", "posteam")) %>%
    dplyr::rename(Player_ID_Name = Passer_ID_Name) %>%
    dplyr::mutate(Pass_Attempts = ifelse(is.na(Pass_Attempts),
                                         0, Pass_Attempts),
                  Rush_Attempts = ifelse(is.na(Rush_Attempts),
                                         0, Rush_Attempts),
                  Sacks = ifelse(is.na(Sacks), 0, Sacks),
                  Total_Plays = Pass_Attempts + Rush_Attempts + Sacks)

  # Now summarise at the the team level:
  team_qb_table <- qb_table %>%
    dplyr::group_by(posteam) %>%
    dplyr::summarise(Team_Pass_Attempts = sum(Pass_Attempts),
                     Team_Rush_Attempts = sum(Rush_Attempts),
                     Team_Sacks = sum(Sacks),
                     Team_Total_Plays = Team_Pass_Attempts + Team_Rush_Attempts + Team_Sacks)

  # Now join these columns to the qb_table and calculate the player's percentage of each,
  # then return:
  qb_table %>%
    dplyr::left_join(team_qb_table, by = "posteam") %>%
    dplyr::mutate(Perc_Pass_Attempts = Pass_Attempts / Team_Pass_Attempts,
                  Perc_Rush_Attempts = Rush_Attempts / Team_Rush_Attempts,
                  Perc_Sacks = Sacks / Team_Sacks,
                  Perc_Total_Plays = Total_Plays / Team_Total_Plays,
                  Position = "QB") %>%
    return
}

#' Create non-QB Table Stats with Team Information
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
#' @param position String indicating which position to use, can be either
#' (1) RB/FB, (2) WR, or (3) TE.
#' @return A different form of the position tables that are grouped by the player and team
#' level that also includes the player's number and percentage of different attempts with
#' respect to their team.
#' @examples
#' # Create the RB team table:
#' RB_team_table <- create_pos_team_table(model_data_list, "RB")
#' @export

create_pos_team_table <- function(model_data_list, position) {

  # Create the pass_df and rush_df based on filtering the position:
  if (position %in% c("RB", "FB")) {
    pass_df <- model_data_list$pass_model_df %>%
      dplyr::filter(Receiver_Position %in% c("RB", "FB"))
    rush_df <- model_data_list$rush_model_df %>%
      dplyr::filter(Rusher_Position %in% c("RB", "FB"))
  } else if (position == "WR") {
    pass_df <- model_data_list$pass_model_df %>%
      dplyr::filter(Receiver_Position == "WR")
    rush_df <- model_data_list$rush_model_df %>%
      dplyr::filter(Rusher_Position == "WR")
  } else {
    pass_df <- model_data_list$pass_model_df %>%
      dplyr::filter(Receiver_Position == "TE")
    rush_df <- model_data_list$rush_model_df %>%
      dplyr::filter(Rusher_Position == "TE")
  }

  # Create the two player-team tables then join together:

  rec_table <- pass_df %>%
    dplyr::group_by(Receiver_ID_Name, posteam) %>%
    dplyr::summarise(Targets = n()) %>%
    dplyr::rename(Player_ID_Name = Receiver_ID_Name) %>%
    dplyr::ungroup()

  rush_table <- rush_df %>%
    dplyr::group_by(Rusher_ID_Name, posteam) %>%
    dplyr::summarise(Rush_Attempts = n()) %>%
    dplyr::rename(Player_ID_Name = Rusher_ID_Name) %>%
    dplyr::ungroup()

  full_player_table <- rec_table %>%
      dplyr::full_join(rush_table, by = c("Player_ID_Name", "posteam")) %>%
      dplyr::mutate(Targets = ifelse(is.na(Targets),
                                           0, Targets),
                    Rush_Attempts = ifelse(is.na(Rush_Attempts),
                                           0, Rush_Attempts),
                    Total_Plays = Targets + Rush_Attempts)

  # Now summarise at the the team level:
  team_table <- full_player_table %>%
    dplyr::group_by(posteam) %>%
    dplyr::summarise(Team_Targets = sum(Targets),
                     Team_Rush_Attempts = sum(Rush_Attempts),
                     Team_Total_Plays = Team_Targets + Team_Rush_Attempts)

  # Now join these columns to the full_player_table and calculate the player's
  #percentage of each, then return:
  full_player_table <- full_player_table %>%
    dplyr::left_join(team_table, by = "posteam") %>%
    dplyr::mutate(Perc_Targets = Targets / Team_Targets,
                  Perc_Rush_Attempts = Rush_Attempts / Team_Rush_Attempts,
                  Perc_Total_Plays = Total_Plays / Team_Total_Plays)
  full_player_table$Position <- rep(position, nrow(full_player_table))
  return(full_player_table)
}



#' Find Replacement-Level QBs based on the Percentage of Plays Cutoff
#'
#' @param qb_team_table Data frame with QB attempts in relation to their team
#' @param attempt_type String indicating which type of attempts to use from the table
#' to use for the percent cutoff.
#' @param attempt_percent Percentage of the given attempt_type for QBs on their team
#' that serves as a cutoff for replacement level.
#' @return Vector of Player_ID_Name values indicating the replacement level
#' QBs given the inputs.
#' replacement level for the position given the league replacement_depth and attempt_type.
#' @examples
#' # Find the replacement level QBs based on 10% cutoff for Total_Plays:
#' replacement_QBs <- find_percentage_replacement_QB(model_data_list, "Total_Plays", .1)
#' @export

find_percentage_replacement_QB <- function(qb_team_table, attempt_type, attempt_percent) {

  # Create the filter expression to find all QBs who have at least that
  # percentage cutoff:
  filter_qbs <- paste(attempt_type, " >= ", as.character(attempt_percent), sep = "")

  # Pull the names of the QBs with at least that cutoff (this is to ensure
  # that if a QB was traded but on one team had a high percentage of plays
  # then they will not be considered replacement level):
  nfl_level_qbs <- qb_team_table %>%
    dplyr::filter_(.dots = filter_qbs) %>%
    dplyr::distinct(Player_ID_Name) %>%
    dplyr::pull(Player_ID_Name)

  # Now filter to only the replacement level QBs and return:
  qb_team_table %>%
    dplyr::filter(!(Player_ID_Name %in% nfl_level_qbs)) %>%
    dplyr::distinct(Player_ID_Name) %>%
    dplyr::pull(Player_ID_Name) %>%
    return
}

#' Creates Function to Find Percentage Based Replacement Level QBs
#'
#' @param attempt_type String indicating which type of attempts to use from the table
#' to use for the percent cutoff, can be the following options: (1) Perc_Pass_Attempts,
#' (2) Perc_Rush_Attempts, (3) Perc_Sacks, or (4) Perc_Total_Plays.
#' @param attempt_percent Percentage of the given attempt_type for QBs on their team
#' that serves as a cutoff for replacement level.
#' @return Function that only takes in the model_data_list and returns the
#' replacement level QBs.
#' @examples
#' # First create the function to find the replacement QBs based on "Perc_Total_Plays"
#' # using a cutoff of 10%:
#' find_replacement_QBs <- create_percentage_replacement_fn("Perc_Total_Plays", .1)
#' # Use this function to find the replacement level QBs:
#' replacement_QBs <- find_replacement_QBs(model_data_list)
#' @export

create_percentage_replacement_fn <- function(attempt_type, attempt_percent) {
  replacement_function <- function(model_data_list) {
    find_percentage_replacement_QB(create_QB_team_table(model_data_list), attempt_type, attempt_percent)
  }
  return(replacement_function)
}

#' Add Replacement Level Information to Simulation Data
#'
#' @param sim_data_list List of data frames in the simulation:
#' \itemize{
#' \item{"pass_model_df"} - Passing play-by-play data
#' \item{"rush_model_df"} - Rushing play-by-play data
#' \item{"QB_table"} - Table of QBs
#' \item{"RB_table"} - Table of RBs
#' \item{"WR_table"} - Table of WRs
#' \item{"TE_table"} - Table of TEs
#' }
#' @param replacement_tables List of position tables
#' that designate which players are replacement level:
#' \itemize{
#' \item{"QB_table"} - Table of QBs
#' \item{"RB_table"} - Table of RBs
#' \item{"WR_table"} - Table of WRs
#' \item{"TE_table"} - Table of TEs
#' }
#' @return The input sim_data_list but with replacement-level
#' players' names in the play-by-play data all set to be
#' Replacement_POSITION and a new column for each of the
#' positional tables indicating which players are replacement
#' level.
#' @examples
#' # Join positional replacement level information from
#' # the 2017 position tables to the simulated data:
#' sim_data_list <- sim_data_list %>%
#'   add_replacement_level_sim(replacement_tables_17)
#' @export

add_replacement_level_sim <- function(sim_data_list, replacement_tables) {

  # From the replacement_tables grab the IDs of the types of replacement
  # level players:

  replacement_QBs <- replacement_tables$QB_table %>%
    dplyr::filter(Player_Model_ID == "Replacement_QB") %>%
    dplyr::pull(Player_ID_Name)

  replacement_RBs_rush <- replacement_tables$RB_table %>%
    dplyr::filter(Player_Model_ID_Rush == "Replacement_RB_rush") %>%
    dplyr::pull(Player_ID_Name)

  replacement_RBs_rec <- replacement_tables$RB_table %>%
    dplyr::filter(Player_Model_ID_Rec == "Replacement_RB_rec") %>%
    dplyr::pull(Player_ID_Name)

  replacement_WR_TE_rush <- replacement_tables$WR_table %>%
    dplyr::bind_rows(replacement_tables$TE_table) %>%
    dplyr::filter(Player_Model_ID_Rush == "Replacement_WR_TE_rush") %>%
    dplyr::pull(Player_ID_Name)

  replacement_WRs_rec <- replacement_tables$WR_table %>%
    dplyr::filter(Player_Model_ID_Rec == "Replacement_WR_rec") %>%
    dplyr::pull(Player_ID_Name)

  replacement_TEs_rec <- replacement_tables$TE_table %>%
    dplyr::filter(Player_Model_ID_Rec == "Replacement_TE_rec") %>%
    dplyr::pull(Player_ID_Name)


  # Update the play-by-play data:
  sim_data_list$pass_model_df <- sim_data_list$pass_model_df %>%
    dplyr::mutate(Passer_ID_Name = ifelse(Passer_ID_Name %in% replacement_QBs,
                                          "Replacement_QB", Passer_ID_Name),
                  Receiver_ID_Name = ifelse(Receiver_ID_Name %in% replacement_RBs_rec,
                                            "Replacement_RB_rec",
                                            ifelse(Receiver_ID_Name %in% replacement_WRs_rec,
                                                   "Replacement_WR_rec",
                                                   ifelse(Receiver_ID_Name %in% replacement_TEs_rec,
                                                          "Replacement_TE_rec", Receiver_ID_Name))))

  sim_data_list$rush_model_df <- sim_data_list$rush_model_df %>%
    dplyr::mutate(Rusher_ID_Name = ifelse(Rusher_ID_Name %in% replacement_RBs_rush,
                                          "Replacement_RB_rush",
                                          ifelse(Rusher_ID_Name %in% replacement_WR_TE_rush,
                                                 "Replacement_WR_TE_rush",
                                                 ifelse(Rusher_ID_Name %in% replacement_QBs,
                                                        "Replacement_QB",Rusher_ID_Name))))

  # Create the Replacement_Level indicator columns each in positional table,
  # as well as a column that will be used to join the player effects from the model
  # later on for replacement level (Player_Model_ID):
  sim_data_list$QB_table <- sim_data_list$QB_table %>%
    dplyr::mutate(Replacement_Level = ifelse(Player_ID_Name %in% replacement_QBs, 1, 0),
                  Player_Model_ID = ifelse(Replacement_Level == 1, "Replacement_QB",
                                           Player_ID_Name))

  sim_data_list$RB_table <- sim_data_list$RB_table %>%
    dplyr::mutate(Replacement_Level_Rusher = ifelse(Player_ID_Name %in% replacement_RBs_rush, 1, 0),
                  Replacement_Level_Receiver = ifelse(Player_ID_Name %in% replacement_RBs_rec, 1, 0),
                  Player_Model_ID_Rush = ifelse(Replacement_Level_Rusher == 1, "Replacement_RB_rush",
                                                Player_ID_Name),
                  Player_Model_ID_Rec = ifelse(Replacement_Level_Receiver == 1, "Replacement_RB_rec",
                                               Player_ID_Name))

  sim_data_list$WR_table <- sim_data_list$WR_table %>%
    dplyr::mutate(Replacement_Level_Receiver = ifelse(Player_ID_Name %in% replacement_WRs_rec, 1, 0),
                  Replacement_Level_Rusher = ifelse(Player_ID_Name %in% replacement_WR_TE_rush, 1, 0),
                  Player_Model_ID_Rec = ifelse(Replacement_Level_Receiver == 1, "Replacement_WR_rec",
                                               Player_ID_Name),
                  Player_Model_ID_Rush = ifelse(Replacement_Level_Rusher == 1, "Replacement_WR_TE_rush",
                                                Player_ID_Name))

  sim_data_list$TE_table <- sim_data_list$TE_table %>%
    dplyr::mutate(Replacement_Level_Receiver = ifelse(Player_ID_Name %in% replacement_TEs_rec, 1, 0),
                  Replacement_Level_Rusher = ifelse(Player_ID_Name %in% replacement_WR_TE_rush, 1, 0),
                  Player_Model_ID_Rec = ifelse(Replacement_Level_Receiver == 1, "Replacement_TE_rec",
                                               Player_ID_Name),
                  Player_Model_ID_Rush = ifelse(Replacement_Level_Rusher == 1, "Replacement_WR_TE_rush",
                                                Player_ID_Name))

  # Return the updated data:
  return(sim_data_list)

}
