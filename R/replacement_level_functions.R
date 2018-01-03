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
#' find_positional_replacement_level(replacement_functions)
#' @export

find_positional_replacement_level <- function(model_data_list,
                                              replacement_fns) {

  # Use of the corresponding replacement level functions with their
  # associated position tables, creating a vector of Player_ID_Names
  # for each position:
  replacement_QBs <- replacement_fns$find_replacement_QB(model_data_list$pass_model_df)
  replacement_RBs_rec <- replacement_fns$find_replacement_RB_rec(model_data_list$pass_model_df)
  replacement_WRs_rec <- replacement_fns$find_replacement_WR_rec(model_data_list$pass_model_df)
  replacement_TEs_rec <- replacement_fns$find_replacement_TE_rec(model_data_list$pass_model_df)
  replacement_RBs_rush <- replacement_fns$find_replacement_RB_rush(model_data_list$rush_model_df)
  replacement_WR_TE_rush <- replacement_fns$find_replacement_WR_TE_rush(model_data_list$rush_model_df)

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
                                                   "Replacement_WR_TE_rush", Rusher_ID_Name)))

  # Create the Replacement_Level indicator columns each in positional table:
  model_data_list$QB_table <- model_data_list$QB_table %>%
    dplyr::mutate(Replacement_Level = ifelse(Player_ID_Name %in% replacement_QBs, 1, 0))
  model_data_list$RB_table <- model_data_list$RB_table %>%
    dplyr::mutate(Replacement_Level_Rusher = ifelse(Player_ID_Name %in% replacement_RBs_rush, 1, 0),
           Replacement_Level_Receiver = ifelse(Player_ID_Name %in% replacement_RBs_rec, 1, 0))
  model_data_list$WR_table <- model_data_list$WR_table %>%
    dplyr::mutate(Replacement_Level_Receiver = ifelse(Player_ID_Name %in% replacement_WRs_rec, 1, 0),
           Replacement_Level_Rusher = ifelse(Player_ID_Name %in% replacement_WR_TE_rush, 1, 0))
  model_data_list$TE_table <- model_data_list$TE_table %>%
    dplyr::mutate(Replacement_Level_Receiver = ifelse(Player_ID_Name %in% replacement_TEs_rec, 1, 0),
           Replacement_Level_Rusher = ifelse(Player_ID_Name %in% replacement_WR_TE_rush, 1, 0))

  # Return the updated data:
  return(model_data_list)
}

#' Create a Function to Find Replacement Level Based on the Team's Depth
#'
#' @param replacement_depth Number indicating which rank on team's depth chart the
#' replacement level can at most be.
#' @param positions String indicating which position(s) to find the replacement level for,
#' can only be (1) "QB", (2) "RB", (3) "WR", (4) "TE", or (5) "FB".
#' @param pbp_type String indicating which type of play-by-play data to use, either use
#' (1) "pass" and (2) "rush".
#' @return Function that returns a vector of Player_ID_Name values indicating the
#' replacement level for a given play-by-play dataset based on the roster_rank to
#' decide the team depth chart cutoff, as well as the position(s) to search for,
#' and the type of the play-by-play data that will be used.
#' @examples
#' # Create the replacement function for QBs using based on 3rd or higher on the
#' depth chart using the passing play-by-play data:
#' find_replacement_QB <- create_roster_replacement_fn(3, "QB", "pass")
#' replacement_QBs <- find_replacement_QB(model_data_list$pass_model_df)
#' @export

create_roster_replacement_fn <- function(replacement_depth, positions, pbp_type) {
  # ADD ASSERTIONS FOR THE INPUT

  # First case is QBs:
  if ("QB" %in% positions) {
    replacement_function <- function(pbp_data) {
      replacement_qbs <- dplyr::filter(pbp_data) %>%
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
    replacement_function <- function(pbp_data) {
      replacement_rush <- dplyr::filter(pbp_data, Rusher_Position %in% positions) %>%
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
      replacement_function <- function(pbp_data) {
        replacement_rec <- dplyr::filter(pbp_data, Receiver_Position %in% positions) %>%
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
