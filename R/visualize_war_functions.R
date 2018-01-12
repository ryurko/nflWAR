#' Create WAR Interval Plots
#'
#' @param war_tables List that must have the following with the
#' various WAR calculations:
#' \itemize{
#' \item{"QB_table"} - Table of QBs
#' \item{"RB_table"} - Table of RBs
#' \item{"WR_table"} - Table of WRs
#' \item{"TE_table"} - Table of TEs
#' }
#' @param positions Vector of positions to include in the interval
#' plots (default is all positions QB, RB/FB, WR, TE).
#' @param war_type String indicating which type of war: (1) air,
#' (2) yac, (3) rush, (4) total, or (5) all - which plots the intervals
#' for the three different ones overlaid on each other
#' @return Interval plot for the for input positions.
#' @examples
#' # Generate the interval plots for QBs total WAR:
#' create_WAR_interval_plot(war_tables, positions = "QB", "total")
#' @export
create_WAR_interval_plot <- function(war_tables,
                                     positions = c("QB", "RB", "FB", "WR", "TE"),
                                     war_type = "total") {
  # Create the pipeline expression that extracts the Player_ID_Name, Position,
  # and WAR columns from the position tables:
  extract_columns <- . %>%
    dplyr::select(Player_ID_Name, Position,
                  air_WAR, Lower_air_WAR, Upper_air_WAR,
                  yac_WAR, Lower_yac_WAR, Upper_yac_WAR,
                  rush_WAR, Lower_rush_WAR, Upper_rush_WAR,
                  total_WAR, Lower_total_WAR, Upper_total_WAR)
  # Apply this to each position:
  qb_table <- war_tables$QB_table %>% extract_columns
  rb_table <- war_tables$RB_table %>% extract_columns
  wr_table <- war_tables$WR_table %>% extract_columns
  te_table <- war_tables$TE_table %>% extract_columns
  # Bind the rows together and relevel the Player_ID_Name
  # based on the order of WAR:
  player_table <- dplyr::bind_rows(qb_table, rb_table,
                                   wr_table, te_table) %>%
    dplyr::mutate(Player_ID_Name = factor(Player_ID_Name,
                                          levels = Player_ID_Name[order(total_WAR, decreasing = TRUE)]))
  ggplot(player_table, aes(x = total_WAR, y = Player_ID_Name, color = Position)) +
    geom_point() + geom_errorbarh(aes(xmin = Lower_total_WAR,
                                      xmax = Upper_total_WAR, color = Position))
}
