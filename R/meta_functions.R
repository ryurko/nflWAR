#' Evaluate the Discrimination and Stability for Given Metrics
#'
#' @param boot_seasons Data frame of bootstrapped statistics
#' with the provided player_col, season_col, and stat_cols.
#' @param stat_seasons Data frame of statistics with the
#' provided player_col, season_col, and stat_cols.
#' @param player_col String indicating the name of the column
#' in the data sets standing for the player name.
#' @param season_col String indicating the name of the column
#' in the data sets standing for the season.
#' @param attempt_col String indicating the name of the column
#' to filter the data on.
#' @param min_attempts Number of minimum of the attempt_col
#' that a player must have to be included.
#' @param stat_cols String vector of the statistics for which
#' the discrimination and stability will be calculated for.
#' @return Data frame with three columns (1) Statistic - which
#' is the name of one of the stats, (2) Discrimination - the
#' average discrimination of the Statistic over all provided
#' seasons, (3) Stability - the stability for the Statistic
#' over all the provided seasons.
#' @examples
#' # Calculate the discrimination and stability for QB stats:
#' qb_reliability <-
#' measure_meta_reliability(qb_boot_seasons,qb_seasons,
#' "Player_ID_Name", "Season", "Total_Plays", 100,
#' c("total_WAR", "Comp_Perc", "Success_Rate"))

measure_meta_reliability <- function(boot_seasons, stat_seasons,
                                     player_col, season_col, attempt_col,
                                     min_attempts, stat_cols) {

  # Change the player_col to be Player, and season_col to be Season:
  colnames(boot_seasons)[which(colnames(boot_seasons) == player_col)] <- "Player"
  colnames(stat_seasons)[which(colnames(stat_seasons) == player_col)] <- "Player"
  colnames(boot_seasons)[which(colnames(boot_seasons) == season_col)] <- "Season"
  colnames(stat_seasons)[which(colnames(stat_seasons) == season_col)] <- "Season"

  # Filter to the players with the minimum number of attempts:
  boot_attempt_col_i <- which(colnames(boot_seasons) == attempt_col)
  stat_attempt_col_i <- which(colnames(stat_seasons) == attempt_col)

  boot_seasons <- boot_seasons[which(boot_seasons[[boot_attempt_col_i]] >= min_attempts),]
  stat_seasons <- stat_seasons[which(stat_seasons[[stat_attempt_col_i]] >= min_attempts),]

  # Grab only the needed columns:
  stat_seasons <- stat_seasons[,which(colnames(stat_seasons) %in% c("Season", "Player", stat_cols))]
  boot_seasons <- boot_seasons[,which(colnames(boot_seasons) %in% c("Season", "Player", stat_cols))]

  # Reshape the stat_seasons data frame so that each row is a combination
  # of a Season, Player, and Statistic:
  reshape_season_stats <- stat_seasons %>%
    tidyr::gather(Metric, Value, -Season, -Player)

  # Find the player variances:
  player_variances <- stat_seasons %>%
    dplyr::group_by(Player) %>%
    dplyr::select(-Season) %>%
    dplyr::summarise_all(var, na.rm = TRUE) %>%
    tidyr::gather(Metric, Player_Variance, -Player)

  # Find the total variances:
  total_variances<- stat_seasons %>%
    dplyr::select(-Season, -Player) %>%
    dplyr::summarise_all(var, na.rm = TRUE) %>%
    tidyr::gather(Metric, Total_Variance)

  # Find the bootstrap season variance for each player, then reshape:
  boot_variances <- boot_seasons %>%
    dplyr::group_by(Season, Player) %>%
    dplyr::summarise_all(var, na.rm = TRUE) %>%
    tidyr::gather(Metric, Boot_Variance, -Season, -Player)

  # Now join the tables to the reshape_season_stats:
  reshape_season_stats <- reshape_season_stats %>%
    dplyr::left_join(player_variances, by = c("Player", "Metric")) %>%
    dplyr::left_join(total_variances, by = "Metric") %>%
    dplyr::left_join(boot_variances, by = c("Season", "Player", "Metric"))

  # Now calculate the discrimination values:
  discrimination_table <- reshape_season_stats %>%
    dplyr::group_by(Season, Metric) %>%
    dplyr::summarise(Season_Discrimination = 1 - mean(Boot_Variance, na.rm= TRUE) / var(Value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Metric) %>%
    dplyr::summarise(Discrimination = mean(Season_Discrimination))

  # Now the stability values:
  stability_table <- reshape_season_stats %>%
    dplyr::group_by(Metric) %>%
    dplyr::summarise(Stability = 1 - (mean(Player_Variance - Boot_Variance,na.rm=TRUE)) / (dplyr::first(Total_Variance) - mean(Boot_Variance, na.rm= TRUE))) %>%
    dplyr::ungroup()

  # Return the tables as a list:
  return(list("Discrimination_Table" = discrimination_table,
              "Stability_Table" = stability_table))

}

#' Generate the Independence R-Squared Matrix for Given Metrics
#'
#' @param stat_seasons Data frame of statistics with the
#' provided player_col, season_col, and stat_cols.
#' @param attempt_col String indicating the name of the column
#' to filter the data on.
#' @param min_attempts Number of minimum of the attempt_col
#' that a player must have to be included.
#' @param stat_cols String vector of the statistics for which
#' the independence matrix will be generated for.
#' @return The independence R-squared matrix.
#' @examples
#' qb_independence <- measure_meta_independence(qb_seasons,
#' "Total_Plays", 100, c("total_WAR", "Comp_Perc", "Success_Rate"))

measure_meta_independence <- function(stat_seasons, attempt_col,
                                      min_attempts, stat_cols) {

  # Filter to the players with the minimum number of attempts:
  stat_attempt_col_i <- which(colnames(stat_seasons) == attempt_col)

  stat_seasons <- stat_seasons[which(stat_seasons[[stat_attempt_col_i]] >= min_attempts),]

  # Grab only the needed columns:
  stat_seasons <- stat_seasons[,which(colnames(stat_seasons) %in% stat_cols)]

  # The following code is by Alex Franks, taken directly from his Github:

  condVar <- function(cov, varCols, condCols=NULL) {
    if(is.null(condCols)) {
      condCols <- setdiff(colnames(cov), varCols)
    }
    allCols <- union(varCols, condCols)
    cov <- cov[allCols, allCols]
    cov[varCols, varCols] - cov[varCols, condCols] %*% solve(cov[condCols, condCols]) %*% cov[condCols, varCols]
  }

  res <- sbgcop::sbgcop.mcmc(stat_seasons)
  C <- apply(res$C.psamp, c(1,2), mean)
  imputedTab <- res$Y.pmean

  ## Generate R-squared matrix
  RsquaredMatrix <- matrix(1, nrow=ncol(C), ncol=ncol(C))
  rownames(RsquaredMatrix) <- colnames(C)
  for(metric in colnames(C)) {
    print(metric)

    count <- ncol(C)
    removedSet <- c(metric)
    rsq <- condVar(C, metric, setdiff(colnames(C), c(removedSet)))
    RsquaredMatrix[metric, count] <- rsq
    count <- count - 1

    while(count > 1) {

      idx <- which.max(sapply(setdiff(colnames(C), removedSet), function(x) condVar(C, metric, setdiff(colnames(C), c(removedSet, x)))))
      nextOut <- setdiff(colnames(C), removedSet)[idx]
      removedSet <- c(removedSet, nextOut)
      rsq <- condVar(C, metric, setdiff(colnames(C), removedSet))
      RsquaredMatrix[metric, count] <- rsq
      count <- count - 1
    }
  }


  # And return the result:
  return(RsquaredMatrix)
}




