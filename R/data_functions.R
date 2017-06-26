#' Deflation using GitHub-based CSV file
#'
#' @param data_to_deflate A data frame
#' @param money_var The quoted name of the dollar-value variable
#' @param fy_var The quoted name of the fiscal year variable
#' @param deflator_file The quoted file name of the deflators to use;
#' must be a CSV with the columns "FY" and "Deflator."
#' @param path The path or url for the deflator_file CSV.  By default, checks
#' the CSISdefense Github lookups repository at CSISdefense/hamre_lookups/master
#'
#' @return Returns a data frame with the money_var deflated, otherwise identical
#' to the original data frame
#'
#' @section Warning: This function should be used __in data processing only__,
#' not in a live app.  It reads an external file from GitHub,
#' which will slow down an app substantially if done repeatedly.
#'
#' @examples RDTE_data <- deflate(
#'   data_to_deflate = RDTE_data,
#'   money_var = "Millions",
#'   fy_var = "fiscal_year")
#'
#' @import dplyr
#' @export
deflate <- function(
  data_to_deflate,
  money_var = "Amount",
  fy_var = "Fiscal.Year",
  deflator_file = "2016_deflators_actuals.csv",
  path = "https://raw.githubusercontent.com/CSISdefense/hamre_lookups/master/"){

  cat(paste("\n Applying\n", deflator_file, "\n from\n", path, "\n"))
  deflators_retrieved <- readr::read_csv(paste0(path, deflator_file))

  deflators <- deflators_retrieved$Deflator
  names(deflators) <- deflators_retrieved$FY

  data_to_deflate[[money_var]] <- as.numeric(as.character(
    data_to_deflate[[money_var]])) /
    deflators[as.character(data_to_deflate[[fy_var]])]

  return(data_to_deflate)
}


