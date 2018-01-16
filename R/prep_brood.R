#' Prepare a brood table for analysis
#'
#' Function to select ages of interest from the brood table, remove missing values and calculate logs and log(R/S).
#'
#' @param brood dataframe the full brood table
#' @param age_range a vector with the ages of interest
#'
#' @return a dataframe
#'
#' @examples
#' prep_brood(brood, 4:6)
#'
#' @export
prep_brood <- function(brood, age_range){
  select_var <- paste0("age", age_range)
  filter_var <- as.name(select_var[length(select_var)])
  brood %>%
    dplyr::select(byr, S, !!!select_var) %>%
      dplyr::filter(complete.cases(.)) %>%
      dplyr::mutate(lnRS = log((!!filter_var) / S)) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::starts_with("age")), dplyr::funs(ln = log))
}
