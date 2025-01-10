#' Prepare a brood table for analysis
#'
#' Function to select ages of interest from the brood table, remove missing values and calculate logs and log(R/S).
#'
#' @param brood data frame with the full brood table including columns:\describe{
#'  \item{byr}{brood year}
#'  \item{S}{spawning escapement}
#'  \item{age#}{the total return of age # fish following the naming convention age#}}
#' @param age_range a vector with the ages of interest.
#'
#' @return a data frame
#'
#' @examples
#' prep_brood(deshka, 4:6)
#' 
#' @export
prep_brood <- function(brood, age_range){
  select_var <- paste0("age", age_range)
  filter_var <- as.name(select_var[length(select_var)])
  
  temp <- 
    brood %>%
    dplyr::select(byr, S, !!!select_var) 
  
  retain <- complete.cases(temp)
  retain[tail(which(complete.cases(temp)), 1) + 1] <- TRUE
  
  temp %>%
    dplyr::filter(retain) %>%
    dplyr::mutate(lnRS = log((!!filter_var) / S)) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("age"), list(ln = log)))
}
