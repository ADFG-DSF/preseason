# Create a Deshka Brood table based on the most recent SusitnaEG posterior. Should update prior to forecast.
# N.tas, or abundance by calendar year, age and stock is required but was not monitored, calculated here instead.
library(magrittr)
post <- readRDS("H:\\My Documents\\SusitnaEG\\posts\\SuChinook_DsumS_ababd3a.rds")
Y <- 39
A <- 4
N.tas.sim <- array(NA, dim = c(2250, 39, 4, 5))
for (sim in 1:2250){
  for (stock in 1:5){
    for (a in 1:A) {
      for (c in a:(Y + (a - 1))) {
        N.tas.sim[sim, c - (a - 1), (A + 1 - a), stock] <- post$sims.list$p[sim, c, (A + 1 - a)] * post$sims.list$R[sim, c, stock]
      }
    }
  }
}

post$mean$N.tas <- apply(N.tas.sim, c(2, 3, 4), mean)
year_id <- as.character(1979:2017)
deshka <- 
  SusitnaEG::table_brood(post, 1) %>%
  dplyr::filter(year >= 1979) %>%
  dplyr::select(byr = year, S, dplyr::starts_with("age"))
devtools::use_data(deshka, overwrite = TRUE)