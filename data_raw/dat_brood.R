brood0 <-
readxl::read_excel("H:\\My Documents\\Deshka R, Chinook and Coho\\Forecast\\2018\\Copy of Deshka forecast 2018_ND.xlsx",
                   range = "brood (2017)!A6:F46",
                   col_names = c("byr", "age3", "age4", "age5", "age6", "age7")) %>%
  dplyr::mutate_all(round)

spawn <-
  readxl::read_excel("H:\\My Documents\\Deshka R, Chinook and Coho\\Forecast\\2018\\Copy of Deshka forecast 2018_ND.xlsx",
                     range = "brood (2017)!J6:K49",
                     col_names = c("byr", "S")) %>%
  dplyr::mutate_all(round)

deshka <- dplyr::left_join(spawn, brood0, by = "byr") %>% print(n = 100)
devtools::use_data(deshka, overwrite = TRUE)
