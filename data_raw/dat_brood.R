brood0 <-
readxl::read_excel("H:\\My Documents\\Deshka R, Chinook and Coho\\Forecast\\preseason\\data_raw\\Copy of Deshka KS Brood Master.xlsx",
                   range = "Master!CH18:CM61",
                   col_names = c("byr", "age3", "age4", "age5", "age6", "age7")) %>%
  dplyr::mutate_all(round)

spawn <-
  readxl::read_excel("H:\\My Documents\\Deshka R, Chinook and Coho\\Forecast\\preseason\\data_raw\\Copy of Deshka KS Brood Master.xlsx",
                     range = "Master!CX18:Cy61",
                     col_names = c("byr", "S")) %>%
  dplyr::mutate_all(round)

deshka <- dplyr::left_join(spawn, brood0, by = "byr") %>% print(n = 100)
devtools::use_data(deshka, overwrite = TRUE)
