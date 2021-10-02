library(dplyr)
library(readr)
library(magrittr)

read_data <- function(folder_path) {
  dm_path <- file.path(folder_path, "DM.csv")
  pc_path <- file.path(folder_path, "PC.csv")
  hp_path <- file.path(folder_path, "HP.csv")
  
  pc_data <- readr::read_csv(pc_path)
  dm_data <- readr::read_csv(dm_path)
  
  
  # hp structure:
  # character_name, dice_faces
  
  
  hp_data <- readr::read_csv(hp_path)
  # dm structure:
  # level, choice
  
  # player structure:
  # level, character_name, choice
  
  combo <- pc_data %>% 
    inner_join(dm_data, by = "level", suffix = c("", "_dm")) %>%
    inner_join(hp_data, by = "character_name", suffix = c("", "_hp"))
  
  combo %<>%
    mutate(
      added_roll = choice + choice_dm,
      modulo_roll = (added_roll %% dice_faces) + 1,
      final_roll  = dplyr::case_when(
        level == 1                   ~ dice_faces,
        modulo_roll < dice_faces / 2 ~ dice_faces / 2,
        TRUE                         ~ modulo_roll
        )
      )
  
}