library(dplyr)
library(readr)
library(janitor)

process_stream <- function(comp_path, shape_path) {
  
  comp <- read_csv(comp_path, show_col_types = FALSE) %>%
    clean_names()
  
  shape <- read_csv(shape_path, show_col_types = FALSE) %>%
    clean_names()
  
  comp_clean <- comp %>%
    group_by(particle_id) %>%
    summarise(
      particle_area = first(particle_area),
      particle_weight = first(particle_weight),
      .groups = "drop"
    ) %>%
    mutate(
      particle_density = ifelse(
        particle_area == 0,
        NA,
        particle_weight / particle_area
      )
    )
  
  shape %>%
    left_join(comp_clean, by = "particle_id")
}

load_training_data <- function() {
  
  bind_rows(
    
    process_stream(
      "data/S15_Particle_Composition.csv",
      "data/S15_Particle_Shape_Factors.csv"
    ) %>%
      mutate(d_class = factor(
        "very_high",
        levels = c("low","medium","high","very_high"),
        ordered = TRUE
      )),
    
    process_stream(
      "data/S16_Particle_Composition.csv",
      "data/S16_Particle_Shape_Factors.csv"
    ) %>%
      mutate(d_class = factor(
        "medium",
        levels = c("low","medium","high","very_high"),
        ordered = TRUE
      )),
    
    process_stream(
      "data/S17_Particle_Composition.csv",
      "data/S17_Particle_Shape_Factors.csv"
    ) %>%
      mutate(d_class = factor(
        "low",
        levels = c("low","medium","high","very_high"),
        ordered = TRUE
      )),
    
    process_stream(
      "data/S19_Particle_Composition.csv",
      "data/S19_Particle_Shape_Factors.csv"
    ) %>%
      mutate(d_class = factor(
        "high",
        levels = c("low","medium","high","very_high"),
        ordered = TRUE
      ))
    
  )
}