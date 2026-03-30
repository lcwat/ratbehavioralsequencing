# this script takes the coded rat behavioral data and codes the behaviors into
# unique identifying symbols that can be plugged into the determinism algorithm

# 3/29/2026

# Luke Watson


# load libraries ----------------------------------------------------------

library(tidyverse)

source('scripts/determinism.R')
source('scripts/plot_recurrence.R')
#
# load data ---------------------------------------------------------------

# list files
path <- 'data/cleaned/'
file_list <- list.files(path)

# read_csv(paste0(path, file_list[1]))

det_df <- tibble(
  animal_id = character(), 
  group_id = character(), 
  phase = character(), 
  det = numeric()
)

# read in file, summarize and clean, calc det, then get out
for(file in file_list) {
  df <- read_csv(paste0(path, file))
  
  seq <- df |> 
    group_by(behavior_id) |> 
    summarize(
      behavior = unique(behavior)
    ) |> 
    mutate(
      int_behavior = case_when(
        behavior == 'upper_ob' ~ 1, 
        behavior == 'standing' ~ 2,
        behavior == 'lower_ob' ~ 3,
        behavior == 'resting' ~ 4, 
        behavior == 'frozen' ~ 5, 
        behavior == 'None' ~ NA
      )
    ) |> 
    drop_na() |> # remove absence of behavior
    mutate(
      repeated = if_else(
        dplyr::lag(int_behavior, default = 0) == int_behavior, 
        T, 
        F
      )
    ) |>
    dplyr::filter(repeated != T) |> 
    pull(int_behavior) # grab sequence
  
  # calc det
  d <- determinism(seq, 3)
  
  # add to tibble
  det_df_add <- tibble(
    animal_id = df[1,]$animal_id, 
    group_id = df[1,]$group_id, 
    phase = df[1,]$phase, 
    det = d
  )
  
  det_df <- det_df |> 
    rbind(det_df_add)
  
}

det_df |> 
  ggplot(aes(x = det, fill = group_id)) +
  geom_density(alpha = .5, color = NA) + 
  scale_fill_manual(values = c('firebrick', 'goldenrod')) +
  xlim(0, 1) +
  theme_minimal()

df |> 
  distinct(behavior)

behavior_summary <- df |> 
  group_by(behavior_id) |> 
  summarize(
    behavior = unique(behavior), 
    count = n()
  ) |> 
  mutate(
    int_behavior = case_when(
      behavior == 'upper_ob' ~ 1, 
      behavior == 'standing' ~ 2,
      behavior == 'lower_ob' ~ 3,
      behavior == 'resting' ~ 4, 
      behavior == 'frozen' ~ 5, 
      behavior == 'None' ~ NA
    )
  ) 


# clean and det -----------------------------------------------------------

# remove no behavior chunks
behav_seq <- behavior_summary |> 
  drop_na() |> 
  mutate(
    repeated = if_else(
      dplyr::lag(int_behavior, default = 0) == int_behavior, 
      T, 
      F
    )
  ) |>
  dplyr::filter(repeated != T) |> 
  pull(int_behavior)

# remove back to back 

determinism(behav_seq, 3)

# plot
plot_recurrence(behav_seq)

