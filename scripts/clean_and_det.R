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

d <- read_csv(paste0(path, file_list[1]))

summary(d)

d |> 
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
      behavior == 'frozen' ~ 4, # resting and frozen will be same cat of behavior
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
  )

# 10 minutes of video, 36000 frames, split before and after frame 18000

det_df <- tibble(
  animal_id = character(), 
  group_id = character(), 
  phase = character(), 
  overall_det = numeric(), 
  first_half_det = numeric(), 
  second_half_det = numeric()
)

# read in file, summarize and clean, calc det, then get out
for(file in file_list) {
  df <- read_csv(paste0(path, file), show_col_types = F)
  
  overall <- df |> 
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
        behavior == 'frozen' ~ 4, # resting and frozen will be same cat of behavior
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
    dplyr::filter(!repeated) |> 
    pull(int_behavior)
  
  # break up into first half and second half of session and compute separate dets
  first_half <- df |> 
    rownames_to_column('row') |> 
    dplyr::filter(row < 18000) |> 
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
        behavior == 'frozen' ~ 4, # resting and frozen will be same cat of behavior
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
    dplyr::filter(!repeated) |> 
    pull(int_behavior)
  
  second_half <- df |> 
    rownames_to_column('row') |> 
    dplyr::filter(row >= 18000) |> 
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
        behavior == 'frozen' ~ 4, # resting and frozen will be same cat of behavior
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
    dplyr::filter(!repeated) |> 
    pull(int_behavior)
  
  # calc det
  det = determinism(overall, 3)
  fir_det = determinism(first_half, 3)
  sec_det = determinism(second_half, 3)
  
  # also grab sequences and place into list?
  
  # add to tibble
  det_df_add <- tibble(
    animal_id = df[1,]$animal_id, 
    group_id = df[1,]$group_id, 
    phase = df[1,]$phase, 
    overall_det = det, 
    first_half_det = fir_det, 
    second_half_det = sec_det
  )
  
  det_df <- det_df |> 
    rbind(det_df_add)
  
  print(which(file_list == file))
}

write_csv(det_df, 'data/determinism_data/det_sample_halves_and_4_behav.csv')

det_df |> 
  ggplot(aes(x = overall_det, fill = group_id)) +
  geom_density(alpha = .5, color = NA) + 
  scale_fill_manual(values = c('firebrick', 'goldenrod')) +
  xlim(0:1) +
  theme_minimal()

ggsave(
  'fig_output/overall_determinism_by_group.png', device = 'png', 
  width = 4, height = 4, units = 'in'
)

# check out differences from 1st to 2nd half
det_df |> 
  pivot_longer(
    first_half_det:second_half_det
  ) |> 
  ggplot(
    aes(x = value, fill = as.factor(name))
  ) +
  geom_density(color = NA, alpha = .5) +
  scale_fill_manual(
    'Half', 
    labels = c('first', 'second'),
    values = c('firebrick', 'goldenrod')
  ) +
  xlim(0:1) +
  labs(x = 'Determinism') +
  theme_minimal() +
  facet_wrap(~group_id)

ggsave(
  'fig_output/determinism_halves_distribution.png', device = 'png', 
  height = 4, width = 4, units = 'in'
)
  
# check out difference between halves
det_df |> 
  mutate(
    det_diff = first_half_det - second_half_det
  ) |> 
  ggplot(
    aes(x = det_diff)
  ) +
  geom_density(color = NA, fill = 'firebrick') +
  geom_vline(xintercept = 0, linetype = 3) +
  scale_fill_manual() +
  labs(x = 'Determinism difference (1st half - 2nd half)') +
  theme_minimal() +
  facet_wrap(~group_id)

ggsave(
  'fig_output/determinism_contrast_distribution.png', device = 'png', 
  height = 4, width = 4, units = 'in'
)

df |> 
  distinct(behavior)


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

