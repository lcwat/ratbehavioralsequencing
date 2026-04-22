# this script takes the coded rat behavioral data and codes the behaviors into
# unique identifying symbols that can be plugged into the determinism algorithm

# analyzing new test phase datasets that include more behaviors

# 4/22/2026

# Luke Watson

# load libraries ----------------------------------------------------------

library(tidyverse)

source('scripts/determinism.R')

# load and clean data -----------------------------------------------------

# load and split data, get sequence, and calculate determinism
file_list <- list.files('data/cleaned/test_novel_fam/', full.names = T)

# 10 minutes of video, 36000 frames, split before and after frame 18000
det_df <- tibble(
  animal_id = character(), 
  group_id = character(), 
  phase = character(), 
  overall_det = numeric(), 
  first_half_det = numeric(), 
  second_half_det = numeric()
)

seq_list <- list()

# read in file, summarize and clean, calc det, then get out
for(file in file_list) {
  df <- read_csv(file, show_col_types = F)
  
  overall <- df |> 
    group_by(behavior_id) |> 
    summarize(
      behavior = unique(behavior)
    ) |> 
    mutate(
      int_behavior = case_when(
        behavior == 'Familiar' ~ 1, 
        behavior == 'Novel' ~ 2,
        behavior == 'standing' ~ 3,
        behavior == 'resting' ~ 4, 
        behavior == 'frozen' ~ 4, # resting and frozen will be same cat of behavior
        behavior == 'none' ~ NA
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
        behavior == 'Familiar' ~ 1, 
        behavior == 'Novel' ~ 2,
        behavior == 'standing' ~ 3,
        behavior == 'resting' ~ 4, 
        behavior == 'frozen' ~ 4, # resting and frozen will be same cat of behavior
        behavior == 'none' ~ NA
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
        behavior == 'Familiar' ~ 1, 
        behavior == 'Novel' ~ 2,
        behavior == 'standing' ~ 3,
        behavior == 'resting' ~ 4, 
        behavior == 'frozen' ~ 4, # resting and frozen will be same cat of behavior
        behavior == 'none' ~ NA
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
  
  # sequences
  subj <- df |> 
    pull(animal_id) |> 
    unique()
  group <- df |> 
    pull(group_id) |> 
    unique()
  phase <- df |> 
    pull(phase) |> 
    unique()
  
  # add to list
  file_no = which(file_list == file)
  
  if (file_no == 1) {
    seq_list[[1]] = c(subj)
    seq_list[[2]] = c(group)
    seq_list[[3]] = c(phase)
    seq_list[[4]] = list(overall)
  }
  else {
    seq_list[[1]][file_no] = c(subj)
    seq_list[[2]][file_no] = c(group)
    seq_list[[3]][file_no] = c(phase)
    seq_list[[4]][[file_no]] = list(overall)
  }
  
  print(file_no)
}


# write obj to file
write_csv(det_df, 'data/determinism_data/det_test_halves_novel_fam.csv')
write_rds(seq_list, 'data/determinism_data/sequences_nov_fam_behav.rds')


# load and compare --------------------------------------------------------

# complete sequences with upper/lower
det_upp_low <- read_csv('data/determinism_data/det_all_halves_and_upp_low_behav.csv')

# test with novel/familiar
det_nov_fam <- read_csv('data/determinism_data/det_test_halves_novel_fam.csv')

upp_low_test <- det_upp_low |> 
  filter(phase == 'test')

# bind
comp <- bind_rows(
  list('upper_lower' = upp_low_test, 'novel_familiar' = det_nov_fam), 
  .id = 'df'
)

# try a wide binding
left <- upp_low_test |> 
  select(overall_det) |> 
  rename(ul_det = overall_det)

right <- det_nov_fam |> 
  select(overall_det) |> 
  rename(
    nf_det = overall_det
  )

comp_wide <- bind_cols(left, right)

# same 
comp_wide |> 
  mutate(
    diff = ul_det - nf_det
  ) |> 
  summarize(
    total = sum(diff)
  )

comp |> 
  ggplot(aes(x = overall_det)) +
  geom_density(fill = 'goldenrod') + 
  theme_bw() + 
  facet_wrap(~df)

# looks exactly the same, which might be what you would expect if novel/familiar 
# weren't counterbalanced within session but across sessions 