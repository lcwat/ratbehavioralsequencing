# this script takes the coded rat behavioral data and codes the behaviors into
# unique identifying symbols that can be plugged into the determinism algorithm

# 3/29/2026

# Luke Watson


# load libraries ----------------------------------------------------------

library(tidyverse)

source('scripts/determinism.R')
# source('scripts/plot_recurrence.R')
#
# load data ---------------------------------------------------------------

# list files
file_list <- list.files('data/cleaned/simple_sample_test/', full.names = T)

d <- read_csv(file_list[1])

summary(d)

s <- d |> 
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
  filter(!repeated) |> 
  pull(int_behavior)

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
write_csv(det_df, 'data/determinism_data/det_all_halves_and_upp_low_behav.csv')
write_rds(seq_list, 'data/determinism_data/all_sequences_upp_low_behav.rds')

det_df <- read_csv('data/determinism_data/det_sample_halves_and_4_behav.csv')
seq_list <- read_rds('data/determinism_data/sequences_4_behav.rds')

det_df |> 
  ggplot(aes(x = overall_det, fill = group_id)) +
  geom_density(alpha = .5, color = NA) + 
  scale_fill_manual(values = c('firebrick', 'goldenrod')) +
  xlim(0:1) +
  theme_minimal()

ggsave(
  'fig_output/overall_determinism_by_group.png', device = 'png', 
  width = 6, height = 4, units = 'in'
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
  height = 4, width = 6, units = 'in'
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
  height = 4, width = 6, units = 'in'
)

df |> 
  distinct(behavior)

# sequence analysis -------------------------------------------------------

# see what sequences are being repeated in each run
s <- seq_list[[4]][[1]]

m1 <- matrix(cbind(rep(s, length(s))), nrow = length(s))
tm1 <- t(m1)
det <- ((m1-tm1)==0)*1

det2 = matrix(rep(s, nrow(det)), nrow = nrow(det), byrow = TRUE) * det 

# set big diag to zero
diag(det) <- 0

# set lower triangle to 0
det[lower.tri(det)] = 0

seq_df <- tibble(
  start_id1 = numeric(), 
  start_id2 = numeric(),
  seq = numeric()
)

for (i in 1:length(s)) {
  for (j in 1:length(s)) {
    
    k = i
    l = j
    
    # start true
    contiguous = TRUE
    
    little_s = c()
    
    # find sequence
    while (contiguous) {
      
      # break if out of bounds
      if (k > length(s) | l > length(s)) {
        contiguous = FALSE
      }
      else {
        if (det[k, l] != 0) {
          contiguous = TRUE
          
          little_s = c(little_s, det2[k, l])
        }
        else {
          contiguous = FALSE
          
          if (k == i + 1) {
            # ignore
          }
          else {
            # write to df
            seq_df = seq_df |> 
              add_row(
                start_id1 = i,
                start_id2 = j, 
                seq = as.numeric(str_flatten(little_s)) # turn into single number
              )
          }
        }
      }
      
      # iterate
      k = k + 1
      l = l + 1
    }
  }
}

# remove non-sequences and sequences less than 3 long
new_seq_df <- seq_df |> 
  drop_na() |> 
  filter(seq > 100) # 100 is lowest value 3L numeric seq can take 

clean_seq_df <- tibble(
  start_id1 = numeric(), 
  start_id2 = numeric(), 
  seq = numeric()
)

# remove sequences that are just previous sequences
for (i in 1:nrow(new_seq_df)) {
  
  # check if there are previous steps to sequence, if not, keep, if yes, remove
  prev_i = new_seq_df$start_id1[i] - 1
  prev_j = new_seq_df$start_id2[i] - 1
  
  # check
  result = new_seq_df |> 
    filter(
      start_id1 == prev_i & start_id2 == prev_j
    )
  
  if (nrow(result) < 1) {
    # save
    clean_seq_df <- clean_seq_df |> 
      bind_rows(
        slice(new_seq_df, i)
      )
  }
}

# count unique sequences
seq_counts <- clean_seq_df |> 
  count(seq)

nrow(clean_seq_df) / sum(det)

# still not sure how to get the right sequences, not sure if this is the right 
# way to get them or if I'm missing some other part of the determinism calculation
# b/c this seems like too few sequences 
