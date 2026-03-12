# basic exploration of determinism 


# load libraries ----------------------------------------------------------

library(tidyverse)
library(reshape2)

# source functions --------------------------------------------------------

source('scripts/determinism.R')

# create fake sequence ----------------------------------------------------

seq <- rep(1:3, 5)

seqs <- tibble(
  visit_no = 1:length(seq), 
  sterotyped = seq
) |> 
  mutate(
    random = sample(1:3, size = length(visit_no), replace = T), 
    medium = sample(1:3, prob = c(.9, .9, .9), size = length(visit_no), replace = T)
  )

# creates a matrix by lining up sequence along each dimension then add 1 for 
# matching case and 0 for not

m <- matrix(nrow = nrow(seqs), ncol = nrow(seqs))

seqs$medium

for(i in 1:length(seqs$visit_no)) {
  for(j in 1:length(seqs$visit_no)) {
    if (seqs[i,]$medium == seqs[j,]$medium) {
      m[i, j] <- 1
    }
    else {
      m[i, j] <- 0
    }
  }
}

# put matrix in long format
to_plot <- melt(m)

# plot
to_plot |> 
  ggplot(aes(x = Var2, y = Var1)) + 
  geom_point(aes(color=as.factor(value)), size = 3, shape = 15) + 
  scale_color_manual(guide = 'none', values = c(NA, 'dodgerblue')) +
  labs(x="seq", y="seq") +
  scale_x_continuous(breaks = 1:length(seq), labels = seqs$medium) +
  scale_y_continuous(breaks = 1:length(seq), labels = seqs$medium) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())


# get determinism value
determinism(seqs |> pull(medium), 3)

recurrencePlot(seqs |> pull(sterotyped), m = 1, d = 0, eps = 1, nt = 1, end.time = 800, pch = 16, cex = 1)

axis(side = 1, at = 0:19)
