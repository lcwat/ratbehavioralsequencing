library(ggplot2)
library(reshape2)

plot_recurrence <- function(seq=vector('numeric')) {
  m <- matrix(nrow = length(seq), ncol = length(seq))
  
  # fill matrix
  for(i in 1:length(seq)) {
    for(j in 1:length(seq)) {
      if (seq[i] == seq[j]) {
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
    geom_point(aes(color=as.factor(value)), size = 1.2, shape = 15) + 
    scale_color_manual(guide = 'none', values = c(NA, 'firebrick4')) +
    labs(x="seq", y="seq") +
    scale_x_continuous(breaks = 1:length(seq), labels = seq) +
    scale_y_continuous(breaks = 1:length(seq), labels = seq) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
}