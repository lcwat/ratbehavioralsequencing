## CALCULATE TRAPLINING/ROUTINE MOVEMENT METRICS AUTOMATICALLY FROM SUBJECT 
## FORAGE DATA
## this script will calculate both entropy aka routine movement index (rmi) 
## store their values assigned to each subject in a neat tibble.

# speed up entropy calculations
library(compiler)

# source required functions
source("scripts/fun/entropy.R")
source("scripts/fun/AutoO.R")

trapline_metrics <- function(sequence_df, data_type = "subject") {
  # create table for output
  output <- tibble(
    subject = numeric(), 
    level = character(),
    rmi = numeric(),
    ood = numeric()
  )
  
  if(data_type == "simulation") {
    # loop through df and find entropy value for each level, prob best to do this
    # on the desktop, takes quite a bit of memory to do, will be slow on laptop
    for(i in seq_along(unique(sequence_df$subject))) {
      # set subj number
      subj <- sequence_df$subject[[i]]
      
      # set nearest neighbors to look through
      nn <- unique(sequence_df$nn_rule)
      # "subjects" to look through
      fo_nos <- unique(sequence_df$forage_number)
      # levels to look through
      levels <- unique(sequence_df$level)
      
      # loop through df and find entropy value for each level, prob best to do this
      # on the desktop, takes quite a bit of memory to do, will be slow on laptop
      for(i in 1:length(fo_nos)) {
        # set subj/forage number
        subj <- fo_nos[[i]]
        
        for(j in 1:length(nn)) {
          # set nn_rule currently looking at
          nn_using <- nn[[j]]
          
          # loop though the 10 levels
          for(k in 1:length(levels)) {
            # create level string to filter for the sequence
            level_string <- levels[[k]]
            
            # grab vector of coconuts visited
            s <- sequence_df |> 
              dplyr::filter(
                forage_number == subj & level == level_string & nn_rule == nn_using
              ) |> 
              pull(obj_ID)
            
            # calculate determinism (d), entropy (e)/routine movement index (r), order 
            # of dependency (o) (additional info.)
            e <- entropy(s)
            
            r <- 1 - min(e)
            
            o <- AutoO(e)
            
            output <- output |> 
              add_row(
                forage_number = subj, nn_rule = nn_using, level = level_string, 
                rmi = r, ood = o
              )
          }
        }
        
        
      }
    }
  } else if(data_type == "subject") {
    ## SUBJECT
    # create vector of subj ids to loop through
    subjects <- unique(sequence_df$subject)
    
    # loop through df and find entropy value for each level, prob best to do this
    # on the desktop, takes quite a bit of memory to do, will be slow on laptop
    for(i in 1:length(subjects)) {
      # set subj number
      subj <- subjects[[i]]
      
      # loop though the 10 levels
      for(j in 1:10) {
        # create level string to filter for the sequence
        level_string <- paste("_level_", j, sep = "")
        
        s <- sequence_df |> 
          dplyr::filter(subject == subj, level == level_string) |> 
          pull(obj_ID)
        
        # check to see if level was played, if not, add NAs
        if(length(s) == 0) {
          d = NA
          r = NA
          o = NA
        } else {
          # calculate determinism (d), entropy (e)/routine movement index (r), order 
          # of dependency (o) (additional info.)
          e <- entropy(s)
          
          r <- 1 - min(e)
          
          o <- AutoO(e)
        }
        
        # add data to output
        output <- output |> 
          add_row(
            subject = subj, level = level_string, rmi = r, ood = o
          )
      }
    }
  } else {
    stop("Please specify a valid data type: either 'simulation' or 'subject'.")
  }
  
  return(output)
}