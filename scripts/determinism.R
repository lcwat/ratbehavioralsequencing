## CALCULATE DETERMINISM FROM SEQUENCE OF VISITS
## code and theoretical support from Ayers, Armsworth, & Brosi (2015)


#===========================================================# 
#-This R code contains a function to calculate Determinism--# 
#-and create recurrence plots using the fNonlinear package--# 
#===========================================================#  

# Load required packages 
library(fNonlinear) 
library(spatstat)  

#===========================================================# 
#------Begin function to calculate Determinism--------------# 
#===========================================================#  

# x is a vector of numbered resource visits/behaviors 
# minl is the minimum length of a diagonal to be considered in the numerator of 
# the determinism calculation  
determinism <- function(x, minl) {  
  # Depending on the dataset it may be desirable to filter out consecutive visits  
  # to the same flower. See function below and delete ‘#’ in the line below to use 
  # x = filterout(Ldata = x)  
  
  #-----set up matrix resembling a recurrence plot, where a 1 indicates a repeat 
  #-----visit and 0 indicates the absence of a repeat.  
  det1 = matrix(cbind(rep(x, length(x))), nrow=length(x)) 
  tdet = t(det1) 
  det = ((det1 - tdet) == 0) * 1  
  
  # set the main diagonal equal to zero so it won't be included in the calculation  
  diag(det) = 0
  
  # Use spatstat package to create a 'countour map' of the matrix, which assigns 
  # all sets of contiguous 1's a unique number 
  yi <- as.im(det) 
  ycOut <- connected(yi, background = 0) 
  yc <- ycOut$v  
  
  # Depending on the dataset it may be desirable to filter out diagonals perpendicular 
  # to the main diagonal. Code is provided for the ‘removeperpdiag’ function below. 
  # Delete “#” from the line below to filter out perpendicular diagonals  
  # yc = removeperpdiag(yc,minl)  
  # Note: this code may take several minutes to run for very long sequences  
  # ---- filter out short repeats: a ‘trapline’ should include more unique resources 
  # ---- than the minimum cutoff (minl)
  # make an alternative DET matrix that contains the resource IDs 
  det2 = matrix(rep(x, nrow(det)), nrow = nrow(det), byrow = TRUE) * det 
  
  # make a dataframe with the number of times each resource appears in a diagonal 
  listofseq = data.frame(group = yc[1:length(yc)], seq = det2[1:length(det2)]) 
  
  # how many unique resources are in the diagonal 
  uniquevisits = rowSums((table(listofseq) > 0) * 1)  
  
  # only count diagonals with at least ‘minl’ number of unique resources 
  longenough = (uniquevisits >= minl) * table(yc) 
  
  # find the numerator: (remember this still includes both the top and bottom 
  # halves of the matrix)  
  contig = sum(longenough)  
  denominator= sum(det) # This also still includes top and bottom halves of the matrix  
  
  #------------------- total DET score 
  # divide the numerator and denominator in half before calculating DET for just 
  # the top half of the matrix  
  return((contig/2)/(denominator/2)) 
}


#===========================================================# 
##---optional function to filter out visits to same flower in a row 
#===========================================================#  

# filterout <- function(Ldata) { 
#   for (i in 2:length(Ldata)) { 
#     if(Ldata[i] == Ldata[i-1] ) { 
#       Ldata[i - 1]= NA
#     } 
#   }  
#   Ldata = Ldata[!is.na(Ldata)] 
#   
#   Ldata 
# }  

#===========================================================# 
#---optional function to filter out perpendicular diagonals 
#===========================================================#  

# removeperpdiag = function(yc,minl) { 
#   #first, remove observations that are too short to save time 
#   remove = names(table(yc)[table(yc)< minl]) 
#   
#   for (i in remove) { 
#     yc[ yc == i ] = NA 
#   }  
#   
#   #Only do these steps if there are perpendicular diagonals longer than minl 
#   if(sum(!is.na(yc))!= 0) {  
#     #-------remove sequences perpendicular to the main diagonal 
#     # save list of levels (aka groups of continuous points) that weren't removed 
#     # in the previous step 
#     newlevels = levels(droplevels(yc))
#     
#     #use a loop to go through each level and remove all that are not parallel 
#     for (i in 1:length(newlevels)) {  
#       # only look at matrix positions of current contiguous group 
#       set = which(yc == newlevels[i])  
#       
#       #make a list of all possible parallel points 
#       pardiag = c(seq(set[1], length(yc), (nrow(yc) +1))[-1],seq(set[1], 0, (nrow(yc) +1))[-1])  
#       
#       for(i in 1:length(set)) { 
#         pardiag = c(pardiag,c(seq(set[i], length(yc), (nrow(yc) +1))[-1],seq(set[i], 0, -(nrow(yc) +1))[-1])) 
#       }  
#       
#       #remove points that don't fall in these positions 
#       keepers = set[set %in% pardiag] 
#       toberemoved = setdiff(set,keepers)  
#       if(length(toberemoved) > 0) { 
#         yc[toberemoved] = NA 
#       } 
#     }
#   } 
#   
#   yc 
# }  

#===========================================================# 
##------Example DET calculation------------------------------# 
###===========================================================#  
# x = c(1,2,3,8,9,10,2,3,4,5,7,3,8,9,10,7,5,4,8) 
# 
# determinism(x, 3)  

#===========================================================# 
#------Make recurrence plot---------------------------------# 
#===========================================================#  
# recurrencePlot(x, m = 1, d = 0, eps = 1, nt = 1, end.time = 800, pch = 16, cex = 1)
# 
# axis(side = 1, at = 0:19)


