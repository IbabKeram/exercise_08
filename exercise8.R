
setwd("V:/7.Semester/MPC-PRG/3_11/exercise_08")
library(Biostrings)
library(stringr)
library(seqinr)

veckillmeint <- c(0,1,2,3,4,5,6,7,8)
vec_wrong <- c(0,1,2,3,6,7,4,5,8)
vec_wrong_2 <- c(0,4,5,3,2,1,6,7,8)
vec_break <- c(5,1,4,3,7,8,9,2,6) 

FindSorted <- function(vecint) {
  for (i in 1:length(vecint)) {
    if (i-1 != vecint[i] )  {
      return(i)
    }
  }
  return('No index found')
}

FindSorted(vec_wrong)

IndicateAscending <- function(vecint) {
  vec_out <- c(1, rep(0, length(vecint)-2),1)
  for (i in 1:(length(vecint)-1)){
    if (vecint[i]+1 == vecint[i+1]) {
      vec_out[i] <- 1
      vec_out[i+1] <- 1
    }
  }
  return(vec_out)
}

IndicateAscending(vec_wrong_2)

sorted_list <- c(0,veckillmeint, max(veckillmeint)+1)


BreakpointSort <- function(ind) {
  while (TRUE) {
    idx <- FindSorted(ind)
    asc <- IndicateAscending(ind)
    if (asc[min(ind)]==0) {
      ind <- rev(ind[idx]:ind[min(ind)])
    }
    else {
      sorted_list <- c(0,ind, max(ind)+1)
      return(sorted_list)
    }
  }
}

BreakpointSort(vec_break)

