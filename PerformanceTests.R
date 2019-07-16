################################
# GuessCompx Benchmarks
################################

# Linux or Windows?
Sys.info()

# install & load
if (!("GuessCompx" %in% data.frame(installed.packages())$Package)) {
  install.packages("GuessComps")
}
library(GuessCompx)

# reproducibility
set.seed(1234)

#############
# bubble sort ---> O(N^2)
#############

source("bubble_sort_algorithm.R")

# reproducibility
set.seed(1234)
# True Complexity: Quadratic (time), linear (probably, for memory)
f = function(max.time=30, replicates=4) {
  out = CompEst(1:10000, bubble_sort, max.time = max.time, replicates=replicates, random.sampling = TRUE, plot.result = FALSE)
  return(c("time"= out$`TIME COMPLEXITY RESULTS`$best.model, "space"= out$`MEMORY COMPLEXITY RESULTS`$best.model))
}
result_bubblesort_T_3_NULL_4_NULL_2 = replicate(100, f(3, 4))

##########
# find max ---> O(N)
##########

# reproducibility
set.seed(1234)
# True Complexity: linear (time), linear (? depends, constant or linear)
f = function(max.time=30, replicates=4) {
  out = CompEst(1:5E8, max, max.time = max.time, replicates=replicates, random.sampling = TRUE, plot.result = F, power.factor = 2, start.size = 2^12)
  return(c("time"= out$`TIME COMPLEXITY RESULTS`$best.model, "space"= out$`MEMORY COMPLEXITY RESULTS`$best.model))
}
result_max_T_1_NULL_4_NULL_2 = replicate(100, f(1, 4))


###################
# find combinations  ---> O(N^3)
###################

# reproducibility
set.seed(1234)
# True Complexity: linear (time), linear (? depends, constant or linear)
f = function(max.time=30, replicates=4) {
  out = CompEst(1:1E3, function(n) length(combn(n, 3)), max.time = max.time, replicates=replicates, random.sampling = TRUE, plot.result = F, power.factor = 2)
  return(c("time"= out$`TIME COMPLEXITY RESULTS`$best.model, "space"= out$`MEMORY COMPLEXITY RESULTS`$best.model))
}
result_comb_T_1_NULL_4_NULL_2 = replicate(100, f(1, 1))


############
# shell sort  ---> O(N^4/3)
############

# reproducibility
set.seed(1234)
# True Complexity: linear (time), linear (? depends, constant or linear)
f = function(max.time=30, replicates=4) {
  out = CompEst(1:1E7, function(x) sort(x, method = "shell"), max.time = max.time, replicates=replicates, random.sampling = TRUE, plot.result = F, power.factor = 2, start.size = 2^12)
  return(c("time"= out$`TIME COMPLEXITY RESULTS`$best.model, "space"= out$`MEMORY COMPLEXITY RESULTS`$best.model))
}
result_shellsort_T_1_NULL_4_NULL_2 = replicate(100, f(1, 4)) # rep("NLOGN", 10)

############
# cut?  ---> ?
############

# reproducibility
set.seed(1234)
# True Complexity: linear (time), linear (? depends, constant or linear)
f = function(max.time=30, replicates=4) {
  out = CompEst(1:1E7, function(x) cut(x, breaks=2), max.time = max.time, replicates=replicates, random.sampling = TRUE, plot.result = F, power.factor = 3, start.size = 2^12)
  return(c("time"= out$`TIME COMPLEXITY RESULTS`$best.model, "space"= out$`MEMORY COMPLEXITY RESULTS`$best.model))
}
result_cut_T_1_NULL_4_NULL_2 = replicate(100, f(1, 4))


############
# rpart?  ---> ? very diverse!
############

# reproducibility
set.seed(1234)
library(rpart)
# True Complexity: linear (time), linear (? depends, constant or linear)
i = 1E5; d = data.frame(matrix(rnorm(10*i), ncol=10, nrow=i)) %>% setNames(c("Y", 1:9))
f = function(max.time=30, replicates=4) {
  out = CompEst(d, function(x) rpart(data=d, formula=Y~.), max.time = max.time, replicates=replicates, random.sampling = TRUE, plot.result = F, power.factor = 3)
  return(c("time"= out$`TIME COMPLEXITY RESULTS`$best.model, "space"= out$`MEMORY COMPLEXITY RESULTS`$best.model))
}
result_rpart_T_30_NULL_4_NULL_3 = replicate(100, f(30, 4))



###########################################
# detailed analysis with varying max size

##########
# find max ---> O(N)
##########

set.seed(1234)
# True Complexity: linear (time), constant (memory)
# fitting process very long...
f = function(max.time=30, replicates=4, N) {
  out = CompEst(1:N, max, max.time = max.time, replicates=replicates, random.sampling = TRUE, plot.result = F, power.factor = 3, start.size = 1E4)
  return(c("time"= out$`TIME COMPLEXITY RESULTS`$best.model, "space"= out$`MEMORY COMPLEXITY RESULTS`$best.model))
}

result_max_T_1_NULL_4_NULL_3_1E4_1E5 = replicate(20, f(30, 4, N=1e5))
result_max_T_1_NULL_4_NULL_3_1E4_1E6 = replicate(20, f(30, 4, N=1e6))
result_max_T_1_NULL_4_NULL_3_1E4_1E7 = replicate(20, f(30, 4, N=1e7))
result_max_T_1_NULL_4_NULL_3_1E4_1E8 = replicate(20, f(30, 4, N=1e8))
result_max_T_1_NULL_4_NULL_3_1E4_5E8 = replicate(10, f(30, 4, N=5e8))
result_max_T_1_NULL_4_NULL_3_1E4_1E9 = replicate(10, f(30, 4, N=1e9))

# PLOT
library(reshape2); library(ggplot2); library(dplyr)
data.frame(max.size=c(1E5,1E6,1E7,1E8,5E8,1E9), 
           constant=c(20,20,6,0,0,0),
           linear=c(0,0,0,11,18,20),
           quadratic=c(0,0,2,0,0,0),
           cubic=c(0,0,2,0,0,0),
           sqrt=c(0,0,4,1,0,0),
           log=c(0,0,6,0,0,0),
           nlogn=c(0,0,0,8,2,0)) %>% 
  melt(id.var="max.size", variable.name="best.model") %>% mutate(value = 5*value) %>%
  ggplot(.) +
  aes(x=(max.size), y=value, fill=best.model) +
  geom_area() + scale_x_log10() + scale_fill_brewer(palette="Set1") + xlab("increasing vector size") + ylab("proportion of best model returned")


###########################################
# detailed analysis with varying number replicates

##########
# distance ---> O(N^2)
##########

f = function(max.time=30, replicates=4, N) {
  out = CompEst(ggplot2::diamonds[1:N, 5:8], dist, max.time = max.time, replicates=replicates, random.sampling = TRUE, plot.result = F, power.factor = 3, start.size = 3)
  return(c("time"= out$`TIME COMPLEXITY RESULTS`$best.model, "space"= out$`MEMORY COMPLEXITY RESULTS`$best.model))
}
set.seed(1234)
result_dist_T_01_NULL_3_NULL_N1000 = replicate(20, f(1E6, 1, 1000)) # 1 replicate only, 5 points in X axis
result_dist_T_02_NULL_3_NULL_N1000 = replicate(20, f(1E6, 2, 1000)) # 2 replicate only, 5 points in X axis
result_dist_T_03_NULL_3_NULL_N1000 = replicate(20, f(1E6, 3, 1000)) # 3 replicate only, 5 points in X axis
result_dist_T_05_NULL_3_NULL_N1000 = replicate(20, f(1E6, 5, 1000)) # 5 replicate only, 5 points in X axis
result_dist_T_10_NULL_3_NULL_N1000 = replicate(10, f(1E6, 10, 1000)) # 10 replicate only, 5 points in X axis

data.frame(replicates=c(1,2,3,5,10), 
           accuracy=c(0, 55, 55, 65, 85)) %>% 
  ggplot(.) +
  aes(x=replicates, y=accuracy) +
  geom_line(size=2) + xlab("increasing number of replicates") + ylab("% accuracy of the models returned") +ylim(0, 100) + scale_x_continuous(breaks=c(1,2,3,5,10))
