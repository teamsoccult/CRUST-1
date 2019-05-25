################
##
## @description Define constants
##
## @param None
##
## @return None
##
## @lastChange 2018-03-22
##
## @changes
##   Change CARA to BO, NELL to MAVE, ROB to TESS [2018-03-22]
##   Included BIC output parameter [2018-03-22]
##   Included parameters output constants [2017-06-17]
##   Included the O_BETA1_TRUE output field constant [2017-06-08]
##   Change BOB to CARA, NEL to NELL, RAY to REY [2017-10-16]
##
################

## Types of agents
#REY <- 1
#TESS <- 2
#BO <- 3
#MAVE <- 4

## Model comparison
TSTATISTICS <- 1
RSQ <- 2
ARSQ <- 3
AIC <- 4
BIC <- 5

## Model output fields
O_NUM_FIELDS <- 52 #SUPER NEW
O_STUDIES <- 1
O_NETWORK <- 2
O_POPULATION <- 3
O_SIGMA <- 4
O_NET_SIZE <- 5
O_SAMPLE_SIZE <- 6
O_TRUE_MODEL <- 7
O_STRATEGY <- 8
O_AGENT_INDEX <- 9
O_SELECTED_MODEL <- 10
O_SELECTED_TRUE_MODEL <- 11
O_SELECTED_MODEL_DISTANCE <- 12
O_INITIAL_GLOBAL_MODEL <- 13
O_INITIAL_GLOBAL_TRUE_MODEL <- 14
O_INITIAL_GLOBAL_MODEL_DISTANCE <- 15 
O_FINAL_GLOBAL_MODEL <- 16
O_FINAL_GLOBAL_TRUE_MODEL <- 17
O_FINAL_GLOBAL_MODEL_DISTANCE <- 18
O_BETA1_TRUE <- 19
O_BETA1_ESTIMATE <- 20
O_BETA1_ERROR <- 21
O_TSTATISTICS <- 22
O_RSQ <- 23
O_ARSQ <- 24
O_AIC <- 25
O_BIC <- 26
O_EDGES_TESTING <- 27
O_EDGES_CHANGING <- 28
O_EDGES_ALREADY_TRUE <-29
O_EDGES_ALREADY_TRUE_CHANGING <- 30
O_EDGES_ALREADY_TRUE_REJECTING <- 31
O_EDGES_REPLICATION_STUDY <- 32
O_EDGES_REPLICATED <- 33
O_EDGES_NOT_REPLICATED <- 34
O_PROPORTION_1 <- 35
O_PROPORTION_2 <- 36
O_PROPORTION_3 <- 37
O_PROPORTION_4 <- 38
O_PROPORTION_5 <- 39
O_PROPORTION_6 <- 40
O_PROPORTION_7 <- 41
O_PROPORTION_8 <- 42
O_PROPORTION_9 <- 43
O_PROPORTION_10 <- 44
O_PROPORTION_11 <- 45
O_PROPORTION_12 <- 46
O_PROPORTION_13 <- 47
O_PROPORTION_14 <- 48
O_PROPORTION_TRUE <- 49
O_EDGES_TOTAL <- 50
O_EDGES_SAME_GLOBAL <- 51
O_EDGES_NOT_SAME_GLOBAL <- 52 

## File output headers
OUTPUT_HEADER <- c("replica",
                   "turn",
                   "studies",
                    "network", #lattice or small
                   "population", #Bo, Tess, Mave or All
                   "sigma", #0.2, 0.5 or 0.8
                   "net_size", #from net_size
                   "sample_size", #from sampleSize
                   "true_model", #either 2, 7 or 13 (from "tm")
                   "strategy", #type of agent on turn.
                   "agent_index", #index of agent on turn. 
                   "selected_model",#sampled model 
                   "selected_true_model",#0 (FALSE) or 1 (TRUE)
                   "selected_model_distance", 
                   "initial_global_model",
                   "initial_global_true_model",
                   "initial_global_model_distance",
                   "final_global_model",
                   "final_global_true_model",
                   "final_global_model_distance",
                   "beta1_true",
                   "beta1_estimate",
                   "beta1_error",
                   "tStatistics",
                   "RSQ",
                   "ARSQ",
                   "AIC",
                   "BIC",
                   "edges_testing", #new
                   "edges_changing", #new
                   "edges_already_true", #new
                   "edges_already_true_changing",
                   "edges_already_true_rejecting", #new
                   "edges_replication_study", #new!!!
                   "edges_replicated", #new
                   "edges_not_replicated", #new, 
                   "proportion_1", #new
                   "proportion_2", #new
                   "proportion_3", #new
                   "proportion_4", #new
                   "proportion_5", #new
                   "proportion_6", #new
                   "proportion_7", #new
                   "proportion_8", #new
                   "proportion_9", #new
                   "proportion_10", #new
                   "proportion_11", #new
                   "proportion_12", #new
                   "proportion_13", #new
                   "proportion_14", #new
                   "proportion_true", #new
                   "edges_total", #new!!
                   "edges_same_global", #new!!
                   "edges_not_same_global") #new!!

## Parameter output
P_TMODEL <- 1
P_K <- 2
P_SAMPLE_SIZE <- 3
P_SIGMA <- 4
P_CORRELATION <- 5
P_AGENT_WEIGHTS <- 6
P_TRUE_BETAS <- 7
P_XSET <- 8
P_NETWORK <- 9 #new
P_AVG_PATH_LENGTH <- 10
P_CLUSTER_COEF <- 11
