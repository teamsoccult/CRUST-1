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
REY <- 1
TESS <- 2
BO <- 3
MAVE <- 4

## Model comparison
TSTATISTICS <- 1
RSQ <- 2
ARSQ <- 3
AIC <- 4
BIC <- 5

## Model output fields
O_NUM_FIELDS <- 14

# ID VARIABLES
O_POPULATION <- 1
O_SIGMA <- 2
O_NETWORK <- 3
O_SAMPLE_SIZE <- 4
O_TRUE_MODEL <- 5
O_MODELCOMPARE <- 6
O_ORIG_TYPE <- 7


# OTHER VARIABLES
O_SELECTED_MODEL <- 8
O_PROPOSED_TRUE_MODEL <- 9
O_OLD_MODEL <- 10 
O_INITIAL_GLOBAL_TRUE_MODEL <- 11
O_FINAL_GLOBAL_MODEL <- 12
O_FINAL_GLOBAL_TRUE_MODEL <- 13
O_REPLICATED <- 14


## File output headers
OUTPUT_HEADER <- c("replica",
                   "turn",
                   "population",
                   "sigma",
                   "network",
                   "base_SS",
                   "true_model",
                   "modelcompare",
                   "orig_type",
                   "selected_model",
                   "prop_t_mod",
                   "old_model",
                   "init_g_true",
                   "final_g",
                   "final_g_true",
                   "replicated")

## Parameter output
P_TMODEL <- 1
P_K <- 2
P_SAMPLE_SIZE <- 3
P_SIGMA <- 4
P_CORRELATION <- 5
P_AGENT_WEIGHTS <- 6
P_TRUE_BETAS <- 7
P_XSET <- 8
