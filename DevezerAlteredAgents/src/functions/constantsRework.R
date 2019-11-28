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
O_NUM_FIELDS <- 25

# ID VARIABLES
O_POPULATION <- 1
O_SIGMA <- 2
O_SAMPLE_SIZE <- 3
O_TRUE_MODEL <- 4
O_STRATEGY <- 5
O_MODELCOMPARE <- 6

# OTHER VARIABLES
O_SELECTED_MODEL <- 7
O_SELECTED_TRUE_MODEL <- 8
O_SELECTED_MODEL_DISTANCE <- 9
O_INITIAL_GLOBAL_MODEL <- 10
O_INITIAL_GLOBAL_TRUE_MODEL <- 11
O_INITIAL_GLOBAL_MODEL_DISTANCE <- 12
O_FINAL_GLOBAL_MODEL <- 13
O_FINAL_GLOBAL_TRUE_MODEL <- 14
O_FINAL_GLOBAL_MODEL_DISTANCE <- 15
O_NUM_PREDICTORS <- 16
O_BETA1_TRUE <- 17
O_BETA1_ESTIMATE <- 18
O_BETA1_ERROR <- 19
O_TSTATISTICS <- 20
O_RSQ <- 21
O_ARSQ <- 22
O_AIC <- 23
O_BIC <- 24
O_REPLICATED <- 25


## File output headers
OUTPUT_HEADER <- c("replica",
                   "timestep",
                   "population",
                   "sigma",
                   "sample_size",
                   "tmodel",
                   "strategy",
                   "modelcompare",
                   "selected_model",
                   "selected_true_model",
                   "selected_model_distance",
                   "initial_global_model",
                   "initial_global_true_model",
                   "initial_global_model_distance",
                   "final_global_model",
                   "final_global_true_model",
                   "final_global_model_distance",
                   "num_predictors",
                   "beta1_true",
                   "beta1_estimate",
                   "beta1_error",
                   "tStatistics",
                   "RSQ",
                   "ARSQ",
                   "AIC",
                   "BIC",
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
