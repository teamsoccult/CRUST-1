"Trying to make it qualitative for shiny."

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
O_NUM_FIELDS <- 113 #SUPER NEW

O_STUDIES <- 1
O_NETWORK <- 2
O_POPULATION <- 3
O_SIGMA <- 4
O_CRITERION <- 5 #BIC, AIC
O_NET_SIZE <- 6
O_SAMPLE_SIZE <- 7
O_TRUE_MODEL <- 8
O_AGENT_INDEX <- 9
O_SELECTED_MODEL <- 10
O_SELECTED_TRUE_MODEL <- 11
O_EDGES_TESTING <- 12
O_EDGES_CHANGING <- 13

#agent columns. 
str <- paste0("O_AGENT", 1:100)

for(string in str){
  assign(string, as.numeric(str_extract_all(string, "\\d+")) + 13)
}

## File output headers
OUTPUT_HEADER <- c("replica",
                   "turn",
                    "study",
                   "net_type",
                   "pop_type",
                   "sigma",
                   "criterion", #needs implementation
                   "net_size",
                   "sample_size",
                   "true_model",
                   "agent_index",
                   "selected_model",
                   "selected_true",
                   "edges_testing",
                   "edges_changing",
                   paste0("agent", 1:100) #all the agent columns. 
                   )


## Parameter output
P_TMODEL <- 1
P_SAMPLE_SIZE <- 2
P_SIGMA <- 3
P_NETNAME <- 4
P_POP <- 5
P_K <- 6
P_CORRELATION <- 7
P_TRUE_BETAS <- 8
P_NETWORK <- 9
P_XSET <- 10
P_AVG_PATH_LENGTH <- 11
P_CLUSTER_COEF <- 12

