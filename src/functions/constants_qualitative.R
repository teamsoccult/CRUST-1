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

O_AGENT1 <- 14
O_AGENT1 <- 15
O_AGENT1 <- 16
O_AGENT1 <- 17
O_AGENT1 <- 18
O_AGENT1 <- 19
O_AGENT1 <- 20
O_AGENT1 <- 21
O_AGENT1 <- 22
O_AGENT1 <- 23
O_AGENT1 <- 24
O_AGENT1 <- 25
O_AGENT1 <- 26
O_AGENT1 <- 27
O_AGENT1 <- 28
O_AGENT1 <- 29
O_AGENT1 <- 30
O_AGENT1 <- 31
O_AGENT1 <- 32
O_AGENT1 <- 33
O_AGENT1 <- 34
O_AGENT1 <- 35
O_AGENT1 <- 36
O_AGENT1 <- 37
O_AGENT1 <- 38
O_AGENT1 <- 39
O_AGENT1 <- 40
O_AGENT1 <- 41
O_AGENT1 <- 42
O_AGENT1 <- 43
O_AGENT1 <- 44
O_AGENT1 <- 45
O_AGENT1 <- 46
O_AGENT1 <- 47
O_AGENT1 <- 48
O_AGENT1 <- 49
O_AGENT1 <- 50
O_AGENT1 <- 51
O_AGENT1 <- 52
O_AGENT1 <- 53
O_AGENT1 <- 54
O_AGENT1 <- 55
O_AGENT1 <- 56
O_AGENT1 <- 57
O_AGENT1 <- 58
O_AGENT1 <- 59
O_AGENT1 <- 60
O_AGENT1 <- 61
O_AGENT1 <- 62
O_AGENT1 <- 63
O_AGENT1 <- 64
O_AGENT1 <- 65
O_AGENT1 <- 66
O_AGENT1 <- 67
O_AGENT1 <- 68
O_AGENT1 <- 69
O_AGENT1 <- 70
O_AGENT1 <- 71
O_AGENT1 <- 72
O_AGENT1 <- 73
O_AGENT1 <- 74
O_AGENT1 <- 75
O_AGENT1 <- 76
O_AGENT1 <- 77
O_AGENT1 <- 78
O_AGENT1 <- 79
O_AGENT1 <- 80
O_AGENT1 <- 81
O_AGENT1 <- 82
O_AGENT1 <- 83
O_AGENT1 <- 84
O_AGENT1 <- 85
O_AGENT1 <- 86
O_AGENT1 <- 87
O_AGENT1 <- 88
O_AGENT1 <- 89
O_AGENT1 <- 90
O_AGENT1 <- 91
O_AGENT1 <- 92
O_AGENT1 <- 93
O_AGENT1 <- 94
O_AGENT1 <- 95
O_AGENT1 <- 96
O_AGENT1 <- 97
O_AGENT1 <- 98
O_AGENT1 <- 99
O_AGENT1 <- 100
O_AGENT1 <- 101
O_AGENT1 <- 102
O_AGENT1 <- 103
O_AGENT1 <- 104
O_AGENT1 <- 105
O_AGENT1 <- 106
O_AGENT1 <- 107
O_AGENT1 <- 108
O_AGENT1 <- 109
O_AGENT1 <- 110
O_AGENT1 <- 111
O_AGENT1 <- 112
O_AGENT1 <- 113


## File output headers
OUTPUT_HEADER <- c("study",
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
