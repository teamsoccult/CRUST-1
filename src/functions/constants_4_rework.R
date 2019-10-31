#Set-up for new version w. citation networks & meta-studies.
#Last changed 31-10-2019

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
O_NUM_FIELDS <- 38 

#GLOBAL ID VARIABLES#
O_STUDIES <- 1
O_NETWORK <- 2
O_POPULATION <- 3
O_SIGMA <- 4
O_NET_SIZE <- 5 #shows whether it is PT condition or TOM condition. 
O_BASE_SAMPLE_SIZE <- 6 
O_TRUE_MODEL <- 7

#LOCAL ID VARIABLES#
O_TYPE <- 8
O_STRATEGY <- 9 #colab, neighbor, research. 
O_SELECTED_MODEL <- 10
 

#META CATEGORY
O_SAMPLE_SIZE <- 11
O_NUM_AGENTS <- 12 #how many in the study. 

#STICKINESS
O_INITIAL_GLOBAL_TRUE_MODEL <- 13
O_FINAL_GLOBAL_TRUE_MODEL <- 14 

#REPLICATION
O_PROPOSED_TRUE_MODEL <- 15
O_SWITCH_TESTING <- 16
O_SWITCH_CHANGING <- 17
O_SWITCH_ALREADY_TRUE <-18
O_SWITCH_ALREADY_TRUE_CHANGING <- 19
O_SWITCH_ALREADY_TRUE_REJECTING <- 20
O_SWITCH_REPLICATION_STUDY <- 21
O_SWITCH_REPLICATED <- 22
O_SWITCH_NOT_REPLICATED <- 23

#PROPORTIONS 
O_PROPORTION_1 <- 24
O_PROPORTION_2 <- 25
O_PROPORTION_3 <- 26
O_PROPORTION_4 <- 27
O_PROPORTION_5 <- 28
O_PROPORTION_6 <- 29
O_PROPORTION_7 <- 30
O_PROPORTION_8 <- 31
O_PROPORTION_9 <- 32
O_PROPORTION_10 <- 33
O_PROPORTION_11 <- 34
O_PROPORTION_12 <- 35
O_PROPORTION_13 <- 36
O_PROPORTION_14 <- 37
O_PROPORTION_TRUE <- 38


## File output headers
OUTPUT_HEADER <- c("replica",
                   "turn",
                   "studies",
                    "network", #lattice, small, PT or TOM. 
                   "population", #Bo, Tess, Mave or All. 
                   "sigma", #0.2, 0.5 or 0.8.
                   "net_size", #from net_size
                   "base_SS", #base sample size 
                   "true_model", 
                   "orig_type", #type of agent on turn 
                   "strategy", #colab, neighbor, research. 
                   "selected_model",#sampled model 
                   "SS", #sample size (which can change on turns). 
                   "num_agents", #agents conducting orig. study 
                   "init_g_true", #count for agents on turn (cond. orig. stud.)
                   "final_g_true", #count for agents on turn (cond. orig. stud.)
                   "prop_t_mod", #proposed model true?
                   "switch_testing", #count: how many tested. 
                   "switch_changing", #count: how many changed. 
                   "switch_true", #count: how many true?
                   "switch_true_changing", #count: true changing 
                   "switch_true_rejecting", #count: true rejecting
                   "switch_rep_study", #count: rep studies cond. 
                   "switch_rep", #count: succesful rep. 
                   "switch_not_rep", #count: unsuccesful rep. 
                   "proportion_1", 
                   "proportion_2", 
                   "proportion_3", 
                   "proportion_4", 
                   "proportion_5", 
                   "proportion_6", 
                   "proportion_7", 
                   "proportion_8", 
                   "proportion_9", 
                   "proportion_10", 
                   "proportion_11", 
                   "proportion_12", 
                   "proportion_13", 
                   "proportion_14", 
                   "proportion_true")

## Parameter output
P_TMODEL <- 1
P_BASE_SAMPLE_SIZE <- 2
P_SIGMA <- 3
P_NET_TYPE <- 4 #small, lattice, PT or TOM
P_NET_SIZE <- 5 #size..
P_POP <- 6 
P_NETWORK <- 7

