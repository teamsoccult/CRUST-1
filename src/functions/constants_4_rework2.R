#Set-up for new version w. citation networks & meta-studies.
#Last changed 1-11-2019
#Adding O_ORIG_AGENTS & O_SWITCH_AGENTS to test. 

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
O_NUM_FIELDS <- 42

#GLOBAL ID VARIABLES#
O_STUDIES <- 1
O_NETWORK <- 2
O_POPULATION <- 3
O_SIGMA <- 4
O_NET_SIZE <- 5 #shows whether it is PT condition or TOM condition. 
O_BASE_SAMPLE_SIZE <- 6 
O_TRUE_MODEL <- 7
O_COLAB_COND <- 8

#LOCAL ID VARIABLES#
O_TYPE <- 9
O_STRATEGY <- 10 #colab, neighbor, research. 
O_SELECTED_MODEL <- 11

#META CATEGORY
O_SWITCH_TOTAL <- 12
O_ORIG_AGENTS <- 13
O_SWITCH_AGENTS <- 14
O_SAMPLE_SIZE <- 15
O_NUM_AGENTS <- 16 #how many in the study. 

#STICKINESS
O_INITIAL_GLOBAL_TRUE_MODEL <- 17
O_FINAL_GLOBAL_TRUE_MODEL <- 18

#REPLICATION
O_PROPOSED_TRUE_MODEL <- 19
O_SWITCH_TESTING <- 20
O_SWITCH_CHANGING <- 21
O_SWITCH_ALREADY_TRUE <-22
O_SWITCH_ALREADY_TRUE_CHANGING <- 23
O_SWITCH_ALREADY_TRUE_REJECTING <- 24
O_SWITCH_REPLICATION_STUDY <- 25
O_SWITCH_REPLICATED <- 26
O_SWITCH_NOT_REPLICATED <- 27

#PROPORTIONS 
O_PROPORTION_1 <- 28
O_PROPORTION_2 <- 29
O_PROPORTION_3 <- 30
O_PROPORTION_4 <- 31
O_PROPORTION_5 <- 32
O_PROPORTION_6 <- 33
O_PROPORTION_7 <- 34
O_PROPORTION_8 <- 35
O_PROPORTION_9 <- 36
O_PROPORTION_10 <- 37
O_PROPORTION_11 <- 38
O_PROPORTION_12 <- 39
O_PROPORTION_13 <- 40
O_PROPORTION_14 <- 41
O_PROPORTION_TRUE <- 42


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
                   "colab_cond", #colab condition or no. 
                   "orig_type", #type of agent on turn 
                   "strategy", #colab, neighbor, research. 
                   "selected_model",#sampled model 
                   "switch_total", #new: for sanity.
                   "orig_agents", #new: for sanity.
                   "switch_agents", #new: for sanity. 
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
                   "prop_1", 
                   "prop_2", 
                   "prop_3", 
                   "prop_4", 
                   "prop_5", 
                   "prop_6", 
                   "prop_7", 
                   "prop_8", 
                   "prop_9", 
                   "prop_10", 
                   "prop_11", 
                   "prop_12", 
                   "prop_13", 
                   "prop_14", 
                   "prop_true")

## Parameter output
P_TMODEL <- 1
P_BASE_SAMPLE_SIZE <- 2
P_SIGMA <- 3
P_NET_TYPE <- 4 #small, lattice, PT or TOM
P_NET_SIZE <- 5 #size..
P_POP <- 6 
P_NETWORK <- 7

