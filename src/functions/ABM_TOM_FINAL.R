"Now made compatible with the entire framework. 
Recent additions: Cap of studies made compatible with neighbor's neighbor.
Last edit on 12/11/2019"

# should anything change here - do we use all of it? 
ABM_TOM <- function(replications, turns, models, k, 
                   weights, base_sampleSize, correlation, sigma,
                   modelCompare, modelSelection, inputDir, outputDir, 
                   outputFile, paramFile, verbose, ndec, seeds, some_models){
  
  ## REPLICA SIMULATION
  ###################
  parameters <- list()
  
  #Has to be done to run all true models in one go. 
  trueModel <- some_models[tMod] #tMod er en liste med 2, 7, 13. 
  tModel <- strToModel(trueModel, k) #conversion. 
  
  for(replica in 1:replications){
    set.seed(seeds[replica])
    
    ## Select randomly Global Model and previous Global Model
    # Sampling / randomizing stuff. 
    
    if(net_type == "Small"){
      g <- sample_smallworld(1, net_size, 4, 0.05)
    }
    else if(net_type == "Lattice"){
      g <- make_lattice(length = net_size, dim = 1, 
                        nei = 4, circular = T, directed = F)
    }
    else if(net_type == "TOM"){
      g <- readRDS("~/CRUST-1/citation_network/theory_of_mind.rds")
    }
    
    if(pop_type == "All"){ #epi --> All 
      V(g)$type <- sample(c("Bo", "Mave", "Tess"))
    }
    else if(pop_type == "Bo"){ #bo --> Bo 
      V(g)$type <- rep(c("Bo", "Mave", "Tess"), 
                       round(c(net_size*0.98, net_size*0.01, net_size*0.01)))
    }
    else if(pop_type == "Tess"){ #tess --> Tess
      V(g)$type <- rep(c("Tess", "Bo", "Mave"), 
                       round(c(net_size*0.98, net_size*0.01, net_size*0.01)))
    }
    else if(pop_type == "Mave"){ #mave --> Mave
      V(g)$type <- rep(c("Mave", "Bo", "Tess"), 
                       round(c(net_size*0.98, net_size*0.01, net_size*0.01)))
    }
    
    # Sampling agents 
    V(g)$type <- sample(V(g)$type)
    V(g)$name <- make.unique(V(g)$type) 
    V(g)$model <- sample(some_models)
    V(g)$model <- sample(V(g)$model)
    matrix_g <- as_adjacency_matrix(g, sparse = F) 
    agentTurn <- sample(V(g)$name, size = turns, replace = T) 
    
    #######################
    ## OUTPUT PARAMETERS
    #######################
    output <- matrix(data=NA, nrow=turns, ncol=O_NUM_FIELDS)
    
    #######################
    ## SIMULATION EXECUTION
    #######################
    turn <- 0
    study <- 0
      
    while(study < study_cap){ #new condition. 
      turn <- turn + 1 
        
      #INITIALIZING PARAMETERS:
      propModel <- list() #CHANGE IN NAME: from prop_m to propModel
      agentsSwitch <- list() #original agents switching model. 
      old_gModel <- list() #now a list in order to check replications for colabs. 
      init_gModel <- 0
      final_gModel <- 0
      
      ## Select the token of agent
      agentToken <- agentTurn[turn] 
      
      ## Agents 
      agentIndex <- which((V(g)$name) == agentToken)
      agentOriginal <- agentIndex #do we need this information for logging, come back?
      
      ####### SECTION 1 #######
      ## Colab study ##
      colab <- sample(c('yes', 'no'), size = 1, prob = c(colab_prob, 1-colab_prob))
      
      if((study_cap - study) <= 10){
        
        colab <- "no" #NEW CHANGE - EXCLUDING THE POSSIBILITY OF COLAB 
      }
      
      ## Finding agents conducting study ##
      if(colab == 'yes'){
        
        ## Makes a list of the agents indexes ##
        agents_testing <- names(which(matrix_g[, agentIndex] == 1)) #new variable.
        
        ## Appending the original agent
        agents_testing <- append(agents_testing, 
                                 agentToken, after = length(agents_testing))
        
        #empty list holding agents in original study if meta. 
        agentIndex <- list()
        list_additional_agents <- list()
        
        for(i in agents_testing){
          agentIndex[i] <- which((V(g)$name) == i)
        }
        
        #BASE COUNTER OF LAYERS:
        N = 1
        
        #GIVING THE ORIGINAL AGENTS THE COUNTER 1:
        for (i in seq_len(length(agentIndex))){
          agentIndex[1:length(agentIndex)][[i]] <- N
        }
        
        #IF THERE ARE MORE THAN 10 GUYS, THEN OLD_LENGTH IS 0
        old_length <- 0
        
          repeat{ #REPEATS THE FOLLOWING PART UNTIL length(agentIndex >= 10)
            
            if(length(agentIndex) >=10) {
              break
            }
            
            #THE LENGTH OF EVERYTHING BUT THE OUTER LAYER
            old_length <- length(agentIndex)
            
            #COUNTER OF LAYERS:
            N = N+1
            
            #FINDING THE NEIGHBORS OF THE NEIGHBORS:
            
            for (j in names(agentIndex)) {
              
              ## Makes a list of the agents indexes ##
              
              additional_agents <- names(which(matrix_g[, j] == 1))
              for (t in additional_agents){
                list_additional_agents[[t]] <- which((V(g)$name == t))
              }
            
              #ONLY INCLUDING THOSE WHO ARENT IN AGENT INDEX
              list_additional_agents <- list_additional_agents[!(names(list_additional_agents) %in% names(agentIndex))]
              
              #FIGURING OUT WHAT ITERATION THE AGENTS WHERE FOUND IN
              for (i in seq_len(length(list_additional_agents))){
                list_additional_agents[1:length(list_additional_agents)][[i]] <- N
              }
            
              #ALL THE AGENTS FOUND DURING THE REPEAT LOOP:
              agentIndex <- append(agentIndex, list_additional_agents, after = length(agentIndex))
            }
          } #END OF REPEAT
        
        ###SAMPLING THE NEW AGENTS
        
        #OUTER LAYER:
        
        sample_agents <- agentIndex[which(agentIndex == N)]
        
        #INNER LAYER:
        
        agentIndex <- agentIndex[which(!(agentIndex == N))] 
        
        #SAMPLING BASED ON THE OLD LENGTH
        
        sample_agents <- sample(sample_agents, size = (10 - old_length))
        
        #FINAL APPEND
        agentIndex <- append(agentIndex, sample_agents, after = length(agentIndex))
        
        #GETTING BACK TO THE RIGHT FORMAT
        for (i in seq_len(length(agentIndex))){
          agentIndex[1:length(agentIndex)][[i]] <- which((V(g)$name) == names(agentIndex)[i])
        }
      }
      
      ## Finding proposed models ##
      for(i in seq_len(length(agentIndex))){ 
        
        #how many agents are at the true model?
        init_gModel <- init_gModel + as.numeric(compareModels(tModel, strToModel(V(g)$model[agentIndex[[i]]],k)))
        
        #initializing lists 
        edges_legible <- list() 
        edges_total <- list() 
        
        #edges of the i'th agent 
        edges_total <- names(which(matrix_g[, agentIndex[[i]]] == 1)) 
        gModel <- strToModel(V(g)$model[agentIndex[[i]]], k)
        
        ## continuing ##
        for(j in edges_total){ #looping over those edges. 
          
          #token for the i'th edge.  
          edgeToken <- j 
          
          #index for the i'th edge
          edge_agentIndex <- which((V(g)$name) == edgeToken)
          
          #checking their model against agent on turn. 
          #if same then nothing happens. 
          #if not the same then their token is put into edges_legible
          if(identical(strToModel(V(g)$model[agentIndex[[i]]],k),
                       strToModel(V(g)$model[edge_agentIndex],k))){ #check this code. 
          } else {  
            edges_legible[j] <- edgeToken
          }
        }
        #determining reserach strategy. 
        if(length(edges_legible) == 0){ #if no edges in the list then strat. must be research. 
          strategy <- 'research'
        } else { #if there are edges in the list, then research strat. is probabilistic. 
          strategy <- sample(c('research', 'neighbor'), size = 1, prob = c(.50, .50))
        }
        
        #what happens on RESEARCH (the old system copied)
        if(strategy == "research"){ 
          if(V(g)$type[agentIndex[[i]]] == "Tess"){ 
            propModel[[i]] <- modelSimilarByTerm(gModel, models, mode="random",
                                                 modelSelection=modelSelection)
            
            ## Mave
          } else if(V(g)$type[agentIndex[[i]]] == "Mave"){
            propModel[[i]]  <- models[[as.integer(
              runif(1, min=1,max=length(models)+1))]]
            #the change here might not be necessary if it can be changed in models.
            
            ## Bo 
          } else if(V(g)$type[agentIndex[[i]]] == "Bo"){
            propModel[[i]] <- modelSimilarByInteraction(gModel, 
                                                        models, mode="random",
                                                        modelSelection=modelSelection)
          }
          
        } else { 
          chosen_edge <- sample(edges_legible, size = 1) 
          edge_agentIndex <- which((V(g)$name) == chosen_edge) 
          propModel[[i]] <- strToModel(V(g)$model[edge_agentIndex],k) 
        }
      }
      
      ##MODEL SELECTION BY MOST FREQUENT ## 
      
      modelstrings <- NULL
      
      for (p in seq_len(length(propModel))){
        modelstrings[p] <- modelToStr(propModel[[p]])
      }
      
      max_model <- modelstrings[which.is.max(table(modelstrings))]
      
      max_model <- str_replace(max_model, "Y ~", "")
      max_model <- str_replace_all(max_model, ":", "")
      
      model <- strToModel(max_model, k)
      
      ## generate statistics ##
      sampleSize <- length(agentIndex) * base_sampleSize
      Xset <- generateXSet(sampleSize, k, correlation)
      #BETAS#
      tModelBetas <- getBetas(tModel, weights, sigma)
      deterministic <- calculateDet(tModel, Xset, weights, tModelBetas)
      Yset <- generateY(deterministic, sigma)
      
      ## analysis ## 
      for (i in seq_len(length(agentIndex))){ 
        gModel <- strToModel(V(g)$model[agentIndex[[i]]],k) 
        stat <- analysis(model, gModel, Yset, Xset, weights) 
        
        ### MODEL CHANGE ### 
        switchModel <- FALSE
        if((modelCompare == TSTATISTICS) &
           (!is.null(stat$model$tstatistics)) &
           (!is.null(stat$gModel$tstatistics)) &
           (!is.na(stat$model$tstatistics)) &
           (!is.na(stat$gModel$tstatistics))){
          if(stat$model$tstatistics > stat$gModel$tstatistics){
            switchModel <- TRUE
          }
        } else if((modelCompare == RSQ) &
                  (!is.null(stat$model$rsq)) &
                  (!is.null(stat$gModel$rsq)) &
                  (!is.na(stat$model$rsq)) &
                  (!is.na(stat$gModel$rsq))){
          if(stat$model$rsq > stat$gModel$rsq){
            switchModel <- TRUE
          }
        } else if((modelCompare == AIC) &
                  (!is.null(stat$model$aic)) &
                  (!is.null(stat$gModel$aic)) &
                  (!is.na(stat$model$aic)) &
                  (!is.na(stat$gModel$aic))){
          if(stat$model$aic < stat$gModel$aic){
            switchModel <- TRUE
          }
        } else if((modelCompare == BIC) &
                  (!is.null(stat$model$bic)) &
                  (!is.null(stat$gModel$bic)) &
                  (!is.na(stat$model$bic)) &
                  (!is.na(stat$gModel$bic))){
          if(stat$model$bic < stat$gModel$bic){
            switchModel <- TRUE
          }
        } else if((modelCompare == ARSQ) &
                  (!is.null(stat$model$arsq)) &
                  (!is.null(stat$gModel$arsq)) &
                  (!is.na(stat$model$arsq)) &
                  (!is.na(stat$gModel$arsq))){
          if(stat$model$arsq > stat$gModel$arsq){
            switchModel <- TRUE
          }
        }
        
        #logging study. 
        study <- study + 1
        
        ### If they switch ###
        if(switchModel){ 
          old_gModel[[i]] <- gModel 
          #local change of model.
          modelStr <- modelToStr(model)
          modelStr <- str_replace(modelStr, "Y ~", "")
          modelStr <- str_replace_all(modelStr, ":", "")
          V(g)$model[agentIndex[[i]]] <- modelStr
          agentsSwitch[[i]] <- agentIndex[[i]] 
          
        } 
        #How many agents end at the true? 
        final_gModel <- final_gModel + as.numeric(compareModels(tModel, strToModel(V(g)$model[agentIndex[[i]]],k)))
      }
      
      ## Creating variables for the next part ##
      agentsSwitch = list.clean(agentsSwitch, fun = is.null)
      old_gModel = list.clean(old_gModel, fun = is.null)
      
      ### IF MODEL-SWITCH THEN EDGES NOW TEST ### 
      ### EVERYTHING CALLED "SWITCH" SOMETHING ###
      
      ## initialize SWITCH parameters ##
      switch_testing <- 0 #USED (for logging)
      switch_changing <- 0 #USED (for logging)
      switch_already_true <- 0 #USED (for logging)
      switch_already_true_changing <- 0 #USED (for logging)
      switch_already_true_rejecting <- 0 #USED (for logging)
      switch_replication_study <- 0 #USED (for logging)
      switch_replicated <- 0 #USED (for logging)
      switch_not_replicated <- 0 #USED (for logging)
      
      ## more params for SWITCH logging ##
      switch_all <- 0
      switch_global <- 0
      switch_not_global <- 0
      
      ## If any original agents switched model ##
      if(length(agentsSwitch) != 0){ 
        
        ## list of edges switching ##
        switch_agentsSwitch <- list()
        
        ## Finding relevant edges of those agents ## 
        for(i in seq_len(length(agentsSwitch))){
          
          #has to be reset for every i (not redundant).  
          switch_not_same_global <- NULL 
          switch_same_global <- NULL  
          
          #total edges of the agent switching. 
          switch_total <- names(which(matrix_g[, agentsSwitch[[i]]] == 1)) 
          
          if(colab == "yes"){ #only if colab cond. 
          switch_total <- setdiff(switch_total, agents_testing) #name??
          }
          
          ## Is edges' gModel = model (the proposed & global of orig. agents) ##
          for(j in switch_total){
            
            #logging total count of legible switch agents. 
            switch_all <- switch_all + 1
            
            #generating models of the switch agent. 
            switch_agentToken <- j
            switch_agentIndex <- which((V(g)$name) == switch_agentToken)
            switch_gModel <- strToModel(V(g)$model[switch_agentIndex],k)
            
            ## used for gatekeeping & checks ##
            if(compareModels(switch_gModel, model)){ 
              switch_same_global <- c(switch_same_global, switch_agentToken)
              
              #logging 
              switch_global <- switch_global + 1
              
            } else {
              switch_not_same_global <- c(switch_not_same_global, 
                                          switch_agentToken)
              
              #logging 
              switch_not_global <- switch_not_global + 1
            }
          }
          
          ## Variables for logging & gatekeeping ##
          if(length(switch_same_global) > 0){
            switch_same_global <- unique(unlist(strsplit(switch_same_global, " "))) 
          }
          if(length(switch_not_same_global) > 0){
            switch_not_same_global <- unique(unlist(strsplit(switch_not_same_global, " ")))
          }
          
          
          ### THE EDGES WITH DIFF. GLOBAL MOD. NOW TEST ### 
          for(y in switch_not_same_global){ 
            
            #break. 
            if(study >= study_cap){
              break
            }
            
            switch_testing <- switch_testing + 1 
            switch_agentToken <- y
            
            ## Agents 
            switch_agentIndex <- which((V(g)$name) == switch_agentToken) 
            
            ## model & in format
            switch_gModel <- strToModel(V(g)$model[switch_agentIndex], k) 
            
            ## tried replication
            switch_replication_study <- switch_replication_study + 
              as.numeric(compareModels(switch_gModel, old_gModel[[i]]))  
            
            ## already at true model (atm). 
            switch_already_true <- switch_already_true + 
              as.numeric(compareModels(tModel, switch_gModel)) 
            
            ## statistics ##
            Xset <- generateXSet(base_sampleSize, k, correlation)
            deterministic <- calculateDet(tModel, Xset, weights, tModelBetas)
            Yset <- generateY(deterministic, sigma)
            stat <- analysis(model, switch_gModel, Yset, Xset, weights)
            
            #smooth -> should we switch
            switchModel <- FALSE
            if((modelCompare == TSTATISTICS) &
               (!is.null(stat$model$tstatistics)) &
               (!is.null(stat$gModel$tstatistics)) & #is in fact switch_gModel
               (!is.na(stat$model$tstatistics)) &
               (!is.na(stat$gModel$tstatistics))){
              if(stat$model$tstatistics > stat$gModel$tstatistics){
                switchModel <- TRUE
              }
            } else if((modelCompare == RSQ) &
                      (!is.null(stat$model$rsq)) &
                      (!is.null(stat$gModel$rsq)) &
                      (!is.na(stat$model$rsq)) &
                      (!is.na(stat$gModel$rsq))){
              if(stat$model$rsq > stat$gModel$rsq){
                switchModel <- TRUE
              }
            } else if((modelCompare == AIC) &
                      (!is.null(stat$model$aic)) &
                      (!is.null(stat$gModel$aic)) &
                      (!is.na(stat$model$aic)) &
                      (!is.na(stat$gModel$aic))){
              if(stat$model$aic < stat$gModel$aic){
                switchModel <- TRUE
              }
            } else if((modelCompare == BIC) &
                      (!is.null(stat$model$bic)) &
                      (!is.null(stat$gModel$bic)) &
                      (!is.na(stat$model$bic)) &
                      (!is.na(stat$gModel$bic))){
              if(stat$model$bic < stat$gModel$bic){
                switchModel <- TRUE
              }
            } else if((modelCompare == ARSQ) &
                      (!is.null(stat$model$arsq)) &
                      (!is.null(stat$gModel$arsq)) &
                      (!is.na(stat$model$arsq)) &
                      (!is.na(stat$gModel$arsq))){
              if(stat$model$arsq > stat$gModel$arsq){
                switchModel <- TRUE
              }
            } 
            
            #logging study. 
            study = study + 1
            
            ### IF THEY SHOULD SHIFT ### 
            if(switchModel){
              
              switch_changing <- switch_changing + 1 
              
              switch_replicated <- switch_replicated + 
                as.numeric(compareModels(switch_gModel, old_gModel[[i]])) 
              
              switch_already_true_changing <- switch_already_true_changing + 
                as.numeric(compareModels(switch_gModel, tModel))
              
              modelStr <- modelToStr(model)
              modelStr <- str_replace(modelStr, "Y ~", "")
              modelStr <- str_replace_all(modelStr, ":", "")
              V(g)$model[switch_agentIndex] <- modelStr
              switch_agentsSwitch[[y]] <- switch_agentIndex
            } 
            
            ### LOGGING REPLICATION INFORMATION ###
            else {
              
              #how many rejected & had the true model initially. 
              switch_already_true_rejecting <- switch_already_true_rejecting +
                as.numeric(compareModels(switch_gModel, tModel))
              
              #how many did not replicate
              switch_not_replicated <- switch_not_replicated +
                as.numeric(compareModels(switch_gModel, old_gModel[[i]]))
            }
          }
          #hyper-test
          if(study >= study_cap){
            break
          }
        }
      } 
      
      ### Record output data ###
      
      ##IDENTIFICATION COLUMNS ##
      output[turn, O_STUDIES] <- study
      output[turn, O_NETWORK] <- net_type 
      output[turn, O_POPULATION] <- pop_type 
      output[turn, O_SIGMA] <- sigma 
      output[turn, O_NET_SIZE] <- net_size 
      output[turn, O_BASE_SAMPLE_SIZE] <- base_sampleSize 
      output[turn, O_TRUE_MODEL] <- tMod #this edition. 
      output[turn, O_COLAB_COND] <- ifelse(colab_prob > 0, "COLAB", "NOLAB")
      
      #LOCAL ID#
      output[turn, O_TYPE] <- V(g)$type[agentOriginal] 
      output[turn, O_STRATEGY] <- ifelse(colab == "yes", "colab", strategy)
      output[turn, O_SELECTED_MODEL] <- searchModel(model, models)
      
      ## META ## 
      output[turn, O_ORIG_AGENTS] <- length(agentIndex) #new: for sanity.
      output[turn, O_SWITCH_AGENTS] <- length(agentsSwitch) #new: for sanity.
      output[turn, O_SAMPLE_SIZE] <- sampleSize 
      output[turn, O_NUM_AGENTS] <- sampleSize / base_sampleSize
      
      ## STICKINESS ## 
      output[turn, O_INITIAL_GLOBAL_TRUE_MODEL] <- init_gModel
      output[turn, O_FINAL_GLOBAL_TRUE_MODEL] <- final_gModel
      
      ## REPLICATION ##
      output[turn, O_PROPOSED_TRUE_MODEL] <- as.numeric(compareModels(model, tModel))
      output[turn, O_SWITCH_TESTING] <- switch_testing 
      output[turn, O_SWITCH_CHANGING] <- switch_changing 
      output[turn, O_SWITCH_ALREADY_TRUE] <- switch_already_true 
      output[turn, O_SWITCH_ALREADY_TRUE_CHANGING] <- switch_already_true_changing 
      output[turn, O_SWITCH_ALREADY_TRUE_REJECTING] <- switch_already_true_rejecting 
      output[turn, O_SWITCH_REPLICATION_STUDY] <- switch_replication_study 
      output[turn, O_SWITCH_REPLICATED] <- switch_replicated 
      output[turn, O_SWITCH_NOT_REPLICATED] <- switch_not_replicated 
      output[turn, O_SWITCH_ALL] <- switch_all #all legible
      output[turn, O_SWITCH_GLOBAL] <- switch_global #all with same global
      output[turn, O_SWITCH_NOT_GLOBAL] <- switch_not_global #all not same global. 
      
      ## PROPORTION ## 
      output[turn, O_PROPORTION_1] <- mean(as.numeric(V(g)$model == some_models[1])) 
      output[turn, O_PROPORTION_2] <- mean(as.numeric(V(g)$model == some_models[2]))
      output[turn, O_PROPORTION_3] <- mean(as.numeric(V(g)$model == some_models[3]))
      output[turn, O_PROPORTION_4] <- mean(as.numeric(V(g)$model == some_models[4]))
      output[turn, O_PROPORTION_5] <- mean(as.numeric(V(g)$model == some_models[5]))
      output[turn, O_PROPORTION_6] <- mean(as.numeric(V(g)$model == some_models[6]))
      output[turn, O_PROPORTION_7] <- mean(as.numeric(V(g)$model == some_models[7]))
      output[turn, O_PROPORTION_8] <- mean(as.numeric(V(g)$model == some_models[8]))
      output[turn, O_PROPORTION_9] <- mean(as.numeric(V(g)$model == some_models[9]))
      output[turn, O_PROPORTION_10] <- mean(as.numeric(V(g)$model == some_models[10]))
      output[turn, O_PROPORTION_11] <- mean(as.numeric(V(g)$model == some_models[11]))
      output[turn, O_PROPORTION_12] <- mean(as.numeric(V(g)$model == some_models[12]))
      output[turn, O_PROPORTION_13] <- mean(as.numeric(V(g)$model == some_models[13]))
      output[turn, O_PROPORTION_14] <- mean(as.numeric(V(g)$model == some_models[14]))
      output[turn, O_PROPORTION_TRUE] <- mean(as.numeric(V(g)$model == trueModel)) 
    } #ending while
    
    ### Parameter output ###
    param <- list()
    param[[P_TMODEL]] <- tMod #BA 
    param[[P_BASE_SAMPLE_SIZE]] <- base_sampleSize
    param[[P_SIGMA]] <- sigma
    param[[P_NET_TYPE]] <- net_type #BA
    param[[P_NET_SIZE]] <- net_size #BA
    param[[P_POP]] <- pop_type #BA
    param[[P_COLAB]] <- ifelse(colab_prob > 0, "COLAB", "NOLAB")
    param[[P_NETWORK]] <- matrix_g
    parameters[[replica]] <- param
    
    ## Convert output matrix into data table ##
    output <- data.table(cbind(rep(replica, turns), 1:turns, output))
    names(output) <- OUTPUT_HEADER
    
    ## Write output data table into a file ##
    write.table(output, file=paste0(outputDir, "/", net_type, "_", pop_type, "_", 
                                    sigma, "_", paste(ifelse(modelCompare == 5, "BIC", "AIC")), 
                                    "_", net_size, "_", modelSelection, "_", base_sampleSize, "_", 
                                    tMod, "_", paste(ifelse(colab_prob > 0, "COLAB", "NOLAB")), 
                                    outputFile),
                append=ifelse(replica == 1, FALSE, TRUE),
                quote=FALSE, sep=",", row.names=FALSE,
                col.names=ifelse(replica == 1, TRUE, FALSE))
  }
  
  saveRDS(parameters, file=paste0(outputDir, "/", net_type, "_", pop_type, "_", 
                                  sigma, "_", paste(ifelse(modelCompare == 5, "BIC", "AIC")), 
                                  "_", net_size, "_", modelSelection, "_", base_sampleSize, "_", 
                                  tMod, "_", paste(ifelse(colab_prob == 0.02, "COLAB", "NOLAB")),
                                  paramFile))
}
