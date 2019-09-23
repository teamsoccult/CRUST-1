"The function running our ABM (with new selection).
changed 14-09-2019. This document can accomodate running all 
combinations of parameters at once. I have cleaned it a bit,
but we should still go over it together. 
1. check whether the function needs all its arguments. 
2. check that we understand the code at all times. 
3. comment it better than we already have."

# should anything change here - do we use all of it? 
ABMqualitative <- function(replications, turns, models, k, 
                weights, sampleSize, correlation, sigma,
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
    test <- ifelse(net_type == "Small", #changed from "small"
                   "yes", "no")
    
    if(test == "yes"){
      g <- sample_smallworld(1, 100, 2, 0.05)
    }
    else if(test == "no"){
      g <- make_lattice(length = 100, dim = 1, 
                        nei = 2, circular = T, directed = F)
    }
    
    if(pop_type == "All"){ #epi --> All 
      V(g)$type <- sample(c("Bo", "Mave", "Tess"))
    }
    else if(pop_type == "Bo"){ #bo --> Bo 
      V(g)$type <- rep(c("Bo", "Mave", "Tess"), c(98,1,1))
    }
    else if(pop_type == "Tess"){ #tess --> Tess
      V(g)$type <- rep(c("Tess", "Bo", "Mave"), c(98,1,1))
    }
    else if(pop_type == "Mave"){ #mave --> Mave
      V(g)$type <- rep(c("Mave", "Bo", "Tess"), c(98,1,1))
    }
    
    # Sampling agents 
    V(g)$type <- sample(V(g)$type)
    V(g)$name <- make.unique(V(g)$type) 
    V(g)$model <- sample(some_models)
    V(g)$model <- sample(V(g)$model)
    matrix_g <- as_adjacency_matrix(g, sparse = F) 
    agentTurn <- sample(V(g)$name, size = 11000, replace = T) 
    
    ## Generate Xset and deterministic value of the linear regression
    #trueModel <- some_models[replica]
    #tModel <- strToModel(trueModel, k)
    Xset <- generateXSet(sampleSize, k, correlation)
    tModelBetas <- getBetas(tModel, weights, sigma)
    deterministic <- calculateDet(tModel, Xset, weights, tModelBetas)
    #######################
    ## OUTPUT PARAMETERS
    #######################
    output <- matrix(data=NA, nrow=turns, ncol=O_NUM_FIELDS)
    
    #######################
    ## SIMULATION EXECUTION
    #######################
    turn <- 1 
    study <- 0
    
    for(turn in 1:turns){ #11000 time-turns
      
      ## initialize parameters 
      edges_testing <- 0 
      edges_changing <- 0 
      edges_already_true <- 0 
      edges_already_true_changing <- 0 
      edges_already_true_rejecting <- 0 
      edges_replication_study <- 0 
      edges_replicated <- 0 
      edges_not_replicated <- 0
      edges_total <- NULL 
      edges_not_same_global <- NULL #we have this twice.  
      edges_same_global <- NULL #we have this twice. 
      
      ## Select the token of agent
      agentToken <- agentTurn[turn] 
      
      ## Agents 
      agentIndex <- which((V(g)$name) == agentToken)
      
      ## model & in format
      gModel <- strToModel(V(g)$model[agentIndex], k) #local model
      
      ### NEW MODEL SELECTION BLOCK ### 
      
      edges_legible <- list() #empty list.
      
      #Getting edges of agent on turn 
      edges_total <- names(which(matrix_g[, agentIndex] == 1)) #all edges from agent on turn.
      for(i in edges_total){ #looping over those edges. 
        
        #token for the i'th edge.  
        edgeToken <- i #should be edgeToken, not agentToken (as before)
        
        #index for the i'th edge
        edge_agentIndex <- which((V(g)$name) == edgeToken)
        
        #checking their model against agent on turn. 
        #if same then nothing happens. 
        #if not the same then their token is put into edges_legible
        if(identical(gModel, strToModel(V(g)$model[edge_agentIndex],k))){
        } else {  
          edges_legible[i] <- edgeToken
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
        if(V(g)$type[agentIndex] == "Tess"){ 
          model <- modelSimilarByTerm(gModel, models, mode="random",
                                      modelSelection=modelSelection)
          
          ## Mave
        } else if(V(g)$type[agentIndex] == "Mave"){
          model <- models[[as.integer(runif(1, min=1, max=length(models)+1))]]
          
          ## Bo 
        } else if(V(g)$type[agentIndex] == "Bo"){
          model <- modelSimilarByInteraction(gModel, models, mode="random",
                                             modelSelection=modelSelection)
        }
        
      } else { #what happens on NEIGHBOR (could be else if, but it has to be if not research)
        chosen_edge <- sample(edges_legible, size = 1) #sampling just 1 from the list. 
        edge_agentIndex <- which((V(g)$name) == chosen_edge) #finding index.
        model <- strToModel(V(g)$model[edge_agentIndex],k) #finding model. 
      }
      
      ### MODEL SELECTION STOPS ###
      
      ### MODEL STATISTICS GENERATION ###
      #adding variance & statistics for different engines (AIC, BIC)
      Yset <- generateY(deterministic, sigma) #putting variance on the deterministic.
      stat <- analysis(model, gModel, Yset, Xset, weights) #computing all the shit.
      
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
      
      #we need to figure out what we use this for. 
      initGModel <- gModel 
      initGModelStat <- stat$gModel
      
      #changing global if TRUE (for recording)
      if(switchModel){
        #from the old. 
        old_gModel <- gModel #new addition!! (loggin the old model for replication).
        gModel <- model 
        finalGModelStat <- stat$model
        #local change of model
        modelStr <- modelToStr(gModel)
        modelStr <- str_replace(modelStr, "Y ~", "")
        modelStr <- str_replace_all(modelStr, ":", "")
        V(g)$model[agentIndex] <- modelStr
        
      } else {
        finalGModelStat <- stat$gModel
      }
      
      ### END OF ORIGINAL AGENTS TURN ###
      
      ### IF MODEL-SWITCH THEN EDGES NOW TEST ### 
      
      if(switchModel){ 
        #making ready for the loop. 
        edges_total <- names(which(matrix_g[, agentIndex] == 1)) #total edges (already have this??)
        edges_not_same_global <- NULL #new!!
        edges_same_global <- NULL #new!!
        
        for(i in edges_total){
        
        #which one  
        agentToken <- i #"Mave.13" 
        
        #agents
        edge_agentIndex <- which((V(g)$name) == agentToken)
        
        #model & in format
        edge_gModel <-strToModel(V(g)$model[edge_agentIndex],k)
        
        #testing 
        if(!compareModels(edge_gModel, gModel)){ #if they are the same then it does
        edges_not_same_global <- c(edges_not_same_global, agentToken)
        } else {
        edges_same_global <- c(edges_same_global, agentToken)
        }
        }
        
        ### THE EDGES WITH DIFF. GLOBAL MOD. NOW TEST ### 
        
        for(i in edges_not_same_global){ 
          
          ## tried 
          edges_testing <- edges_testing + 1 #how many tested. 
          
          ## which one 
          agentToken <- i
          
          ## Agents 
          edge_agentIndex <- which((V(g)$name) == agentToken) #new change.
          
          ## model & in format
          edge_gModel <- strToModel(V(g)$model[edge_agentIndex], k) #new change.
          
          ## tried replication
          edges_replication_study <- edges_replication_study + 
            as.numeric(compareModels(edge_gModel, old_gModel)) 
          
          #adding variance & statistics for different engines (AIC, BIC)
          Yset <- generateY(deterministic, sigma) #putting variance on the                  deterministic.
          stat <- analysis(gModel, edge_gModel, Yset, Xset, weights) #new change!! 
          
          ## already at true model (atm). 
          edges_already_true <- edges_already_true + 
            as.numeric(compareModels(tModel, edge_gModel)) #new change. 
          
          #smooth -> should we switch
          edge_switchModel <- FALSE
          if((modelCompare == TSTATISTICS) &
             (!is.null(stat$model$tstatistics)) &
             (!is.null(stat$gModel$tstatistics)) & #is in fact edge_gModel
             (!is.na(stat$model$tstatistics)) &
             (!is.na(stat$gModel$tstatistics))){
            if(stat$model$tstatistics > stat$gModel$tstatistics){
              edge_switchModel <- TRUE
            }
          } else if((modelCompare == RSQ) &
                    (!is.null(stat$model$rsq)) &
                    (!is.null(stat$gModel$rsq)) &
                    (!is.na(stat$model$rsq)) &
                    (!is.na(stat$gModel$rsq))){
            if(stat$model$rsq > stat$gModel$rsq){
              edge_switchModel <- TRUE
            }
          } else if((modelCompare == AIC) &
                    (!is.null(stat$model$aic)) &
                    (!is.null(stat$gModel$aic)) &
                    (!is.na(stat$model$aic)) &
                    (!is.na(stat$gModel$aic))){
            if(stat$model$aic < stat$gModel$aic){
              edge_switchModel <- TRUE
            }
          } else if((modelCompare == BIC) &
                    (!is.null(stat$model$bic)) &
                    (!is.null(stat$gModel$bic)) &
                    (!is.na(stat$model$bic)) &
                    (!is.na(stat$gModel$bic))){
            if(stat$model$bic < stat$gModel$bic){
              edge_switchModel <- TRUE
            }
          } else if((modelCompare == ARSQ) &
                    (!is.null(stat$model$arsq)) &
                    (!is.null(stat$gModel$arsq)) &
                    (!is.na(stat$model$arsq)) &
                    (!is.na(stat$gModel$arsq))){
            if(stat$model$arsq > stat$gModel$arsq){
              edge_switchModel <- TRUE
            }
          } 
          
          ### IF THEY SHOULD SHIFT ### 
          
          if(edge_switchModel){
            
            #how many changes in total 
            edges_changing <- edges_changing + 1 #new change. 
            
            #replicated
            edges_replicated <- edges_replicated + 
              as.numeric(compareModels(edge_gModel, old_gModel)) #new change!!
            
            #changed away from true 
            edges_already_true_changing <- edges_already_true_changing + #new change!!
              as.numeric(compareModels(edge_gModel, tModel))
            
            #changing their information 
            modelStr <- modelToStr(edge_gModel)
            modelStr <- str_replace(modelStr, "Y ~", "")
            modelStr <- str_replace_all(modelStr, ":", "")
            V(g)$model[edge_agentIndex] <- modelStr
            
          } 
          
          ### LOGGING REPLICATION INFORMATION ###
          else {
            
            #how many rejected rightfully
            edges_already_true_rejecting <- edges_already_true_rejecting +
              as.numeric(compareModels(edge_gModel, tModel))
            
            #how many did not replicate
            edges_not_replicated <- edges_not_replicated +
              as.numeric(compareModels(edge_gModel, old_gModel))
          }
        }
      }
      
      # how many studies conducted? 
      study <- study + (1 + edges_testing)
      
      ### Record output data ###
      output[turn, O_STUDIES] <- study
      output[turn, O_NETWORK] <- net_type 
      output[turn, O_POPULATION] <- pop_type 
      output[turn, O_SIGMA] <- sigma 
      output[turn, O_CRITERION] <- ifelse(modelCompare == 5, "BIC", "AIC")
      output[turn, O_NET_SIZE] <- net_size 
      output[turn, O_SAMPLE_SIZE] <- sampleSize 
      output[turn, O_TRUE_MODEL] <- tMod #this edition. 
      output[turn, O_AGENT_INDEX] <- V(g)$name[agentIndex] 
      output[turn, O_SELECTED_MODEL] <- searchModel(model, models)
      output[turn, O_SELECTED_TRUE_MODEL] <- as.numeric(compareModels(model, tModel))
      output[turn, O_EDGES_TESTING] <- edges_testing 
      output[turn, O_EDGES_CHANGING] <- edges_changing 
      
      #new output stuff
      output[turn, paste0("agent", 1:100)] <- a

      "selected_model",
      "selected_true",
      "edges_testing",
      "edges_changing",
    }

    ### Parameter output ###
    param <- list()
    param[[P_TMODEL]] <- tMod #BA 
    param[[P_SAMPLE_SIZE]] <- sampleSize
    param[[P_SIGMA]] <- sigma
    param[[P_NETNAME]] <- net_type #BA
    param[[P_POP]] <- pop_type #BA
    param[[P_K]] <- k #what is this. 
    param[[P_CORRELATION]] <- correlation #should we use this?
    param[[P_TRUE_BETAS]] <- tModelBetas #use this?
    param[[P_NETWORK]] <- matrix_g
    param[[P_XSET]] <- Xset #use this?
    param[[P_AVG_PATH_LENGTH]] <- average.path.length(g)
    param[[P_CLUSTER_COEF]] <- transitivity(g)
    parameters[[replica]] <- param
    
    ## Convert output matrix into data table ##
    output <- data.table(cbind(rep(replica, turns), 1:turns, output))
    names(output) <- OUTPUT_HEADER
    
    ## Write output data table into a file ##
    write.table(output, file=paste0(outputDir, "/", net_type, "_", pop_type, "_", 
                                    sigma, "_", paste(ifelse(modelCompare == 5, "BIC", "AIC")), "_", net_size, "_", 
                                    modelSelection, "_", sampleSize, "_", 
                                    tMod, outputFile),
                append=ifelse(replica == 1, FALSE, TRUE),
                quote=FALSE, sep=";", row.names=FALSE,
                col.names=ifelse(replica == 1, TRUE, FALSE))
  }
  
  saveRDS(parameters, file=paste0(outputDir, "/", net_type, "_", pop_type, "_", 
                                  sigma, "_", paste(ifelse(modelCompare == 5, "BIC", "AIC")), "_", net_size, "_", 
                                  modelSelection, "_", sampleSize, "_", tMod, 
                                  paramFile))
}
