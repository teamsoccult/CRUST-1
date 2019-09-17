"the function running our ABM, changed september 2019. Model sampling with 
neighbors but without the sophistication of ABM5, so should be redundant.
Also, it has not been cleaned like ABM5."

# should anything change here?
ABM4 <- function(replications, turns, models, k, 
                weights, sampleSize, correlation, sigma,
                modelCompare, modelSelection, inputDir, outputDir, outputFile, paramFile,
                verbose, ndec, seeds, some_models){
  
  ## REPLICA SIMULATION
  ###################
  parameters <- list()
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
    #does it work?
    for(turn in 1:turns){ #11000 time-turns
      
      #if(study < turns){
      ## initialize parameters 
      edges_testing <- 0 #new change
      edges_changing <- 0 #new change
      edges_already_true <- 0 #new change
      edges_already_true_changing <- 0 #new change!!
      edges_already_true_rejecting <- 0 #new change
      edges_replication_study <- 0 #new change!!
      edges_replicated <- 0 #new change
      edges_not_replicated <- 0 #new change
      edges_total <- NULL #from NULL
      #edges_not_same_global <- 0 #not same
      #edges_same_global <- 0 #same
      edges_not_same_global <- NULL #new!!
      edges_same_global <- NULL #new!!
      
      ## Select the token of agent
      agentToken <- agentTurn[turn] 
      
      ## Agents 
      agentIndex <- which((V(g)$name) == agentToken)
      
      ## model & in format
      gModel <- strToModel(V(g)$model[agentIndex], k) #local model
      
      ## crazy new segment w. selection 
      strategy <- sample(c('research','neighbor'), size=1, prob=c(.50,.50))
      
      if(strategy == "research"){
        if(V(g)$type[agentIndex] == "Tess"){ #detect
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
        
      }
      
      if(strategy == "neighbor"){
        edges_total <- names(which(matrix_g[, agentIndex] == 1))
        chosen_edge <- sample(edges_total, size = 1)
        edge_agentIndex <- which((V(g)$name) == chosen_edge)
        model <- strToModel(V(g)$model[edge_agentIndex],k)
      }
      
      #new commented. 
      #model1 <- model 
      #model2 <- gModel
      
      #adding variance & statistics for different engines (AIC, BIC)
      Yset <- generateY(deterministic, sigma) #putting variance on the deterministic.
      stat <- analysis(model, gModel, Yset, Xset, weights) #computing all the shit.
      
      #smooth -> should we switch
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
      
      #what do you do?
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
        
        #this ends the original agents turn
      } else {
        finalGModelStat <- stat$gModel
      }
      
      if(switchModel){ #NOW STARTS FUCKED. 
        #making ready for the loop. 
        edges_total <- names(which(matrix_g[, agentIndex] == 1)) #total edges 
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
        
        # logging 
        #timeturns <- timeturns - length(edges_not_same_global) #new!!
        
        #the edges now do stuff. 
        for(i in edges_not_same_global){ #new!!
          
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
          } #else { 
            #finalGModelStat <- stat$gModel #potential bug.
          #}
          
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
            
          } #this ends the for loop ()
          
          #did they already have the true model?
          else {
            
            #how many rejected rightfully
            edges_already_true_rejecting <- edges_already_true_rejecting +
              as.numeric(compareModels(edge_gModel, tModel))
            
            #how many did not replicate
            edges_not_replicated <- edges_not_replicated +
              as.numeric(compareModels(edge_gModel, old_gModel))
            
          }
          
          #else {
            #finalGModelStat <- stat$gModel #potential bug. 
          #}
        }
      }
      
      #trying to change turn 
      study <- study + (1 + edges_testing)
      
      ## Record output data
      output[turn, O_STUDIES] <- study
      output[turn, O_NETWORK] <- net_type #new
      output[turn, O_POPULATION] <- pop_type #new
      output[turn, O_SIGMA] <- sigma #new 
      output[turn, O_NET_SIZE] <- net_size #new
      output[turn, O_SAMPLE_SIZE] <- sampleSize #new
      output[turn, O_TRUE_MODEL] <- tm #new
      output[turn, O_STRATEGY] <- V(g)$type[agentIndex] #new.
      output[turn, O_AGENT_INDEX] <- V(g)$name[agentIndex] #new
      output[turn, O_SELECTED_MODEL] <- searchModel(model, models)
      output[turn, O_SELECTED_TRUE_MODEL] <- as.numeric(compareModels(model, tModel))
      output[turn, O_SELECTED_MODEL_DISTANCE] <- round(calculateDistance(tModelBetas, stat$model$betasEst), ndec)
      output[turn, O_INITIAL_GLOBAL_MODEL] <- searchModel(initGModel, models)
      output[turn, O_INITIAL_GLOBAL_TRUE_MODEL] <- as.numeric(compareModels(initGModel, tModel))
      output[turn, O_INITIAL_GLOBAL_MODEL_DISTANCE] <- round(calculateDistance(tModelBetas, initGModelStat$betasEst), ndec)
      output[turn, O_FINAL_GLOBAL_MODEL] <- searchModel(gModel, models)
      output[turn, O_FINAL_GLOBAL_TRUE_MODEL] <- as.numeric(compareModels(gModel, tModel))
      output[turn, O_FINAL_GLOBAL_MODEL_DISTANCE] <- round(calculateDistance(tModelBetas, finalGModelStat$betasEst), ndec)
      output[turn, O_BETA1_TRUE] <- round(tModelBetas[,2], ndec)
      output[turn, O_BETA1_ESTIMATE] <- round(finalGModelStat$betasEst[2], ndec)
      output[turn, O_BETA1_ERROR] <- round(finalGModelStat$betasErr[2], ndec)
      output[turn, O_TSTATISTICS] <- round(finalGModelStat$tstatistics, ndec)
      output[turn, O_RSQ] <- round(finalGModelStat$rsq, ndec)
      output[turn, O_ARSQ] <- round(finalGModelStat$arsq, ndec)
      output[turn, O_AIC] <- round(finalGModelStat$aic, ndec)
      output[turn, O_BIC] <- round(finalGModelStat$bic, ndec)
      output[turn, O_EDGES_TESTING] <- edges_testing #new
      output[turn, O_EDGES_CHANGING] <- edges_changing #new
      output[turn, O_EDGES_ALREADY_TRUE] <- edges_already_true #new
      output[turn, O_EDGES_ALREADY_TRUE_CHANGING] <- edges_already_true_changing #new!!
      output[turn, O_EDGES_ALREADY_TRUE_REJECTING] <- edges_already_true_rejecting #new
      output[turn, O_EDGES_REPLICATION_STUDY] <- edges_replication_study #new
      output[turn, O_EDGES_REPLICATED] <- edges_replicated #new
      output[turn, O_EDGES_NOT_REPLICATED] <- edges_not_replicated #new
      output[turn, O_PROPORTION_1] <- mean(as.numeric(V(g)$model == some_models[1])) #new
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
      output[turn, O_PROPORTION_TRUE] <- mean(as.numeric(V(g)$model == trueModel)) #new
      output[turn, O_EDGES_TOTAL] <- length(edges_total) #new!!
      output[turn, O_EDGES_SAME_GLOBAL] <- length(edges_same_global) #new!!
      output[turn, O_EDGES_NOT_SAME_GLOBAL] <- length(edges_not_same_global) #new!!
    }
  #}
    ## Parameter output
    param <- list()
    param[[P_TMODEL]] <- gsub(" ", "", substr(modelToStr(tModel), 5,
                                              nchar(modelToStr(tModel))), 
                              fixed=TRUE)
    param[[P_K]] <- k
    param[[P_SAMPLE_SIZE]] <- sampleSize
    param[[P_SIGMA]] <- sigma
    param[[P_CORRELATION]] <- correlation
    #param[[P_AGENT_WEIGHTS]] <- c(nRey, nTess, nBo, nMave) /
    #(nRey + nTess + nBo + nMave)
    param[[P_TRUE_BETAS]] <- tModelBetas
    param[[P_XSET]] <- Xset
    param[[P_NETWORK]] <- matrix_g
    param[[P_AVG_PATH_LENGTH]] <- average.path.length(g)
    param[[P_CLUSTER_COEF]] <- transitivity(g)
    parameters[[replica]] <- param
    
    ## Convert output matrix into data table
    output <- data.table(cbind(rep(replica, turns), 1:turns, output))
    names(output) <- OUTPUT_HEADER
    #output <- na.omit(output)
    
    ## Write output data table into a file
    write.table(output, file=paste0(outputDir, "/", net_type, "_", pop_type, "_", 
                                    sigma, "_", criterion, "_", net_size, "_", 
                                    modelSelection, "_", sampleSize, "_", 
                                    tm, outputFile),
                append=ifelse(replica == 1, FALSE, TRUE),
                quote=FALSE, sep=";", row.names=FALSE,
                col.names=ifelse(replica == 1, TRUE, FALSE))
  }
  
  saveRDS(parameters, file=paste0(outputDir, "/", net_type, "_", pop_type, "_", 
                                  sigma, "_", criterion, "_", net_size, "_", 
                                  modelSelection, "_", sampleSize, "_", tm, 
                                  paramFile))
}
