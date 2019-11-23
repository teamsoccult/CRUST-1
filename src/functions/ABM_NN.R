"Now made compatible with the entire framework. 
Recent additions: Cap of studies made compatible with neighbor's neighbor.
Last edit on 12/11/2019"

# should anything change here - do we use all of it? 
ABM_NN <- function(replications, turns, models, k, 
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
      g <- sample_smallworld(1, net_size, 3, 0.05)
    }
    else if(net_type == "Lattice"){
      g <- make_lattice(length = net_size, dim = 1, 
                        nei = 3, circular = T, directed = F)
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
    
    
    ## generate statistics ##
    
    sampleSize <- base_sampleSize
    Xset <- generateXSet(sampleSize, k, correlation)
    #BETAS#
    tModelBetas <- getBetas(tModel, weights, sigma)
    deterministic <- calculateDet(tModel, Xset, weights, tModelBetas)
    
    
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
      
      ## Finding proposed models ##
        
      #how many agents are at the true model?
      init_gModel <- init_gModel + as.numeric(compareModels(tModel, strToModel(V(g)$model[agentIndex],k)))
      gModel <- strToModel(V(g)$model[agentIndex], k)
      
     strategy <- "research" #hard set for first part of the paper
        
        #what happens on RESEARCH (the old system copied)
        if(strategy == "research"){ 
          if(V(g)$type[agentIndex] == "Tess"){ 
            model <- modelSimilarByTerm(gModel, models, mode="random",
                                                 modelSelection=modelSelection)
            
            ## Mave
          } else if(V(g)$type[agentIndex] == "Mave"){
            model  <- models[[as.integer(
              runif(1, min=1,max=length(models)+1))]]
            #the change here might not be necessary if it can be changed in models.
            
            ## Bo 
          } else if(V(g)$type[agentIndex] == "Bo"){
            model <- modelSimilarByInteraction(gModel, 
                                                        models, mode="random",
                                                        modelSelection=modelSelection)
          }
          
        } 
      
     #generate stats
      Yset <- generateY(deterministic, sigma)
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
          old_gModel <- gModel 
          #local change of model.
          modelStr <- modelToStr(model)
          modelStr <- str_replace(modelStr, "Y ~", "")
          modelStr <- str_replace_all(modelStr, ":", "")
          V(g)$model[agentIndex] <- modelStr
          
        } 
        #How many agents end at the true? 
        final_gModel <- final_gModel + as.numeric(compareModels(tModel, strToModel(V(g)$model[agentIndex],k)))
      
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
        
        if(switchModel){
          #has to be reset for every i (not redundant).  
          switch_not_same_global <- NULL 
          switch_same_global <- NULL  
          
          #total edges of the agent switching. 
          switch_total <- names(which(matrix_g[, agentIndex] == 1)) 
          
          for(j in switch_total){
            
            #logging total count of legible switch agents. 
            switch_all <- switch_all + 1
            
            #generating models of the switch agent. 
            switch_agentToken <- j
            switch_agentIndex <- which((V(g)$name) == switch_agentToken)
            switch_gModel <- strToModel(V(g)$model[switch_agentIndex],k)
            
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
                as.numeric(compareModels(switch_gModel, old_gModel))  
              
              ## already at true model (atm). 
              switch_already_true <- switch_already_true + 
                as.numeric(compareModels(tModel, switch_gModel)) 
              
              ## statistics ##
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
                as.numeric(compareModels(switch_gModel, old_gModel)) 
              
              switch_already_true_changing <- switch_already_true_changing + 
                as.numeric(compareModels(switch_gModel, tModel))
              
              modelStr <- modelToStr(model)
              modelStr <- str_replace(modelStr, "Y ~", "")
              modelStr <- str_replace_all(modelStr, ":", "")
              V(g)$model[switch_agentIndex] <- modelStr
            } 
            
            ### LOGGING REPLICATION INFORMATION ###
            else {
              
              #how many rejected & had the true model initially. 
              switch_already_true_rejecting <- switch_already_true_rejecting +
                as.numeric(compareModels(switch_gModel, tModel))
              
              #how many did not replicate
              switch_not_replicated <- switch_not_replicated +
                as.numeric(compareModels(switch_gModel, old_gModel))
            }
          }
          #hyper-test
          if(study >= study_cap){
            break
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
      output[turn, O_COLAB_COND] <- colab
      
      #LOCAL ID#
      output[turn, O_TYPE] <- V(g)$type[agentOriginal] 
      output[turn, O_STRATEGY] <- strategy
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
    param[[P_NETWORK]] <- matrix_g
    parameters[[replica]] <- param
    
    ## Convert output matrix into data table ##
    output <- data.table(cbind(rep(replica, turns), 1:turns, output))
    names(output) <- OUTPUT_HEADER
    
    ## Write output data table into a file ##
    write.table(output, file=paste0(outputDir, "/", net_type, "_", pop_type, "_", 
                                    sigma, "_", paste(ifelse(modelCompare == 5, "BIC", "AIC")), 
                                    "_", net_size, "_", modelSelection, "_", 
                                    tMod, "_", "NN", 
                                    outputFile),
                append=ifelse(replica == 1, FALSE, TRUE),
                quote=FALSE, sep=",", row.names=FALSE,
                col.names=ifelse(replica == 1, TRUE, FALSE))
  }
  
  saveRDS(parameters, file=paste0(outputDir, "/", net_type, "_", pop_type, "_", 
                                  sigma, "_", paste(ifelse(modelCompare == 5, "BIC", "AIC")), 
                                  "_", net_size, "_", modelSelection, "_", base_sampleSize, "_", 
                                  tMod, "_", "NN",
                                  paramFile))
}
