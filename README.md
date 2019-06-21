# `TTH` The Truth Hurts - agent-based modelling of science in a network simulation

TTH is an addition to the model-centric meta-scientific framework presented in the [**CRUST-project**](https://github.com/gnardin/CRUST).

## Download Repository from GitHub
* Open a terminal
* Navigate to the directory where you want to download the CRUST code
* Type: **git clone https://github.com/teamsoccult/CRUST-1.git**

The additions made are all inside the **connectivity** folder. The description of the content of each subfolder of this folder is provided in the table below.

| **Subfolder** | **Description** |
| :------------ | :-------------- |
| _ABM_                  | Contains the main markdown file with the agent-based model |
| _loads_                | Contains certain the models and the description of the models used for the ABM |
| _networks_             | Generates and plots different networks. This is only for testing and plotting purposes |
| _outcome measures_     | Calculates the outcome measures for replicas |
| _plots_                | Generating the different plots |

## Additional functions

In addition to our ABM-function, several of the original functions from the [**CRUST-project**](https://github.com/gnardin/CRUST) were needed. 
The following is a table of the different functions names, inputs and description. Notably, we fixed an error with generateModels.R, which generated the right models but in the wrong order. We altered constants to include the constants we were interested in. 

| Script File Name              | Function                  | Description |
|-------------------------------|---------------------------|-------------|
| _analysis.R_                  | `analysis(sModel, gModel, yset, xset, weights)` | Calculate the statistics for the selected and the global models assuming data generated under the True Model, randomly generated X values, and betas weigths. |
| _calculateDet.R_              | `calculateDet(model, xset, weights, betas)` | Calculate the deterministic part of a model |
| _calculateDistance.R_         | `calculateDistance(betas1, betas2)` | Calculate the distance among betas of two models |
| _compareModels.R_             | `compareModels(model1, model2)` | Compare if two models are the equal |
| _constants.R_                 |                                 | Define all the constants |
| _convertBinary.R_             | `convertBinary(v, k)` | Convert a number into binary format |
| _generateBetas.R_             | `generateBetas(models)` | Set the weights of all betas |
| _generateModels.R_            | `generateModels(k)` | Generate all possible models with factor k |
| _generateXSet.R_              | `generateXSet(n, k, correlation)` | Generate a set n of predictor values |
| _generateY.R_                 | `generateY(deterministic, sigma)` | Generate a set of stochastic values under the True Model |
| _getBetas.R_                  | `getBetas(model, weights, sigma)` | Generate a set of random betas to the True Model |
| _getModelComparison.R_        | `getModelComparison(xset, sampleSize, tModel, sigma, models, nIter, ms, msConstant)` | Calculate the ProbMC of switching from model i to model j. |
| _getModelSelectionConstant.R_ | `getModelSelectionConstant(models, xset)` | Generate the constants to be used at `getModelComparison.R` |
| _getPredictors.R_             | `getPredictors(models)` | Get a list of predictors of all models |
| _modelSimilarByInteraction.R_ | `modelSimilarByInteraction(model, models, mode=["all", "random"], modelSelection=["hard", "soft"])` | Generate a similar model adding an interaction |
| _modelSimilarByTerm.R_        | `modelSimilarByTerm(model, models, mode=["all", "random"], modelSelection=["hard", "soft"])` | Generate a similar model adding or removing a term |
| _modelToStr.R_                | `modelToStr(model)` | Convert a model represented as a matrix into a string format |
| _searchModel.R_               | `searchModel(model, models)` |Search for the index of the model in a list of models |
| _seedGenerator.R_             | `seedGenerator(N, filename)` | Upload seeds from a text file or generate them randomly |
| _simulator.R_                 | `simulator(replications, timesteps, models, k, tModel, nRey, nTess, nBo, nMave, weights, sampleSize, correlation, sigma, modelCompare, modelSelection, inputDir, outputDir, outputFile, paramFile, verbose, ndec, seeds)` | Execute a certain number of replications of the reproducibility model |
| _strToModel.R_                | `strToModel(modelStr, k)` | Convert a model represented as a string into a matrix format |
