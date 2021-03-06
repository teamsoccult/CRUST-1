# `CRUST` Conceptualizing Reproducibility Using Simulations and Theory

CURST is a model-centric meta-scientific framework in which scientific discovery progresses by confirming models proposed in idealized and replication experiments.

## Software Pre-requisites

The CRUST theoretical and agent-based simulation models are written in [**R Statistics**](https://www.r-project.org/), thus it can be run on any R-supported operating system. In addition to **R v3.3.2+**, these implementations use a set of libraries that need to be installed prior to their execution:

* `caTools v.1.17.1`
* `cowplot v.0.9.2`
* `data.table v.1.10.4-3`
* `directlabels v.2017.03.31`
* `dplyr v.0.7.4`
* `ggExtra v.0.7`
* `ggplot2 v.2.2.1`
* `ggpubr v.0.1.6`
* `ggthemes v.3.4`
* `grid v.3.4.4`
* `gridExtra v.2.3`
* `Hmisc v.4.1-1`
* `lattice v.0.20-35`
* `matrixStats v.0.53.1`
* `MCMCpack v.1.4-2`
* `permute v.0.9-4`
* `reshape2 v.1.4.3`
* `stringi v.1.1.7`
* `tibble v.1.4.2`
* `tidyverse v.1.2.1`

## Download Repository from GitHub
* Open a terminal
* Navigate to the directory where you want to download the CRUST code
* Type: **git clone https://github.com/gnardin/CRUST.git**

The directory created by cloning the CRUST repository will henceforth be referred as ``<crustDir>``.

The description of the content of each directory under `<crustDir>` is provided in the table below.

| **Directory** | **Description** |
| :------------ | :-------------- |
| _data_                 | Input and output data used and generated by the scripts |
| _data/modelComparison_ | Store the model comparison probability file |
| _data/plot_            | Store the generated plots |
| _data/raw_             | Store the raw data generated by the _reproducibility.R_ script |
| _data/summary_         | Store the summary data generated by the _summary.R_ script |
| _src_                  | Script source-code|
| _src/abm_              | Agent-based model scripts |
| _src/functions_        | Auxiliary functions used by the simulation scripts |
| _src/theory_           | Theorethical simulation scripts |

## Theoretical Simulation Model

The theoretical simulation version of the reproducibility model is a temporal stochastic process of scientific discovery in which we define scientists with diverse research strategies who search the true model generating the data.

The scripts of the theoretical model are available in the directory _<crustDir>/src/theory_.

### noReplicator.R
The _noReplicator.R_ script is the main script to execute the reproducibility theoretical simulation.

#### Configuration
You can configure the theoretical reproducibility model by changing the values of the configuration parameters in the _noReplicator.R_ script file as described below.

| **Parameter** | **Description** |
| :------------ | :-------------- |
| `baseDir`     | Full path to the base directory of `CRUST` (i.e., `<crustDir>`) |
| `inpuDir`     | Directory where the model comparison probability file is stored (default: `<crust>/data/modelComparison`) |
| `outputDir`   | Directory where to store plots generated (default: `<crust>/data/plot`) |
| `k`           | Maximum number of factors that linear models explored by scientistics |
| `sigma`       | Data generation error variance |

#### Execution

To execute the theoretical simulation:

* Navigate to the `<crustDir>` folder
* Edit the _src/theory/noReplicator.R_ script file and set the parameters described in the **Configuration**
* Execute: **Rscript src/theory/noReplication.R --no-save**

### modelComparisonProbabilitiesByMonteCarlo.R
The _modelComparisonProbabilitiesByMonteCarlo.R_ script generates estimates for the `noReplicator.R` script using the Monte Carlo method.

#### Configuration
You can configure the script by changing the values of the configuration parameters in the _modelComparisonProbabilitiesByMonteCarlo.R_ script file as described below.

| **Parameter** | **Description** |
| :------------ | :-------------- |
| `baseDir`     | Full path to the base directory of `CRUST` (i.e., `<crustDir>`) |
| `outpuDir`    | Directory where to store the model comparison probability file (default: `<crust>/data/modelComparison`) |
| `k`           | Maximum number of factors that linear models explored by scientistics |
| `sigmas`      | List of data generation error variances |
| `sampleSize`  | Size of the set of stochastic values generated |
| `nIter`       | Number of independent samples on which model comparison statistic is based |

#### Execution

To execute the script:

* Navigate to the `<crustDir>` folder
* Edit the _src/theory/modelComparisonProbabilitiesByMonteCarlo.R_ script file and set the parameters described in the **Configuration**
* Execute: **Rscript src/theory/modelComparisonProbabilitiesByMonteCarlo.R --no-save**

## Agent-Based Simulation Model

The agent-based (ABM) simulation version of the reproducibility model is a forward-in-time simulation-based implementation of a process at the individual level. In our ABM, each scientist is represented as an agent that updates the scientific community consensus. ABM helps us assess interesting properties of our scientific process by allowing the inclusion of replication in the system.

The scripts of the agent-based model are available in the directory _<crustDir>/src/abm_.

### reproducibility.R
The _reproducibility.R_ script is the main script to execute the reproducibility agent-based simulation.

#### Configuration
You can configure the ABM reproducibility model by changing the values of the configuration parameters in the _reproducibility.R_ script file as described below.

| **Parameter**    | **Description** |
| :----------------| :-------------- |
| `baseDir`        | Full path to the base directory of `CRUST` (i.e., `<crustDir>`) |
| `replications`   | Number of replications to run the simulation |
| `timesteps`      | Number of time steps of each simulation run |
| `k`              | Maximum number of factors that linear models explored by scientists |
| `sigma`          | Data generation error variance |
| `sampleSize`     | Size of the set of stochastic values generated under the True Model |
| `trueModel`      | True Model |
| `correlation`    | Correlation of the predictor values |
| `nRey`           | Number of agents of the _Rey_ type |
| `nTess`          | Number of agents of the _Tess_ type |
| `nBo`            | Number of agents of the _Bo_ type |
| `nMave`          | Number of agents of the _Maverick_ type |
| `modelCompare`   | Type of statistic used for model comparison: `TSTATISTICS` _t_ statistics, `RSQ` R-Squared, `ARSQ` Adjusted R-Squared, `AIC` Akaike Information Criterion, `BIC` Bayesian Information Criterion (Schwarz Criterion) |
| `modelSelection` | Type of research strategy: `soft` or `hard`|
| `outputFile`     | Name of the file storing the simulation output data |
| `paramFile`      | Name of the file storing the parameters of the simulation |
| `verbose`        | Indicates if log messages are shown during the simulation execution |
| `ndec`           | Decimal precision of the stored values |

#### Execution

To execute the reproducibility simulation:

* Navigate to the `<crustDir>` folder
* Edit the _src/abm/reproducibility.R_ script file and set the parameters described in the **Configuration**
* Execute: **Rscript src/abm/reproducibility.R --no-save**

### summary.R
The _summary.R_ script is used to summarize data generated by a completely randomized factorial design experiment.

#### Configuration
The values of the configuration parameters of the _summary.R_ script depends on the parameters you used when executing the _reproducibility.R_ script. Additionally, you can define the number of time steps you want to discard as burn-in (i.e., `skip` parameter). The configuration parameters for the _summary.R_ script are shown below.

| **Parameter** | **Description** |
| :------------ | :-------------- |
| `baseDir`      | Full path to the base directory of `CRUST` (i.e., `<crustDir>`) |
| `inpuDir`      | Directory where the raw data is stored (default: `<crust>/data/raw`) |
| `outputDir`    | Directory where to write the summary data (default: `<crust>/data/summary`) |
| `replications` | Number of replications executed at each simulation |
| `timesteps`    | Number of time steps executed at each simulation |
| `k`            | Maximum number of factors that linear models explored by scientistics |
| `m`            | List of model indexes |
| `sigmas`       | List of sigma indexes |
| `types`        | List of combination of scientist types indexes |
| `verbose`       | Indicates if log messages are shown during the simulation execution |

#### Execution

To execute the summary script:

* Navigate to the `<crustDir>` folder
* Edit the _src/abm/summary.R_ script file and set the parameters described in the **Configuration**
* Execute: **Rscript src/abm/summary.R --no-save**

### createABMPlotsSummary10000.R
The _createABMPlotsSummary10000.R_ script calculates statistics and generates plots  from the summary data files created through a completely randomized factorial design experiment using the _summary.R_ script (11,000 timesteps, initial 1,000 timesteps discarded).

#### Configuration
The values of the configuration parameters of the _createABMPlotsSummary10000.R_ script depends on storage location of the summary data generated by the _summary.R_ script for the `AIC` and `SC` model comparison statistic and `hard` and `soft` research strategies, files `summaryAIChard10000.csv`, `summaryAICsoft10000.csv`, `summarySCshard10000.csv` and `summarySCsoft10000.csv`. The configuration parameters are shown below.

| **Parameter** | **Description** |
| :------------ | :-------------- |
| `baseDir`      | Full path to the base directory of `CRUST` (i.e., `<crustDir>`) |
| `inpuDir`      | Directory where the summary data is stored (default: `<crust>/data/summary`) |
| `outputDir`    | Directory where to store the generated plots (default: `<crust>/data/plot`) |

#### Execution

To execute the summary script:

* Navigate to the `<crustDir>` folder
* Edit the _src/abm/createABMPlotsSummary10000.R_ script file and set the parameters described in the **Configuration**
* Execute: **Rscript src/abm/createABMPlotsSummary10000.R --no-save**

### createABMPlotsSummary11000.R
The _createABMPlotsSummary11000.R_ script calculates statistics and generates plots from the summary data files created through a completely randomized factorial design experiment using the _summary.R_ script (11,000 timesteps and no timesteps discarded).

#### Configuration
The values of the configuration parameters of the _createABMPlotsSummary11000.R_ script depends on storage location of the summary data generated by the _summary.R_ script for the `AIC` and `SC` model comparison statistic and `hard` and `soft` research strategies, files `summaryAIChard11000.csv`, `summaryAICsoft11000.csv`, `summarySCshard11000.csv` and `summarySCsoft11000.csv`. The configuration parameters are shown below.

| **Parameter** | **Description** |
| :------------ | :-------------- |
| `baseDir`      | Full path to the base directory of `CRUST` (i.e., `<crustDir>`) |
| `inpuDir`      | Directory where the summary data is stored (default: `<crust>/data/summary`) |
| `outputDir`    | Directory where to store the generated plots (default: `<crust>/data/plot`) |

#### Execution

To execute the summary script:

* Navigate to the `<crustDir>` folder
* Edit the _src/abm/createABMPlotsSummary11000.R_ script file and set the parameters described in the **Configuration**
* Execute: **Rscript src/abm/createABMPlotsSummary11000.R --no-save**

## Auxiliary Functions

The Theoretical Simulation Model and Agent-Based Simulation Model require several auxiliary functions in the _src/functions_ directory.

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
