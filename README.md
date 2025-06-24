MultiSrcAutEdit
===============

## Multisource Automatic Editing

This repository contains the public version of the R code for multisource automatic editing that was developed under Work Package 1 (WP1) of Eurostat grant SMP-ESS-2023-EBS-IBA. This code implements a five-step procedure for multisource automatic editing as proposed in the Final Report of WP1:

- Step 1. Deductive correction of common variables across data sources.

- Step 2. Specifying reliability weights of common variables.

- Step 3. Automatic editing of common variables across data sources.

- Step 4. Imputing universal values and deriving additional edit rules for common variables.

- Step 5. Automatic editing within individual data sources.

We refer to the Final Report for more details on the methodology.


===============

Some technical points about the code follow.

The code only works if it is called within the R project *agos.Rproj*.

In order to run the code the first time, the following scripts must be executed first (but only once):

1. *dataprep\data_prep_ag.R* – This script imports and stores data from year t and year t-1 in the (personal) input folder.

2. *dataprep\variables_PS.R* – This script loads and stores an overview of SBS variables per questionnaire number.

3. *dataprep\prepare_rules_PS.R* – This script imports edit rules from the SBS production system and translates them into R syntax.

4. *dataprep\generate_rules.R* – This script derives edit rules across data sources and stores them in (local) folder *edited_input*.

After that, multisource automatic editing can be executed by running the following scripts in order:
- *dataprep_beforehand.R* – This script carries out Steps 1 and 2 of the five-step procedure. It also creates files with reliability weights (as input for Step 3) and stratum means (as input for imputation in Steps 4 and 5) in the (personal) output folder.
- *orchestratie.r* - This script carries out Steps 3, 4 and 5 of the five-step procedure. Step 5 is currently implemented only for SBS.

In *global.r*, choose option "use_calculator <- TRUE" if the code is run on a multi-user cluster, to avoid disturbing other users by using up all the available cores for parallel processing. Otherwise, if no other users are working on the same cluster, choose "use_calculator <- FALSE".

Set desired constants (e.g., maximum relative and absolute margin between common variables) in advance in *input_files\constants.R*. Optionally, a predefined scenario can be loaded. This is done by deleting script *input_files\constants.R*, copying one of the scripts from folder *scenarios* into folder *input_files*, and renaming it to *constants.R*.

For the evaluation of results, the scripts in folder *evaluate* can be used. First, run *prepare_evaluation.R* (once) and then run *run_evaluation.R*.


===============

Note: The microdata used in the grant are sensitive and therefore must remain within the secure environment of Statistics Netherlands. In addition, six input files of rules that may contain sensitive information were suppressed from this public repository. For clarity, references to the names of these files remain in the code.

