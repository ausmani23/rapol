# The Racial Politics of the Punitive Turn

This repository contains replication code and materials for 'The Racial Politics of the Punitive Turn' 

## Folder and File Structure

+ 'code/01_public opinion' contains the code to generate the figures that correspond to Part 4.1 of the paper, 'Public Opinion'.

    + 01_group.R - Combines constituent polling datasets
    + 02_prep.R - Recodes combined dataset
    + 03_summarize.R - Summarizes average responses by race and average black-white differences 
    + 04_summarize_output.R - Illustrates the output of 10_summarize.R *[fig_po_averages.png, fig_po_diffs.png]*
    + 05_regmods.R - Estimates generalized linear mixed-effects models on the probability of a positive response to questions in each of the three dimensions
    + 06_regmods_output.R - Illustrates the marginal (adjusted) effect of race on p(anxious), p(mistrustful), p(punitive) *[fig_po_effectofrace.png]*
    + 07_predict.R - Combines estimates from the multilevel models with Census population data to generate predictions for each demographic cell between 1956 and 2014 
    + 08_predict_output.R - Summarizes race-specific trends over time (with and without confidence intervals) and the black-white gap (with confidence interval). *[fig_po_trends.png, fig_po_trends_blackwhitegap.pg, fig_po_trends_byrace.png]*
    + functions.R - Contains misc functions used in the scripts
    + getcode.R - Crosswalk function
    + getinfo2.R - Wrapper to getcode for this data
    
+ 'code/02_voting' contains the code to generate the figures that correspond to Part 4.2 of the paper, 'Voting Patterns in the House of Representatives'.

    + 01_getvotes.R - Loads a dataset of 45 hand-coded punitive votes in the House, merges this with the universe of house votes since 1945, and uses the issue codes of the hand-coded votes to identify a sample of 'punitive' votes
    + 02_matcheos.R - Loads a dataset with information about all congressmen, merges this with a dataset on members of the Congressional Black Caucus
    + 03_merge.R - Merges the votes, members, and a members-votes dataset together
    + 04_predict.R - Trains a model using the average DW-Nominate scores of those voting 'yes' and 'no' in the hand-coded bills to predict the direction of a punitive vote in the not-hand-coded bills
    + 05_summarize.R - Summarizes levels and differences in punitive voting in the house *[fig_voting_levels.png, fig_voting_differences.png]*
    
+ 'code/03_dind' contains the code to generate the figures and results which correspond to Part 4.3 of the paper, 'Black Political Representation'

    + 01_dindmods.R - Estimates the effect of federally-mandated redistricting on incarceration and police rates. Runs preferred specifications and a variety of alternative specifications.
    + 02_regmods.R - Estimates the effect of black political representation on incarceration and police rates via a variety of autoregressive distributed lag (ADL) models. 
    + 03_regmods_robustness.R - Estimates a variety of alternative specifications of the ADL models.
    + 04_summarize.R - Summarizes estimates from preferred models, from both approaches. Illustrates the robustness (or lack of robustness) of the main conclusions to alternative specifications. *[fig_dind_prefests.png, fig_dind_robests.png]*
    
+ 'meta' contains files that carry metadata which is more convenient to store in spreadsheet form than in an .R script

## Notes

+ To replicate results, you will need to create three additional folders

   + 'data', which should contain the original inputs to these files. We don't include this folder or these data in the repository, but you will need them to replicate our results. Please contact us for more information. 
   
   + 'files', which contains intermediate inputs. The output of .R files in this repository which are then used as input by other .R files in this repository. 
   
   + 'output', which contains ultimate outputs. You will need to either modify the working directories in the .R files to store these outputs somewhere else, or create this folder. 


