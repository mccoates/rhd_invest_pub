# RHD Investment Case Modelling

This repository contains code used for analysis in Modelling a case for investment in the prevention and management of rheumatic heart disease in the African Union 2021-2030 

The repository is currently in progress as code becomes sufficiently documented. Full code expected to be pushed to repository by Feb 20.

Note: Each piece of code typically has directory filepaths that will need to be filled shown by [Insert own directory]. Several pieces of code are designed to run both on 
a cluster computing environment and locally (though with fewer draws of uncertainty if locally). There will be elements of the analysis that cannot be reproduced locally as they 
require excessive amounts of RAM.

Description of directories:

1. data -- contains inputs used to run model
  a. gbd_inputs -- contains extracted, formatted, reduced-size files with GBD-related inputs
  b. id_maps -- contains files that link IDs to names of GBD variables, allowing the files in the gbd_inputs folder to be saved as smaller files
  c. other_inputs -- contains input data from e.g. World Bank GDP/GNI numbers, IMF WEO estimates, data extracted directly from studies, and intermediate data files
  that were somewhat processed (e.g. derived from GBD and other estimates, UN WPP estimates that were processed and formatted)
  
  Additionally, 3 files--a spreadsheet of AU member states with regions, the input parameters regardning intervention effect sizes and target coverage assumed in the AU as a
  whole, and the run key that specifies all of the parameters that varied in the different scenarios run

2. functions -- contains functions used by multiple pieces of code in analysis (one to submit jobs to computing cluster, one that maps GBD IDs to names, one that swaps names
of locations that vary by input source

3. model_code -- contains code used to do final prep of inputs and to run model
  a. input_draw_generation -- folder that contains 4 data prep pieces of code to be run in sequence
    i. 01_est_baseline_coverage.R -- generating draws of baseline coverage
    ii. 02_starting_postsurg.R -- generating estimates of people who have already had surgery before projection period
    iii. 03_compile_inputs.R -- compiles inputs across various sources, generates draws of epidemiological paramters and effect size paramters
    iv. 04_pct_severe_by_year.R -- generates an estimated distribution of the number of years that people have had severe disease (among people with severe disease starting 
    in the first year of projection) -- this allowed the sensitivity analysis on the # of years that HF management impacted mortality
  b. multiple pieces of code in the modelling process--
    i.
    ii.
    iii.
