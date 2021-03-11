# RHD Investment Case Modelling

This repository contains code used for analysis in Modelling a case for investment in the prevention and management of rheumatic heart disease in the African Union 2021-2030 

The repository is currently in progress as code becomes sufficiently documented. Full code expected to be pushed to repository by Feb 20.

Note: Each piece of code typically has directory filepaths that will need to be filled shown by [Insert own directory]. Several pieces of code are designed to run both on 
a cluster computing environment and locally (though with fewer draws of uncertainty if locally). There will be elements of the analysis that cannot be reproduced locally as they 
require excessive amounts of RAM.

Description of directories:

## 1. data -- contains inputs used to run model
    a. gbd_inputs -- contains extracted, formatted, reduced-size files with GBD-related inputs
    b. id_maps -- contains files that link IDs to names of GBD variables, allowing the files in the gbd_inputs folder to be saved as smaller files
    c. other_inputs -- contains input data from e.g. World Bank GDP/GNI numbers, IMF WEO estimates, data extracted directly from studies, and intermediate data files that were somewhat processed (e.g. derived from GBD and other estimates, UN WPP estimates that were processed and formatted)
  
  Additionally, 3 files--a spreadsheet of AU member states with regions, the input parameters regarding intervention effect sizes and target coverage assumed in the AU as a
  whole, and the run key that specifies all of the parameters that varied in the different scenarios run

## 2. functions -- contains functions used by multiple pieces of code in analysis (one to submit jobs to computing cluster, one that maps GBD IDs to names, one that swaps names
of locations that vary by input source)

## 3. model_code -- contains code used to do final prep of inputs and to run model

  a. input_draw_generation -- folder that contains 4 data prep pieces of code to be run *in sequence*
  
    i. 01_est_baseline_coverage.R -- generating draws of baseline coverage
    
    ii. 02_starting_postsurg.R -- generating estimates of people who have already had surgery before projection period
    
    iii. 03_compile_inputs.R -- compiles inputs across various sources, generates draws of epidemiological paramters and effect size paramters
    
    iv. 04_pct_severe_by_year.R -- generates an estimated distribution of the number of years that people have had severe disease (among people with severe disease starting 
    in the first year of projection) -- this allowed the sensitivity analysis on the # of years that HF management impacted mortality
    
  b. multiple pieces of code in the modelling process --
  
    i. 00_run_all.R -- code that runs steps 01-04 of the modelling process
    
    ii. 00b_cluster_run_scen.R -- if running on the cluster, 00_run_all.R will run 00b, which runs 01-04 without submitting 01-04 separately
    
    iii. 01_pharyngitis_arf_estimation_prep.R -- prepare pharyngitis/ARF process
    
    iv. 02_rhd_model_uncer.R -- run health impact model
    
    v. 03_costing.R -- calculate costs
    
    vi. 04_health_impact_monetization.R -- convert health impacts to $, calculate benefit-cost ratios and net benefits
    
    vii. 05a_submit_au_compilation.R -- if running for regions, compile AU results from the regions (this submits code on the cluster to do this)
    
    viii. 05b_compile_au_from_regions.R -- compiles results for  AU from regions
    
    ix. 06_compile_sensitivity_results_table.R -- compile results from running many scenarios
    
    x. costing functions.R -- costing functions used in 03 and 04
    
    xi. functions_separate_causes_uncert.R -- function that does the year-to-year calculations for the health impact model
    
  ## 4. other_code -- contains certian code to create results (for appendix)
  
    a. country_scenario_results.R -- contains code to examine results from the scenarios that were run for particular countries
    
    b. cumulative_ROI.R -- contains code to examine benefit-cost ratio for each year using cumulative cost and benefits (main results calculated for whole time range)
    
    c. model_states_plot.R -- plots results for more model states for appendix
    
    d. UI_draw_tests.R -- examines uncertainty intervals for run with 4000 draws for appendix
  
  ## 5. prep_code -- contains code used to prep inputs from raw data (sometimes to compress to smaller files so that data could be stored in git repository)
  
    a. calc_gni_for_vsl.R -- formatting GNI input
    
    b. dismod_ihdenvelope_mort_params.R -- create death numbers for calibration for main approach (DisMod/CODEm hybrid deaths using a CVD envelope)
    
    c. fiji_mort_parameters.R -- examine Fiji mortality numbers in appendix
    
    d. gbd_longitudinal_hf_inc_derive.R -- derive HF incidence implied by the GBD estimates
    
    e. gni_gdp_projections.R -- create projections of GDP/GNI for costing/health impact monetization
    
    f. pharyngitis_meta_analysis.R -- redo meta-analysis for pharyngitis cases (see appendix)
    
    g. prep_gbd_inputs.R -- take GBD data and format for easier use
    
    h. soweto_hf_inc_parameter.R -- derive HF transition probabilities using alternative death pattern from Heart of Soweto study
    
    i. wpp_mort_fert_projections.R -- project mortality and fertility using WPP estimates
    

    
    
