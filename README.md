# DIVA_paper_subsidence

This repository contains the data and the scripts used to create results, figures and tables in the paper "A global analysis of subsidence, relative sea-level change and coastal flood exposure" by R. Nicholls et al. published in Nature Climate Change (DOI-link to follow).

The Repository is organized as follows:

- subdirectory input: the external input data
  * subdirectory gis: a shapefile of the global coastline, divided into the DIVA-segments
  * admin_input_regions_gva.csv: the table that connect coastline segments with GVA regions
  * city_subsidence_controlled_low.csv: the table with the low estimates of controlled city subsidence
  * city_subsidence_uncontrolled_low.csv: the table with the low estimates of uncontrolled city subsidence   
  * city_subsidence_controlled_high.csv: the table with the high estimates of controlled city subsidence
  * city_subsidence_uncontrolled_high.csv: the table with the high estimates of uncontrolled city subsidence
  * cls_input_33.csv: the DIVA-database for the coastline segments as a table. Population exposure and coastal length is based on this data.
  * cls_output.csv: year 2015 output of a DIVA-simulation - used for obtaining 2015 numbers for population and area exposure 
  * cls_input_uplift_ICE-6G.csv: coastline segment level input data of glacial isostatic adjustment (based on https://www.psmsl.org/train_and_info/geo_signals/gia/peltier/)
  * delta_input_1mm.csv: the input table for the 117 deltas used in the DIVA database
  * satelitte_SLRrates1p0deg.csv: the recent satelitte measured SLR data on coastline segment level 
  
- subdirectory results: the computed results
  * cls_slrrates_2015.csv: the main results used in the paper - SLR rates from different components on coastline segment level  
  * global_output.csv: a global output file of the DIVA runs perfomed for Figures 3 and S3
  * label_data.R: a script that was used to label the global output data
  
- subdirectory tables: the scripts for producing Table 1 in the paper: 
  * table1.R: a script used to produce the data shown in Table 1 in the paper
  * table1.csv: the results shown in Table 1 in the paper
  
- subdirectory figures: the scripts to produce the figures - the resulting figures are stores in the subdirectories
  * eps: a subdirectory containing the figures as eps
  * jpg: a subdirectory containing the figures as jpg
  * source_data: a subdirectory containing the source data of each figures as csv
  * Cumulative_slr_different_components_panel.R: the script producing Figure S1
  * Cumulative_slr_with_table.R: the script producing Figure 1 in different version (with table 1 in the figure and table 1 seperated from the figure)  
  * Exposure.R: the script producing Figure 3 and Figure S3
  * layout.r: common style file for all figures
  * subsidence_map_GVA.R: the script producing the map in Figure 2
  * subsidence_scatterplot.R: the script producing the map in Figure 2

- create_2015_slrates.R: the main script producing the main results of the paper - assembles ./results/cls_slrrates_2015.csv
