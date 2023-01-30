# WidraLuduvice_BK

## Overview

This repository contains all code and data associated with the FRBC's Economic Commentary "Boomerang Kids in the Pandemic: How High-Income Families are Their Own Safety Net" by Rachel Widra and André Victor D. Luduvice.

The data and code in this replication package constructs the data cleaning, transformations, and calculations from three data sources (IPUMS BLS CPS, O\*NET, U.S. Census) using R. The script ``boomerang_kids_code.R`` runs the replication package for the main text and the script ``boomerang_kids_appendix.R`` runs the replication for the content provided in the appendix.

## Recommended Citation

Widra, Rachel and Luduvice, André Victor D. . 2021. "Boomerang Kids in the Pandemic: How High-Income Families Are Their Own Safety Net" Economic Commentary, no. 2021-21 (December). https://doi.org/10.26509/frbc-ec-202121.

## Contents

- Charts: 
  - This folder will be automatically generated by the R code and will house the output figures.

- Data:
  - `2018-occupation-code-list-and-crosswalk.xlsx`: U.S. Census 2018 excel spreadsheet with occupation code list and crosswalk
  - `boomerang_kids_data.xml`: IPUMS BLS CPS xml file to be read using the IPUMS R package
  - `cps_00026.cbk`:  Codebook description of variables selected to create the IPUMS BLS CPS extract
  - `onet_work_activities.xlsx`: O\*NET excel spreadsheet with work activities ratings
  - `onet_work_context.xlsx`: O\*NET excel spreadsheet with work context ratings

- Scripts:
  - ``boomerang_kids_code.R``: script to generate Figures 1 to 4 and Tables 1 and 2 of the main text
  - ``boomerang_kids_appendix.R``: script to generate Figures 5 to 8 and Table 3 of the appendix

## Data Availability

All data are publicly available.

- This paper uses data extracted from IPUMS BLS CPS. Data can be accessed from the folder and link provided (below) or directly from cps.ipums.org. The extract can be recreated using the selections described in `cps_00026.cbk`.

  - Any use of these data should be cited as follows: Sarah Flood, Miriam King, Renae Rodgers, Steven Ruggles, J. Robert Warren and Michael Westberry. Integrated Public Use Microdata Series, Current Population Survey: Version 10.0 [dataset]. Minneapolis, MN: IPUMS, 2022. https://doi.org/10.18128/D030.V10.0
Note: the data files provided in the Data folder as boomerang_kids_data.xml, cps_00026.cbk and in the Dropbox folder as cps_00026.dat.gz

  - `cps_00026.dat.gz`: IPUMS BLS CPS zipped file with the extracted data to be read using the xml file. Data can be directly downloaded from the following Dropbox link https://www.dropbox.com/s/il7idpgqack5sck/cps_00026.dat.gz?dl=0.

  - The attached data files are shared with permission, and intended only for replication purposes; please access the data directly via IPUMS for all other applications. Individuals are not to redistribute the data without permission. Contact ipums@umn.edu for redistribution requests.
  
 - This paper uses raw data from the O\*NET Resource Center. The raw data are the mapping between O\*NET-SOC codes and Work Context and Activities ratings. Data can be freely downloaded from https://www.onetcenter.org/dictionary/25.0/excel/work_context.html and https://www.onetcenter.org/dictionary/25.0/excel/work_activities.html. Note: the O*NET Work Activities data is not provided. Save the file in the directory as /Data/onet_work_context.xlsx.

- This paper uses raw data from the U.S. Census Bureau Occupation Code List. The raw data is the 2018 occupation code list and crosswalk. Data can be freely downloaded from https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/2018-occupation-code-list-and-crosswalk.xlsx. Note: the raw data file is provided in the Data folder as `2018-occupation-code-list-and-crosswalk.xlsx`.

## References

Beland, L.-P., Brodeur, A., Mikola, D. and Wright, T. (2022), The short-term economic consequences of COVID-19: Occupation tasks and mental health in Canada. Canadian Journal of Economics/Revue canadienne d'économique, 55: 214-247. https://doi.org/10.1111/caje.12543   
"Work Context - O\*NET 25.0 Data Dictionary" by the National Center for O\*NET Development. Used under the CC BY 4.0 license.   
"Work Activities - O\*NET 25.0 Data Dictionary" by the National Center for O\*NET Development. Used under the CC BY 4.0 license.

## License

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[cc-by]: http://creativecommons.org/licenses/by/4.0/
