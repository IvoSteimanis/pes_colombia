## This repository contains the data and code that replicates tables and figures for the following paper:
__Title:__ No crowding out among those terminated from an ongoing PES program in Colombia <br>
__Authors:__ Esther Blanco<sup>a,b</sup>, Lina Moros<sup>c</sup>, Alexander Pfaff<sup>d</sup>, Ivo Steimanis<sup>e</sup>, Maria Alejandra Velez<sup>f</sup>, Björn Vollan<sup>e</sup> <br>
__Affiliations:__ <sup>a</sup> Faculty of Economics and Statistics, University of Innsbruck, Austria; <sup>b</sup> The Ostrom Workshop, Indiana University, USA; <sup>c</sup> Faculty of Business Administration, University of Los Andes, Colombia; <sup>d</sup> Sanford School of Public Policy, Duke University, USA; <sup>e</sup> School of Business and Economics, University of Marburg, Germany; <sup>f</sup> Faculty of Economics, University of Los Andes, Colombia  <br>
__JEL Codes:__ H0, Q28, D91  <br>
__Keywords:__ payments for ecosystem services, crowding, motivations, conservation, Colombia  <br>

### License
The data and code are licensed under a Creative Commons Attribution 4.0 International Public License. See LICENSE.txt for details.
### Software requirements
All analysis were done in Stata version 16:
‒	Add-on packages are included in Programs and do not need to be installed by user. The names, installation sources, and installation dates of these packages are available in Programs/stata.trk.
### Instructions
1.	Save the folder ‘replication_package’ to your local drive.
2.	Open the master script ‘run.do’ and change the global pointing to the working direction (line 20) to the location where you save the folder on your local drive.
3.	Run the master script ‘run.do’ to replicate the analysis and generate all tables and figures reported in the paper and supplementary online materials.
### Dataset
‒	The anonymized survey data are stored in data/ psa_data.xlsx
### Descriptions of scripts
__scripts/01_clean_generate.do__ <br>
‒	This script processes the raw survey data from and prepares them for analysis.
__scripts/02_analysis.do:__ <br>
‒	This script creates figures and tables shown in the paper (online appendix) and saves them to results/Figures and results/Tables

