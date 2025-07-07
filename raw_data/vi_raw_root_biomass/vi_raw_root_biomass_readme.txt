This is the readme file for the vi_raw_root_biomass dataset
This files describes the different root biomass measurements from the elevational gradient. See the associated data paper for more details on the methods. 

There are two files (csv), one zipped folder and two folders

PFTC7_SA_raw_root_biomass_2023.csv
This file contains the data collected for correlating Ground Penetrating Radar (GPR) detections with validated root biomass data.

Variables:
site_id is the integer ranging between 1-5 identifying the site
elevation_m_asl is the elevation of the site measured in m above sea level
aspect is the aspect of the site, in this case only west facing
transect is the integer ranging between 1-2 identifying the transect
position_m is the position along the transect of the randomly selected detection measured in m
sample_no is the integer ranging between 1-10 identifying the unique sample per site and transect that was randomly selected
sample_depth_cm is the depth of the belowground detection sample measured in cm
dry_root_mass_g is the dry mass of the roots excavated at the sample measured in g 
stone_mass_g is the mass of the stones excavated at the sample measured in g
dry_soil_mass_g is the dry mass of the soil excavated at the sample measured in g

scan_file_name_changes.csv
This file contains the matched names to harmonise the GPR file names between the plot and transect datasets.

Variables:
original_name is the file name provided by the GPR
new_name is the determined file name to harmonise the naming structure

transects folder
This folder contains the eight raw transect-level datasets, with two transects at each of the four sampled sites.

plots folder
This folder contains the eight raw plot-level datasets, with measurements taken at each of the five plots at each of the four sampled sites.

Variables (these are the same for all of the GPR files within both transects and plots folders):
SCAN FILE is the unique ID for each GPR scan
# is the number of detections
TYPE is whether the measurement was a detections (DETECT) or a position mark (MARKER)
POSITION (m) is the position of the detection or position mark measured in m
DEPTH (m) is the depth of the detection measured in m
AMPLITUDE is the strength of the return signal at the peak of the reflection hyperbola measured in dB
PIXEL COUNT is the overall size of the reflection measured as a count

dzt.zip
This zipped folder contains the raw unprocessed dzt files, the output file of the GPR, within a transects and plots folder.