This is the readme file for the v_raw_root_traits dataset.
This file describes the different below- and above-ground trait data files from the elevational gradient. See the associated data paper for more details on the methods.

There are four different files and one zipped folder containing root scans.
The root scan traits were measured using RhizoVision Explorer (v 2.0.3) and represent the raw output from the application. 

PFTC7_SA_raw_root_trait_field_data_2023.csv
This file contains the raw below- and above-ground trait data from the elevational gradient.

Variables:
FileName is a unique ID for each sample
date is the day of the data collection
elevation is the elevation of the site measured in m above sea level
siteID is the integer ranging between 1-5 identifying the site
aspect is the aspect of the site, in this case only west facing
project is the project (or dataset) for which the data was collect, here R = roots
sp is the abbreviated species name (EC = Eragrostis capensis, TT = Themeda triandra, HF = Harpochloa falx, HPA = Helichrysum pilosellum, SG = Senecio glaberrimus) 
plantID is an integer for each unique plant collected. This number repeats for each species, plot and site
leafID is the reproductive height measured in cm
height is the vegetative height measured in cm
root_depth is the maximum rooting depth in cm
grazing is whether there was evidence on the leaves of grazing (Y = confirmed grazing, N = no clear evidence of grazing, blank = not completed)
fire is whether there was evidence on the plant of fire, such as charcoal on old stems (Y = confirmed fire, blank = no clear evidence of fire)
resprout is whether there was evidence of the plant resprouting after disturbance (Y = confirmed resprouting, N = no clear evidence of resprouting, blank = not completed)
no_root_scan is the number of fine roots processed (i.e. scanned, weighed) per unique sample
root_wet_mass_g is the wet mass of the fine roots measured in g
no_tuber_scan is the number of tubers processed per unique sample
tuber_wet_mass_g is the wet mass of the tubers measured in g
no_leaf is the number of leaves processed (i.e. scanned, weighed) per unique sample
leaf_wet_mass_g is the wet mass of a leaf measured in g
leaf_thick_1_mm is the first leaf thickness measure in mm
leaf_thick_2_mm is the second leaf thickness measure in mm
leaf_thick_3_mm is the third leaf thickness measure in mm
root_dry_mass is the dry mass of the fine roots measured in g
tuber_dry_mass is the dry mass of the tubers measured in g
BGB is the dry mass of the full belowground portion of the plant measured in g
leaf_dry_mass is the dry mass of the leaf measured in g
AGB is the dry mass of the full aboveground portion of the plant measured in g
leaf_area is the area calculated of the leaf/leaves using ImageJ measured in cm2 after the leaf was scanned. The scans can be found using their unique ID (FileName) in the iv_raw_aboveground_traits/iv_PFTC7_raw_scans/all_scans_raw.zip folder.

PFTC7_SA_raw_scans_2023.csv
This file contains the measured root traits using RhizoVision Explorer (v 2.0.3) of the unedited root scans. Scans were only masked to remote the scan metadata.

Variables:
File.Name is a unique ID for each sample
Region.of.Interest is the chosen area of the scan to measure (full = full image)
Number.of.Root.Tips is the number of terminal points for all roots on a scan
Number.of.Branch.Points is the number of branches for all roots on a scan
Total.Root.Length.mm is the summed root length across diameter range classes measured in mm
Branching.Frequency.per.mm is the number of branches divided by the total root length measured in branches per mm
Network.Area.mm2 is the total number of pixels in the segmented scan measured in mm2
Average.Diameter.mm is the mean diameter of all roots measured in mm
Median.Diameter.mm is the median diameter of all roots measured in mm
Maximum.Diameter.mm is the maximum diameter of all roots measured in mm
Perimeter.mm is the outer contour of all roots measured in mm
Volume.mm3 is calculated as the sum of values from each root pixel by multiplying the cross-sectional area (calculated using the pixel-level root radii) by the length of each root pixel, measured in mm3
Surface.Area.mm2 is calculated as the length of the pixel multiplied by the circumference of the cross-section of the pixel-level root and summed across all root pixels, measured in mm2
Computation.Time.s is the time taken to analyse a scan in seconds
Root.Length.Diameter.Range.1.mm (1-9) is the summed root length for all pixels within the binned diameter range (1-9 mm) measured in mm
Projected.Area.Diameter.Range.1.mm2 (1-9) is the summed projected area on the surface of the image plane within the binned diameter range (1-9 mm) measured in mm2
Surface.Area.Diameter.Range.1.mm2 (1-9) is the summed surface area (as calculated above: Surface.Area.mm2) within the binned diameter range (1-9 mm) measured in mm2
Volume.Diameter.Range.1.mm2 (1-9) is the summed volume (as calculated above: Volume.mm3) within the binned diameter range (1-9 mm) measured in mm3

PFTC7_SA_raw_gimp_scans_2023.csv
This file contains the measured root traits, as in PFTC7_SA_raw_scans_2023.csv, of the manually processed scans using GIMP to remove root shadows. 

PFTC7_SA_raw_rootpainter_scans_2023.csv
This file contains the measured root traits, as in PFTC7_SA_raw_scans_2023.csv, of the computationally edited scans using RootPainter to remove root shadows. 

PFTC7_SA_scans_2023.zip
This zipped folder contains the png files of the roots that were processed using different methods. 
- /raw_scans contains the unedited png files of the root scans
- /raw_scans_masked contains the png files of the root scans which have only had the metadata masked out of the image
- /gimp_scans contains the png files of the root scans that were manually processed to remove root shadows
- /rootpainter_scans contains the png files of the root scans that were computationally processed to remove root shadows