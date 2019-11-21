This R.project was created as part of the PhD project "Engineering Systems Design in Healthcare" at the Technical University of Denmark (DTU) in collaboration with Rigshospitalet-Glostrup. The project investigates the use of smart mobile and wearable technology for support and monitoring in dementia rehabilitation. 

The overall purpose of this repository is to analyse study data including a pilot followed by series of case studies. The scripts contained here are used to first extract behavioural features from raw data, and then to run analyses on the extracted features. Each of the main scripts used for feature extraction and further analysis are written as R-notebooks with further explanation provided therein.

Note: No study data is stored in this repository to protect the privacy of study participants (neither raw data nor extracted features).

FEATURE EXTRACTION: 
(filename)
purpose: extracts a set of mobility features from location data, and a set of physical activity features from recognised activity data and step count data.
input: raw data (csv format) collected from Android mobile phones and smartwatches
output: sets of behavioural features stored as data frames (Rds files)

ANALYSES:
(filenames)
purpose:
input: behavioural features (Rds format)
output: analysis results including plots and tables provided in notebook

Rscripts folder:
Contains functions and modules called by the main scripts e.g. to extract specific features, restructure raw data.

Drawing board: 
Contains scripts written specifically to create plots, in order to keep main analyses scripts clean and simple.

