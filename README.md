## REUPLOAD OF PROJECT

Original repository was owned by my univeristy and could not be forked. I have copied all the files here.
the project was from August 2021.

original repository: [https://github.com/MAST30034-2021-S2/mast30034_2021_s2_project_1-Andre-Medina](https://github.com/MAST30034-2021-S2/mast30034_2021_s2_project_1-Andre-Medina)

# MAST30034 Project 1 - Quantitative Analysis
- Student Name: Andre Medina
- Student ID: 908155
- Due Date: Thursday 18th of August 11:59:00 am (AEST) 2021.

# Dependencies
- Language: Python 3.8.3 and R 4.05
- Packages / Libraries: pandas, pyspark, sklearn, seaborn, bokeh, feather-format4, statsmodels

# Datasets
- NYC TLC trip records: https://www1.nyc.gov/site/tlc/about/tlc-trip-record-data.page
- NYC TLC lookup table: https://www1.nyc.gov/site/tlc/about/tlc-trip-record-data.page
- COVID-19 case counts:  https://data.cityofnewyork.us/Health/COVID-19-Daily-Counts-of-Cases-Hospitalizations-an/rc75-m7u3
- COVID-19 restrictions in NYC: self made dataset, contained in repository (in both raw_data and research named 'covid_restrictions.csv')

# Directory
- `raw_data`: Some raw data, the
- `preprocessed_data`: Bin and counted data
- `tallied_data`: fully processed data ready for analysis, visualisation and statistical modelling
- `research`: Reseach conducted, mainly COVID-19 restriction dataset creation excel file along side final CSV file
- `plots`: Plots generated for the final report
- `code`: All code used to complete analysis, to be ran in the following order (python to r)
    - `notebooks` Python programs
        - Download for "downloading all data"
        - general processing for "Preprocessing"
        - stage 1 tallying for "Tallying data for stage 1 analysis"
        - stage 1 visualizing for "Anlysing and Visualizeing tallied stage 1 data"
        - stage 2 tallying for "Tallying data for stage 2 analysis"
        - stage 2 visualizing for "Anlysing and Visualizing tallied stage 2 data"
        - stage 3 tallying for "Tallying data for stage 3 analysis"
    - `r` R programs
        - stage 3 modelling for "Inital modelling and analysis of tallied stage 3 data"
        - stage 4 modelling for "Final modelling of stage 3 data"        
- `deprecated`: deprecated code
- `test_download`: file used for testing download notebook

# Other
- The code is created to run the notebooks and R programs in the above listed order. They should all be able to be run independently as all but the proccessed taxi datasets have been uploaded. 
- This being said, as there are no taxi data sets so the "general processing" notebook wont run until the data is downloaded
- The "general processing" file took my computer a few days to run, there is a testing variable in the first code box that will only analyse a subset of the data. (also need to set only process_grn and process_fhv to True while process_ywl and process_hvf to False for testing as green and fvh are the only predownloaded datasets. all four of these variabels are found right next to the testing variabel in the first code block, ive added a comment saying what to change, remember this just tests the code works)
- The "downlaod" file will attempt to download ~40GBs of files, it too has a testing variable to limit this to only a few GBs.
- There were some issues with my multiline comment wordart in R, I got them working but they might look a bit broken in R. They look nice in python



# Reflection for future Andre
## Time spent
I counted the time I spent on this assignment for my sanity
- Preprocessing: 34 hours (Included learning spark and researching)
- Analysing and modelling: 30 hours (Included revising machine learning algos as I did machine learning a year and a bit ago)
- Tidying code and plots: 15 hours (Definetly need a better way to organise everything, my onenote file was a mess)
- Report writing: 13 hours (Spent a lot of time trying to remove words to fit the word count, started at 3k got to the 2k)

Total time spent: 92 hours over 14 days, averaged around 6.5 productive hours per day
## Reflection
Plots needed larger font, should also write report as I go next time. I did start with an abstract in my mind but I should have written it down as I went. Went fairly smoothly, defintly should have researched more models to do with how time modelling works however I am happy with my outcome and this its accurate. The size of the data was fine, however the scope of the project was too large. There were too many factors to talk about in the 2k words for the report. Im not sure what I could have removed, maybe just have skipped some steps in the annalysis stage? maybe have reduced the amount of preprocessing I did? both? the modelling stage was fine in scope I think. Need to learn how to test a variable plus its correlated variables in R. Also should really have learnt how to use olm in python, it wasnt working exactly like lm in R so just switched to R as it was easier and more familiar for linear models. Still happy with how I managed to fit everything into the report and am happy with the outcome. but that doesnt mean I couldnt have done it better/ more efficently. lmao if the marker reads this please leave a note XD

