# PeakAUC

An R Shiny application for the analysis of time series data, designed for peak selection and AUC calculations. 

A link to use this application can be found here: [PeakAUC](http://gregp89.shinyapps.io/nox_-_app_for_publication)

Instructions for use for generating a standard curve: 

1) Under the standards tab, upload any .csv file containing time-series data
2) Select columns (x should always be time, y should be your collected values). If an error is displayed before columns are selected, this can be ignored
3) Enter an amount (any desired units) for each standard as you highlight peaks. Highlighted peaks in the displayed graph. 
4) Click "Save" to store each peak to a dataframe
5) After all desired peaks are saved, click calculate to generate AUCs for each peak
6) Click Export to recieve .csv file containing standards and corresponding AUCs

Instructions for use for using standard curve to estimate sample amounts

1) Under the sample tab .csv file containing time-series data (under "Upload Sample File")
2) Select columns (x should always be time, y should be your collected values). If an error is displayed before columns are selected, this can be ignored
3) Under the sample tab, upload the .csv generated from the standard tab (under "Upload Standard Curve")
4) Choose a desired length for the baseline calculation (ideal length will vary based on inidividual experiments)
5) Enter sample names as you highlight peaks. Highlighted peaks in the displayed graph.
6) Click "Save" to store each peak to a dataframe 
7) After all desired peaks are saved, click calculate to generate both AUCs and estimated sample amounts (from the standard curve)
8) Click Export to recieve .csv file containing AUCs and corresponding estimated sample amounts
