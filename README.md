## get_clean_data
====

#### This repo contains the files for the Coursera - Getting & Cleaning Data project assignment

* README.md   -  This file - Lists contents of this repo
* CodeBook.md - The markdown file that describes the process used by the R script `run_analysis.R`
* run_analysis.R - R script to download/import Human Activity Recognition (HAR) data and calculate the average of variables that related to `std()` and `mean()` for each Activity and Subject number.   

<br>
<br>
* Can execute the code several ways:  
    + source("https://github.com/edlewis4/get_clean_data/run_analysis.R`
    + Download the source to your working directory and Run  `source("./run_analysis.R")`  to execute.  
* If the UCI HAR Dataset data does not exist in the workingdirectory, It will download it from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
* If it downloads the zip file, it will unzip it to `"./UCI HAR Dataset"` directory
* It will then automatically execute the remaining commands to read in the data and write out the final tidy dataset "./samsung_HAR_out.txt"


 



