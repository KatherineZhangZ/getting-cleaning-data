# Getting-cleaning-data Course Project
   This repo contains the R code and otuput for the course work for getdata-015
   * README.md       : Introdution of the repo 
   * R-code          : run_analysis.R
   * Tidy data output: tidy_data.txt
   * CookBook.MD     : Variable list
   
## Input Data 
Here are the data for the project: 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

## Assignment 
You should create one R script called run_analysis.R that does the following.
* Merges the training and the test sets to create one data set.
* Extracts only the measurements on the mean and standard deviation for each measurement. 
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive variable names. 
* From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Steps to recreate tidy data output 
* Unzip the downloaded data under R working directory ./UCI HAR Dataset
* Set the new working directory as ./UCI HAR Dataset using setwd()
* Run run_analysis.R, which outputs the tidy_data.txt file under ./UCI HAR Dataset



 

   
   
