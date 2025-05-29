# Competitive Interaction Analysis in Marine Species in an aquarium setting(R)

This project analyses competitive interactions among marine species using data collected during feeding events. 
The analysis focuses on interaction types, feeding methods, and the frequency of interactions, using statistical models and visualizations in R.

## Features

- Counts and summaries of interaction types by feeding method
- Boxplots comparing interaction frequency across categories
- Mixed-effects models using `lme4` to test fixed and random effects
- Cumulative frequency plots for victim and competitor species
- Frequency of interactions per species per day

## Technologies Used

- **R** (language)
- R packages:
  - 'dplyr'
  - 'lme4'
  - 'ggplot2' *(optional if you use additional plots)*

## Files
- 'Sharks.R': R script
- 'Data': Folder where data files should be placed
- 'Total': Data file
- 'ModelData': Data file
- 'Data': Data file
- 'README.md': Project description

## How to Run

1. Open 'Sharks.R' in RStudio.
2. Make sure your data (e.g. 'Total.csv', 'Data.csv', etc.) is available in a folder named 'Data'.
3. Install required packages:
   install.packages("dplyr")
   install.packages("lme4")

## Notes
Interaction types include Contest and Scramble.
Feeding methods include Target and Scatter.
Mixed models account for variation across species, dates, and surveys.

## Project
This project contains the R code used in my master's dissertation, which investigated feeding competition in a controlled aquarium environment containing multiple marine species.
The aim of the research was to quantify and analyze competitive interactions between species during feeding events, and to assess how interaction type and feeding method influenced competition intensity.
