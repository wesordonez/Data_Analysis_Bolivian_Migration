# Google Data Analytics Capstone Project

## Overview
This project is the culmination of a rigorous journey through Google’s Data Analytics Professional Certificate Program. It represents the final capstone project, allowing the application of a wide array of skills and knowledge acquired throughout the course. Unlike other projects, this capstone is self-directed, navigating the entire data analytics process, from data collection to visualization and insights generation.

## Executive Summary
This research delves into the displacement and migration patterns observed within Bolivia, focusing on the influx into the department of Santa Cruz. The rapid growth in Santa Cruz has significant implications for resource management, sociopolitical stability, and cultural preservation.

The primary objective is to chart the migratory paths of individuals, identify their residences from five years prior, and understand the motivations behind their relocation to Santa Cruz. The study leverages data from the 2021 Survey for Homes of Bolivia to provide a comprehensive snapshot of Bolivian migratory habits. 

To offer a holistic understanding of growth dynamics, various socioeconomic indicators like income levels, gender, civil status, and labor statistics were also explored.

Key stakeholders for this research include city officials, urban planners, political leaders, economists, business leaders, and cultural advocates. The findings offer a roadmap for addressing growth challenges in Santa Cruz, supporting a sustainable and inclusive future.

## Preparation

### Data Sourcing
The primary data source is the "Survey of Homes 2021," conducted by Bolivia’s National Institute of Statistics (NIS). It includes responses from 12,948 homes, covering topics such as homeownership, labor, education, and migration. The data was accessed via the Archivo Nacional de Datos (ANDA) Catalogue, a credible repository backed by the NIS.

### Formatting
The original data was provided in SPSS format and was converted into a CSV file for easier manipulation. The final dataset contains 357 variables across 42,090 rows, encompassing a broad range of topics relevant to the research.

### Processing
Data processing involved the following steps:
- **Variable Reduction:** The variables were reduced from 357 to 18, focusing on factors such as sex, education, labor, income, and migration history.
- **Data Conversion:** Numerical data was converted into readable strings using the dataset's dictionary/key information.
- **Translation & Renaming:** Key variable names and a few text/string rows were translated and renamed for better readability.

## Libraries and Tools Used
The following R libraries were used in the data processing and analysis phases:
- **rmarkdown:** For dynamic report generation.
- **haven:** For importing/exporting SPSS, Stata, and SAS files.
- **skimr:** For summary statistics.
- **janitor:** For simple data cleaning.
- **tidyr:** For tidying data.
- **dplyr:** For data manipulation.
- **ggplot2:** For creating plots.
- **stringr:** For string manipulation.
- **kable:** For table formatting.

These tools were essential in preparing and refining the data for the final analysis.
