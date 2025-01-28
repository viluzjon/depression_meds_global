# Mental Health Analysis Project

This repository contains an analysis of global mental health data, focusing on depression prevalence, medication usage, and the perceived helpfulness of medication across different countries. The analysis includes data visualization and statistical summaries.

## Data Sources

1. **Primary Dataset:** [Wellcome Global Monitor Mental Health Report 2020](https://wellcome.org/reports/wellcome-global-monitor-mental-health/2020)  
2. **Secondary/Validation Dataset:** [OECD Pharmaceutical Sales Data](https://data-explorer.oecd.org/vis?df[ds]=DisseminateFinalDMZ&df[id]=HEALTH_PHMC%40DF_KEY_INDIC&df[ag]=OECD.ELS.HD&dq=..DDD_10P3HB..N06A&pd=2010%2C&to[TIME_PERIOD]=false)

## Files

- `mental_health_analysis.R`: The main R script for cleaning, analyzing, and visualizing the data.
- `wgm_csv.csv`: Primary dataset containing mental health survey data.
- `2023valid.csv`: Secondary dataset containing pharmaceutical sales data for validation purposes.
- `README.md`: This file, providing an overview of the project.
- `.gitignore`: Specifies files and directories to be ignored by Git.
- `LICENSE`: License for the repository.

## Analysis Steps

1. **Data Cleaning:**
   - Renamed columns for better readability.
   - Converted relevant variables into factors with descriptive labels.
   - Filtered out incomplete cases and irrelevant responses.

2. **Depression Prevalence Analysis:**
   - Created frequency tables and calculated percentages of responses (e.g., "Yes," "No").
   - Visualized data with bar charts showing the distribution of responses by country.

3. **Medication Usage Analysis:**
   - Analyzed responses to medication usage for depression.
   - Calculated and visualized the percentage of respondents taking medication by country.

4. **Medication Helpfulness Analysis:**
   - Filtered data to exclude "Don't know" responses.
   - Calculated and visualized the perceived helpfulness of medication across countries.

5. **Integrated Analysis:**
   - Merged datasets to compare depression prevalence, medication usage, and helpfulness.
   - Created a consolidated bar chart for all metrics by country.

6. **Validation:**
   - Compared and correlated self-reported medication usage with pharmaceutical sales data from OECD.
   
7. **Estimation**
   - Calculated estimated depression prevalence in countries that weren't included in the dataset and were experiencing armed conflict/occupation in 2020.

8. **Clustering**
   - Created regional clusters.
   - Added depression estimations of missing countries to clusters.
   
## Visualization

The analysis includes multiple visualizations created using `ggplot2`, such as:
- Bar charts for depression prevalence.
- Stacked bar charts for medication helpfulness.
- Consolidated charts comparing all metrics by country.

## How to Run the Code

1. Install the required R libraries:
   ```r
   install.packages(c("tidyverse", "dplyr", "ggplot2", "pastecs"))
   ```
2. Update the file paths in the script to point to the correct locations of the datasets.
3. Run the script `mental_health_analysis.R` in RStudio or any R environment.

## License

This project is licensed under the MIT License. See the LICENSE file for details.

## Contact

For questions or feedback, please create an issue in this repository.
```
