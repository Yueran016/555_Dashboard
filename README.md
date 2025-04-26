# Impact of Missing Data on PCA Dashboard

## Overview
This repository contains two components:
- **Interactive Plotting (`interactive_plot.R`)**: Static exploratory plots analyzing the effects of missing data on PCA results.
- **Interactive Dashboard (`shiny.Rmd`)**: A Shiny dashboard allowing users to interactively explore PCA results based on different missing data treatments.

The project demonstrates how different levels of missingness (zero values) can impact principal component analysis outcomes, providing insights for preprocessing strategies.

## Repository Structure
- `data/data17`: Simulated small molecule feature dataset (included for reproducibility)
- `interactive_plot.R`: Static plotting and data exploration script
- `shiny.Rmd`: Shiny dashboard with interactive widgets
- `README.md`: This file

## Project Importance
Visualizing the impact of missing data helps improve the design and analysis of high-dimensional studies by informing better data cleaning and preprocessing strategies.

## Dataset Information
- **Source**: Havard Dataset (https://dataverse.harvard.edu/dataset.xhtml;jsessionid=27101bc602061041cf162b2c3678?persistentId=doi%3A10.7910%2FDVN%2FJDRJGY&version=&q=&fileAccess=&fileTag=&fileSortField=&fileSortOrder=
- **Sample Size**: 480 samples, 150 features

## How to Use
- To explore the static plots, open `interactive_plot.R` and run it.
- To launch the dashboard, open `shiny.Rmd` and run the Shiny app.
