#Impact of study
Instrument detection limits introduce varying degrees of left-censored zeros that 
can distort data distributions and undermine the effectiveness of batch effect correction. 
This dashboard empowers researchers to interactively assess how different zero-inflation 
thresholds impact batch correction outcomes, enabling more robust preprocessing choices 
and more reliable downstream analyses.

# How to Run the Dashboard

1. Install the required packages:

```r
install.packages(c("shiny", "shinydashboard", "plotly", "dplyr", "ggplot2", "RColorBrewer", "missMDA"))
```
2. Run the first section of app.R to load data and prepare interactive objects.

3. Then run the rest of app.R to launch the dashboard:
```r
shiny::runApp("app.R")
```
