
# ———— 1. Caculate needed data  p_list, t_list, s_list ————

# 1. required packages
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(missMDA)
library(plotly)
library(shiny)
library(shinydashboard)
library(plotly)

# 2. load data
feature_table <- readRDS("data/data17")
cols_meta     <- 2
df_raw        <- feature_table[, -c(1:cols_meta)]
metadata      <- feature_table[, 1:cols_meta]


set.seed(2025)
metadata$Sample_type <- sample(c("S1","S2","S3","S4"),
                               nrow(metadata),
                               replace = TRUE)

# 3. define a function to create: PCA + ANOVA + 4 ggplot
make_plot_list <- function(df, metadata, color_index) {
  # 3.1 impute
  imp    <- imputePCA(df, ncp = 2)
  mat    <- imp$completeObs
  pca    <- prcomp(mat, scale = TRUE)
  pca_df <- as.data.frame(pca$x)
  pca_df$sum_int <- rowSums(mat)
  pca_df        <- cbind(pca_df, metadata)
  
  # 3.2 ANOVA
  aov_int <- summary(aov(sum_int ~ batch, data = pca_df))[[1]]$`Pr(>F)`[1]
  aov_p1  <- summary(aov(PC1      ~ batch, data = pca_df))[[1]]$`Pr(>F)`[1]
  aov_p2  <- summary(aov(PC2      ~ batch, data = pca_df))[[1]]$`Pr(>F)`[1]
  
  # count missing propotion
  missing_counts <- apply(df_ft, 1, function(x) sum(is.na(x)))
  df_summary <- data.frame(
    sample  = rownames(df_ft),
    missing = missing_counts
  )
  df_summary$group <- cut(
    df_summary$missing,
    breaks = c(-Inf, 9, 24, 49, Inf),
    labels = c("<10", "10–24", "25–49", "≥50")
  )
  
  # 3.3 generate ggplot
  plots <- list()
  
  # (1) Sum Intensity
  plots[[1]] <- ggplot(pca_df, aes(x = seq_len(nrow(pca_df)),
                                   y = sum_int,
                                   fill = factor(batch))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = color_index) +
    labs(x = "Sample #", y = "Sum Intensity") +
    ggtitle(sprintf("Sum Intensity (p = %.4f)", aov_int)) +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
  
  # (2) PC1 vs PC2
  plots[[2]] <- ggplot(pca_df, aes(x = PC1, y = PC2, color = factor(batch))) +
    geom_point(size = 2) +
    scale_color_manual(values = color_index) +
    ggtitle("PC1 vs PC2") +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
  
  # (3) PC1 distribution by batch
  plots[[3]] <- ggplot(pca_df, aes(x = factor(batch), y = PC1, color = factor(batch))) +
    geom_jitter(width = 0.2, height = 0) +
    scale_color_manual(values = color_index) +
    labs(x = "Batch", y = "PC1") +
    ggtitle(sprintf("PC1 by Batch (p = %.4f)", aov_p1)) +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
  
  # (4) PC2 distribution by batch
  plots[[4]] <- ggplot(pca_df, aes(x = factor(batch), y = PC2, color = factor(batch))) +
    geom_jitter(width = 0.2, height = 0) +
    scale_color_manual(values = color_index) +
    labs(x = "Batch", y = "PC2") +
    ggtitle(sprintf("PC2 by Batch (p = %.4f)", aov_p2)) +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
  
  return(plots)
}

# 4. color
batch_num <- length(unique(metadata$batch))
if (batch_num <= 8) {
  color_index <- brewer.pal(8, "Dark2")[1:batch_num]
} else {
  color_index <- rep(brewer.pal(8, "Dark2"), length.out = batch_num)
}

# 5. generate 0% zero / 10% zero / 25% zero 
# 5.1 0% zero
p_list <- make_plot_list(df_raw, metadata, color_index)

# 5.2 10% zero
th10   <- apply(df_raw, 2, function(x) quantile(x, 0.10, na.rm = TRUE))
df_10  <- as.data.frame(lapply(seq_along(df_raw), function(i)
  ifelse(df_raw[[i]] < th10[i], 0, df_raw[[i]])))
colnames(df_10) <- colnames(df_raw)
t_list <- make_plot_list(df_10, metadata, color_index)

# 5.3 25% zero
th25   <- apply(df_raw, 2, function(x) quantile(x, 0.25, na.rm = TRUE))
df_25  <- as.data.frame(lapply(seq_along(df_raw), function(i)
  ifelse(df_raw[[i]] < th25[i], 0, df_raw[[i]])))
colnames(df_25) <- colnames(df_raw)
s_list <- make_plot_list(df_25, metadata, color_index)

# 6. transform to Plotly 
p_list_interactive <- lapply(p_list, ggplotly)
t_list_interactive <- lapply(t_list, ggplotly)
s_list_interactive <- lapply(s_list, ggplotly)

# ———— 2. UI  ————
#
ui <- dashboardPage(
  dashboardHeader(
    title = "Yueran‘s Dashboard",
    tags$li(
      a(
        href = "https://github.com/Yueran016/555_Dashboard",
        icon("github"), "GitHub Link",
        title = "View on GitHub",
        target = "_blank"
      ),
      class = "dropdown"
    )
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Batch Correction Plots", tabName = "plots1", icon = icon("chart-bar")),
      menuItem("Missing-Data Plots",    tabName = "plots2", icon = icon("table")),
      menuItem("Data Source",           tabName = "datasource", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      ## — Tab 1:
      tabItem(tabName = "plots1",
              h2("Batch Effect Plots"),
              p("This presents three different data processing methods: Original Data, 10% Zero Data, and 25% Zero Data. Each data group is displayed through four types of plots: Sum Intensity, PC1 vs PC2, PC1 Distribution, and PC2 Distribution. These plots help the reader understand the impact of different missing data treatments on PCA results and explore how missing data influences the overall data batch effect."),
              fluidRow(
                box(width = 4,
                    selectInput("data_choice1", "Select data group:",
                                choices = c("Original Data" = "p",
                                            "10% Zero Data"   = "t",
                                            "25% Zero Data"   = "s")),
                    actionButton("plot1_1", "Sum Intensity"),
                    actionButton("plot1_2", "PC1 vs PC2"),
                    actionButton("plot1_3", "PC1 Distribution"),
                    actionButton("plot1_4", "PC2 Distribution")
                ),
                box(width = 8,
                    plotlyOutput("selected_plot1", height = "600px")
                )
              )
      ),
      
      ## — Tab 2: Missing-Data
      tabItem(tabName = "plots2",
              h2("Missing Values Distribution"),
              p("This page summarizes the number of missing values per sample, categorized into four groups: <10, 10–24, 25–49, and ≥50. Switch between counts or percentages; hover to see exact values, click a bar to highlight a group."),
              fluidRow(
                box(width = 4,
                    radioButtons("view_mode", "View as:", choices = c("Count", "Percent"), inline = TRUE),
                    hr(),
                    tags$ul(
                      tags$li("✔️ Hover: show details"),
                      tags$li("✔️ Click: highlight selected group")
                    )
                ),
                box(width = 8,
                    plotlyOutput("missing_bar", height = "400px"),
                    verbatimTextOutput("click_info")
                )
              )
      ),
      
      ## — Tab 3: Data Source
      tabItem(tabName = "datasource",
              h2("Data Source"),
              p("• Source: ", tags$a(href="https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/JDRJGY", 
                                     "Harvard Dataverse GC/MS Simulated Dataset", target="_blank")),
              p("• Sample size: 480 samples × 150 features."),
              p("• Collection method: Simulated GC/MS data using R."),
              p("• Population: Fully simulated dataset."),
              tags$ul(
                tags$li("data/data17.rds —— original feature_table"),
                tags$li("data/df_10.rds   —— 10% Zero Data"),
                tags$li("data/df_25.rds   —— 25% Zero Data")
              )
      )
    ),
    
    ## — Footer: Real-world impact (applies to all tabs) —
    fluidRow(
      column(width = 12,
             br(),
             p("Instrument detection limits introduce varying degrees of left-censored zeros that can distort data distributions and undermine the effectiveness of batch effect correction. This dashboard empowers researchers to interactively assess how different zero-inflation thresholds impact batch correction outcomes, enabling more robust preprocessing choices and more reliable downstream analyses.",
               style = "text-align:center; font-style:italic; color:gray; font-size:90%;")
      )
    )
  )
)



#
# ———— 3. Server  ————
#
server <- function(input, output, session) {
  
  # logic of first one 
  current1 <- reactiveVal(1)
  observeEvent(input$plot1_1, { current1(1) })
  observeEvent(input$plot1_2, { current1(2) })
  observeEvent(input$plot1_3, { current1(3) })
  observeEvent(input$plot1_4, { current1(4) })
  
  selected_data1 <- reactive({
    switch(input$data_choice1,
           p = p_list,
           t = t_list,
           s = s_list)
  })
  
  output$selected_plot1 <- renderPlotly({
    plotly::ggplotly(selected_data1()[[ current1() ]])
  })
  
  # second page
  get_plot_data <- function(view){
    tab <- as.data.frame(table(df_summary$group))
    names(tab) <- c("group", "count")
    if(view == "Percent"){
      tab$y <- tab$count / sum(tab$count) * 100
      tab$hover_text <- sprintf("%s: %d (%.1f%%)", tab$group, tab$count, tab$y)
    } else {
      tab$y <- tab$count
      tab$hover_text <- sprintf("%s: %d", tab$group, tab$count)
    }
    tab
  }
  
  output$missing_bar <- renderPlotly({
    df <- get_plot_data(input$view_mode)
    plot_ly(
      df,
      x    = ~group,
      y    = ~y,
      type = "bar",
      text = ~hover_text,
      hoverinfo = "text",
      source    = "bar_source"
    ) %>%
      layout(
        xaxis = list(title = "Missing Count Group"),
        yaxis = list(title = if(input$view_mode=="Percent") "Percent (%)" else "Count")
      )
  })
  
  output$click_info <- renderPrint({
    d <- event_data("plotly_click", source = "bar_source")
    if(is.null(d)){
      cat("click bar and check details")
    } else {
      cat("👉 you click：", d$x)
    }
  })
}

# run App
shinyApp(ui, server)