# =========================
# Load All Required Packages
# =========================



library(shiny); library(ggplot2); library(dplyr); library(readxl); library(readr); library(tools); library(broom);
library(DT); library(htmlwidgets); library(htmltools); library(pagedown); library(mcr); library(lme4);
library(writexl); library(htmlTable); library(shinyBS); library(pROC); library(purrr); library(binom);
library(stringr); library(zoo); library(tidyr); library(janitor); library(boot); library(outliers); library(lmtest);
library(DBI); library(RSQLite); library(rsconnect); library(triangle); library(openxlsx); library(tibble); library(openxlsx2)
library(shinyjs); library(plotly);library(lubridate);library(stats);library(MethComp);library(lmtest);library(strucchange); library(exactci) ; library(rmda); library(nricens);library(dcurves);library(rlang);library(patchwork)
library(reactable);library(broom.mixed);library(qcc);library(randtoolbox);library(slider);library(furrr); library(qqplotr); library(robustbase)
library(MASS);library(shinyjs);library(KSgeneral);library(merDeriv);library(pbkrtest);library(cowplot);library(gridExtra);library(VCA)


# =========================
# UI
# =========================
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Unified Analysis App"),
  downloadButton("download_sample", "Download Sample Data"),
  tabsetPanel(
    id = "analysis_type",
    # --- 1. Method Comparison ---
    tabPanel("Method Comparison",
             sidebarLayout(
               sidebarPanel(
                 fileInput("mc_file", "Upload CSV or Excel File", accept = c(".csv", ".xlsx", ".xls")),
                 div(style = "margin-top: -25px; margin-bottom: 10px;",
                     tags$small("Required columns: Reference method, Test method")
                 ),
                 uiOutput("mc_column_selector"),
                 radioButtons("x_axis_type", "Bland-Altman X-axis:",
                              choices = c("Average of Methods" = "average", "Reference" = "reference")),
                 radioButtons("y_axis_type", "Bland-Altman Y-axis:",
                              choices = c("Absolute Difference" = "absolute", "Percentage Difference" = "percentage")),
                 selectInput("ba_method", "Bland–Altman Method",
                             choices = c("Classic" = "classic",
                                         "Nonparametric" = "nonparametric",
                                         "Regression-based" = "regression")),
                 
                 radioButtons("regression_method", "Regression Method:",
                              choices = c("Passing-Bablok" = "PaBa", "Deming" = "Deming")),
                 
                 conditionalPanel(
                   condition = "input.regression_method == 'Deming'",
                   tagList(
                     fluidRow(
                       column(12,
                              selectInput("dup_id_col", "Duplicate Sample ID Column ", choices = NULL, selected = character(0))
                       )
                     ),
                     fluidRow(
                       column(6,
                              selectInput("dup_x", "Duplicate for Reference (X):", choices = NULL, selected = character(0))
                       ),
                       column(6,
                              numericInput("cv_x", "or CV for Reference (X) (%)", value = NA, min = 0)
                       )
                     ),
                     fluidRow(
                       column(6,
                              selectInput("dup_y", "Duplicate for Test (Y):", choices = NULL, selected = character(0))
                       ),
                       column(6,
                              numericInput("cv_y", "or CV for Test (Y) (%)", value = NA, min = 0)
                       )
                     )
                   )
                 ),
                 sliderInput("font_size", "Font Size (pt):", min = 8, max = 24, value = 12),
                 actionButton("analyze", "Analyze"),
                
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Data Preview",
                            fluidPage(
                              h3("Method Comparison Preview"),
                              reactable::reactableOutput("mc_preview", height = "500px")
                            )
                   ),
                   tabPanel("Analysis",
                            fluidPage(
                              plotOutput("regression_plot"),
                              downloadButton("download_regression_plot", "Download Plot"),
                              actionLink("toggle_regression_options", label = "▸ Show options"),
                              hidden(
                                div(id = "regression_download_options",
                                    radioButtons("regression_file_type", "Select file type:",
                                                 choices = c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"),
                                                 selected = "png"),
                                    numericInput("regression_dpi", "DPI (resolution)", value = 300, min = 72, max = 1200)
                                )
                              ),
                              h4("Summary Statistics Table"),
                              tableOutput("summary_stats_table"),
                              tableOutput("pb_table"),
                              tableOutput("deming_table"),
                              plotOutput("ba_plot"),
                              downloadButton("download_ba_plot", "Download BA Plot"),
                              actionLink("toggle_ba_options", label = "▸ Show options"),
                              hidden(
                                div(id = "ba_download_options",
                                    radioButtons("ba_format", "Select file type:",
                                                 choices = c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"),
                                                 selected = "png"),
                                    numericInput("ba_dpi", "DPI (resolution)", value = 300, min = 72, max = 1200)
                                )
                              ),
                              verbatimTextOutput("ba_trend_text"),
                              uiOutput(outputId = paste0("outlier_ui_", "method_name")),
                              uiOutput("ba_plots"),
                              conditionalPanel(
                                condition = "input.regression_method == 'Deming'",
                                textOutput("lambda_text")
                              )
                            )
                   )
                   
                   
                 )
                 
               )
             )
    ),
    
    # --- 3. Linearity ---
    tabPanel("Linearity",
             sidebarLayout(
               sidebarPanel(
                 fileInput("lin_file", "Upload CSV or Excel File", accept = c(".csv", ".xls", ".xlsx")),
                 conditionalPanel(
                   condition = "input.tabselected == 'OLS'",
                 uiOutput("y_col_ui"),
                 uiOutput("x_col_ui"),
                 uiOutput("group_col_ui"),
                 uiOutput("substance_ui"),
                 selectInput("x_transform", "X Transformation:",
                             choices = c(
                               "No Transformation" = "none",
                               "Natural Logarithm: log(x)" = "log",
                               "Square Root: sqrt(x)" = "sqrt",
                               "Square: x^2" = "square",
                               "Reciprocal: 1/x" = "reciprocal",
                               "Exponential: e^x" = "exp"
                             ),
                             selected = "none"
                 ),
                 selectInput("y_transform", "Y Transformation:",
                             choices = c(
                               "No Transformation" = "none",
                               "Natural Logarithm: log(y)" = "log",
                               "Square Root: sqrt(y)" = "sqrt",
                               "Square: y^2" = "square",
                               "Reciprocal: 1/y" = "reciprocal",
                               "Exponential: e^y" = "exp"
                             ),
                             selected = "none"
                 ),
                 selectInput(
                   inputId = "poly_degree",
                   label = "Poly Degree:",
                   choices = 1:6,
                   selected = 1
                 )
                 ),
                 conditionalPanel(condition = "input.tabselected == 'CLSI EP6'",
                                  uiOutput("spec_id_ui"),  
                                  uiOutput("assigned_ui"),   
                                  uiOutput("rep_cols_ui"),
                                  uiOutput("allowable_error_ui"),
                                  sliderInput("x_range", "X Range", min = 0, max = 1000, value = c(0, 100))
                                  
                 ),
                 
        
                 actionButton("run_linearity", "Run Linearity Analysis")
               ),
               mainPanel(
                 tabsetPanel(id= "tabselected",
                   tabPanel("Data Preview",  reactable::reactableOutput("lin_preview")),
                   tabPanel("OLS",
                            plotOutput("linearity_plot"),
                            downloadButton("download_linearity_plot", "Download Linearity Plot"),
                            actionLink("toggle_download_options", label = "▸ Show options"),
                            hidden(
                              div(id = "download_options",
                                  radioButtons("file_type", "Select file type:",
                                               choices = c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"),
                                               selected = "png"),
                                  numericInput("dpi", "DPI (resolution)", value = 300, min = 72, max = 1200)
                              )
                            ),
                            tableOutput("cor_summary_1"),
                            tableOutput("cor_summary_2"),
                           h4("OLS Regression Summary"),
                            tableOutput("fit_summary_ols"),
                           h4("Lack of fit"),
                           tableOutput("lack_of_fit"),
                            h4("ANOVA"),
                            tableOutput("anova_summary"),
                            h4("Parameter estimates"),
                            tableOutput("coef_summary"),
                            ),
                   tabPanel("Graphs",
                            fluidRow(
                              column(6, plotOutput("residual_by_predicted", height = "300px")),
                              column(6, plotOutput("actual_by_predicted", height = "300px"))
                            ),
                            br(),
                            fluidRow(
                              column(6, plotOutput("residual_by_row", height = "300px")),
                              column(6, plotOutput("residual_by_x", height = "300px"))
                            ),
                            br(),
                            fluidRow(
                              column(6, plotOutput("residual_qqplot", height = "300px"))
                            )
                   
                   ),
                   tabPanel("CLSI EP6",
                            DT::dataTableOutput("ep6_table"),
                            DT::dataTableOutput("results_table"),
                            tags$div(style = "font-size: small; color: gray; margin-top: 5px;", textOutput("reg_method_text")),
                            DT::dataTableOutput("deviation_table"),
                            plotOutput("ep6_linearity_plot"),
                            plotOutput("ep6_residual_plot")
                   )
                 )
                 
               )
             )
    ),
    
    # --- 4. Precision ---
    tabPanel("Precision",sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload CSV or Excel File", 
                  accept = c(".csv", ".xlsx", ".xls")),

        uiOutput("var_select_ui"),
        actionButton("run_precision", "Run Precision")
        
        
        
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Data Preview",
                   h4("Precision Data Preview"),
                   reactableOutput("precision_preview")
                   
          ),
          
          tabPanel("Outlier Rejection",
                   h4("Outlier Rejection Criteria"),
                   conditionalPanel(
                     condition = "input.outlier_mode == 'Calculated'",
                     fileInput("outlier_file", "Upload CSV or Excel file", accept = c(".csv", ".xlsx", ".xls"))
                   ),
                   helpText("Set criteria for rejecting outliers based on preliminary SD."),
                   radioButtons("outlier_mode", "Source of Preliminary SD",
                                choices = c("Calculated", "Manual Entry", "No Outlier Rejection"),
                                selected = "Calculated"),
                   
                   conditionalPanel(
                     condition = "input.outlier_mode == 'Manual Entry'",
                     numericInput("manual_sd", "Preliminary SD", value = NA, step = 0.01)
                   ),
                   
                   conditionalPanel(
                     condition =  "input.outlier_mode == 'Manual Entry' || input.outlier_mode == 'Calculated'",
                     numericInput("sd_multiplier", "Multiplier", value = 5.5, step = 0.1)
                   ),
                   conditionalPanel(
                     condition = "input.outlier_mode == 'Calculated'",
                     uiOutput("outlier_column_ui"),
                     tableOutput("outlier_table")
                   ),
                   conditionalPanel(
                     condition = "input.outlier_mode == 'Manual Entry' || input.outlier_mode == 'Calculated'",
                     uiOutput("max_diff_text")
                   )
                   
                   
                   
          ),
          tabPanel("Outlier Data Preview",
                   h4("Outlier Precision Data Preview"),
                   reactableOutput("precision_tagged_preview")
                   
          ),
          tabPanel("Summary",
                   h4("Squared ANOVA Variance Components"),
                   div(
                     style = "text-align: center; font-weight: bold; font-size: 18px;",
                     textOutput("grand_mean_text")
                   ),
                   tableOutput("squared_table"),
                   downloadButton("download_sqd_table_excel", "Download Excel"),
                   downloadButton("download_sqd_table_pdf", "Download PDF"),
                   div(
                     id = "download_options_panel",
                     style = "display: none;",
                     selectInput("plot_format", "Select Format", choices = c("png", "jpeg", "tiff")),
                     numericInput("plot_dpi", "DPI", value = 300, min = 72, max = 600, step = 10)
                   ),
                   plotOutput("sd_index_plot"),
                   actionLink("toggle_download_options", label = "▸ Show download options"),
                   conditionalPanel(
                     condition = "input.toggle_download_options % 2 == 1",
                     selectInput("plot_format", "Select Format", choices = c("png", "jpeg", "tiff")),
                     numericInput("plot_dpi", "DPI", value = 300, min = 72, max = 600, step = 10)
                   ),
                   downloadButton("download_plot", "Download Plot")
          )
        )
      )
      
    )
             
           
    ),
    # --- 5. ROC Analysis ---
    tabPanel("ROC Analysis",
             sidebarLayout(
               
               sidebarPanel(
                 fileInput("roc_file", "Upload CSV or Excel File", accept = c(".csv", ".xlsx")),
                 div(style = "margin-top: -25px; margin-bottom: 10px;",
                     tags$small("Required columns: outcome (binary), predictor score. Optional: second predictor score, stratification variable")
                 ),
                 
                 conditionalPanel(
                   condition = "input.roc_tabs == 'Single ROC'",
                   uiOutput("roc_column_selector_single")
                 ),
                 conditionalPanel(
                   condition = "input.roc_tabs == 'Compare Independent'",
                   uiOutput("roc_column_selector_compare_independent")
                 ),
                 conditionalPanel(
                   condition = "input.roc_tabs == 'Compare Dependent'",
                   uiOutput("roc_column_selector_compare_dependent")
                 ),
                 conditionalPanel(
                   condition = "input.roc_tabs == 'Partial ROC'",
                   uiOutput("roc_column_selector_partial_single")
                 ),
                 conditionalPanel(
                   condition = "input.roc_tabs == 'Compare Partial ROC'",
                   uiOutput("roc_column_selector_partial_compare")
                 ),
                 actionButton("run_all_roc", "Run Full ROC Analysis")
               ),
               
               mainPanel(
                 tabsetPanel(id = "roc_tabs",
                             tabPanel("Data Preview", reactable::reactableOutput("roc_preview")),
                             tabPanel("Single ROC", 
                                      plotOutput("roc_plot_standard"),
                                      h4("Sample Summary"),
                                      tableOutput("roc_thresh_1"),
                                      h4("AUC"),
                                      tableOutput("roc_thresh_2"),
                                      h4("Youden Index"),
                                      tableOutput("roc_thresh_3"),
                                      h4("Optimal Criterion"),
                                      tableOutput("roc_thresh_4")),
                             tabPanel("Compare Dependent",
                                      plotOutput("roc_plot_compare"),
                                      tableOutput("roc_dependent_table1"),
                                      tableOutput("roc_dependent_table2"),
                                      tableOutput("roc_dependent_table3")),
                             tabPanel("Compare Independent", 
                                      plotOutput("roc_plot_stratified"),
                                      tableOutput("roc_compare_independent_table1"), 
                                      tableOutput("roc_compare_independent_table2"),
                                      tableOutput("roc_compare_independent_table3")),
                             tabPanel("Partial ROC",
                                      plotOutput("roc_plot_partial_single"),
                                      tableOutput("roc_partial_single_1"),
                                      tableOutput("roc_partial_single_2"),
                                       ),
                                      
                             tabPanel("Compare Partial ROC", 
                                      fluidPage(
                                        plotOutput("roc_plot_partial_compare"),
                                        tableOutput("roc_partial_comp_1"),
                                        tableOutput("roc_partial_comp_2"),
                                        tableOutput("roc_partial_comp_3")
                                      ))
                 )
               )
             )
    ),
    # --- 8. Stability Study ---
    tabPanel("Stability",
             sidebarLayout(
               sidebarPanel(
                 fileInput("stability_file", "Upload Stability Data (.csv, .xlsx)", accept = c(".csv", ".xlsx")),
                 
                 div(style = "margin-top: -25px; margin-bottom: 10px;",
                     tags$small(" Required columns: L1, L2, L3, H1, H2, H3, L4 in each run")
                 ),
                 uiOutput("stability_time_col_ui"),
                 uiOutput("stability_measurement_col_ui"),
                 
                 uiOutput("stability_group_col_ui"),
                 selectInput("stability_time_scale", "Time Scale:",
                             choices = c("linear", "log", "square"), selected = "linear"),
                 
                 selectInput("stability_measurement_scale", "Measurement Scale:",
                             choices = c("linear", "log", "square"), selected = "linear"),
                 
                 numericInput("lsl", "Lower Spec Limit (LSL):", value = NULL),
                 numericInput("usl", "Upper Spec Limit (USL):", value = NULL),
                 sliderInput(
                   inputId = "x_axis_range",
                   label = "Select X-axis range",
                   min = -1000,   
                   max = 1000, 
                   value = c(0, 100),  
                   step = 1
                 ),
                 selectInput("stability_model_choice", "Select model to visualize:",
                             choices = c("model1", "model2", "model3"),
                             selected = "model1"),
                 selectInput("interval_type", "Interval Type:",
                             choices = c("Confidence Interval (CI)" = "CI",
                                         "Prediction Interval (PI)" = "PI"),
                             selected = "CI"),
                 actionButton("run_stability", "Analyze Stability")
                
                 
                 
               ),
               mainPanel(
                 
                 h4("Stability Plot"),
                 verbatimTextOutput("best_model_recommendation"),
                 plotOutput("stability_plot"),
                 downloadButton("download_stability_plot", "Download Plot"),
                 actionLink("toggle_download_options", label = "▸ Show options"),
                 hidden(
                   div(id = "download_options",
                       radioButtons("file_type", "Select file type:",
                                    choices = c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"),
                                    selected = "png"),
                       numericInput("dpi", "DPI (resolution)", value = 300, min = 72, max = 1200)
                   )
                 ),
                 tableOutput("stability_crossing_table"),
                 plotOutput("residual_plot"),
                 h4("Model Summary"),
                 uiOutput("stability_summary"),
                 tags$b("Model Comparisons"),
                 tableOutput("model_comparison_table"),
                 tableOutput("model_legend")
               )
             )
    )
  )
)

# =========================
# Server
# =========================
server <- function(input, output, session) {
  
  output$download_sample <- downloadHandler(
    filename = function() {
      "Sample_Files.zip"
    },
    content = function(file) {
      files_to_zip <- c("www/Ludbrook, 1997.xlsx", "www/Peterson, 2018.xlsx")
      zip::zip(zipfile = file, files = files_to_zip)
    },
    contentType = "application/zip"
  )

  crossover_bias_df <- reactiveVal(data.frame())
  cv_df <- reactiveVal(data.frame())

  bias_ci_store <- reactiveValues()
  
  
  get_linear_combination_stats <- function(model, terms) {
    coefs <- coef(model)
    vcov_mat <- vcov(model)
    missing_terms <- setdiff(terms, names(coefs))
    if (length(missing_terms) > 0) {
      warning(glue::glue(" Missing terms in model: {paste(missing_terms, collapse = ', ')}"))
      return(list(est = NA_real_, se = NA_real_, t = NA_real_, p = NA_real_))
    }
    contrast <- numeric(length(coefs))
    names(contrast) <- names(coefs)
    contrast[terms] <- 1
    est <- sum(contrast * coefs)
    se <- tryCatch({
      sqrt(as.numeric(t(contrast) %*% vcov_mat %*% contrast))
    }, error = function(e) {
      warning(glue::glue("SE computation failed: {e$message}"))
      return(NA_real_)
    })
    t_val <- if (!is.na(se) && se != 0) est / se else NA_real_
    df_resid <- df.residual(model)
    p_val <- if (!is.na(t_val) && !is.na(df_resid)) {
      2 * pt(abs(t_val), df = df_resid, lower.tail = FALSE)
    } else {
      NA_real_
    }
    return(list(est = est, se = se, t = t_val, p = p_val))
  }

  all_bias_ci_list <- list()
  all_bias_df_list <- list()

  plan(multisession) 

  delong_se <- function(roc_obj) {
    labels <- as.numeric(as.character(roc_obj$response))
    scores <- roc_obj$predictor
    
    pos_scores <- scores[labels == 1]
    neg_scores <- scores[labels == 0]
    m <- length(pos_scores)
    n <- length(neg_scores)
    kernel_matrix <- outer(pos_scores, neg_scores, FUN = function(x, y) {
      ifelse(x > y, 1, ifelse(x == y, 0.5, 0))
    })

    V10 <- rowMeans(kernel_matrix)
    V01 <- colMeans(kernel_matrix)
    auc <- mean(V10)
    s10 <- var(V10)
    s01 <- var(V01)
    var_auc <- s10 / m + s01 / n
    se_auc <- sqrt(var_auc)
    
    return(se_auc)
  }
  

  hanley_se <- function(roc) {
    auc_val <- as.numeric(pROC::auc(roc))
    labels <- as.numeric(as.character(roc$response))
    scores <- roc$predictor
    
    pos_scores <- scores[labels == 1]
    neg_scores <- scores[labels == 0]
    n_pos <- length(pos_scores)
    n_neg <- length(neg_scores)
    
    Q1 <- mean(sapply(neg_scores, function(xn) {
      mean(pos_scores > xn)^2
    }))
    
    Q2 <- mean(sapply(pos_scores, function(xa) {
      mean(xa > neg_scores)^2
    }))
    
    numerator <- auc_val * (1 - auc_val) +
      (n_pos - 1) * (Q1 - auc_val^2) +
      (n_neg - 1) * (Q2 - auc_val^2)
    
    denominator <- n_pos * n_neg
    
    if (numerator >= 0 && denominator > 0) {
      auc_se <- sqrt(numerator / denominator)
    } else {
      auc_se <- NA
    }
    return(auc_se)
  }
  binom_ci <- function(x, n, conf = 0.95, return_numeric = FALSE) {
    ci_df <- binom::binom.confint(x, n, conf.level = conf, methods = "exact")
    ci <- c(ci_df$lower, ci_df$upper)
    
    if (return_numeric) return(ci)
    paste0(round(ci[1], 4), " to ", round(ci[2], 4))
  }
  
  auc_binom_ci <- function(auc, n_trials = NULL, conf = 0.95, return_numeric = FALSE) {
    if (is.null(n_trials)) {
      stop("You must provide a total number of trials (n_trials), e.g., n_pos * n_neg")
    }
    successes <- round(auc * n_trials)
    binom_ci(successes, n_trials, conf = conf, return_numeric = return_numeric)
  }

  
  
  compute_pb_ci <- function(x, y, alpha = 0.05) {
    n <- length(x)
    slopes <- c()
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        dx <- x[j] - x[i]
        dy <- y[j] - y[i]
        if (dx != 0) slopes <- c(slopes, dy / dx)
      }
    }
    slopes <- sort(slopes)
    N <- length(slopes)
    z <- qnorm(1 - alpha/2)
    k <- floor((N - z * sqrt(N)) / 2)
    l <- ceiling((N + z * sqrt(N)) / 2)
    k <- max(1, k)
    l <- min(N, l)
    list(
      slope = median(slopes),
      slope_LCI = slopes[k],
      slope_UCI = slopes[l]
    )
  }


  
  pooled_sd_from_wide <- function(data, value_cols, id_col) {
    if (length(value_cols) != 2) stop("value_cols must be a character vector of length 2.")
    if (!all(c(value_cols, id_col) %in% colnames(data))) stop("One or more columns not found in data.")
   
    df_long <- data %>%
      rename(dup_id = !!sym(id_col),
             value1 = !!sym(value_cols[1]),
             value2 = !!sym(value_cols[2])) %>%
      pivot_longer(cols = c(value1, value2), names_to = "rep", values_to = "value") %>%
      filter(!is.na(value))
    
    if (nrow(df_long) == 0) {
      warning("⚠️ No valid values after pivoting.")
      return(NA_real_)
    }
    stats <- df_long %>%
      group_by(dup_id) %>%
      summarise(n = n(), s = sd(value), .groups = "drop") %>%
      filter(n > 1)
    numerator <- sum((stats$n - 1) * stats$s^2, na.rm = TRUE)
    denominator <- sum(stats$n - 1, na.rm = TRUE)
    pooled_sd <- sqrt(numerator / denominator)
    
    if (!is.na(pooled_sd)) {
      message("Calculated pooled SD (correct): ", round(pooled_sd, 5))
    } else {
      warning(" Pooled SD calculation returned NA.")
    }
    
    return(pooled_sd)
  }

  
  
  safe_round <- function(x, digits = 4) {
    
    if (is.numeric(x) && all(is.finite(x))) {
      out <- round(x, digits)
      
      return(out)
    } else {
      cat("❌ safe_round invalid input. Returning NA\n")
      return(NA)
    }
  }
  
  
  safe_ci_text <- function(ci) {
    if (length(ci) == 2 && all(is.finite(ci))) {
      return(sprintf("%.3f to %.3f", ci[1], ci[2])) 
    } else {
      return("NA to NA")
    }
  }
  
  
  regression_result <- reactiveVal(NULL)  
  
  regression_store <- reactiveValues(models = list())
  
  fit_pb_model <- function(x, y, conf.level = 0.95) {
    stopifnot(length(x) == length(y))
    n <- length(x)
    
    sij <- c()
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        dx <- x[j] - x[i]
        dy <- y[j] - y[i]
        
        if (dx == 0) {
          # 수직선: ±Inf 처리 → 정렬 위해 큰 수로 대체
          slope <- ifelse(dy > 0, 1e6, ifelse(dy < 0, -1e6, NA))
        } else {
          slope <- dy / dx
        }
        
        # slope = -1 제외
        if (!is.na(slope) && slope != -1) {
          sij <- c(sij, slope)
        }
      }
    }
    
    sij <- sort(sij)
    M <- length(sij)
    K <- sum(sij < -1)
    
    if (M %% 2 == 1) {
      b <- sij[(M + 1) / 2 + K]
    } else {
      b <- mean(sij[M / 2 + K], sij[M / 2 + 1 + K])
    }
    
    a <- median(y - b * x)
    
    z <- qnorm(1 - (1 - conf.level) / 2)
    c_val <- z * sqrt(n * (n - 1) * (2 * n + 5) / 18)
    M1 <- round((M - c_val) / 2)
    M2 <- M - M1 + 1
    
    slope_lci <- sij[M1 + K]
    slope_uci <- sij[M2 + K]
    
    intercept_lci <- median(y - slope_uci * x)
    intercept_uci <- median(y - slope_lci * x)
    
    list(
      method = "pb",
      intercept = list(est = a, lci = intercept_lci, uci = intercept_uci),
      slope = list(est = b, lci = slope_lci, uci = slope_uci),
      fitted = a + b * x,
      residuals = y - (a + b * x)
    )
  }
  
  
  
  compute_cusum_pb <- function(x, y, intercept, slope, alpha = 0.05) {
    n <- length(x)
    stopifnot(n == length(y))

    fitted <- intercept + slope * x
    tau <- sign(y - fitted)

    denom <- sqrt(1 + slope^2)
    D <- (y + slope * x - intercept) / denom

    ord <- order(D)
    tau_sorted <- tau[ord]

    tau_nonzero <- tau_sorted[tau_sorted != 0]
    L <- length(tau_nonzero)
    if (L == 0) {
      warning("All residual signs are zero.")
      return(NULL)
    }

    cusum <- cumsum(tau_nonzero)
    T_stat <- max(abs(cusum))

    H_stat <- T_stat / sqrt(L + 1)

    h_alpha <- switch(as.character(alpha),
                      "0.01" = 1.63,
                      "0.05" = 1.36,
                      "0.10" = 1.22,
                      1.36)
    crit_value <- h_alpha * sqrt(L + 1)

    ks_pvalue <- function(D, tol = 1e-10) {
      k <- 1
      sum <- 0
      repeat {
        term <- 2 * (-1)^(k - 1) * exp(-2 * k^2 * D^2)
        sum <- sum + term
        if (abs(term) < tol) break
        k <- k + 1
      }
      return(max(min(sum, 1), 0))
    }
    p_value <- ks_pvalue(H_stat)

    reject <- if (T_stat > crit_value) {
      "Linearity rejected: significant deviation (P < alpha)"
    } else {
      "Linearity not rejected: no significant deviation"
    }

    list(
      L = L,
      tau = tau_nonzero,
      cusum = cusum,
      stat = T_stat,
      H_stat = H_stat,
      critical_value = crit_value,
      p_value = p_value,
      reject = reject
    )
  }
  
  jackknife_deming <- function(x, y, lambda) {
    N <- length(x)
    slopes <- numeric(N)
    intercepts <- numeric(N)
    
    
    x_bar_full <- mean(x)
    y_bar_full <- mean(y)
    Sx2_full <- var(x)
    Sy2_full <- var(y)
    Sxy_full <- cov(x, y)
    
    b_full <- (Sy2_full - (1 / lambda) * Sx2_full + 
                 sqrt((Sy2_full - (1 / lambda) * Sx2_full)^2 + 4 * (1 / lambda) * Sxy_full^2)) / 
      (2 * Sxy_full)
    a_full <- y_bar_full - b_full * x_bar_full
    
    for (i in 1:N) {
      idx <- setdiff(1:N, i)
      x_i <- x[idx]
      y_i <- y[idx]
      x_bar <- mean(x_i)
      y_bar <- mean(y_i)
      Sx2 <- var(x_i)
      Sy2 <- var(y_i)
      Sx <- sqrt(Sx2)
      Sy <- sqrt(Sy2)
      r <- cor(y_i, x_i)
      
      U <- (Sy2 - (1/lambda) * Sx2) / (2 * Sx * Sy * r)
      b <- U + sqrt(U^2 + 1/lambda)
      a <- y_bar - b * x_bar
      
      slopes[i] <- b
      intercepts[i] <- a
    }
    
    slope_pseudo <- N * b_full - (N - 1) * slopes
    intercept_pseudo <- N * a_full - (N - 1) * intercepts
    
    mean_slope_jack <- mean(slope_pseudo)
    mean_intercept_jack <- mean(intercept_pseudo)
    t_val <- qt(0.975, df = N - 1)
    
    se_slope <- sqrt(sum((slope_pseudo - mean_slope_jack)^2) / (N * (N - 1)))
    se_intercept <- sqrt(sum((intercept_pseudo - mean_intercept_jack)^2) / (N * (N - 1)))
    
    slope_CI <- b_full+ c(-1, 1) * t_val * se_slope
    intercept_CI <- a_full + c(-1, 1) * t_val * se_intercept
    
    return(list(
      b_full = b_full,
      a_full = a_full,
      se_slope = se_slope,
      se_intercept = se_intercept,
      slope_CI = slope_CI,
      intercept_CI = intercept_CI
    ))
  }
  
  method_results <- reactiveVal()
  
  # 1. METHOD COMPARISON MODULE
  
  mc_data <- reactive({
    req(input$mc_file)
    ext <- tools::file_ext(input$mc_file$name)
    if (ext == "csv") read_csv(input$mc_file$datapath) else read_excel(input$mc_file$datapath)
  })
  
  output$mc_column_selector <- renderUI({
    req(mc_data())
    cols <- names(mc_data())
    tagList(
      selectInput("reference_col", "Reference Method", choices = cols),
      selectInput("test_col", "Test Methods", choices = cols)
    )
  })
  observe({
    req(mc_data())
    colnames_available <- names(mc_data())
    updateSelectInput(session, "dup_id_col", choices = c("None", colnames_available), selected= character(0))
    updateSelectInput(session, "dup_x", choices= c("None", colnames_available), selected = character(0))
    updateSelectInput(session, "dup_y", choices = c("None", colnames_available), selected = character(0))
  })
  
  analysis <- eventReactive(input$analyze, {
    req(input$reference_col, input$test_col)
    df <- mc_data()
    
    if (!is.null(input$dup_x) && input$dup_x != "") {
      df$ref_avg <- rowMeans(df[, c(input$reference_col, input$dup_x)], na.rm = TRUE)
    } else {
      df$ref_avg <- df[[input$reference_col]]
    }
    
    if (!is.null(input$dup_y) && input$dup_y != "") {
      df$test_avg <- rowMeans(df[, c(input$test_col, input$dup_y)], na.rm = TRUE)
    } else {
      df$test_avg <- df[[input$test_col]]
    }
    
    valid <- complete.cases(df$ref_avg, df$test_avg)
    
    list(reference = df$ref_avg[valid],
         test_data = data.frame(test_col = df$test_avg[valid]))
  })
  
  expanded_results <- reactiveValues()
  perform_regression_analysis <- function(ref, test, method_type, input) {
    if (method_type == "Deming") {
      ref_vals <- analysis()$reference
      test_vals <- analysis()$test_data$test_col
      x_bar <- mean(ref_vals, na.rm = TRUE)
      y_bar <- mean(test_vals, na.rm = TRUE)
      sd_x <- if (!is.null(input$dup_x) && nzchar(input$dup_x)) {
        cat("Using duplicate columns for sd_x\n")
        pooled_sd_from_wide(
          mc_data(),
          value_cols = c(input$reference_col, input$dup_x),
          id_col = input$dup_id_col
        )
      } else if (!is.null(input$cv_x) && nzchar(input$cv_x)) {
        cat("Using manual CV_x input\n")
        (as.numeric(input$cv_x) / 100) * x_bar
      } else {
        cat("Error: Neither dup_x nor cv_x is provided. sd_x is set to NA.\n")
        NA
      }
      sd_y <- if (!is.null(input$dup_y) && nzchar(input$dup_y)) {
        cat("Using duplicate columns for sd_y\n")
        pooled_sd_from_wide(
          mc_data(),
          value_cols = c(input$test_col, input$dup_y),
          id_col = input$dup_id_col
        )
      } else if (!is.null(input$cv_y) && nzchar(input$cv_y)) {
        cat("Using manual CV_y input\n")
        (as.numeric(input$cv_y) / 100) * y_bar
      } else {
        cat("Error: Neither dup_y nor cv_y is provided. sd_y is set to NA.\n")
        NA
      }
      
 
      N <- length(ref_vals)
      Sx2 <- var(ref_vals, na.rm = TRUE)
      Sy2 <- var(test_vals, na.rm = TRUE)
      Sx <- sqrt(Sx2)
      Sy <- sqrt(Sy2)
      r <- cor(test_vals, ref_vals )
      n <- sum(complete.cases(test_vals, ref_vals))
      z <- 0.5 * log((1 + r) / (1 - r))
      se_z <- 1 / sqrt(n - 3)
      z_crit <- qnorm(0.975)
      
      z_low <- z - z_crit * se_z
      z_high <- z + z_crit * se_z
      
      r_low <- (exp(2 * z_low) - 1) / (exp(2 * z_low) + 1)
      r_high <- (exp(2 * z_high) - 1) / (exp(2 * z_high) + 1)
      
      pearson_ci <- c(r_low, r_high)
      lambda <- (sd_x^2) / (sd_y^2)
      numerator <- Sy2 - (1/lambda) * Sx2 
      denominator <- 2 * Sx*Sy*r
      U <- numerator / denominator
      slope <- U + sqrt(U^2 + (1/lambda))
      intercept <- y_bar - slope * x_bar
      jackknife_res <- jackknife_deming(ref_vals, test_vals, lambda)
      
      
      coef_summary <- data.frame(
        EST = c(intercept, slope),
        SE = c(jackknife_res$se_intercept, jackknife_res$se_slope),
        LCI = c(jackknife_res$intercept_CI[1], jackknife_res$slope_CI[1]),
        UCI = c(jackknife_res$intercept_CI[2], jackknife_res$slope_CI[2]),
        row.names = c("Intercept", "Slope")
      )
      
      
      return(list(
        model = NULL,
        slope = slope,
        intercept = intercept,
        sd_x=sd_x,
        sd_y=sd_y,
        r=r,
        pearson_ci=pearson_ci,
        intercept_CI = jackknife_res$intercept_CI,
        slope_CI = jackknife_res$slope_CI,
        se_intercept = jackknife_res$se_intercept,
        se_slope = jackknife_res$se_slope,
        coef_summary = coef_summary
      ))
      
    
    } else {
      ref_vals <- analysis()$reference
      test_vals <- analysis()$test_data$test_col
      model_pb <- fit_pb_model(ref_vals, test_vals)
      intercept <- model_pb$intercept$est
      slope     <- model_pb$slope$est
      intercept_CI <- c(model_pb$intercept$lci, model_pb$intercept$uci)
      slope_CI     <- c(model_pb$slope$lci,     model_pb$slope$uci)
      resid_orth <- (test_vals - (intercept + slope * ref_vals)) /sqrt(1 + slope^2)
      resid_sd <- sqrt(sum(resid_orth^2, na.rm=TRUE)/(sum(!is.na(resid_orth))-2))
      rsd_interval <- c(-1.96*resid_sd, 1.96*resid_sd)
      
      cusum_res <- compute_cusum_pb(
        x = ref_vals, y = test_vals,
        intercept = intercept, slope = slope, alpha = 0.05
      )
      
      return(list(
        model = model_pb,
        slope = slope,
        intercept = intercept,
        intercept_CI = intercept_CI,
        slope_CI = slope_CI,
        resid_sd = resid_sd,
        rsd_interval = rsd_interval,
        cusum_stat = cusum_res$statistic,
        cusum_critical = cusum_res$critical_value,
        cusum_H_stat = cusum_res$H_statistic,       
        cusum_p_value = cusum_res$p_value, 
        cusum_reject = cusum_res$reject,
        coef_summary = data.frame(
          EST = c(intercept, slope),
          LCI = c(intercept_CI[1], slope_CI[1]),
          UCI = c(intercept_CI[2], slope_CI[2]),
          row.names = c("Intercept","Slope")
        )
      ))
    }
  }
  observeEvent(input$analyze, {
    req(analysis())
    method_name <- input$test_col
    result <- perform_regression_analysis(
      analysis()$reference,
      analysis()$test_data[[method_name]],
      input$regression_method,
      input
    )
    regression_store$models[[method_name]] <- result
  })
  
  regression_plot <- reactive({
    req(input$analyze)
    req(regression_store$models[[input$test_col]])
    df_raw <- mc_data()
    
    use_avg <- (
    input$regression_method == "Deming" &&
    !is.null(input$dup_x) && input$dup_x != "None" &&
    !is.null(input$dup_y) && input$dup_y != "None" &&
    (is.null(input$cv_x) || is.null(input$cv_y))
    )
    
    if (use_avg) {
      ref_vals <- rowMeans(df_raw[, c(input$reference_col, input$dup_x)], na.rm = TRUE)
      test_vals <- rowMeans(df_raw[, c(input$test_col, input$dup_y)], na.rm = TRUE)
      x_lab <- paste0("Mean of ", input$reference_col, " & ", input$dup_x)
      y_lab <- paste0("Mean of ", input$test_col, " & ", input$dup_y)
      cat("[Deming] Using mean of duplicates for plot\n")
    } else {
      ref_vals <- analysis()$reference
      test_vals <- analysis()$test_data[["test_col"]]
      x_lab <- input$reference_col
      y_lab <- input$test_col
    }
    
    cat("length x,y:", length(ref_vals), length(test_vals), "\n")
    df <- data.frame(x = ref_vals, y = test_vals)
    cat("Plotting with", nrow(df), "points\n")
    
    res <- regression_store$models[[input$test_col]]
    eq_label <- paste0("y = ", round(res$intercept, 5), " + ", round(res$slope, 5), "·x")
    
    ggplot(df, aes(x = x, y = y)) +
      geom_point(alpha = 0.7) +
      geom_abline(intercept = res$intercept, slope = res$slope, color = "blue", size = 1) +
      labs(
        title = paste(input$regression_method, "Regression:", input$test_col, "vs", input$reference_col),
        subtitle = paste0(
          eq_label, "\n",
          "Slope 95% CI: ", round(res$slope_CI[1], 5), " – ", round(res$slope_CI[2], 5), "\n",
          "Intercept 95% CI: ", round(res$intercept_CI[1], 5), " – ", round(res$intercept_CI[2], 5)
        ),
        x = x_lab,
        y = y_lab
      ) +
      theme_minimal(base_size = input$font_size)
  })
  
  output$regression_plot <- renderPlot({
    regression_plot()
  })
  output$download_regression_plot <- downloadHandler(
    filename = function() {
      ext <- input$regression_file_type
      paste0("regression_plot_", input$test_col, ".", ext)
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot = regression_plot(),
        width = 8,
        height = 6,
        dpi = input$regression_dpi %||% 300,         # default fallback
        device = input$regression_file_type %||% "png"
      )
    }
  )
  
  regression_options_visible <- reactiveVal(FALSE)
  observeEvent(input$toggle_regression_options, {
    shinyjs::toggle("regression_download_options")
    regression_options_visible(!regression_options_visible())
    
    label <- if (regression_options_visible()) {
      "▾ Hide options"
    } else {
      "▸ Show options"
    }
    
    updateActionLink(session, "toggle_regression_options", label = label)
  })

  
  output$pb_table <- renderTable({
    req(input$analyze)
    req(input$regression_method == "PaBa")
    req(regression_store$models[[input$test_col]])
    model <- regression_store$models[[input$test_col]]
    
    intercept <- model$intercept
    slope     <- model$slope
    intercept_CI <- model$intercept_CI
    slope_CI     <- model$slope_CI
    RSD     <- model$resid_sd
    cusum <- model$cusum_reject
    cusum_p <- model$cusum_p_value
    
    tibble::tibble(
      Section = c("Regression Equation",
                  "Intercept", 
                  "95% CI",
                  "Slope", 
                  "95% CI",
                  "RSD",
                  "±1.96 × RSD Interval",
                  "CUSUM test",
                  "CUSUM p-value"),
      
      Value = c(
        paste0("y = ", round(intercept, 6), " + ", round(slope, 6), " * x"),
        round(intercept, 4),
        paste0(round(intercept_CI[1], 4), " to ", round(intercept_CI[2], 4)),
        round(slope, 4),
        paste0(round(slope_CI[1], 4), " to ", round(slope_CI[2], 4)),
        round(RSD,4),
        paste0(round(-1.96 * RSD, 2), " to ", round(1.96 * RSD, 2)),
        cusum,
        round(cusum_p,2)
      )
    )
  },digits=4)
  output$deming_table <- renderTable({
    req(input$regression_method == "Deming")
    req(regression_store$models[[input$test_col]])
    model <- regression_store$models[[input$test_col]]
    
    intercept <- model$intercept
    slope     <- model$slope
    intercept_CI <- model$intercept_CI
    slope_CI     <- model$slope_CI
    se_intercept <- model$se_intercept
    se_slope     <- model$se_slope
    pearson_r    <- model$r
    pearson_ci  <- model$pearson_ci
    print(paste("intercept:", intercept))
    print(paste("slope:", slope))
    print(paste("intercept_CI:", paste(intercept_CI, collapse = ", ")))
    print(paste("slope_CI:", paste(slope_CI, collapse = ", ")))
    print(paste("se_intercept:", se_intercept))
    print(paste("se_slope:", se_slope))
    print(paste("pearson_r:", pearson_r))
    print(paste("pearson_ci:", paste(pearson_ci, collapse = ", ")))
    tibble::tibble(
      Section = c("Regression Equation",
                  "Intercept", 
                  "SE", 
                  "95% CI",
                  "Slope", 
                  "SE",
                  "95% CI",
                  "Pearson correlation coefficeint",
                  "95% CI"),
      
      Value = c(
        paste0("y = ", round(intercept, 6), " + ", round(slope, 6), " * x"),
        round(intercept, 4),
        round(se_intercept, 4),
        paste0(round(intercept_CI[1], 4), " to ", round(intercept_CI[2], 4)),
        round(slope, 4),
        round(se_slope, 4),
        paste0(round(slope_CI[1], 4), " to ", round(slope_CI[2], 4)),
        round(pearson_r, 4),
        paste0(round(pearson_ci[1], 4), " to ", round(pearson_ci[2], 4))
        
      )
    )
  })
        observeEvent(input$analyze, {
          req(analysis())
          
          ref <- analysis()$reference
          test <- analysis()$test_data$test_col
          method_name <- input$test_col
          ref_name <- input$reference_col
          

          
          
          output$ba_plot <- renderPlot({ 
            req(input$analyze)
            req(ba_plot())
            ba_plot()
          })
          
          output$download_ba_plot <- downloadHandler(
            filename = function() {
              ext <- if (!is.null(input$ba_format)) input$ba_format else "png"
              paste0("ba_plot_", method_name, ".", ext)
            },
            content = function(file) {
              req(input$analyze)
              fmt <- if (!is.null(input$ba_format)) input$ba_format else "png"
              dpi_val <- if (!is.null(input$ba_dpi)) input$ba_dpi else 300
              ggsave(
                filename = file,
                plot = ba_plot(),
                width = 7,
                height = 5,
                dpi = dpi_val,
                device = fmt
              )
            }
          )
          ba_options_visible <- reactiveVal(FALSE)
          
          observeEvent(input$toggle_ba_options, {
            shinyjs::toggle("ba_download_options")
            ba_options_visible(!ba_options_visible())
            
            label <- if (ba_options_visible()) {
              "▾ Hide options"
            } else {
              "▸ Show options"
            }
            
            updateActionLink(session, "toggle_ba_options", label = label)
          })
          loa_coefs <- reactiveVal(
            c(upper_int    = NA_real_, upper_slope  = NA_real_, lower_int = NA_real_,
              lower_slope  = NA_real_))
           quantile_vals <- reactiveVal(c(lower = NA_real_, median = NA_real_, upper = NA_real_))
          output$ba_trend_text <- renderPrint({
            req(input$analyze)
            avg <- if (input$x_axis_type == "average") (test + ref) / 2 else ref
            diff <- if (input$y_axis_type == "absolute") test - ref else (test - ref) / avg * 100
            
            cat("Shapiro–Wilk Normality Test:\n")
            shap_result <- shapiro.test(diff)
            print(shap_result)
            cat("\n")
            if (input$ba_method == "classic") {
            model <- lm(diff ~ avg)
            smry <- summary(model)
            ci <- confint(model)
              
            intercept <- coef(model)[1]
            slope <- coef(model)[2]
            
            n <- length(diff)
            mean_bias <- mean(diff)
            sd_bias <- sd(diff)
            
            t_crit <- qt(0.975, df = n - 1)
            se_bias <- sd_bias / sqrt(n)
            ci_mean <- mean_bias + c(-1, 1) * t_crit * se_bias
            p_bias <- 2 * pt(-abs(mean_bias / se_bias), df = n - 1)
            
            loa_upper <- mean_bias + 1.96 * sd_bias
            loa_lower <- mean_bias - 1.96 * sd_bias
            
            se_loa <- sqrt((sd_bias^2 / n) + ((1.96^2 * sd_bias^2) / (2 * (n - 1))))
            ci_loa_upper <- loa_upper + c(-1, 1) * t_crit * se_loa
            ci_loa_lower <- loa_lower + c(-1, 1) * t_crit * se_loa
            
            cat(" Bland–Altman Summary Statistics\n")
            cat(sprintf("Mean  = %.4f\n", mean_bias))
            cat(sprintf("95%% CI for Mean = %.4f to %.4f\n", ci_mean[1], ci_mean[2]))
           
            
            cat(sprintf("Upper LoA = %.4f\n", loa_upper))
            cat(sprintf("95%% CI for Upper LoA = %.4f to %.4f\n", ci_loa_upper[1], ci_loa_upper[2]))
            cat(sprintf("Lower LoA = %.4f\n", loa_lower))
            cat(sprintf("95%% CI for Lower LoA = %.4f to %.4f\n\n", ci_loa_lower[1], ci_loa_lower[2]))
            
            cat(" Regression Equation (Y ~ X from BA plot):\n")
            eqn_str <- sprintf("Y = %.4f + %.4f × X", intercept, slope)
            cat(eqn_str, "\n\n")
            
            coefs <- smry$coefficients
            result_df <- data.frame(
              Term = rownames(coefs),
              Estimate = round(coefs[, 1], 6),
              `Std. Error` = round(coefs[, 2], 6),
              `t value` = round(coefs[, 3], 4),
              `Pr(>|t|)` = format.pval(coefs[, 4], digits = 4, eps = .0001),
              `95% CI Lower` = round(ci[, 1], 6),
              `95% CI Upper` = round(ci[, 2], 6),
              check.names = FALSE
            )
            cat(" Regression Summary:\n")
            print(result_df, row.names = FALSE)
            
            }else if (input$ba_method == "nonparametric") {
              set.seed(850) 
              R <- 10000
              boot_medians <- replicate(R, median(sample(diff, replace = TRUE)))

              ci_median<- quantile(boot_medians, probs = c(0.025, 0.975))
              median_bias <- median(diff)
              lower_percentile <- quantile(diff, probs = 0.025, type = 5)
              upper_percentile <- quantile(diff, probs = 0.975, type = 5)
              quantile_vals(c(
                median_bias       = median_bias,
                lower_percentile = lower_percentile,
                upper_percentile = upper_percentile
              ))
              cat("Nonparametric Bland–Altman Summary (LoA = empirical percentiles only)\n")
              cat(sprintf("Median Bias = %.4f\n", median_bias))
              cat(sprintf("95%% CI for Median = %.4f to %.4f\n", ci_median[1], ci_median[2]))
              cat(sprintf("2.5%% Percentile (Lower LoA) = %.4f\n", lower_percentile))
              cat(sprintf("97.5%% Percentile (Upper LoA) = %.4f\n", upper_percentile))
            } else {
              model <- lm(diff ~ avg)
              smry <- summary(model)
              ci <- confint(model)
              
              intercept <- coef(model)[1]
              slope <- coef(model)[2]
              residuals <- resid(model)
              s_resid <- summary(model)$sigma
              
              n <- length(diff)
              mean_bias <- mean(diff)
              sd_bias <- sd(diff)
              
              t_crit <- qt(0.975, df = n - 1)
              se_bias <- sd_bias / sqrt(n)
              ci_mean <- mean_bias + c(-1, 1) * t_crit * se_bias
              p_bias <- 2 * pt(-abs(mean_bias / se_bias), df = n - 1)
              
              loa_upper <- mean_bias + 1.96 * sd_bias
              loa_lower <- mean_bias - 1.96 * sd_bias
              
              se_loa <- sqrt((sd_bias^2 / n) + ((1.96^2 * sd_bias^2) / (2 * (n - 1))))
              ci_loa_upper <- loa_upper + c(-1, 1) * t_crit * se_loa
              ci_loa_lower <- loa_lower + c(-1, 1) * t_crit * se_loa
              
              b0 <- coef(model)[1]
              b1 <- coef(model)[2]
              abs_resid <- abs(residuals)
              model2 <- lm(abs_resid ~ avg)
              c0 <- coef(model2)[1]
              c1 <- coef(model2)[2]
              z <- 1.96*sqrt(pi/2)
              
             
              loa_upper_intercept <- b0 + z *   c0
              loa_upper_slope     <- b1 + z *  c1
              loa_lower_intercept <- b0 - z *   c0
              loa_lower_slope     <- b1 - z *   c1
             
              loa_coefs(c(
                upper_int    = loa_upper_intercept,
                upper_slope  = loa_upper_slope,
                lower_int    = loa_lower_intercept,
                lower_slope  = loa_lower_slope
              ))
              cat("Regression-Based Bland–Altman Summary Statistics\n")
              cat(sprintf("Mean Difference at Mean(avg) = %.4f\n", mean_bias))
              cat(sprintf("95%% CI for Mean Difference = %.4f to %.4f\n\n", ci_mean[1], ci_mean[2]))
              
              cat("Regression Equation (Y ~ X from BA plot):\n")
              eqn_str <- sprintf("Y = %.4f + %.4f × X", intercept, slope)
              cat(eqn_str, "\n\n")
              cat(sprintf("Residual Standard Deviation (RSD): %.4f\n\n", s_resid))

              coefs <- smry$coefficients
              result_df <- data.frame(
                Term = rownames(coefs),
                Estimate = round(coefs[, 1], 6),
                `Std. Error` = round(coefs[, 2], 6),
                `t value` = round(coefs[, 3], 4),
                `Pr(>|t|)` = format.pval(coefs[, 4], digits = 4, eps = .0001),
                `95% CI Lower` = round(ci[, 1], 6),
                `95% CI Upper` = round(ci[, 2], 6),
                check.names = FALSE
              )
              
              print(result_df, row.names = FALSE)
              cat("Regression-based Limits of Agreement\n")
              cat(sprintf("Lower Limit of Agreement: %.4f - %.5f × x\n", loa_lower_intercept, abs(loa_lower_slope)))
              cat(sprintf("Upper Limit of Agreement: %.4f + %.5f × x\n", loa_upper_intercept, loa_upper_slope))
              
            }
          })
          
          ba_plot <- reactive({
            avg <- if(input$x_axis_type=="average") (test+ref)/2 else ref
            diff <- if(input$y_axis_type=="absolute") test-ref else (test-ref)/avg*100
            
            df_plot <- data.frame(avg=avg, diff=diff)
            p <- ggplot(df_plot, aes(x=avg, y=diff)) + geom_point(alpha=0.7)
            
            if (input$ba_method == "classic") {
              m <- mean(diff); s <- sd(diff)
              loa_u <- m + 1.96 * s; loa_l <- m - 1.96 * s
              p <- p +
                geom_hline(yintercept = m, color="blue") +
                geom_hline(yintercept = loa_u, linetype="dashed", color="red") +
                geom_hline(yintercept = loa_l, linetype="dashed", color="red")
            } else if (input$ba_method == "nonparametric") {
              qv     <- quantile_vals()
              
              med    <- qv["median_bias"]
              loa_u  <- qv["upper_percentile.97.5%"]
              loa_l  <- qv["lower_percentile.2.5%"]
              p <- p +
                geom_hline(
                  aes(yintercept = med,      # the constant y
                      color        = "Median",
                      linetype     = "Median"),
                  size        = 1,
                  inherit.aes = FALSE,
                  show.legend  = TRUE
                ) +
                geom_hline(
                  aes(yintercept = loa_l,
                      color        = "2.5%",
                      linetype     = "2.5%"),
                  size        = 1,
                  inherit.aes = FALSE,
                  show.legend  = TRUE
                ) +
                geom_hline(
                  aes(yintercept = loa_u,
                      color        = "97.5%",
                      linetype     = "97.5%"),
                  size        = 1,
                  inherit.aes = FALSE,
                  show.legend  = TRUE
                ) +

                scale_color_manual(
                  name   = NULL,
                  values = c(
                    "Median" = "blue",
                    "2.5%"   = "red",
                    "97.5%"  = "red"
                  )
                ) +
                scale_linetype_manual(
                  name   = NULL,
                  values = c(
                    "Median" = "solid",
                    "2.5%"   = "dashed",
                    "97.5%"  = "dashed"
                  )
                )
              p
              
            } else if (input$ba_method == "regression") {
              coefs       <- loa_coefs()
              xseq      <- seq(min(avg), max(avg), length.out = 100)
              upper_int   <- as.numeric(coefs["upper_int"])
              upper_slope <- as.numeric(coefs["upper_slope"])
              lower_int   <- as.numeric(coefs["lower_int"])
              lower_slope <- as.numeric(coefs["lower_slope"])
              
              p <- p +
                geom_abline(intercept = upper_int,
                            slope     = upper_slope,
                            color     = "blue",
                            linetype  = "dashed",
                            size      = 1,
                            inherit.aes = FALSE) +
                geom_abline(intercept = lower_int,
                            slope     = lower_slope,
                            color     = "blue",
                            linetype  = "dashed",
                            size      = 1,
                            inherit.aes = FALSE)
            }
            
            p + theme_minimal(base_size=input$font_size) +
              labs(
                title = paste("BA Plot:", input$test_col, "vs", input$reference_col),
                x = if(input$x_axis_type=="average") paste("Mean of", input$test_col, "&", input$reference_col)
                else input$reference_col,
                y = if(input$y_axis_type=="absolute")
                  paste(input$test_col, "-", input$reference_col)
                else "Percent Difference (%)"
              )
          })
        })
        

  observe({
    df <- crossover_bias_df()
    req(nrow(df) > 0, "Threshold" %in% names(df))
    
    updateSelectInput(
      session,
      "bias_threshold",
      choices = unique(df$Threshold),
      selected = unique(df$Threshold)[1]
    )
  })
  
  output$summary_stats_table <- renderTable({
    req(input$analyze)
    req(analysis())
    ref <- analysis()$reference
    test_df <- analysis()$test_data
    test <- test_df[[1]]  
    
    if (input$regression_method == "PaBa") {
      basic_metrics <- tibble(
        Metric = c("Lowest value", "Highest value", "Arithmetic mean",
                   "Median", "Standard deviation", "Standard error of the mean"),
        `Reference` = c(
          min(ref, na.rm = TRUE),
          max(ref, na.rm = TRUE),
          mean(ref, na.rm = TRUE),
          median(ref, na.rm = TRUE),
          sd(ref, na.rm = TRUE),
          sd(ref, na.rm = TRUE) / sqrt(sum(!is.na(ref)))
        ),
        `Test` = c(
          min(test, na.rm = TRUE),
          max(test, na.rm = TRUE),
          mean(test, na.rm = TRUE),
          median(test, na.rm = TRUE),
          sd(test, na.rm = TRUE),
          sd(test, na.rm = TRUE) / sqrt(sum(!is.na(test)))
        )
      )%>%
        mutate(Test = as.character(Test))
     
        basic_metrics  
      
    } else if (input$regression_method == "Deming") {
      complete <- complete.cases(ref, test)
      ref_clean <- ref[complete]
      test_clean <- test[complete]
      n <- length(ref_clean)
      mean_ref <- mean(ref_clean)
      mean_test <- mean(test_clean)
      

      if (!is.null(input$dup_x) && nzchar(input$dup_x)) {
        pooled_sd_x <- pooled_sd_from_wide(
          mc_data(),
          value_cols = c(input$reference_col, input$dup_x),
          id_col = input$dup_id_col
        )
        
        cv_x <- (pooled_sd_x / mean_ref) * 100
       
        
      } else if (!is.null(input$cv_x) && nzchar(input$cv_x)) {
        cv_x <- as.numeric(input$cv_x)
    
      } else {
        cv_x <- NA
      }
     
      if (!is.null(input$dup_y) && nzchar(input$dup_y)) {
        pooled_sd_y <- pooled_sd_from_wide(
          mc_data(),
          value_cols = c(input$test_col, input$dup_y),
          id_col = input$dup_id_col
        )
        cv_y <- (pooled_sd_y / mean_test) * 100

        
      } else if (!is.null(input$cv_y) && nzchar(input$cv_y)) {
        cv_y <- as.numeric(input$cv_y)
      } else {
        cv_y <- NA
      }
     
      var_ratio <- ((cv_x * mean_ref) / (cv_y * mean_test))^2
      tibble(
        Metric = c(
          "Sample size (n)",
          "Mean of Reference (X)",
          "Mean of Test (Y)",
          "Coefficient of Variation (CV_x, %)",
          "Coefficient of Variation (CV_y, %)",
          "Variance ratio (Test / Reference)"
        ),
        Value = c(
          n,
          mean_ref,
          mean_test,
          cv_x,
          cv_y,
          var_ratio
        )
      )
    } 
  },digits=4)
  
  output$mc_preview <- reactable::renderReactable({
    req(mc_data())
    df <- as.data.frame(mc_data())
    
    reactable::reactable(
      df,
      pagination = FALSE,
      searchable = TRUE,
      filterable = FALSE,
      striped = TRUE,
      bordered = TRUE,
      highlight = TRUE,
      defaultColDef = reactable::colDef(
        align = "center",
        style = function(value) {
          if (is.na(value)) {
            list(background = "#f8d7da", color = "#721c24", fontWeight = "bold")
          } else {
            list()
          }
        }
      ),
      style = list(height = "500px", overflowY = "scroll")
    )
  })
  

  # === 3. LINEARITY MODULE ===
  output$y_col_ui <- renderUI({
    req(linearity_data())
    selectInput("y_col", "Select Y Variable (Response):", choices = names(linearity_data()))
  })
  
  output$x_col_ui <- renderUI({
    req(linearity_data())
    selectInput("x_col", "Select X Variable (Predictor):", choices = names(linearity_data()))
  })
  
  output$group_col_ui <- renderUI({
    req(linearity_data())
    selectInput("group_col", "Select Grouping Variable (optional):", 
                choices = c("None", names(linearity_data())))
  })
  
  
  output$substance_ui <- renderUI({
    req(linearity_data(), input$group_col)
    if (input$group_col == "None") return(NULL)
    
    substances <- unique(linearity_data()[[input$group_col]])
    selectInput("substance", "Select Substance:", choices = substances)
  })
  
 
  

  linearity_data <- reactive({
    req(input$lin_file)
    ext <- tools::file_ext(input$lin_file$name)
    
    df <- if (ext == "csv") {
      read.csv(input$lin_file$datapath)
    } else {
      readxl::read_excel(input$lin_file$datapath)
    }
    
    colnames(df) <- trimws(colnames(df))
    df <- df %>% rename_all(trimws)

    df
  })

  linearity_transformed <- reactive({
    df <- linearity_data()
    
    df <- df %>%
      mutate(
        Measured = case_when(  
          input$x_transform == "log" & Measured > 0 ~ log(Measured),
          input$x_transform == "sqrt" & Measured >= 0 ~ sqrt(Measured),
          input$x_transform == "square" ~ Measured^2,
          input$x_transform == "reciprocal" & Measured != 0 ~ 1 / Measured,
          input$x_transform == "exp" ~ exp(Measured),
          TRUE ~ Measured
        ),
        Expected = case_when( 
          input$y_transform == "log" & Expected > 0 ~ log(Expected),
          input$y_transform == "sqrt" & Expected >= 0 ~ sqrt(Expected),
          input$y_transform == "square" ~ Expected^2,
          input$y_transform == "reciprocal" & Expected != 0 ~ 1 / Expected,
          input$y_transform == "exp" ~ exp(Expected),
          TRUE ~ Expected
        )
      )
    
    df
  })


  # === Triggered Analysis ===
  selected_model <- reactive({
    req(linearity_data(), input$x_col, input$y_col)
    
    df <- if (input$x_transform == "none" && input$y_transform == "none") {
      linearity_data()
    } else {
      linearity_transformed()
    }
    
    if (input$group_col != "None" && !is.null(input$substance)) {
      df <- df %>% filter(.data[[input$group_col]] == input$substance)
    }
    
    x <- df[[input$x_col]]
    y <- df[[input$y_col]]
    
    
      if (input$x_transform == "none") {
        x_mean <- mean(x, na.rm = TRUE)
        poly_terms <- c(
          input$x_col,
          if (input$poly_degree >= 2) {
            paste0("I((", input$x_col, " - ", x_mean, ")^", 2:input$poly_degree, ")")
          }
        )
      } else {
        poly_terms <- c(
          input$x_col,
          if (input$poly_degree >= 2) {
            paste0("I(", input$x_col, "^", 2:input$poly_degree, ")")
          }
        )
      }
      model_formula <- as.formula(paste(input$y_col, "~", paste(poly_terms, collapse = " + ")))
      model <- lm(model_formula, data = df)
      
    
    
    tibble(
      data = list(df),
      model = list(model),
      x_mean = list(x_mean),
      method = list("ols")
    )
  })

  linearity_plot_object <- reactive({
    req(selected_model(), input$x_col, input$y_col)
    
    df <- selected_model()$data[[1]]
    model <- selected_model()$model[[1]]
    method <- selected_model()$method[[1]]
    x_col <- input$x_col
    y_col <- input$y_col
    info <- if (!is.null(ols_data())) ols_data() else NULL
    print("info:")
    print(info)
    if (!is.null(info)) {
      df$predicted <- info$intercept$est + info$slope$est * df[[x_col]]
    } else {
      df$predicted <- predict(model)
    }

    title_text <- paste("Model Fit (", method, "):", ifelse(input$group_col == "None", "All", input$substance))
    if (!is.null(info)) {
      title_text <- paste0(
        title_text, "\n",
        sprintf("Intercept: %.3f [%.3f, %.3f], ", info$intercept$est, info$intercept$lci, info$intercept$uci),
        sprintf("Slope: %.3f [%.3f, %.3f]", info$slope$est, info$slope$lci, info$slope$uci)
      )
    }
    p <- ggplot(df, aes_string(x = x_col, y = y_col)) +
      geom_point(size = 2) +
      geom_hline(yintercept = mean(df[[y_col]], na.rm = TRUE), color = "red", size = 1) +
      labs(x = x_col, y = y_col, title = title_text) +
      theme_minimal()
    if (!is.null(info)) {
      p <- p +
        geom_abline(intercept = info$intercept$est, slope = info$slope$est, color = "purple", size = 1) +
        geom_abline(intercept = info$intercept$lci, slope = info$slope$lci, linetype = "dashed", color = "gray50") +
        geom_abline(intercept = info$intercept$uci, slope = info$slope$uci, linetype = "dashed", color = "gray50")
    } else {
      p <- p + geom_line(aes(y = predicted), color = "purple", size = 1)
    }
    
    p
  })
  output$linearity_plot <- renderPlot({
    linearity_plot_object()
  })
  output$download_linearity_plot <- downloadHandler(
    filename = function() {
      req(input$file_type)
      paste0("linearity_plot.", input$file_type)
    },
    content = function(file) {
      req(input$dpi)
      ggsave(
        filename = file,
        plot = linearity_plot_object(),
        device = input$file_type,
        dpi = input$dpi,
        width = 8, height = 6
      )
    }
  )
  

  
  
  ols_data <- reactiveVal(NULL) 
  
  output$fit_summary_ols <- renderTable({
    req(selected_model())
    
    model <- selected_model()$model[[1]]
    df <- selected_model()$data[[1]]
    x <- df[[input$x_col]]  
    coefs <- coef(model)
    x_mean <- mean(x, na.rm = TRUE)
    coefs <- coef(model)
    conf <- confint(model)
    ols_data(list(
      intercept = list(est = coefs[1], lci = conf[1, 1], uci = conf[1, 2]),
      slope     = list(est = coefs[2], lci = conf[2, 1], uci = conf[2, 2]),
      method    = "ols"
    ))
    
    x_label <- switch(
      input$x_transform,
      "log" = paste0("log(", input$x_col, ")"),
      "sqrt" = paste0("sqrt(", input$x_col, ")"),
      "square" = paste0("(", input$x_col, ")^2"),
      "reciprocal" = paste0("1/(", input$x_col, ")"),
      "exp" = paste0("exp(", input$x_col, ")"),
      input$x_col
    )
    
    y_label <- switch(
      input$y_transform,
      "log" = paste0("log(", input$y_col, ")"),
      "sqrt" = paste0("sqrt(", input$y_col, ")"),
      "square" = paste0("(", input$y_col, ")^2"),
      "reciprocal" = paste0("1/(", input$y_col, ")"),
      "exp" = paste0("exp(", input$y_col, ")"),
      input$y_col
    )
    
    terms <- paste0(
      round(coefs[1], 5),
      if (input$poly_degree >= 1 && length(coefs) >= 2) {
        paste0(
          ifelse(coefs[2] >= 0, " + ", " - "),
          abs(round(coefs[2], 5)), "*", x_label
        )
      } else "",
      if (input$poly_degree >= 2 && length(coefs) >= 3) {
        mapped_terms <- purrr::map2_chr(
          coefs[-c(1,2)], 
          2:input$poly_degree,
          ~ {
            if (input$x_transform == "none") {
              paste0(
                ifelse(.x >= 0, " + ", " - "),
                abs(round(.x, 5)),
                "*(", x_label, " - ", round(x_mean, 4), ")^", .y
              )
            } else {
              paste0(
                ifelse(.x >= 0, " + ", " - "),
                abs(round(.x, 5)),
                "*", x_label, "^", .y
              )
            }
          }
        )
        paste0(mapped_terms, collapse = "")
      } else ""
    )

    tibble::tibble(
      Metric = c(
        "Regression Equation (Hybrid)",
        "R-Squared",
        "Adjusted R-Squared",
        "Root Mean Square Error (RMSE)",
        "Mean of Response (Y)",
        "Observations"
      ),
      Value = c(
        paste(y_label, "=", terms),
        round(summary(model)$r.squared, 6),
        round(summary(model)$adj.r.squared, 6),
        round(summary(model)$sigma, 6),
        round(mean(df[[input$y_col]], na.rm = TRUE), 5),
        nrow(df)
      )
    )
  })

  output$cor_summary_1<- renderTable({
    req(linearity_data(), input$x_col, input$y_col)
    
    df <- linearity_data()
    
    if (input$group_col != "None") {
      req(input$substance)
      df <- df %>% filter(.data[[input$group_col]] == input$substance)
    }
    x <- df[[input$x_col]]
    y <- df[[input$y_col]]
    
    cor_test <- cor.test(x, y)
    
    tibble::tibble(
      Statistic = c("Correlation", "Covariance", "Count"),
      Value = c(round(cor_test$estimate, 6),
                round(cov(x, y), 6),
                length(x)),
      `Lower 95%` = c(round(cor_test$conf.int[1], 6), NA, NA),
      `Upper 95%` = c(round(cor_test$conf.int[2], 6), NA, NA),
      `Signif. Prob` = c(format.pval(cor_test$p.value, digits = 4, eps = .0001), NA, NA)
    )
  },digits=6)
  
  output$cor_summary_2 <- renderTable({
    req(linearity_data(), input$x_col, input$y_col)
    
    df <- linearity_data()
    
    if (input$group_col != "None") {
      req(input$substance)
      df <- df %>% filter(.data[[input$group_col]] == input$substance)
    }
    x <- df[[input$x_col]]
    y <- df[[input$y_col]]
    
    tibble::tibble(
      Variable = c(input$x_col, input$y_col),
      Mean = c(round(mean(x), 6), round(mean(y), 6)),
      `Std Dev` = c(round(sd(x), 6), round(sd(y), 6))
    )
  },digits=6)

    output$anova_summary <- renderTable({
      req(selected_model())
        
      
      model <- selected_model()$model[[1]]
      aov_table <- anova(model)
      
      tibble::tibble(
        Source = rownames(aov_table),
        DF = aov_table[["Df"]],
        `Sum of Squares` = round(aov_table[["Sum Sq"]], 6),
        `Mean Square` = round(aov_table[["Mean Sq"]], 6),
        `F Ratio` = round(aov_table[["F value"]], 3),
        `Prob > F` = format.pval(aov_table[["Pr(>F)"]], digits = 4, eps = .0001)
      )
    })
  output$coef_summary <- renderTable({
    req(selected_model())
      
    
    model <- selected_model()$model[[1]]
    smry <- summary(model)$coefficients
    
    tibble::tibble(
      Term = rownames(smry),
      Estimate = round(smry[, "Estimate"], 6),
      `Std Error` = round(smry[, "Std. Error"], 6),
      `t Ratio` = round(smry[, "t value"], 3),
      `Prob > |t|` = format.pval(smry[, "Pr(>|t|)"], digits = 4, eps = .0001)
    )
  })
  output$lack_of_fit <- renderTable({
    df <- selected_model()$data[[1]]
    x <- df[[input$x_col]]
    y <- df[[input$y_col]]
    
    if (length(unique(x)) == length(x)) {
      return(tibble::tibble(Result = "Cannot compute lack-of-fit: no replicated x values"))
    }
    
    full_model <- lm(y ~ as.factor(x), data = df)
    linear_model <- selected_model()$model[[1]]

    ss_total <- sum(resid(linear_model)^2)
    df_total <- df.residual(linear_model)
    
    ss_pure <- sum(resid(full_model)^2)
    df_pure <- df.residual(full_model)
    
    ss_lack <- ss_total - ss_pure
    df_lack <- df_total - df_pure
    
    ms_lack <- ss_lack / df_lack
    ms_pure <- ss_pure / df_pure
    f_value <- ms_lack / ms_pure
    p_value <- pf(f_value, df_lack, df_pure, lower.tail = FALSE)
    
    ss_total_corrected <- sum((y - mean(y))^2)
    r2_max <- 1 - (ss_pure / ss_total_corrected)
 
    tibble::tibble(
      Component = c("Lack of Fit", "Pure Error", "Total Error"),
      Df = c(df_lack, df_pure, df_total),
      `Sum of Squares` = round(c(ss_lack, ss_pure, ss_total), 4),
      `Mean Square` = c(round(ms_lack, 4), round(ms_pure, 4), ""),
      `F Value` = c(round(f_value, 3), "", ""),
      `Pr(>F)` = c(format.pval(p_value, digits = 4, eps = .0001), "", ""),
      `Max R-squared` = c("", "", round(r2_max, 4))
    )
  })

  output$lin_preview <- reactable::renderReactable({
    req(linearity_data())
    df <- as.data.frame(linearity_data())
    
    reactable::reactable(
      df,
      pagination = FALSE,
      searchable = TRUE,
      filterable = FALSE,
      striped = TRUE,
      bordered = TRUE,
      highlight = TRUE,
      defaultColDef = reactable::colDef(
        align = "center",
        style = function(value) {
          if (is.na(value)) {
            list(background = "#f8d7da", color = "#721c24", fontWeight = "bold")
          } else {
            list()
          }
        }
      ),
      style = list(height = "500px", overflowY = "scroll")
    )
  })
  
  output$residual_by_predicted <- renderPlot({
    method <- selected_model()$method[[1]]
    df <- selected_model()$data[[1]]
    x_col <- input$x_col
    y_col <- input$y_col
    info <- ols_data()
    
    intercept <- info$intercept$est
    slope <- info$slope$est
    df$Predicted <- intercept + slope * df[[x_col]]
    df$Residual <- df[[y_col]] - df$Predicted
    
    ggplot(df, aes(x = Predicted, y = Residual)) +
      geom_point(size = 2) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "blue") +
      labs(title = "Residual by Predicted Plot", x = "Predicted", y = "Residual") +
      theme_minimal()
  })
  
  output$actual_by_predicted <- renderPlot({
    method <- selected_model()$method[[1]]
   
    
    df <- selected_model()$data[[1]]
    x_col <- input$x_col
    y_col <- input$y_col
    info <-  ols_data()
    
    intercept <- info$intercept$est
    slope <- info$slope$est
    df$Predicted <- intercept + slope * df[[x_col]]
    
    ggplot(df, aes(x = Predicted, y = .data[[y_col]])) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      geom_hline(yintercept = mean(df[[y_col]]), linetype = "dotted", color = "blue") +
      labs(title = "Actual by Predicted Plot", x = "Predicted", y = y_col) +
      theme_minimal()
  })
  
  output$residual_by_row <- renderPlot({
    method <- selected_model()$method[[1]]

    
    df <- selected_model()$data[[1]]
    x_col <- input$x_col
    y_col <- input$y_col
    info <-  ols_data()
    
    intercept <- info$intercept$est
    slope <- info$slope$est
    df$Predicted <- intercept + slope * df[[x_col]]
    df$Residual <- df[[y_col]] - df$Predicted
    df$Row <- seq_len(nrow(df))
    
    ggplot(df, aes(x = Row, y = Residual)) +
      geom_point() +
      geom_line(color = "red") +
      geom_hline(yintercept = 0, linetype = "dotted", color = "blue") +
      labs(title = "Residual by Row Plot", x = "Row Number", y = "Residual") +
      theme_minimal()
  })
  
  output$residual_by_x <- renderPlot({
    method <- selected_model()$method[[1]]
  
    
    df <- selected_model()$data[[1]]
    x_col <- input$x_col
    y_col <- input$y_col
    info <- ols_data()
    
    intercept <- info$intercept$est
    slope <- info$slope$est
    df$Predicted <- intercept + slope * df[[x_col]]
    df$Residual <- df[[y_col]] - df$Predicted
    
    ggplot(df, aes_string(x = input$x_col, y = "Residual")) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dotted", color = "blue") +
      labs(title = "Residual by X Plot", x = input$x_col, y = "Residual") +
      theme_minimal()
  })
  
  output$residual_qqplot <- renderPlot({
    method <- selected_model()$method[[1]]
    
    
    df <- selected_model()$data[[1]]
    x_col <- input$x_col
    y_col <- input$y_col
    info <-  ols_data()
    
    intercept <- info$intercept$est
    slope <- info$slope$est
    df$Predicted <- intercept + slope * df[[x_col]]
    df$Residual <- df[[y_col]] - df$Predicted
    
    res <- df$Residual
    n <- length(res)
    blom_p <- (1:n - 3/8) / (n + 1/4)
    
    qq_data <- data.frame(
      quantile_prob = blom_p,
      residual = sort(res)
    )
    
    fit_line <- lm(residual ~ quantile_prob, data = qq_data)
    
    ggplot(qq_data, aes(x = quantile_prob, y = residual)) +
      geom_point() +
      geom_abline(
        intercept = coef(fit_line)[1],
        slope = coef(fit_line)[2],
        color = "red"
      ) +
      labs(
        title = "Residual Normal Quantile Plot",
        x = "Normal Quantile",
        y = "Expected Residual"
      ) +
      theme_minimal()
  })
  
  output$spec_id_ui <- renderUI({
    df <- linearity_data()
    selectInput("spec_id_col", "Spec ID:", 
                choices = names(df), 
                selected = names(df)[1])
  })
  
  output$assigned_ui <- renderUI({
    df <- linearity_data()
    selectInput("assigned_col", "Assigned:", 
                choices = names(df), 
                selected = names(df)[2])
  })
  
  output$rep_cols_ui <- renderUI({
    df <- linearity_data()
    selectInput("rep_cols", "Replicate Columns:",
                choices = names(df), 
                selected = names(df)[grep("Rep", names(df), ignore.case = TRUE)], 
                multiple = TRUE)
  })
  output$allowable_error_ui <- renderUI({
    tagList(
      numericInput("TEa_conc", label = "Conc", value = 6, min = 0),
      numericInput("TEa_pct", label = "Pct", value = 10, min = 0),
      numericInput("pct_nonlin", label = "% for Nonlinearity", value = 25, min = 0, max = 100)
    )
  })
  
  
  
  
  bartlett_test_cv <- function(df, value_col, group_col) {
    group_stats <- df %>%
      group_by(.data[[group_col]]) %>%
      summarise(
        Mean = mean(.data[[value_col]], na.rm = TRUE),
        SD = sd(.data[[value_col]], na.rm = TRUE),
        CV = ifelse(Mean == 0, NA, 100 * SD / Mean),
        n = n()
      ) %>%
      ungroup()
    k <- nrow(group_stats)
    n_vec <- group_stats$n
    cv_vec <- group_stats$CV
    log_cv <- log(cv_vec)
    weighted_mean_log_cv <- sum((n_vec - 1) * log_cv) / sum(n_vec - 1)
    numerator <- (sum(n_vec) - k) * log(sum((n_vec - 1) * cv_vec^2) / (sum(n_vec) - k)) -
      sum((n_vec - 1) * log(cv_vec^2))
    c_factor <- 1 + (1 / (3 * (k - 1))) * (sum(1 / (n_vec - 1)) - 1 / (sum(n_vec) - k))
    chi_sq <- numerator / c_factor
    df <- k - 1
    p_value <- 1 - pchisq(chi_sq, df)
    
    list(
      statistic = chi_sq,
      parameter = df,
      p.value = p_value,
      group_stats = group_stats
    )
  }
  
  ep6_summary <- reactive({
    df <- linearity_data()
    req(input$assigned_col, input$rep_cols,input$spec_id_col)
    assigned_var <- input$assigned_col
    spec_id  <- input$spec_id_col
    df_long <- tidyr::pivot_longer(df, cols = input$rep_cols, names_to = "Rep", values_to = "Value")
    ep6_summary <- df_long %>%
      group_by(.data[[assigned_var]]) %>%
      summarise(
        n = sum(!is.na(Value)),
        Mean = mean(Value, na.rm=TRUE),
        SD = sd(Value, na.rm=TRUE),
        CV = ifelse(Mean == 0, NA, 100 * SD / Mean),
        Raw_Values = list(Value),
        Specimen = first(.data[[spec_id]]) 
      ) %>% ungroup()
    print(df_long)
    
    
    bartlett_res <- tryCatch(
      bartlett.test(df_long$Value, as.factor(df_long[[assigned_var]])),
      error = function(e) NULL
    )
    result_cv <- bartlett_test_cv(df_long, "Value", "Specimen")
    bartlett_p <- if(!is.null(bartlett_res)) bartlett_res$p.value else NA
    bartlett_txt <- if(is.null(bartlett_res)) "Bartlett's test: error" else 
      paste0("Bartlett's p = ", signif(bartlett_p, 4), 
             ifelse(bartlett_p > 0.05, " (homoskedastic, OLS)", " (heteroskedastic, WLS)"))
    
    is_homoskedastic <- !is.na(bartlett_p) && (bartlett_p > 0.05)
    print(paste0("Bartlett's test p-value: ", bartlett_p))
    results <- list(
      bartlett_sd_p_value = bartlett_p,
      bartlett_cv_p_value = result_cv$p.value,
      mean_values = ep6_summary$Mean,
      sd_values = ep6_summary$SD,
      cv_values = ep6_summary$CV
    )
    mean_values_str <- paste(round(results$mean_values, 3), collapse = ", ")
    sd_values_str <- paste(round(results$sd_values, 3), collapse = ", ")
    cv_values_str <- paste(round(results$cv_values, 3), collapse = ", ")
    specimen_names <- paste(unique(df_long[[input$spec_id_col]]), collapse = ", ")
    assigned_vals <- paste(unique(df_long[[input$assigned_col]]), collapse = ", ")

    table_by_specimen <- ep6_summary %>%
      transmute(
        Specimen = .data[[input$spec_id_col]],
        Mean = as.character(round(Mean, 1)),
        SD   = as.character(round(SD, 1)),
        CV   = as.character(round(CV, 1))
      )

    bartlett_row <- data.frame(
      Specimen = "Bartlett's p",
      Mean = as.character(signif(results$bartlett_sd_p_value, 3)),
      SD   = as.character(signif(results$bartlett_cv_p_value, 3)),
      CV   = ""
    )
    
    equality_row <- data.frame(
      Specimen = "Accept equality hypothesis?",
      Mean = ifelse(results$bartlett_sd_p_value > 0.05, "Yes", "No"),
      SD   = ifelse(results$bartlett_cv_p_value > 0.05, "Yes", "No"),
      CV   = ""
    )

    results_table <- dplyr::bind_rows(
      table_by_specimen,
      bartlett_row,
      equality_row
    )

    if (!is_homoskedastic) {
      var_table <- df_long %>%
        group_by(.data[[assigned_var]]) %>%
        summarise(Variance = var(Value, na.rm = TRUE)) %>%
        ungroup()
      print(var_table)
      ep6_summary <- left_join(ep6_summary, var_table, by = assigned_var)
      ep6_summary <- ep6_summary %>%
        mutate(weights = 1 / Variance)
      reg_method <- "Weighted Regression"
    } else {
      ep6_summary$Variance <- NA
      ep6_summary$weights <- NA  
      reg_method <- "Ordinary Regression"
    }
    ep6_summary_long <- ep6_summary %>%
      unnest(cols = c(Raw_Values)) %>%
      rename(Value = Raw_Values)
    
    if (!is_homoskedastic) {
      ep6_summary_long <- left_join(
        ep6_summary_long,
        ep6_summary[, c(assigned_var, "weights")],
        by = assigned_var
      )
    }
    
    n_sample <- nrow(ep6_summary_long)
    poly_fits <- list()
    max_order <- -1
    for (deg in 1:3) {
      form_poly <- as.formula(
        paste0("Value ~ poly(as.numeric(as.character(", assigned_var, ")), ", deg, ", raw=TRUE)")
      )
      if (!is_homoskedastic) {fit_poly <- lm(
        form_poly,
        data = ep6_summary_long,
        weights = weights.x
      )
      reg_method <- "Weighted Regression"
      } else {fit_poly <- lm(
        form_poly,
        data = ep6_summary_long
      )
      reg_method <- "OLS"
      }
      
      sm <- summary(fit_poly)
      coef_names <- rownames(sm$coefficients)
    
      df_residual <- n_sample - length(coef_names)
      poly_terms <- coef_names[grepl("^poly\\(", coef_names)]
    
      orders <- as.integer(sub(".*([0-9]+)$", "\\1", poly_terms))

      orders[is.na(orders)] <- 0
      print(orders)
        max_order_term_idx <- which.max(orders)

        highest_order_term <- poly_terms[max_order_term_idx]
        highest_order_t <- sm$coefficients[highest_order_term, "t value"]
        
      critical_t <- qt(0.975, df_residual)
      
      if (!is.na(highest_order_t) && abs(highest_order_t) > critical_t) {
        if (!is.na(max_order) && orders[max_order_term_idx] > max_order) {
          max_order <- orders[max_order_term_idx]
          best_fit_deg <- deg
        } else if (max_order == -1) {
          max_order <- orders[max_order_term_idx]
          best_fit_deg <- deg
        }
      }
      poly_fits[[deg]] <- list(
        coefs = coef(fit_poly),
        se = sm$coefficients[, "Std. Error"],
        t = sm$coefficients[, "t value"],
        fit = fit_poly,
        rmse = sm$sigma,
        regression_method = reg_method 
      )
      cat("Best fit polynomial degree:", best_fit_deg, "\n")
    }
    
    make_poly_table <- function(poly_fits) {
      get_rmse_col <- function(len, rmse) c(rmse, rep(NA, len - 1))
      n_coef1 <- length(poly_fits[[1]]$coefs)
      n_coef2 <- length(poly_fits[[2]]$coefs)
      n_coef3 <- length(poly_fits[[3]]$coefs)
      
      clean_coef_label <- function(lbl) {
        if (lbl == "(Intercept)") {
          return("Intercept")
        } else if (grepl("^poly\\(", lbl)) {
          if (!grepl("\\)[0-9]+$", lbl)) return("X")
          k <- sub(".*\\) *([0-9]+)$", "\\1", lbl)
          return(ifelse(k == "1", "X", paste0("X^", k)))
        } else {
          return(lbl)
        }
      }
      format_scientific <- function(x, digits = 2, sci_threshold = -3) {
        if (is.na(x)) return(NA)
        if (x == 0) return("0")
        sci <- formatC(x, format = "e", digits = digits)
        sci_exp <- as.integer(sub(".*e([+-]?\\d+)$", "\\1", sci))
        if (!is.na(sci_exp) && sci_exp <= sci_threshold) {
          return(sci)
        } else {
          return(formatC(x, format = "f", digits = digits))
        }
      }
      poly_labels <- c(
        rep("Line", n_coef1),
        rep("2nd Order", n_coef2),
        rep("3rd Order", n_coef3)
      )
      coef_labels <- c(
        sapply(names(poly_fits[[1]]$coefs), clean_coef_label),
        sapply(names(poly_fits[[2]]$coefs), clean_coef_label),
        sapply(names(poly_fits[[3]]$coefs), clean_coef_label)
      )
      rmse_col <- c(
        get_rmse_col(n_coef1, poly_fits[[1]]$rmse),
        get_rmse_col(n_coef2, poly_fits[[2]]$rmse),
        get_rmse_col(n_coef3, poly_fits[[3]]$rmse)
      )
      df_res1 <- poly_fits[[1]]$fit$df.residual
      df_res2 <- poly_fits[[2]]$fit$df.residual
      df_res3 <- poly_fits[[3]]$fit$df.residual
      crit1 <- qt(0.975, df_res1)
      crit2 <- qt(0.975, df_res2)
      crit3 <- qt(0.975, df_res3)
      critical_t_all <- c(rep(crit1, n_coef1), rep(crit2, n_coef2), rep(crit3, n_coef3))
      t_val_all <- c(poly_fits[[1]]$t, poly_fits[[2]]$t, poly_fits[[3]]$t)
      sig_col <- ifelse(abs(t_val_all) < critical_t_all, "red", "black")
      tab <- data.frame(
        Polynomial = poly_labels,
        Coefficient = coef_labels,
        Estimate = c(poly_fits[[1]]$coefs, poly_fits[[2]]$coefs, poly_fits[[3]]$coefs),
        RMSE = rmse_col,
        t.value = t_val_all,
        SigColor = sig_col
      )
      tab$Estimate <- sapply(tab$Estimate, format_scientific)
      tab$RMSE <- sapply(tab$RMSE, format_scientific)
      tab$t.value <- sapply(abs(tab$t.value), format_scientific)
      print(tab)
      colnames(tab) <- c("Polynomial", "Coefficeints", "Estimates", "RMSE","T Statistics", "SigColor")
      tab
    }
    reg_method_summary <- poly_fits[[length(poly_fits)]]$regression_method
    poly_fit_table <- make_poly_table(poly_fits)
    list(
      results_table= results_table,
      poly_fit=poly_fits,
      poly_fit_table = poly_fit_table,
      reg_method_summary = reg_method_summary,
      best_fit=best_fit_deg
    )
  })
  output$ep6_table <- DT::renderDataTable({
    tab <- ep6_summary()$poly_fit_table
    sigcolor_vec <- as.character(tab$SigColor)
    tab <- tab[, !(names(tab) == "SigColor")]
    tstat_col <- which(names(tab) == "T Statistics")
    tab[[tstat_col]] <- paste0(
      "<span style='color:", sigcolor_vec, "'>", tab[[tstat_col]], "</span>"
    )
    
    DT::datatable(
      tab,
      escape = FALSE,
      options = list(
        pageLength = 15
      )
    )
  })
  output$reg_method_text <- renderText({
    paste("Regression method used:",ep6_summary()$reg_method_summary)
  })
  
  output$results_table <- DT::renderDataTable({
    DT::datatable(
      ep6_summary()$results_table,
      rownames = FALSE,
      options = list(
        dom = 't',      
        paging = FALSE  
      )
    )
  })
  
  output$deviation_table <- DT::renderDataTable({
    ep6_res <- ep6_summary()
    assigned_var <- input$assigned_col
    spec_id_col <- input$spec_id_col
    
    poly_fits <- ep6_res$poly_fit
    best_fit_deg <- ep6_res$best_fit
    line_fit <- poly_fits[[1]]$fit
    poly_fit <- poly_fits[[best_fit_deg]]$fit
    df <- linearity_data()
    rep_cols <- input$rep_cols
    df_long <- tidyr::pivot_longer(df,
                                   cols = rep_cols,
                                   names_to = "Rep",
                                   values_to = "Value")
    summary_df <- df_long %>%
      group_by(
        Specimen = .data[[spec_id_col]],
        Assigned = .data[[assigned_var]]
      ) %>%
      summarise(
        Mean = mean(Value, na.rm = TRUE),
        .groups = "drop"
      )
    summary_df <- summary_df %>%
      mutate(
        Poly_Fit = predict(poly_fit, newdata = setNames(data.frame(Assigned), assigned_var)),
        Line_Fit = predict(line_fit, newdata = setNames(data.frame(Assigned), assigned_var)),
        Deviation = Poly_Fit - Line_Fit,
        Deviation_Percent = ifelse(Line_Fit == 0, NA, 100 * Deviation / Line_Fit)
      ) %>%
      mutate(
        Mean = round(Mean, 1),
        Poly_Fit = round(Poly_Fit, 1),
        Line_Fit = round(Line_Fit, 1),
        Deviation = round(Deviation, 1),
        Deviation_Percent = round(Deviation_Percent, 1)
      ) %>%
      rename(
        `Spec ID` = Specimen,
        `Assigned Value` = Assigned,
        `Deviation from Linearity` = Deviation,
        `Deviation Percent` = Deviation_Percent,
        `Poly Fit` = Poly_Fit,
        `Line Fit` = Line_Fit
      )
    
    DT::datatable(summary_df,
                  rownames = FALSE,
                  options = list(dom = 't', paging = FALSE))
  })
  
  
 
  output$ep6_linearity_plot <- renderPlot({
    df <- linearity_data()
    assigned_var <- input$assigned_col
    rep_cols <- input$rep_cols
    tab <- ep6_summary()
    best_fit <- tab$best_fit
    poly_fits <- tab$poly_fit
    
    df_long <- tidyr::pivot_longer(
      df,
      cols = rep_cols,
      names_to = "Rep",
      values_to = "Value"
    )
    
    xlim <- input$x_range
    x_seq <- seq(xlim[1], xlim[2], length.out = 100)
    newdata <- setNames(data.frame(x_seq), assigned_var)
    line_pred <- predict(poly_fits[[1]]$fit, newdata = newdata)
    poly_pred <- predict(poly_fits[[best_fit]]$fit, newdata = newdata)
    fit_df <- dplyr::bind_rows(
      data.frame(Assigned = x_seq, Fit = as.numeric(line_pred), Type = "Line Fit"),
      data.frame(Assigned = x_seq, Fit = as.numeric(poly_pred), Type = "Poly. Fit")
    )
    scatter_df <- df_long %>%
      dplyr::select(Assigned = all_of(assigned_var), Measured = Value) %>%
      dplyr::filter(Assigned >= xlim[1], Assigned <= xlim[2])
    
    ggplot(scatter_df, aes(x = Assigned, y = Measured)) +
      geom_point(color = "blue", size = 3, alpha = 0.8) +
      geom_line(data = fit_df %>% dplyr::filter(Type == "Line Fit"), aes(x = Assigned, y = Fit, linetype = Type), color = "black", size = 1) +
      geom_line(data = fit_df %>% dplyr::filter(Type == "Poly. Fit"), aes(x = Assigned, y = Fit, linetype = Type), color = "red", size = 1) +
      scale_linetype_manual(values = c("Line Fit" = "solid", "Poly. Fit" = "dashed")) +
      labs(
        title = "Scatter Plot",
        x = assigned_var,
        y = "Measured"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$ep6_residual_plot <- renderPlot({
    df <- linearity_data()
    assigned_var <- input$assigned_col
    rep_cols <- input$rep_cols
    tab <- ep6_summary()
    best_fit <- tab$best_fit
    poly_fits <- tab$poly_fit
    
    df_long <- tidyr::pivot_longer(
      df,
      cols = rep_cols,
      names_to = "Rep",
      values_to = "Value"
    )
    xlim <- input$x_range
    x_seq <- seq(xlim[1], xlim[2], length.out = 100)
    newdata <- setNames(data.frame(x_seq), assigned_var)
    
    line_pred <- predict(poly_fits[[1]]$fit, newdata = newdata)
    poly_pred <- predict(poly_fits[[best_fit]]$fit, newdata = newdata)
    
    line_fit_val <- predict(poly_fits[[1]]$fit, newdata = setNames(data.frame(df_long[[assigned_var]]), assigned_var))
    df_long <- df_long %>%
      mutate(
        Residual = Value - line_fit_val
      )
    mean_resid <- df_long %>%
      group_by(Assigned = .data[[assigned_var]]) %>%
      summarise(
        Mean_Residual = mean(Residual, na.rm = TRUE)
      )
    
    deviation_df <- data.frame(
      Assigned = x_seq,
      Deviation = poly_pred - line_pred
    )
    
    allowable_conc <- input$TEa_conc
    allowable_pct <- input$TEa_pct * x_seq / 100
    allowable <- pmax(allowable_conc, allowable_pct)
    allowable_val <- allowable * (input$pct_nonlin/100)
    allowable_type <- ifelse(allowable_conc >= allowable_pct, "Conc", "Pct")
    allowable_df <- data.frame(
      Assigned = x_seq,
      upper = allowable_val,
      lower = -allowable_val,
      Type = allowable_type   
    )
    
    # 범례용 가상 데이터
    legend_df <- data.frame(
      x = c(1, 1, 1, 1),
      y = c(1, 2, 3, 4),
      Group = factor(c("Deviation from Linearity", "Allowable Nonlinearity", "Means", "Results"),
                     levels = c("Deviation from Linearity", "Allowable Nonlinearity", "Means", "Results"))
    )
    
    ggplot() +
      geom_ribbon(
        data = deviation_df,
        aes(x = Assigned, ymin = Deviation, ymax = 0),
        fill = "yellow", alpha = 0.3,
        show.legend = FALSE
      ) +
      geom_ribbon(
        data = deviation_df,
        aes(x = Assigned, ymin = 0, ymax = Deviation),
        fill = "yellow", alpha = 0.3,
        show.legend = FALSE
      ) +
      geom_line(
        data = allowable_df,
        aes(x = Assigned, y = upper),
        linetype = "solid", color = "grey30", size = 1,
        show.legend = FALSE
      ) +
      geom_line(
        data = allowable_df,
        aes(x = Assigned, y = lower),
        linetype = "solid", color = "grey30", size = 1,
        show.legend = FALSE
      ) +
      
      geom_point(
        data = df_long,
        aes(x = .data[[assigned_var]], y = Residual),
        shape = 1, color = "black", fill = "white", size = 3, stroke = 1.2,
        show.legend = FALSE
      ) +
      geom_point(
        data = mean_resid,
        aes(x = Assigned, y = Mean_Residual),
        shape = 22, fill = "blue", color = "black", size = 4, stroke = 1.5,
        show.legend = FALSE
      ) +
      geom_line(data = legend_df[legend_df$Group == "Allowable Nonlinearity", ],
                aes(x = x, y = y, color = Group, linetype = Group), size = 1) +
      geom_point(data = legend_df[legend_df$Group == "Means", ],
                 aes(x = x, y = y, color = Group, shape = Group), size = 4) +
      geom_point(data = legend_df[legend_df$Group == "Results", ],
                 aes(x = x, y = y, color = Group, shape = Group), size = 4, fill = "white") +
      geom_ribbon(data = legend_df[legend_df$Group == "Deviation from Linearity", ],
                  aes(x = x, ymin = 0, ymax = 0.5, fill = Group), alpha = 0.3) +
      
      scale_fill_manual(
        name = NULL,
        values = c("Deviation from Linearity" = "yellow")
      ) +
      scale_color_manual(
        name = NULL,
        values = c("Allowable Nonlinearity" = "grey30", "Means" = "blue", "Results" = "black")
      ) +
      scale_shape_manual(
        name = NULL,
        values = c("Means" = 22, "Results" = 1)
      ) +
      scale_linetype_manual(
        name = NULL,
        values = c("Allowable Nonlinearity" = "dotted")
      ) +
      labs(
        title = "Residual Plot",
        x = paste0(assigned_var),
        y = "Residual"
      ) +
      guides(
        fill = guide_legend(order = 1),
        color = guide_legend(order = 2),
        shape = "none",
        linetype = "none"  # linetype 범례 숨기기
      )+
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  

  # === =============4. precision ===
  outlier_values <- reactive({
    req(input$outlier_file, input$selected_column)
    ext <- tools::file_ext(input$outlier_file$name)
    
    df <- switch(ext,
                 csv = read.csv(input$outlier_file$datapath),
                 xlsx = readxl::read_excel(input$outlier_file$datapath),
                 xls  = readxl::read_excel(input$outlier_file$datapath),
                 validate("Unsupported file type.")
    )
    
    col_vals <- df[[input$selected_column]]
    validate(need(is.numeric(col_vals), "Selected column must be numeric"))
    na.omit(col_vals)
  })
  
  output$outlier_column_ui <- renderUI({
    req(input$outlier_file)
    ext <- tools::file_ext(input$outlier_file$name)
    
    df <- switch(ext,
                 csv = read.csv(input$outlier_file$datapath),
                 xlsx = readxl::read_excel(input$outlier_file$datapath),
                 xls  = readxl::read_excel(input$outlier_file$datapath),
                 validate("Unsupported file type.")
    )
    
    selectInput("selected_column", "Select column for analysis", choices = names(df))
  })
  
  preliminary_sd_val <- reactiveVal()
  
  outlier_table <- reactive({
    vals <- outlier_values()
    n <- length(vals)
    m <- mean(vals)
    s <- sd(vals)
    preliminary_sd_val(s) 
    cv <- if (m == 0) NA else round((s / m) * 100, 1)
    
    mat <- matrix(NA, ncol = 4, nrow = ceiling(length(vals) / 4))
    mat[1:length(vals)] <- vals
    
    stat_row <- c(
      paste0("Mean: ", round(m, 1)),
      paste0("SD: ", round(s, 1)),
      paste0("CV: ", paste0(cv, "%")),
      paste0("N: ", n)
    )
    
    final_table <- rbind(mat, stat_row)
    df <- as.data.frame(final_table)
    colnames(df) <- rep("", ncol(df)) 
    
    df
  })
  
 
 max_diff_val <- reactiveVal(NA)
 
 output$max_diff_text <- renderUI({
   s <- preliminary_sd_val()
   
   if (input$outlier_mode == "Manual Entry") {
     s <- input$manual_sd
   }
   
   req(s, input$sd_multiplier)
   
   s <- round(s, 1)  
   
   max_diff <- round(s * input$sd_multiplier, 2)
   max_diff_val(max_diff)
   HTML(paste0(
     '<div style="background-color:#f0f0f0; padding:10px; border-radius:5px; font-size:16px; font-weight:bold; color:#333;">
      Max difference between allowable replicates: ', max_diff, '
    </div>'
   ))
 })

  output$outlier_table <- renderTable({
    outlier_table()
  }, striped = TRUE, bordered = TRUE, spacing = "xs")
  
  precision_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    
    if (ext == "csv") {
      readr::read_csv(input$file$datapath)
    } else if (ext %in% c("xls", "xlsx")) {
      readxl::read_excel(input$file$datapath, sheet = 1)
    } else {
      validate("Unsupported file type. Please upload a .csv, .xls, or .xlsx file.")
    }
  })
  
  tagged_precision_data <- reactive({
    df <- precision_data()
    req(df, input$response, input$run, input$day)
    max_diff <- max_diff_val()
    
    if (is.null(max_diff) || is.na(max_diff) || input$outlier_mode == "No Outlier Rejection") {
      return(
        df %>%
          dplyr::mutate(OutlierStatus = "Accepted")
      )
    }
    run_groups <- df %>%
      dplyr::group_by(.data[[input$day]], .data[[input$run]]) %>%
      tidyr::nest()
    run_groups <- run_groups %>%
      dplyr::mutate(
        is_outlier = purrr::map_lgl(data, function(sub_df) {
          x <- na.omit(sub_df[[input$response]])
          if (length(x) < 2) return(FALSE)
          
          combos <- combn(x, 2, simplify = FALSE)
          for (pair in combos) {
            diff_val <- abs(pair[1] - pair[2])
            if (diff_val > max_diff) {
              return(TRUE)
            }
          }
          return(FALSE)
        })
      )
    
    outlier_keys <- run_groups %>%
      dplyr::filter(is_outlier) %>%
      dplyr::select(.data[[input$day]], .data[[input$run]])
    df %>%
      dplyr::mutate(
        OutlierStatus = ifelse(
          paste0(.data[[input$day]], "_", .data[[input$run]]) %in%
            paste0(outlier_keys[[input$day]], "_", outlier_keys[[input$run]]),
          "Rejected", "Accepted"
        )
      )
  })

  outlier_precision_data <- reactive({
    tagged_df <- tagged_precision_data()
    
    rejected_days <- tagged_df %>%
      dplyr::filter(OutlierStatus == "Rejected") %>%
      dplyr::pull(.data[[input$day]]) %>%
      unique()
    tagged_df %>%
      dplyr::filter(!.data[[input$day]] %in% rejected_days)
  })

  output$var_select_ui <- renderUI({
    req(precision_data())

    
    cols <- names(precision_data())
    
    common_inputs <- list(
      selectInput("response", "Response variable", choices = cols),
      selectInput("day", "Day factor column", choices = cols),
      selectInput("run", "Run factor column", choices = cols),
      numericInput("claim_within_sd", "Claim SD (Within-run)", value = 0.2, step = 0.01),
      numericInput("claim_total_sd", "Claim SD (Total)", value = 0.4, step = 0.01),
      numericInput("tea_conc", "Allowable TEa (Conc-based, same unit)", value = NA, step = 0.01),
      numericInput("tea_pct", "Allowable TEa (% of Mean)", value = 10, step = 0.1),
      numericInput("imp_sd", "% for Random Error (SD)", value = NA, step = 0.01)
                                     
    )
    
    tagList(common_inputs)
  })

  
  output$precision_preview <- reactable::renderReactable({
    df <- as.data.frame(precision_data())
    
    reactable::reactable(
      df,
      pagination = FALSE,
      searchable = TRUE,
      filterable = FALSE,
      striped = TRUE,
      bordered = TRUE,
      highlight = TRUE,
      defaultColDef = reactable::colDef(
        align = "center",
        style = function(value) {
          if (is.na(value)) {
            list(background = "#f8d7da", color = "#721c24", fontWeight = "bold")
          } else {
            list()
          }
        }
      ),
      style = list(height = "500px", overflowY = "scroll")
    )
  })
  
  output$precision_tagged_preview <- reactable::renderReactable({
    tagged_df <- tryCatch({
      df <- as.data.frame(tagged_precision_data())
      attr(df, "valid") <- TRUE
      df
    }, error = function(e) {
      df_err <- data.frame(Error = paste("tagged_precision_data() failed:", e$message))
      attr(df_err, "valid") <- FALSE
      df_err
    })
    valid <- isTRUE(attr(tagged_df, "valid")) && "OutlierStatus" %in% names(tagged_df) && nrow(tagged_df) > 0
    
    if (!valid) {
      if (!"Error" %in% names(tagged_df)) {
        tagged_df <- data.frame(Warning = "tagged_precision_data() doesn't have OutlierStatus.")
      }
      return(reactable::reactable(
        tagged_df,
        bordered = TRUE,
        highlight = TRUE,
        defaultColDef = reactable::colDef(
          align = "left",
          style = list(background = "#f8d7da", color = "#721c24", fontWeight = "bold")
        )
      ))
    }

    reactable::reactable(
      tagged_df,
      pagination = FALSE,
      searchable = TRUE,
      filterable = FALSE,
      striped = TRUE,
      bordered = TRUE,
      highlight = TRUE,
      defaultColDef = reactable::colDef(
        align = "center",
        style = function(value, index, name) {
          row_status <- tagged_df[index, "OutlierStatus"]
          if (is.na(value)) {
            list(background = "#f8d7da", color = "#721c24", fontWeight = "bold")
          } else if (!is.na(row_status) && row_status == "Rejected") {
            list(background = "#fff3cd", color = "#856404", fontWeight = "bold")
          } else {
            list()
          }
        }
      ),
      style = list(height = "500px", overflowY = "scroll")
    )
  })
  
  

  grand_mean_val <- reactiveVal(NULL)
    
  result_df <- eventReactive(input$run_precision, {
    req(input$run_precision)
    df <- outlier_precision_data()
    req(input$response, input$day, input$run)
    req(input$response != input$day, input$response != input$run, input$day != input$run)
    df <- df %>%
      dplyr::rename(
        value = !!sym(input$response),
        day = !!sym(input$day),
        run = !!sym(input$run)
      ) %>%
      dplyr::mutate(
        day = as.factor(day),
        run = as.factor(run)
      ) %>%
      group_by(day, run) %>%
      mutate(rep = row_number()) %>%
      ungroup() %>%
      mutate(run_day = interaction(day, run)) %>%
      as.data.frame()
  
    fit <- anovaVCA(value ~ day/run_day, Data = df)
    print(fit)
    vca_table <- fit$aov.tab
    var_within <- vca_table["error", "VC"]
    var_run    <- vca_table["day:run_day", "VC"]
    var_day    <- vca_table["day", "VC"]
    var_total  <- vca_table["total", "VC"]
    sd_within <- vca_table["error", "SD"]
    cv_within <- vca_table["error", "CV[%]"]
    df_within <- fit$aov.tab["error", "DF"]
    df_total  <- fit$aov.tab["total", "DF"]
    grand_mean <- mean(df$value, na.rm = TRUE)
    grand_mean_val(round(grand_mean, 2))
    sds <- sqrt(c(var_within, var_run, var_day, var_total))
    cvs <- 100 * sds / grand_mean


    claim_sds <- c(input$claim_within_sd, NA, NA, input$claim_total_sd)
    
    vv_within <- if (!is.na(claim_sds[1])) {
      sqrt((claim_sds[1]^2) * qchisq(0.95, df_within) / df_within)
    } else { NA }
    
    vv_total <- if (!is.na(claim_sds[4])) {
      sqrt((claim_sds[4]^2) * qchisq(0.95, round(df_total,0)) / round(df_total,0))
    } else { NA }
    
    verif_vals <- c(
      if (!is.na(vv_within)) round(vv_within, 2) else "",
      "", "",
      if (!is.na(vv_total)) round(vv_total, 2) else ""
    )
    sds <- sqrt(c(var_within, var_run, var_day, var_total))
    
    pass_fail <- c(
      ifelse(!is.na(vv_within) && sds[1] <= vv_within, "Pass", "Fail"),
      "", "",
      ifelse(!is.na(vv_total) && sds[4] <= vv_total, "Pass", "Fail")
    )

    tea_conc_val <- input$tea_conc
    tea_pct_val <- input$tea_pct
    imp_sd_val <- input$imp_sd
    

    medical_sd <- if (!is.na(imp_sd_val)) {
      if (!is.na(tea_conc_val)) {
        imp_sd_val / tea_conc_val  
      } else {
        imp_sd_val / ((tea_pct_val/ grand_mean)*100)
      }
    } else {
      NA
    }
    vv_medical <- if (!is.na(medical_sd)) {
      sqrt((medical_sd^2) * qchisq(0.95, round(df_total,0)) / round(df_total,0))
    } else {
      NA
    }
    
    pass_medical <- if (!is.na(medical_sd)) {
      if (sqrt(var_total) <= medical_sd) "Pass" else "Fail"
    } else {
      ""
    }

    result_df <- data.frame(
      Component = c("Within Run", "Between Run", "Between Day", "Total"),
      Users_CV = round(cvs, 1),
      Users_SD = round(sds, 2),
      Claim_SD = ifelse(is.na(claim_sds), "", round(claim_sds, 3)),
      Verification_Value_95 = verif_vals,
      DF = c(round(df_within,0), "", "", round(df_total,0)),
      Pass_Fail = pass_fail
    )
    
    result_df <- rbind(
      result_df,
      data.frame(
        Component = "Medical",
        Users_CV = "",
        Users_SD = "",
        Claim_SD = round(medical_sd,2),
        Verification_Value_95 = round(vv_medical,2),
        DF = "",
        Pass_Fail = pass_medical
      )
    )
    
    result_df
  })
  
  output$squared_table <- renderTable({
    req(result_df())
  }, digits = 2)
  
  output$download_sqd_table_excel <- downloadHandler(
    filename = function() {
      paste0("squared_table_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(result_df(), file)
    }
  )
  
  output$download_sqd_table_pdf <- downloadHandler(
    filename = function() {
      paste0("squared_table_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      if (input$outlier_mode%in%c("Calculated", "Manual Entry")) {
        title_plot <- ggdraw() + 
          draw_label("Precision", fontface = "bold", size = 18, x = 0, hjust = 0) +
          theme(plot.margin = margin(10, 10, 0, 10))
        
        grand_mean_plot <- ggdraw() + 
          draw_label(paste0("Grand Mean: ", grand_mean_val()), fontface = "italic", size = 12, x = 0, hjust = 0) +
          theme(plot.margin = margin(0, 10, 10, 10))
        
        table_plot <- ggdraw() + draw_grob(tableGrob(result_df(), rows = NULL))
        
        outlier_title <- ggdraw() +
          draw_label("Outlier Table", fontface = "bold", size = 14, x = 0, hjust = 0) +
          theme(plot.margin = margin(10, 10, 0, 10))
        outlier_plot <- ggdraw() + draw_grob(tableGrob(outlier_table(), rows = NULL))
        
        max_diff_plot <- ggdraw() + 
          draw_label(paste0("Max Difference Threshold: ", max_diff_val()), fontface = "italic", size = 12, x = 0, hjust = 0) +
          theme(plot.margin = margin(0, 10, 10, 10))
        
        precision_title <- ggdraw() +
          draw_label("Precision Plot", fontface = "bold", size = 14, x = 0, hjust = 0) +
          theme(plot.margin = margin(10, 10, 0, 10))
        precision_plot <- plot_obj()
        
        plots <- list(
          title_plot,
          grand_mean_plot,
          table_plot,
          outlier_title,
          outlier_plot,
          max_diff_plot,
          precision_title,
          precision_plot
        )
        rel_heights <- c(0.08, 0.06, 0.4, 0.06, 0.25, 0.05, 0.07, 0.7)
        
      } else {
        title_plot <- ggdraw() + 
          draw_label("Precision", fontface = "bold", size = 18, x = 0, hjust = 0) +
          theme(plot.margin = margin(10, 10, 0, 10))
        
        grand_mean_plot <- ggdraw() + 
          draw_label(paste0("Grand Mean: ", grand_mean_val()), fontface = "italic", size = 12, x = 0, hjust = 0) +
          theme(plot.margin = margin(0, 10, 10, 10))
        
        table_plot <- ggdraw() + draw_grob(tableGrob(result_df(), rows = NULL))
        
        precision_title <- ggdraw() +
          draw_label("Precision Plot", fontface = "bold", size = 14, x = 0, hjust = 0) +
          theme(plot.margin = margin(10, 10, 0, 10))
        precision_plot <- plot_obj()
        
        plots <- list(
          title_plot,
          grand_mean_plot,
          table_plot,
          precision_title,
          precision_plot
        )
        rel_heights <- c(0.08, 0.06, 0.5, 0.08, 0.7)
      }
      
      final_plot <- cowplot::plot_grid(plotlist = plots, ncol = 1, rel_heights = rel_heights)
      ggsave(file, final_plot, width = 8.3, height = 11.7, units = "in", dpi = 300)
    }
  )

  output$grand_mean_text <- renderText({
    req(input$run_precision)
    paste("Grand Mean:", grand_mean_val())
  })
  
  plot_obj <- reactive({
    req(precision_data(), input$day, input$run, input$response)
    
    df <- tagged_precision_data()
    
    df <- df %>%
      dplyr::mutate(
        Day_raw = .[[input$day]],
        Run = .[[input$run]],
        Value = .[[input$response]]
      )
    df <- df %>%
      dplyr::mutate(
        Day = if (is.numeric(Day_raw)) {
          factor(Day_raw, levels = sort(unique(Day_raw)))
        } else {
          factor(Day_raw, levels = unique(Day_raw))
        }
      )
    rejected_days <- df %>%
      dplyr::filter(OutlierStatus == "Rejected") %>%
      dplyr::pull(Day_raw) %>%
      unique()
    df <- df %>%
      dplyr::mutate(
        OutlierStatus = ifelse(Day_raw %in% rejected_days, "Rejected", "Accepted")
      )
    df <- df %>%
      dplyr::mutate(
        GroupLabel = dplyr::case_when(
          OutlierStatus == "Rejected" ~ "Rejected",
          Run == 1 ~ "Run 1",
          Run == 2 ~ "Run 2",
          TRUE ~ "Other"
        ),
        GroupLabel = factor(GroupLabel, levels = c("Run 1", "Run 2", "Rejected"))
      )
    grand_mean <- mean(df$Value, na.rm = TRUE)
    var_total <- var(df$Value, na.rm = TRUE)
    df$SD_Index <- (df$Value - grand_mean) / sqrt(var_total)
    ggplot(df, aes(x = Day, y = SD_Index, color = GroupLabel, shape = GroupLabel)) +
      geom_point(size = 3) +
      scale_color_manual(values = c(
        "Run 1" = "blue",
        "Run 2" = "purple",
        "Rejected" = "red"
      )) +
      scale_shape_manual(values = c(
        "Run 1" = 16,  # ●
        "Run 2" = 16,  # ●
        "Rejected" = 4 # X
      )) +
      geom_hline(yintercept = 0, size = 1) +
      geom_hline(yintercept = c(-1, 1), linetype = "dashed") +
      geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
      coord_cartesian(ylim = c(-3, 3)) +
      labs(
        x = input$day,
        y = "SD Index",
        color = "Group",
        shape = "Group"
      ) +
      theme_minimal()
  })
  

  output$sd_index_plot <- renderPlot({
    req(input$run_precision)
    plot_obj()
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("sd_index_plot_", Sys.Date(), ".", input$plot_format)
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot = plot_obj(),
        device = input$plot_format,
        dpi = input$plot_dpi,
        width = 7, height = 5, units = "in"
      )
    }
  )

  # === =============6. ROC DATA REACTIVE ===
  
  roc_data <- reactive({
    req(input$roc_file)
    ext <- tools::file_ext(input$roc_file$datapath)
    if (ext == "csv") {
      readr::read_csv(input$roc_file$datapath)
    } else {
      readxl::read_excel(input$roc_file$datapath)
    }
  })
  
  output$roc_column_selector_single <- renderUI({
    req(roc_data())
    cols <- names(roc_data())
    tagList(
      selectInput("roc_outcome", "Outcome (0/1):", choices = cols),
      selectInput("roc_predictor", "Predictor Score:", choices = cols),
      numericInput("real_prev", "Real-World Prevalence (%)", value = 5, min = 0, max = 100, step = 0.1),
      textInput("label_0", "Label for 0", value = "Negative"),
      textInput("label_1", "Label for 1", value = "Positive"),
      radioButtons("ci_auc_method", "AUC Standard Error Method:",
                   choices = c( "DeLong" = "delong","Hanley & McNeil" = "hanley"),
                   selected = "delong", inline = TRUE)
    )
  })
  output$roc_column_selector_compare_independent <- renderUI({
    req(roc_data())
    cols <- names(roc_data())
    tagList(
      selectInput("roc_outcome", "Outcome (0/1):", choices = cols),
      selectInput("roc_predictor", "Predictor 1:", choices = cols),
      selectInput("stratify_by", "Grouping Variable (for Comparison):",
                  choices = c("None", cols), selected = "None"),
      textInput("label_0", "Label for 0", value = "Negative"),
      textInput("label_1", "Label for 1", value = "Positive"),
      radioButtons("ci_auc_method", "AUC Standard Error Method:",
                   choices = c( "DeLong" = "delong","Hanley & McNeil" = "hanley"),
                   selected = "delong", inline = TRUE)
    )
  })
  
  
  output$roc_column_selector_compare_dependent <- renderUI({
    req(roc_data())
    cols <- names(roc_data())
    tagList(
      selectInput("roc_outcome", "Outcome:", choices = cols),
      selectInput("roc_predictor", "Predictor 1:", choices = cols),
      selectInput("second_predictor", "Predictor 2:", choices = cols),
      textInput("label_0", "Label for 0", value = "Negative"),
      textInput("label_1", "Label for 1", value = "Positive"),
      radioButtons("ci_auc_method", "AUC Standard Error Method:",
                   choices = c( "DeLong" = "delong","Hanley & McNeil" = "hanley"),
                   selected = "delong", inline = TRUE)
    )
  })
  
  output$roc_column_selector_partial_single <- renderUI({
    req(roc_data())
    cols <- names(roc_data())
    tagList(
      selectInput("roc_outcome", "Outcome (0/1):", choices = cols),
      selectInput("roc_predictor", "Predictor Score:", choices = cols),
      selectInput("partial_auc_focus", "Partial AUC :", choices = c("specificity", "sensitivity")),
      fluidRow(
        column(6, numericInput("partial_from", "From", value = 0.9, min = 0, max = 1, step = 0.01)),
        column(6, numericInput("partial_to", "To", value = 1, min = 0, max = 1, step = 0.01))
      ),
      textInput("label_0", "Label for 0", value = "Negative"),
      textInput("label_1", "Label for 1", value = "Positive")
    )
  })

  output$roc_column_selector_partial_compare <- renderUI({
    req(roc_data())
    cols <- names(roc_data())
    tagList(
      selectInput("roc_outcome", "Outcome (0/1):", choices = cols),
      selectInput("roc_predictor", "Predictor 1:", choices = cols),
      selectInput("second_predictor", "Predictor 2:", choices = cols),
      
      selectInput("partial_auc_focus", "Partial AUC :", choices = c("specificity", "sensitivity")),
      fluidRow(
        column(6, numericInput("partial_from", "From", value = 0.9, min = 0, max = 1, step = 0.01)),
        column(6, numericInput("partial_to", "To", value = 1, min = 0, max = 1, step = 0.01))
      ),
      
      textInput("label_0", "Label for 0", value = "Negative"),
      textInput("label_1", "Label for 1", value = "Positive")
    )
  })
  
  
  roc_obj <- reactive({
    df <- roc_data()
    req(input$roc_outcome, input$roc_predictor)
    
    df[[input$roc_outcome]] <- as.numeric(df[[input$roc_outcome]])
    df[[input$roc_predictor]] <- as.numeric(df[[input$roc_predictor]])
    
    if (isTRUE(input$partial_auc)) {
      from <- input$partial_from
      to <- input$partial_to
      focus <- input$partial_auc_focus
      
      pROC::roc(
        response = df[[input$roc_outcome]],
        predictor = df[[input$roc_predictor]],
        ci = TRUE,
        partial.auc = c(from, to),
        partial.auc.focus = focus,
        partial.auc.correct = TRUE
      )
    } else {
      pROC::roc(
        response = df[[input$roc_outcome]],
        predictor = df[[input$roc_predictor]],
        ci = TRUE
      )
    }
  })
  
  roc_threshold_data_single <- reactive({
    method_label <- switch(input$ci_auc_method,
                           "delong" = "DeLong",
                           "hanley" = "Hanley & McNeil")
    
    df <- tryCatch(roc_data(), error = function(e) NULL)
    req(df, input$roc_outcome, input$roc_predictor)
    
    outcome <- input$roc_outcome
    predictor <- input$roc_predictor
    df[[outcome]] <- as.numeric(df[[outcome]])
    df[[predictor]] <- as.numeric(df[[predictor]])
    
    roc <- tryCatch(roc_obj(), error = function(e) NULL)
    if (is.null(roc)) return(NULL)
    
    total_n <- nrow(df)
    pos_n <- sum(df[[outcome]] == 1)
    neg_n <- sum(df[[outcome]] == 0)
    pos_pct <- round(100 * pos_n / total_n, 2)
    neg_pct <- round(100 * neg_n / total_n, 2)
    
    auc_val <- as.numeric(pROC::auc(roc))
    auc_se <- if (input$ci_auc_method == "hanley") hanley_se(roc) else delong_se(roc)
    auc_ci <- auc_binom_ci(auc_val, total_n)
    
    z_val <- (auc_val - 0.5) / auc_se
    p_val <- signif(2 * (1 - pnorm(abs(z_val))), 3)
    youden_j <- roc$sensitivities + roc$specificities - 1
    youden_idx <- which.max(youden_j)
    ideal_thr <- roc$thresholds[youden_idx]
    all_scores <- sort(unique(df[[input$roc_predictor]]), decreasing = TRUE)
    actual_thr <- all_scores[which.min(abs(all_scores - ideal_thr))]
    
 
    predicted <- ifelse(df[[input$roc_predictor]] > actual_thr, 1, 0)
    TP <- sum(predicted == 1 & df[[input$roc_outcome]] == 1)
    FP <- sum(predicted == 1 & df[[input$roc_outcome]] == 0)
    TN <- sum(predicted == 0 & df[[input$roc_outcome]] == 0)
    FN <- sum(predicted == 0 & df[[input$roc_outcome]] == 1)
    
    Se <- TP / (TP + FN)
    Sp <- TN / (TN + FP)
    J <- Se + Sp - 1
    
    
    custom_thresholds <- sort(unique(df[[input$roc_predictor]]), decreasing = TRUE)
    
    coords_tbl <- purrr::map_dfr(custom_thresholds, function(t) {
      predicted <- ifelse(df[[input$roc_predictor]] > t, 1, 0)  
      
      TP <- sum(predicted == 1 & df[[input$roc_outcome]] == 1)
      FP <- sum(predicted == 1 & df[[input$roc_outcome]] == 0)
      TN <- sum(predicted == 0 & df[[input$roc_outcome]] == 0)
      FN <- sum(predicted == 0 & df[[input$roc_outcome]] == 1)
      
      Se <- TP / (TP + FN)
      Sp <- TN / (TN + FP)
      FNR <- 1 - Se
      FPR <- 1 - Sp
      prev <- input$real_prev / 100
      expected_cost <- prev * FNR + (1 - prev) * FPR
      
      tibble(threshold = t, sensitivity = Se, specificity = Sp, expected_cost = expected_cost)
    })
    
    
    min_cost <- min(coords_tbl$expected_cost, na.rm = TRUE)
    
    cost_table <- coords_tbl %>%
      filter(abs(expected_cost - min_cost) < 1e-10) %>%
      arrange(threshold) %>%
      slice(1)
    
    list(
      sample_summary = tibble::tibble(
        `Total N` = total_n,
        `Positive` = pos_n,
        `Negative ` = neg_n,
        `% Positive` = pos_pct,
        `% Negative` = neg_pct
      ),
      auc_stats = tibble::tibble(
        AUC = auc_val,
        `SE ` = auc_se, 
        `95% CI ` = auc_ci,
        `z value` = z_val,
        `P-value` = format.pval(p_val, digits = 4)
      ),
      youden_table = tibble::tibble(
        `Youden J` = J,
        `Threshold` = actual_thr,
        Sensitivity = Se,
        Specificity = Sp
      ),
      cost_table = cost_table
    )
  })
  output$roc_thresh_1 <- renderTable({
    req(input$run_all_roc)
    df <- t(roc_threshold_data_single()$sample_summary)
    df <- data.frame(df)
    df <- cbind(rownames(df), df)
    colnames(df) <- NULL
    df
  }, digits = 4)
  
  output$roc_thresh_2 <- renderTable({
    req(input$run_all_roc)
    df <- t(roc_threshold_data_single()$auc_stats)
    df <- data.frame(df)
    df <- cbind(rownames(df), df)
    colnames(df) <- NULL
    df
  }, digits = 4)
  
  output$roc_thresh_3 <- renderTable({
    req(input$run_all_roc)
    df <- t(roc_threshold_data_single()$youden_table)
    df <- data.frame(df)
    df <- cbind(rownames(df), df)
    colnames(df) <- NULL
    df
  }, digits = 4)
  
  output$roc_thresh_4 <- renderTable({
    req(input$run_all_roc)
    req(!is.null(roc_threshold_data_single()$cost_table))
    df <- t(roc_threshold_data_single()$cost_table)
    df <- data.frame(df)
    df <- cbind(rownames(df), df)
    colnames(df) <- NULL
    df
  }, digits = 4)
  
  output$roc_plot_standard <- renderPlot({
    df <- roc_data()
    req(input$roc_outcome, input$roc_predictor)
    df[[input$roc_outcome]] <- as.numeric(df[[input$roc_outcome]])
    df[[input$roc_predictor]] <- as.numeric(df[[input$roc_predictor]])
    
    r <- roc(df[[input$roc_outcome]], df[[input$roc_predictor]])
    plot(r, main = "Standard ROC", legacy.axes = TRUE)
    legend("bottomright", legend = sprintf("AUC = %.3f", auc(r)), col = "black", lwd = 2)
  })
  output$download_roc <- downloadHandler(
    filename = function() {
      paste0("ROC_plot.", input$roc_format)
    },
    content = function(file) {
      df <- roc_data()
      req(input$roc_outcome, input$roc_predictor, input$roc_format, input$roc_dpi)
      df[[input$roc_outcome]] <- as.numeric(df[[input$roc_outcome]])
      df[[input$roc_predictor]] <- as.numeric(df[[input$roc_predictor]])
      
      r <- roc(df[[input$roc_outcome]], df[[input$roc_predictor]])
      switch(input$roc_format,
             "png"  = png(file, res = input$roc_dpi, width = 6, height = 6, units = "in"),
             "jpeg" = jpeg(file, res = input$roc_dpi, width = 6, height = 6, units = "in"),
             "tiff" = tiff(file, res = input$roc_dpi, width = 6, height = 6, units = "in")
      )
      
      plot(r, main = "Standard ROC", legacy.axes = TRUE)
      legend("bottomright", legend = sprintf("AUC = %.3f", auc(r)), col = "black", lwd = 2)
      
      dev.off()
    }
  )
  
  roc_compare_data_dependent <- reactive({
    method_label <- switch(input$ci_auc_method,
                           "delong" = "DeLong",
                           "hanley" = "Hanley & McNeil")
    
    df <- tryCatch(roc_data(), error = function(e) NULL)
    req(df, input$roc_outcome, input$roc_predictor, input$second_predictor)
    
    outcome <- input$roc_outcome
    pred1 <- input$roc_predictor
    pred2 <- input$second_predictor
    
    df[[outcome]] <- as.numeric(df[[outcome]])
    df[[pred1]] <- as.numeric(df[[pred1]])
    df[[pred2]] <- as.numeric(df[[pred2]])
    
    labels <- df[[outcome]]
    total_n <- nrow(df)
    pos_n <- sum(labels == 1)
    neg_n <- sum(labels == 0)
    
    roc1 <- pROC::roc(labels, df[[pred1]], ci = FALSE)
    roc2 <- pROC::roc(labels, df[[pred2]], ci = FALSE)
    
    auc1 <- as.numeric(pROC::auc(roc1))
    auc2 <- as.numeric(pROC::auc(roc2))
    diff_auc <- abs(auc1 - auc2)
    
    se_auc1 <- if (input$ci_auc_method == "hanley") hanley_se(roc1) else delong_se(roc1)
    se_auc2 <- if (input$ci_auc_method == "hanley") hanley_se(roc2) else delong_se(roc2)
    
    ci_auc1 <- auc_binom_ci(auc1, pos_n + neg_n)
    ci_auc2 <- auc_binom_ci(auc2, pos_n + neg_n)
    
    se_diff <- tryCatch({
      if (input$ci_auc_method == "hanley") {
        pos1 <- df[[pred1]][labels == 1]
        neg1 <- df[[pred1]][labels == 0]
        pos2 <- df[[pred2]][labels == 1]
        neg2 <- df[[pred2]][labels == 0]
        r_A <- suppressWarnings(cor(pos1, pos2, method = "kendall"))
        r_N <- suppressWarnings(cor(neg1, neg2, method = "kendall"))
        r <- mean(c(r_A, r_N), na.rm = TRUE)
        sqrt(se_auc1^2 + se_auc2^2 - 2 * r * se_auc1 * se_auc2)
      } else {
        test <- pROC::roc.test(roc1, roc2, method = "delong", paired = TRUE)
        abs(diff_auc / qnorm(1 - test$p.value / 2))
      }
    }, error = function(e) NA_real_)
    
    z_val <- diff_auc / se_diff
    p_val <- 2 * (1 - pnorm(abs(z_val)))
    ci_low <- round(diff_auc - 1.96 * se_diff, 5)
    ci_up <- round(diff_auc + 1.96 * se_diff, 5)
    
    list(
      sample_summary = tibble::tibble(
        `Total N` = total_n,
        `Positive ` = pos_n,
        `Negative ` = neg_n,
        `% Positive` = round(100 * pos_n / total_n, 2),
        `% Negative` = round(100 * neg_n / total_n, 2)
      ),
      auc_summary = tibble::tibble(
        Model = c("Model 1", "Model 2"),
        AUC = c(auc1,  auc2),
        SE = c(round(se_auc1, 5), round(se_auc2, 5)),
        `95% CI` = c(ci_auc1, ci_auc2)
      ),
      auc_comparison = tibble::tibble(
        Metric = c("Difference between AUCs", "Standard Error",
                   "95% Confidence Interval", "z statistic", "P-value"),
        Value = c(round(diff_auc, 5), round(se_diff, 5),
                  paste0(ci_low, " to ", ci_up), round(z_val, 4),
                  format.pval(p_val, digits = 4))
      ),
      method = method_label
    )
  })
  output$roc_dependent_table1 <- renderTable({
    req(input$run_all_roc)
    roc_compare_data_dependent()$sample_summary
  },digits=4)
  
  output$roc_dependent_table2 <- renderTable({
    req(input$run_all_roc)
    roc_compare_data_dependent()$auc_summary
  },digits=4)
  
  output$roc_dependent_table3<- renderTable({
    req(input$run_all_roc)
    roc_compare_data_dependent()$auc_comparison
  },digits=4)
  
 output$roc_plot_compare <- renderPlot({
  df <- roc_data()
  req(input$roc_outcome, input$roc_predictor, input$second_predictor)
  df[[input$roc_outcome]] <- as.numeric(df[[input$roc_outcome]])
  df[[input$roc_predictor]] <- as.numeric(df[[input$roc_predictor]])
  df[[input$second_predictor]] <- as.numeric(df[[input$second_predictor]])
  
  r1 <- roc(df[[input$roc_outcome]], df[[input$roc_predictor]])
  r2 <- roc(df[[input$roc_outcome]], df[[input$second_predictor]])
  
  plot(r1, col = "blue", main = "Compare ROC", legacy.axes = TRUE)
  lines(r2, col = "red")
  legend("bottomright", legend = c(
    sprintf("Model 1: AUC = %.3f", auc(r1)),
    sprintf("Model 2: AUC = %.3f", auc(r2))
  ), col = c("blue", "red"), lwd = 2)
})

  
  roc_compare_data_independent<- reactive({
    method_label <- switch(input$ci_auc_method,
                           "delong" = "DeLong",
                           "hanley" = "Hanley & McNeil")
    df <- tryCatch(roc_data(), error = function(e) NULL)
    req(df, input$roc_outcome, input$roc_predictor, input$stratify_by)
    
    strat <- input$stratify_by
    outcome <- input$roc_outcome
    pred <- input$roc_predictor
    
    df[[outcome]] <- as.numeric(df[[outcome]])
    df[[pred]] <- as.numeric(df[[pred]])
    df <- df[!is.na(df[[strat]]), ]
    
    roc_list <- df %>%
      group_by(.data[[strat]]) %>%
      group_map(~{
        group_label <- .y[[1]] 
        roc_obj <- tryCatch(pROC::roc(.x[[outcome]], .x[[pred]], ci = FALSE), error = function(e) NULL)
        if (is.null(roc_obj)) return(NULL)
        auc_val <- as.numeric(pROC::auc(roc_obj))
        se_val <- if (input$ci_auc_method == "hanley") hanley_se(roc_obj) else delong_se(roc_obj)
        ci <- auc_binom_ci(auc_val, length(.x[[outcome]]))
        list(group = group_label, roc = roc_obj, AUC = auc_val, SE = se_val, CI = ci)
      })
    sample_summary <- tibble::tibble(
      `Total N` = nrow(df),
      `Positive ` = sum(df[[outcome]] == 1, na.rm = TRUE),
      `Negative` = sum(df[[outcome]] == 0, na.rm = TRUE),
      `% Positive` = round(100 * mean(df[[outcome]] == 1, na.rm = TRUE), 2),
      `% Negative` = round(100 * mean(df[[outcome]] == 0, na.rm = TRUE), 2)
    )
    roc_summary <- purrr::map_dfr(roc_list, ~{
      grp <- .x$group
      if (is.null(grp) || is.na(grp)) message("⚠️ Group is NA or NULL!")
      tibble::tibble(
        Group = grp,
        AUC = round(.x$AUC, 4),
        SE = round(.x$SE, 5),
        CI = .x$CI
      )
    })
    
    pairs <- combn(seq_along(roc_list), 2, simplify = FALSE)
    pair_comp <- purrr::map_dfr(pairs, ~{
      roc1 <- roc_list[[.x[1]]]
      roc2 <- roc_list[[.x[2]]]
      label1 <- roc1$group
      label2 <- roc2$group
      auc_diff <- abs(roc1$AUC - roc2$AUC)
      se1 <- roc1$SE
      se2 <- roc2$SE
      se_diff <- sqrt(se1^2 + se2^2)
      z_stat <- auc_diff / se_diff
      p_val <- 2 * (1 - pnorm(abs(z_stat)))
      ci_low <- round(auc_diff - 1.96 * se_diff, 5)
      ci_up <- round(auc_diff + 1.96 * se_diff, 5)
      tibble::tibble(
        Group1 = label1,
        Group2 = label2,
        `Difference Between Areas` = auc_diff,
        `SE of Diff` = round(se_diff, 5),
        `95% CI` = paste0(ci_low, " to ", ci_up),
        `Z-statistic` = round(z_stat, 4),
        `P-value` = format.pval(p_val, digits = 4)
      )
    })
    
    list(sample_summary = sample_summary,roc_summary = roc_summary, pairwise = pair_comp, method = method_label)
  })
  output$roc_compare_independent_table1 <- renderTable({
    req(input$run_all_roc)
    roc_compare_data_independent()$sample_summary
  },digits=4)
  output$roc_compare_independent_table2 <- renderTable({
    req(input$run_all_roc)
    roc_compare_data_independent()$roc_summary
  },digits=4)
  
  output$roc_compare_independent_table3 <- renderTable({
    req(input$run_all_roc)
    roc_compare_data_independent()$pairwise
  },digits=4)
  output$roc_plot_stratified <- renderPlot({
    df <- roc_data()
    req(input$roc_outcome, input$roc_predictor, input$stratify_by)
    df[[input$roc_outcome]] <- as.numeric(df[[input$roc_outcome]])
    df[[input$roc_predictor]] <- as.numeric(df[[input$roc_predictor]])
    df[[input$stratify_by]] <- as.factor(df[[input$stratify_by]])
    
    groups <- levels(df[[input$stratify_by]])
    colors <- rainbow(length(groups))
    
    plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "1 - Specificity", ylab = "Sensitivity", main = "Stratified ROC")
    for (i in seq_along(groups)) {
      g <- groups[i]
      d <- df[df[[input$stratify_by]] == g, ]
      if (nrow(d) < 10) next
      r <- roc(d[[input$roc_outcome]], d[[input$roc_predictor]])
      lines(r, col = colors[i], lwd = 2)
    }
    legend("bottomright", legend = sapply(seq_along(groups), function(i) {
      d <- df[df[[input$stratify_by]] == groups[i], ]
      r <- roc(d[[input$roc_outcome]], d[[input$roc_predictor]])
      sprintf("%s: AUC = %.3f", groups[i], auc(r))
    }), col = colors, lwd = 2)
  })
  
  
  roc_partial_data <- reactive({
    showNotification("Bootstrapping in progress,please wait a few seconds.", type = "warning", duration = 5)
    df <- tryCatch(roc_data(), error = function(e) NULL)
    req(df, input$roc_outcome, input$roc_predictor)
    
    df[[input$roc_outcome]] <- as.numeric(df[[input$roc_outcome]])
    df[[input$roc_predictor]] <- as.numeric(df[[input$roc_predictor]])
    
    y <- df[[input$roc_outcome]]
    score <- df[[input$roc_predictor]]
    
    total_n <- length(y)
    pos_n <- sum(y == 1)
    neg_n <- sum(y == 0)
    
    roc_obj <- tryCatch(pROC::roc(y, score, ci = FALSE), error = function(e) NULL)
    req(roc_obj)
    
    auc_full <- as.numeric(pROC::auc(roc_obj))
    
    focus <- input$partial_auc_focus
    lower <- input$partial_from
    upper <- input$partial_to
    
    df_boot <- tibble(y = y, score = score)
    
    calc_auc_vals <- function(data, indices) {
      d <- data[indices, ]
      roc_obj <- tryCatch(pROC::roc(d$y, d$score, ci = FALSE), error = function(e) return(c(NA, NA)))
      if (is.null(roc_obj)) return(c(NA, NA))
      pauc <- as.numeric(pROC::auc(roc_obj, partial.auc = c(lower, upper), partial.auc.focus = focus, partial.auc.correct = FALSE))
      std <- as.numeric(pROC::auc(roc_obj, partial.auc = c(lower, upper), partial.auc.focus = focus, partial.auc.correct = TRUE))
      c(pauc, std)
    }
    
    set.seed(123)
    b <- boot(data = df_boot, statistic = calc_auc_vals, R = 2000)
    
    pAUC <- b$t0[1]
    standardized <- b$t0[2]
    
    ci_pauc <- boot.ci(b, type = "bca", index = 1)$bca[4:5]
    ci_std <- boot.ci(b, type = "bca", index = 2)$bca[4:5]
    
    list(
      sample_summary = tibble::tibble(
        `Total N` = total_n,
        `Positive (1)` = pos_n,
        `Negative (0)` = neg_n
      ),
      auc_summary = tibble::tibble(
        Metric = c("AUC (full)",
                   "Partial AUC",
                   "Standardized Partial AUC"),
        Value = c(round(auc_full, 4), round(pAUC, 4), round(standardized, 4)),
        `95% CI (BCa)` = c(
          "",  
          paste0("[", paste(round(ci_pauc, 4), collapse = ", "), "]"),
          paste0("[", paste(round(ci_std, 4), collapse = ", "), "]")
        )
      )
    )
  })
  output$roc_plot_partial_single <- renderPlot({
    df <- roc_data()
    req(input$roc_outcome, input$roc_predictor)
    df[[input$roc_outcome]] <- as.numeric(df[[input$roc_outcome]])
    df[[input$roc_predictor]] <- as.numeric(df[[input$roc_predictor]])
    
    r <- pROC::roc(df[[input$roc_outcome]], df[[input$roc_predictor]], quiet = TRUE)
    from <- input$partial_from
    to <- input$partial_to
    focus <- input$partial_auc_focus
    
    plot(r, main = sprintf("Partial ROC (%.2f–%.2f)", from, to),
         col = "black", lwd = 2, xlab = "1 - Specificity", ylab = "Sensitivity")
    
    coords_df <- coords(r, "all", ret = c("specificity", "sensitivity"), transpose = FALSE)
    coords_df <- coords_df %>% mutate(FPR = 1 - specificity)
    
    if (focus == "specificity") {
      shade_df <- coords_df %>% filter(specificity >= from & specificity <= to)
      x <-shade_df$specificity
      y <- shade_df$sensitivity
      if (length(x) > 1) {
        polygon(c(x, rev(x)), c(rep(0, length(x)), rev(y)),
                col = rgb(0.7, 0.7, 0.7, 0.5), border = NA)
      }
    } else {
      shade_df <- coords_df %>% filter(sensitivity >= from & sensitivity <= to)
      x <- shade_df$specificity
      y <- shade_df$sensitivity
      if (length(y) > 1) {
        polygon(c(rep(0, length(y)), rev(x)), c(y, rev(y)),
                col = rgb(0.7, 0.7, 0.7, 0.5), border = NA)
      }
    }
    
    auc_val <- pROC::auc(r, partial.auc = c(from, to), partial.auc.focus = focus)
    legend("bottomright", legend = sprintf("pAUC = %.3f", auc_val), col = "black", lwd = 2)
  })
  
  output$roc_partial_single_1 <- renderTable({
    req(input$run_all_roc)
    roc_partial_data()$sample_summary
  }, digits = 0)
  output$roc_partial_single_2 <- renderTable({
    req(input$run_all_roc)
    roc_partial_data()$auc_summary
  }, digits = 4)
  
  roc_partial_compare <- reactive({
    showNotification("Bootstrapping in progress, please wait a few seconds.", type = "warning", duration = 5)
    df <- tryCatch(roc_data(), error = function(e) NULL)
    req(df, input$roc_outcome, input$roc_predictor, input$second_predictor)
    
    df[[input$roc_outcome]] <- as.numeric(df[[input$roc_outcome]])
    model1 <- as.numeric(df[[input$roc_predictor]])
    model2 <- as.numeric(df[[input$second_predictor]])
    y <- df[[input$roc_outcome]]
    
    total_n <- length(y)
    pos_n <- sum(y == 1)
    neg_n <- sum(y == 0)
    
    aucs <- function(score, score_name) {
      df_boot <- tibble(y = y, score = score)
      focus <- input$partial_auc_focus
      lower <- input$partial_from
      upper <- input$partial_to
      
      calc_auc_vals <- function(data, indices) {
        d <- data[indices, ]
        roc_obj <- tryCatch(roc(d$y, d$score, ci = FALSE), error = function(e) return(c(NA, NA, NA)))
        if (is.null(roc_obj)) return(c(NA, NA, NA))
        full <- as.numeric(auc(roc_obj))
        pauc <- as.numeric(auc(roc_obj, partial.auc = c(lower, upper), partial.auc.focus = focus, partial.auc.correct = FALSE))
        std <- as.numeric(auc(roc_obj, partial.auc = c(lower, upper), partial.auc.focus = focus, partial.auc.correct = TRUE))
        c(full, pauc, std)
      }
      
      set.seed(123)
      b <- boot(data = df_boot, statistic = calc_auc_vals, R = 2000)
      
      
      ci_pauc <- boot.ci(b, type = "bca", index = 2)$bca[4:5]
      ci_std <- boot.ci(b, type = "bca", index = 3)$bca[4:5]
      
      list(
        full = b$t0[1], 
        pauc = b$t0[2], pauc_ci = ci_pauc,
        standardized = b$t0[3], standardized_ci = ci_std
      )
    }
    auc_diff_bca <- function(model1, model2, y, lower, upper, focus, R = 2000) {
      df_diff <- tibble(y = y, m1 = model1, m2 = model2)
      
      stat <- function(data, indices) {
        d <- data[indices, ]
        roc1 <- tryCatch(roc(d$y, d$m1), error = function(e) return(NA))
        roc2 <- tryCatch(roc(d$y, d$m2), error = function(e) return(NA))
        if (is.null(roc1) || is.null(roc2)) return(NA)
        
        auc1 <- tryCatch(auc(roc1, partial.auc = c(lower, upper), partial.auc.focus = focus, partial.auc.correct = FALSE), error = function(e) return(NA))
        auc2 <- tryCatch(auc(roc2, partial.auc = c(lower, upper), partial.auc.focus = focus, partial.auc.correct = FALSE), error = function(e) return(NA))
        auc1 - auc2
      }
      
      set.seed(43)
      b <- boot(data = df_diff, statistic = stat, R = 2000)
      
      ci_pauc <- boot.ci(b, type = "bca")$bca[4:5]
      
      list(
        pauc_diff = b$t0,
        pauc_ci = ci_pauc
      )
    }
    m1 <- aucs(model1)
    m2 <- aucs(model2)
    diff_ci <- auc_diff_bca(model1, model2, y,
                            lower = input$partial_from,
                            upper = input$partial_to,
                            focus = input$partial_auc_focus)
    list(
      sample_summary = tibble::tibble(
        `Total N` = total_n,
        `Positive (1)` = pos_n,
        `Negative (0)` = neg_n
      ),
      auc_by_model = tibble::tibble(
        Model = c("Model 1", "Model 2"),
        
        `AUC (full)` = c(m1$full, m2$full),
        
        
        `Partial AUC` = c(m1$pauc, m2$pauc),
        `Partial AUC CI` = c(
          paste0("[", paste(round(m1$pauc_ci, 3), collapse = ", "), "]"),
          paste0("[", paste(round(m2$pauc_ci, 3), collapse = ", "), "]")
        ),
        
        `Standardized Partial AUC` = c(m1$standardized, m2$standardized),
        `Standardized Partial AUC CI` = c(
          paste0("[", paste(round(m1$standardized_ci, 3), collapse = ", "), "]"),
          paste0("[", paste(round(m2$standardized_ci, 3), collapse = ", "), "]")
        )
      ) |> dplyr::mutate(across(where(is.numeric), round, 4)),
      
      auc_differences = tibble::tibble(
        Metric = "Partial AUC",
        `Difference between areas` = round(m1$pauc - m2$pauc, 4),
        `95% CI (BCa)` = paste0("[", paste(round(diff_ci$pauc_ci, 3), collapse = ", "), "]")
      )
      
    )})
  
  output$roc_partial_comp_1 <- renderTable({
    req(input$run_all_roc)
    roc_partial_compare()$sample_summary
  }, digits = 0)
  
  output$roc_partial_comp_2 <- renderTable({
    req(input$run_all_roc)
    roc_partial_compare()$auc_by_model
  }, digits = 4)
  
  output$roc_partial_comp_3 <- renderTable({
    req(input$run_all_roc)
    roc_partial_compare()$auc_differences
  }, digits = 4)
  
  output$roc_plot_partial_compare <- renderPlot({
    df <- roc_data()
    req(input$roc_outcome, input$roc_predictor, input$second_predictor)
    df[[input$roc_outcome]] <- as.numeric(df[[input$roc_outcome]])
    df[[input$roc_predictor]] <- as.numeric(df[[input$roc_predictor]])
    df[[input$second_predictor]] <- as.numeric(df[[input$second_predictor]])
    
    y <- df[[input$roc_outcome]]
    score1 <- df[[input$roc_predictor]]
    score2 <- df[[input$second_predictor]]
    
    from <- input$partial_from
    to <- input$partial_to
    focus <- input$partial_auc_focus
    
    roc1 <- tryCatch(pROC::roc(y, score1, ci = FALSE), error = function(e) NULL)
    roc2 <- tryCatch(pROC::roc(y, score2, ci = FALSE), error = function(e) NULL)
    req(roc1, roc2)
    
    plot(roc1, col = "blue", lwd = 2,
         partial.auc = c(from, to),
         partial.auc.focus = focus,
         main = sprintf("Partial ROC Comparison (%.2f–%.2f)", from, to),
         xlab = "1 - Specificity", ylab = "Sensitivity")
    lines(roc2, col = "red", lwd = 2,
          partial.auc = c(from, to),
          partial.auc.focus = focus)
    
    coords1 <- coords(roc1, "all", ret = c("specificity", "sensitivity"), transpose = FALSE) %>%
      mutate(FPR = 1 - specificity)
    coords2 <- coords(roc2, "all", ret = c("specificity", "sensitivity"), transpose = FALSE) %>%
      mutate(FPR = 1 - specificity)
    
    if (focus == "specificity") {
      shade1 <- coords1 %>% filter(specificity >= from & specificity <= to)
      shade2 <- coords2 %>% filter(specificity >= from & specificity <= to)
      if (nrow(shade1) > 1) {
        x1 <-  shade1$specificity
        y1 <- shade1$sensitivity
        polygon(c(x1, rev(x1)), c(rep(0, length(x1)), rev(y1)),
                col = rgb(0, 0, 1, 0.2), border = NA)
      }
      if (nrow(shade2) > 1) {
        x2 <-  shade2$specificity
        y2 <- shade2$sensitivity
        polygon(c(x2, rev(x2)), c(rep(0, length(x2)), rev(y2)),
                col = rgb(1, 0, 0, 0.2), border = NA)
      }
    } else {
      shade1 <- coords1 %>% filter(sensitivity >= from & sensitivity <= to)
      shade2 <- coords2 %>% filter(sensitivity >= from & sensitivity <= to)
      if (nrow(shade1) > 1) {
        x1 <- shade1$specificity
        y1 <- shade1$sensitivity
        polygon(c(rep(0, length(y1)), rev(x1)), c(y1, rev(y1)),
                col = rgb(0, 0, 1, 0.2), border = NA)
      }
      if (nrow(shade2) > 1) {
        x2 <- shade2$specificity
        y2 <- shade2$sensitivity
        polygon(c(rep(0, length(y2)), rev(x2)), c(y2, rev(y2)),
                col = rgb(1, 0, 0, 0.2), border = NA)
      }
    }
    
    auc1 <- round(pROC::auc(roc1, partial.auc = c(from, to), partial.auc.focus = focus), 3)
    auc2 <- round(pROC::auc(roc2, partial.auc = c(from, to), partial.auc.focus = focus), 3)
    
    legend("bottomright", legend = c(
      paste("Model 1: pAUC =", auc1),
      paste("Model 2: pAUC =", auc2)
    ), col = c("blue", "red"), lwd = 2)
  })
  
  
  
  
  output$roc_preview <- reactable::renderReactable({
    df <- as.data.frame(roc_data())
    
    reactable::reactable(
      df,
      pagination = FALSE,
      searchable = TRUE,
      filterable = FALSE,
      striped = TRUE,
      bordered = TRUE,
      highlight = TRUE,
      defaultColDef = reactable::colDef(
        align = "center",
        style = function(value) {
          if (is.na(value)) {
            list(background = "#f8d7da", color = "#721c24", fontWeight = "bold")
          } else {
            list()
          }
        }
      ),
      style = list(height = "500px", overflowY = "scroll")
    )
  })
  

  



  # ========== STABILITY MODULE ==========
  model_comparison <- reactiveVal()
  stability_data <- reactive({
    req(input$stability_file)
    ext <- tools::file_ext(input$stability_file$name)
    
    tryCatch({
      if (ext == "csv") {
        read.csv(input$stability_file$datapath)
      } else if (ext %in% c("xls", "xlsx")) {
        readxl::read_excel(input$stability_file$datapath)
      } else {
        showNotification("Unsupported file type. Use .csv or Excel.", type = "error")
        return(NULL)
      }
    }, error = function(e) {
      showNotification(paste("Error reading stability file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$stability_time_col_ui <- renderUI({
    req(stability_data())
    selectInput("stability_time_col", "Select Time Column:", choices = names(stability_data()))
  })
  
  output$stability_measurement_col_ui <- renderUI({
    req(stability_data())
    selectInput("stability_measurement_col", "Select Measurement Column:", choices = names(stability_data()))
  })
  
  output$stability_group_col_ui <- renderUI({
    req(stability_data())
    selectInput("stability_group_col", "Select Grouping Column (e.g. Sample/Batch):",
                choices = names(stability_data()), 
                selected = names(stability_data())[1])  
  })
  
  
  stability_model <- reactiveVal(NULL)
  run_snapshot <- reactiveVal(list())
  
  
  observeEvent(input$run_stability, {
    run_snapshot(list(
      time_scale = input$stability_time_scale,
      meas_scale = input$stability_measurement_scale,
      run_count = input$run_stability
    ))
  })
  
  
  observeEvent(input$run_stability, {

    if (is.null(input$stability_time_col) || input$stability_time_col == "" ||
        is.null(input$stability_measurement_col) || input$stability_measurement_col == "") {
      showNotification("Please select valid time and measurement columns.", type = "error")
      return(NULL)
    }
    
    df <- stability_data()
    req(df)
    snap <- run_snapshot()
    req(snap$time_scale == input$stability_time_scale &&
          snap$meas_scale == input$stability_measurement_scale)
    
    
    df <- df %>%
      rename(
        time_raw = !!sym(input$stability_time_col),
        meas = !!sym(input$stability_measurement_col)
      ) %>%
      mutate(
        time = case_when(
          inherits(time_raw, "Date") ~ as.numeric(difftime(time_raw, min(time_raw, na.rm = TRUE), units = "days")),
          inherits(time_raw, "POSIXct") ~ as.numeric(difftime(time_raw, min(time_raw, na.rm = TRUE), units = "days")),
          TRUE ~ suppressWarnings(as.numeric(time_raw))
        ),
        meas = suppressWarnings(as.numeric(meas))
      ) %>%
      filter(!is.na(time), !is.na(meas), time >= 0, meas > 0)
    
    
    df$meas_orig <- df$meas
    df$time_orig <- df$time

    
    if (!is.numeric(df$time) || !is.numeric(df$meas)) {
      showNotification("Time and Measurement must be numeric.", type = "error")
      return()
    }
    
    df <- df %>%
      mutate(Group = if (input$stability_group_col != "None" &&
                         input$stability_group_col %in% names(.)) {
        .data[[input$stability_group_col]]
      } else {
        "All"
      }) %>%
      mutate(Group = factor(Group))
    

    group_levels <- levels(df$Group)

    df <- df %>%
      mutate(
        time_trans = case_when(
          input$stability_time_scale == "log" & is.numeric(time) ~ log(pmax(time, 1e-6)),
          input$stability_time_scale == "square" & is.numeric(time) ~ time^2,
          TRUE ~ time
        ),
        meas_trans = case_when(
          input$stability_measurement_scale == "log" ~ log(pmax(meas, 1e-6)),
          input$stability_measurement_scale == "square" ~ meas^2,
          TRUE ~ meas
        )
      )
    

    model4_list <- df %>%
      group_by(Group) %>%
      group_split() %>%
      set_names(map_chr(., ~ as.character(unique(.x$Group)))) %>%
      map(~ lm(meas_trans ~ time_trans, data = .x))

      model_formulas <- list(
        model1 = meas_trans ~ Group * time_trans,
        model2 = meas_trans ~ Group + time_trans,
        model3 = meas_trans ~ time_trans
      )
      model_fits <- map(model_formulas, ~ lm(.x, data = df))
      model_fits$model4 <- model4_list
    
    
    model_summaries <- map2(names(model_fits), model_fits, function(name, model) {
      if (name == "model4") {
        map_dfr(names(model), function(g) {
          m <- model[[g]]
          s <- summary(m)
          tibble(
            Model = "model4",
            Group = g,
            nparm = length(coef(m)),
            df = s$df[2],
            mse = s$sigma^2,
            rsquare = s$r.squared,
            sse = sum(residuals(m)^2)
          )
        })
      } else {
        s <- summary(model)
        tibble(
          Model = name,
          Group = NA,  
          nparm = length(coef(model)),
          df = s$df[2],
          mse = s$sigma^2,
          rsquare = s$r.squared,
          sse = sum(residuals(model)^2)
        )
      }
    }) %>%
      bind_rows() %>%
      mutate(
        measurement_scale = input$stability_measurement_scale,
        time_scale = input$stability_time_scale
      )
    
    model_estimates <- map2(names(model_fits), model_fits, function(name, model) {
      if (name == "model4") {
        map_dfr(names(model), function(g) {
          broom::tidy(model[[g]]) %>%
            dplyr::mutate(
              Group = g,
              `t Ratio` = statistic,
              `Prob > |t|` = p.value
            ) %>%
            dplyr::rename(
              Parameter = term,
              Estimate = estimate,
              `Std Error` = std.error
            ) %>%
            dplyr::select(Group, Parameter, Estimate, `Std Error`, `t Ratio`, `Prob > |t|`)
        })
      } else {
        broom::tidy(model) %>%
          dplyr::mutate(
            Group = NA,
            `t Ratio` = statistic,
            `Prob > |t|` = p.value
          ) %>%
          dplyr::rename(
            Parameter = term,
            Estimate = estimate,
            `Std Error` = std.error
          ) %>%
          dplyr::select(Group, Parameter, Estimate, `Std Error`, `t Ratio`, `Prob > |t|`)
      }
    }) %>%
      set_names(names(model_fits))
    

    model_comparison <- reactive({
      m1 <- model_fits$model1
      m2 <- model_fits$model2
      m3 <- model_fits$model3
      str(m2) 
      model1_summary <- summary(m1)
      model2_summary <- summary(m2)
      model3_summary <- summary(m3)
      a_test <- anova(m3, m1)  
      b_test <- anova(m3, m2)  
      c_test <- anova(m2, m1)
      df1 <- summary(m1)$df[2]  
      df2 <- summary(m2)$df[2]
      df3 <- summary(m3)$df[2]
      rss1 <- sum(resid(m1)^2)
      rss2 <- sum(resid(m2)^2)
      rss3 <- sum(resid(m3)^2)
      ss_A <- rss3 - rss1  
      ss_B <- rss3 - rss2  
      ss_C <- rss2 - rss1  
      df_A <- df3 - df1
      df_B <- df3 - df2
      df_C <- df2 - df1
      ms_A <- ss_A / df_A
      ms_B <- ss_B / df_B
      ms_C <- ss_C / df_C
      mse1 <- rss1 / df1
      F_A <- ms_A / mse1
      F_B <- ms_B / mse1
      F_C <- ms_C / mse1
      p_A <- pf(F_A, df_A, df1, lower.tail = FALSE)
      p_B <- pf(F_B, df_B, df1, lower.tail = FALSE)
      p_C <- pf(F_C, df_C, df1, lower.tail = FALSE)
      df_D <- df1
      ss_D <- rss1
      ms_D <- mse1
      df_E <- model1_summary$df[1]
      ss_E <- sum((df$meas_orig)^2)-rss1
      ms_E <- ss_E / df_E
      tibble::tibble(
        Source = c("A", "B", "C", "D", "E"),
        DF = c(df_A, df_B, df_C, df_D, df_E),
        SS = c(ss_A, ss_B, ss_C, ss_D, ss_E),
        `Mean Square` = c(ms_A, ms_B, ms_C, ms_D, ms_E),
        `F Statistic` = c(F_A, F_B, F_C, NA, NA),
        `Prob > F` = c(
          format.pval(p_A, digits = 4, eps = 1e-4),
          format.pval(p_B, digits = 4, eps = 1e-4),
          format.pval(p_C, digits = 4, eps = 1e-4),
          NA, NA
        )
      )
    })
    legend_table <- tibble::tibble(
      Source = c("A", "B", "C", "D", "E"),
      `Intercept` = c("Different", "Different", "Different", "Residual", "Whole Model"),
      `Slope`     = c("Different", "Common",   "Different", "", ""),
      `Intercept ` = c("Common", "Common", "Different", "", ""),
      `Slope `     = c("Common", "Common", "Common", "", "")
    )
    stability_model(list(
      models = model_fits,
      model_summaries = model_summaries,
      model_estimates = model_estimates,
      group_levels = group_levels,
      model4_list = model4_list,
      model_comparison = model_comparison(),
      legend_table=legend_table
    ))

    sm <- stability_model()
    if (!is.null(sm) && "models" %in% names(sm)) {
      model4 <- sm$models$model4
      
      if (is.list(model4)) {
        walk2(names(model4), model4, function(group, m) {
          s <- summary(m)
          coefs <- broom::tidy(m)  
          intercept_row <- coefs %>% filter(term == "(Intercept)")
          intercept <- intercept_row$estimate
          std_err_int <- intercept_row$std.error
          t_ratio_int <- intercept_row$statistic
          p_int <- intercept_row$p.value
          slope_row <- coefs %>% filter(term == "time_trans")
          slope <- slope_row$estimate
          std_err_slope <- slope_row$std.error
          t_ratio_slope <- slope_row$statistic
          p_slope <- slope_row$p.value
          
          rsq <- s$r.squared
          mse <- s$sigma^2
          df <- s$df[2]
        })
      }
      
    }
  })
  interval_side <- reactive({
    lsl <- suppressWarnings(as.numeric(input$lsl))
    usl <- suppressWarnings(as.numeric(input$usl))
    
    case_when(
      !is.na(lsl) && !is.na(usl) ~ "two-sided",
      !is.na(lsl) &&  is.na(usl) ~ "lower",
      is.na(lsl) && !is.na(usl) ~ "upper",
      TRUE ~ "invalid"  
    )
  })
  
  ci_components_list <- list() 
  stability_crossing_data<- reactive({
    df <- stability_data()
    req(df)
    snap <- run_snapshot()
    req(snap$time_scale == input$stability_time_scale &&
          snap$meas_scale == input$stability_measurement_scale)
    
    
    df <- df %>%
      rename(
        time = !!sym(input$stability_time_col),
        meas = !!sym(input$stability_measurement_col)
      ) %>%
      filter(!is.na(time), !is.na(meas)) %>%
      mutate(
        Group = if (input$stability_group_col != "None" &&
                    input$stability_group_col %in% names(.)) {
          factor(.data[[input$stability_group_col]])
        } else {
          factor("All", levels = "All")
        },
        time_trans = case_when(
          input$stability_time_scale == "log" ~ log(time),
          input$stability_time_scale == "square" ~ time^2,
          TRUE ~ time
        ),
        meas_trans = case_when(
          input$stability_measurement_scale == "log" ~ log(meas),
          input$stability_measurement_scale == "square" ~ meas^2,
          TRUE ~ meas
        )
      )
    
    sm <- stability_model()
    req(sm$models)
    
    selected_model <- input$stability_model_choice
    model <- sm$models[[selected_model]]
    req(model)
    
    if ("Group" %in% all.vars(formula(model))) {
      model_frame <- model.frame(model)
      df$Group <- factor(df$Group, levels = levels(model_frame$Group))
    }
    
    group_levels <- levels(df$Group)
    lsl <- suppressWarnings(as.numeric(input$lsl))
    usl <- suppressWarnings(as.numeric(input$usl))
    x_max <- max(df$time_trans, na.rm = TRUE)
    
    validate(
      need(!is.na(lsl) || !is.na(usl), "Please enter at least one numeric LSL or USL.")
    )
    
    alpha <- 0.05
    interval_type <- input$interval_type
    get_crossing_time <- function(b0, b1, mse_g, x_bar, sxx, n_g,
                                            y_target, df_g_resid, group_name, bound_type,interval_side,
                                            alpha = 0.05) {
      
      if (is.na(df_g_resid) || df_g_resid <= 0) {
        message(sprintf("[Group: %s] Invalid df_g_resid = %s → skipping", group_name, df_g_resid))
        return(NA)
      }
      if (mse_g <= 0 || sxx <= 0) {
        message(sprintf("[Group: %s] Invalid MSE or SXX → skipping", group_name))
        return(NA)
      }
      t_val <- if (interval_side == "two-sided") {
        qt(1 - alpha / 2, df = df_g_resid)
      } else {
        qt(1 - alpha, df = df_g_resid)
      }
      
      A <- sxx / (t_val^2 * mse_g)
      C1 <- 1 - A * b1^2
      C2 <- 2 * A * b1 * (y_target - b0) - 2 * x_bar
      C3 <- x_bar^2 + sxx / n_g - A * (y_target - b0)^2
      D <- C2^2 - 4 * C1 * C3
      
      if (is.na(D) || D < 0) {
        message(sprintf("[%s - %s] Invalid or negative discriminant → skipping", group_name, bound_type))
        return(NA)
      }
      
      
        root <- (-C2 + sqrt(D)) / (2 * C1)
       
      if (bound_type == "USL" && b1 > 0 && (b0 >= y_target)) {
        message(sprintf("[%s - %s] Intercept above target → not stable at baseline", group_name, bound_type))
        return(NA)
      } else if (bound_type == "LSL" && b1 < 0 && (b0 <= y_target)) {
        message(sprintf("[%s - %s] Intercept below target → not stable at baseline", group_name, bound_type))
        return(NA)
      }
      
      return(root)
    }
    
    

    result <- map_dfr(group_levels, function(g) {
      df_g <- df %>% filter(Group == g)
      if (selected_model%in% c("model3","model2")) {
        n <- nrow(df)
        x_bar <- mean(df$time_trans)
        sxx <- sum((df$time_trans - x_bar)^2)
        df_g_resid <- length(df$meas_trans) - length(coef(model))  
        
      } else {
        n <- nrow(df_g)
        x_bar <- mean(df_g$time_trans)
        sxx <- sum((df_g$time_trans - x_bar)^2)
        df_g_resid <- n - 2
      }

      if (selected_model == "model3") {
        b0 <- coef(model)["(Intercept)"]
        b1 <- coef(model)["time_trans"]
      } else {
        if (g == group_levels[1]) {
          b0 <- coef(model)["(Intercept)"]
          b1 <- coef(model)["time_trans"]
          
        } else {
          intercept_term <- paste0("Group", g)
          intercept_effect <- if (intercept_term %in% names(coef(model))) coef(model)[intercept_term] else 0

          if (selected_model == "model1") {
           
            slope_term <- paste0("Group", g, ":time_trans")
            slope_effect <- if (slope_term %in% names(coef(model))) coef(model)[slope_term] else 0
            b0 <- coef(model)["(Intercept)"] + intercept_effect
            b1 <- coef(model)["time_trans"] + slope_effect
            
          } else {
            
            b0 <- coef(model)["(Intercept)"] + intercept_effect
            b1 <- coef(model)["time_trans"]
          }
        }
        }
      
  
        mse_common <- {
          y_i_all <- df$meas_trans
          x_i_all <- df$time_trans
          y_hat_all <- predict(model, newdata = df)
          residual_all <- y_i_all - y_hat_all
          rss_all <- sum(residual_all^2)
          df_all_resid <- length(y_i_all) - length(coef(model))
          rss_all / df_all_resid
        }
        mse <- if (selected_model == "model1") {
          y_i <- df_g$meas_trans
          x_i <- df_g$time_trans
          y_hat_i <- b0 + b1 * x_i
          residual_i <- y_i - y_hat_i
          rss <- sum(residual_i^2)
          rss / df_g_resid
        } else {
          mse_common
        }
  
        ci_components_list[[g]] <<- list(
          b0 = b0,
          b1 = b1,
          mse = mse,
          x_bar = x_bar,
          sxx = sxx,
          n = n,
          df_resid = df_g_resid
        )
        crossing_lsl <- if (!is.na(lsl)) {
          get_crossing_time(b0, b1, mse, x_bar, sxx, n,
                            lsl, df_g_resid, g, "LSL",
                            interval_side = interval_side(), 
                            alpha = alpha)
        } else NA
        
        crossing_usl <- if (!is.na(usl)) {
          get_crossing_time(b0, b1, mse, x_bar, sxx, n,
                            usl, df_g_resid, g, "USL",
                            interval_side = interval_side(), 
                            alpha = alpha)
        } else NA
        
        
        tibble(
          Group = g,
          `Earliest LSL Crossing` = ifelse(is.finite(crossing_lsl) && crossing_lsl >= 0,
                                           sprintf("%.5f", crossing_lsl),
                                           "No crossing"),
          `Earliest USL Crossing` = ifelse(is.finite(crossing_usl) && crossing_usl >= 0,
                                           sprintf("%.5f", crossing_usl),
                                           "No crossing")
        )
      })
      
      return(result)
    })
  
  output$stability_crossing_table <- renderTable({
    stability_crossing_data()
  })
  
  plot_reactive <- reactive({
    req(interval_side)
    
    crossing_df <- stability_crossing_data()
    
    df <- stability_data()
    req(df,input$run_stability)
    
    df <- df %>%
      rename(
        time = !!sym(input$stability_time_col),
        meas = !!sym(input$stability_measurement_col)
      ) %>%
      filter(!is.na(time), !is.na(meas)) %>%
      mutate(
        Group = if (input$stability_group_col != "None" && input$stability_group_col %in% names(.)) {
          factor(.data[[input$stability_group_col]])
        } else {
          factor("All", levels = "All")
        },
        time_trans = case_when(
          input$stability_time_scale == "log" ~ log(time),
          input$stability_time_scale == "square" ~ time^2,
          TRUE ~ time
        ),
        meas_trans = case_when(
          input$stability_measurement_scale == "log" ~ log(meas),
          input$stability_measurement_scale == "square" ~ meas^2,
          TRUE ~ meas
        )
      )
    
    sm <- stability_model()
    req(sm$models)
    selected_model <- input$stability_model_choice
    model <- sm$models[[selected_model]]
    req(model)
    
    if ("Group" %in% all.vars(formula(model))) {
      model_frame <- model.frame(model)
      df$Group <- factor(df$Group, levels = levels(model_frame$Group))
    }
    
    x_min <- input$x_axis_range[1]
    x_max <- input$x_axis_range[2]
    group_levels <- levels(df$Group)
    
    interval_for_predict <- case_when(
      input$interval_type == "CI" ~ "confidence",
      input$interval_type == "PI" ~ "prediction",
      TRUE ~ "none"
    )
    
    alpha <- 0.05
    
    pred_df <- map_dfr(group_levels, function(g) {
      
      comp <- ci_components_list[[g]]
      if (is.null(comp)) return(NULL)
      message("b0: ", comp$b0)
      message("b1: ", comp$b1)
      message("mse: ", comp$mse)
      message("x_bar: ", comp$x_bar)
      message("sxx: ", comp$sxx)
      message("n: ", comp$n)
      message("df_resid: ", comp$df_resid)
      
      time_seq <- seq(x_min, x_max, length.out = 200)
      x_bar <- comp$x_bar
      sxx <- comp$sxx
      n <- comp$n
      b0 <- comp$b0
      b1 <- comp$b1
      mse <- comp$mse
      df_resid <- comp$df_resid
      
      t_val <- if (interval_side() == "two-sided") {
        qt(1 - alpha / 2, df = df_resid)
      } else {
        qt(1 - alpha, df = df_resid)
      }
      
      y_hat <- b0 + b1 * time_seq
      se <- sqrt(mse * (1 / n + (time_seq - x_bar)^2 / sxx))
      
      if (interval_for_predict == "prediction") {
        se <- sqrt(mse * (1 + 1 / n + (time_seq - x_bar)^2 / sxx))
      }
      
      tibble(
        time_trans = time_seq,
        Group = g,
        fit = y_hat,
        lwr = y_hat - t_val * se,
        upr = y_hat + t_val * se
      )
    })
  
    
    lsl_trans <- case_when(
      input$stability_measurement_scale == "log" ~ log(input$lsl),
      input$stability_measurement_scale == "square" ~ input$lsl^2,
      TRUE ~ input$lsl
    )
    usl_trans <- case_when(
      input$stability_measurement_scale == "log" ~ log(input$usl),
      input$stability_measurement_scale == "square" ~ input$usl^2,
      TRUE ~ input$usl
    )
    
    crossing_df <- crossing_df %>%
      mutate(
        LSL_Crossing = suppressWarnings(as.numeric(`Earliest LSL Crossing`)),
        USL_Crossing = suppressWarnings(as.numeric(`Earliest USL Crossing`))
      )
    base_plot <- ggplot() +
      geom_point(data = df, aes(x = time_trans, y = meas_trans, color = Group, shape = Group), size = 2, alpha = 0.7) +
      geom_hline(yintercept = lsl_trans, color = "black", linetype = "dashed") +
      geom_hline(yintercept = usl_trans, color = "black", linetype = "dashed") +
      scale_x_continuous(limits = input$x_axis_range) +
      theme_minimal() +
      labs(
        title = "Stability Plot with Confidence Bands and Crossings",
        x = paste0(input$stability_time_col, " (", input$stability_time_scale, ")"),
        y = paste0(input$stability_measurement_col, " (", input$stability_measurement_scale, ")"),
        color = "Group", shape = "Group"
      ) +
      geom_line(data = pred_df, aes(x = time_trans, y = fit, color = Group), linewidth = 1) +
      geom_ribbon(data = pred_df, aes(x = time_trans, ymin = lwr, ymax = upr, fill = Group), alpha = 0.2) +
      geom_vline(data = crossing_df %>% filter(!is.na(LSL_Crossing)),
                 aes(xintercept = LSL_Crossing, color = Group), linetype = "dotted") +
      geom_vline(data = crossing_df %>% filter(!is.na(USL_Crossing)),
                 aes(xintercept = USL_Crossing, color = Group), linetype = "dotted")
    base_plot
  })
  output$stability_plot <- renderPlot({
    plot_reactive()
  })
  
  output$stability_summary <- renderUI({
    sm <- stability_model()
    req(sm)
    snap <- run_snapshot()
    req(snap$time_scale == input$stability_time_scale &&
          snap$meas_scale == input$stability_measurement_scale)
    
    
    model_ids <- names(sm$model_estimates)
    
    
    model_ids <- names(sm$model_estimates)
    tagList(
      lapply(model_ids, function(mid) {
        model_num <- str_extract(mid, "\\d+")
        intercept_label <- case_when(
          mid == "model1" ~ "Different",
          mid == "model2" ~ "Different",
          mid == "model3" ~ "Common",
          mid == "model4" ~ "Different",
          TRUE ~ "Unknown"
        )
        slope_label <- case_when(
          mid == "model1" ~ "Different",
          mid == "model2" ~ "Common",
          mid == "model3" ~ "Common",
          mid == "model4" ~ "Different by group",
          TRUE ~ "Unknown"
        )
        wellPanel(
          h4(glue::glue("Model {model_num} - Intercept:{intercept_label}; Slope:{slope_label}")),
          
          tags$b("Model Summary"),
          tableOutput(paste0("summary_table_", mid)),
          
          tags$b("Estimate"),
          tableOutput(paste0("estimate_table_", mid))
        )
      })
    )
  })
  observe({
    sm <- stability_model()
    req(sm)
    req(stability_data())
    req(input$stability_group_col)
    df <- stability_data()
    if (input$stability_group_col == "None" || !(input$stability_group_col %in% names(df))) {
      group_levels <- "All"
    } else {
      group_var <- df[[input$stability_group_col]]
      group_levels <- levels(as.factor(group_var))
    }
    model_ids <- names(sm$models)
    for (mid in model_ids) {
      local({
        model_id <- mid
        group_levels_local <- group_levels
        output[[paste0("estimate_table_", model_id)]] <- renderTable({
          sm2 <- stability_model()
          req(sm2)
          rows <- list()
          if (model_id == "model4") {
            model_list <- sm2$models$model4
            for (g in names(model_list)) {
              m <- model_list[[g]]
              if (!inherits(m, "lm")) next
              coefs <- coef(m)
              s <- summary(m)
              rows[[length(rows) + 1]] <- tibble(
                Parameter = paste0("Intercept (", g, ")"),
                Estimate = round(coefs["(Intercept)"], 5),
                `Std Error` = round(s$coefficients["(Intercept)", "Std. Error"], 5),
                `t Ratio` = round(s$coefficients["(Intercept)", "t value"], 3),
                `Prob > |t|` = format.pval(s$coefficients["(Intercept)", "Pr(>|t|)"], digits = 3, eps = .001)
              )
              
              rows[[length(rows) + 1]] <- tibble(
                Parameter = paste0("Slope (", g, ")"),
                Estimate = round(coefs["time_trans"], 5),
                `Std Error` = round(s$coefficients["time_trans", "Std. Error"], 5),
                `t Ratio` = round(s$coefficients["time_trans", "t value"], 3),
                `Prob > |t|` = format.pval(s$coefficients["time_trans", "Pr(>|t|)"], digits = 3, eps = .001)
              )
            }
            return(bind_rows(rows))
          }
          model <- sm2$models[[model_id]]
          coefs <- coef(model)
          summary_model <- summary(model)
          baseline <- group_levels_local[1]
          if (model_id == "model3") {
            rows[[1]] <- tibble(
              Parameter = "Intercept",
              Estimate = round(coefs["(Intercept)"], 5),
              `Std Error` = round(summary_model$coefficients["(Intercept)", "Std. Error"], 5),
              `t Ratio` = round(summary_model$coefficients["(Intercept)", "t value"], 3),
              `Prob > |t|` = format.pval(summary_model$coefficients["(Intercept)", "Pr(>|t|)"], digits = 3, eps = .001)
            )
            rows[[2]] <- tibble(
              Parameter = "Slope",
              Estimate = round(coefs["time_trans"], 5),
              `Std Error` = round(summary_model$coefficients["time_trans", "Std. Error"], 5),
              `t Ratio` = round(summary_model$coefficients["time_trans", "t value"], 3),
              `Prob > |t|` = format.pval(summary_model$coefficients["time_trans", "Pr(>|t|)"], digits = 3, eps = .001)
            )
            return(bind_rows(rows))
          }
          if (model_id == "model2") {
            rows[[1]] <- tibble(
              Parameter = "Slope (All)",
              Estimate = round(coefs["time_trans"], 5),
              `Std Error` = round(summary_model$coefficients["time_trans", "Std. Error"], 5),
              `t Ratio` = round(summary_model$coefficients["time_trans", "t value"], 3),
              `Prob > |t|` = format.pval(summary_model$coefficients["time_trans", "Pr(>|t|)"], digits = 3, eps = .001)
            )
          }
          if (model_id %in% c("model1", "model2")) {
            for (g in group_levels_local) {
              is_baseline <- g == baseline
              if (model_id == "model3") {
              } else if (is_baseline) {
                rows[[length(rows) + 1]] <- tibble(
                  Parameter = paste0("Intercept (", g, ")"),
                  Estimate = round(coefs["(Intercept)"], 5),
                  `Std Error` = round(summary_model$coefficients["(Intercept)", "Std. Error"], 5),
                  `t Ratio` = round(summary_model$coefficients["(Intercept)", "t value"], 3),
                  `Prob > |t|` = format.pval(summary_model$coefficients["(Intercept)", "Pr(>|t|)"], digits = 3, eps = .001)
                )
                } else {
                term <- paste0("Group", g)
                if (term %in% names(coefs)) {
                  stat <- get_linear_combination_stats(model, c("(Intercept)", term))
                  rows[[length(rows) + 1]] <- tibble(
                    Parameter = paste0("Intercept (", g, ")"),
                    Estimate = round(stat$est, 5),
                    `Std Error` = round(stat$se, 5),
                    `t Ratio` = round(stat$t, 3),
                    `Prob > |t|` = format.pval(stat$p, digits = 3, eps = .001)
                  )
                }
              }
              if (model_id %in% c("model1")) {
                base <- "time_trans"
                interaction_terms <- c(paste0("Group", g, ":time_trans"), paste0("time_trans:Group", g))
                interaction_term <- intersect(interaction_terms, names(coefs))
                if (is_baseline) {
                  rows[[length(rows) + 1]] <- tibble(
                    Parameter = paste0("Slope (", g, ")"),
                    Estimate = round(coefs[base], 5),
                    `Std Error` = round(summary_model$coefficients[base, "Std. Error"], 5),
                    `t Ratio` = round(summary_model$coefficients[base, "t value"], 3),
                    `Prob > |t|` = format.pval(summary_model$coefficients[base, "Pr(>|t|)"], digits = 3, eps = .001)
                  )
                } else if (length(interaction_term) == 1) {
                  stat <- get_linear_combination_stats(model, c(base, interaction_term))
                  rows[[length(rows) + 1]] <- tibble(
                    Parameter = paste0("Slope (", g, ")"),
                    Estimate = round(stat$est, 5),
                    `Std Error` = round(stat$se, 5),
                    `t Ratio` = round(stat$t, 3),
                    `Prob > |t|` = format.pval(stat$p, digits = 3, eps = .001)
                  )
                }
              }
            }
          }
          bind_rows(rows)
        },digits = 5)
      })
    }
    for (mid in model_ids) {
      local({
        model_id <- mid
        output[[paste0("summary_table_", model_id)]] <- renderTable({
          sm2 <- stability_model()
          req(sm2)
          req(input$stability_measurement_scale)
          req(input$stability_time_scale)
          method <- paste0("meas: ", input$stability_measurement_scale, ", time: ", input$stability_time_scale)
          if (model_id == "model4") {
            model_list <- sm2$models$model4
            rows <- map_dfr(names(model_list), function(g) {
              m <- model_list[[g]]
              if (!inherits(m, "lm")) return(NULL)
              s <- summary(m)
              tibble(
                Group = g,
                mse = round(s$sigma^2, 6),
                df = s$df[2],
                method = method
                
              )
            })
          } else {
            m <- sm2$models[[model_id]]
            s <- summary(m)
            rows <- tibble(
              sse = sum(residuals(m)^2),
              nparm = length(coef(m)),
              df = s$df[2],
              rsquare = round(s$r.squared, 4),
              mse = round(s$sigma^2, 6),
              method = method
            )
          }
          return(rows %>% relocate(method, .after = last_col()))
        },digits = 5)
      })
    }
    
  })
  output$model_comparison_table <- renderTable({
    sm <- stability_model()
    req(sm)
    req(sm$model_comparison)
    sm$model_comparison
  }, digits = 5)
  
  output$best_model_recommendation <- renderText({
    sm <- stability_model()
    req(sm)
    comp <- sm$model_comparison
    
    get_pval <- function(source_letter) {
      p_str <- comp %>% filter(Source == source_letter) %>% pull(`Prob > F`)
      p_clean <- gsub("<", "", p_str)  # "<0.0001" → "0.0001"
      suppressWarnings(as.numeric(p_clean))
    }
    
    
    p_A <- get_pval("A")  
    p_B <- get_pval("B")  
    p_C <- get_pval("C") 
    
    alpha <- 0.25
    
    suggestion <- case_when(
      p_A >= alpha & p_B >= alpha & p_C >= alpha ~ " Model 3 is recommended (common intercept and slope).",
      p_B < alpha & p_C >= alpha ~ " Model 2 is recommended (different intercepts, common slope).",
      p_C < alpha ~ " Model 1 is recommended (different intercepts and slopes).",
      TRUE ~ "⚠️ Inconclusive model selection; review diagnostics."
    )
    
    glue::glue(
      "Model Recommendation (F-test, α = {alpha}):\n",
      "{suggestion}\n\n")
  })
  output$download_stability_plot <- downloadHandler(
    filename = function() {
      paste0("stability_plot.", input$file_type)
    },
    content = function(file) {
      ggsave(file, plot = plot_reactive(), width = 8, height = 6, dpi = input$dpi, units = "in")
    }
  )
  
  
  options_visible <- reactiveVal(FALSE)
  
  observeEvent(input$toggle_download_options, {
    shinyjs::toggle("download_options")
    options_visible(!options_visible())
    label <- if (options_visible()) "▾ Hide options" else "▸ Show options"
    updateActionLink(session, "toggle_download_options", label = label)
  })
  
  
  output$residual_plot <- renderPlot({
    sm <- stability_model()
    req(sm)
    selected_model <- input$stability_model_choice
    model <- sm$models[[selected_model]]
    req(model)
    
    df <- stability_data()
    req(df)
    df <- df %>%
      rename(
        time = !!sym(input$stability_time_col),
        meas = !!sym(input$stability_measurement_col)
      ) %>%
      filter(!is.na(time), !is.na(meas)) %>%
      mutate(
        Group = if (input$stability_group_col != "None" &&
                    input$stability_group_col %in% names(.)) {
          factor(.data[[input$stability_group_col]])
        } else {
          factor("All", levels = "All")
        }
      )
    df <- df %>%
      mutate(
        time_trans = sm$models$model3$model$time_trans,  # 또는 직접 캐싱된 값 사용
        meas_trans = sm$models$model3$model$meas_trans
      )
    
    if (selected_model == "model4") {
      df_aug <- sm$group_levels %>%
        map_df(function(g) {
          m <- sm$models$model4[[g]]
          df_g <- df %>% filter(Group == g)
          pred <- predict(m, newdata = df_g)
          df_g %>%
            mutate(.fitted = pred, .resid = meas_trans - pred)
        })
    } else {
      df_aug <- df %>%
        mutate(
          .fitted = predict(model, newdata = df),
          .resid = meas_trans - .fitted
        )
    }
    
    p1 <- ggplot(df_aug, aes(x = time_trans, y = .resid, color = Group, shape = Group)) +
      geom_point(alpha = 0.8, size = 2) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(title = "Residuals vs Time (All Groups Combined)", x = "Time", y = "Residuals")
    
    p2 <- ggplot(df_aug, aes(x = time_trans, y = .resid, color = Group, shape = Group)) +
      geom_point(alpha = 0.8, size = 2) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      facet_wrap(~Group) +
      labs(title = "Residuals vs Time (By Group)", x = "Time", y = "Residuals")
    
    
    cowplot::plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1, 1))
  })

}

shinyApp(ui, server)


