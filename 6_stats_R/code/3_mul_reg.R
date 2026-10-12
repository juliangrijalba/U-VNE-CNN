# authors: Julián Grijalba <juliangrijalba@unimayor.edu.co> and Carlos Duran <duranurbanphony@unimayor.edu.co>
# output: PCA FOR MORPHOLOGICAL INDICATORS AND MULTILINEAR REGRESSION
# DOMINANCE SOUNDSCAPE COMPONENT INDICATORS (DSCI) ~ ACOUSTIC INDICATORS + MORPHOLOGICAL INDICATORS

# ===================================================================================
# 0. LIBRARIES
# ===================================================================================

library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)
library(ggrepel)
library(writexl)
library(FactoMineR)
library(factoextra)
library(broom)
library(openxlsx)
library(stringr)

# ===================================================================================
# 1. DATASETS
# ===================================================================================

sound_data <- read.csv(
  "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/data/m_acoustics_sound_comp.csv"
)

morph_data <- read.csv(
  "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/data/m_morph_sound_comp.csv"
)

cat("\n--- DIMENSIONS ---\n")
cat("Sound data:", nrow(sound_data), "filas\n")
cat("Morphology data:", nrow(morph_data), "filas\n")
print(sound_data)
print(morph_data)
      
# ===================================================================================
# 2. CHECK DATASETS MATCH
# ===================================================================================

if (nrow(sound_data) != nrow(morph_data)) {
  
  stop(
    paste0(
      "ERROR: the two files have not the same amount of files. ",
      "Sound data = ", nrow(sound_data),
      "; Morph data = ", nrow(morph_data), "."
    )
  )
}

cat(
  "\nOK: BOTH DATASETS HAVE",
  nrow(sound_data),
  "OBSERVATIONS.\n"
)


# ===================================================================================
# 3. MORPHOLOGICAL VARIABLES
# ===================================================================================

morph_vars <- c(
  "BPAF",
  "EGR",
  "RAF",
  "bldg_A",
  "bldg_P",
  "gr_A",
  "gr_P",
  "sq_A",
  "sq_P",
  "op_A",
  "op_P",
  "grd_A",
  "rd_A"
)

# ===================================================================================
# 4. DEPENDENT VARIABLES: DCI
# ===================================================================================

dci_vars <- c(
  "TM",
  "MH",
  "VO",
  "MU",
  "EM",
  "SO",
  "BI",
  "GE",
  "SI"
)

# ===================================================================================
# 5. PCA OF MORPHOLOGICAL INDICATORS
# ===================================================================================

pca_data <- morph_data %>%
  select(
    all_of(morph_vars)
  ) %>%
  mutate(
    across(
      everything(),
      ~ replace(.x, is.na(.x), 0)
    )
  )

stopifnot(
  all(
    sapply(
      pca_data,
      is.numeric
    )
  )
)

# ===================================================================================
# 6. KMO Y BARTLETT
# ===================================================================================

cor_matrix <- cor(
  pca_data,
  method = "pearson"
)

kmo_result <- psych::KMO(
  cor_matrix
)

bartlett_result <- psych::cortest.bartlett(
  cor_matrix,
  n = nrow(pca_data)
)

cat("\n============================================================\n")
cat("VALIDATION TEST FOR PCA\n")
cat("============================================================\n")

cat(
  "KMO = ",
  round(
    kmo_result$MSA,
    3
  ),
  "\n",
  sep = ""
)

cat(
  "Bartlett: χ²(",
  bartlett_result$df,
  ") = ",
  round(
    bartlett_result$chisq,
    2
  ),
  ", p = ",
  format.pval(
    bartlett_result$p.value,
    digits = 3
  ),
  "\n",
  sep = ""
)

# ===================================================================================
# 7. APPLY PCA
# Based on the morphological indices, a preliminary grouping
# of variables was expected, defined by: buildings, open public space,
# roads, and exposure areas.
#
# Eigenvalues were used to select the number of components
# among the computed morphological indicators.
# ===================================================================================

pca_full <- psych::principal(
  pca_data,
  nfactors = ncol(pca_data),
  rotate = "none",
  scores = FALSE,
  covar = FALSE
)

eigenvalues <- pca_full$values

# selection criterium
eigenvalues_table <- data.frame(
  Component = seq_along(eigenvalues),
  Eigenvalue = eigenvalues,
  Retained = eigenvalues > 1
)

print(eigenvalues_table, row.names = FALSE)

n_components <- sum(
  eigenvalues > 1
)

cat(
  "\nRetained components (eigenvalue > 1): ",
  n_components,
  "\n",
  sep = ""
)

if (n_components != 4) {
  
  stop(
    paste0(
      "ERROR: 4 components were expected based on the predefined PCA. ",
      "The eigenvalue > 1 criterion retained ",
      n_components,
      " components."
    )
  )
}

# The selection criterion based on eigenvalues indicates that there are
# 4 principal components.

# ===================================================================================
# 8. PCA WITH VARIMAX ROTATION
# ===================================================================================

pca_result <- psych::principal(
  pca_data,
  nfactors = n_components,
  rotate = "varimax",
  scores = TRUE,
  covar = FALSE
)

# ===================================================================================
# 9. EXPLAINED VARIANCE
# ===================================================================================

variance <- (
  pca_result$Vaccounted[
    "Proportion Var",
  ]
) * 100

cumulative_variance <- (
  pca_result$Vaccounted[
    "Cumulative Var",
  ]
) * 100

# ===================================================================================
# 10. ROTATED COMPONENTS MATRIX
# ===================================================================================

loading_matrix <- as.data.frame(
  unclass(
    pca_result$loadings
  )
)

loading_matrix$Variable <- rownames(
  loading_matrix
)

rownames(
  loading_matrix
) <- NULL

loading_matrix <- loading_matrix[
  ,
  c(
    "Variable",
    setdiff(
      names(loading_matrix),
      "Variable"
    )
  )
]

component_names_raw <- paste0(
  "Component ",
  seq_len(n_components),
  " (",
  sprintf(
    "%.2f",
    variance
  ),
  "%)"
)

names(
  loading_matrix
)[-1] <- component_names_raw

print(loading_matrix)

# Although 4 components were selected based on the eigenvalues, the interpretation
# of the loadings suggests a different grouping: buildings/roads (C1), green
# areas (C2), ground exposure (C3), and squares (C4).

# ===================================================================================
# 11. VARIABLE GROUPING ACCORDING TO MORPHOLOGICAL MEANING
#
# These groupings are the morphological groups ultimately interpreted
# based on the PCA.
#
# ===================================================================================

morphological_groups <- list(
  
  `Buildings & Roads` = c(
    "BPAF",
    "bldg_A",
    "bldg_P",
    "RAF",
    "rd_A",
    "op_A"
  ),
  
  `Green Area` = c(
    "gr_A",
    "gr_P",
    "op_P"
  ),
  
  `Ground Exposure` = c(
    "EGR",
    "grd_A"
  ),
  
  `Square` = c(
    "sq_A",
    "sq_P"
  )
)


# ===================================================================================
# 12. CHECK THAT ALL VARIABLES ARE INCLUDED IN THE GROUPS
# ===================================================================================

grouped_variables <- unlist(
  morphological_groups
)

if (
  !setequal(
    grouped_variables,
    morph_vars
  )
) {
  
  stop(
    paste0(
      "ERROR: the variables defined in the groups do not match ",
      "exactly the variables used in the PCA."
    )
  )
}

# ===================================================================================
# 13. NUMERIC MATRIX OF LOADINGS
# ===================================================================================

# Exclude the variable column and retain only the numeric loading columns

numeric_loadings <- as.matrix(
  loading_matrix[
    ,
    -1
  ]
)

head(numeric_loadings)

# ===================================================================================
# 14. ASSIGNMENT OF VARIABLES TO EACH COMPONENT BASED ON ABSOLUTE LOADINGS
# ===================================================================================

dominant_component <- apply(
  abs(numeric_loadings),
  1,
  which.max
)

dominant_loading <- apply(
  abs(numeric_loadings),
  1,
  max
)

variable_assignment <- data.frame(
  Variable = loading_matrix$Variable,
  Component = dominant_component,
  Loading = round(
    dominant_loading,
    3
  )
)

print(
  variable_assignment
)

# ===================================================================================
# 15. AVERAGE LOADINGS BY COMPONENT AND GROUP
# ===================================================================================

group_component_strength <- data.frame()

for (group_name in names(morphological_groups)) {
  
  vars <- morphological_groups[[group_name]]
  
  group_loadings <- numeric_loadings[
    match(
      vars,
      loading_matrix$Variable
    ),
    ,
    drop = FALSE
  ]
  
  mean_abs_loadings <- colMeans(
    abs(group_loadings)
  )
  
  temp <- data.frame(
    Group = group_name,
    Component = seq_len(n_components),
    Mean_Absolute_Loading = mean_abs_loadings
  )
  
  group_component_strength <- bind_rows(
    group_component_strength,
    temp
  )
}

print(
  group_component_strength
)

# ===================================================================================
# 16. SELECT THE GROUP WITH THE HIGHEST AVERAGE LOADING FOR EACH COMPONENT
# ===================================================================================

group_component_assignment <- group_component_strength %>%
  group_by(Group) %>%
  slice_max(
    Mean_Absolute_Loading,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

print(
  group_component_assignment
)

# ===================================================================================
# 19. DISPLAY ROTATED COMPONENT MATRIX
# ===================================================================================

cat("\n============================================================\n")
cat("ROTATED COMPONENT MATRIX\n")
cat("============================================================\n\n")

print(
  loading_matrix,
  row.names = FALSE
)

# ===================================================================================
# 20. DISPLAY VARIABLE GROUPING
# ===================================================================================

cat("\n============================================================\n")
cat("VARIABLE GROUPING BY DOMINANT COMPONENT\n")
cat("============================================================\n\n")

for (i in seq_len(4)) {
  
  vars <- variable_assignment$Variable[
    variable_assignment$Component == i
  ]
  
  cat(
    "Component ",
    i,
    ": ",
    paste(
      vars,
      collapse = ", "
    ),
    "\n",
    sep = ""
  )
}

# ===================================================================================
# 21. EXTRACT COMPONENT SCORES
# ===================================================================================

pca_scores <- as.data.frame(
  pca_result$scores
)

names(
  pca_scores
) <- paste0(
  "PC",
  1:4
)

# ===================================================================================
# 22. ASSIGN INTERPRETABLE NAMES TO THE SCORES
#
# The correspondence has been previously verified based on the rotated loadings.
# ===================================================================================

pca_scores <- pca_scores %>%
  rename(
    Buildings_Roads = PC1,
    Green_Open_Space = PC2,
    Ground_Exposure = PC3,
    Square = PC4
  )

# ===================================================================================
# 23. DISPLAY SUMMARY OF COMPONENT SCORES
# ===================================================================================

cat("\n============================================================\n")
cat("COMPONENT SCORES\n")
cat("============================================================\n\n")

print(
  head(
    pca_scores
  )
)

# ===================================================================================
# 26. EXPORT PCA RESULTS
# ===================================================================================

write_xlsx(
  list(
    Rotated_Component_Matrix =
      loading_matrix,
    
    Component_Scores =
      pca_scores
  ),
  "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/6_stats_R/results/VNE_PCA.xlsx"
)


# REGRESSION FROM HERE-------------------------------------------
#----------------------------------------------------------------

#----------------------------------------------------------------

#----------------------------------------------------------------

#----------------------------------------------------------------

#----------------------------------------------------------------

#----------------------------------------------------------------

# ===================================================================================
# 27. BUILD UNIQUE DATASET
# ===================================================================================

data <- cbind(
  morph_data[, morph_vars],
  sound_data[, setdiff(names(sound_data), "fid")]
)

cat("\n--- COMBINED DATASET ---\n")
print(
  dim(data)
)

# ===================================================================================
# 28. ADD THE 4 COMPONENT SCORES
# ===================================================================================

data <- cbind(
  data,
  pca_scores
)

# ===================================================================================
# 29. RENAME SOUND VARIABLES
# ===================================================================================

data <- data %>%
  rename(
    LAeq_P1 = leq_p1_r,
    LAeq_P2 = leq_p2_r,
    
    LCeq_Aeq_P1 = leqA_LeqC_p1_r,
    LCeq_Aeq_P2 = leqA_leqC_p2_r,
    
    L10_90_P1 = l10_l90_p1_r,
    L10_90_P2 = l10_l90_p2_r
  )

# ===================================================================================
# 30. RENAME DSCI
# ===================================================================================

data <- data %>%
  rename(
    TM_P1 = tm_p1,
    MH_P1 = mh_p1,
    VO_P1 = vo_p1,
    MU_P1 = mu_p1,
    EM_P1 = em_p1,
    SO_P1 = so_p1,
    BI_P1 = bi_p1,
    GE_P1 = ge_p1,
    SI_P1 = si_p1,
    
    TM_P2 = tm_p2,
    MH_P2 = mh_p2,
    VO_P2 = vo_p2,
    MU_P2 = mu_p2,
    EM_P2 = em_p2,
    SO_P2 = so_p2,
    BI_P2 = bi_p2,
    GE_P2 = ge_p2,
    SI_P2 = si_p2
  )

# ===================================================================================
# 31. CREATE TOTALS
# ===================================================================================

for (v in dci_vars) {
  
  data[[paste0(v, "_Total")]] <-
    rowMeans(
      data[, c(
        paste0(v, "_P1"),
        paste0(v, "_P2")
      )],
      na.rm = FALSE
    )
}


data$LAeq_Total <- rowMeans(
  data[, c(
    "LAeq_P1",
    "LAeq_P2"
  )],
  na.rm = FALSE
)

data$LCeq_Aeq_Total <- rowMeans(
  data[, c(
    "LCeq_Aeq_P1",
    "LCeq_Aeq_P2"
  )],
  na.rm = FALSE
)

data$L10_90_Total <- rowMeans(
  data[, c(
    "L10_90_P1",
    "L10_90_P2"
  )],
  na.rm = FALSE
)

# ===================================================================================
# 32. PREPARE DATASETS
#
# THE 4 COMPONENT SCORES ARE NOW USED INSTEAD OF THE 14 ORIGINAL
# MORPHOLOGICAL VARIABLES.
# ===================================================================================

pca_score_vars <- c(
  "Buildings_Roads",
  "Green_Open_Space",
  "Ground_Exposure",
  "Square"
)

data_P1 <- data %>%
  select(
    all_of(pca_score_vars),
    all_of(paste0(dci_vars, "_P1")),
    LAeq_P1,
    LCeq_Aeq_P1,
    L10_90_P1
  ) %>%
  rename(
    LAeq = LAeq_P1,
    LCeq_Aeq = LCeq_Aeq_P1,
    L10_90 = L10_90_P1
  )

data_P2 <- data %>%
  select(
    all_of(pca_score_vars),
    all_of(paste0(dci_vars, "_P2")),
    LAeq_P2,
    LCeq_Aeq_P2,
    L10_90_P2
  ) %>%
  rename(
    LAeq = LAeq_P2,
    LCeq_Aeq = LCeq_Aeq_P2,
    L10_90 = L10_90_P2
  )

data_Total <- data %>%
  select(
    all_of(pca_score_vars),
    all_of(paste0(dci_vars, "_Total")),
    LAeq_Total,
    LCeq_Aeq_Total,
    L10_90_Total
  ) %>%
  rename(
    LAeq = LAeq_Total,
    LCeq_Aeq = LCeq_Aeq_Total,
    L10_90 = L10_90_Total
  )

# ===================================================================================
# 33. STANDARDIZED REGRESSION FUNCTION
#
# Predictors:
#
# Acoustic:
#   LAeq
#   LCeq-Aeq
#   L10-90
#
# Morphological PCA:
#   Buildings & Roads
#   Green & Open Space
#   Ground Exposure
#   Square
#
# The β coefficients are fully standardized.
# ===================================================================================

run_standardized_model <- function(
    df,
    dependent,
    period
) {
  
  predictors <- c(
    "LAeq",
    "LCeq_Aeq",
    "L10_90",
    pca_score_vars
  )
  
  # -------------------------------------------------------------------------------
  # ORIGINALL MODEL FOR R²
  # -------------------------------------------------------------------------------
  
  formula_text <- paste(
    dependent,
    "~",
    paste(
      predictors,
      collapse = " + "
    )
  )
  
  model <- lm(
    as.formula(
      formula_text
    ),
    data = df
  )
  
  # -------------------------------------------------------------------------------
  # MODEL COMPLETELY STANDARIZED
  # -------------------------------------------------------------------------------
  
  model_std <- lm(
    
    scale(
      df[[dependent]]
    ) ~
      
      scale(
        df$LAeq
      ) +
      
      scale(
        df$LCeq_Aeq
      ) +
      
      scale(
        df$L10_90
      ) +
      
      scale(
        df$Buildings_Roads
      ) +
      
      scale(
        df$Green_Open_Space
      ) +
      
      scale(
        df$Ground_Exposure
      ) +
      
      scale(
        df$Square
      )
  )
  
  
  # -------------------------------------------------------------------------------
  # EXTRACT COEFFICIENTS AND STATS
  # -------------------------------------------------------------------------------
  
  coef_table <- broom::tidy(
    model_std
  )
  
  model_stats <- broom::glance(
    model
  )
  
  
  coef_table <- coef_table %>%
    mutate(
      Dependent = dependent,
      Period = period,
      R2 = model_stats$r.squared,
      Adj_R2 = model_stats$adj.r.squared,
      N = nobs(model)
    )
  
  
  return(
    coef_table
  )
}


# ===================================================================================
# 34. EJECUTE ALL MODELS
# ===================================================================================

results <- list()

# -----------------------------------------------------------------------------------
# P1
# -----------------------------------------------------------------------------------

for (
  v in paste0(
    dci_vars,
    "_P1"
  )
) {
  
  results[[length(results) + 1]] <-
    run_standardized_model(
      data_P1,
      v,
      "P1"
    )
}


# -----------------------------------------------------------------------------------
# P2
# -----------------------------------------------------------------------------------

for (
  v in paste0(
    dci_vars,
    "_P2"
  )
) {
  
  results[[length(results) + 1]] <-
    run_standardized_model(
      data_P2,
      v,
      "P2"
    )
}


# -----------------------------------------------------------------------------------
# TOTAL
# -----------------------------------------------------------------------------------

for (
  v in paste0(
    dci_vars,
    "_Total"
  )
) {
  
  results[[length(results) + 1]] <-
    run_standardized_model(
      data_Total,
      v,
      "Total"
    )
}


# ===================================================================================
# 35. JOIN RESULTS
# ===================================================================================

regression_results <- bind_rows(
  results
)

# ===================================================================================
# 36. IDENTIFY PREDICTORS
# ===================================================================================

regression_results <- regression_results %>%
  mutate(
    
    Predictor = case_when(
      
      term == "scale(df$LAeq)" ~
        "LAeq",
      
      term == "scale(df$LCeq_Aeq)" ~
        "LCeq-Aeq",
      
      term == "scale(df$L10_90)" ~
        "L10-90",
      
      term == "scale(df$Buildings_Roads)" ~
        "Buildings & Roads",
      
      term == "scale(df$Green_Open_Space)" ~
        "Green & Open Space",
      
      term == "scale(df$Ground_Exposure)" ~
        "Ground Exposure",
      
      term == "scale(df$Square)" ~
        "Square",
      
      TRUE ~ term
    ),
    
    
    Significance = case_when(
      
      p.value < 0.01 ~
        "**",
      
      p.value < 0.05 ~
        "*",
      
      TRUE ~
        ""
    ),
    
    
    Beta = estimate,
    P = p.value
  )

# ===================================================================================
# 37. PREDICTORS ORDER
# ===================================================================================

predictor_order <- c(
  
  "LAeq",
  "LCeq-Aeq",
  "L10-90",
  
  "Buildings & Roads",
  "Green & Open Space",
  "Ground Exposure",
  "Square"
)

regression_results$Predictor <- factor(
  regression_results$Predictor,
  levels = predictor_order
)

# ===================================================================================
# 38. RESULTS LONG TABLE
# ===================================================================================

regression_long <- regression_results %>%
  mutate(
    Dependent = str_remove(
      Dependent,
      "_(P1|P2|Total)$"
    )
  ) %>%
  select(
    Dependent,
    Period,
    Predictor,
    Beta,
    P,
    Significance,
    R2,
    Adj_R2,
    N
  )

# ===================================================================================
# 39. FUNCTION FOR FORMATTING β + SIGNIFICANCE
# ===================================================================================

format_beta <- function(
    beta,
    significance
) {
  
  ifelse(
    
    is.na(beta),
    
    "",
    
    paste0(
      sprintf(
        "%.3f",
        beta
      ),
      significance
    )
  )
}


# ===================================================================================
# 40. BUILD TABLE FOR PAPER
# ===================================================================================

dependent_order <- c(
  "TM",
  "MH",
  "VO",
  "MU",
  "EM",
  "SO",
  "BI",
  "GE",
  "SI"
)

period_order <- c(
  "Total",
  "P1",
  "P2"
)


paper_table <- data.frame(
  
  Predictor = c(
    
    "R²",
    
    "Acoustic",
    
    "LAeq",
    "LCeq-Aeq",
    "L10-90",
    
    "Morphological PCA",
    
    "Buildings & Roads",
    "Green & Open Space",
    "Ground Exposure",
    "Square"
  ),
  
  stringsAsFactors = FALSE
)


# Create 27 columns

for (
  dep in dependent_order
) {
  
  for (
    per in period_order
  ) {
    
    col_name <- paste0(
      dep,
      "_",
      per
    )
    
    paper_table[[col_name]] <- ""
  }
}


# ===================================================================================
# 41. FILL IN COEFFICIENTS
# ===================================================================================

for (
  i in 1:nrow(regression_long)
) {
  
  row <- regression_long[i, ]
  
  dep <- as.character(
    row$Dependent
  )
  
  per <- as.character(
    row$Period
  )
  
  pred <- as.character(
    row$Predictor
  )
  
  col_name <- paste0(
    dep,
    "_",
    per
  )
  
  
  # R²
  r2_row <- which(
    paper_table$Predictor == "R²"
  )
  
  paper_table[
    r2_row,
    col_name
  ] <-
    sprintf(
      "%.3f",
      row$R2
    )
  
  
  # Coefficient
  if (
    pred %in% predictor_order
  ) {
    
    pred_row <- which(
      paper_table$Predictor == pred
    )
    
    paper_table[
      pred_row,
      col_name
    ] <-
      format_beta(
        row$Beta,
        row$Significance
      )
  }
}


# ===================================================================================
# 42. CREATE WORKBOOK
# ===================================================================================

wb <- createWorkbook()

addWorksheet(
  wb,
  "Regression table"
)

addWorksheet(
  wb,
  "Coefficient data"
)

addWorksheet(
  wb,
  "Model summary"
)


# ===================================================================================
# 43. STYLES
# ===================================================================================

header_style <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight",
  borderStyle = "thin"
)

subheader_style <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight",
  borderStyle = "thin"
)

section_style <- createStyle(
  textDecoration = "italic",
  halign = "left",
  border = "Bottom",
  borderStyle = "thin"
)

body_style <- createStyle(
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight",
  borderStyle = "thin"
)

predictor_style <- createStyle(
  halign = "left",
  valign = "center",
  border = "TopBottomLeftRight",
  borderStyle = "thin"
)


# ===================================================================================
# 44. HIERARCHICAL HEADER
# ===================================================================================

writeData(
  wb,
  "Regression table",
  x = "Standardized regression coefficients",
  startRow = 1,
  startCol = 1
)

mergeCells(
  wb,
  "Regression table",
  cols = 1:28,
  rows = 1
)

addStyle(
  wb,
  "Regression table",
  header_style,
  rows = 1,
  cols = 1:28,
  gridExpand = TRUE
)


# Second row: DCI blocks

writeData(
  wb,
  "Regression table",
  x = "Predictor",
  startRow = 3,
  startCol = 1
)

for (
  i in seq_along(
    dependent_order
  )
) {
  
  dep <- dependent_order[i]
  
  start_col <- 2 + (
    i - 1
  ) * 3
  
  end_col <- start_col + 2
  
  writeData(
    wb,
    "Regression table",
    x = dep,
    startRow = 3,
    startCol = start_col
  )
  
  mergeCells(
    wb,
    "Regression table",
    cols = start_col:end_col,
    rows = 3
  )
  
  addStyle(
    wb,
    "Regression table",
    header_style,
    rows = 3,
    cols = start_col:end_col,
    gridExpand = TRUE
  )
}


# Third row: Total / P1 / P2

for (
  i in seq_along(
    dependent_order
  )
) {
  
  start_col <- 2 + (
    i - 1
  ) * 3
  
  writeData(
    wb,
    "Regression table",
    x = data.frame(
      Total = "Total",
      P1 = "P1",
      P2 = "P2"
    ),
    startRow = 4,
    startCol = start_col,
    colNames = FALSE
  )
  
  addStyle(
    wb,
    "Regression table",
    subheader_style,
    rows = 4,
    cols = start_col:(start_col + 2),
    gridExpand = TRUE
  )
}


# ===================================================================================
# 45. WRITE BODY
# ===================================================================================

writeData(
  wb,
  "Regression table",
  x = paper_table,
  startRow = 5,
  startCol = 1,
  colNames = FALSE
)

addStyle(
  wb,
  "Regression table",
  body_style,
  rows = 5:(4 + nrow(paper_table)),
  cols = 1:28,
  gridExpand = TRUE
)

addStyle(
  wb,
  "Regression table",
  predictor_style,
  rows = 5:(4 + nrow(paper_table)),
  cols = 1,
  gridExpand = TRUE
)


# ===================================================================================
# 46. SECTION ROWS
# ===================================================================================

acoustic_row <- 5 +
  which(
    paper_table$Predictor == "Acoustic"
  ) - 1

morphological_row <- 5 +
  which(
    paper_table$Predictor == "Morphological PCA"
  ) - 1


addStyle(
  wb,
  "Regression table",
  section_style,
  rows = acoustic_row,
  cols = 1:28,
  gridExpand = TRUE
)

addStyle(
  wb,
  "Regression table",
  section_style,
  rows = morphological_row,
  cols = 1:28,
  gridExpand = TRUE
)


# ===================================================================================
# 47. R² IN BOLD
# ===================================================================================

r2_excel_row <- 5 +
  which(
    paper_table$Predictor == "R²"
  ) - 1

r2_style <- createStyle(
  textDecoration = "bold",
  halign = "center",
  border = "TopBottomLeftRight",
  borderStyle = "thin"
)

addStyle(
  wb,
  "Regression table",
  r2_style,
  rows = r2_excel_row,
  cols = 1:28,
  gridExpand = TRUE
)


# ===================================================================================
# 48. FOOTNOTE
# ===================================================================================

note_row <- 7 +
  nrow(paper_table)

writeData(
  wb,
  "Regression table",
  x = "* p < 0.05, ** p < 0.01. Coefficients are standardized β.",
  startRow = note_row,
  startCol = 1
)

mergeCells(
  wb,
  "Regression table",
  cols = 1:28,
  rows = note_row
)

writeData(
  wb,
  "Regression table",
  x = "Total represents the row-wise mean of P1 and P2.",
  startRow = note_row + 1,
  startCol = 1
)

mergeCells(
  wb,
  "Regression table",
  cols = 1:28,
  rows = note_row + 1
)


# ===================================================================================
# 49. ADJUST WIDE
# ===================================================================================

setColWidths(
  wb,
  "Regression table",
  cols = 1,
  widths = 22
)

setColWidths(
  wb,
  "Regression table",
  cols = 2:28,
  widths = 11
)

setRowHeights(
  wb,
  "Regression table",
  rows = 3:4,
  heights = 22
)

freezePane(
  wb,
  "Regression table",
  firstActiveRow = 5,
  firstActiveCol = 2
)


# ===================================================================================
# 50. COEFFICIENTS SHEET
# ===================================================================================

writeData(
  wb,
  "Coefficient data",
  regression_long
)

addStyle(
  wb,
  "Coefficient data",
  header_style,
  rows = 1,
  cols = 1:ncol(
    regression_long
  ),
  gridExpand = TRUE
)

setColWidths(
  wb,
  "Coefficient data",
  cols = 1:ncol(
    regression_long
  ),
  widths = "auto"
)


# ===================================================================================
# 51. MODEL SUMMARY
# ===================================================================================

model_summary <- regression_results %>%
  select(
    Dependent,
    Period,
    R2,
    Adj_R2,
    N
  ) %>%
  distinct() %>%
  mutate(
    R2 = round(
      R2,
      3
    ),
    Adj_R2 = round(
      Adj_R2,
      3
    )
  ) %>%
  arrange(
    factor(
      Dependent,
      levels = dependent_order
    ),
    factor(
      Period,
      levels = period_order
    )
  )

writeData(
  wb,
  "Model summary",
  model_summary
)

addStyle(
  wb,
  "Model summary",
  header_style,
  rows = 1,
  cols = 1:5,
  gridExpand = TRUE
)

setColWidths(
  wb,
  "Model summary",
  cols = 1:5,
  widths = "auto"
)


# ===================================================================================
# 52. EXPORT EXCEL
# ===================================================================================

output_dir <- "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/6_stats_R/results/"

output_file <- paste0(
  output_dir,
  "sound_aco_morph_mul_reg.xlsx"
)

saveWorkbook(
  wb,
  output_file,
  overwrite = TRUE
)


# ===================================================================================
# 53. FINAL MESSEAGE
# ===================================================================================

cat("\n")
cat("============================================================\n")
cat("ANALYSIS COMPLETED\n")
cat("============================================================\n")

cat("\nPCA:\n")
cat(" - Retained components: ", n_components, "\n", sep = "")
cat(
  " - Cumulative variance: ",
  sprintf(
    "%.2f",
    cumulative_variance[4]
  ),
  "%\n",
  sep = ""
)

cat("\nCorrespondence with component scores:\n")
cat(" - Component 1 = Buildings & Roads\n")
cat(" - Component 2 = Green & Open Space\n")
cat(" - Component 3 = Ground Exposure\n")
cat(" - Component 4 = Square\n")

cat("\nRegressions:\n")
cat(" - 3 acoustic predictors\n")
cat(" - 4 PCA morphological scores\n")
cat(" - 9 DCI × 3 periods = 27 models\n")

cat("\nExcel File:\n")
cat(
  output_file,
  "\n"
)

cat(
  "\nModels: ",
  nrow(model_summary),
  "\n",
  sep = ""
)

cat(
  "Coefficients: ",
  nrow(regression_long),
  "\n",
  sep = ""
)

cat("============================================================\n")
