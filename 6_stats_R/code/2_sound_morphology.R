# authors: Julián Grijalba <juliangrijalba@unimayor.edu.co> and Carlos Duran <duranurbanphony@unimayor.edu.co>
# output: CORREATIONS BETWEEN DOMINANCE SOUNDSCAPE COMPONENT INDICATORS (DSCI) AND MORPHOLOGICAL INDICATORS

# ===================================================================================
# 0. LIBRARIES
# ===================================================================================

library(dplyr)
library(writexl)

# ===================================================================================
# 1. DATA
# ===================================================================================

sound_data <- read.csv(
  "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/data/m_acoustics_sound_comp.csv"
)

morph_data <- read.csv(
  "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/data/m_morph_sound_comp.csv"
)

print(sound_data)
print(morph_data)

# ===================================================================================
# 2. VARIABLES
# ===================================================================================

sound_vars <- names(sound_data)[c(3:8, 10:12, 19:24, 26:28)]

sound_vars_p1 <- sound_vars[grepl("_p1", sound_vars)]
sound_vars_p2 <- sound_vars[grepl("_p2", sound_vars)]

cat("\n--- SOUND ---\n")

print(sound_vars)
print(sound_vars_p1)
print(sound_vars_p2)

morph_vars <- names(morph_data)[1:13]

cat("\n--- MORPHOLOGY ---\n")
print(morph_vars)

# ===================================================================================
# 3. CORRELATION FUNCTION
# ===================================================================================

compute_cor_row <- function(sound_df, morph_df, var, period) {
  
  results <- c()
  
  for (m in morph_vars) {
    
    x <- morph_df[[m]]
    y <- sound_df[[var]]
    
    valid <- complete.cases(x, y)
    
    if (sum(valid) > 3) {
      
      test <- suppressWarnings(
        cor.test(
          x[valid],
          y[valid],
          method = "spearman"
        )
      )
      
      r <- round(test$estimate, 2)
      p <- test$p.value
      
      stars <- ifelse(
        p < 0.01, "**",
        ifelse(p < 0.05, "*", "")
      )
      
      results <- c(
        results,
        paste0(r, stars)
      )
      
    } else {
      results <- c(results, NA)
    }
  }
  
  df_row <- data.frame(
    PII = var,
    Period = period
  )
  
  for (i in seq_along(morph_vars)) {
    df_row[[morph_vars[i]]] <- results[i]
  }
  
  return(df_row)
}

# ===================================================================================
# 4. CALCULATE CORRELATIONS
# ===================================================================================

results_list <- list()

for (v in sound_vars_p1) {
  results_list[[length(results_list) + 1]] <- 
    compute_cor_row(sound_data, morph_data, v, "P1")
}

for (v in sound_vars_p2) {
  results_list[[length(results_list) + 1]] <- 
    compute_cor_row(sound_data, morph_data, v, "P2")
}

for (v1 in sound_vars_p1) {
  
  base <- sub("_p1", "", v1)
  v2 <- paste0(base, "_p2")
  
  if (v2 %in% names(sound_data)) {
    
    sound_total <- sound_data[, c(v1, v2)]
    
    sound_total[[base]] <- rowMeans(
      sound_total[, c(v1, v2)],
      na.rm = FALSE
    )
    
    results_list[[length(results_list) + 1]] <- 
      compute_cor_row(
        sound_total,
        morph_data,
        base,
        "Total"
      )
  }
}

# ===================================================================================
# 5. JOIN RESULTS
# ===================================================================================

cor_table <- do.call(rbind, results_list)

cat("\n--- RESULTS ---\n")
print(colnames(cor_table))

# ===================================================================================
# 6. RENAME COLUMNS
# ===================================================================================

name_map <- c(
  "bldg_A" = "Bldg(A)",
  "bldg_P" = "Bldg(P)",
  "gr_A" = "Gr(A)",
  "gr_P" = "Gr(P)",
  "sq_A" = "Sq(A)",
  "sq_P" = "Sq(P)",
  "op_A" = "Op(A)",
  "op_P" = "Op(P)",
  "grd_A" = "Grd(A)",
  "rd_A" = "Rd(A)"
)

names(cor_table) <- sapply(names(cor_table), function(x) {
  if (x %in% names(name_map)) name_map[[x]] else x
})

cat("\n--- RENAME COLUMNS ---\n")
print(colnames(cor_table))

# ===================================================================================
# 7. FINAL ORDER
# ===================================================================================

cor_table$PII <- toupper(gsub("_p[12]|_202", "", cor_table$PII)) # clean PII

pii_order <- c("TM","MH","VO","MU","EM","SO","BI","GE","SI")

cor_table$PII <- factor(cor_table$PII, levels = pii_order)
cor_table$Period <- factor(cor_table$Period, levels = c("P1","P2","Total"))

cor_table <- cor_table[order(cor_table$PII, cor_table$Period), ]

# ===================================================================================
# 8. FINAL NAMES
# ===================================================================================

cor_table <- cor_table[, c(
  "PII", "Period",
  "Bldg(A)", "Bldg(P)", "BPAF",
  "Gr(A)", "Gr(P)",
  "Sq(A)", "Sq(P)",
  "Op(A)", "Op(P)",
  "Rd(A)", "RAF",
  "Grd(A)", "EGR"
)]

cat("\n--- FINAL NAMES ---\n")
print(colnames(cor_table))

# ===================================================================================
# 9. FINAL OUTCOME
# ===================================================================================

cat("\n--- TABLA FINAL ---\n")
print(head(cor_table, 10))

# ===================================================================================
# 10. EXPORT
# ===================================================================================

write_xlsx(
  cor_table,
  "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/6_stats_R/results/sound_morphology_corr.xlsx"
)