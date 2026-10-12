# authors: Julián Grijalba <juliangrijalba@unimayor.edu.co> and Carlos Duran <duranurbanphony@unimayor.edu.co>
# output: CORREATIONS BETWEEN DOMINANCE SOUNDSCAPE COMPONENT INDICATORS (DSCI) AND ACOUSTIC INDICATORS

# ===================================================================================
# 0. LIBRARIES
# ===================================================================================

library(dplyr)
library(tidyr)
library(writexl)

# ===================================================================================
# 1. DATA
# ===================================================================================

sound_p1 <- read.csv("C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/results/sound_p1.csv")
sound_p2 <- read.csv("C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/results/sound_p2.csv")

# ===================================================================================
# 2. VARIABLES
# ===================================================================================

clases <- c("TM", "MH", "VO", "MU", "EM", "SO", "BI", "GE", "SI")
vars_ac <- c("LAeq", "LAeq_LCeq", "L10_L90")

head(sound_p1)
head(sound_p2)

# P1
sound_p1 <- sound_p1 %>%
  rename(
    LAeq = LAeq_p1,
    LAeq_LCeq = LAeq_LCeq_p1,
    L10_L90 = L10_L90_p1
  )

# P2
sound_p2 <- sound_p2 %>%
  rename(
    LAeq = LAeq_p2,
    LAeq_LCeq = LAeq_LCeq_p2,
    L10_L90 = L10_L90_p2
  )

# ===================================================================================
# 3. FUNCTION FOR CORRELATIONS
# ===================================================================================

cor_table <- function(df, period_name, clases, vars_ac) {
  
  results <- list()
  
  k <- 1
  
  for (cl in clases) {
    
    for (v in vars_ac) {
      
      x <- df[[cl]]
      y <- df[[v]]
      
      valid <- complete.cases(x, y)
      x <- x[valid]
      y <- y[valid]
      
      if (length(x) > 2) {
        
        test <- cor.test(x, y, method = "spearman")
        
        r <- unname(test$estimate)
        p <- test$p.value
        
        r_round <- round(r, 2)
        
        stars <- if (p < 0.01) {
          "**"
        } else if (p < 0.05) {
          "*"
        } else {
          ""
        }
        
        value <- paste0(r_round, stars)
        
      } else {
        value <- NA
      }
      
      results[[k]] <- data.frame(
        Class = cl,
        Variable = v,
        Value = value,
        Period = period_name
      )
      
      k <- k + 1
    }
  }
  
  bind_rows(results)
}

# ===================================================================================
# 4. CORRELATIONS PER PERIOD
# ===================================================================================

corr_p1 <- cor_table(sound_p1, "P1", clases, vars_ac)
corr_p2 <- cor_table(sound_p2, "P2", clases, vars_ac)

# ===================================================================================
# 5. DATASET
# ===================================================================================

sound_total <- bind_rows(sound_p1, sound_p2)
corr_total <- cor_table(sound_total, "Total", clases, vars_ac)

# ===================================================================================
# 6. FINAL JOIN
# ===================================================================================

corr_all <- bind_rows(corr_p1, corr_p2, corr_total)

# ===================================================================================
# 7. FINAL TABLE
# ===================================================================================

table_final <- corr_all %>%
  pivot_wider(
    names_from = Variable,
    values_from = Value
  ) %>%
  mutate(
    Period = factor(Period, levels = c("P1", "P2", "Total"))
  ) %>%
  arrange(Class, Period)

table_final <- table_final %>%
  mutate(
    Class = factor(Class, levels = clases),
    Period = factor(Period, levels = c("P1", "P2", "Total"))
  ) %>%
  arrange(Class, Period)

# ===================================================================================
# 8. RESULTS
# ===================================================================================

print(table_final, n = Inf)
write_xlsx(table_final, "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/6_stats_R/results/sound_acoustic_corr.xlsx")

