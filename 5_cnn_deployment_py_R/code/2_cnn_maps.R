# authors: Julián Grijalba <juliangrijalba@unimayor.edu.co> and Carlos Duran <duranurbanphony@unimayor.edu.co>
# output: interpolation surfaces of urban soudnscape composition

library(sp)
library(raster) 
library(geoR)
library(RColorBrewer) 
library(sf)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# ===================================================================================
# 01. DATA
# ===================================================================================

## Phonic Interaction Indices (STI)

pii_data <- read.csv("C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/results/VNE_mels_inf.csv")

## Acoustic indicators
aco_p1 <- read.csv("C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/data/day_2023_unified.csv", sep = ",")
aco_p2 <- read.csv("C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/data/night_2023_unified.csv", sep = ",")

# Add acoustic spectral and temporal indicators

aco_p1 <- aco_p1 %>%
  mutate(
    LAeq_LCeq = .[[2]] - .[[3]],
    L10_L90 = .[[4]] - .[[5]],
  )

aco_p2 <- aco_p2 %>%
  mutate(
    LAeq_LCeq = .[[2]] - .[[3]],
    L10_L90 = .[[4]] - .[[5]],
  )

## Coordinates
coor <- read.csv("C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/data/vne_coor_ok.csv", sep = ",")

# ===================================================================================
# 02. THRESHOLDS BETA = 0.75
# ===================================================================================
tm_threshold <- 0.4252
mh_threshold <- 0.3594
vo_threshold <- 0.4299
mu_threshold <- 0.4888
em_threshold <- 0.4150
so_threshold <- 0.4360
bi_threshold <- 0.4592
ge_threshold <- 0.5096
si_threshold <- 0.5759

# ===================================================================================
# 03. BINARY CLASSIFICATION FOR COMPONENTS
# ===================================================================================
pii_morse <- pii_data %>%
  mutate(
    TM = as.integer(TM >= tm_threshold),
    MH = as.integer(MH >= mh_threshold),
    VO = as.integer(VO >= vo_threshold),
    MU = as.integer(MU >= mu_threshold),
    EM = as.integer(EM >= em_threshold),
    SO = as.integer(SO >= so_threshold),
    BI = as.integer(BI >= bi_threshold),
    GE = as.integer(GE >= ge_threshold),
    SI = as.integer(SI >= si_threshold)
  )

# ===================================================================================
# 04. BINARY CLASSIFICATION FOR MAIN-CATEGORIES
# ===================================================================================
pii_morse <- pii_morse %>%
  mutate(
    A = as.integer(TM | MH | VO | MU | EM | SO),  # Anthropophony
    N = as.integer(BI | GE),                     # Naturaphony
  )

# ===================================================================================
# 05. % UNIDETIFIED
# ===================================================================================
zero_rows <- sum(rowSums(pii_morse[, c("TM","MH","VO","MU","EM","SO","BI","GE","SI")]) == 0)
cat("Unidentified avarage:", round(zero_rows / nrow(pii_morse) * 100, 2), "%\n")

# ===================================================================================
# 06. BINARY CLASSIFICATION AND POINT AND PERIOD ASSIGMENT FOR MAPS
# ===================================================================================
pii_morse_maps <- pii_morse %>%
  mutate(
    Punto = as.integer(str_extract(PNG_name, "^\\d{3}")),
    Periodo = str_extract(PNG_name, "P[12]")
  ) %>%
  group_by(Punto, Periodo) %>%
  mutate(Segmento = row_number()) %>%
  ungroup()

# ===================================================================================
# 07. DATASET BY POINT AND PERIOD AND CORREPONDING SUM OF DETECTIONS
# ===================================================================================
pii_maps <- pii_morse_maps %>%
  group_by(Punto, Periodo) %>%
  summarise(
    
    A = sum(A),
    N = sum(N),
    
    TM = sum(TM),
    MH = sum(MH),
    VO = sum(VO),
    MU = sum(MU),
    EM = sum(EM),
    SO = sum(SO),
    
    BI = sum(BI),
    GE = sum(GE),
    SI = sum(SI),
    
    T = n(),
    
    .groups = "drop"
  )

# ===================================================================================
# 08. DSMI (DOMINANCE SOUNDSCAPE MAIN-CATEGORY INDEX) 
#     AND
#     DSCI (DOMINANCE SOUNDSCAPE COMPONENT INDEX)
# ===================================================================================

pii_maps <- pii_maps %>%
  mutate(
    
    # Normalized dominance (0–1)
    TM = TM / T,
    MH = MH / T,
    VO = VO / T,
    MU = MU / T,
    EM = EM / T,
    SO = SO / T,
    
    BI = BI / T,
    GE = GE / T,
    SI = SI / T,
    
    A = A / T,
    N = N / T
  ) %>%
  mutate(
    
    # Convert dominance indices to percentages (0–100)
    TM = round(TM * 100, 0),
    MH = round(MH * 100, 0),
    VO = round(VO * 100, 0),
    MU = round(MU * 100, 0),
    EM = round(EM * 100, 0),
    SO = round(SO * 100, 0),
    
    BI = round(BI * 100, 0),
    GE = round(GE * 100, 0),
    SI = round(SI * 100, 0),
    
    A = round(A * 100, 0),
    N = round(N * 100, 0)
  )

# ===================================================================================
# 09. ANBI (ANTROPOPHONY AND NATURAPHONY BALANCE INDEX)
# ===================================================================================
pii_maps <- pii_maps %>%
  mutate(
    ANBI = ifelse((A + N) == 0,
                 NA,
                 (N - A) / (N + A))
  )

# ===================================================================================
# 10. RTI (RED AND TRAFFIC INDEX)
# ===================================================================================

RTI_maps <- pii_morse_maps %>%
  mutate(
    R_presence = as.integer(MH == 1 | VO == 1 | MU == 1)
  ) %>%
  group_by(Punto, Periodo) %>%
  summarise(
    RTI = mean(TM * R_presence),
    .groups = "drop"
  )

# ===================================================================================
# 11. GTI (GREEN AND TRAFFIC INDEX)
# ===================================================================================

GTI_maps <- pii_morse_maps %>%
  mutate(
    N_presence = as.integer(BI == 1 | GE == 1)
  ) %>%
  group_by(Punto, Periodo) %>%
  summarise(
    GTI = mean(TM * N_presence),
    .groups = "drop"
  )

# ===================================================================================
# 12. AEI (ANTHROPOPHONY ENTROPY INDEX)
# ===================================================================================

# Function

calc_AEI_weighted <- function(df) {
  
  df <- as.data.frame(df)
  
  # Total counts for each anthropophony component (n_ca)
  n_ca <- colSums(df)
  
  # Total anthropophony (A_tot)
  A_tot <- sum(n_ca)
  
  # If no anthropophony is present, return NA
  if (A_tot == 0) return(NA_real_)
  
  # Relative contribution of each anthropophony component (p_ca)
  p_ca <- n_ca / A_tot
  p_ca <- p_ca[p_ca > 0]
  
  # Shannon Entropy H'
  H <- -sum(p_ca * log(p_ca))
  
  # Normalized AEI (0–1), with 6 anthropophony components
  AEI <- H / log(6)
  
  # Anthropophony intensity factor
  A_factor <- (A_tot / (A_tot + 20)) * (560 / 540)
  
  # Final weighted Anthropophony Diversity Index AEI_w
  AEI_w <- AEI * A_factor
  
  return(AEI_w)
}

# Apply function

AEI_maps <- pii_morse_maps %>%
  group_by(Punto, Periodo) %>%
  summarise(
    
    AEI_weighted = calc_AEI_weighted(
      across(c("TM","MH","VO","MU","EM","SO"))
    ),
    
    .groups = "drop"
  )

# ===================================================================================
# 13. NEI (NATURAPHONY ENTROPY INDEX)
# ===================================================================================

# Function

calc_NEI_weighted <- function(df) {
  
  df <- as.data.frame(df)
  
  # Total counts for each naturaphony component (n_ca)
  n_ca <- colSums(df)
  
  # Total naturaphony (N_tot)
  N_tot <- sum(n_ca)
  
  # If no naturaphony is present, return NA
  if (N_tot == 0) return(NA_real_)
  
  # Relative contribution of each naturaphony component (p_ca)
  p_ca <- n_ca / N_tot
  p_ca <- p_ca[p_ca > 0]
  
  # Shannon Entropy H'
  H <- -sum(p_ca * log(p_ca))
  
  # Normalized NEI (0–1), with 2 naturaphony components
  NEI <- H / log(2)
  
  # Naturapophony intensity factor
  N_factor <- (N_tot / (N_tot + 20)) * (560 / 540)
  
  # Final weighted Naturaphony Diversity Index NEI_w
  NEI_w <- NEI * N_factor
  
  return(NEI_w)
}

# Apply function

NEI_maps <- pii_morse_maps %>%
  group_by(Punto, Periodo) %>%
  summarise(
    
    NEI_weighted = calc_NEI_weighted(
      across(c("BI","GE"))
    ),
    
    .groups = "drop"
  )

# ===================================================================================
# 14. JOIN ALL STI
# ===================================================================================
pii_maps <- pii_maps %>%
  left_join(RTI_maps, by = c("Punto","Periodo"))%>%
  left_join(GTI_maps, by = c("Punto","Periodo")) %>%
  left_join(AEI_maps, by = c("Punto","Periodo"))%>%
  left_join(NEI_maps, by = c("Punto","Periodo"))

# ===================================================================================
# 15. DIVIDE INTO TWO DATASETS BY PERIOD
# ===================================================================================
pii_maps_p1 <- pii_maps[pii_maps$Periodo == "P1", ]
pii_maps_p2 <- pii_maps[pii_maps$Periodo == "P2", ]

# ===================================================================================
# 16. CHECK DATASETS
# ===================================================================================
lapply(list(pii_maps_p1, pii_maps_p2), head, 5)

### BASIC STATS FROM HERE ------------------------------------------------------------
#-------------------------------------------------------------------------------------

# ===================================================================================
# 17. Anthropophony component contributions (by period and global)
# ===================================================================================

anthro_period <- pii_morse_maps %>%
  group_by(Periodo) %>%
  summarise(
    TM_tot = sum(TM), MH_tot = sum(MH), VO_tot = sum(VO),
    MU_tot = sum(MU), EM_tot = sum(EM), SO_tot = sum(SO),
    A_tot  = TM_tot + MH_tot + VO_tot + MU_tot + EM_tot + SO_tot,
    .groups = "drop"
  ) %>%
  mutate(
    TM_pct = TM_tot / A_tot * 100,
    MH_pct = MH_tot / A_tot * 100,
    VO_pct = VO_tot / A_tot * 100,
    MU_pct = MU_tot / A_tot * 100,
    EM_pct = EM_tot / A_tot * 100,
    SO_pct = SO_tot / A_tot * 100
  )

anthro_global <- pii_morse_maps %>%
  summarise(
    TM_tot = sum(TM), MH_tot = sum(MH), VO_tot = sum(VO),
    MU_tot = sum(MU), EM_tot = sum(EM), SO_tot = sum(SO),
    A_tot  = TM_tot + MH_tot + VO_tot + MU_tot + EM_tot + SO_tot
  ) %>%
  mutate(
    TM_pct = TM_tot / A_tot * 100,
    MH_pct = MH_tot / A_tot * 100,
    VO_pct = VO_tot / A_tot * 100,
    MU_pct = MU_tot / A_tot * 100,
    EM_pct = EM_tot / A_tot * 100,
    SO_pct = SO_tot / A_tot * 100,
    Periodo = "Global"
  )

anthro_all_long <- bind_rows(anthro_period, anthro_global) %>%
  select(Periodo, TM_pct, MH_pct, VO_pct, MU_pct, EM_pct, SO_pct) %>%
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "Component",
    values_to = "Contribution_pct"
  ) %>%
  mutate(
    Component = gsub("_pct", "", Component),
    Component = factor(Component,
                       levels = c("TM","MH","VO","MU","EM","SO"),
                       labels = c("MT","HM","VO","MU","EM","SO"))
  )

legend_df_anthro <- anthro_all_long %>%
  pivot_wider(
    id_cols = Component,
    names_from = Periodo,
    values_from = Contribution_pct
  ) %>%
  mutate(
    label = paste0(
      Component, " (",
      "Global: ", sprintf("%.1f%%", Global), ", ",
      "P1: ", sprintf("%.1f%%", P1), ", ",
      "P2: ", sprintf("%.1f%%", P2),
      ")"
    )
  )

ggplot(anthro_all_long, aes(x = Periodo, y = Contribution_pct, fill = Component)) +
  geom_col() +
  scale_fill_manual(
    values = c("#56B4E9", "#E69F00", "#F0E442", "#009E73", "#D55E00", "#0072B2"),
    breaks = legend_df_anthro$Component,
    labels = legend_df_anthro$label
  ) +
  labs(
    title = "Anthropophony components",
    x = "Period",
    y = "Contribution (%)",
    fill = "Component"
  ) +
  theme_minimal()

# ===================================================================================
# 18. Naturaphony component contributions (by period and global)
# ===================================================================================

# By period: percentage contribution of each naturaphony component to total naturaphony
natura_period <- pii_morse_maps %>%
  group_by(Periodo) %>%
  summarise(
    BI_tot = sum(BI),
    GE_tot = sum(GE),
    N_tot  = BI_tot + GE_tot,
    .groups = "drop"
  ) %>%
  mutate(
    BI_pct = BI_tot / N_tot * 100,
    GE_pct = GE_tot / N_tot * 100
  )

# Global: percentage contribution of each naturaphony component to global naturaphony
natura_global <- pii_morse_maps %>%
  summarise(
    BI_tot = sum(BI),
    GE_tot = sum(GE),
    N_tot  = BI_tot + GE_tot
  ) %>%
  mutate(
    BI_pct = BI_tot / N_tot * 100,
    GE_pct = GE_tot / N_tot * 100,
    Periodo = "Global"
  )

# Long format for plotting
natura_period_long <- natura_period %>%
  select(Periodo, BI_pct, GE_pct) %>%
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "Component",
    values_to = "Contribution_pct"
  ) %>%
  mutate(
    Component = gsub("_pct", "", Component)
  )

natura_global_long <- natura_global %>%
  select(Periodo, BI_pct, GE_pct) %>%
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "Component",
    values_to = "Contribution_pct"
  ) %>%
  mutate(
    Component = gsub("_pct", "", Component)
  )

natura_all_long <- bind_rows(natura_period_long, natura_global_long) %>%
  mutate(
    Component = factor(Component, levels = c("BI","GE"))
  )

# Labels
legend_df_natura <- natura_all_long %>%
  reframe(
    Global = Contribution_pct[Periodo == "Global"],
    P1     = Contribution_pct[Periodo == "P1"],
    P2     = Contribution_pct[Periodo == "P2"],
    .by    = Component
  ) %>%
  mutate(
    label = paste0(
      Component, " (",
      "Global: ", sprintf("%.1f%%", Global), ", ",
      "P1: ", sprintf("%.1f%%", P1), ", ",
      "P2: ", sprintf("%.1f%%", P2),
      ")"
    )
  )

# Stacked bar plot: naturaphony component contributions by period and global
ggplot(natura_all_long, aes(x = Periodo, y = Contribution_pct, fill = Component)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("#FF29C9", "#AA44F7"),  # BI, GE
    breaks = legend_df_natura$Component,
    labels = legend_df_natura$label
  ) +
  labs(
    title = "Naturaphony components",
    x = "Period",
    y = "Contribution (%)",
    fill = "Component"
  ) +
  theme_minimal()

# ===================================================================================
# 19. Anthropophony vs Naturaphony contributions (by period and global)
# ===================================================================================

# By period: total A and N and their relative contributions
AN_period <- pii_morse_maps %>%
  group_by(Periodo) %>%
  summarise(
    A_tot = sum(A),
    N_tot = sum(N),
    AN_tot = A_tot + N_tot,
    .groups = "drop"
  ) %>%
  mutate(
    A_pct = A_tot / AN_tot * 100,
    N_pct = N_tot / AN_tot * 100
  )

# Global
AN_global <- pii_morse_maps %>%
  summarise(
    A_tot = sum(A),
    N_tot = sum(N),
    AN_tot = A_tot + N_tot
  ) %>%
  mutate(
    A_pct = A_tot / AN_tot * 100,
    N_pct = N_tot / AN_tot * 100,
    Periodo = "Global"
  )

AN_period_long <- AN_period %>%
  select(Periodo, A_pct, N_pct) %>%
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "Category",
    values_to = "Contribution_pct"
  ) %>%
  mutate(
    Category = gsub("_pct", "", Category)
  )

AN_global_long <- AN_global %>%
  select(Periodo, A_pct, N_pct) %>%
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "Category",
    values_to = "Contribution_pct"
  ) %>%
  mutate(
    Category = gsub("_pct", "", Category)
  )

AN_all_long <- bind_rows(AN_period_long, AN_global_long) %>%
  mutate(
    Category = factor(Category, levels = c("A","N"))
  )

# Labels
legend_df_AN <- AN_all_long %>%
  reframe(
    Global = Contribution_pct[Periodo == "Global"],
    P1     = Contribution_pct[Periodo == "P1"],
    P2     = Contribution_pct[Periodo == "P2"],
    .by    = Category
  ) %>%
  mutate(
    label = paste0(
      Category, " (",
      "Global: ", sprintf("%.1f%%", Global), ", ",
      "P1: ", sprintf("%.1f%%", P1), ", ",
      "P2: ", sprintf("%.1f%%", P2),
      ")"
    )
  )

# Stacked bar plot: anthropophony vs naturaphony contributions
ggplot(AN_all_long, aes(x = Periodo, y = Contribution_pct, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("#E69F00", "#56B4E9"),  # A, N
    breaks = legend_df_AN$Category,
    labels = legend_df_AN$label
  ) +
  labs(
    title = "Anthropophony and Naturaphony",
    x = "Period",
    y = "Contribution (%)",
    fill = "Category"
  ) +
  theme_minimal()

# ===================================================================================
# 20. Anthropophony, Naturaphony and Silence contributions (by period and global)
# ===================================================================================

ANS_period <- pii_morse_maps %>%
  group_by(Periodo) %>%
  summarise(
    A_tot = sum(A),
    N_tot = sum(N),
    SI_tot = sum(SI),
    ANS_tot = A_tot + N_tot + SI_tot,
    .groups = "drop"
  ) %>%
  mutate(
    A_pct  = A_tot  / ANS_tot * 100,
    N_pct  = N_tot  / ANS_tot * 100,
    SI_pct = SI_tot / ANS_tot * 100
  )

ANS_global <- pii_morse_maps %>%
  summarise(
    A_tot = sum(A),
    N_tot = sum(N),
    SI_tot = sum(SI),
    ANS_tot = A_tot + N_tot + SI_tot
  ) %>%
  mutate(
    A_pct  = A_tot  / ANS_tot * 100,
    N_pct  = N_tot  / ANS_tot * 100,
    SI_pct = SI_tot / ANS_tot * 100,
    Periodo = "Global"
  )

ANS_period_long <- ANS_period %>%
  select(Periodo, A_pct, N_pct, SI_pct) %>%
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "Category",
    values_to = "Contribution_pct"
  ) %>%
  mutate(
    Category = gsub("_pct", "", Category)
  )

ANS_global_long <- ANS_global %>%
  select(Periodo, A_pct, N_pct, SI_pct) %>%
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "Category",
    values_to = "Contribution_pct"
  ) %>%
  mutate(
    Category = gsub("_pct", "", Category)
  )

ANS_all_long <- bind_rows(ANS_period_long, ANS_global_long) %>%
  mutate(
    Category = factor(Category, levels = c("A","N","SI"))
  )

# Labels
legend_df_ANS <- ANS_all_long %>%
  reframe(
    Global = Contribution_pct[Periodo == "Global"],
    P1     = Contribution_pct[Periodo == "P1"],
    P2     = Contribution_pct[Periodo == "P2"],
    .by    = Category
  ) %>%
  mutate(
    label = paste0(
      Category, " (",
      "Global: ", sprintf("%.1f%%", Global), ", ",
      "P1: ", sprintf("%.1f%%", P1), ", ",
      "P2: ", sprintf("%.1f%%", P2),
      ")"
    )
  )

# Plot
ggplot(ANS_all_long, aes(x = Periodo, y = Contribution_pct, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(
    breaks = legend_df_ANS$Category,
    labels = legend_df_ANS$label
  ) +
  labs(
    title = "Anthropophony, Naturaphony and Silence",
    x = "Period",
    y = "Contribution (%)",
    fill = "Category"
  ) +
  theme_minimal()

# ===================================================================================
# 21. Global contributions of all 9 components to total detections (by period and global)
# ===================================================================================

# By period: total detections per component and global total
comp_period <- pii_morse_maps %>%
  group_by(Periodo) %>%
  summarise(
    TM_tot = sum(TM),
    MH_tot = sum(MH),
    VO_tot = sum(VO),
    MU_tot = sum(MU),
    EM_tot = sum(EM),
    SO_tot = sum(SO),
    BI_tot = sum(BI),
    GE_tot = sum(GE),
    SI_tot = sum(SI),
    ALL_tot = TM_tot + MH_tot + VO_tot + MU_tot + EM_tot + SO_tot +
      BI_tot + GE_tot + SI_tot,
    .groups = "drop"
  ) %>%
  mutate(
    TM_pct = TM_tot / ALL_tot * 100,
    MH_pct = MH_tot / ALL_tot * 100,
    VO_pct = VO_tot / ALL_tot * 100,
    MU_pct = MU_tot / ALL_tot * 100,
    EM_pct = EM_tot / ALL_tot * 100,
    SO_pct = SO_tot / ALL_tot * 100,
    BI_pct = BI_tot / ALL_tot * 100,
    GE_pct = GE_tot / ALL_tot * 100,
    SI_pct = SI_tot / ALL_tot * 100
  )

# Global
comp_global <- pii_morse_maps %>%
  summarise(
    TM_tot = sum(TM),
    MH_tot = sum(MH),
    VO_tot = sum(VO),
    MU_tot = sum(MU),
    EM_tot = sum(EM),
    SO_tot = sum(SO),
    BI_tot = sum(BI),
    GE_tot = sum(GE),
    SI_tot = sum(SI),
    ALL_tot = TM_tot + MH_tot + VO_tot + MU_tot + EM_tot + SO_tot +
      BI_tot + GE_tot + SI_tot
  ) %>%
  mutate(
    TM_pct = TM_tot / ALL_tot * 100,
    MH_pct = MH_tot / ALL_tot * 100,
    VO_pct = VO_tot / ALL_tot * 100,
    MU_pct = MU_tot / ALL_tot * 100,
    EM_pct = EM_tot / ALL_tot * 100,
    SO_pct = SO_tot / ALL_tot * 100,
    BI_pct = BI_tot / ALL_tot * 100,
    GE_pct = GE_tot / ALL_tot * 100,
    SI_pct = SI_tot / ALL_tot * 100,
    Periodo = "Global"
  )

comp_period_long <- comp_period %>%
  select(Periodo,
         TM_pct, MH_pct, VO_pct, MU_pct, EM_pct, SO_pct,
         BI_pct, GE_pct, SI_pct) %>%
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "Component",
    values_to = "Contribution_pct"
  ) %>%
  mutate(
    Component = gsub("_pct", "", Component)
  )

comp_global_long <- comp_global %>%
  select(Periodo,
         TM_pct, MH_pct, VO_pct, MU_pct, EM_pct, SO_pct,
         BI_pct, GE_pct, SI_pct) %>%
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "Component",
    values_to = "Contribution_pct"
  ) %>%
  mutate(
    Component = gsub("_pct", "", Component)
  )

comp_all_long <- bind_rows(comp_period_long, comp_global_long) %>%
  mutate(
    Component = factor(
      Component,
      levels = c("TM","MH","VO","MU","EM","SO","BI","GE","SI"),
      labels = c("MT","HM","VO","MU","EM","SO","BI","GE","SI")
    )
  )

# Labels
legend_df_comp <- comp_all_long %>%
  reframe(
    Global = Contribution_pct[Periodo == "Global"],
    P1     = Contribution_pct[Periodo == "P1"],
    P2     = Contribution_pct[Periodo == "P2"],
    .by    = Component
  ) %>%
  mutate(
    label = paste0(
      Component, " (",
      "Global: ", sprintf("%.1f%%", Global), ", ",
      "P1: ", sprintf("%.1f%%", P1), ", ",
      "P2: ", sprintf("%.1f%%", P2),
      ")"
    )
  )

# Plot: contributions of all 9 components to total detections
ggplot(comp_all_long, aes(x = Periodo, y = Contribution_pct, fill = Component)) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(
    breaks = legend_df_comp$Component,
    labels = legend_df_comp$label
  ) +
  labs(
    title = "Contributions of all components",
    x = "Period",
    y = "Contribution (%)",
    fill = "Component"
  ) +
  theme_minimal()

### MAPS FROM HERE ------------------------------------------------------------------
#------------------------------------------------------------------------------------

#########        ####
###   ###        ####
###   ###        ####
########         ####
###              ####
###              ####
###              ####

# ===================================================================================
# 22. MAPS FOR P1
# ===================================================================================

# Join PII and acoustic indicators with coordinates
head(coor)

sound_p1 <- merge(
  pii_maps_p1,
  coor,
  by.x = "Punto",
  by.y = "puntos_un",
  all.x = TRUE
)

head(sound_p1)
head(aco_p1)

sound_p1 <- merge(
  sound_p1,
  aco_p1,
  by.x = "Punto",
  by.y = "puntos_un",
  all.x = TRUE
)

head(sound_p1)

# Convert NAs to 0

sound_p1 <- sound_p1 %>%
  mutate(
    across(everything(), ~replace(.x, is.na(.x), 0))
  )

head(sound_p1)

# Transform EPSG:3857 -> EPSG:25833
coor_sf <- st_as_sf(
  sound_p1,
  coords = c("POINT_X", "POINT_Y"),
  crs = 3857,
  remove = FALSE
)

coor_sf <- st_transform(
  coor_sf,
  crs = 25833
)

coords_25833 <- st_coordinates(coor_sf)

sound_p1$POINT_X <- coords_25833[,1]
sound_p1$POINT_Y <- coords_25833[,2]

# Add spatial reference to soundscape data
sound_space_p1 = as.geodata(sound_p1, coords.col = 20:21, data.col = 3)

# Plot points
sound_points <- as.data.frame(sound_space_p1$coords)

ggplot(sound_points, aes(x = POINT_X, y = POINT_Y)) +
  geom_point(size = 2, color = "black") +
  labs(title = "Sampling points", x = "X", y = "Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5
    )
  )

# Variogram cloud
summary(dist(sound_space_p1$coords))
cloud <- variog(sound_space_p1, option = "cloud")

# Convert cloud to data frame
sound_v_cloud <- data.frame(
  u = cloud$u,
  v = cloud$v
)

# Plot
ggplot(sound_v_cloud, aes(x = u, y = v)) +
  geom_point(alpha = 0.4, color = "black") +
  labs(
    title = "Variogram cloud",
    x = "Lag distance",
    y = "Semivariance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5   # centra el título
    )
  )

# Experimental variogram
variogram <- variog(sound_space_p1, option = "bin", uvec = seq(0,3000,200), estimator.type = "classic")

# Convert experimental variogram to data frame
sound_variogram <- data.frame(
  u = variogram$u,
  v = variogram$v
)

#Plot
ggplot(sound_variogram, aes(x = u, y = v)) +
  geom_point(size = 2, color = "black") +
  labs(
    title = "Experimental semivariogram A P1",
    x = "Lag distance",
    y = "Semivariance"
  ) +
  expand_limits(y = 0) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10)
  )

ggsave(
  filename = "variogram_A_P1.jpeg",
  plot = last_plot(),
  path = "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/results/geo_analysis",
  width = 10,
  height = 8,
  dpi = 300,
  scale = 0.6
)

# Fit theorical models
ini.vals <- expand.grid(sigmasq = seq(0, 600, length.out = 10),
                        phi = seq(0, 2000, length.out = 10))

model_mco_exp = variofit(variogram, ini = ini.vals, cov.model = "exponential", fix.nug = TRUE, wei = "npair", min = "optim")
model_mco_gaus = variofit(variogram, ini = ini.vals, cov.model = "gaussian", fix.nug = TRUE, wei = "npair", min = "optim")
model_mco_spe = variofit(variogram, ini = ini.vals, cov.model = "spheric", fix.nug = TRUE, wei = "npair", min = "optim")

# Parameters
sigma_exp <- model_mco_exp$cov.pars[1]
phi_exp   <- model_mco_exp$cov.pars[2]

sigma_gaus <- model_mco_gaus$cov.pars[1]
phi_gaus   <- model_mco_gaus$cov.pars[2]

sigma_spe <- model_mco_spe$cov.pars[1]
phi_spe   <- model_mco_spe$cov.pars[2]

# Sequence of distances
h_seq <- seq(0, max(sound_variogram$u), length.out = 200)

# Compute theoretical curves
gamma_exp <- sigma_exp * (1 - exp(-h_seq / phi_exp))
gamma_gaus <- sigma_gaus * (1 - exp(-(h_seq / phi_gaus)^2))
gamma_spe <- ifelse(h_seq < phi_spe,
                    sigma_spe * (1.5 * (h_seq / phi_spe) - 0.5 * (h_seq / phi_spe)^3),
                    sigma_spe)

# Data frame para las líneas
sound_models <- data.frame(
  h = h_seq,
  exponential = gamma_exp,
  gaussian = gamma_gaus,
  spherical = gamma_spe
)


### Plot fitted theoretical models

ggplot(sound_variogram, aes(x = u, y = v)) +
  geom_point(size = 2, color = "black") +
  
  # Exponential
  geom_line(
    data = sound_models,
    aes(x = h, y = exponential),
    color = "#CC00FF",
    linewidth = 1.0,
  ) +
  
  # Gaussian
  geom_line(
    data = sound_models,
    aes(x = h, y = gaussian),
    color = "#FF0000",
    linewidth = 1.0,
  ) +
  
  # Spherical
  geom_line(
    data = sound_models,
    aes(x = h, y = spherical),
    color = "blue",
    linewidth = 1.0,
  ) +
  
  labs(
    title = "Fitted theoretical models A P1",
    x = "Lag distance",
    y = "Semivariance",
    caption = "Purple = Exponential | Red = Gaussian | Blue = Spherical"
  ) +
  expand_limits(y = 0) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5     
    ),
    plot.caption = element_text(
      size = 10,
      hjust = 0.5    
    )
  )

ggsave(
  filename = "fitted_models_A_P1.jpeg",
  plot = last_plot(),
  path = "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/results/geo_analysis",
  width = 10,
  height = 8,
  dpi = 300,
  scale = 0.6
)

# List of models
models <- list(
  exponential = model_mco_exp,
  gaussian    = model_mco_gaus,
  spherical   = model_mco_spe
)

# Extract loss values
loss_values <- sapply(models, function(m) m$value)
print(loss_values)

# Select best model
best_name <- names(which.min(loss_values))
best_model <- models[[best_name]]

cat("\n✅ Best model selected:", best_name,"\n")
cat("Model:", best_name, "\n")
cat("Loss value:", min(loss_values), "\n")
cat("Sigma^2 (variance):", best_model$cov.pars[1], "\n")
cat("Phi (range):", best_model$cov.pars[2], "\n")

# Kriging

xrange <- range(sound_p1$POINT_X)
yrange <- range(sound_p1$POINT_Y)

margen <- 1200

tabla_grid <- expand.grid(
  Este = seq(xrange[1] - margen, xrange[2] + margen, length.out = 500),
  Norte = seq(yrange[1] - margen, yrange[2] + margen, length.out = 500)
)

tabla_ko = krige.conv(
  sound_space_p1,
  loc = tabla_grid,
  krige = krige.control(
    nugget = best_model$nugget,
    cov.pars = best_model$cov.pars,
    trend.d = "cte",
    trend.l = "cte"
  )
)

# Colors for plot
colors_discrete <- c(
  "#08306B",
  "#08519C",
  "#2171B5",
  "#6BAED6",
  "#C6DBEF",
  "#FEE08B",
  "#FDAE61",
  "#F46D43",
  "#D73027",
  "#A50026"
)

# Kriging surface data frame
df_ko_sf <- cbind(tabla_grid, predict = tabla_ko$predict)

# Prediction plot
ggplot(df_ko_sf, aes(x = Este, y = Norte, fill = predict)) +
  geom_raster() +
  scale_fill_gradientn(colors = colors_discrete) +
  labs(
    title = "Kriging Predict A P1",
    x = "Oeste",
    y = "Norte",
    fill = "Predict"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(
  filename = "kriging_predict_A_P1.jpeg",
  plot = last_plot(),
  path = "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/results/geo_analysis",
  width = 10,
  height = 8,
  dpi = 300,
  scale = 0.6
)

# Kriging error data frame
df_ko_sd <- cbind(tabla_grid, sd = sqrt(tabla_ko$krige.var))

# Kriging error plot
ggplot(df_ko_sd, aes(x = Este, y = Norte, fill = sd)) +
  geom_raster() +
  scale_fill_gradientn(colors = colors_discrete) +
  labs(
    title = "Kriging StdDev A P1",
    x = "West",
    y = "North",
    fill = "StdDev"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(
  filename = "kriging_error_A_P2.jpeg",
  plot = last_plot(),
  path = "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/results/geo_analysis",
  width = 10,
  height = 8,
  dpi = 300,
  scale = 0.6
)

# Raster prediction surface
map_surface <- rasterFromXYZ(cbind(tabla_grid, tabla_ko$predict))
crs(map_surface) <- sp::CRS("EPSG:25833")

# Raster prediction error
map_error <- rasterFromXYZ(cbind(tabla_grid, tabla_ko$krige.var))
crs(map_error) <- sp::CRS("EPSG:25833")

# Save surface
writeRaster(
  map_surface,
  "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/results/TIF/a_p1.tif",
  overwrite = TRUE
)

#########   ###########
###   ###   ###   ####
###   ###        ####
########        ####
###            ####
###           ####
###          ##########

# ===================================================================================
# 23. MAPS FOR P2
# ===================================================================================

# Join PII and acoustic indicators with coordinates

head(coor)

sound_p2 <- merge(
  pii_maps_p2,
  coor,
  by.x = "Punto",
  by.y = "puntos_un",
  all.x = TRUE
)

head(sound_p2)
head(aco_p2)

sound_p2 <- merge(
  sound_p2,
  aco_p2,
  by.x = "Punto",
  by.y = "puntos",
  all.x = TRUE
)

head(sound_p2)

#Convert NAs to 0

sound_p2 <- sound_p2 %>%
  mutate(
    across(everything(), ~replace(.x, is.na(.x), 0))
  )

head(sound_p2)

# Transform EPSG:3857 -> EPSG:25833
coor_sf <- st_as_sf(
  sound_p2,
  coords = c("POINT_X", "POINT_Y"),
  crs = 3857,
  remove = FALSE
)

coor_sf <- st_transform(
  coor_sf,
  crs = 25833
)

coords_25833 <- st_coordinates(coor_sf)

sound_p2$POINT_X <- coords_25833[,1]
sound_p2$POINT_Y <- coords_25833[,2]

# Add spatial reference to soundscape data
sound_space_p2 = as.geodata(sound_p2, coords.col = 20:21, data.col = 3)

# Plot points
sound_points <- as.data.frame(sound_space_p2$coords)

ggplot(sound_points, aes(x = POINT_X, y = POINT_Y)) +
  geom_point(size = 2, color = "black") +
  labs(title = "Sampling points", x = "X", y = "Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5
    )
  )

# Variogram cloud
summary(dist(sound_space_p2$coords))
cloud <- variog(sound_space_p2, option = "cloud")

# Convert cloud to data frame
sound_v_cloud <- data.frame(
  u = cloud$u,
  v = cloud$v
)

# Plot
ggplot(sound_v_cloud, aes(x = u, y = v)) +
  geom_point(alpha = 0.4, color = "black") +
  labs(
    title = "Variogram cloud",
    x = "Lag distance",
    y = "Semivariance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5   # centra el título
    )
  )

# Experimental variogram
variogram <- variog(sound_space_p1, option = "bin", uvec = seq(0,2500,200), estimator.type = "classic")

# Convert experimental variogram to data frame
sound_variogram <- data.frame(
  u = variogram$u,
  v = variogram$v
)

#Plot
ggplot(sound_variogram, aes(x = u, y = v)) +
  geom_point(size = 2, color = "black") +
  labs(
    title = "Experimental semivariogram A P2",
    x = "Lag distance",
    y = "Semivariance"
  ) +
  expand_limits(y = 0) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 30, 10, 10)  # margen derecho ampliado
  )

ggsave(
  filename = "variogram_A_P2.jpeg",
  plot = last_plot(),
  path = "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/results/geo_analysis",
  width = 10,
  height = 8,
  dpi = 300,
  scale = 0.6
)

# Fit theorical models
ini.vals <- expand.grid(sigmasq = seq(0, 600, length.out = 10),
                        phi = seq(100, 2000, length.out = 10))

model_mco_exp = variofit(variogram, ini = ini.vals, cov.model = "exponential", fix.nug = TRUE, wei = "npair", min = "optim")
model_mco_gaus = variofit(variogram, ini = ini.vals, cov.model = "gaussian", fix.nug = TRUE, wei = "npair", min = "optim")
model_mco_spe = variofit(variogram, ini = ini.vals, cov.model = "spheric", fix.nug = TRUE, wei = "npair", min = "optim")

# Parameters
sigma_exp <- model_mco_exp$cov.pars[1]
phi_exp   <- model_mco_exp$cov.pars[2]

sigma_gaus <- model_mco_gaus$cov.pars[1]
phi_gaus   <- model_mco_gaus$cov.pars[2]

sigma_spe <- model_mco_spe$cov.pars[1]
phi_spe   <- model_mco_spe$cov.pars[2]

# Sequence of distances
h_seq <- seq(0, max(sound_variogram$u), length.out = 200)

# Compute theoretical curves
gamma_exp <- sigma_exp * (1 - exp(-h_seq / phi_exp))
gamma_gaus <- sigma_gaus * (1 - exp(-(h_seq / phi_gaus)^2))
gamma_spe <- ifelse(h_seq < phi_spe,
                    sigma_spe * (1.5 * (h_seq / phi_spe) - 0.5 * (h_seq / phi_spe)^3),
                    sigma_spe)

# Data frame para las líneas
sound_models <- data.frame(
  h = h_seq,
  exponential = gamma_exp,
  gaussian = gamma_gaus,
  spherical = gamma_spe
)

### Plot fitted theoretical models

ggplot(sound_variogram, aes(x = u, y = v)) +
  geom_point(size = 2, color = "black") +
  
  # Exponential
  geom_line(
    data = sound_models,
    aes(x = h, y = exponential),
    color = "#CC00FF",
    linewidth = 1.0,
  ) +
  
  # Gaussian
  geom_line(
    data = sound_models,
    aes(x = h, y = gaussian),
    color = "#FF0000",
    linewidth = 1.0,
  ) +
  
  # Spherical
  geom_line(
    data = sound_models,
    aes(x = h, y = spherical),
    color = "blue",
    linewidth = 1.0,
  ) +
  
  labs(
    title = "Fitted theoretical models A P2",
    x = "Lag distance",
    y = "Semivariance",
    caption = "Purple = Exponential | Red = Gaussian | Blue = Spherical"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5     
    ),
    plot.caption = element_text(
      size = 10,
      hjust = 0.5    
    )
  )

ggsave(
  filename = "fitted_models_A_P2.jpeg",
  plot = last_plot(),
  path = "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/results/geo_analysis",
  width = 10,
  height = 8,
  dpi = 300,
  scale = 0.6
)

# List of models
models <- list(
  exponential = model_mco_exp,
  gaussian    = model_mco_gaus,
  spherical   = model_mco_spe
)

# Extract loss values
loss_values <- sapply(models, function(m) m$value)
print(loss_values)

# Select best model
best_name <- names(which.min(loss_values))
best_model <- models[[best_name]]

cat("\n✅ Best model selected:", best_name,"\n")
cat("Model:", best_name, "\n")
cat("Loss value:", min(loss_values), "\n")
cat("Sigma^2 (variance):", best_model$cov.pars[1], "\n")
cat("Phi (range):", best_model$cov.pars[2], "\n")

# Kriging
xrange <- range(sound_p2$POINT_X)
yrange <- range(sound_p2$POINT_Y)

margen <- 1200

tabla_grid <- expand.grid(
  Este = seq(xrange[1] - margen, xrange[2] + margen, length.out = 500),
  Norte = seq(yrange[1] - margen, yrange[2] + margen, length.out = 500)
)

tabla_ko = krige.conv(
  sound_space_p2,
  loc = tabla_grid,
  krige = krige.control(
    nugget = best_model$nugget,
    cov.pars = best_model$cov.pars,
    trend.d = "cte",
    trend.l = "cte"
  )
)

# Colors for plot
colors_discrete <- c(
  "#08306B",
  "#08519C",
  "#2171B5",
  "#6BAED6",
  "#C6DBEF",
  "#FEE08B",
  "#FDAE61",
  "#F46D43",
  "#D73027",
  "#A50026"  
)

# Kriging surface data frame
df_ko_sf <- cbind(tabla_grid, predict = tabla_ko$predict)

# Prediction plot
ggplot(df_ko_sf, aes(x = Este, y = Norte, fill = predict)) +
  geom_raster() +
  scale_fill_gradientn(colors = colors_discrete) +
  labs(
    title = "Kriging Predict A P2",
    x = "Oeste",
    y = "Norte",
    fill = "Predict"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(
  filename = "kriging_predict_A_P2.jpeg",
  plot = last_plot(),
  path = "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/results/geo_analysis",
  width = 10,
  height = 8,
  dpi = 300,
  scale = 0.6
)

# Kriging error data frame
df_ko_sd <- cbind(tabla_grid, sd = sqrt(tabla_ko$krige.var))

# Kriging error plot
ggplot(df_ko_sd, aes(x = Este, y = Norte, fill = sd)) +
  geom_raster() +
  scale_fill_gradientn(colors = colors_discrete) +
  labs(
    title = "Kriging StdDev A P2",
    x = "West",
    y = "North",
    fill = "StdDev"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave(
  filename = "kriging_error_A_P2.jpeg",
  plot = last_plot(),
  path = "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/results/geo_analysis",
  width = 10,
  height = 8,
  dpi = 300,
  scale = 0.6
)

# Raster prediction surface
map_surface <- rasterFromXYZ(cbind(tabla_grid, tabla_ko$predict))
crs(map_surface) <- sp::CRS("EPSG:25833")

# Raster prediction error
map_error <- rasterFromXYZ(cbind(tabla_grid, tabla_ko$krige.var))
crs(map_error) <- sp::CRS("EPSG:25833")

# Save surface
writeRaster(
  map_surface,
  "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/results/TIF/a_p2.tif",
  overwrite = TRUE
)

# ===================================================================================
# 24. FINAL OUTPUTS
# ===================================================================================

write.csv(sound_p1, "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/results/sound_p1.csv", row.names = FALSE)
write.csv(sound_p2, "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/5_cnn_deployment_py_R/results/sound_p2.csv", row.names = FALSE)