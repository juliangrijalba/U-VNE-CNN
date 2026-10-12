
library(dplyr)

# Semilla para reproducibilidad
set.seed(1234)

seleccionar_27_estratificados <- function(folder, outfolder, periodo){
  
  files <- list.files(folder, pattern="\\.wav$", full.names=TRUE)
  
  df <- data.frame(
    file = files,
    name = basename(files),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      punto = sub("_.*", "", name),
      segmento = as.numeric(sub(".*_(\\d+)_.*", "\\1", name)),
      periodo_detect = sub(".*_(P\\d)\\.wav", "\\1", name)
    ) %>%
    filter(periodo_detect == periodo)
  
  puntos <- unique(df$punto)
  
  for(pt in puntos){
    
    df_sub <- df %>% filter(punto == pt) %>% arrange(segmento)
    
    # Crear 27 estratos temporales para 90 segmentos
    df_sub$estrato <- cut(df_sub$segmento, breaks = 27, labels = FALSE)
    
    # Seleccionar 1 segmento aleatorio por estrato
    selected <- df_sub %>%
      group_by(estrato) %>%
      sample_n(1)
    
    # Copiar segmentos seleccionados
    file.copy(from = selected$file,
              to = file.path(outfolder, selected$name))
  }
  
  cat("✔ Selección estratificada para", periodo, "completada.\n")
}

# Carpetas del usuario:
p1_folder <- "C:/Users/Acer/Desktop/ss_filtered/P1/recortados_3min/normalizados/segmentos_2s_todos"
p2_folder <- "C:/Users/Acer/Desktop/ss_filtered/P2/recortados_3min/normalizados/segmentos_2s_todos"

p1_out <- file.path(p1_folder, "seleccion_27_estratificada")
p2_out <- file.path(p2_folder, "seleccion_27_estratificada")

if(!dir.exists(p1_out)) dir.create(p1_out)
if(!dir.exists(p2_out)) dir.create(p2_out)

# Ejecutar selección estratificada para P1 y P2
seleccionar_27_estratificados(p1_folder, p1_out, "P1")
seleccionar_27_estratificados(p2_folder, p2_out, "P2")

cat("✔ Proceso finalizado para ambos periodos.\n")
