## RECORTE P1

# Instalar si no los tienes
# install.packages("tuneR")
# install.packages("seewave")

library(tuneR)
library(seewave)

# Carpeta donde están los wav
input_dir <- "C:/Users/Acer/Desktop/ss_filtered/P1"
output_dir <- file.path(input_dir, "recortados_3min")

# Crear carpeta de salida si no existe
if(!dir.exists(output_dir)) dir.create(output_dir)

# Duración deseada en segundos
target_duration <- 180  # 3 minutos

# Obtener lista de wav
wav_files <- list.files(input_dir, pattern = "\\.wav$", full.names = TRUE)

for (file in wav_files) {
  
  # Leer WAV preservando formato
  wav <- readWave(file, from = 0, to = Inf, units = "seconds")
  
  # Duración del audio
  dur <- length(wav@left) / wav@samp.rate
  
  # Si ya es menor o igual → se copia tal cual sin modificar
  if (dur <= target_duration) {
    new_path <- file.path(output_dir, basename(file))
    writeWave(wav, filename = new_path)
    next
  }
  
  # Recortar exactamente a 3 minutos
  wav_cut <- cutw(
    wav,
    from = 0,
    to = target_duration,
    output = "Wave"
  )
  
  # Guardar sin sobrescribir el original
  new_path <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(file)), "_3min.wav"))
  writeWave(wav_cut, filename = new_path)
}

cat("✔ Recorte completado. Archivos guardados en:", output_dir)


## NORMALIZACIÓN P1

# Instalar si no los tienes
# install.packages("tuneR")
# install.packages("seewave")

library(tuneR)
library(seewave)

# Carpeta donde están los wav recortados
input_dir <- "C:/Users/Acer/Desktop/ss_filtered/P1/recortados_3min"
output_dir <- file.path(input_dir, "normalizados")

# Crear carpeta de salida si no existe
if(!dir.exists(output_dir)) dir.create(output_dir)

# Obtener lista de wav
wav_files <- list.files(input_dir, pattern = "\\.wav$", full.names = TRUE)

for (file in wav_files) {
  
  # Leer WAV manteniendo formato y codificación
  wav <- readWave(file, from = 0, to = Inf, units = "seconds")
  
  # Normalizar amplitud (peak normalization a -1..1)
  wav_norm <- normalize(wav, unit = "16")  # Mantener escala PCM si es 16 bits
  
  # Construir nombre de salida sin sobrescribir
  new_path <- file.path(output_dir,
                        paste0(tools::file_path_sans_ext(basename(file)),
                               "_norm.wav"))
  
  # Guardar manteniendo sample rate, bits y canales
  writeWave(wav_norm, filename = new_path)
}


output_dir <- "C:/Users/Acer/Desktop/ss_filtered/P1/recortados_3min/normalizados"

cat("✔ Normalización completada. Archivos guardados en:", output_dir)

## SEGMENTACIÓN P1

# Necesita:
# install.packages("tuneR")
# install.packages("seewave")

library(tuneR)
library(seewave)

# Carpeta con los audios normalizados
input_dir <- "C:/Users/Acer/Desktop/ss_filtered/P1/recortados_3min/normalizados"

# Carpeta única donde irán TODOS los segmentos
output_dir <- file.path(input_dir, "segmentos_2s_todos")
if (!dir.exists(output_dir)) dir.create(output_dir)

# Duración del segmento
segment_length <- 2

# Lista de WAV
wav_files <- list.files(input_dir, pattern = "\\.wav$", full.names = TRUE)

for (file in wav_files) {
  
  # Leer el wav manteniendo el formato original
  wav <- readWave(file)
  
  # Duración total en segundos
  duration <- length(wav@left) / wav@samp.rate
  
  # Número de segmentos completos
  num_segments <- floor(duration / segment_length)
  
  # Base del nombre
  base <- tools::file_path_sans_ext(basename(file))
  
  # Generar segmentos
  for (i in 0:(num_segments - 1)) {
    
    start <- i * segment_length
    end <- start + segment_length
    
    seg <- cutw(
      wav,
      from = start,
      to = end,
      output = "Wave",
      units = "seconds"
    )
    
    # Crear nombre único para cada segmento
    seg_name <- sprintf("%s_seg_%04d.wav", base, i + 1)
    seg_path <- file.path(output_dir, seg_name)
    
    # Guardar conservando el formato
    writeWave(seg, filename = seg_path)
  }
}

cat("✔ Segmentación completada. Segmentos guardados en:", output_dir)



## RECORTE P2


# Instalar si no los tienes
# install.packages("tuneR")
# install.packages("seewave")

library(tuneR)
library(seewave)

# Carpeta donde están los wav
input_dir <- "C:/Users/Acer/Desktop/ss_filtered/P2"
output_dir <- file.path(input_dir, "recortados_3min")

# Crear carpeta de salida si no existe
if(!dir.exists(output_dir)) dir.create(output_dir)

# Duración deseada en segundos
target_duration <- 180  # 3 minutos

# Obtener lista de wav
wav_files <- list.files(input_dir, pattern = "\\.wav$", full.names = TRUE)

for (file in wav_files) {
  
  # Leer WAV preservando formato
  wav <- readWave(file, from = 0, to = Inf, units = "seconds")
  
  # Duración del audio
  dur <- length(wav@left) / wav@samp.rate
  
  # Si ya es menor o igual → se copia tal cual sin modificar
  if (dur <= target_duration) {
    new_path <- file.path(output_dir, basename(file))
    writeWave(wav, filename = new_path)
    next
  }
  
  # Recortar exactamente a 3 minutos
  wav_cut <- cutw(
    wav,
    from = 0,
    to = target_duration,
    output = "Wave"
  )
  
  # Guardar sin sobrescribir el original
  new_path <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(file)), "_3min.wav"))
  writeWave(wav_cut, filename = new_path)
}

cat("✔ Recorte completado. Archivos guardados en:", output_dir)


##  NORMALIZACIÓN P2

# Instalar si no los tienes
# install.packages("tuneR")
# install.packages("seewave")

library(tuneR)
library(seewave)

# Carpeta donde están los wav recortados
input_dir <- "C:/Users/Acer/Desktop/ss_filtered/P2/recortados_3min"
output_dir <- file.path(input_dir, "normalizados")

# Crear carpeta de salida si no existe
if(!dir.exists(output_dir)) dir.create(output_dir)

# Obtener lista de wav
wav_files <- list.files(input_dir, pattern = "\\.wav$", full.names = TRUE)

for (file in wav_files) {
  
  # Leer WAV manteniendo formato y codificación
  wav <- readWave(file, from = 0, to = Inf, units = "seconds")
  
  # Normalizar amplitud (peak normalization a -1..1)
  wav_norm <- normalize(wav, unit = "16")  # Mantener escala PCM si es 16 bits
  
  # Construir nombre de salida sin sobrescribir
  new_path <- file.path(output_dir,
                        paste0(tools::file_path_sans_ext(basename(file)),
                               "_norm.wav"))
  
  # Guardar manteniendo sample rate, bits y canales
  writeWave(wav_norm, filename = new_path)
}


output_dir <- "C:/Users/Acer/Desktop/ss_filtered/P1/recortados_3min/normalizados"

cat("✔ Normalización completada. Archivos guardados en:", output_dir)


## SEGMENTACIÓN P2

# Necesita:
# install.packages("tuneR")
# install.packages("seewave")

library(tuneR)
library(seewave)

# Carpeta con los audios normalizados
input_dir <- "C:/Users/Acer/Desktop/ss_filtered/P2/recortados_3min/normalizados"

# Carpeta única donde irán TODOS los segmentos
output_dir <- file.path(input_dir, "segmentos_2s_todos")
if (!dir.exists(output_dir)) dir.create(output_dir)

# Duración del segmento
segment_length <- 2

# Lista de WAV
wav_files <- list.files(input_dir, pattern = "\\.wav$", full.names = TRUE)

for (file in wav_files) {
  
  # Leer el wav manteniendo el formato original
  wav <- readWave(file)
  
  # Duración total en segundos
  duration <- length(wav@left) / wav@samp.rate
  
  # Número de segmentos completos
  num_segments <- floor(duration / segment_length)
  
  # Base del nombre
  base <- tools::file_path_sans_ext(basename(file))
  
  # Generar segmentos
  for (i in 0:(num_segments - 1)) {
    
    start <- i * segment_length
    end <- start + segment_length
    
    seg <- cutw(
      wav,
      from = start,
      to = end,
      output = "Wave",
      units = "seconds"
    )
    
    # Crear nombre único para cada segmento
    seg_name <- sprintf("%s_seg_%04d.wav", base, i + 1)
    seg_path <- file.path(output_dir, seg_name)
    
    # Guardar conservando el formato
    writeWave(seg, filename = seg_path)
  }
}

cat("✔ Segmentación completada. Segmentos guardados en:", output_dir)


## NOMBRAMIENTO P1 Y P2


# Script para renombrar los segmentos P1

folder <- "C:/Users/Acer/Desktop/ss_filtered/P1/recortados_3min/normalizados/segmentos_2s_todos"

files <- list.files(folder, pattern="\\.wav$", full.names = TRUE)

for (file in files) {
  
  oldname <- basename(file)
  
  # Extraer número de punto (antes del primer "-")
  point <- sub("-.*", "", oldname)
  point <- sprintf("%03d", as.numeric(point))
  
  # Extraer número de segmento (seg_XXXX)
  seg <- sub(".*_seg_", "", oldname)
  seg <- sub("\\.wav$", "", seg)
  seg <- sprintf("%02d", as.numeric(seg))
  
  # Nuevo nombre siguiendo la lógica
  newname <- paste0(point, "_", seg, "_P1.wav")
  newpath <- file.path(folder, newname)
  
  file.rename(file, newpath)
}

cat("✔ Renombrado de archivos P1 completado.\n")



# Script para renombrar los segmentos P2

folder <- "C:/Users/Acer/Desktop/ss_filtered/P2/recortados_3min/normalizados/segmentos_2s_todos"

files <- list.files(folder, pattern="\\.wav$", full.names = TRUE)

for (file in files) {
  
  oldname <- basename(file)
  
  # Extraer número de punto
  point <- sub("-.*", "", oldname)
  point <- sprintf("%03d", as.numeric(point))
  
  # Extraer número de segmento
  seg <- sub(".*_seg_", "", oldname)
  seg <- sub("\\.wav$", "", seg)
  seg <- sprintf("%02d", as.numeric(seg))
  
  # Nuevo nombre
  newname <- paste0(point, "_", seg, "_P2.wav")
  newpath <- file.path(folder, newname)
  
  file.rename(file, newpath)
}

cat("✔ Renombrado de archivos P2 completado.\n")



