# Función para modificar archivos gl_hru_par
gl_hru_modify <- function(params, path) {
  gl_file <- file.path(path, "gl_hru_par.txt")
  gl_data <- read.table(gl_file, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  # Modificar solo filas donde HRU != 0
  for (col in names(params)) {
    if (col %in% colnames(gl_data)) {
      gl_data[[col]] <- ifelse(gl_data$HRU != 0, params[[col]], gl_data[[col]])
    }
  }
  
  # Redondear valores a 3 decimales
  gl_data <- gl_data %>% mutate(across(-HRU, ~ round(.x, digits = 3)))
  
  write.table(gl_data, gl_file, row.names = FALSE, sep = ",", quote = FALSE)
}

# Función para modificar archivos .sno
sno_modify <- function(params, path) {
  sno_files <- list.files(path, pattern = "\\.sno$", full.names = TRUE)
  for (file in sno_files) {
    sno_data <- readLines(file)
    data <- read.table(text = paste(sno_data[-1], collapse = "\n"), header = FALSE)
    if (!is.null(params$SFTMP)) data[1, ] <- params$SFTMP
    if (!is.null(params$SMTMP)) data[2, ] <- params$SMTMP
    if (!is.null(params$SMFMX)) data[3, ] <- params$SMFMX
    if (!is.null(params$SMFMN)) data[4, ] <- params$SMFMN
    formatted_data <- apply(data, 1, function(row) paste(sprintf("%7.3f", row), collapse = "   "))
    writeLines(c(sno_data[1], formatted_data), file)
  }
}