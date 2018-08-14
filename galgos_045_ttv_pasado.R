# Script para TRAIN+TEST sobre PASADO-TTV persistiendo en fichero el mejor modelo entrenado

source("/home/carloslinux/Desktop/WORKSPACES/wksp_for_r/r_galgos/galgos_inteligencia.R") # para reciclar funciones

entradas <- commandArgs(trailingOnly = TRUE)

##### Solo para debug
# modo <- 2
# tag <- "DOW_LAB"
# limiteSql <- "1000"
# tipoReduccion <- "PCA"
# path_modelo_pca_prefijo <- "/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/pca_modelo_"
# pca_umbral_varianza <- 0.95
# tsne_num_features_output <- 12
# entradas <- c(modo, tag, limiteSql, tipoReduccion, path_modelo_pca_prefijo, pca_umbral_varianza, tsne_num_features_output)
#######


if (length(entradas) == 0) {
  print("Necesario indicar parametros de entrada")
  
} else if (length(entradas) >= 1) {
  
  print( paste('Numero de parametros: ', length(entradas)) )
  modo <- entradas[1];  print( paste( 'modo=', modo, sep = '' )   ) # 1=train+test+validation, 2=train+test (ttv), 3=prediccion_futuro
  tag <- entradas[2];  print( paste( 'tag=', tag, sep = '' )   )
  limiteSql <- entradas[3];  print( paste( 'limiteSql=', limiteSql, sep = '' ) )
  tipoReduccion <- entradas[4];  print( paste( 'tipoReduccion=', tipoReduccion, sep = '' )   )
  path_modelo_pca_prefijo <- entradas[5];  print( paste( 'path_modelo_pca_prefijo=', path_modelo_pca_prefijo, sep = '' )   )
  pca_umbral_varianza <- as.numeric(entradas[6]);  print( paste( 'pca_umbral_varianza=', pca_umbral_varianza, sep = '' )   )
  tsne_num_features_output <- as.integer(entradas[7]);  print( paste( 'tsne_num_features_output=', tsne_num_features_output, sep = '' )   )
  
  ############ LLAMADA PRINCIPAL ##########
  if (modo == 2) {
    ejecutarCadenaEntrenamientoTTV(tag, limiteSql, tipoReduccion, path_modelo_pca_prefijo, pca_umbral_varianza, tsne_num_features_output)
  }
  
  #Borramos array de parametros, para evitar confusiones
  rm(entradas)
}

