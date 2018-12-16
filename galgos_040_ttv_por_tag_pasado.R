# Script para TRAIN+TEST sobre PASADO-TTV persistiendo en fichero el mejor modelo entrenado
rm(list=ls())

source("/home/carloslinux/Desktop/WORKSPACES/wksp_for_r/r_galgos/galgos_inteligencia.R") # para reciclar funciones

entradas <- commandArgs(trailingOnly = TRUE)

##### Solo para debug
 # modo <- 1 # 1=train+test+validation
 # tag <- "LARGA_Y_ALGUNO_LENTO"
 # limiteSql <- "3000"
 # tipoReduccion <- "PCA"
 # path_modelo_pca_prefijo <- "/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/pca_modelo_"
 # pca_umbral_varianza <- 0.82
 # tsne_num_features_output <- 12
 # entradas <- c(modo, tag, limiteSql, tipoReduccion, path_modelo_pca_prefijo, pca_umbral_varianza, tsne_num_features_output)
#######


if (length(entradas) == 0) {
  print("Necesario indicar parametros de entrada")
  
} else if (length(entradas) >= 1) {
  
  modo <- entradas[1] # 1=train+test+validation, 2=train+test (ttv), 3=prediccion_futuro
  tag <- entradas[2]
  limiteSql <- entradas[3]
  tipoReduccion <- entradas[4]
  path_modelo_pca_prefijo <- entradas[5]
  pca_umbral_varianza <- entradas[6]
  tsne_num_features_output <- entradas[7]
  
  ############ LLAMADA PRINCIPAL ##########
  if (modo == 1) {
    ejecutarCadenaEntrenamientoValidation(tag, limiteSql, tipoReduccion, path_modelo_pca_prefijo, pca_umbral_varianza, tsne_num_features_output)
  }
  
  #Borramos array de parametros, para evitar confusiones
  rm(entradas)
  
}
