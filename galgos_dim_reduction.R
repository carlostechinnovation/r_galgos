# Script para TRAIN+TEST sobre PASADO-TTV persistiendo en fichero el mejor modelo entrenado

source("/home/carloslinux/Desktop/WORKSPACES/wksp_for_r/r_galgos/galgos_inteligencia.R") # para reciclar funciones


######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
print('-------------------- PRINCIPAL --------------------------')

options(echo = FALSE) # En la salida, queremos ver los comandos ejecutados

entradas <- commandArgs(trailingOnly = TRUE)

##### Solo para debug
#modo <- 0 # 0=reduccion_dimensiones
#tag <- "DOW_LAB"
#limiteSql <- "1000"
#tipoReduccion <- "PCA"
#path_modelo_pca_prefijo <- "/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/pca_modelo_"
#pca_umbral_varianza <- 0.95
#tsne_num_features_output <- 12
#entradas <- c(modo, tag, limiteSql, tipoReduccion, path_modelo_pca_prefijo, pca_umbral_varianza, tsne_num_features_output)
#######


if (length(entradas) == 0) {
  print("Necesario indicar parametros de entrada")
  
} else if (length(entradas) >= 1) {
  
  print( paste('Numero de parametros: ', length(entradas)) )
  modo <- entradas[1];  print( paste( 'modo=', modo, sep = '' )   ) # 0=dim_reduction 1=train+test+validation, 2=train+test (ttv), 3=prediccion_futuro
  tag <- entradas[2];  print( paste( 'tag=', tag, sep = '' )   )
  limiteSql <- entradas[3];  print( paste( 'limiteSql=', limiteSql, sep = '' ) )
  tipoReduccion <- entradas[4];  print( paste( 'tipoReduccion=', tipoReduccion, sep = '' )   )
  path_modelo_pca_prefijo <- entradas[5];  print( paste( 'path_modelo_pca_prefijo=', path_modelo_pca_prefijo, sep = '' )   )
  pca_umbral_varianza <- entradas[6];  print( paste( 'pca_umbral_varianza=', pca_umbral_varianza, sep = '' )   )
  tsne_num_features_output <- entradas[7];  print( paste( 'tsne_num_features_output=', tsne_num_features_output, sep = '' )   )
  
  ############ LLAMADA PRINCIPAL ##########
  if (modo == 0) {
    ejecutarReduccionDimensiones('datos_desa.tb_ds_pasado_train_features_', 
                                 'datos_desa.tb_ds_pasado_train_targets_',
                                 tag, limiteSql, tipoReduccion, path_modelo_pca_prefijo, pca_umbral_varianza, tsne_num_features_output)
    
  }
  
  #Borramos array de parametros, para evitar confusiones
  rm(entradas)
}
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
