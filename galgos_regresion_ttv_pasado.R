# Script para TRAIN+TEST persistiendo en fichero el mejor modelo entrenado

source("/home/carloslinux/Desktop/WORKSPACES/wksp_for_r/r_galgos/galgos_regresion_train_test.R") # para reciclar funciones

# --------- FUNCIONES -----------------------------------------

#' CADENA de ENTRENAMIENTO (train+test) con todo el pasado que conocemos (ya sabemos el SUBGRUPO ganador)
#'
#' @param tag 
#'
#' @return
#' @export
#'
#' @examples
ejecutarCadenaEntrenamientoTTV <- function(tag, limiteSql){
  
  print('--------------- ejecutarCadenaEntrenamientoTTV ------------')
  
  tag <- args[1];  print( paste( 'tag=', tag, sep = '' ) )
  limiteSql <- args[2];  print( paste( 'limiteSql=', tag, sep = '' ) )
  
  establecerConfigGeneral()
  listaDatos <- leerDesdeBaseDatosYEscribirCSV(1, 
                                               'datos_desa.tb_ds_pasado_ttv_features_', 
                                               'datos_desa.tb_ds_pasado_ttv_targets_',
                                               'NO_HACEMOS_VALIDATION', 
                                               tag, 
                                               format(limiteSql, scientific = FALSE),
                                               FALSE)
  lista_ft_cortasmediaslargas <- crearFeaturesyTargetDelPasadoParaDistancias(listaDatos[[1]])
  obtenerModelosParaDistancias(lista_ft_cortasmediaslargas)
}


######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
print('-------------------- PRINCIPAL --------------------------')

options(echo = FALSE) # En la salida, queremos ver los comandos ejecutados

entradas <- commandArgs(trailingOnly = TRUE)

if (length(entradas) == 0) {
  print("Necesario indicar parametros de entrada")
  
} else if (length(entradas) >= 1) {
  
  print( paste('Numero de parametros: ', length(entradas)) )
  modo <- entradas[1];  print( paste( 'modo=', modo, sep = '' )   ) # 1=train+test+validation, 2=train+test (ttv), 3=prediccion_futuro
  tag <- entradas[2];  print( paste( 'tag=', tag, sep = '' )   )
  limiteSql <- entradas[3];  print( paste( 'limiteSql=', limiteSql, sep = '' ) )
  
  #Para quitar las COLUMNAS que no son UTILES para esa DISTANCIA
  col_cortas <- c("vel_real_cortas_mediana_norm", "vel_real_cortas_max_norm", "vel_going_cortas_mediana_norm", "vel_going_cortas_max_norm")
  col_medias <- c("vel_real_longmedias_mediana_norm", "vel_real_longmedias_max_norm", "vel_going_longmedias_mediana_norm", "vel_going_longmedias_max_norm")
  col_largas <- c("vel_real_largas_mediana_norm", "vel_real_largas_max_norm", "vel_going_largas_mediana_norm", "vel_going_largas_max_norm")
  
  ############ LLAMADA PRINCIPAL ##########
  if (modo == 2) {
    ejecutarCadenaEntrenamientoTTV(tag, limiteSql)
  }
  
  #Borramos array de parametros, para evitar confusiones
  rm(entradas)
  
}
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################




