# Script para predecir el FUTURO usando el MODELO (YA ENTRENADO sobre datos del pasado-ttv)

source("/home/carloslinux/Desktop/WORKSPACES/wksp_for_r/r_galgos/galgos_inteligencia.R") # para reciclar funciones

# --------- FUNCIONES -----------------------------------------

#' CADENA de PREDICCION DEL FUTURO (train+test)
#'
#' @param tag 
#'
#' @return
#' @export
#'
#' @examples
ejecutarCadenaPredecirFuturo <- function(tag, limiteSql){
  
  print('--------------- ejecutarCadenaPredecirFuturo ------------')
  
  tag <- args[1];  print( paste( 'tag=', tag, sep = '' ) )
  limiteSql <- args[2];  print( paste( 'limiteSql=', tag, sep = '' ) )
  
  establecerConfigGeneral()
  listaDatos <- leerDesdeBaseDatosYEscribirCSV(1, 
                                               'datos_desa.tb_ds_futuro_features_', 
                                               'NO SABEMOS TARGETS DEL FUTURO',
                                               'NO_HACEMOS_VALIDATION', 
                                               tag, 
                                               format(limiteSql, scientific = FALSE),
                                               FALSE, FALSE)
  
  futuro_f <- listaDatos[[1]]
  
  predecir(tag, futuro_f, "/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/FILELOAD_ds_futuro_targets_2_", "FUTURO")
  
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
  if (modo == 3) {
    ejecutarCadenaPredecirFuturo(tag, limiteSql)
  }
  
  #Borramos array de parametros, para evitar confusiones
  rm(entradas)
  
}
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################




