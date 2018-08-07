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

#---------------------- CUERPO de este SCRIPT -------------



# PARAMETROS
# 1 TAG (subgrupo)

options(echo = TRUE) # En la salida, queremos ver los comandos ejecutados

args <- commandArgs(trailingOnly = TRUE) #devolver solo los argumentos, pero no el comando (nombre del script)
print(args)

if (length(args) == 0) {
  print("Necesario indicar parametros de entrada")
  
} else if (length(args) >= 1) {
  print( paste('Numero de parametros: ', length(args)) )
  tag <- args[1];  print( paste( 'tag=', tag, sep = '' ) )
  limiteSql <- args[2];  print( paste( 'limiteSql=', tag, sep = '' ) )
  
  #Borramos array de parametros, para evitar confusiones
  rm(args)
}

ejecutarCadenaEntrenamientoTTV(tag, limiteSql)



