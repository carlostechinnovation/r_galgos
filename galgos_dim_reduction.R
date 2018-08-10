# Script para TRAIN+TEST sobre PASADO-TTV persistiendo en fichero el mejor modelo entrenado

source("/home/carloslinux/Desktop/WORKSPACES/wksp_for_r/r_galgos/galgos_inteligencia.R") # para reciclar funciones

# --------- FUNCIONES -----------------------------------------
#' Reducir dimensiones usando PCA (no supervisado).
#' Calcula los eigenvectores que definen las correlaciones entre features.
#'
#' @param input_ft Sin valores NA
#' @param tipo 
#'
#' @return
#' @export
#'
#' @examples
reducirConPCA <- function(input_ft, tipo, path_modelo_pca){
  
  print( paste(" **** PCA:", tipo,"**** "))
  library(stats)
  
  indice_target <- which( colnames(input_ft) == "TARGET" ) #Columna TARGET
  input_f_full <- subset(input_ft, select = -indice_target)
  print(paste("input_f_full:", nrow(input_f_full), "x", ncol(input_f_full)))
  #print(head(input_f_full, n=5L))
  
  #ALGORITMO:
  pca_modelo <- princomp(x = input_f_full, cor = FALSE, scores = T)
  
  #print("print:"); print(pca_modelo)
  print("plot:"); plot(pca_modelo)
  print("summary:"); summary(pca_modelo)
  print("biplot:"); biplot(pca_modelo) #PC2 vs PC1
  
  #Los pesos de cada individuo (fila) proyectado en las componentes (PC1, PC2...)
  pca_pesos <- pca_modelo$scores
  print("pesos:"); print(head(pca_pesos, n=10L))
  pca_pesos_pc1pc2 <- pca_pesos[, 1:2] #Pesos sólo en las componentes que más influyen en la varianza total
  print("plot:"); #plot(pca_pesos_pc1pc2[,1], pca_pesos_pc1pc2[,2]) #misma gráfica que veiamos en el biplot
  
  #Guardando modelo a fichero:
  save(pca_modelo, file = path_modelo_pca)
  
  rm(pca_modelo)
  ls() # Compruebo que se han borrado
}


#' Reducir dimensiones usando TSNE (supervisado)
#'
#' @param input_ft 
#' @param tipo 
#' @param path_modelo_pca 
#'
#' @return
#' @export
#'
#' @examples
reducirConTSNE <- function(input_ft, tipo, path_modelo_pca){
  
  print( paste(" **** TSNE:", tipo,"**** "))
  library(tsne)
  
  colors = rainbow(length(unique(iris$Species)))
  names(colors) = unique(iris$Species)
  
  ecb <- function(x,y){
    plot(x,t = 'n');
    text(x,labels = iris$Species, col = colors[iris$Species])
  }
  
  # Tabla inicial: 4 columnas ==> Tabla final: 2 columnas
  
  tsne_iris = tsne(X = iris[,1:4], initial_config = NULL, k = 2, initial_dims = 30, perplexity = 100,
                   max_iter = 1000, min_cost = 0, epoch_callback = ecb, whiten = TRUE,
                   epoch = 500)
  
}


#' CADENA de ENTRENAMIENTO (train+test) con todo el pasado que conocemos (ya sabemos el SUBGRUPO ganador)
#'
#' @param tag 
#'
#' @return
#' @export
#'
#' @examples
ejecutarReduccionDimensiones <- function(tag, limiteSql, tipoReduccion){
  
  print('--------------- ejecutarReduccionDimensiones: INICIO ------------')
  print( paste( 'tag=', tag, sep = '' ) )
  print( paste( 'limiteSql=', tag, sep = '' ) )
  print( paste( 'tipoReduccion=', tipoReduccion, sep = '' ) )
  
  #Para quitar las COLUMNAS que no son UTILES para esa DISTANCIA
  col_cortas <- c("vel_real_cortas_mediana_norm", "vel_real_cortas_max_norm", "vel_going_cortas_mediana_norm", "vel_going_cortas_max_norm")
  col_medias <- c("vel_real_longmedias_mediana_norm", "vel_real_longmedias_max_norm", "vel_going_longmedias_mediana_norm", "vel_going_longmedias_max_norm")
  col_largas <- c("vel_real_largas_mediana_norm", "vel_real_largas_max_norm", "vel_going_largas_mediana_norm", "vel_going_largas_max_norm")
  
  establecerConfigGeneral()
  listaDatos <- leerDesdeBaseDatosYEscribirCSV(1, 
                                               'datos_desa.tb_ds_pasado_train_features_', 
                                               'datos_desa.tb_ds_pasado_train_targets_',
                                               'NO_HACEMOS_VALIDATION', 
                                               tag, 
                                               format(limiteSql, scientific = FALSE),
                                               TRUE, FALSE)
  lista_ft_cortasmediaslargas <- crearFeaturesyTargetDelPasadoParaDistancias(listaDatos[[1]], col_cortas,col_medias,col_largas)
  
  pasado_ft_cortas <- lista_ft_cortasmediaslargas[[1]]
  pasado_ft_medias <- lista_ft_cortasmediaslargas[[2]]
  pasado_ft_largas <- lista_ft_cortasmediaslargas[[3]]
  
  if (tipoReduccion == "PCA") {
    reducirConPCA(pasado_ft_cortas, "cortas", paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/pca_cortas_',tag, sep = ''))
    #reducirConPCA(pasado_ft_medias, "medias", paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/pca_medias_',tag, sep = ''))
    #reducirConPCA(pasado_ft_largas, "largas", paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/pca_largas_',tag, sep = ''))
    
  } else if (tipoReduccion == "TSNE") {
    reducirConTSNE(pasado_ft_cortas, "cortas", paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/tsne_cortas_',tag, sep = ''))
    #reducirConTSNE(pasado_ft_medias, "medias", paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/tsne_medias_',tag, sep = ''))
    #reducirConTSNE(pasado_ft_largas, "largas", paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/tsne_largas_',tag, sep = ''))
  }
  
  
  # PENDIENTE salidas
  
  
  
  print('--------------- ejecutarReduccionDimensiones: FIN ------------')
}


######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
print('-------------------- PRINCIPAL --------------------------')

options(echo = TRUE) # En la salida, queremos ver los comandos ejecutados

entradas <- commandArgs(trailingOnly = TRUE)

##### TEMP
modo <- 0 # 0=reduccion_dimensiones
tag <- "DOW_LAB"
limiteSql <- "1000"
tipoReduccion <- "PCA"

entradas <- c(modo,tag, limiteSql, tipoReduccion)
#######






if (length(entradas) == 0) {
  print("Necesario indicar parametros de entrada")
  
} else if (length(entradas) >= 1) {
  
  print( paste('Numero de parametros: ', length(entradas)) )
  modo <- entradas[1];  print( paste( 'modo=', modo, sep = '' )   ) # 0=dim_reduction 1=train+test+validation, 2=train+test (ttv), 3=prediccion_futuro
  tag <- entradas[2];  print( paste( 'tag=', tag, sep = '' )   )
  limiteSql <- entradas[3];  print( paste( 'limiteSql=', limiteSql, sep = '' ) )
  tipoReduccion <- entradas[4];  print( paste( 'tipoReduccion=', tipoReduccion, sep = '' )   )
  
  ############ LLAMADA PRINCIPAL ##########
  if (modo == 0) {
    ejecutarReduccionDimensiones(tag, limiteSql, tipoReduccion)
  }
  
  #Borramos array de parametros, para evitar confusiones
  rm(entradas)
  
}
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
