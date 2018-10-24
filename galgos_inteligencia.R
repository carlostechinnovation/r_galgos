# Script para TRAIN+TEST persistiendo en fichero el mejor modelo entrenado

# --------- VARIABLES GLOBALES -----------------------------------------
R_OUT <- "#R_OUT#--"
PORCENTAJE_TEST <- 0.20 #Divide el dataset de entrenamiento de PASADO en TRAIN+TEST para entrenar el modelo

# --------- FUNCIONES -----------------------------------------

#' Configuraciones generales
#'
#' @return
#' @export
#'
#' @examples
establecerConfigGeneral <- function(){
  print('--------------- establecerConfigGeneral ------------')
  
  options(echo = TRUE) # En la salida, queremos ver los comandos ejecutados
  
  library(SuperLearner)
  library(nnls)
  library(nnet)
  library(randomForest)
  library(glmnet)
  library(ggplot2)
  library(parallel)
  library(Matrix)
  library(foreach)
  library(arm)
  library(MASS)
  library(lme4)
  library(polspline)
  
  options(java.parameters = '-Xmx5g') #Memoria 5GB
  
}


#' Conexion a base de datos para LECTURA MASIVA y creacion de dataframes con esos datos.
#'
#' @param tag Subgrupo
#' @param limiteSql Limite de las consultas MYSQL (normalmente, sin limite, es decir, 999999999)
#' @param nombre_tabla_f 
#' @param nombre_tabla_t 
#' @param nombre_tabla_v 
#' @param incluyeTargets 
#' @param incluyeValidation BOOLEAN Indica si incluye validation
#'
#' @return
#' @export
#'
#' @examples
leerDesdeBaseDatosYEscribirCSV <- function(nombre_tabla_f, nombre_tabla_t, nombre_tabla_v, tag, limiteSql, incluyeTargets, incluyeValidation){
  
  print('--------------- leerDesdeBaseDatosYEscribirCSV: INICIO ------------')
  print(paste('tag=',tag))
  print(paste('limiteSql=',limiteSql))
  print(paste('incluyeTargets=',incluyeTargets))
  print(paste('incluyeValidation=',incluyeValidation))
  
  library(RMySQL)
  library(DBI)
  
  
  print('---------------Conexion a BBDD ------------')
  #Conexiones ya abiertas
  conexiones_abiertas <- dbListConnections(MySQL())
  
  print('Conectando...')
  mydb = dbConnect(MySQL(), user = 'root', password = 'datos1986', dbname = 'datos_desa', host = 'localhost')
  on.exit(dbDisconnect(mydb))
  #dbListTables(mydb)  #Todas las tablas que tengo
  
  #Default
  leido_f <- ""
  leido_t <- ""
  
  print('------------- Features (INPUT) -----------------------')
  consulta_train_f <- paste('SELECT * FROM ', nombre_tabla_f, tag, ' LIMIT ', limiteSql, ';', sep = '')
  print(consulta_train_f)
  leido_f_rs <- dbSendQuery(mydb,  consulta_train_f)
  leido_f <- dbFetch(leido_f_rs, n = -1)
  dbClearResult(leido_f_rs)
  
  mes_indice <- which( colnames(leido_f) == "mes_norm" ) #EXCEPCION Quito columna mes
  leido_f_sinmes <- leido_f[, -mes_indice]
  print(paste("FEATURES (train o futuro): ", nrow(leido_f_sinmes), "x", ncol(leido_f_sinmes)))
  # print(head(leido_f_sinmes, n = 3L))
  
  if (incluyeTargets == TRUE) {
    print('------------------- Targets (INPUT) -----------------')
    consulta_train_t <- paste("SELECT * FROM ", nombre_tabla_t, tag, " LIMIT ", limiteSql, ';', sep = '')
    print(consulta_train_t)
    pasado_t_rs <- dbSendQuery(mydb, consulta_train_t)
    typeof(pasado_t_rs)
    leido_t <- dbFetch(pasado_t_rs, n = -1) 
    dbClearResult(pasado_t_rs)
    print(paste("TRAIN-T: ", nrow(leido_t), "x", ncol(leido_t)))
    # print(head(leido_t, n = 5L))
  }
  
  if (incluyeValidation == TRUE ) {
    print('------------- VALIDATION-Features (INPUT) -----------------------')
    consulta_validation_f <- paste("SELECT * FROM ", nombre_tabla_v, tag, " LIMIT ", limiteSql, ';', sep = '')
    print(consulta_validation_f)
    pasado_vf_rs <- dbSendQuery(mydb, consulta_validation_f)
    typeof(pasado_vf_rs)
    pasado_vf <- dbFetch(pasado_vf_rs, n = -1)
    dbClearResult(pasado_vf_rs)
    mes_indice_v <- which( colnames(pasado_vf) == "mes_norm" ) #EXCEPCION Quito columna mes
    pasado_vf_sinmes <- pasado_vf[, -mes_indice_v]
    print(paste("VALIDATION-F: ", nrow(pasado_vf_sinmes), "x", ncol(pasado_vf_sinmes)))
    # print(head(pasado_vf_sinmes, n = 5L))
    }

    
  # ------------ SALIDAS
  salida_1 <- "" #default
  salida_2 <- "" #default
  salida_3 <- TRUE #Default: indica si la salida deseada (salida_1 ó salida_2) tiene mas de 0 filas.
  
  if (incluyeTargets == TRUE) {
    #Juntar FEATURES y TARGET
    leido_ft <- cbind(leido_f_sinmes, leido_t)
    if (nrow(leido_ft) == 0) {
      salida_3 <- FALSE
    } else {
      print(paste("leerDesdeBaseDatosYEscribirCSV --->nrow(leido_ft)=",nrow(leido_ft)))
      leido_ft[,"INDICE_ORDEN"] <- seq.int(from = 1, to = nrow(leido_ft), by = 1) #anhado una columna INDICE_ORDEN (al dataframe), para poder recuperar el ORDEN al final
      print(paste("leido_ft: ", nrow(leido_ft), "x", ncol(leido_ft)))
      salida_1 <- leido_ft
    }
    
  } else {
    if (nrow(leido_f_sinmes) == 0) {
      salida_3 <- FALSE
    } else {
      print(paste("leerDesdeBaseDatosYEscribirCSV --->nrow(leido_f_sinmes)=",nrow(leido_f_sinmes)))
      leido_f_sinmes[,"INDICE_ORDEN"] <- seq.int(from = 1, to = nrow(leido_f_sinmes), by = 1) #anhado una columna INDICE_ORDEN (al dataframe), para poder recuperar el ORDEN al final
      salida_1 <- leido_f_sinmes
    }
  }
  
  if (incluyeValidation == TRUE ) {
    if (nrow(pasado_vf_sinmes) == 0) {
      salida_3 <- FALSE
    } else {
      print(paste("leerDesdeBaseDatosYEscribirCSV --->nrow(pasado_vf_sinmes)=",nrow(pasado_vf_sinmes)))
      pasado_vf_sinmes[,"INDICE_ORDEN"] <- seq.int(from = 1, to = nrow(pasado_vf_sinmes), by = 1) #anhado una columna INDICE_ORDEN (al dataframe), para poder recuperar el ORDEN al final
      salida_2 <- pasado_vf_sinmes
    }
    
  }
  
  #Cerramos las conexiones abiertas
  # dbDisconnect(dbListConnections(MySQL())[[1]])
  
  print('--------------- leerDesdeBaseDatosYEscribirCSV: FIN ------------')
  
  return(list(salida_1, salida_2, salida_3))
}


#' Creamos un modelo para cada tipo de DISTANCIA, para usar sólo las columnas útiles para esa distancia
#'
#' @param pasado_ft 
#' @param col_cortas 
#' @param col_medias 
#' @param col_largas 
#' @param quitarColumnasDeOtrasDistancias 
#' @param quitarNas 
#'
#' @return
#' @export
#'
#' @examples
crearFeaturesyTargetDelPasadoParaDistancias <- function(pasado_ft, col_cortas,col_medias,col_largas, quitarColumnasDeOtrasDistancias, quitarNas) {
  
  print('--------------- crearFeaturesyTargetDelPasadoParaDistancias ------------')
  print(paste("pasado_ft:", nrow(pasado_ft), "x", ncol(pasado_ft)))
  print(paste("quitarColumnasDeOtrasDistancias:", quitarColumnasDeOtrasDistancias))
  print(paste("quitarNas:", quitarNas))
  
  pasado_ft_cortas_conNAs <- subset(pasado_ft, distancia_norm <= 0.33)
  pasado_ft_medias_conNAs <- subset(pasado_ft, distancia_norm > 0.33 & distancia_norm <= 0.66)
  pasado_ft_largas_conNAs <- subset(pasado_ft, distancia_norm > 0.66)
  
  print(paste("pasado_ft_cortas_conNAs:", nrow(pasado_ft_cortas_conNAs), "x", ncol(pasado_ft_cortas_conNAs)))
  print(paste("pasado_ft_medias_conNAs:", nrow(pasado_ft_medias_conNAs), "x", ncol(pasado_ft_medias_conNAs)))
  print(paste("pasado_ft_largas_conNAs:", nrow(pasado_ft_largas_conNAs), "x", ncol(pasado_ft_largas_conNAs)))
  
  # Por defecto
  pasado_ft_cortas_coldis <- pasado_ft_cortas_conNAs
  pasado_ft_medias_coldis <- pasado_ft_medias_conNAs
  pasado_ft_largas_coldis <- pasado_ft_largas_conNAs
  
  if (quitarColumnasDeOtrasDistancias == TRUE) {
    print('--Quitando columnas de otras DISTANCIAS...')
    pasado_ft_cortas_coldis <- pasado_ft_cortas_conNAs[, !(names(pasado_ft_cortas_conNAs) %in% col_medias | names(pasado_ft_cortas_conNAs) %in% col_largas)] 
    pasado_ft_medias_coldis <- pasado_ft_medias_conNAs[, !(names(pasado_ft_medias_conNAs) %in% col_cortas | names(pasado_ft_medias_conNAs) %in% col_largas)] 
    pasado_ft_largas_coldis <- pasado_ft_largas_conNAs[, !(names(pasado_ft_largas_conNAs) %in% col_cortas | names(pasado_ft_largas_conNAs) %in% col_medias)]
  }
  
  print(paste("pasado_ft_cortas_coldis:", nrow(pasado_ft_cortas_coldis), "x", ncol(pasado_ft_cortas_coldis)))
  print(paste("pasado_ft_medias_coldis:", nrow(pasado_ft_medias_coldis), "x", ncol(pasado_ft_medias_coldis)))
  print(paste("pasado_ft_largas_coldis:", nrow(pasado_ft_largas_coldis), "x", ncol(pasado_ft_largas_coldis)))
  
  # Por defecto
  pasado_ft_cortas <- pasado_ft_cortas_coldis
  pasado_ft_medias <- pasado_ft_medias_coldis
  pasado_ft_largas <- pasado_ft_largas_coldis
  
  if (quitarNas == TRUE) {
    print('-- Quitando NAs (aunque mantengo columna INDICE_ORDEN)...')
    pasado_ft_cortas <- na.omit(pasado_ft_cortas_coldis)
    pasado_ft_medias <- na.omit(pasado_ft_medias_coldis)
    pasado_ft_largas <- na.omit(pasado_ft_largas_coldis)
  
    print('Comprobacion de NAs...')
    indices_sin_na_cortas <- as.numeric( na.action(pasado_ft_cortas) ) #Indices en los que habia NAs
    if (sum( colSums(is.na(pasado_ft_cortas)) ) != 0) { print('ERROR: Hay columnas con missing data!') } #comprobamos que no hay missing data
    
    indices_sin_na_medias <- as.numeric( na.action(pasado_ft_medias) ) #Indices en los que habia NAs
    if (sum( colSums(is.na(pasado_ft_medias)) ) != 0) { print('ERROR: Hay columnas con missing data!') } #comprobamos que no hay missing data
    
    indices_sin_na_largas <- as.numeric( na.action(pasado_ft_largas) ) #Indices en los que habia NAs
    if (sum( colSums(is.na(pasado_ft_largas)) ) != 0) { print('ERROR: Hay columnas con missing data!') } #comprobamos que no hay missing data
  } 
  
  print(paste("CORTAS (train+test):", nrow(pasado_ft_cortas), "x", ncol(pasado_ft_cortas)))
  print(paste("MEDIAS (train+test):", nrow(pasado_ft_medias), "x", ncol(pasado_ft_medias)))
  print(paste("LARGAS (train+test):", nrow(pasado_ft_largas), "x", ncol(pasado_ft_largas)))
  
  return(list(pasado_ft_cortas, pasado_ft_medias, pasado_ft_largas))
}

#' Leer modelos PCA desde ficheros
#'
#' @param tag 
#'
#' @return
#' @export
#'
#' @examples
cargarModelosPCADesdeFicheros <- function(tag){
  print("Cargando reductores PCA (desde ficheros)...")
  path_pca_cortas <- paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/pca_modelo_cortas_', tag, sep = ''); print(path_pca_cortas)
  path_pca_medias <- paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/pca_modelo_medias_', tag, sep = ''); print(path_pca_medias)
  path_pca_largas <- paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/pca_modelo_largas_', tag, sep = ''); print(path_pca_largas)
  
  modelo_pca_cortas <- readRDS(file = path_pca_cortas)
  modelo_pca_medias <- readRDS(file = path_pca_medias)
  modelo_pca_largas <- readRDS(file = path_pca_largas)

  return(list(modelo_pca_cortas, modelo_pca_medias, modelo_pca_largas))
}

#' Leer modelos PREDICTIVOS entrenados (ganadores) desde ficheros
#'
#' @param tag 
#'
#' @return
#' @export
#'
#' @examples
cargarModelosPredictivosDesdeFicheros <- function(tag) {
  print("Cargando modelos PREDICTIVOS ENTRENADOS (desde fichero)...")
  
  uri_cortas <- paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/modelo_cortas_', tag, sep = '')
  print( paste("URI modelo entrenado para CORTAS: ", uri_cortas))
  modelo_predictivo_cortas <- readRDS(file = uri_cortas)
  print(modelo_predictivo_cortas)
  
  uri_medias <- paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/modelo_medias_', tag, sep = '')
  print( paste("URI modelo entrenado para MEDIAS: ", uri_medias))
  modelo_predictivo_medias <- readRDS(file = uri_medias)
  print(modelo_predictivo_medias)
  
  uri_largas <- paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/modelo_largas_', tag, sep = '')
  print( paste("URI modelo entrenado para LARGAS: ", uri_largas))
  modelo_predictivo_largas <- readRDS(file = uri_largas)
  print(modelo_predictivo_largas)
  
  print("Ya hemos cargado los PREDICTIVOS ENTRENADOS.")
  return(list(modelo_predictivo_cortas, modelo_predictivo_medias, modelo_predictivo_largas))
}

#' Calcula el indice maximo de la variable que está mas cerca del umbral de varianza acumulada deseada.
#'
#' @param pca_modelo_sdev 
#' @param umbral_varianza 
#'
#' @return
#' @export
#'
#' @examples
aplicarUmbralVarianza <- function(distancia_str, pca_modelo_sdev, umbral_varianza) {
  
  # VARIANZA ACUMULADA: aplico el umbral para coger sólo las variables PCx mas importantes
  var_acum <- cumsum(pca_modelo_sdev^2 / sum(pca_modelo_sdev^2))
  indice_umbral <- min( which(var_acum >= umbral_varianza) )
  print(paste(distancia_str, " - Umbral varianza acumulada = ", umbral_varianza))
  print(paste(distancia_str, " - Cogemos ", indice_umbral, "variables transformadas PCx..."))
  
  return(indice_umbral)
}


#' La matriz de covarianza debe ser definida positiva. Si no lo es, se debe a pequenhos errores de precision en el calculo informatico. 
#' Asi que les doy un valor positivo minusculo, para que PCA funcione siempre bien.
#' Explicacion: https://stackoverflow.com/questions/29615440/one-of-eigenvalues-of-covariance-matrix-is-negative-in-r
#'
#' @param A Matriz de covarianza sin ajsutar
#'
#' @return
#' @export
#'
#' @examples
ajustarMatrizCovarianza <- function(A){
  print("**** ajustarMatrizCovarianza() ****")
  library(Matrix)
  # saveRDS(A, file = "/home/carloslinux/Desktop/LOGS/temp_a")
  A_corregida <- nearPD(A, eig.tol = 1e-10, conv.tol = 1e-10)
  A_corregida_matriz <- as.matrix(A_corregida$mat)
  # saveRDS(A_corregida_matriz, file = "/home/carloslinux/Desktop/LOGS/temp_a_corregida")
  print(paste("A_corregida_matriz:", nrow(A_corregida_matriz), "x", ncol(A_corregida_matriz)))
  return(A_corregida_matriz)
}


#' Reducir dimensiones usando PCA (no supervisado).
#' Calcula los eigenvectores que definen las correlaciones entre features.
#'
#' @param path_modelo_pca 
#' @param umbral_varianza 
#' @param input_ft Sin valores NA
#'
#' @return
#' @export
#' 
#'
#' @examples
reducirConPCA <- function(input_ft, path_modelo_pca, umbral_varianza, tipoPCA){
  
  print( paste(" **************************************** reducirConPCA: INICIO *************************** "))
  print(paste("input_ft:", nrow(input_ft), "x", ncol(input_ft)))
  # print(head(input_ft, n = 5L))
  print(paste("path_modelo_pca:", path_modelo_pca))
  print(paste("umbral_varianza:", umbral_varianza))
  print(paste("tipoPCA:", tipoPCA))
  
  library(stats)
  
  indice_target <- which( colnames(input_ft) == "TARGET" ) #Columna TARGET
  input_f_full <- subset(input_ft, select = -indice_target)
  print(paste("input_f_full (sin TARGET):", nrow(input_f_full), "x", ncol(input_f_full)))
  # print(head(input_f_full, n = 5L))
  
  if (sum( colSums(is.na(input_f_full)) ) != 0) { print('ERROR: Hay columnas con missing data en input_f_full!') } #comprobamos que no hay missing data
  
  print('reducirConPCA() --> ALGORITMO...')
  
  
  if (tipoPCA == 'princomp') {
    
    matriz_covarianza_sin_ajustar <- cov(input_f_full)
    eigen_val_vec <- eigen(matriz_covarianza_sin_ajustar)
    # print("Eigenvalores (ANTES de ajustar):"); print(eigen_val_vec$values)
    eigenvaloresNegativos <- sum(eigen_val_vec$values < 0)
    print(paste("matriz_covarianza_sin_ajustar:", nrow(matriz_covarianza_sin_ajustar), "x", ncol(matriz_covarianza_sin_ajustar)))
    print(paste("Cuantos eigenvalores son negativos (antes de ajustar):", eigenvaloresNegativos))
    
    if (eigenvaloresNegativos > 0) {
      ###################################
      # print('PENDIENTE La funcion princomp me indica error (covariance matrix is not non-negative definite).')
      # print('Asi que de momento, limito el numero de filas de entrada, pero esta PENDIENTE de ARREGLAR...')
      #input_f_full_chapuza <- head(input_f_full, 500L)
      ###################################
      print("Covarianza -> Hay eigenvalores negativos. NO tiene sentido. Ajustamos la matriz de covarianza asignandoles un valor positivo casi cero...")
      
      matrizCovAjustada <- ajustarMatrizCovarianza(matriz_covarianza_sin_ajustar)
      eigen_val_vec_AJUSTADO <- eigen(matrizCovAjustada)
      # print("Eigenvalores (DESPUES de ajustar):"); print(eigen_val_vec_AJUSTADO$values)
      eigenvaloresNegativosAjustada <- sum(eigen_val_vec_AJUSTADO$values < 0)
      print(paste("Covarianza -> Eigenvalores negativos en la matriz AJUSTADA:", eigenvaloresNegativosAjustada))
      
      pca_modelo <- princomp(x = input_f_full, cor = FALSE, scores = T, covmat = matrizCovAjustada)
      
    } else {
      print('Covarianza --> Matriz de covarianza NORMAL, con todos sus eigenvalores cero o positivos...')
      pca_modelo <- princomp(x = input_f_full, cor = FALSE, scores = T)
    }
    
    
  } else if (tipoPCA == 'prcomp') {
    pca_modelo <- prcomp(x = input_f_full, retx = TRUE) #Usa SVD, no eigenvalores sobre la matriz de covarianza
  }
  
  
  print(paste("Guardando modelo PCA fichero:", path_modelo_pca))
  saveRDS(pca_modelo, file = path_modelo_pca)
  print("Descripcion del modelo PCA:")
  print(pca_modelo)
  
  #Borrar variable
  rm(pca_modelo)
  
  print( paste(" ************************************** reducirConPCA: FIN **************************** "))
  # return(pca_modelo)
}


#' Reducir dimensiones usando TSNE (supervisado)
#'
#' @param input_ft 
#' @param path_modelo_tsne 
#' @param tipo 
#' @param num_features_output 
#'
#' @return
#' @export
#'
#' @examples
reducirConTSNE <- function(input_ft, tipo, path_modelo_tsne, num_features_output){
  
  print( paste(" **** TSNE:", tipo,"**** "))
  library(tsne)
  library(Rtsne)
  
  indice_target <- which( colnames(input_ft) == "TARGET" ) #Columna TARGET
  input_f_full <- subset(input_ft, select = -indice_target)
  print(paste("input_f_full:", nrow(input_f_full), "x", ncol(input_f_full)))
  #print(head(input_f_full, n=5L))
  
  input_f_full_matrix <- as.matrix(input_f_full)
  set.seed(42) # Set a seed if you want reproducible results
  
  #ALGORITMO
  tsne_modelo <- Rtsne(X = input_f_full_matrix, dims = num_features_output, initial_dims = 50, perplexity = 30,
                       theta = 0.3, check_duplicates = TRUE, pca = TRUE, max_iter = 1000,
                       verbose = TRUE, is_distance = FALSE, pca_center = TRUE, pca_scale = FALSE,
                       momentum = 0.5, final_momentum = 0.8, eta = 200, exaggeration_factor = 12)
  
  #TABLON ANALITICO TRANSFORMADO con los pesos de las componentes (PCx) para cada individuo (fila), solo con las variables que mas peso tienen:
  output_f <- tsne_modelo$Y
  
  print("Ejemplo de filas en TABLON ANALITICO TRANSFORMADO:")
  print(head(output_f, n = 5L))
  
  # PREDICCION CON OTRO DATASET (reciclar esto)
  #pred <- predict(pca, newdata=iris.valid[,1:4])
  
  return(output_f)
}



#' Aplica PCA o TSNE para reducir las dimensioens
#'
#' @param lista_ft_cortasmediaslargas F+T del PASADO (incluye INDICE_ORDEN)
#' @param path_modelo_pca_prefijo 
#' @param pca_umbral_varianza 
#' @param tsne_num_features_output 
#' @param tag 
#'
#' @return
#' @export
#'
#' @examples
reducirDimensionesYObtenerReductores <- function(tag, lista_ft_cortasmediaslargas, tipoReduccion, path_modelo_pca_prefijo, pca_umbral_varianza, tsne_num_features_output) {
  
  print('-------- reducirDimensionesYObtenerReductores: INICIO -----------')
  print(paste("tipoReduccion:", tipoReduccion))
  print(paste("Lista:", length(lista_ft_cortasmediaslargas)))
  
  pasado_ft_cortas <- lista_ft_cortasmediaslargas[[1]]
  pasado_ft_medias <- lista_ft_cortasmediaslargas[[2]]
  pasado_ft_largas <- lista_ft_cortasmediaslargas[[3]]
  
  cortas_ft_sinindice <- subset(pasado_ft_cortas, select = -which( colnames(pasado_ft_cortas) == "INDICE_ORDEN" ))
  medias_ft_sinindice <- subset(pasado_ft_medias, select = -which( colnames(pasado_ft_medias) == "INDICE_ORDEN" ))
  largas_ft_sinindice <- subset(pasado_ft_largas, select = -which( colnames(pasado_ft_largas) == "INDICE_ORDEN" ))
  
  # print('---- CORTAS ----'); print(head(pasado_ft_cortas, n = 5L))
  # print('---- MEDIAS ----'); print(head(pasado_ft_medias, n = 5L))
  # print('---- LARGAS ----'); print(head(pasado_ft_largas, n = 5L))
  
  lista_out <- ""
  
  print('Reduciendo FEATURES por cada dataset por distancia...')
  if (tipoReduccion == "PCA") {
    reducirConPCA(cortas_ft_sinindice, paste(path_modelo_pca_prefijo, 'cortas_', tag, sep = ''), pca_umbral_varianza, "prcomp")
    reducirConPCA(medias_ft_sinindice, paste(path_modelo_pca_prefijo, 'medias_', tag, sep = ''), pca_umbral_varianza, "prcomp")
    reducirConPCA(largas_ft_sinindice, paste(path_modelo_pca_prefijo, 'largas_', tag, sep = ''), pca_umbral_varianza, "prcomp")
    
    lista_out <- cargarModelosPCADesdeFicheros(tag)
    
  } else if (tipoReduccion == "TSNE") {
    #PENDIENTE
    #reducirConTSNE(pasado_ft_cortas, paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/tsne_modelo_',tag, sep = ''), tsne_num_features_output)
  }
  
  print('Devuelve los reductores (transformadores)...')
  print('-------- reducirDimensionesYObtenerReductores: FIN -----------')
  
  return(lista_out)
}


#' Resumen de los meta-pesos (coeficientes) de un objeto CV.SuperLearner
#'
#' @param modelo_cv 
#'
#' @return
#' @export
#'
#' @examples
resumen_pesos <- function(modelo_cv) {
  
  meta_weights = coef(modelo_cv)
  means = colMeans(meta_weights)
  sds = apply(meta_weights, MARGIN = 2,  FUN = function(col) { sd(col) })
  mins = apply(meta_weights, MARGIN = 2, FUN = function(col) { min(col) })
  maxs = apply(meta_weights, MARGIN = 2, FUN = function(col) { max(col) })
  # Combine the stats into a single matrix.
  sl_stats = cbind("mean(weight)" = means, "sd" = sds, "min" = mins, "max" = maxs)
  # Sort by decreasing mean weight.
  sl_stats[order(sl_stats[, 1], decreasing = T), ]
}

#' SELECCION DEL MODELO USANDO SUPERLEARNER
#'
#' @param matrizentrada
#' @param distancia_str 
#' @param ejecutarMulticore True/False. Indica si se quiere ejecutar la parte multicore (para elegir el algoritmo, rendimiento)
#'
#' @return
#' @export
#'
#' @examples
analisis_modelos_superlearner <- function(matrizentrada, distancia_str, ejecutarMulticore){
  
  print(paste("****************** analisis_modelos_superlearner ==>", distancia_str, "******************"))
  
  #Crear datasets features+target para train y test
  index     <- 1:nrow(matrizentrada)
  num_filas_test <- trunc(PORCENTAJE_TEST * length(index)) #(100-X)%-TRAIN, X%-Test
  testindex <- sample(index, num_filas_test, replace = FALSE, prob = NULL)
  testset   <- na.omit(matrizentrada[testindex,])
  trainset  <- na.omit(matrizentrada[-testindex,])
  
  indice_target <- which( colnames(trainset) == "TARGET" ) #Columna TARGET
  x_train <- subset(trainset, select = -indice_target)
  y_train <- trainset$TARGET
  x_test <- subset(testset, select = -indice_target)
  y_test <- testset$TARGET
  
  print( paste( "matrizentrada=", nrow(matrizentrada), "x", ncol(matrizentrada) ) )
  print( paste( "ejecutarMulticore=", ejecutarMulticore ) )
  print( paste( "testset=", nrow(testset), "x", ncol(testset) ) )
  print( paste( "trainset=", nrow(trainset), "x", ncol(trainset) ) )
  print( paste( "x_train=", nrow(x_train), "x", ncol(x_train) ) )
  print( paste( "y_train=", length(y_train) ) ) #Es un vector, no una matriz
  print( paste( "x_test=", nrow(x_test), "x", ncol(x_test) ) )
  print( paste( "y_test=", length(y_test) ) ) #Es un vector, no una matriz
  
  print(paste(R_OUT,"MODELOS_PRED [distancia_str|ejecutarMulticore|x_train|x_test] =",
              distancia_str,'|', ejecutarMulticore,'|',
              nrow(x_train), "x", ncol(x_train),"|",
              nrow(x_test), "x", ncol(x_test),"|",
              sep = ''))
  
  #MODELOS DISPONIBLES en libreria SuperLearner:
  #listWrappers(what = "both")

  set.seed(150)
  
  # print('---------------------HYPERPARAMETROS--------------')
  alpha_seq <- seq(from = 0, to = 1, by = 0.2) #0=ridge regression and 1=lasso
  nfolds_num <- 3
  glmnet_bis <- create.Learner("SL.glmnet",
                               params = list(nfolds = nfolds_num),
                               # tune = list(alpha = alpha_seq),
                               detailed_names = T, verbose = FALSE)
  glmnet_bis
  #print(glmnet_bis$grid)
  
  alpha_seq <- seq(from = 0, to = 1, by = 0.2) #0=ridge regression and 1=lasso
  nfolds_num <- 3
  bayesglm_bis <- create.Learner("SL.bayesglm",
                               params = list(nfolds = nfolds_num),
                               detailed_names = T, verbose = FALSE)
  bayesglm_bis
  #print(bayesglm_bis$grid)
   
  # mtry_seq <- floor( ncol(30) / c(2,3,4) ) #MTRY: how many features are randomly chosen within each decision tree node
  # ntree_seq <- seq(from = 100, to = 1000, by = 300)
  # rf_bis <- create.Learner("SL.randomForest", tune = list(mtry = mtry_seq, ntree = ntree_seq), detailed_names = T, verbose = TRUE, name_prefix = "rf")
  # rf_bis
  # print(rf_bis$grid)
  # -------------------------------------------------------------
  
  print('------- Algoritmos usados -------')
  algoritmosPredictivosUsados <- list( 
    # rf_bis$names,
    glmnet_bis$names,
    bayesglm_bis$names,
    "SL.glmnet","SL.bayesglm", "SL.caret.rpart", "SL.glm", "SL.nnet", "SL.polymars"
    )
  print( t(algoritmosPredictivosUsados) )
  
  
  internal_v <- 3 #inner cross-validation process (replicated across all folds)  
  print( paste('Cross-validation (INTERNA, dentro de cada algoritmo):', toString(internal_v) ) )
  num_v <- 3
  print( paste('Cross-validation (EXTERNA):', toString(num_v) ) )
  
  #PENDIENTE: https://cran.r-project.org/web/packages/SuperLearner/vignettes/Guide-to-SuperLearner.html#test-algorithm-with-multiple-hyperparameter-settings
  #CROSS_VALIDATION: https://cran.r-project.org/web/packages/SuperLearner/vignettes/Guide-to-SuperLearner.html#fit-ensemble-with-external-cross-validation
  
  
  print('-------- UNICORE (con cross validation) ----------')
  modelo_unicore <- SuperLearner::SuperLearner(Y = y_train, X = x_train,
                                 family = gaussian(), # describe the error distribution
                                 SL.library = algoritmosPredictivosUsados, method = "method.NNLS",
                                 id = NULL, verbose = FALSE,
                                 control = list(), cvControl = list(V = num_v, shuffle = FALSE))
  modelo_unicore
  summary(modelo_unicore)
  
  if (ejecutarMulticore) {
    
    print('-------- MULTICORE-CV (con cross validation) ----------')
    print('Se usa para EVALUAR EL RENDIMIENTO: para elegir algoritmos y sus parametros adecuados. Luego habrá que usarlos en un modelo sin CV')
    print('Se puede usar la libreria SNOW (Windows, Linux; pero dificil) o multicore (Linux).')
    num_cores_disponibles <- RhpcBLASctl::get_num_cores()
    num_cores_usados <- (num_cores_disponibles)
    print(paste('Uso ', num_cores_usados, ' cores de ', num_cores_disponibles, ' cores disponibles'))
    options(mc.cores = num_cores_usados) #Uso todas las CPUs (menos una, para dejar libre el PC para trabajar)
    getOption("mc.cores") #En Linux, comprobamos su estamos usando todos los cores
    
    # We need to set a different type of seed that works across cores.
    # Otherwise the other cores will go rogue and we won't get repeatable results.
    # This version is for the "multicore" parallel system in R.
    set.seed(1, "L'Ecuyer-CMRG")
    
    cross_validation_v_external_folds <- 2 # splits the data into V folds and then calls SuperLearner
    
    
    print('SUPERLEARNER con MULTICORE:')
    print('Con método NNLS (Non-negative Least Squares)')
    print(paste('cross_validation_v_external_folds = ', cross_validation_v_external_folds))
    print(paste('internal_v = ', internal_v))
    print('Esta forma con CV no mide el rendimiento, asi que lo medimos por fuera...')
  
    system.time({
      modelo_multicore_con_cv <- CV.SuperLearner(Y = y_train, X = x_train, family = gaussian(), 
                                               parallel = "multicore",
                                               SL.library = algoritmosPredictivosUsados, 
                                               method = "method.NNLS", verbose = F,
                                               cvControl = list(V = num_v, shuffle = FALSE), 
                                               innerCvControl = list(list(V = internal_v)), 
                                               saveAll = FALSE)
      })
    
    library(ggplot2)
    
    print('Modelo:')
    print( summary(modelo_multicore_con_cv) )
    
    print('En cada fold, ha ganado este algoritmo:')
    table( simplify2array( modelo_multicore_con_cv$whichDiscreteSL ) )
    print('En cada fold, cada algoritmo tiene estos pesos:')
    modelo_multicore_con_cv$coef
    print('Plot the performance with 95% CIs (use a better ggplot theme):')
    plot(modelo_multicore_con_cv) + theme_bw()
    
    print(resumen_pesos(modelo_multicore_con_cv), digits = 3)
    }
  
  return(list(modelo_unicore, x_test, y_test))
  }

#' Para cada distancia, obtengo el mejor modelo PREDICTIVO y los GUARDO en fichero.
#'
#' @param lista Input F+T (sin INDICE_ORDEN)
#' @param tag 
#'
#' @return
#' @export
#'
#' @examples
calcularModelosPredictivosParaDistanciasYGuardarlos <- function(lista, tag){
  
  print('----------------------------------------- calcularModelosPredictivosParaDistanciasYGuardarlos: INICIO ----------------------------')
  print(paste("tag:", tag))
  
  pasado_ft_cortas <- lista[[1]]
  pasado_ft_medias <- lista[[2]]
  pasado_ft_largas <- lista[[3]]
  
  print(paste(R_OUT,"MODELOS_PRED INPUT [tag|PORCENTAJE_TEST|pasado_ft_cortas|pasado_ft_medias|pasado_ft_largas] =",
              tag,'|',
              PORCENTAJE_TEST,'|',
              nrow(pasado_ft_cortas), "x", ncol(pasado_ft_cortas),'|',
              nrow(pasado_ft_medias), "x", ncol(pasado_ft_medias),'|',
              nrow(pasado_ft_largas), "x", ncol(pasado_ft_largas),
              sep = ''))
  
  if (nrow(pasado_ft_cortas) > 0) {
    print( paste( "pasado_ft_cortas=", nrow(pasado_ft_cortas), "x", ncol(pasado_ft_cortas) ) ); 
    # print(head(pasado_ft_cortas))
    boxplot(pasado_ft_cortas, cex.axis=0.5) 
    out_cortas <- analisis_modelos_superlearner(pasado_ft_cortas, "CORTAS", FALSE)
    modelo_cortas <- out_cortas[[1]]
    path_modelo_cortas <- paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/modelo_cortas_', tag, sep = '');
    print(paste('Guardando modelo PREDICTIVO: ', path_modelo_cortas))
    saveRDS(modelo_cortas, file = path_modelo_cortas)
    rm(modelo_cortas)
    
  } else {
    print("WARNING El dataset FT de CORTAS NO tiene suficientes filas para poder crear un MODELO PREDICTIVO")
  }
  
  
  if (nrow(pasado_ft_medias) > 0) {
    print( paste( "pasado_ft_medias=", nrow(pasado_ft_medias), "x", ncol(pasado_ft_medias) ) ); 
    # print(head(pasado_ft_medias))
    boxplot(pasado_ft_medias, cex.axis=0.5) 
    
    out_medias <- analisis_modelos_superlearner(pasado_ft_medias, "MEDIAS", FALSE)
    modelo_medias <- out_medias[[1]]
    path_modelo_medias <- paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/modelo_medias_', tag, sep = '');
    print(paste('Guardando modelo PREDICTIVO: ', path_modelo_medias))
    saveRDS(modelo_medias, file = path_modelo_medias)
    rm(modelo_medias)
  } else {
    print("WARNING El dataset FT de MEDIAS NO tiene suficientes filas para poder crear un MODELO PREDICTIVO")
  }
  
  
  if (nrow(pasado_ft_largas) > 0) {
    print( paste( "pasado_ft_largas=", nrow(pasado_ft_largas), "x", ncol(pasado_ft_largas) ) ); 
    # print(head(pasado_ft_largas))
    boxplot(pasado_ft_largas, cex.axis=0.5) 
    out_largas <- analisis_modelos_superlearner(pasado_ft_largas, "LARGAS", FALSE)
    modelo_largas <- out_largas[[1]]
    path_modelo_largas <- paste('/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/modelo_largas_', tag, sep = ''); 
    print(paste('Guardando modelo PREDICTIVO: ', path_modelo_largas))
    saveRDS(modelo_largas, file = path_modelo_largas)
    rm(modelo_largas)
  } else {
    print("WARNING El dataset FT de LARGAS NO tiene suficientes filas para poder crear un MODELO PREDICTIVO")
  }
  
  print('----------------------------------------- calcularModelosPredictivosParaDistanciasYGuardarlos: FIN ----------------------------')
}

#' Calcular PREDICCION para un subgrupo TAG.
#'
#' @param tag 
#' @param output_file_prefijo 
#' @param tipo 
#' @param lista_modelos_predictivos 
#' @param input_f_transformadas Incluye columna INDICE_ORDEN
#'
#' @return
#' @export
#'
#' @examples
predecir <- function(tag, input_f_transformadas, lista_modelos_predictivos, output_file_prefijo, tipo) {
  
  print(paste('--------------- predecir(): INICIO -----------------'))
  print(paste('tag=',tag))
  print(paste("input_f_transformadas: ", length(input_f_transformadas)))
  print(paste('output_file_prefijo=',output_file_prefijo))
  print(paste('tipo=',tipo))
    
  library("SuperLearner")
  
  #Modelos predictivos (no confundir con los reductores de dimensiones)
  modelo_cortas <- lista_modelos_predictivos[[1]]
  modelo_medias <- lista_modelos_predictivos[[2]]
  modelo_largas <- lista_modelos_predictivos[[3]]
  
  # Las entradas estan divididas en 3 subsets segun DISTANCIA. Predecimos cada una por separado con su modelo adecuado.
  # Luego juntamos resultados en un solo dataset de salida, pero manteniendo el ORDEN!!!!!!!!!
  
  
  print(' ---------------Entradas (features: mantener el ORDEN)-----------')
  input_f_transformada_cortas <- as.data.frame(input_f_transformadas[[1]])
  input_f_transformada_medias <- as.data.frame(input_f_transformadas[[2]])
  input_f_transformada_largas <- as.data.frame(input_f_transformadas[[3]])
  
  
  print(paste('input_f_transformada_cortas: ', nrow(input_f_transformada_cortas),'x', ncol(input_f_transformada_cortas)))
  print(paste('input_f_transformada_medias: ', nrow(input_f_transformada_medias),'x', ncol(input_f_transformada_medias)))
  print(paste('input_f_transformada_largas: ', nrow(input_f_transformada_largas),'x', ncol(input_f_transformada_largas)))

  num_cortas <- nrow(input_f_transformada_cortas);
  num_medias <- nrow(input_f_transformada_medias);
  num_largas <- nrow(input_f_transformada_largas);
  
  #anhado una columna INDICE_ORDEN, para poder recuperar el ORDEN al final
  # input_f_transformada_cortas[,"INDICE_ORDEN"] <- seq.int(from = 1, to = num_cortas, by = 1)
  # input_f_transformada_medias[,"INDICE_ORDEN"] <- seq.int(from = num_cortas + 1, to = num_cortas + num_medias, by = 1)
  # input_f_transformada_largas[,"INDICE_ORDEN"] <- seq.int(from = num_cortas + num_medias + 1, to = num_cortas + num_medias + num_largas, by = 1)
  
  print('Omitiendo nulos...')
  input_f_cortas <- na.omit(input_f_transformada_cortas)
  input_f_medias <- na.omit(input_f_transformada_medias)
  input_f_largas <- na.omit(input_f_transformada_largas)
  
  print(paste("CORTAS-F (sin NAs):", nrow(input_f_cortas), "x", ncol(input_f_cortas)))
  print(paste("MEDIAS-F (sin NAs):", nrow(input_f_medias), "x", ncol(input_f_medias)))
  print(paste("LARGAS-F (sin NAs):", nrow(input_f_largas), "x", ncol(input_f_largas)))
  
  
  indice_de_indiceorden_c <- which( colnames(input_f_cortas) == "INDICE_ORDEN" )
  indice_de_indiceorden_m <- which( colnames(input_f_medias) == "INDICE_ORDEN" )
  indice_de_indiceorden_l <- which( colnames(input_f_largas) == "INDICE_ORDEN" )
  
  input_f_cortas_sinindice <- as.data.frame( subset(input_f_cortas, select = -indice_de_indiceorden_c ) );
  print(paste("input_f_cortas_sinindice:", nrow(input_f_cortas_sinindice), "x", ncol(input_f_cortas_sinindice))); print(head(input_f_cortas_sinindice))
  input_f_medias_sinindice <- as.data.frame( subset(input_f_medias, select = -indice_de_indiceorden_m ) );
  print(paste("input_f_medias_sinindice:", nrow(input_f_medias_sinindice), "x", ncol(input_f_medias_sinindice))); print(head(input_f_medias_sinindice))
  input_f_largas_sinindice <- as.data.frame( subset(input_f_largas, select = -indice_de_indiceorden_l ) );
  print(paste("input_f_largas_sinindice:", nrow(input_f_largas_sinindice), "x", ncol(input_f_largas_sinindice))); print(head(input_f_largas_sinindice))
  
  print(paste(R_OUT, tipo, " Input-Features [cortas|medias|largas] = ",
              nrow(input_f_cortas_sinindice), "x", ncol(input_f_cortas_sinindice), "|",
              nrow(input_f_medias_sinindice), "x", ncol(input_f_medias_sinindice), "|",
              nrow(input_f_largas_sinindice), "x", ncol(input_f_largas_sinindice), 
              sep = ''))
  
  #Prediccion CORTAS (sin la columna indice_orden)
  print("Prediciendo usando un modelo ya entrenado y un nuevo dataset...")
  predicciones_t_model_cortas <- predict.SuperLearner(object = modelo_cortas, newdata = input_f_cortas_sinindice, onlySL = TRUE) #No usa los que tienen peso =0
  predicciones_t_cortas <- predicciones_t_model_cortas$pred #Prediccion
  print(paste("predicciones_t_cortas:", nrow(predicciones_t_cortas), "x", ncol(predicciones_t_cortas)))
  print(head(predicciones_t_cortas, n = 5L))

  
  #Prediccion MEDIAS (sin la columna indice_orden)
  print("Prediciendo usando un modelo ya entrenado y un nuevo dataset...")
  predicciones_t_model_medias <- predict.SuperLearner(object = modelo_medias, newdata = input_f_medias_sinindice, onlySL = TRUE) #No usa los que tienen peso =0
  predicciones_t_medias <- predicciones_t_model_medias$pred #Prediccion
  print(paste("predicciones_t_medias:", nrow(predicciones_t_medias), "x", ncol(predicciones_t_medias)))
  # print(head(predicciones_t_medias, n = 5L))
  
  
  #Prediccion LARGAS (sin la columna indice_orden)
  print("Prediciendo usando un modelo ya entrenado y un nuevo dataset...")
  predicciones_t_model_largas <- predict.SuperLearner(object = modelo_largas, newdata = input_f_largas_sinindice, onlySL = TRUE) #No usa los que tienen peso =0
  predicciones_t_largas <- predicciones_t_model_largas$pred #Prediccion
  print(paste("predicciones_t_largas:", nrow(predicciones_t_largas), "x", ncol(predicciones_t_largas)))
  # print(head(predicciones_t_largas, n = 5L))
  
  
  library(plyr)
  print('CBIND: entrada (con INDICE_ORDEN) + salida...')
  output_ft_cortas <- rename( cbind(input_f_cortas, predicciones_t_cortas) , c("predicciones_t_cortas" = "TARGET_predicho"))
  output_ft_medias <- rename( cbind(input_f_medias, predicciones_t_medias) , c("predicciones_t_medias" = "TARGET_predicho"))
  output_ft_largas <- rename( cbind(input_f_largas, predicciones_t_largas) , c("predicciones_t_largas" = "TARGET_predicho"))
  
  print('Cogemos solo las columnas que queremos (indice y target)...')
  output_it_cortas <- subset(output_ft_cortas, select = c(INDICE_ORDEN, TARGET_predicho))
  output_it_medias <- subset(output_ft_medias, select = c(INDICE_ORDEN, TARGET_predicho))
  output_it_largas <- subset(output_ft_largas, select = c(INDICE_ORDEN, TARGET_predicho))
  
  print('Juntamos cortas, medias y largas...')
  cortasymedias <- as.data.frame( rbind(output_it_cortas, output_it_medias) )
  pasado_it <- as.data.frame( rbind( cortasymedias, output_it_largas) )
  print(paste("Dim de pasado_it:", nrow(pasado_it), "x", ncol(pasado_it)));
  # print( head(pasado_it, n = 5L))
  
  print('Ordenamos por INDICE_ORDEN...')
  pasado_it_ordenado <- pasado_it[order(pasado_it$INDICE_ORDEN),] 
  print(paste("Dim de pasado_it_ordenado:", nrow(pasado_it_ordenado), "x", ncol(pasado_it_ordenado)))
  # print(head(pasado_it_ordenado, n = 5L))
  
  print('Rellenamos las filas que eran NAs (rellenando en los huecos del indice, hasta el numero de elementos de entrada)...')
  num_input <- num_cortas + num_medias + num_largas
  df_nulos <- data.frame( matrix(NA, nrow = num_input, ncol = 1) )
  df_nulos$INDICE_ORDEN <- seq.int(nrow(df_nulos))
  print(paste("Dim de df_nulos:", nrow(df_nulos), "x", ncol(df_nulos)))
  # print(head(df_nulos, n = 5L))
  
  # rellenos (con huecos) LEFT OUTER JOIN df_nullos
  juntos <- merge(x = df_nulos, y = pasado_it_ordenado, by = "INDICE_ORDEN", all = TRUE) 
  print(paste("Dimensiones de juntos:", nrow(juntos), "x", ncol(juntos)))
  # print(head(juntos, n = 5L))


  print('Fichero de salida...')
  out_target_predicho <- subset(juntos, select = c(TARGET_predicho))
  
  
  path_output_file <- paste(output_file_prefijo, tag, ".txt", sep = '')
  print(paste('Escribiendo a fichero (target predicho) con', nrow(out_target_predicho), 'filas...'))
  print(paste('Ruta resultado:',path_output_file, sep = ' '))
  out_target_predicho = data.frame(out_target_predicho)
  write.table(out_target_predicho , file = path_output_file, append = FALSE, quote = TRUE, sep = " ",
              eol = "\n", na = "\\N", dec = ".", row.names = FALSE, col.names = FALSE)
  
  
  print(paste('--------------- predecir(): FIN -----------------'))
  
}


#' CADENA de REDUCCION DE DIMENSIONES con todo el PASADO de un TAG
#'
#' @param tag 
#' @param limiteSql 
#' @param tipoReduccion 
#' @param pca_umbral_varianza 
#' @param tsne_num_features_output 
#' @param path_modelo_pca_prefijo 
#'
#' @return
#' @export
#'
#' @examples
ejecutarReduccionDimensiones <- function(tabla_train_f, tabla_test_f, tag, limiteSql, tipoReduccion, path_modelo_pca_prefijo, pca_umbral_varianza, tsne_num_features_output) {
  
  print('--------------- ejecutarReduccionDimensiones: INICIO ------------')
  print( paste( 'tag=', tag, sep = '' ) )
  print( paste( 'limiteSql=', tag, sep = '' ) )
  print( paste( 'tipoReduccion=', tipoReduccion, sep = '' ) )
  print( paste( 'path_modelo_pca_prefijo=', path_modelo_pca_prefijo, sep = '' ) )
  print( paste( 'pca_umbral_varianza=', pca_umbral_varianza, sep = '' ) )
  print( paste( 'tsne_num_features_output=', tsne_num_features_output, sep = '' ) )
  
  #Para quitar las COLUMNAS que no son UTILES para esa DISTANCIA
  col_cortas <- c("vel_real_cortas_mediana_norm", "vel_real_cortas_max_norm", "vel_going_cortas_mediana_norm", "vel_going_cortas_max_norm")
  col_medias <- c("vel_real_longmedias_mediana_norm", "vel_real_longmedias_max_norm", "vel_going_longmedias_mediana_norm", "vel_going_longmedias_max_norm")
  col_largas <- c("vel_real_largas_mediana_norm", "vel_real_largas_max_norm", "vel_going_largas_mediana_norm", "vel_going_largas_max_norm")
  
  establecerConfigGeneral()
  
  #Datos (con INDICE_ORDEN)
  listaDatos <- leerDesdeBaseDatosYEscribirCSV(tabla_train_f, tabla_test_f, "NO USAMOS VALIDATION", 
                                               tag, 
                                               format(limiteSql, scientific = FALSE),
                                               TRUE, FALSE)
  
  # ---------------
  #A=listaDatos[[1]] contiene una matriz de 1000x41 (F+T)
  A <- listaDatos[[1]]
  print(paste(class(A), "A:", nrow(A), "x", ncol(A)))
  
  #B=Subset de todas las columnas excepto "las de distancia": 1000x29
  columnas_distancia <- as.vector( c(col_cortas, col_medias, col_largas) )
  B <- A[, !(names(A) %in% columnas_distancia)]
  print(paste(class(B), "B:", nrow(B), "x", ncol(B)))
  
  #C=Quitar los valores NA, apuntando qué indices tenían NAs (D). Ej: 800x29, con indices NA borrados
  C <- na.omit(B)
  indices_con_na <- as.numeric( na.action(C) ) #Filas de B con NAs
  if (sum( colSums(is.na(C)) ) != 0) { print('ERROR: Hay columnas con missing data en C!') } #comprobamos que no hay missing data
  print(paste(class(C), "C:", nrow(C), "x", ncol(C)))
  print('Numero de filas de C que tenian algun NA:')
  print(length(indices_con_na))
  
  #E = sobre A, quitar las filas de los índices D.
  E <- A[-indices_con_na, ]
  print(paste(class(E), "E:", nrow(E), "x", ncol(E)))
  
  # Aplicar crearFeaturesyTargetDelPasadoParaDistancias sobre E: divide en 3 tablas (por distancias), quitando NAs en esas 3 tablas por separado (por si hubiera valores NA dentro de las columnas de esa distancia)
  lista_ft_cortasmediaslargas <- crearFeaturesyTargetDelPasadoParaDistancias(E, col_cortas,col_medias,col_largas, TRUE, TRUE)
  #-----------------
  
  
  print('------- Reductor (PCA, TSNE...) ------')
  reductores <- reducirDimensionesYObtenerReductores(tag, lista_ft_cortasmediaslargas, tipoReduccion, path_modelo_pca_prefijo, pca_umbral_varianza, tsne_num_features_output)
  
  modelo_pca_cortas <- reductores[[1]]
  modelo_pca_medias <- reductores[[2]]
  modelo_pca_largas <- reductores[[3]]
  
  pasado_ft_cortas_transformada <- NA #default
  pasado_ft_medias_transformada <- NA #default
  pasado_ft_largas_transformada <- NA #default
  
  
  if (is.null(modelo_pca_cortas$scores) && is.null(modelo_pca_cortas$rotation)) {
    print("Modelo PCA para CORTAS es incorrecto, porque su SCORES es NULL. Revisarlo!")
  } else {
    indice_umbral_cortas <- aplicarUmbralVarianza("CORTAS", modelo_pca_cortas$sdev, pca_umbral_varianza)
    #print('CORTAS: separar TARGET, reducir las FEATURES y pegar el TARGET otra vez...')
    pasado_ft_temp <- lista_ft_cortasmediaslargas[[1]]
    indice_t_temp <- which( colnames(pasado_ft_temp) == "TARGET" )
    pasado_f_temp <- subset(pasado_ft_temp, select = -indice_t_temp); #print("CORTAS (antes de PCA):"); print(head(pasado_f_temp, n=5L))
    pasado_f_temp_transformada <- predict(modelo_pca_cortas, pasado_f_temp)  #Reduccion (solo sobre F)
    pasado_ft_cortas_transformada <- cbind(pasado_f_temp_transformada[, 1:indice_umbral_cortas], subset(pasado_ft_temp, select = indice_t_temp)) # F (reducidas) + t
    print(paste(class(pasado_ft_cortas_transformada), "pasado_ft_cortas_transformada:", nrow(pasado_ft_cortas_transformada), "x", ncol(pasado_ft_cortas_transformada)))
    # print(head(pasado_ft_cortas_transformada))
    rm(pasado_ft_temp); rm(indice_t_temp); rm(pasado_f_temp); rm(pasado_f_temp_transformada)
  }
  
  if (is.null(modelo_pca_medias$scores) && is.null(modelo_pca_medias$rotation)) {
    print("Modelo PCA para MEDIAS es incorrecto, porque su SCORES es NULL. Revisarlo!")
  } else {
    indice_umbral_medias <- aplicarUmbralVarianza("MEDIAS", modelo_pca_medias$sdev, pca_umbral_varianza)
    #print('MEDIAS: separar TARGET, reducir las FEATURES y pegar el TARGET otra vez...')
    pasado_ft_temp <- lista_ft_cortasmediaslargas[[2]]
    indice_t_temp <- which( colnames(pasado_ft_temp) == "TARGET" )
    pasado_f_temp <- subset(pasado_ft_temp, select = -indice_t_temp); #print("MEDIAS (antes de PCA):"); print(head(pasado_f_temp, n=5L))
    pasado_f_temp_transformada <- predict(modelo_pca_medias, pasado_f_temp)  #Reduccion (solo sobre F)
    pasado_ft_medias_transformada <- cbind(pasado_f_temp_transformada[, 1:indice_umbral_medias], subset(pasado_ft_temp, select = indice_t_temp)) # F (reducidas) + t
    print(paste(class(pasado_ft_medias_transformada), "pasado_ft_medias_transformada:", nrow(pasado_ft_medias_transformada), "x", ncol(pasado_ft_medias_transformada)))
    # print(head(pasado_ft_medias_transformada))
    rm(pasado_ft_temp); rm(indice_t_temp); rm(pasado_f_temp); rm(pasado_f_temp_transformada)
  }
  
  if (is.null(modelo_pca_largas$scores) && is.null(modelo_pca_largas$rotation)) {
    print("Modelo PCA para LARGAS es incorrecto, porque su SCORES es NULL. Revisarlo!")
  } else {
    indice_umbral_largas <- aplicarUmbralVarianza("LARGAS", modelo_pca_largas$sdev, pca_umbral_varianza)
    #print('LARGAS: separar TARGET, reducir las FEATURES y pegar el TARGET otra vez...')
    pasado_ft_temp <- lista_ft_cortasmediaslargas[[3]]
    indice_t_temp <- which( colnames(pasado_ft_temp) == "TARGET" )
    pasado_f_temp <- subset(pasado_ft_temp, select = -indice_t_temp); #print("MEDIAS (antes de PCA):"); print(head(pasado_f_temp, n=5L))
    pasado_f_temp_transformada <- predict(modelo_pca_largas, pasado_f_temp)  #Reduccion (solo sobre F)
    pasado_ft_largas_transformada <- cbind(pasado_f_temp_transformada[, 1:indice_umbral_largas], subset(pasado_ft_temp, select = indice_t_temp)) # F (reducidas) + t
    print(paste(class(pasado_ft_largas_transformada), "pasado_ft_largas_transformada:", nrow(pasado_ft_largas_transformada), "x", ncol(pasado_ft_largas_transformada)))
    # print(head(pasado_ft_largas_transformada))
    rm(pasado_ft_temp); rm(indice_t_temp); rm(pasado_f_temp); rm(pasado_f_temp_transformada)
  }
  
  
  print('--------------- ejecutarReduccionDimensiones: FIN ------------')
  
  return(list(pasado_ft_cortas_transformada, pasado_ft_medias_transformada, pasado_ft_largas_transformada,
              modelo_pca_cortas, modelo_pca_medias, modelo_pca_largas))
}


#' Aplicar el REDUCTOR de cada distancia.
#'
#' @param input_f Features que incluyen INDICE_ORDEN
#' @param lista_modelos_pca 
#' @param tipo 
#' @param umbral_varianza 
#'
#' @return
#' @export
#'
#' @examples
aplicarReductores <- function(input_f, lista_modelos_pca, tipo, umbral_varianza) {
  
  modelo_pca_cortas <- lista_modelos_pca[[1]]
  modelo_pca_medias <- lista_modelos_pca[[2]]
  modelo_pca_largas <- lista_modelos_pca[[3]]
  
  #Para quitar las COLUMNAS que no son UTILES para esa DISTANCIA
  col_cortas <- c("vel_real_cortas_mediana_norm", "vel_real_cortas_max_norm", "vel_going_cortas_mediana_norm", "vel_going_cortas_max_norm")
  col_medias <- c("vel_real_longmedias_mediana_norm", "vel_real_longmedias_max_norm", "vel_going_longmedias_mediana_norm", "vel_going_longmedias_max_norm")
  col_largas <- c("vel_real_largas_mediana_norm", "vel_real_largas_max_norm", "vel_going_largas_mediana_norm", "vel_going_largas_max_norm")
  
  input_f_cortas <- subset(input_f[, !(names(input_f) %in% col_medias | names(input_f) %in% col_largas)], distancia_norm <= 0.33) 
  input_f_medias <- subset(input_f[, !(names(input_f) %in% col_cortas | names(input_f) %in% col_largas)], distancia_norm > 0.33 & distancia_norm <= 0.66)
  input_f_largas <- subset(input_f[, !(names(input_f) %in% col_cortas | names(input_f) %in% col_medias)], distancia_norm > 0.66)
  
  print( paste(tipo,"-F cortas:", nrow(input_f_cortas), "x", ncol(input_f_cortas)) )
  print( paste(tipo,"-F medias:", nrow(input_f_medias), "x", ncol(input_f_medias)) )
  print( paste(tipo,"-F largas:", nrow(input_f_largas), "x", ncol(input_f_largas)) )
  
  print('Reduciendo dimensiones....')
  indice_de_indiceorden_cortas <- which( colnames(input_f_cortas) == "INDICE_ORDEN" )
  indice_de_indiceorden_medias <- which( colnames(input_f_medias) == "INDICE_ORDEN" )
  indice_de_indiceorden_largas <- which( colnames(input_f_largas) == "INDICE_ORDEN" )
  
  cortas_f_sinindice <- subset(input_f_cortas, select = -indice_de_indiceorden_cortas)
  medias_f_sinindice <- subset(input_f_medias, select = -indice_de_indiceorden_medias)
  largas_f_sinindice <- subset(input_f_largas, select = -indice_de_indiceorden_largas)
  
  input_f_transformada_cortas <- predict(modelo_pca_cortas, cortas_f_sinindice ) #Reduciendo
  input_f_transformada_medias <- predict(modelo_pca_medias, medias_f_sinindice ) #Reduciendo
  input_f_transformada_largas <- predict(modelo_pca_largas, largas_f_sinindice ) #Reduciendo
  
  #Coger solo las features que mas impacto tengan en la varianza
  indice_umbral_cortas <- aplicarUmbralVarianza("CORTAS", modelo_pca_cortas$sdev, umbral_varianza)
  indice_umbral_medias <- aplicarUmbralVarianza("MEDIAS", modelo_pca_medias$sdev, umbral_varianza)
  indice_umbral_largas <- aplicarUmbralVarianza("LARGAS", modelo_pca_largas$sdev, umbral_varianza)
  
  #TABLON ANALITICO TRANSFORMADO con los pesos de las componentes (PCx) para cada individuo (fila), solo con las variables que mas peso tienen:
  input_f_transformada_cortas_conumbral <- input_f_transformada_cortas[, 1:indice_umbral_cortas]
  input_f_transformada_medias_conumbral <- input_f_transformada_medias[, 1:indice_umbral_medias]
  input_f_transformada_largas_conumbral <- input_f_transformada_largas[, 1:indice_umbral_largas]
  
  cortas_f_transf_conindice <- cbind(input_f_transformada_cortas_conumbral, subset(input_f_cortas, select = indice_de_indiceorden_cortas))
  medias_f_transf_conindice <- cbind(input_f_transformada_medias_conumbral, subset(input_f_medias, select = indice_de_indiceorden_medias))
  largas_f_transf_conindice <- cbind(input_f_transformada_largas_conumbral, subset(input_f_largas, select = indice_de_indiceorden_largas))
  
  print('Salida (incluye INDICE_ORDEN)...')
  lista_f_transformadas <- list(cortas_f_transf_conindice, medias_f_transf_conindice, largas_f_transf_conindice)
    
  return(lista_f_transformadas)
}




#' CADENA de ENTRENAMIENTO (train+test) y VALIDATION (rentabilidad externa).
#'
#' @param tag 
#' @param limiteSql 
#' @param tipoReduccion 
#' @param path_modelo_pca_prefijo 
#' @param pca_umbral_varianza 
#'
#' @return
#' @export
#'
#' @examples
ejecutarCadenaEntrenamientoValidation <- function(tag, limiteSql, tipoReduccion, path_modelo_pca_prefijo, pca_umbral_varianza, tsne_num_features_output){
  
  print(paste(R_OUT, '--------------- ejecutarCadenaEntrenamientoValidation: INICIO ------------', sep=''))
  print( paste(R_OUT, 
               'PARAM [tag|limiteSql|tipoReduccion|pca_umbral_varianza|tsne_num_features_output] = ', 
               tag,'|',limiteSql,'|',tipoReduccion,'|',pca_umbral_varianza,'|',tsne_num_features_output, 
               sep = '') )
  
  print( paste( 'tag=', tag, sep = '' ) )
  print( paste( 'limiteSql=', tag, sep = '' ) )
  print( paste( 'tipoReduccion=', tipoReduccion, sep = '' ) )
  print( paste( 'path_modelo_pca_prefijo=', path_modelo_pca_prefijo, sep = '' ) )
  print( paste( 'pca_umbral_varianza=', pca_umbral_varianza, sep = '' ) )
  print( paste( 'tsne_num_features_output=', tsne_num_features_output, sep = '' ) )
  
  establecerConfigGeneral()
  listaDatos <- leerDesdeBaseDatosYEscribirCSV('datos_desa.tb_ds_pasado_train_features_', 
                                               'datos_desa.tb_ds_pasado_train_targets_', 
                                               'datos_desa.tb_ds_pasado_validation_features_', 
                                               tag, 
                                               format(limiteSql, scientific = FALSE), 
                                               TRUE, TRUE)
  
  #pasado_train_ft <- listaDatos[[1]]
  pasado_validation_f <- listaDatos[[2]] #con INDICE_ORDEN
  
  #Aplico PCA sobre train-FT (no devuelve INDICE_ORDEN)
  reduccion_out <- ejecutarReduccionDimensiones('datos_desa.tb_ds_pasado_train_features_', 
                                                'datos_desa.tb_ds_pasado_train_targets_',
                                                tag, limiteSql, tipoReduccion, path_modelo_pca_prefijo, pca_umbral_varianza, tsne_num_features_output)
  
  datasetsBienTransformados <- ( sum(is.na(reduccion_out[[1]])) + sum(is.na(reduccion_out[[2]])) + sum(is.na(reduccion_out[[3]])) == 0)
  
  if (datasetsBienTransformados == TRUE) {
    
    lista_ft_transformadas <- list(reduccion_out[[1]], reduccion_out[[2]], reduccion_out[[3]]) #Sin INDICE_ORDEN
    lista_modelos_pca <- list(reduccion_out[[4]], reduccion_out[[5]], reduccion_out[[6]])
    
    # Modelos predictivos (sobre los datos transformados) y los guarda en ficheros
    calcularModelosPredictivosParaDistanciasYGuardarlos(lista_ft_transformadas, tag)
    
    print('----- VALIDATION (pasado-features): Aplicando reduccion PCA y modelos predictivos para adivinar el target ---')
    
    print('Aplicando PCA sobre Validation (pasado-features)...')
    validation_f_transformadas <- aplicarReductores(pasado_validation_f, lista_modelos_pca, "validation", pca_umbral_varianza) #Con INDICE_ORDEN
    print('Predeciendo el target sobre el validation (pasado-features), usando modelos predictivos...')
    lista_modelos_predictivos <- cargarModelosPredictivosDesdeFicheros(tag)
    predecir(tag, validation_f_transformadas, lista_modelos_predictivos, "/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/pasado_validation_targets_predichos_", "VALIDATION")
    
  } else {
    print("ERROR Algun dataset no ha sido bien transformado")
  }
  
  
  print(paste(R_OUT, '--------------- ejecutarCadenaEntrenamientoValidation: FIN ------------', sep=''))
}


#' CADENA de ENTRENAMIENTO (train+test) con todo el PASADO-TTV que conocemos (ya sabemos el SUBGRUPO ganador)
#'
#' @param tag 
#'
#' @return
#' @export
#'
#' @examples
ejecutarCadenaEntrenamientoTTV <- function(tag, limiteSql, tipoReduccion, path_modelo_pca_prefijo, pca_umbral_varianza, tsne_num_features_output){
  
  print(paste(R_OUT, '--------------- ejecutarCadenaEntrenamientoTTV: INICIO ------------', sep=''))
  print( paste(R_OUT, 
               'PARAM [tag|limiteSql|tipoReduccion|pca_umbral_varianza|tsne_num_features_output] = ', 
               ,tag,'|',limiteSql,'|',tipoReduccion,'|',pca_umbral_varianza,'|',tsne_num_features_output, 
               sep = '') )
  
  print( paste( 'tag=', tag, sep = '' ) )
  print( paste( 'limiteSql=', tag, sep = '' ) )
  print( paste( 'tipoReduccion=', tipoReduccion, sep = '' ) )
  print( paste( 'pca_umbral_varianza=', pca_umbral_varianza, sep = '' ) )
  print( paste( 'tsne_num_features_output=', tsne_num_features_output, sep = '' ) )
  
  establecerConfigGeneral()
  
  #Aplico PCA sobre train-FT (no devuelve INDICE_ORDEN)
  reduccion_out <- ejecutarReduccionDimensiones('datos_desa.tb_ds_pasado_ttv_features_', 
                                                'datos_desa.tb_ds_pasado_ttv_targets_',
                                                tag, limiteSql, tipoReduccion, path_modelo_pca_prefijo, pca_umbral_varianza, tsne_num_features_output)
  
  lista_ft_transformadas <- list(reduccion_out[[1]], reduccion_out[[2]], reduccion_out[[3]])
  # lista_modelos_pca <- list(reduccion_out[[4]], reduccion_out[[5]], reduccion_out[[6]])
  
  # Modelos predictivos (sobre los datos transformados)
  calcularModelosPredictivosParaDistanciasYGuardarlos(lista_ft_transformadas, tag)
  
  
  print(paste(R_OUT, '--------------- ejecutarCadenaEntrenamientoTTV: FIN ------------', sep=''))
}


#' CADENA de PREDICCION DEL FUTURO (train+test)
#'
#' @param tag 
#'
#' @return
#' @export
#'
#' @examples
ejecutarCadenaPredecirFuturo <- function(tag, limiteSql, tipoReduccion, path_modelo_pca_prefijo, pca_umbral_varianza, tsne_num_features_output){
  
  print(paste(R_OUT, '--------------- ejecutarCadenaPredecirFuturo: INICIO ------------', sep=''))
  print( paste(R_OUT, 
               'PARAM [modo|tag|limiteSql|tipoReduccion|pca_umbral_varianza|tsne_num_features_output] = ', 
               modo,'|',tag,'|',limiteSql,'|',tipoReduccion,'|',pca_umbral_varianza,'|',tsne_num_features_output, 
               sep = '') )
  
  print( paste( 'tag=', tag, sep = '' ) )
  print( paste( 'limiteSql=', limiteSql, sep = '' ) )
  print( paste( 'tipoReduccion=', tipoReduccion, sep = '' ) )
  print( paste( 'pca_umbral_varianza=', pca_umbral_varianza, sep = '' ) )
  print( paste( 'tsne_num_features_output=', tsne_num_features_output, sep = '' ) )
  
  #Para quitar las COLUMNAS que no son UTILES para esa DISTANCIA
  col_cortas <- c("vel_real_cortas_mediana_norm", "vel_real_cortas_max_norm", "vel_going_cortas_mediana_norm", "vel_going_cortas_max_norm")
  col_medias <- c("vel_real_longmedias_mediana_norm", "vel_real_longmedias_max_norm", "vel_going_longmedias_mediana_norm", "vel_going_longmedias_max_norm")
  col_largas <- c("vel_real_largas_mediana_norm", "vel_real_largas_max_norm", "vel_going_largas_mediana_norm", "vel_going_largas_max_norm")
  
  establecerConfigGeneral()
  
  #Devuelve los datasets (con columna INDICE_ORDEN)
  listaDatos <- leerDesdeBaseDatosYEscribirCSV('datos_desa.tb_ds_futuro_features_', 
                                               'NO SABEMOS TARGETS DEL FUTURO',
                                               'NO_HACEMOS_VALIDATION', 
                                               tag, 
                                               format(limiteSql, scientific = FALSE),
                                               FALSE, FALSE)
  
  if (listaDatos[[3]] == FALSE){
    print('ERROR El metodo leerDesdeBaseDatosYEscribirCSV() ha devuelto 0 filas. Revisara mano. Puede que tenga sentido.')
    
  } else {
    
    futuro_f <- as.data.frame(listaDatos[[1]]) #con INDICE_ORDEN
    print(paste("futuro_f:", nrow(futuro_f), 'x', ncol(futuro_f)))
    print(head(futuro_f, n = 5L))
    
    # ---------------
    #A=listaDatos[[1]] contiene una matriz de 1000x39 (F)
    A <- futuro_f
    print(paste(class(A), "A:", nrow(A), "x", ncol(A)))
    
    #B=Subset de todas las columnas excepto "las de distancia": 1000x29
    columnas_distancia <- as.vector( c(col_cortas, col_medias, col_largas) )
    B <- A[, !(names(A) %in% columnas_distancia)]
    print(paste(class(B), "B:", nrow(B), "x", ncol(B)))
    
    #C=Quitar los valores NA, apuntando qué indices tenían NAs (D). Ej: 800x29, con indices NA borrados
    C <- na.omit(B)
    indices_con_na <- as.numeric( na.action(C) ) #Filas de B con NAs
    if (sum( colSums(is.na(C)) ) != 0) { print('ERROR: Hay columnas con missing data en C!') } #comprobamos que no hay missing data
    print(paste(class(C), "C:", nrow(C), "x", ncol(C)))
    print('Numero de filas de C que tenian algun NA:')
    print(indices_con_na)
    
    #E = sobre A, quitar las filas de los índices D.
    E <- A[-indices_con_na, ]
    print(paste(class(E), "E:", nrow(E), "x", ncol(E)))
    
    # Aplicar crearFeaturesyTargetDelPasadoParaDistancias sobre E: divide en 3 tablas (por distancias), quitando NAs en esas 3 tablas por separado (por si hubiera valores NA dentro de las columnas de esa distancia)
    lista_f_cortasmediaslargas <- crearFeaturesyTargetDelPasadoParaDistancias(E, col_cortas,col_medias,col_largas, TRUE, TRUE)
    #-----------------
    
    
    print('######## REDUCIR DIMENSIONES ##########')
    lista_modelos_pca <- cargarModelosPCADesdeFicheros(tag)
    modelo_pca_cortas <- lista_modelos_pca[[1]]
    modelo_pca_medias <- lista_modelos_pca[[2]]
    modelo_pca_largas <- lista_modelos_pca[[3]]
    
    #Coger solo las features que mas impacto tengan en la varianza
    indice_umbral_cortas <- aplicarUmbralVarianza("CORTAS", modelo_pca_cortas$sdev, pca_umbral_varianza)
    indice_umbral_medias <- aplicarUmbralVarianza("MEDIAS", modelo_pca_medias$sdev, pca_umbral_varianza)
    indice_umbral_largas <- aplicarUmbralVarianza("LARGAS", modelo_pca_largas$sdev, pca_umbral_varianza)
    
    print('Reduciendo dimensiones....')
    indice_de_indiceorden_cortas <- which( colnames(lista_f_cortasmediaslargas[[1]]) == "INDICE_ORDEN" )
    indice_de_indiceorden_medias <- which( colnames(lista_f_cortasmediaslargas[[2]]) == "INDICE_ORDEN" )
    indice_de_indiceorden_largas <- which( colnames(lista_f_cortasmediaslargas[[3]]) == "INDICE_ORDEN" )
    
    cortas_f_sinindice <- subset(lista_f_cortasmediaslargas[[1]], select = -indice_de_indiceorden_cortas)
    medias_f_sinindice <- subset(lista_f_cortasmediaslargas[[2]], select = -indice_de_indiceorden_medias)
    largas_f_sinindice <- subset(lista_f_cortasmediaslargas[[3]], select = -indice_de_indiceorden_largas)
    
    print('CORTAS: reducir las FEATURES...')
    futuro_f_temp_transformada <- predict(modelo_pca_cortas, cortas_f_sinindice)  #Reduccion
    futuro_f_cortas_transformada <- futuro_f_temp_transformada[, 1:indice_umbral_cortas] # F (reducidas)
    
    print('MEDIAS: reducir las FEATURES...')
    futuro_f_temp_transformada <- predict(modelo_pca_medias, medias_f_sinindice)  #Reduccion
    futuro_f_medias_transformada <- futuro_f_temp_transformada[, 1:indice_umbral_medias] # F (reducidas)
    
    print('LARGAS: reducir las FEATURES...')
    futuro_f_temp_transformada <- predict(modelo_pca_largas, largas_f_sinindice)  #Reduccion
    futuro_f_largas_transformada <- futuro_f_temp_transformada[, 1:indice_umbral_largas] # F (reducidas)
    
    print("Metiendo columna INDICE_ORDEN...")
    cortas_f_transf_conindice <- cbind(futuro_f_cortas_transformada, subset(lista_f_cortasmediaslargas[[1]], select = indice_de_indiceorden_cortas)); print(head(cortas_f_transf_conindice, n=5L))
    medias_f_transf_conindice <- cbind(futuro_f_medias_transformada, subset(lista_f_cortasmediaslargas[[2]], select = indice_de_indiceorden_medias)); print(head(medias_f_transf_conindice, n=5L))
    largas_f_transf_conindice <- cbind(futuro_f_largas_transformada, subset(lista_f_cortasmediaslargas[[3]], select = indice_de_indiceorden_largas)); print(head(largas_f_transf_conindice, n=5L))
    
    lista_f_cortasmediaslargas <- list(cortas_f_transf_conindice, medias_f_transf_conindice, largas_f_transf_conindice) #Lista de features, agrupadas por DISTANCIA, y con columna INDICE_ORDEN
    
    print('######## PREDICCION DEL FUTURO ##########')
    predecir(tag, lista_f_cortasmediaslargas, cargarModelosPredictivosDesdeFicheros(tag), "/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/FILELOAD_ds_futuro_targets_2_", "FUTURO")
  }
  
  print(paste(R_OUT, '--------------- ejecutarCadenaPredecirFuturo: FIN ------------', sep=''))
}


