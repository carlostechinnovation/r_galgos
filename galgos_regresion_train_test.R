# Script para TRAIN+TEST persistiendo en fichero el mejor modelo entrenado

# --------- FUNCIONES -----------------------------------------

#' Configuraciones generales
#'
#' @return
#' @export
#'
#' @examples
establecerConfigGeneral <- function(){
  print('--------------- establecerConfigGeneral ------------')
  options(java.parameters = '-Xmx5g') #Memoria 5GB
}


#' Conexion a base de datos para LECTURA MASIVA y escritura a CSV.
#'
#' @param modo 1=(leer de BBDD hacia DF), 2=(leer BBDD, escribir CSV, leer CSV, escribir DF), 3 = (leer CSV hacia DF)
#' @param tag Subgrupo
#' @param limiteSql Limite de las consultas MYSQL (normalmente, sin limite, es decir, 999999999)
#' @param nombre_tabla_f 
#' @param nombre_tabla_t 
#' @param nombre_tabla_v 
#' @param incluyeValidation BOOLEAN Indica si incluye validation
#'
#' @return
#' @export
#'
#' @examples
leerDesdeBaseDatosYEscribirCSV <- function(modo, nombre_tabla_f, nombre_tabla_t, nombre_tabla_v, tag, limiteSql, incluyeValidation){
  
  print(paste('modo=',modo))
  print(paste('tag=',tag))
  print(paste('limiteSql=',limiteSql))
  
  library(RMySQL)
  library(DBI)
  
  if (modo == 1 || modo == 2) {
    print('---------------Conexion a BBDD ------------')
    #Conexiones ya abiertas
    conexiones_abiertas <- dbListConnections(MySQL())
    #Cerramos las conexiones abiertas
    #dbDisconnect(dbListConnections(MySQL())[[1]])
    
    print('Conectando...')
    mydb = dbConnect(MySQL(), user = 'root', password = 'datos1986', dbname = 'datos_desa', host = 'localhost')
    on.exit(dbDisconnect(mydb))
    #dbListTables(mydb)  #Todas las tablas que tengo
    
    print('------------- TRAIN-Features (INPUT) -----------------------')
    consulta_train_f <- paste('SELECT * FROM ', nombre_tabla_f, tag, ' LIMIT ', limiteSql, ';', sep = '')
    print(consulta_train_f)
    pasado_f_rs <- dbSendQuery(mydb,  consulta_train_f)
    typeof(pasado_f_rs)
    pasado_f <- dbFetch(pasado_f_rs, n = -1)
    #dbClearResult(pasado_f_rs)
    mes_indice <- which( colnames(pasado_f) == "mes_norm" ) #EXCEPCION Quito columna mes
    pasado_f <- pasado_f[, -mes_indice]
    attach(pasado_f)
    names(pasado_f)
    print(paste("TRAIN-F: ", nrow(pasado_f), "x", ncol(pasado_f)))
    print(typeof(pasado_f)) #Formato: lista
    
    print('------------------- TRAIN-Targets (INPUT) -----------------')
    consulta_train_t <- paste("SELECT * FROM ", nombre_tabla_t, tag, " LIMIT ", limiteSql, ';', sep = '')
    print(consulta_train_t)
    pasado_t_rs <- dbSendQuery(mydb, consulta_train_t)
    typeof(pasado_t_rs)
    pasado_t <- dbFetch(pasado_t_rs, n = -1) 
    #dbClearResult(pasado_t_rs)
    attach(pasado_t)
    names(pasado_t)
    print(paste("TRAIN-T: ", nrow(pasado_t), "x", ncol(pasado_t)))
    print(typeof(pasado_t))
    
    if (incluyeValidation == TRUE ) {
      print('------------- VALIDATION-Features (INPUT) -----------------------')
      consulta_validation_f <- paste("SELECT * FROM ", nombre_tabla_v, tag, " LIMIT ", limiteSql, ';', sep = '')
      print(consulta_validation_f)
      pasado_vf_rs <- dbSendQuery(mydb, consulta_validation_f)
      typeof(pasado_vf_rs)
      pasado_vf <- dbFetch(pasado_vf_rs, n = -1)
      #dbClearResult(pasado_vf_rs)
      mes_indice_v <- which( colnames(pasado_vf) == "mes_norm" ) #EXCEPCION Quito columna mes
      pasado_vf <- pasado_vf[, -mes_indice_v]
      attach(pasado_vf)
      names(pasado_vf)
      print(paste("VALIDATION-F: ", nrow(pasado_vf), "x", ncol(pasado_vf)))
      print(typeof(pasado_vf)) #Formato: lista
      }
    
    }
  
  if (modo == 2) {
    print('----- Escritura a fichero CSV (para poder trabajar en PCs donde no tengo montada la BBDD con datos) ----')
    print(getwd())
    write.csv(x = pasado_f, file = 'input_features.csv', append = FALSE, quote = TRUE, sep = '|', eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "UTF-8")
    write.csv(x = pasado_t, file = 'input_targets.csv', append = FALSE, quote = TRUE, sep = '|', eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "UTF-8")
    if (incluyeValidation == TRUE ) {
      write.csv(x = pasado_vf, file = 'input_validation_features.csv', append = FALSE, quote = TRUE, sep = '|', eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "UTF-8")
      }
    
    }
  
  if (modo == 2 || modo == 3) {
    print('----- Lectura desde fichero CSV (para poder trabajar en PCs donde no tengo montada la BBDD con datos) ----')
    print(getwd())
    pasado_f <- read.csv('input_features.csv')
    pasado_t <- read.csv('input_targets.csv')
    if (incluyeValidation == TRUE ) {
      pasado_vf <- read.csv('input_validation_features.csv')
      }
  }
  
  #Juntar FEATURES y TARGET
  pasado_ft <- cbind(pasado_f, pasado_t)
  print(paste("pasado_ft: ", nrow(pasado_ft), "x", ncol(pasado_ft)))
  
  
  return(list(pasado_ft, pasado_vf))
}

#' Creamos un modelo para cada tipo de DISTANCIA, para usar sólo las columnas útiles para esa distancia
#'
#' @return
#' @export
#'
#' @examples
crearFeaturesyTargetDelPasadoParaDistancias <- function(pasado_ft) {
  
  print('crearFeaturesyTargetDelPasadoParaDistancias...')
  print(paste("pasado_ft: ", nrow(pasado_ft), "x", ncol(pasado_ft)))
  
  print('--------------------- TRAIN (+test interno) -------------------')
  pasado_ft_cortas <- na.omit( pasado_ft[, !(names(pasado_ft) %in% col_medias | names(pasado_ft) %in% col_largas)] ) #quitar columnas innecesarias
  indices_sin_na_cortas <- as.numeric( na.action(pasado_ft_cortas) ) #Indices en los que habia NAs
  if (sum( colSums(is.na(pasado_ft_cortas)) ) != 0) { print('ERROR: Hay columnas con missing data!') } #comprobamos que no hay missing data
  
  pasado_ft_medias <- na.omit( pasado_ft[, !(names(pasado_ft) %in% col_cortas | names(pasado_ft) %in% col_largas)] )
  indices_sin_na_medias <- as.numeric( na.action(pasado_ft_medias) ) #Indices en los que habia NAs
  if (sum( colSums(is.na(pasado_ft_medias)) ) != 0) { print('ERROR: Hay columnas con missing data!') } #comprobamos que no hay missing data
  
  pasado_ft_largas <- na.omit( pasado_ft[, !(names(pasado_ft) %in% col_cortas | names(pasado_ft) %in% col_medias)] )
  indices_sin_na_largas <- as.numeric( na.action(pasado_ft_largas) ) #Indices en los que habia NAs
  if (sum( colSums(is.na(pasado_ft_largas)) ) != 0) { print('ERROR: Hay columnas con missing data!') } #comprobamos que no hay missing data
  
  paste("CORTAS (train+test):", nrow(pasado_ft_cortas), "x", ncol(pasado_ft_cortas))
  paste("MEDIAS (train+test):", nrow(pasado_ft_medias), "x", ncol(pasado_ft_medias))
  paste("LARGAS (train+test):", nrow(pasado_ft_largas), "x", ncol(pasado_ft_largas))
  
  return(list(pasado_ft_cortas, pasado_ft_medias, pasado_ft_largas))
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
  
  print(paste("******************", distancia_str, "******************"))
  
  library(SuperLearner)
  library(nnls)
  library(nnet)
  library(randomForest)
  library(glmnet)
  library(ggplot2)
  library(parallel)
  
  #Crear datasets features+target para train y test
  index     <- 1:nrow(matrizentrada)
  tercio <- trunc(0.30 * length(index)) #70%-TRAIN, 30%-Test
  testindex <- sample(index, tercio, replace = FALSE, prob = NULL)
  testset   <- na.omit(matrizentrada[testindex,])
  trainset  <- na.omit(matrizentrada[-testindex,])
  
  indice_target <- which( colnames(trainset) == "TARGET" ) #Columna TARGET
  x_train <- subset(trainset, select = -indice_target)
  y_train <- trainset$TARGET
  x_test <- subset(testset, select = -indice_target)
  y_test <- testset$TARGET
  
  print( paste( "matrizentrada=",nrow(matrizentrada), "x", ncol(matrizentrada) ) )
  print( paste( "testset=",nrow(testset), "x", ncol(testset) ) )
  print( paste( "trainset=",nrow(trainset), "x", ncol(trainset) ) )
  print( paste( "x_train=",nrow(x_train), "x", ncol(x_train) ) )
  print( paste( "y_train=",length(y_train) ) )
  print( paste( "x_test=",nrow(x_test), "x", ncol(x_test) ) )
  print( paste( "y_test=",length(y_test) ) )
  
  #listWrappers()  #MODELOS DISPONIBLES en libreria SuperLearner:
  
  set.seed(150)
  
  algoritmosPredictivosTodos <- list("SL.bartMachine", "SL.bayesglm",  "SL.biglasso",  "SL.caret",
                                     "SL.caret.rpart",  "SL.cforest", "SL.dbarts",  "SL.earth",
                                     "SL.extraTrees",  "SL.gam",  "SL.gbm",  "SL.glm",  "SL.glm.interaction",
                                     "SL.glmnet",  "SL.ipredbagg",   "SL.kernelKnn",  "SL.knn",
                                     "SL.ksvm",  "SL.lda",  "SL.leekasso",  "SL.lm",  "SL.loess",
                                     "SL.logreg",  "SL.mean",  "SL.nnet",  "SL.nnls",  "SL.polymars",
                                     "SL.qda",  "SL.randomForest",  "SL.ranger",  "SL.ridge",  "SL.rpart",
                                     "SL.rpartPrune",  "SL.speedglm",  "SL.speedlm",  "SL.step",
                                     "SL.stepAIC",  "SL.step.forward",  "SL.step.interaction",  "SL.svm",
                                     "SL.template",  "SL.xgboost")
    
  print('---------------------HYPERPARAMETROS--------------')
  #Explicacion: override default parameters of some functions to fit better
  mtry_seq <- floor(sqrt(ncol(x_train)) * c(1,3,5,8,13)) #MTRY: how many features are randomly chosen within each decision tree node
  learners_rf <- create.Learner(base_learner = "SL.randomForest", params = list(), tune = list(mtry = mtry_seq), verbose = TRUE)
  #print(learners_rf)
  
  
  print('------- Algoritmos usados -------')
  algoritmosPredictivosUsados <- list( 
    #learners_rf$names, 
    "SL.randomForest", "SL.bayesglm", "SL.caret.rpart", "SL.glm", "SL.glmnet", "SL.nnet", "SL.polymars")
  print(algoritmosPredictivosUsados)
  

  internal_v <- 2 #inner cross-validation process (replicated across all folds)  
  print( paste('Cross-validation (INTERNA, dentro de cada algoritmo):', toString(internal_v) ) )
  num_v <- 2
  print( paste('Cross-validation (EXTERNA):', toString(num_v) ) )
  
  #PENDIENTE: https://cran.r-project.org/web/packages/SuperLearner/vignettes/Guide-to-SuperLearner.html#test-algorithm-with-multiple-hyperparameter-settings
  #CROSS_VALIDATION: https://cran.r-project.org/web/packages/SuperLearner/vignettes/Guide-to-SuperLearner.html#fit-ensemble-with-external-cross-validation
  
  
  print('-------- UNICORE (con cross validation) ----------')
  modelo_unicore <- SuperLearner(Y = y_train, X = x_train, family = gaussian(), 
                                 SL.library = algoritmosPredictivosUsados, method = "method.NNLS",
                                 id = NULL, verbose = TRUE,
                                 control = list(), cvControl = list(V = num_v, shuffle = FALSE), obsWeights = NULL, env = parent.frame())
  summary(modelo_unicore)
  
  if (ejecutarMulticore) {
    
    print('-------- MULTICORE-CV (con cross validation) ----------')
    print('Se usa para EVALUAR EL RENDIMIENTO: para elegir algoritmos y sus parametros adecuados. Luego habrá que usarlos en un modelo sin CV')
    print('Se puede usar la libreria SNOW (Windows, Linux; pero dificil) o multicore (Linux).')
    num_cores_disponibles <- RhpcBLASctl::get_num_cores()
    num_cores_usados <- (num_cores_disponibles)
    print(paste('Uso ', num_cores_usados, ' cores de ', num_cores_disponibles, ' cores disponibles'))
    options(mc.cores = num_cores_usados) #Uso todas las CPUs (menos una, para dejar libre el PC para trabajar)
    getOption("mc.cores") #En Linuxsave(out_multicore_simple_predich, comprobamos su estamos usando todos los cores
    
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

#' Para cada distancia, obtengo el mejor modelo y los GUARDO en fichero.
#'
#' @return
#' @export
#'
#' @examples
obtenerModelosParaDistancias <- function(lista){
  
  pasado_ft_cortas <- lista[[1]]
  pasado_ft_medias <- lista[[2]]
  pasado_ft_largas <- lista[[3]]
  
  out_cortas <- analisis_modelos_superlearner(pasado_ft_cortas, "CORTAS", FALSE)
  out_medias <- analisis_modelos_superlearner(pasado_ft_medias, "MEDIAS", FALSE)
  out_largas <- analisis_modelos_superlearner(pasado_ft_largas, "LARGAS", FALSE)
  
  modelo_cortas <- out_cortas[[1]]
  modelo_medias <- out_medias[[1]]
  modelo_largas <- out_largas[[1]]
  
  #Guardando los modelos a ficheros:
  save(modelo_cortas, file = paste('/home/carloslinux/Desktop/WORKSPACES/wksp_for_r/r_galgos/modelo_cortas_',tag, sep=''))
  save(modelo_medias, file = paste('/home/carloslinux/Desktop/WORKSPACES/wksp_for_r/r_galgos/modelo_medias_',tag, sep=''))
  save(modelo_largas, file = paste('/home/carloslinux/Desktop/WORKSPACES/wksp_for_r/r_galgos/modelo_largas_',tag, sep=''))
  
  rm(modelo_cortas)
  rm(modelo_medias)
  rm(modelo_largas)
  ls() # Compruebo que se han borrado
  }

#' Calcular VALIDATION para un subgrupo TAG.
#'
#' @param tag 
#'
#' @return
#' @export
#'
#' @examples
calcular_y_guardar_validation_file <- function(tag, pasado_vf){
  
  library("SuperLearner")
  
  print(getwd())
  
  # ----------- Modelos ENTRENADOS----------
  load(file = paste('/home/carloslinux/Desktop/WORKSPACES/wksp_for_r/r_galgos/modelo_cortas_',tag, sep=''))
  load(file = paste('/home/carloslinux/Desktop/WORKSPACES/wksp_for_r/r_galgos/modelo_medias_',tag, sep=''))
  load(file = paste('/home/carloslinux/Desktop/WORKSPACES/wksp_for_r/r_galgos/modelo_largas_',tag, sep=''))
  ls() # Compruebo que se han cargado
  
  # Las entradas (validation-features) estan divididas en en 3 subsets segun DISTANCIA. Predecimos cada una por separado con su modelo adecuado. Luego juntamos resultados en un solo dataset de salida, pero manteniendo el ORDEN!!!!!!!!!
  
  
  # ---------------Entradas (validation features: mantener el ORDEN)-----------
  #anhado un indice, para poder recuperar el ORDEN al final
  pasado_vf$INDICE_ORDEN <- seq.int(nrow(pasado_vf))
  
  pasado_vf_cortas <- na.omit( subset(pasado_vf[, !(names(pasado_vf) %in% col_medias | names(pasado_vf) %in% col_largas)], distancia_norm <= 0.33) )
  pasado_vf_medias <- na.omit( subset(pasado_vf[, !(names(pasado_vf) %in% col_cortas | names(pasado_vf) %in% col_largas)], distancia_norm > 0.33 & distancia_norm <= 0.66) )
  pasado_vf_largas <- na.omit( subset(pasado_vf[, !(names(pasado_vf) %in% col_cortas | names(pasado_vf) %in% col_medias)], distancia_norm > 0.66) )
  
  paste("CORTAS-VF (sin NAs):", nrow(pasado_vf_cortas), "x", ncol(pasado_vf_cortas))
  paste("MEDIAS-VF (sin NAs):", nrow(pasado_vf_medias), "x", ncol(pasado_vf_medias))
  paste("LARGAS-VF (sin NAs):", nrow(pasado_vf_largas), "x", ncol(pasado_vf_largas))
  
  
  #Prediccion CORTAS
  predicciones_vt_model_cortas <- predict.SuperLearner(object = modelo_cortas, newdata = subset(pasado_vf_cortas, select = -c(INDICE_ORDEN)), onlySL = TRUE) #No usa los que tienen peso =0
  predicciones_vt_cortas <- predicciones_vt_model_cortas$pred #Prediccion
  print(paste("PREDICCION CORTAS =", length(predicciones_vt_cortas)))
  
  #Prediccion MEDIAS
  predicciones_vt_model_medias <- predict.SuperLearner(object = modelo_medias, newdata = subset(pasado_vf_medias, select = -c(INDICE_ORDEN)), onlySL = TRUE) #No usa los que tienen peso =0
  predicciones_vt_medias <- predicciones_vt_model_medias$pred #Prediccion
  print(paste("PREDICCION MEDIAS =", length(predicciones_vt_medias)))
  
  #Prediccion LARGAS
  predicciones_vt_model_largas <- predict.SuperLearner(object = modelo_largas, newdata = subset(pasado_vf_largas, select = -c(INDICE_ORDEN)), onlySL = TRUE) #No usa los que tienen peso =0
  predicciones_vt_largas <- predicciones_vt_model_largas$pred #Prediccion
  print(paste("PREDICCION LARGAS =", length(predicciones_vt_largas)))
  
  
  library(plyr)
  print('CBIND: entrada(con orden) + salida...')
  pasado_vft_cortas <- rename( cbind(pasado_vf_cortas, predicciones_vt_cortas) , c("predicciones_vt_cortas"="TARGET_predicho"))
  pasado_vft_medias <- rename( cbind(pasado_vf_medias, predicciones_vt_medias) , c("predicciones_vt_medias"="TARGET_predicho"))
  pasado_vft_largas <- rename( cbind(pasado_vf_largas, predicciones_vt_largas) , c("predicciones_vt_largas"="TARGET_predicho"))
  print('Cogemos solo las columnas que queremos (indice y target)...')
  pasado_v_it_cortas <- subset(pasado_vft_cortas, select = c(INDICE_ORDEN, TARGET_predicho))
  pasado_v_it_medias <- subset(pasado_vft_medias, select = c(INDICE_ORDEN, TARGET_predicho))
  pasado_v_it_largas <- subset(pasado_vft_largas, select = c(INDICE_ORDEN, TARGET_predicho))
  print('Juntamos cortas, medias y largas...')
  pasado_v_it <- rbind( rbind(pasado_v_it_cortas, pasado_v_it_medias), pasado_v_it_largas)
  
  print('Ordenamos por INDICE_ORDEN...')
  pasado_v_it_ordenado <- pasado_v_it[order(pasado_v_it$INDICE_ORDEN),] 
  
  print('Rellenamos las  filas que eran NAs (rellenando en los huecos del indice, hasta el numero de elementos de entrada)...')
  num_input <- nrow(pasado_vf)
  df_nulos <- data.frame( matrix(NA, nrow = num_input, ncol = 1) )
  df_nulos$INDICE_ORDEN <- seq.int(nrow(df_nulos))
  juntos <- merge(x = df_nulos, y = pasado_v_it_ordenado, by = "INDICE_ORDEN", all = TRUE) #LEFT OUTER JOIN
  
  print('Fichero de salida...')
  out_validation_target_predicho <- subset(juntos, select = c(TARGET_predicho))
  
  
  path_validation_targets <- paste("/home/carloslinux/Desktop/DATOS_LIMPIO/galgos/pasado_validation_targets_predichos_", tag, ".txt", sep = '')
  print(paste('Escribiendo a fichero de VALIDATION (target predicho) con', nrow(out_validation_target_predicho), 'filas...'))
  print(paste('Ruta resultado:',path_validation_targets, sep = ' '))
  out_validation_target_predicho.df = data.frame(out_validation_target_predicho)
  write.table(out_validation_target_predicho.df , file = path_validation_targets, append = FALSE, quote = TRUE, sep = " ",
              eol = "\n", na = "\\N", dec = ".", row.names = FALSE, col.names = FALSE)
  
}


#' CADENA de ENTRENAMIENTO (train+test) y VALIDATION (rentabilidad externa).
#'
#' @param tag 
#'
#' @return
#' @export
#'
#' @examples
ejecutarCadenaEntrenamientoValidation <- function(tag, limiteSql){
  
  print('--------------- ejecutarCadenaEntrenamientoValidation ------------')
  print( paste( 'tag=', tag, sep = '' ) )
  print( paste( 'limiteSql=', tag, sep = '' ) )
  
  establecerConfigGeneral()
  listaDatos <- leerDesdeBaseDatosYEscribirCSV(1, 
                                               'datos_desa.tb_ds_pasado_train_features_', 
                                               'datos_desa.tb_ds_pasado_train_targets_', 
                                               'datos_desa.tb_ds_pasado_validation_features_', 
                                               tag, 
                                               format(limiteSql, scientific = FALSE), 
                                               TRUE)
  lista_ft_cortasmediaslargas <- crearFeaturesyTargetDelPasadoParaDistancias(listaDatos[[1]])
  obtenerModelosParaDistancias(lista_ft_cortasmediaslargas)
  calcular_y_guardar_validation_file(tag, listaDatos[[2]])
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
  if (modo == 1) {
    ejecutarCadenaEntrenamientoValidation(tag, limiteSql)
  }
  
  #Borramos array de parametros, para evitar confusiones
  rm(entradas)
  
}
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################


