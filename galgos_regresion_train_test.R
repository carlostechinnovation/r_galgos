# Script para TRAIN+TEST persistiendo en fichero el mejor modelo entrenado

options(echo = TRUE) # En la salida, queremos ver los comandos ejecutados

# --------- FUNCIONES -----------------------------------------

#' Configuraciones generales
#'
#' @return
#' @export
#'
#' @examples
establecerConfigGeneral <- function(){
  options(echo = FALSE) # En la salida, queremos ver los comandos ejecutados
  options(java.parameters = '-Xmx5g') #Memoria 5GB
}


#' Conexion a base de datos para LECTURA MASIVA y escritura a CSV.
#'
#' @return
#' @export
#'
#' @examples
leerDesdeBaseDatosYEscribirCSV <- function(tag, limiteSql){
  
  print(paste('tag=',tag))
  
  
  library(RMySQL)
  library(DBI)
  
  # ---------------Conexion a BBDD ------------
  #Conexiones ya abiertas
  conexiones_abiertas <- dbListConnections(MySQL())
  #Cerramos las conexiones abiertas
  #dbDisconnect(dbListConnections(MySQL())[[1]])
  
  mydb = dbConnect(MySQL(), user = 'root', password = 'datos1986', dbname = 'datos_desa', host = 'localhost')
  on.exit(dbDisconnect(mydb))
  #dbListTables(mydb)  #Todas las tablas que tengo
  
  
  # ------------- TRAIN-Features (INPUT) -----------------------
  pasado_f_rs <- dbSendQuery(mydb, paste('SELECT * FROM datos_desa.tb_ds_pasado_train_features_', tag, ' LIMIT ', limiteSql, sep = '') )
  typeof(pasado_f_rs)
  pasado_f <- dbFetch(pasado_f_rs, n = -1)
  #dbClearResult(pasado_f_rs)
  mes_indice <- which( colnames(pasado_f) == "mes_norm" ) #EXCEPCION Quito columna mes
  pasado_f <- pasado_f[, -mes_indice]
  attach(pasado_f)
  names(pasado_f)
  print(paste("FEATURE MATRIX: ", nrow(pasado_f), "x", ncol(pasado_f)))
  print(typeof(pasado_f)) #Formato: lista
  
  # -------------------- TRAIN-Targets (INPUT) -----------------
  pasado_t_rs <- dbSendQuery(mydb,paste("SELECT * FROM datos_desa.tb_ds_pasado_train_targets_", tag, " LIMIT ", limiteSql, sep = '') )
  typeof(pasado_t_rs)
  pasado_t <- dbFetch(pasado_t_rs, n = -1)
  #dbClearResult(pasado_t_rs)
  attach(pasado_t)
  names(pasado_t)
  print(paste("TARGET MATRIX: ", nrow(pasado_t), "x", ncol(pasado_t)))
  print(typeof(pasado_t))
  
  # ------------- VALIDATION-Features (INPUT) -----------------------
  pasado_vf_rs <- dbSendQuery(mydb, paste("SELECT * FROM datos_desa.tb_ds_pasado_validation_features_", tag, " LIMIT ", limiteSql, sep = '') )
  typeof(pasado_vf_rs)
  pasado_vf <- dbFetch(pasado_vf_rs, n = -1)
  #dbClearResult(pasado_vf_rs)
  mes_indice_v <- which( colnames(pasado_vf) == "mes_norm" ) #EXCEPCION Quito columna mes
  pasado_vf <- pasado_vf[, -mes_indice_v]
  attach(pasado_vf)
  names(pasado_vf)
  print(paste("FEATURE MATRIX: ", nrow(pasado_vf), "x", ncol(pasado_vf)))
  print(typeof(pasado_vf)) #Formato: lista
  
  
  # ----- Escritura a fichero CSV (para poder trabajar en PCs donde no tengo montada la BBDD con datos) ----
  print(getwd())
  write.csv(x = pasado_f, file = 'input_features.csv', append = FALSE, quote = TRUE, sep = '|', eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "UTF-8")
  write.csv(x = pasado_t, file = 'input_targets.csv', append = FALSE, quote = TRUE, sep = '|', eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "UTF-8")
  write.csv(x = pasado_vf, file = 'input_validation_features.csv', append = FALSE, quote = TRUE, sep = '|', eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "UTF-8")
  
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
  
  #PENDIENTE: https://cran.r-project.org/web/packages/SuperLearner/vignettes/Guide-to-SuperLearner.html#test-algorithm-with-multiple-hyperparameter-settings
  #CROSS_VALIDATION: https://cran.r-project.org/web/packages/SuperLearner/vignettes/Guide-to-SuperLearner.html#fit-ensemble-with-external-cross-validation
  
  
  print('-------- UNICORE (con cross validation) ----------')
  modelo_unicore <- SuperLearner(Y = y_train, X = x_train, family = gaussian(), 
                                 SL.library = algoritmosPredictivosUsados, method = "method.NNLS",
                                 id = NULL, verbose = FALSE,
                                 control = list(), cvControl = list(), obsWeights = NULL, env = parent.frame())
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


#---------------------- CUERPO de este SCRIPT -------------
# PARAMETROS
# 1 TAG (subgrupo)

args <- commandArgs(trailingOnly = TRUE) #devolver solo los argumentos, pero no el comando (nombre del script)
print(args)

if (length(args) == 0) {
  print("Necesario indicar parametros de entrada")
  
} else if (length(args) >= 1) {
  print( paste('Numero de parametros: ', length(args)) )
  print(args)
  tag <- args[1];  print( paste( 'tag=', tag )   )
  
  #Borramos array de parametros, para evitar confusiones
  rm(args)
}


