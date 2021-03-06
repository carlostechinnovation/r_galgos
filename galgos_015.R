# Script para TRANSFORMAR variables
rm(list = ls())

R_OUT <- "#R_OUT#--"

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
  library(RMySQL)
  library(DBI)
  library("standardize")
  
  library(ggplot2)
  library(parallel)
  library(Matrix)
  library(foreach)
  
  options(java.parameters = '-Xmx5g') #Memoria 5GB
}

#' Conexion a base de datos para LECTURA MASIVA de tablas INPUT.
#'
#' 
#' @param input_1 
#' @param input_2 
#' @param input_3 
#'
#' @return
#' @export
#'
#' @examples
leerTablasInput <- function(limiteSql, input_1, input_2, input_3){
  
  print(paste(R_OUT, '--------------- leerTablasInput: INICIO ------------', sep=''))
  
  print('Conectando a BBDD para LEER...')
  mydb = dbConnect(MySQL(), user = 'root', password = 'datos1986', dbname = 'datos_desa', host = 'localhost')
  #on.exit(dbDisconnect(mydb))
  
  leido_1 <- "" #Default
  leido_1_rs <- dbSendQuery(mydb,  paste('SELECT * FROM ', input_1, ' LIMIT ', limiteSql, ';', sep = ''))
  leido_1 <- dbFetch(leido_1_rs, n = -1)
  dbClearResult(leido_1_rs)
  
  leido_2 <- "" #Default
  leido_2_rs <- dbSendQuery(mydb,  paste('SELECT * FROM ', input_2, ' LIMIT ', limiteSql, ';', sep = ''))
  leido_2 <- dbFetch(leido_2_rs, n = -1)
  dbClearResult(leido_2_rs)
  
  leido_3 <- "" #Default
  leido_3_rs <- dbSendQuery(mydb,  paste('SELECT * FROM ', input_3, ' LIMIT ', limiteSql, ';', sep = ''))
  leido_3 <- dbFetch(leido_3_rs, n = -1)
  dbClearResult(leido_3_rs)
  
  
  dbDisconnect(conn = mydb)
  
  print('--------------- leerTablasInput: FIN ------------')
  
  return(list(leido_1, leido_2, leido_3))
}

analizarUmbralesDistancia <- function(input){
  print('--------------- analizarUmbralesDistancia: INICIO ------------')
  print('--------------- analizarUmbralesDistancia: FIN ------------')
}

#' Transformaciones y Escritura a tabla MYSQL
#'
#'
#' @param input_1 
#' @param input_2 
#' @param input_3 
#' @param output_1_nombre 
#' @param output_2_nombre 
#' @param output_3 
#'
#' @return
#' @export
#'
#' @examples
transformaryGuardar <- function(input_1, input_2, input_3, output_1_nombre, output_2_nombre, output_3_nombre){
  print('--------------- transformaryGuardar: INICIO ------------')

  print('Transformamos explícitamente CADA columna, aplicando una fórmula adecuada a los histogramas q hemos visto en KNIME (o en R usando hist(my_df$col) ) para obtener una forma de GAUSSIANA.')
  print('Despues evaluaremos cómo se ajusta a la gaussiana ideal aplicando TESTS DE NORMALIDAD.')
  print('Tambien hacemos normalizacion z-score (gaussiana mu=0, std=1) de la gaussiana parcial intermedia.')
  input_1 <- analizarColumnasTransformarYCbind(input_1, output_1_nombre)
  input_2 <- analizarColumnasTransformarYCbind(input_2, output_2_nombre)
  input_3 <- analizarColumnasTransformarYCbind(input_3, output_3_nombre)
  
  #View(input_1)
  #View(input_2)
  #View(input_3)
  
  
  print('Conectando a BBDD para ESCRIBIR...')
  mydb = dbConnect(MySQL(), user = 'root', password = 'datos1986', dbname = 'datos_desa', host = 'localhost')
  #on.exit(dbDisconnect(mydb))
  
  print(paste('Escribiendo DFs en TABLAS transformadas...'))
  dbWriteTable(mydb, value = input_1, name = output_1_nombre, overwrite = TRUE )
  dbWriteTable(mydb, value = input_2, name = output_2_nombre, overwrite = TRUE ) 
  dbWriteTable(mydb, value = input_3, name = output_3_nombre, overwrite = TRUE ) 
  
  dbDisconnect(conn = mydb)
  
  return(list(input_1, input_2, input_3))
  print('--------------- transformaryGuardar: FIN ------------')
}


#' Para cada columna, transforma las columnas (si es numérica) o las deja sin transformar (si no numérica).
#'
#' @param a Dataframe con muchas columnas
#'
#' @return
#' @export
#'
#' @examples
analizarColumnasTransformarYCbind <- function(a, prefijo){
  
  ##############################################################
  dir.create(file.path("/home/carloslinux/Desktop/LOGS/", "015_antes"), showWarnings = FALSE)
  file_antes_feature <- paste("/home/carloslinux/Desktop/LOGS/015_antes/", prefijo, ".csv", sep='')
  print(paste('Guardando CSV de tabla antes:', file_antes_feature))
  write.csv(a, file = file_antes_feature)
  ##############################################################
  
  
  for (n in names(a)) {
    
    num_elementos_distintos <- length( na.omit(unique( a[[n]] )) )
    col_identificadora <- (n == 'id_carrera' || n == 'id_campeonato' || n == 'galgo_nombre')
    renombrado_col = paste(n,"_z", sep = "")
    
    #Solo transformamos las numericas, y que no sean IDENTIFICADORAS
    if (num_elementos_distintos >= 2 && !col_identificadora && !is.na(n) && (class(a[[n]]) == "numeric" || class(a[[n]]) == "integer" )) {
      print(paste('Bucle transformando columna:', n, 'que es de tipo:', class(a[[n]])))
      
      #Aplicar formula para conseguir una gaussiana (aprox) intermedia y evaluar su NORMALIDAD (test Shapiro-Wilk: si p-value > 0.05, la variable es casi una gaussiana, que es lo que queremos)
      col_transformada <- transformarColumnaYEvaluarNormalidad(a[[n]], 'TABLA', n)
      
      col_zscored <- scale(col_transformada, center = TRUE, scale = TRUE) #STANDARDIZATION Z-SCORE: resta mu (media) y divide entre std (desviacion estandar)
      
      if (length(a) == 0 || sum(is.na(a)) == length(a)) { #Si vacio o todos son NA
        anchura = 1
        min_czs = 0
        print(paste("Caso raro -> length=",length(a), " sum_NAs=", sum(is.na(a)) ))
        
      } else {
        min_czs <- min(col_zscored, na.rm = T)
        max_csz <- max(col_zscored, na.rm = T)
        anchura <- (max_csz - min_czs )
      }
      
      print(paste("min_czs=", min_czs, "max_csz=",max_csz, "anchura:", anchura))
      
      if (is.infinite(anchura)) {
        a[[renombrado_col]] <- a[[n]] # no transformo
      } else {
        a[[renombrado_col]] <- round( (col_zscored - min_czs) / anchura , 6) #normalizado a rango [0,1]
      }
      
      a[[n]] = NULL
      a[[n]] <- a[[renombrado_col]]
      a[[renombrado_col]] = NULL
      
    } else{
      print(paste('No transformamos columna:', n)) #No la transformo, pero uso la auxiliar para que al final todas las columnas esten en el mismo orden que las columnas en el DF de entrada
      a[[renombrado_col]] <- a[[n]]
      a[[n]] = NULL
      a[[n]] <- a[[renombrado_col]]
      a[[renombrado_col]] = NULL
    }
  }

  return(a)
}

#' SKEWNESS: correccion explicada en http://rstudio-pubs-static.s3.amazonaws.com/1563_1ae2544c0e324b9bb7f6e63cf8f9e098.html
#'
#' @param c 
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
skew.score <- function(c, x){
  return( (e1071::skewness(log(x + c)))^2 )
}


#' Ejecuta TRANSFORMACIONES y evalua cual de ellas es la que mejor se ha aproximado a una GAUSSIANA. Coge la ganadora.
#'
#' @param col_in 
#' @param nombre_tabla 
#' @param nombre_columna 
#'
#' @return
#' @export
#'
#' @examples
transformarColumnaYEvaluarNormalidad <- function(col_in, nombre_tabla, nombre_columna) {
  
  MAX_ELEMENTOS_SHAPIRO_WILK = 4999
  
  col_transformada <- col_in; #DEFAULT
  
  #print(paste('Transformando columna:', nombre_columna))
  
  #Debe ser una columna numerica
  col_in_num <- col_in
  if (!is.numeric(col_in)) {
    col_in_num <- as.numeric(col_in)
  }
  
  num_elementos_numericos_distintos <- length( na.omit(unique(col_in)) )
  
  if (num_elementos_numericos_distintos >= 3) {
    
    #Para poder aplicar algunas transformaciones (srqt, log) los valores no pueden ser negativos. Asi que desplazo toda la distribucion al lado positivo (no pasa nada, porque despues la vamos a hacer z-score SOBRE LA VARIABLE TRANSFORMADA...)
    min_valor <- as.numeric(min(col_in_num, na.rm = T))
    col_in_shifted <- (col_in_num + abs(min_valor))
    
    #Algunas tranformaciones (exp) darian resultados infinitos si la entrada es grande. Por tanto, normalizo
    col_in_shifted_normalizada <- scale(col_in_shifted, center = F, scale = T)
    
    #print('Transformacion 0: SIN TRANSFORMAR')
    trans0 <- col_in_shifted_normalizada
    recortada <- na.omit( head(trans0, MAX_ELEMENTOS_SHAPIRO_WILK) ) #quito todos los valores NA, que no aportan nada estadistico. Pero solo aqui, para mantenerlos en el dataframe de salida
    p0 <- NA #default
    if (length(recortada) >= 3) {
      shapiro_out <- shapiro.test(recortada)
      p0 <- shapiro_out$p.value
    }
    
    #print('Transformacion 1: CUADRADO')
    trans1_aux <- col_in_shifted_normalizada ^ 2
    best.c <- optimise(skew.score, c(0, 20), x = trans1_aux)$minimum
    trans1 <- log(trans1_aux + best.c)
    recortada <- na.omit( head(trans1, MAX_ELEMENTOS_SHAPIRO_WILK) ) #quito todos los valores NA, que no aportan nada estadistico. Pero solo aqui, para mantenerlos en el dataframe de salida
    p1 <- NA #default
    if (length(recortada) >= 3) {
      shapiro_out <- shapiro.test(recortada)
      p1 <- shapiro_out$p.value
    }
    
    #print('Transformacion 2: SQRT')
    trans2_aux <- sqrt(col_in_shifted_normalizada)
    best.c <- optimise(skew.score, c(0, 20), x = trans2_aux)$minimum
    trans2 <- log(trans2_aux + best.c)
    recortada <- na.omit( head(trans2, MAX_ELEMENTOS_SHAPIRO_WILK) ) #quito todos los valores NA, que no aportan nada estadistico.Pero solo aqui, para mantenerlos en el dataframe de salida
    p2 <- NA #default
    if (length(recortada) >= 3) {
      shapiro_out <- shapiro.test(recortada)
      p2 <- shapiro_out$p.value
    }
    
    #print('Transformacion 3: EXPONENCIAL')
    trans3_aux <- exp(col_in_shifted_normalizada)
    best.c <- optimise(skew.score, c(0, 20), x = trans3_aux)$minimum
    trans3 <- log(trans3_aux + best.c)
    recortada <- na.omit( head(trans3, MAX_ELEMENTOS_SHAPIRO_WILK) ) #quito todos los valores NA, que no aportan nada estadistico. Pero solo aqui, para mantenerlos en el dataframe de salida
    p3 <- NA #default
    if (length(recortada) >= 3) {
      shapiro_out <- shapiro.test(recortada)
      p3 <- shapiro_out$p.value
    }
    
    #print('Transformacion 4: LOG_e(A+x)')
    SUMANDO_LOG <- 0.1
    trans4_aux <- log(SUMANDO_LOG + col_in_shifted_normalizada)
    #print(paste("T4 - Skewness antes: ", skewness(trans4_aux)))
    best.c <- optimise(skew.score, c(0, 20), x = trans4_aux)$minimum
    trans4 <- log(trans4_aux + best.c)
    #print(paste("T4 - Skewness despues: ", skewness(trans4)))
    recortada <- na.omit( head(trans4, MAX_ELEMENTOS_SHAPIRO_WILK) ) #quito todos los valores NA, que no aportan nada estadistico. Pero solo aqui, para mantenerlos en el dataframe de salida
    p4 <- NA #default
    if (length(recortada) >= 3) {
      shapiro_out <- shapiro.test(recortada)
      p4 <- shapiro_out$p.value
    }
    
    #print('Transformacion 5: POWER')
    FACTOR <- 0.45
    trans5_aux <- sign(col_in_shifted_normalizada) * abs(col_in_shifted_normalizada)^FACTOR
    best.c <- optimise(skew.score, c(0, 20), x = trans5_aux)$minimum
    trans5 <- log(trans5_aux + best.c)
    recortada <- na.omit( head(trans5, MAX_ELEMENTOS_SHAPIRO_WILK) ) #quito todos los valores NA, que no aportan nada estadistico. Pero solo aqui, para mantenerlos en el dataframe de salida
    p5 <- NA #default
    if (length(recortada) >= 3) {
      shapiro_out <- shapiro.test(recortada)
      p5 <- shapiro_out$p.value
    }
    
    #print('Transformacion 6: x*log(1+x)')
    trans6_aux <- col_in_shifted_normalizada * log(1 + col_in_shifted_normalizada)
    best.c <- optimise(skew.score, c(0, 20), x = trans6_aux)$minimum
    trans6 <- log(trans6_aux + best.c)
    recortada <- na.omit( head(trans6, MAX_ELEMENTOS_SHAPIRO_WILK) ) #quito todos los valores NA, que no aportan nada estadistico. Pero solo aqui, para mantenerlos en el dataframe de salida
    p6 <- NA #default
    if (length(recortada) >= 3) {
      shapiro_out <- shapiro.test(recortada)
      p6 <- shapiro_out$p.value
    }
    
    #print('Transformacion 7: LOG_e(1+x)')
    SUMANDO_LOG <- 1
    trans7_aux <- log(SUMANDO_LOG + col_in_shifted_normalizada)
    best.c <- optimise(skew.score, c(0, 20), x = trans7_aux)$minimum
    trans7 <- log(trans7_aux + best.c)
    recortada <- na.omit( head(trans7, MAX_ELEMENTOS_SHAPIRO_WILK) ) #quito todos los valores NA, que no aportan nada estadistico. Pero solo aqui, para mantenerlos en el dataframe de salida
    p7 <- NA #default
    if (length(recortada) >= 3) {
      shapiro_out <- shapiro.test(recortada)
      p7 <- shapiro_out$p.value
    }
    
    #print('Transformacion 1: CUBO')
    trans8_aux <- col_in_shifted_normalizada ^ 3
    best.c <- optimise(skew.score, c(0, 20), x = trans8_aux)$minimum
    trans8 <- log(trans8_aux + best.c)
    recortada <- na.omit( head(trans8, MAX_ELEMENTOS_SHAPIRO_WILK) ) #quito todos los valores NA, que no aportan nada estadistico. Pero solo aqui, para mantenerlos en el dataframe de salida
    p8 <- NA #default
    if (length(recortada) >= 3) {
      shapiro_out <- shapiro.test(recortada)
      p8 <- shapiro_out$p.value
    }
    
    #print('Transformacion 1: POT_CUATRO')
    trans9_aux <- col_in_shifted_normalizada ^ 4
    best.c <- optimise(skew.score, c(0, 20), x = trans9_aux)$minimum
    trans9 <- log(trans9_aux + best.c)
    recortada <- na.omit( head(trans9, MAX_ELEMENTOS_SHAPIRO_WILK) ) #quito todos los valores NA, que no aportan nada estadistico. Pero solo aqui, para mantenerlos en el dataframe de salida
    p9 <- NA #default
    if (length(recortada) >= 3) {
      shapiro_out <- shapiro.test(recortada)
      p9 <- shapiro_out$p.value
    }
    # SHAPIRO-WILK (test de normalidad):
    # - Si p-value es mayor que alpha (0.05) entonces no se puede rechazar la hipotesis nula ("la muestra viene de una distribucion NORMAL")
    # - Si p-value es menor que alpha (0.05) entonces rechazamos la hipotesis nula: SEGURO que la muestra NO viene de una distribucion NORMAL.
    print(paste('p0=',p0,'p1=',p1,'p2=',p2,'p3=',p3,'p4=',p4,'p5=',p5,'p6=',p6,'p7=',p7,'p8=',p8,'p9=',p9))
    
    
    ##### GUARDAR transformaciones en PNG
    nombre_fichero <- paste('TRANS_',nombre_tabla,'_',nombre_columna,'.png', sep = '')
    pathFichero <- paste('/home/carloslinux/Desktop/LOGS/015_graficos/', nombre_fichero, sep = '')
    print(pathFichero)
    png(filename = pathFichero, width = 800, height = 2800, units = "px")
    graphics::par(mfrow = c(11,2))  # GRID para pintar plots
    algun_plot = F
    
    
    
    
    #SELECCION DE tranformacion ganadora: la de mayor p-value.
    if (!is.na(p1) && !is.na(p2) && !is.na(p3) && !is.na(p4) && !is.na(p5) && !is.na(p6) && !is.na(p7) && !is.na(p8) && !is.na(p9)) {
      
      p_mayor = max(p0, p1,p2,p3,p4,p5,p6,p7,p8,p9, na.rm = T)
      
      print(paste(R_OUT, nombre_tabla, '.', nombre_columna, ' --> ', p_mayor, sep = ''))
      
      if (!is.nan(p0)) {
        algun_plot=T
        hist(na.omit(trans0), main = paste('HIST-trans0 p=',p0));  qqnorm(na.omit(trans0)); qqline(na.omit(trans0))
        if (p0 == p_mayor) {
          col_transformada <- trans0; print(paste("Tranformo columna", nombre_columna, 'con', 'T0'))
        }
      }
      
      if (!is.nan(p1)) {
        algun_plot=T
        hist(na.omit(trans1), main = paste('HIST-trans1 p=',p1));  qqnorm(na.omit(trans1)); qqline(na.omit(trans1))
        if (p1 == p_mayor) {
          col_transformada <- trans1; print(paste("Tranformo columna", nombre_columna, 'con', 'T1'))
        }
      }
      
      if (!is.nan(p2)) {
        algun_plot=T
        hist(na.omit(trans2), main = paste('HIST-trans2 p=',p2));  qqnorm(na.omit(trans2)); qqline(na.omit(trans2))
        if (p2 == p_mayor) {
          col_transformada <- trans2; print(paste("Tranformo columna", nombre_columna, 'con', 'T2'))  
        }
      }
      
      if (!is.nan(p3)) {
        algun_plot=T
        hist(na.omit(trans3), main = paste('HIST-trans3 p=',p3));  qqnorm(na.omit(trans3));  qqline(na.omit(trans3))
        if (p3 == p_mayor) {
          col_transformada <- trans3; print(paste("Tranformo columna", nombre_columna, 'con', 'T3'))
        }
      }
      
      if (!is.nan(p4)) {
        algun_plot=T
        hist(na.omit(trans4), main = paste('HIST-trans4 p=',p4));  qqnorm(y=na.omit(trans4));  qqline(na.omit(trans4))
        if (p4 == p_mayor) {
          col_transformada <- trans4; print(paste("Tranformo columna", nombre_columna, 'con', 'T4'))
        }
      }
      
      if (!is.nan(p5)) {
        algun_plot=T
        hist(na.omit(trans5), main = paste('HIST-trans5 p=',p5));  qqnorm(y=na.omit(trans5));  qqline(na.omit(trans5))
        if (p5 == p_mayor) {
          col_transformada <- trans5; print(paste("Tranformo columna", nombre_columna, 'con', 'T5'))
        }
      }
      
      if (!is.nan(p6)) {
        algun_plot=T
        hist(na.omit(trans6), main = paste('HIST-trans6 p=',p6));  qqnorm(y=na.omit(trans6));  qqline(na.omit(trans6))
        if (p6 == p_mayor) {
          col_transformada <- trans6; print(paste("Tranformo columna", nombre_columna, 'con', 'T6'))
        }
      }
      
      if (!is.nan(p7)) {
        algun_plot=T
        hist(na.omit(trans7), main = paste('HIST-trans7 p=',p7));  qqnorm(y=na.omit(trans7));  qqline(na.omit(trans7))
        if (p7 == p_mayor) {
          col_transformada <- trans7; print(paste("Tranformo columna", nombre_columna, 'con', 'T7'))
        }
      }
      
      if (!is.nan(p8)) {
        algun_plot=T
        hist(na.omit(trans8), main = paste('HIST-trans8 p=',p8));  qqnorm(y=na.omit(trans8));  qqline(na.omit(trans8))
        if (p8 == p_mayor) {
          col_transformada <- trans8; print(paste("Tranformo columna", nombre_columna, 'con', 'T8'))
        }
      }
      
      if (!is.nan(p9)) {
        algun_plot=T
        hist(na.omit(trans9), main = paste('HIST-trans9 p=',p9));  qqnorm(y=na.omit(trans9));  qqline(na.omit(trans9))
        if (p9 == p_mayor) {
          col_transformada <- trans8; print(paste("Tranformo columna", nombre_columna, 'con', 'T9'))
        }
      }
      
    }
    
    #Cerrar grafico
    if(algun_plot == T) {
      title(paste('TABLA:', nombre_tabla, ' COLUMNA:',nombre_columna), outer = T)
    }
    dev.off()
    
  } else {
    print(paste('Hay menos de 3 elementos numericos distintos. No puedo usar Shapiro-Wilk para comprobar test de normalidad de las transformadas, asi que ni la transformo:', nombre_columna))
  }
  
  
  return(col_transformada)
}


print('-------------------------------- 015:INICIO ---------------------')
entradas <- commandArgs(trailingOnly = TRUE)
#limiteSql <- "5000" #Para debug solo
if (length(entradas) == 0) {
  print("MAL No tiene parametros de entrada")
} else if (length(entradas) >= 1) {
  print("BIEN Tiene parametros de entrada")
  limiteSql <- entradas[1]
}
rm(entradas) #Borramos array de parametros, para evitar confusiones

establecerConfigGeneral()
lista_df_input <- leerTablasInput(limiteSql, "tb_elaborada_carreras", "tb_elaborada_galgos", "tb_elaborada_carrerasgalgos")
analizarUmbralesDistancia(lista_df_input[[1]]$distancia)
lista_trans <- transformaryGuardar(lista_df_input[[1]], lista_df_input[[2]], lista_df_input[[3]], "tb_trans_carreras", "tb_trans_galgos", "tb_trans_carrerasgalgos")
print('-------------------------------- 015:FIN ---------------------')
