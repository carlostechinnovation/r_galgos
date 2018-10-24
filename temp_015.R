rm(list=ls())

library(RMySQL)
library(DBI)
library("standardize")

library(ggplot2)
library(parallel)
library(Matrix)
library(foreach)
library(graphics)

options(java.parameters = '-Xmx5g') #Memoria 5GB
limiteSql <- "100"

mydb = dbConnect(MySQL(), user = 'root', password = 'datos1986', dbname = 'datos_desa', host = 'localhost')
on.exit(dbDisconnect(mydb))

leido_1 <- "" #Default
columna_nombre <- 'distancia'
schema_tabla <- 'datos_desa.tb_elaborada_carreras'
consulta <- paste('SELECT * FROM ', schema_tabla, ' LIMIT ', limiteSql, ';', sep = '')
leido_1_rs <- dbSendQuery(mydb,  consulta)
leido_1 <- dbFetch(leido_1_rs, n = -1)
dbClearResult(leido_1_rs)


#' Title
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
  num_elementos_distintos <- length( na.omit(unique(col_in)) )
  
  col_transformada <- col_in; #DEFAULT
  col_identificadora <- (nombre_columna == 'id_carrera' || nombre_columna == 'id_campeonato' || nombre_columna == 'galgo_nombre')
  
  if (num_elementos_distintos == 1 ) {
    print(paste('Todos los elementos son iguales (numerico, texto o incluso NA). No transformo esta columna:', nombre_columna))
    
  } else if (col_identificadora == TRUE) {
    print(paste('La columna es un ID, asi que no la transformo:', nombre_columna))
    
  } else {
    
    print(paste('Transformando columna:', nombre_columna))
    
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
      trans1 <- col_in_shifted_normalizada ^ 2
      recortada <- na.omit( head(trans1, MAX_ELEMENTOS_SHAPIRO_WILK) ) #quito todos los valores NA, que no aportan nada estadistico. Pero solo aqui, para mantenerlos en el dataframe de salida
      p1 <- NA #default
      if (length(recortada) >= 3) {
        shapiro_out <- shapiro.test(recortada)
        p1 <- shapiro_out$p.value
      }
      
      
      #print('Transformacion 2: SQRT')
      trans2 <- sqrt(col_in_shifted_normalizada)
      recortada <- na.omit( head(trans2, MAX_ELEMENTOS_SHAPIRO_WILK) ) #quito todos los valores NA, que no aportan nada estadistico.Pero solo aqui, para mantenerlos en el dataframe de salida
      p2 <- NA #default
      if (length(recortada) >= 3) {
        shapiro_out <- shapiro.test(recortada)
        p2 <- shapiro_out$p.value
      }
      
      #print('Transformacion 3: EXPONENCIAL')
      trans3 <- exp(col_in_shifted_normalizada)
      recortada <- na.omit( head(trans3, MAX_ELEMENTOS_SHAPIRO_WILK) ) #quito todos los valores NA, que no aportan nada estadistico. Pero solo aqui, para mantenerlos en el dataframe de salida
      p3 <- NA #default
      if (length(recortada) >= 3) {
        shapiro_out <- shapiro.test(recortada)
        p3 <- shapiro_out$p.value
      }
      
      #print('Transformacion 4: LOG_e')
      trans4 <- log(col_in_shifted_normalizada)
      recortada <- na.omit( head(trans4, MAX_ELEMENTOS_SHAPIRO_WILK) ) #quito todos los valores NA, que no aportan nada estadistico. Pero solo aqui, para mantenerlos en el dataframe de salida
      p4 <- NA #default
      if (length(recortada) >= 3) {
        shapiro_out <- shapiro.test(recortada)
        p4 <- shapiro_out$p.value
      }
      
      # SHAPIRO-WILK (test de normalidad):
      # - Si p-value es mayor que alpha (0.05) entonces no se puede rechazar la hipotesis nula ("la muestra viene de una distribucion NORMAL")
      # - Si p-value es menor que alpha (0.05) entonces rechazamos la hipotesis nula: SEGURO que la muestra NO viene de una distribucion NORMAL.
      #print(paste('p0=',p0,'p1=',p1,'p2=',p2,'p3=',p3,'p4=',p4))
      
      
      ##### GUARDAR transformaciones en PNG
      nombre_fichero <- paste('TRANS_',nombre_tabla,'_',nombre_columna,'.png', sep = '')
      pathFichero <- paste('/home/carloslinux/Desktop/LOGS/015_graficos/', nombre_fichero, sep = '')
      print(pathFichero)
      png(filename = pathFichero, width = 500, height = 1000, units = "px")
      graphics::par(mfrow = c(5, 2))  # GRID para pintar plots
      algun_plot = F
      
      #SELECCION DE tranformacion ganadora: la de mayor p-value.
      if (!is.na(p1) && !is.na(p2) && !is.na(p3) && !is.na(p4)) {
        
          p_mayor = max(p0, p1,p2,p3,p4, na.rm = T)
          
          if (!is.nan(p0)) {
            algun_plot=T
            hist(na.omit(trans0), main = paste('HIST-trans1 p=',p0));  qqnorm(na.omit(trans0)); qqline(na.omit(trans0))
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
            hist(na.omit(trans4), main = paste('HIST-trans4 p=',p4));  qqline(y=na.omit(trans4), ylim=100);  qqline(na.omit(trans4))
            if (p4 == p_mayor) {
              col_transformada <- trans4; print(paste("Tranformo columna", nombre_columna, 'con', 'T4'))
            }
          }
      }
      
      #Cerrar grafico
      if(algun_plot == T) {
        title(paste('TABLA:', nombre_tabla, ' COLUMNA:',nombre_columna), outer = T)
      }
      dev.off()
      
    } else {
      print(paste('Hay',num_elementos_distintos,'elementos numericos distintos. No transformo esta columna:', nombre_columna))
    }
    
    
  }
  
  
  
  
  return(col_transformada)
}


View(leido_1[[columna_nombre]])
salida <- transformarColumnaYEvaluarNormalidad(leido_1[[columna_nombre]], schema_tabla, columna_nombre);
View(salida)
dbDisconnect(conn = mydb)

print('----------FIN --------------')
