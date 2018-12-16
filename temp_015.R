rm(list = ls())

# --------- VARIABLES GLOBALES -----------------------------------------
R_OUT <- "#R_OUT#--"

library(RMySQL)
library(DBI)
library("standardize")

library(ggplot2)
library(parallel)
library(Matrix)
library(foreach)
library(graphics)
library(e1071)

options(java.parameters = '-Xmx5g') #Memoria 5GB
limiteSql <- "1000000"

mydb = dbConnect(MySQL(), user = 'root', password = 'datos1986', dbname = 'datos_desa', host = 'localhost')
on.exit(dbDisconnect(mydb))

leido_1 <- "" #Default
columna_nombre <- 'remarks_puntos_historico'
schema_tabla <- 'datos_desa.tb_elaborada_carrerasgalgos'
consulta <- paste('SELECT * FROM ', schema_tabla, ' LIMIT ', limiteSql, ';', sep = '')
leido_1_rs <- dbSendQuery(mydb,  consulta)
leido_1 <- dbFetch(leido_1_rs, n = -1)
dbClearResult(leido_1_rs)


#SKEWNESS: correccion explicada en http://rstudio-pubs-static.s3.amazonaws.com/1563_1ae2544c0e324b9bb7f6e63cf8f9e098.html
skew.score <- function(c, x){
  return( (e1071::skewness(log(x + c)))^2 )
}

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
transformarColumnaYEvaluarNormalidad <- function(col_in, nombre_tabla, nombre_columna) {}


#View(leido_1[[columna_nombre]])
hist( leido_1[[columna_nombre]], breaks = 50)
# analizarUmbralesDistancia(lista_df_input[[1]]$distancia)
salida <- transformarColumnaYEvaluarNormalidad(leido_1[[columna_nombre]], schema_tabla, columna_nombre);
#hist( salida, breaks = 100)
dbDisconnect(conn = mydb)

print('----------FIN --------------')
