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
limiteSql <- "1000000"

mydb = dbConnect(MySQL(), user = 'root', password = 'datos1986', dbname = 'datos_desa', host = 'localhost')
on.exit(dbDisconnect(mydb))

leido_1 <- "" #Default
columna_nombre <- 'experiencia'
schema_tabla <- 'datos_desa.tb_elaborada_carrerasgalgos'
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
transformarColumnaYEvaluarNormalidad <- function(col_in, nombre_tabla, nombre_columna) {}


#View(leido_1[[columna_nombre]])
#hist( leido_1[[columna_nombre]], breaks = 50)
# analizarUmbralesDistancia(lista_df_input[[1]]$distancia)
salida <- transformarColumnaYEvaluarNormalidad(leido_1[[columna_nombre]], schema_tabla, columna_nombre);
#hist( salida, breaks = 100)
dbDisconnect(conn = mydb)

print('----------FIN --------------')
