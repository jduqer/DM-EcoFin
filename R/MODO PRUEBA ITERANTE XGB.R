rm(list=ls())
gc()
# empezar corriendo las funciones que estan abajo de todo

library("data.table")
library("ROCR")
library("Matrix")
library("xgboost")
library("DiceKriging")
library('dplyr')

#---------------------PARAMETROS----------------------------------#
#raiz del environment
env <- list()

directory <-list()
switch (Sys.info()[['sysname']],
        Windows = { 
          directory$work     <-  "E:\\Personal\\DM ECyF\\UBA2019\\work"
          directory$datasets <-  "E:\\Personal\\DM ECyF\\UBA2019\\dias\\"
        },
        Darwin  = { 
          #directory$work     <-  "~/dm/work/"
          #directory$datasets <-  "~/dm/datasets/dias/"
          directory$work     <-  "/Users/lucaspecina/Desktop/Data/Economia 
          y Finanzas/pruebas-eyf/pruebaitinerante/"
          directory$datasets <-  "/Users/lucaspecina/Desktop/Data/Economia 
          y Finanzas/pruebas-eyf/pruebaitinerante/"          
        },
        Linux   = { 
          directory$work     <-  "~/cloud/cloud1/work/"
          directory$datasets <-  "~/cloud/cloud1/datasets/"
        }
)
env$directory <- directory

env$experimento          <-  'XGBoost01-Filtrado'
env$archivo_grande       <-  "dataset_filtrado.txt.gz"
#-------------------------PARAMETROS--------------------------#

#informacion de meses
mes_primero_ganancia <-  201804
mes_ultimo_ganancia  <-  201904
undersampling <- 1.0
#param del xgboost
semilla_undersampling <- 11111
semilla_xgboost       <- 751007
max_bin               <-     31
subsample             <-      1.0
nround_max            <-   700
nround_early_stopping <-     60
#nuestros hiperparametros fijos: los mejores de 1a0
pventana <- 12
nround <- 195
peta <- 0.112053823824251
palpha <- 11.2863825447698
plambda <- 50.2644261243473
pgamma <- 4.91687856934657
pmin_child_weight <- 44.4373865747519
pmax_depth <- 17
pcolsample_bytree <- 0.977275677906442
#ganancia
ganancia_acierto     <-  19500 
ganancia_noacierto   <-   -500
prob_corte           <-   -ganancia_noacierto*(1/undersampling)/
  ( ganancia_acierto - ganancia_noacierto*(1/undersampling) )


#-------------------------FUNCIONES--------------------------#

#funcion ganancia
fganancia_logistic_xgboost   <- function(probs, clases){
  vlabels <- getinfo(clases, "label")
  gan <-sum(   (probs > prob_corte  ) * ifelse( vlabels== 1, ganancia_acierto, ganancia_noacierto ))
  return(  list(metric = "gananciA", value =  ifelse(  is.na(gan) , 0, gan ) )  )}
#-----------------------------------------------------
#funcion ganancia simple (devuelve un valor para cada fila)
fganancia_simple <- function(probs, clases){
  gan <- (probs > prob_corte  ) * ifelse( clases== 1, ganancia_acierto, ganancia_noacierto )
  return( gan ) }
#-------------------------------------------------------------------------
#------------------------------------------------------------------------------

agregar_canaritos <- function( pdataset,  pcanaritos_cantidad )
{
  vcanaritos <-  paste0( "canarito", 1:pcanaritos_cantidad )
  
  #uso esta semilla para los canaritos
  set.seed(500317)
  
  pdataset[ , (vcanaritos) := 0 ]
  pdataset[ , (vcanaritos) := lapply(.SD, runif), .SDcols = vcanaritos]
  
  #ahora hago que los canaritos sean las primeras variables del dataset
  nuevo_orden <-  c( vcanaritos, setdiff( colnames( pdataset), vcanaritos )) 
  setcolorder( pdataset, nuevo_orden )
}
#------------------------------------------------------------------------------

# Funci?n para calcular la l??nea de muerte de todos los meses
linea_muerte <- function(pmes){
  
  # Los meses en su c?digo de "mes"
  futuro = as.numeric(tb_meses[foto_mes == pmes, mes])
  fin = as.numeric(futuro+2)
  inicio = as.numeric(fin+pventana-1)
  
  
  # Los meses en su forma foto_mes
  primero = as.numeric(tb_meses[mes == inicio, foto_mes])
  ultimo = as.numeric(tb_meses[mes == fin, foto_mes])
  pred = as.numeric(pmes)
  
  #entreno en 10 meses,  periodo  [ primero, ultimo ]
  dgeneracion  <-   xgb.DMatrix( data  = data.matrix( dataset_grande[(sample < undersampling | clase_ternaria==1) & foto_mes>=primero & foto_mes<=ultimo , !c("mes","sample","numero_de_cliente","clase_ternaria"), with=FALSE]),
                                 label = dataset_grande[(sample < undersampling | clase_ternaria==1) & foto_mes>=primero & foto_mes<=ultimo, clase_ternaria ],
                                 missing=NA
  )
  
  #llamo al XGBoost,  notar lo frugal de los hiperparametros
  set.seed( semilla_xgboost ) #mi querida random seed, para que las corridas sean reproducibles
  
  modelo <- xgb.train( 
    data = dgeneracion,  
    missing = NA,
    objective="binary:logistic",
    base_score= mean( getinfo(dgeneracion, "label") ),
    nround= nround_max,
    tree_method = "hist",
    max_bin = max_bin,
    subsample = subsample, 
    colsample_bytree = pcolsample_bytree, 
    eta = peta,
    min_child_weight = pmin_child_weight, 
    max_depth = pmax_depth,
    alpha = palpha, lambda = plambda, gamma = pgamma )
  
  
  #aplico a los datos de pred, que tienen la clase vacia
  daplicacion  <-   xgb.DMatrix( data  = data.matrix( dataset_grande[ foto_mes==pred, 
                                                                      !c("mes","sample","numero_de_cliente","clase_ternaria"), with=FALSE]),
                                 label = dataset_grande[ foto_mes==pred, clase_ternaria ],
                                 missing=NA
  )
  
  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, daplicacion )
  
  #uno las columnas de numero_de_cliente y la probabilidad recien calculada
  prediccion_final  <-  cbind(  dataset_grande[ foto_mes==pred, c("numero_de_cliente","clase_ternaria") ], 
                                aplicacion_prediccion )
  #View(prediccion_final)
  #le doy nombre a las columnas
  colnames( prediccion_final )  <-  c( "numero_de_cliente", "clase01", "prob_positivo" )
  
  #para calcular la ganancia, cuando se corre para meses del pasado
  ganancia_obtenida <- sum(  prediccion_final[ prob_positivo>prob_corte,   
                                               ifelse( clase01 == 1  , ganancia_acierto, ganancia_noacierto)])
  print(paste(pred,":", ganancia_obtenida ))
  
  #Genero las TRES salidas
  #escribo los  titulos  del archivo salida
  setwd(env$directory$work)
  cat("experimento",
      "ganancia obtenida",
      "mes test",
      "undersampling",
      "semilla xgboost",
      "semilla undersampling",
      "max_bin",
      "subsample",
      "nround_max",
      "nround_early_stopping",
      "pventana",
      "nround",
      "peta",
      "palpha",
      "plambda",
      "pgamma",
      "pmin_child_weight",
      "pmax_depth",
      "pcolsample_bytree",
      "\n", 
      sep="\t", 
      file=paste0( "parametros_", env$experimento,"_mes_",pred, "_ganancia.txt" ), 
      fill=FALSE, append=TRUE  
      
  )
  #Grabo todas los parametros
  
  cat(
    env$experimento,
    ganancia_obtenida,
    pred,
    undersampling,
    semilla_xgboost,
    semilla_undersampling,
    max_bin,
    subsample,
    nround_max,
    nround_early_stopping,
    pventana,
    nround,
    peta,
    palpha,
    plambda,
    pgamma,
    pmin_child_weight,
    pmax_depth,
    pcolsample_bytree,
    "\n", 
    sep="\t", 
    file=paste0( "parametros_", env$experimento,"_mes_", pred, "_ganancia.txt" ), 
    fill=FALSE, append=TRUE  
  )         
  
  #grabo todas las probabilidad, simplemente para tenerlo
  fwrite( prediccion_final[ order( -prob_positivo) ],
          file= paste( "ldm_prob_",env$experimento, "_mes_", pred, ".txt", sep="" ),
          sep="\t",
          eol = "\r\n")
  
  #grabo la importancia de las variables
  write.table(  xgb.importance( model = modelo ),
                file=paste( "ldm_importancia_",env$experimento, "_mes_", pred, ".txt", sep="" ),
                sep="\t",
                eol = "\r\n"
  )
}



#-------------------------PREPARACION DE DATOS--------------------------#
#-----------SI CORREMOS LOCAL DESCOMENTAR---------------------------------#
#
#cargar datos
#  dataset_1904   <- fread('/Users/lucaspecina/Desktop/Data/Economia y Finanzas/datasets/dias/201904_dias.txt')
# dataset_1903   <- fread('/Users/lucaspecina/Desktop/Data/Economia y Finanzas/datasets/dias/201903_dias.txt')
#  dataset_1902   <- fread('/Users/lucaspecina/Desktop/Data/Economia y Finanzas/datasets/dias/201902_dias.txt')
#  dataset_1901   <- fread('/Users/lucaspecina/Desktop/Data/Economia y Finanzas/datasets/dias/201901_dias.txt')
# dataset_1812   <- fread('/Users/lucaspecina/Desktop/Data/Economia y Finanzas/datasets/dias/201812_dias.txt')
# dataset_1811   <- fread('/Users/lucaspecina/Desktop/Data/Economia y Finanzas/datasets/dias/201811_dias.txt')
# dataset_1810   <- fread('/Users/lucaspecina/Desktop/Data/Economia y Finanzas/datasets/dias/201810_dias.txt')
#  dataset_1809   <- fread('/Users/lucaspecina/Desktop/Data/Economia y Finanzas/datasets/dias/201809_dias.txt')
#  dataset_1808   <- fread('/Users/lucaspecina/Desktop/Data/Economia y Finanzas/datasets/dias/201808_dias.txt')
#  dataset_1807   <- fread('/Users/lucaspecina/Desktop/Data/Economia y Finanzas/datasets/dias/201807_dias.txt')
# # # 
# # # #unir meses del dataset
# dataset_base <- funion(dataset_1807, dataset_1808, all = TRUE)
# dataset_base <- funion(dataset_base, dataset_1809, all = TRUE)
#  dataset_base <- funion(dataset_base, dataset_1810, all = TRUE)
#  dataset_base <- funion(dataset_base, dataset_1811, all = TRUE)
# dataset_base <- funion(dataset_base, dataset_1812, all = TRUE)
#  dataset_base <- funion(dataset_base, dataset_1901, all = TRUE)
#  dataset_base <- funion(dataset_base, dataset_1902, all = TRUE)
# dataset_base <- funion(dataset_base, dataset_1903, all = TRUE)
#  dataset_base <- funion(dataset_base, dataset_1904, all = TRUE)
# # # 
# dataset_grande <- copy(dataset_base) # dejo copia para volver a cargar cuando haya errores
# 
# table(dataset_grande$foto_mes)

#----------------------SI CORREMOS EN LA NUBE DESCOMENTAR-------------------------#
#cargo los archivos de entrada
setwd( env$directory$datasets)
if( env$archivo_grande %like% "gz" )
{
  dataset_grande   <- fread(env$archivo_grande)
} else {
  dataset_grande   <- fread(cmd=paste("cat", env$archivo_grande))
}
# 
# #----------------------------------------------------------------------#
# gc()

#-------------------------PREPARACION DE DATOS--------------------------#
# Lista de todos los foto_mes
vmeses <-  abs(sort(-unique( dataset_grande$foto_mes )))

# Tabla codificando cada foto_mes con un n?mero
tb_meses <-  as.data.table(  cbind( seq(length(vmeses)), vmeses ) )

# Renombro las columnas de la tabla
colnames( tb_meses ) <- c( "mes", "foto_mes" )

# Agrego la columna "mes" a dataset
dataset_grande[  tb_meses,  on="foto_mes",  mes:= i.mes ]

#dejo la clase en 0,1
dataset_grande[  , clase_ternaria := as.integer(clase_ternaria=="BAJA+2") ]
#agrego variable para el undersampling
set.seed(semilla_undersampling)
dataset_grande[ ,  sample :=  runif( nrow(dataset_grande) )]

#agrego las variables canarito
#datasest_filtrado_canaritos <- agregar_canaritos( dataset_filtrado, 70 )

# Selecci?n de los meses a procesar
meses_a_procesar  <-   tb_meses[  foto_mes>=mes_primero_ganancia  & foto_mes<=mes_ultimo_ganancia, foto_mes ]

# Aplico la funci?n a la lista de meses
lapply( meses_a_procesar, linea_muerte )









