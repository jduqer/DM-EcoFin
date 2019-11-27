rm(list=ls())
gc()
# empezar corriendo las funciones que estan abajo de todo

library("data.table")
library("ROCR")
library("Matrix")
library("xgboost")
library("DiceKriging")
library('dplyr')
library( "lightgbm" )

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

env$experimento          <-  'LGBM-Completo'
env$archivo_grande       <-  "paquete_premium_exthistavg.txt.gz"
#-------------------------PARAMETROS--------------------------#

#informacion de meses
mes_primero_ganancia <-  201804
mes_ultimo_ganancia  <-  201904
undersampling        <- 1.0
pventana             <- 12

semilla_undersampling <- 11111
semilla_lgbm          <- 751007
########
pmax_bin               <-     31
pbagging_fraction      <-      1.0
pnum_iterations        <-   700
plearning_rate <- 0.112053823824251
plambda_l1 <- 11.2863825447698
plambda_l2 <- 50.2644261243473
pmin_gain_to_split <- 4.91687856934657
pmin_child_weight <- 44.4373865747519
pmax_depth <- 17
pfeature_fraction <- 0.977275677906442
#ganancia
ganancia_acierto     <-  19500 
ganancia_noacierto   <-   -500
prob_corte           <-   -ganancia_noacierto*(1/undersampling)/
  ( ganancia_acierto - ganancia_noacierto*(1/undersampling) )

#-------------------------FUNCIONES--------------------------#

#-----------------------------------------------------
#funcion ganancia simple (devuelve un valor para cada fila)
fganancia_simple <- function(probs, clases){
  gan <- (probs > prob_corte  ) * ifelse( clases== 1, ganancia_acierto, ganancia_noacierto )
  return( gan ) }
#-------------------------------------------------------------------------

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
  dgeneracion  <-   lgb.Dataset( data  = data.matrix( dataset_grande[(sample < undersampling | clase_ternaria==1) & foto_mes>=primero & foto_mes<=ultimo , !c("mes","sample","numero_de_cliente","clase_ternaria"), with=FALSE]),
                                 label = dataset_grande[(sample < undersampling | clase_ternaria==1) & foto_mes>=primero & foto_mes<=ultimo, clase_ternaria ],
                                 free_raw_data=FALSE, missing=NA
  )
  
  #llamo al XGBoost,  notar lo frugal de los hiperparametros
  set.seed( semilla_lgbm ) #mi querida random seed, para que las corridas sean reproducibles
  
  modelo = lgb.train(
    data = dgeneracion,
    objective = "binary",
    max_bin=pmax_bin, #max_bin
    bagging_fraction=pbagging_fraction, #subsample
    num_iterations=pnum_iterations, #nrounds
    feature_fraction=pfeature_fraction, #colsample_bytree
    learning_rate=plearning_rate, #eta
    min_child_weight=pmin_child_weight, #min_child_weight
    max_depth=pmax_depth, #max_depth
    lambda_l1=plambda_l1, #alpha
    lambda_l2=plambda_l2, #lambda
    min_gain_to_split=pmin_gain_to_split
    ) 
  
  
  
  #aplico a los datos de pred, que tienen la clase vacia
  daplicacion  <-   dataset_grande[ foto_mes==pred, !c("mes","sample","numero_de_cliente","clase_ternaria"), with=FALSE]

  
  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, as.matrix(daplicacion) )
  
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
      "semilla lgbm",
      "semilla undersampling",
      "ventana",
      "max_bin",
      "bagging_fraction",
      "num_iterations",
      "learning_rate",
      "lambda_l1",
      "lambda_l2",
      "min_gain_to_split",
      "min_child_weight",
      "max_depth",
      "feature_fraction",
      "\n", 
      sep="\t", 
      file=paste0( "parametros_", env$experimento,"_mes",pred, "_ganancia.txt" ), 
      fill=FALSE, append=TRUE  
      
  )
  #Grabo todas los parametros
  
  cat(
    env$experimento,
    ganancia_obtenida,
    pred,
    undersampling,
    semilla_lgbm,
    semilla_undersampling,
    pventana,
    pmax_bin,
    pbagging_fraction,
    pnum_iterations,
    plearning_rate,
    plambda_l1,
    plambda_l2,
    pmin_gain_to_split,
    pmin_child_weight,
    pmax_depth,
    pfeature_fraction,
    "\n", 
    sep="\t", 
    file=paste0( "parametros_", env$experimento,"_mes", pred, "_ganancia.txt" ), 
    fill=FALSE, append=TRUE  
  )         
  
  #grabo todas las probabilidad, simplemente para tenerlo
  fwrite( prediccion_final[ order( -prob_positivo) ],
          file= paste( "ldm_prob_", pred, ".txt", sep="" ),
          sep="\t",
          eol = "\r\n")
  
  #grabo la importancia de las variables
  #write.table(  xgb.importance( model = modelo ),
  #              file=paste( "ldm_importancia_", pred, ".txt", sep="" ),
  #              sep="\t",
  #              eol = "\r\n"
  #)
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

# Selecci?n de los meses a procesar
meses_a_procesar  <-   tb_meses[  foto_mes>=mes_primero_ganancia  & foto_mes<=mes_ultimo_ganancia, foto_mes ]

# Aplico la funci?n a la lista de meses
lapply( meses_a_procesar, linea_muerte )











