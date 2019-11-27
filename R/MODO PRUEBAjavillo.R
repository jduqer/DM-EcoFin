rm(list=ls())
gc()

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
          directory$work     <-  "~/dm/work/"
          directory$datasets <-  "~/dm/datasets/dias/"
        },
        Linux   = { 
          directory$work     <-  "~/cloud/cloud1/work/"
          directory$datasets <-  "~/cloud/cloud1/datasets/"
        }
)
env$directory <- directory

env$experimento          <-  '1a0_07_conmediamovil_sincolumnaid'
env$archivo_grande       <-  "paquete_premium_exthistavg.txt.gz"
#-------------------------PARAMETROS--------------------------#

#informacion de meses
mes_primero          <-  201803
mes_ultimo           <-  201902
mes_ultimo_conclase  <-  201904
undersampling <- 1.0
#param del xgboost
semilla               <- 751007
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
prob_corte           <-   -ganancia_noacierto*(1/undersampling)/( ganancia_acierto - ganancia_noacierto*(1/undersampling) )


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


#-------------------------PREPARACION DE DATOS--------------------------#
#-----------SI CORREMOS LOCAL DESCOMENTAR---------------------------------#
#
#cargar datos
# dataset_1904   <- fread('E:/Personal/DM ECyF/UBA2019/datasets/dias/201904_dias.txt')
# #dataset_1903   <- fread('E:/Personal/DM ECyF/UBA2019/datasets/dias/201903_dias.txt')
# dataset_1902   <- fread('E:/Personal/DM ECyF/UBA2019/datasets/dias/201902_dias.txt')
# dataset_1901   <- fread('E:/Personal/DM ECyF/UBA2019/datasets/dias/201901_dias.txt')
# dataset_1812   <- fread('E:/Personal/DM ECyF/UBA2019/datasets/dias/201812_dias.txt')
# # dataset_1811   <- fread('/Users/lucaspecina/Desktop/Data/Economia y Finanzas/datasets/dias/201811_dias.txt')
# # dataset_1810   <- fread('/Users/lucaspecina/Desktop/Data/Economia y Finanzas/datasets/dias/201810_dias.txt')
# # dataset_1809   <- fread('/Users/lucaspecina/Desktop/Data/Economia y Finanzas/datasets/dias/201809_dias.txt')
# # dataset_1808   <- fread('/Users/lucaspecina/Desktop/Data/Economia y Finanzas/datasets/dias/201808_dias.txt')
# # dataset_1807   <- fread('/Users/lucaspecina/Desktop/Data/Economia y Finanzas/datasets/dias/201807_dias.txt')
# # 
# # #unir meses del dataset
# # dataset_base <- funion(dataset_1807, dataset_1808, all = TRUE)
# # dataset_base <- funion(dataset_base, dataset_1809, all = TRUE)
# # dataset_base <- funion(dataset_base, dataset_1810, all = TRUE)
# # dataset_base <- funion(dataset_base, dataset_1811, all = TRUE)
# #dataset_base <- funion(dataset_base, dataset_1812, all = TRUE)
# dataset_base <- funion(dataset_1812, dataset_1901, all = TRUE)
# dataset_base <- funion(dataset_base, dataset_1902, all = TRUE)
# #dataset_base <- funion(dataset_base, dataset_1903, all = TRUE)
# dataset_base <- funion(dataset_base, dataset_1904, all = TRUE)
# # 
# dataset_grande <- copy(dataset_base) # dejo copia para volver a cargar cuando haya errores


#----------------------SI CORREMOS EN LA NUBE DESCOMENTAR-------------------------#
#cargo los archivos de entrada
setwd( env$directory$datasets)
if( env$archivo_grande %like% "gz" )
{
  dataset_grande   <- fread(env$archivo_grande)
} else {
  dataset_grande   <- fread(cmd=paste("cat", env$archivo_grande))
}

dataset_grande <- dataset_grande[ foto_mes>=mes_primero  & foto_mes<=mes_ultimo_conclase, ]

#----------------------------------------------------------------------#
gc()

#-------------------------PREPARACION DE DATOS--------------------------#
#paso clase a (0,1)
dataset_grande[, ('clase_ternaria') := ifelse(clase_ternaria=='BAJA+2',1,0)] 
table(dataset_grande$clase_ternaria) # comparar si todo ok
#table(dataset_base$clase_ternaria) 

#divido train/test para los dos
train_x <- dataset_grande[foto_mes<=mes_ultimo, !c("numero_de_cliente","clase_ternaria"), with=FALSE]
train_y <- dataset_grande[foto_mes<=mes_ultimo, "clase_ternaria", with=FALSE]
test_x <- dataset_grande[foto_mes==mes_ultimo_conclase, !c("numero_de_cliente","clase_ternaria"), with=FALSE]
test_y <- dataset_grande[foto_mes==mes_ultimo_conclase, "clase_ternaria", with=FALSE]




#-------------------------MODELO--------------------------#

#armo matrix para armar el xgboost
dtrain <-   xgb.DMatrix(data = as.matrix(train_x), label = as.matrix(train_y), missing=NA)
dtest  <-   xgb.DMatrix(data  = as.matrix(test_x), label = as.matrix(test_y), missing=NA)

#ENTRENAMIENTO XGBOOST. Evaluar con uno (no pueden usarse los dos al mismo tiempo):
# eval_metric = 'auc'
# feval = fganancia_logistic_xgboost
modelo <- xgb.train( 
  data = dtrain,  
  missing = NA,
  objective="binary:logistic",
  base_score= mean( getinfo(dtrain, "label") ),
  nround= nround_max,
  tree_method = "hist",
  max_bin = max_bin,
  subsample = subsample, 
  colsample_bytree = pcolsample_bytree, 
  eta = peta,
  min_child_weight = pmin_child_weight, 
  max_depth = pmax_depth,
  alpha = palpha, lambda = plambda, gamma = pgamma )

#mejor ganancia
#modelo$best_score

#calcular ganancia a mano
pred <- predict(modelo, dtest) #segun el modelo, predice 201904
tb_prediccion <-  cbind(test_x$numero_de_cliente, pred) # agrego columna ID
tb_prediccion <-  cbind(tb_prediccion, test_y) # agrego columna CLASE
#ordeno por probabilidad descendente
setorder( tb_prediccion, -pred)
tb_prediccion <- tb_prediccion %>% filter(pred>0.025) 
tb_prediccion <- tb_prediccion %>% mutate(Ganancia= fganancia_simple(pred,clase_ternaria)) # calculo GANANCIA
#View(tb_prediccion)
ganancia <- sum(tb_prediccion$Ganancia)
colnames( tb_prediccion ) <-  c( "ID", "prob", "Clase", "Ganancia" ) 


#importancia de variables
feature_importance <- xgb.importance (feature_names = colnames(dtest),model = modelo)
#xgb.plot.importance (importance_matrix = feature_importance[1:20])


# Curva ROC
#result.roc <- roc(test_y$clase_ternaria, tb_prediccion$prob) 
#plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft") # grafico ROC
#auc(result.roc) # AUC

#-----------GRABAR LA SALIDA----------------------------#
#escribo los  titulos  del archivo salida
setwd(env$directory$work)
cat("experimento",
    "ganancia",
    "mes_primero",
    "mes_ultimo",
    "mes_ultimo_conclase",
    "undersampling",
    "semilla",
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
    file=paste0( "parametros_", env$experimento, "_ganancia.txt" ), 
    fill=FALSE, append=TRUE  
    
)
#Grabo todas los parametros

cat(
  env$experimento,
  ganancia,
  mes_primero,
  mes_ultimo,
  mes_ultimo_conclase,
  undersampling,
  semilla,
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
  file=paste0( "parametros_", env$experimento, "_ganancia.txt" ), 
  fill=FALSE, append=TRUE  
)         

#Genero la salida de la materia 
#Entrene sobre todos los datos, la probabilidad de corte es 0.025
fwrite(  tb_prediccion,
         file =paste0( "salida_", env$experimento, "_entregamateria.txt" ),
         col.names= FALSE,
         sep="\t" 
)         

