rm(list=ls())
gc()
# empezar corriendo las funciones que estan abajo de todo

library("data.table")
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
          y Finanzas/datasets/"
          directory$datasets <-  "/Users/lucaspecina/Desktop/Data/Economia 
          y Finanzas/pruebas-eyf/pruebaitinerante/"          
        },
        Linux   = { 
          directory$work     <-  "~/cloud/cloud1/work/"
          directory$datasets <-  "~/cloud/cloud1/datasets/"
        }
)
env$directory <- directory

env$experimento          <-  'Agregar_var_probs'
env$archivo_grande       <-  "paquete_premium_exthist.txt.gz"     # CAMBIAR ACA SEGUN SEAS LUCAS O JAVI



#cargo los archivos de entrada
setwd( env$directory$datasets)
if( env$archivo_grande %like% "gz" )
{
  dataset_grande   <- fread(env$archivo_grande)
} else {
  dataset_grande   <- fread(cmd=paste("cat", env$archivo_grande))
}


# XGBOOST FILTRADO

xgb_filt_201904 <- fread("XGB filtrado/ldm_prob_XGBoost01-Filtrado_mes_201904.txt") 
xgb_filt_201904$foto_mes <- 201904
xgb_filt_201903 <- fread("XGB filtrado/ldm_prob_XGBoost01-Filtrado_mes_201903.txt")
xgb_filt_201903$foto_mes <- 201903
xgb_filt_completo <- funion(xgb_filt_201904,xgb_filt_201903)
xgb_filt_201902 <- fread("XGB filtrado/ldm_prob_XGBoost01-Filtrado_mes_201902.txt")
xgb_filt_201902$foto_mes <- 201902
xgb_filt_completo <- funion(xgb_filt_completo,xgb_filt_201902)
xgb_filt_201901 <- fread("XGB filtrado/ldm_prob_XGBoost01-Filtrado_mes_201901.txt") 
xgb_filt_201901$foto_mes <- 201901
xgb_filt_completo <- funion(xgb_filt_completo,xgb_filt_201901)
xgb_filt_201812 <- fread("XGB filtrado/ldm_prob_XGBoost01-Filtrado_mes_201812.txt") 
xgb_filt_201812$foto_mes <- 201812
xgb_filt_completo <- funion(xgb_filt_completo,xgb_filt_201812)
xgb_filt_201811 <- fread("XGB filtrado/ldm_prob_XGBoost01-Filtrado_mes_201811.txt") 
xgb_filt_201811$foto_mes <- 201811
xgb_filt_completo <- funion(xgb_filt_completo,xgb_filt_201811)
xgb_filt_201810 <- fread("XGB filtrado/ldm_prob_XGBoost01-Filtrado_mes_201810.txt") 
xgb_filt_201810$foto_mes <- 201810
xgb_filt_completo <- funion(xgb_filt_completo,xgb_filt_201810)
xgb_filt_201809 <- fread("XGB filtrado/ldm_prob_XGBoost01-Filtrado_mes_201809.txt") 
xgb_filt_201809$foto_mes <- 201809
xgb_filt_completo <- funion(xgb_filt_completo,xgb_filt_201809)
xgb_filt_201808 <- fread("XGB filtrado/ldm_prob_XGBoost01-Filtrado_mes_201808.txt") 
xgb_filt_201808$foto_mes <- 201808
xgb_filt_completo <- funion(xgb_filt_completo,xgb_filt_201808)
xgb_filt_201807 <- fread("XGB filtrado/ldm_prob_XGBoost01-Filtrado_mes_201807.txt") 
xgb_filt_201807$foto_mes <- 201807
xgb_filt_completo <- funion(xgb_filt_completo,xgb_filt_201807)
xgb_filt_201806 <- fread("XGB filtrado/ldm_prob_XGBoost01-Filtrado_mes_201806.txt") 
xgb_filt_201806$foto_mes <- 201806
xgb_filt_completo <- funion(xgb_filt_completo,xgb_filt_201806)
xgb_filt_201805 <- fread("XGB filtrado/ldm_prob_XGBoost01-Filtrado_mes_201805.txt") 
xgb_filt_201805$foto_mes <- 201805
xgb_filt_completo <- funion(xgb_filt_completo,xgb_filt_201805)
xgb_filt_201804 <- fread("XGB filtrado/ldm_prob_XGBoost01-Filtrado_mes_201804.txt") 
xgb_filt_201804$foto_mes <- 201804
xgb_filt_completo <- funion(xgb_filt_completo,xgb_filt_201804)
xgb_filt_201803 <- fread("XGB filtrado/ldm_prob_XGBoost01-Filtrado_mes_201803.txt") 
xgb_filt_201803$foto_mes <- 201803
xgb_filt_completo <- funion(xgb_filt_completo,xgb_filt_201803)
xgb_filt_201802 <- fread("XGB filtrado/ldm_prob_XGBoost01-Filtrado_mes_201802.txt") 
xgb_filt_201802$foto_mes <- 201802
xgb_filt_completo <- funion(xgb_filt_completo,xgb_filt_201802)

# JOIN con dataset_completo: con dplyr
# dataset_con_prob <- dataset_completo %>% inner_join(xgb_filt_completo, by=c('numero_de_cliente','foto_mes')) %>% 
#   select(-clase01) %>% rename(model_xgb_filt=prob_positivo)

# JOIN con dataset_completo: con data.table
setkey(xgb_filt_completo, numero_de_cliente, foto_mes)
setkey(dataset_completo, numero_de_cliente, foto_mes)
dataset_con_prob <- dataset_completo[xgb_filt_completo,nomatch=0]
dataset_con_prob <- dataset_con_prob %>% select(-clase01) %>% rename(model_xgb_filt=prob_positivo)



#-------------------------PREPARACION DE DATOS--------------------------#

# LIGHTGBM COMPLETO
lgbm_completo_201904 <- fread("LGBM completo/ldm_prob_LGBM_201904.txt") 
lgbm_completo_201904$foto_mes <- 201904
lgbm_completo_201903 <- fread("LGBM completo/ldm_prob_LGBM_201903.txt")
lgbm_completo_201903$foto_mes <- 201903
lgbm_completo_completo <- funion(lgbm_completo_201904,lgbm_completo_201903)
lgbm_completo_201902 <- fread("LGBM completo/ldm_prob_LGBM_201902.txt")
lgbm_completo_201902$foto_mes <- 201902
lgbm_completo_completo <- funion(lgbm_completo_completo,lgbm_completo_201902)
lgbm_completo_201901 <- fread("LGBM completo/ldm_prob_LGBM_201901.txt") 
lgbm_completo_201901$foto_mes <- 201901
lgbm_completo_completo <- funion(lgbm_completo_completo,lgbm_completo_201901)
lgbm_completo_201812 <- fread("LGBM completo/ldm_prob_LGBM_201812.txt") 
lgbm_completo_201812$foto_mes <- 201812
lgbm_completo_completo <- funion(lgbm_completo_completo,lgbm_completo_201812)
lgbm_completo_201811 <- fread("LGBM completo/ldm_prob_LGBM_201811.txt") 
lgbm_completo_201811$foto_mes <- 201811
lgbm_completo_completo <- funion(lgbm_completo_completo,lgbm_completo_201811)
lgbm_completo_201810 <- fread("LGBM completo/ldm_prob_LGBM_201810.txt") 
lgbm_completo_201810$foto_mes <- 201810
lgbm_completo_completo <- funion(lgbm_completo_completo,lgbm_completo_201810)
lgbm_completo_201809 <- fread("LGBM completo/ldm_prob_LGBM_201809.txt") 
lgbm_completo_201809$foto_mes <- 201809
lgbm_completo_completo <- funion(lgbm_completo_completo,lgbm_completo_201809)
lgbm_completo_201808 <- fread("LGBM completo/ldm_prob_LGBM_201808.txt") 
lgbm_completo_201808$foto_mes <- 201808
lgbm_completo_completo <- funion(lgbm_completo_completo,lgbm_completo_201808)
lgbm_completo_201807 <- fread("LGBM completo/ldm_prob_LGBM_201807.txt") 
lgbm_completo_201807$foto_mes <- 201807
lgbm_completo_completo <- funion(lgbm_completo_completo,lgbm_completo_201807)
lgbm_completo_201806 <- fread("LGBM completo/ldm_prob_LGBM_201806.txt") 
lgbm_completo_201806$foto_mes <- 201806
lgbm_completo_completo <- funion(lgbm_completo_completo,lgbm_completo_201806)
lgbm_completo_201805 <- fread("LGBM completo/ldm_prob_LGBM_201805.txt") 
lgbm_completo_201805$foto_mes <- 201805
lgbm_completo_completo <- funion(lgbm_completo_completo,lgbm_completo_201805)
lgbm_completo_201804 <- fread("LGBM completo/ldm_prob_LGBM_201804.txt") 
lgbm_completo_201804$foto_mes <- 201804
lgbm_completo_completo <- funion(lgbm_completo_completo,lgbm_completo_201804)
lgbm_completo_201803 <- fread("LGBM completo/ldm_prob_LGBM_201803.txt") 
lgbm_completo_201803$foto_mes <- 201803
lgbm_completo_completo <- funion(lgbm_completo_completo,lgbm_completo_201803)
lgbm_completo_201802 <- fread("LGBM completo/ldm_prob_LGBM_201802.txt") 
lgbm_completo_201802$foto_mes <- 201802
lgbm_completo_completo <- funion(lgbm_completo_completo,lgbm_completo_201802)

# ahora hacer el join con dataset_completo: dplyr
# dataset_con_prob <- dataset_con_prob %>% inner_join(lgbm_completo_completo, by=c('numero_de_cliente','foto_mes')) %>% 
#   select(-clase01) %>% rename(model_lgbm_completo=prob_positivo)

# JOIN con dataset_completo: con data.table
setkey(lgbm_completo_completo,numero_de_cliente, foto_mes)
dataset_con_prob <- dataset_con_prob[lgbm_completo_completo,nomatch=0]
dataset_con_prob <- dataset_con_prob %>% select(-clase01) %>% rename(model_lgbm_completo=prob_positivo)

colnames(dataset_con_prob)

setwd( env$directory$datasets)
fwrite( dataset_con_prob, file="dataset_con_probs.txt", sep="\t" )
#comprimo el archivo con gzip
system(  paste("gzip -f", "dataset_con_probs.txt") )


