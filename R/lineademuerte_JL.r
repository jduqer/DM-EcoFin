#Este programa genera la salida final
#La salida queda en  /work/  lineademuerte_entregar_UBA.txt  , lineademuerte_probabilidades_UBA.txt  y lineademuerte_importancia_UBA.txt
#Este programa necesita para correr 8vCPU y 128GB de RAM
#para correr se merece una digna maquina Inmortal
#Correrlo desde RStudio

#Atencion, se trabaja con el archivo  _hist 
#No se crea ninguna variable en el mismo mes
#No se imputan nulos, no se quitan outliers
#No se crean variables que sean predicciones de modelos
#No se hace undersampling, se trabaja con todos los datos (y por eso necesita 128GB de RAM)
#No se optimiza la probabilidad de corte, se deja en la original de 0.025
#No se trabaja con clase_binaria2  positivos = { BAJA+1, BAJA+2 }
#No se ajusta por inflacion
#No se traen variables externas como el valor mensual del dolar
#No se hace stacking de modelos al final

#limpio la memoria
rm(list=ls())
gc()

library( "data.table" )
library( "xgboost" )

#aplico el modelo a junio, que NO tiene clase
#la decision de entrenar en 10 meses se obtuvo de los testeos hechos sobre junio del 2018
#debe tenerse en cuenta que para 201906 la clase esta VACIA
#por lo que NO se puede calcular la ganancia
#esta vez, realmente estamos prediciendo el futuro
#es un salto al vacio

#cargo los datasets
setwd( "~/cloud/cloud1/datasets/")
dataset   <-   fread( "paquete_premium_exthist.txt.gz" )

#dejo la clase en 0,1
dataset[  , clase_ternaria := as.integer(clase_ternaria=="BAJA+2") ]

#entreno en 10 meses,  periodo  [ 201807, 201904 ]
dgeneracion  <-   xgb.DMatrix( data  = data.matrix( dataset[ foto_mes>=201805 & foto_mes<=201902 , !c("numero_de_cliente","clase_ternaria"), with=FALSE]),
                               label = dataset[ foto_mes>=201805 & foto_mes<=201902, clase_ternaria ]
                             )

#llamo al XGBoost,  notar lo frugal de los hiperparametros
set.seed( 399989 ) #mi querida random seed, para que las corridas sean reproducibles

modelo = xgb.train( 
                    data= dgeneracion,
                    objective= "binary:logistic",
                    tree_method= "hist",
                    max_bin= 31,
                    base_score= mean( getinfo(dgeneracion, "label") ),
                    eta= 0.04,
                    nrounds= 300, 
                    colsample_bytree= 0.6
                  )

#aplico a los datos de 201906, que tienen la clase vacia
daplicacion  <-   xgb.DMatrix( data  = data.matrix( dataset[ foto_mes==201904, !c("numero_de_cliente","clase_ternaria"), with=FALSE]),
                               label = dataset[ foto_mes==201904, clase_ternaria ]
                             )
#aplico el modelo a datos nuevos
aplicacion_prediccion  <- predict(  modelo, daplicacion )

#uno las columnas de numero_de_cliente y la probabilidad recien calculada
prediccion_final  <-  cbind(  dataset[ foto_mes==201904, c("numero_de_cliente","clase_ternaria") ], aplicacion_prediccion )

#le doy nombre a las columnas
colnames( prediccion_final )  <-  c( "numero_de_cliente", "clase01", "prob_positivo" )

#para calcular la ganancia, cuando se corre para meses del pasado
sum(  prediccion_final[ prob_positivo>0.025,   ifelse( clase01 == 1  , 19500, -500) ] )

#Genero las TRES salidas
#grabo todas las probabilidad, simplemente para tenerlo
setwd(  "~/cloud/cloud1/work/")
fwrite( prediccion_final[ order( -prob_positivo) ], 
        file="lineademuerte_probabilidades_JL_nueva.txt", 
        sep="\t", 
        eol = "\r\n")

#Ahora grabo la salida que debo entregar en la materia, que son solamente los ids
#me quedo solamente con los numero_de_cliente donde probabilidad > 0.025
fwrite( as.data.table( prediccion_final[ prob_positivo > 0.025  , "numero_de_cliente" ] ), 
        file="lineademuerte_entregar_JL_nueva.txt", 
        col.names=FALSE, 
        sep="\t", 
        eol = "\r\n")

#grabo la importancia de las variables
write.table(  xgb.importance( model = modelo )
              , file= "lineademuerte_importancia_JL_nueva.txt"
              , sep="\t"
              , eol = "\r\n"
           )
