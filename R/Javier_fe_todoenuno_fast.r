#este programa necesita de una maquina virtual con :
# inmortal  ( NO preemptive )
# disco local SSD persistent disk (para velocidad)
# 512 GB  de espacio en el disco local de la maquina virtual (para velocidad, si como leyeron, velocidad)
# 8 vCPU
# 128 GB de memoria RAM
# una vez que corre, esta muy costosa maquina virtual debe eliminarse


#Feature Engineering  TODO EN UNO
#paso las fechas abolutas a dias
#creo las variables nuevas en cada mes
#creo TENDENCIA, MAX, MIN, AVERAGE   en funcion de los ultimos 6 meses historia para cada variable, son moviles

#por eficiencia, la parte critica esta escrita en lenguaje C  
#ya que R es insoportablemente lento
#alguna vez se le tenia que poner freno a la demencia R/Python



#source( "~/cloud/cloud1/R/FeatureEngineering/fe_todoenuno_fast.r" )


#limpio la memoria
rm( list=ls() )
gc()

t99     <-  Sys.time()


#vuelvo a instalar la libreria  data.table  ,  necesito la version posterior al 03-oct-2019  que no estaba disponible al inicio de la materia
#deberia recrearse la imagen para ser prolijos
cat("options(repos = c(\"https://cloud.r-project.org/\"))\n install.packages(\"data.table\") \n quit(save=\"no\") \n", file="~/instalo_nuevo.r")
cat( "sudo  Rscript --verbose  ~/instalo_nuevo.r \n",  file="~/instalo_nuevo.sh" )
system( "chmod 777 ~/instalo_nuevo.sh" )
system(  "~/instalo_nuevo.sh" )


library( "data.table" )
library( "Rcpp")



kcarpeta_datasetsOri     <-  "~/cloud/cloud1/datasetsOri/"
kcarpeta_datasets        <-  "~/cloud/cloud1/datasets/"
karchivo_entrada_zip     <-  "paquete_premium.zip" 

kcampos_separador        <-  "\t"
kcampo_id                <-  "numero_de_cliente"
kcampo_foto              <-  "foto_mes"
kclase_nomcampo          <-  "clase_ternaria"


kcampos_no_procesar  <- c( "numero_de_cliente", "foto_mes", "clase_ternaria" )
ventana_regresion    <- 6


#La salida
karchivo_salida_completo <-  "paquete_premium_exthist.txt.gz"
kextension               <-  "exthist"
karchivo_salida_prefijo  <-  paste( "./", kextension, "/",    sep="" )
karchivo_salida_sufijo   <-  paste( "_", kextension, ".txt",    sep="" )

#-------------------------------------------------

cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 4*n );
  

  for(int i = 0; i < n; i++) 
  {
   
    int  libre    = 0 ;
    int  xvalor   = 1 ;
 
    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;


       if( !R_IsNA( a ) ) 
       {
           y[ libre ]= a ;
       x[ libre ]= xvalor ;
          
       libre++ ;
       }
     
       xvalor++ ;
    }

 
    /* Si hay al menos dos valores */
    if( libre > 1 )
    {   
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ; 
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++) 
      { 
        xsum  += x[h] ; 
        ysum  += y[h] ; 
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;
 
        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ; 
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else  
    { 
      out[ i       ]  =  NA_REAL ; 
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }

  }


  return out;
}')

#------------------------------------------------------
#Esta funcion calcula la cantidad de dias entre la foto_mes y la fecha
#la foto_mes 201904 se interpreta como la fecha "20190501 00:00:00"

fdias_entre  = function( pfoto_mes, pfecha )
{
  
  foto_mes       <- as.POSIXlt( as.Date(  paste(pfoto_mes, "01", sep=""), format='%Y%m%d'  ) )
  foto_mes$mon   <- foto_mes$mon +1

  fecha         <-  as.Date(  as.character(pfecha), format='%Y%m%d'  )

  return( as.numeric( difftime(foto_mes, fecha, units = c("days")) ) )
}
#------------------------------------------------------

setwd( kcarpeta_datasetsOri )
dataset <- fread(  cmd=paste( "gunzip -cq", karchivo_entrada_zip), header=TRUE, sep=kcampos_separador ) 


#ordeno por  numero_de_cliente y foto_mes
setorder( dataset,  numero_de_cliente, foto_mes )


nrow( dataset )
ncol( dataset )



#----------
#paso los campos fecha a dias relativos

#paso los campos fecha a dias relativos

dataset[  , Master_Fvencimiento    := fdias_entre( get(kcampo_foto), Master_Fvencimiento )  ]
dataset[  , Master_Finiciomora     := fdias_entre( get(kcampo_foto), Master_Finiciomora )   ]
dataset[  , Master_fultimo_cierre  := fdias_entre( get(kcampo_foto), Master_fultimo_cierre )]
dataset[  , Master_fechaalta       := fdias_entre( get(kcampo_foto), Master_fechaalta )     ]
dataset[  , Visa_Fvencimiento      := fdias_entre( get(kcampo_foto), Visa_Fvencimiento )    ]
dataset[  , Visa_Finiciomora       := fdias_entre( get(kcampo_foto), Visa_Finiciomora )     ]
dataset[  , Visa_fultimo_cierre    := fdias_entre( get(kcampo_foto), Visa_fultimo_cierre )  ]
dataset[  , Visa_fechaalta         := fdias_entre( get(kcampo_foto), Visa_fechaalta )       ]



#----------

#se crean los nuevos campos para Master y Visa, teniendo en cuenta los NA's
#Aqui se deben agregar nuevos campois a voluntad

dataset[ , mv_cuenta_estado2       := pmax( Master_cuenta_estado,  Visa_cuenta_estado, na.rm = TRUE) ]
dataset[ , mv_marca_atraso         := pmax( Master_marca_atraso, Visa_marca_atraso, na.rm = TRUE) ]

dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
dataset[ , mv_tconsumos            := rowSums( cbind( Master_tconsumos,  Visa_tconsumos) , na.rm=TRUE ) ]
dataset[ , mv_tadelantosefectivo   := rowSums( cbind( Master_tadelantosefectivo,  Visa_tadelantosefectivo) , na.rm=TRUE ) ]
dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]



#----------

last <- nrow( dataset )
kcampo_id_idx  <-  match( kcampo_id, names(dataset) )
#----------


#creo el vector_desde que indica cada ventana
#de esta forma se acelera el procesamiento ya que lo hago una sola vez

vector_ids   <- dataset[[  kcampo_id_idx  ]]

vector_desde <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
vector_desde[ 1:ventana_regresion ]  <-  1

for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }
#----------



#agrego al dataset las TENDENCIAS
campos_a_procesar  <- setdiff( names(dataset) ,  kcampos_no_procesar  )  

for(  campo  in  campos_a_procesar )
{
   campo_idx     <-   match( campo,  names(dataset) )
   col_original  <-   dataset[[  campo_idx  ]]

   nueva_col     <- fhistC( col_original, vector_desde ) 

   #agrego las nuevas columnas al dataset
   dataset[ , paste( campo, "__tend", sep="" ):= nueva_col[ (0*last +1):(1*last) ]  ]
   dataset[ , paste( campo, "__min" , sep="" ):= nueva_col[ (1*last +1):(2*last) ]  ]
   dataset[ , paste( campo, "__max" , sep="" ):= nueva_col[ (2*last +1):(3*last) ]  ]
   #dataset[ , paste( campo, "__avg" , sep="" ):= nueva_col[ (3*last +1):(4*last) ]  ]
}


#----------

#dejo la clase como ultimo campo
nuevo_orden <-  c( setdiff( colnames( dataset ) , kclase_nomcampo ) , kclase_nomcampo )
setcolorder( dataset, nuevo_orden )
colnames(dataset)



#grabo el archivo completo
#al  agregarle el .gz  al final del nombre del archivo,  data.table sabe que debe comprimir el archivo
setwd( kcarpeta_datasets )
fwrite( dataset, 
        file= paste0(karchivo_salida_completo), 
        sep=kcampos_separador, 
        na="", 
        row.names=FALSE )


t100     <-  Sys.time()

tcorrida <-  as.numeric( t100 - t99, units = "secs")
print( tcorrida)


#limpio la memoria

rm( list=ls() )
gc()



quit( save="no" )


