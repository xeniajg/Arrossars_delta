###############################################################################################################
###############################################################################################################
############################ DANYS CAUSATS PER LA POLLA BLAVA AL DELTA DE L'EBRE ##############################
################################################### 2023 ######################################################
###############################################################################################################
### CARREGUEM PAQUETS, ESTABLIM DIRECTORI I L'ORDENEM. -------------------------------

paquets<-c('terra','rgdal','dplyr','sp','RStoolbox','spatialEco','reticulate','rgee','stringr', 'mapview')
lapply(paquets, require, character.only = TRUE)
rm(paquets)

directori<-"C:/Users/david.munoz/OneDrive - ctfc.cat/ARROSSARS/2023/SCRIPTS/ARROSSARS_DELTA"
setwd(directori) #Posem aquí la carpeta Arrossars genèrica, i creem les carpetes i subcarpetes: 
dir.create("../../MEMORIA")
dir.create("../../MODEL")
dir.create("../../SENTINEL")
dir.create("../../SENTINEL/imatges_crues", recursive = TRUE)
dir.create("../../SENTINEL/NDVI", recursive = TRUE)
dir.create("../../SENTINEL/PCA_R", recursive = TRUE)
dir.create("../../SIGPAC")
dir.create("../../SIGPAC/mosaic_R", recursive = TRUE)
dir.create("../../SIGPAC/parceles_amb_danys", recursive = TRUE)
dir.create("../../trameses")

### RETALL MONTSIA ----------------------------------------------------------------

mont<-vect("C:/Users/david.munoz/OneDrive - ctfc.cat/ARROSSARS/2022/SIGPAC/mosaic_R/TA_MONTSIA.shp") #Obrim els SIGPAC directament descarregats. És lent. 
#xmin, xmax,ymin,ymax
x_coord<-c(295363,295367,320614,320614,311013)
y_coord<-c(4497774,4510180,4511053,4503551,4498274)
xym <- cbind(x_coord, y_coord)

ambit_montsia<-vect(xym, "polygons")
crs(ambit_montsia)<-"EPSG:25831"


retall_mont<-crop(mont, ambit_montsia)
retall_mont <- terra::subset(retall_mont, retall_mont$US == "TA")
### RETALL BAIX EBRE -------------------------------------------------------------

baix<-vect("C:/Users/david.munoz/OneDrive - ctfc.cat/ARROSSARS/2022/SIGPAC/mosaic_R/TA_BAIX_EBRE.shp") #Obrim els SIGPAC directament descarregats. És lent. 
x_coord<-c(306355,317456,318621,295412,301375)
y_coord<-c(4520267,4516103,4507170,4507170,4515210)
xym <- cbind(x_coord, y_coord)

ambit_baix<-vect(xym, "polygons")
crs(ambit_baix)<-"EPSG:25831"


retall_baix<-crop(baix, ambit_baix)
retall_baix <- terra::subset(retall_baix, retall_baix$US == "TA")

#Mosaiquem i guardem. 
tot<-rbind(retall_baix,retall_mont)
writeVector(tot, "../../SIGPAC/mosaic_r/mosaicat.shp")

### AJUNTEM PARCELES AMB PROBLEMES AMB SIGPAC ------------------------------------------------
#Obrim el SHP i els problemes
SIGPAC<-vect("../../SIGPAC/mosaic_R/mosaicat.shp")  #Obrim el SHP amb tot
SIGPAC$MUNICIPI = toupper(SIGPAC$MUNICIPI)
SIGPAC$MUNICIPI[SIGPAC$MUNICIPI == "SANT CARLES DE LA RAPITA"] <- "LA RAPITA"
SIGPAC$MUNICIPI[SIGPAC$MUNICIPI == "LA RÀPITA"] <- "LA RAPITA"#De vegades es la Ràpita i de vegades Sant Carles de la Ràpita...
#Li donem un valor únic, tant aquí com a les parcel·les que han presentat problemes.
SIGPAC$COD_PAR<-1:nrow(SIGPAC)                                  #Generem una fila dins del SHP amb el COD_PAR i la omplim.
SIGPAC$COD_PAR<- with(SIGPAC, paste0(SIGPAC$POL,"_", SIGPAC$PAR,"_", SIGPAC$MUNICIPI))    

trameses<-read.csv("trameses/trameses_juntes.csv", sep = ";", check.names = F)
trameses$TM = toupper(trameses$TM) #tot en majuscula
trameses[trameses == "SANT CARLES DE LA RAPITA"] <- "LA RAPITA"  
trameses[trameses == "LA RÀPITA"] <- "LA RAPITA"  
trameses$COD_PAR<-1:nrow(trameses)   #Generem una fila dins del SHP amb el COD_PAR i la omplim
trameses$COD_PAR<- with(trameses, paste0(trameses$Poligon,"_", trameses$Parcela,"_", trameses$TM)) 
trameses$Ha <- gsub(",", ".", trameses$Ha) #replace some "," for "."

observacions<-as.data.frame(SIGPAC) #Per a consultar mes facilment que tenen de diferent, i aleshores modificar-ho al csv de problemes
diferent<-setdiff(trameses$COD_PAR, observacions$COD_PAR) #Genera un vector amb els valors que estan a problemes i no a SIGPAC danys. 

SIGPAC_DANYS <- terra::subset(SIGPAC, SIGPAC$COD_PAR %in% trameses$COD_PAR)#Dins el SIGPAC, seleccionem els poligons que tinguin danys declarats. 
writeVector(SIGPAC_DANYS, "../../SIGPAC/parceles_amb_danys/SIGPAC_danys.shp") 

### SENTINEL CODI INSTRUCCIONS GEE ----------------------------------------------
#Abans fèiem a GEE, ara des de R. El codi en JScript està EN UN ARXIU .CSV A LA CARPETA ARROSSARS/SENTINEL. 3 PASSOS:

### 1.- Hem de descarregar les imatges de GEE. Primer seleccionavem les fotos sense núvols a traves de https://earthexplorer.usgs.gov/, 
#per trobar fotos sense nuvols, ara ja no serveix. 

#Ara podem fer servir el HUB de Sentinel: https://www.sentinel-hub.com/explore/eobrowser/. Explorar i buscar dates sense núvols, es poden fer
#fer filtratges. 2023: https://apps.sentinel-hub.com/eo-browser/?zoom=11&lat=40.69647&lng=0.56854&themeId=DEFAULT-THEME&visualizationUrl=https%3A%2F%2Fservices.sentinel-hub.com%2Fogc%2Fwms%2Fbd86bcc0-f318-402b-a145-015f85b9427e&datasetId=S2L2A&fromTime=2023-04-30T00%3A00%3A00.000Z&toTime=2023-04-30T23%3A59%3A59.999Z&layerId=1_TRUE_COLOR&demSource3D=%22MAPZEN%22
#D'aquí traiem les dates que posarem unes línies més a sota. 

### 2.- Aconseguir la llista de les imatges. 

use_python("C:/Users/david.munoz/AppData/Local/Programs/Python/Python311") #Lliguem environment de python. 
ee_Initialize() #Obrim GEE, caldrà identificar-se la primera vegada

delta<- ee$Geometry$Rectangle(    #Seleccionem l'area d'estudi
  coords = c(0.565066015548572,40.604362720607654,0.883669531173572,40.80009008801863), proj = "EPSG:4326", geodesic = FALSE)

dataset <- ee$ImageCollection('COPERNICUS/S2_SR')$filterDate('2023-03-01', '2023-08-28')$filterBounds(delta)$
  select(c('B8','B4','B3'))$toBands() #Consultem una serie d'imatges, amb filtre de satèlit i producte, data, àrea i bandes concretes.
#$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 1)) Si li afegim això podem filtrar imatges sense núvols, però brutes, inclouen altres 
#passades, etc. De moment, millor mirar a Sentinel Hub, directament. 

ee_print(dataset)$bandNames #Podem mirar les propietats, de fet aquí ja veiem la llista amb les bandes que ens interessen. 
prova<-dataset$getInfo() #Creem una llista amb la info de la col·leccio. 
ve<-vector(length = length(prova$bands))

for (i in 1:length(prova$bands)) { #Posem la informació que ens interessa a un vector ve, que tindrà la llista sencera
  ve[[i]]<-prova$bands[[i]]$id
}

ve<-ve[!grepl('TBE', ve)] #I dic sencera perquè porta finestres que no ens interessa, aquí les eliminem
ve<-ve[!grepl('TCE', ve)]
ve<-ve[!grepl('TYL', ve)]

#A més a més no ens interessen totes les dates, sinó només aquelles que sabem que tenen una imatge sense núvols. Ho fem amb un match. 
#Al vector match li posem les dates en format anymesdia. Aquestes dates són les que venen de veure quins dies no hi havia núvols.

match<-(c('20230301','20230405','20230420','20230430','20230510','20230515','20230614','20230624','20230714','20230823'))

###############################26 DE MARÇ ALGUN NUVOLET. 14 i 24 JUNY una banda com més clara. Està, però, a la llista

llistafinal<-list(length = match) 
TBF<-vector()
TCF<-vector()
for (i in 1:length(match)) {  #
  final<-grep(paste0("^",match[[i]]), ve,value=TRUE)   #Que agafi només les imatges que tinguin les dates que toquen. 
  llistafinal[[i]]<-final                              #Ho posem en una llista
  TBF[[i]]<-llistafinal[[i]][[1]]                      #Fem dues llistes, agafem la banda 8 de TBF aquí i la banda 8 de la TCF a baix. 
  TCF[[i]]<-llistafinal[[i]][[4]]
}

vefinal<-c(TBF,TCF)                                         #Ajuntem les dues dallades (TBF i TCF)
vefinal<-gsub(pattern = "_B8", replacement = "", vefinal)   #I eliminem la part de la banda 8, per a tenir el nom net. 

vefinal_dir <- paste0("COPERNICUS/S2_SR/",vefinal) #Hem de fer que el vector contingui el directori de Copernicus per a descarregar després
vefinal_dir<-str_sort(vefinal_dir)                 #Ordenem tots dos vectors segons ordre alfabètic i de nombre, i ja tenim la llista!    
vefinal<-str_sort(vefinal)

### 3.- Descarregar les imatges a través de la llista:

llista<-list(24)
llista.r<-list(24)

#El bucle seguent descarrega les imatges. Important haver creat la carpeta al drive (container) i que no tingui ràsters amb el nom dels que
#volem crear, i que a la carpeta de destí de l'ordinador no hi hagi tampoc cap ràster amb el nom dels ràsters que estem guardant. 
#Això és lent, triga una hora aprox.

setwd("C:/Users/david.munoz/OneDrive - ctfc.cat/arrossars/2023/SENTINEL/imatges_crues") #Posem directori, així ja es creen les imatges on volem

for (i in 1:length(vefinal)) { 
  llista[[i]]<- ee$Image(vefinal_dir[[i]])$select(c('B8', 'B4', 'B3'))$clip(delta)
  llista.r[[i]] <- ee_as_raster(llista[[i]], via = "drive", container = "arrossars_2023", dsn=paste0(vefinal[[i]],".tif"), 
                                timePrefix = F,  scale = 10)
}

setwd(directori) #Posem el directori general

### PCA SENTINEL  ------------------------------------------------------
d.envs <- "../../SENTINEL/imatges_crues"
BuserRas.paths<- list.files(d.envs, pattern="*TBF.tif", full.names=TRUE)
CuserRas.paths<- list.files(d.envs, pattern="*TCF.tif", full.names=TRUE)

TBF<-vector("list", length = length(match))
TCF<-vector("list", length = length(match))
NDVITBF<-vector("list", length = length(match))
NDVITCF<-vector("list", length = length(match))
NDVI<-vector("list", length = length(match))

for (i in 1:length(TBF)) {                                #Entren les capes, amb brick perque son multibanda,
  TBF[[i]]<-rast(BuserRas.paths[i])            #i es calcula el NDVI per les dues dallades
  TCF[[i]]<-rast(CuserRas.paths[i])
  NDVITBF[[i]]<-(TBF[[i]][[1]]-TBF[[i]][[2]])/(TBF[[i]][[1]]+TBF[[i]][[2]])
  NDVITCF[[i]]<-(TCF[[i]][[1]]-TCF[[i]][[2]])/(TCF[[i]][[1]]+TCF[[i]][[2]])
  NDVI[[i]]<-terra::merge(NDVITBF[[i]], NDVITCF[[i]])
  writeRaster(NDVI[[i]], paste0("../../SENTINEL/NDVI/NDVI_",i,".tif"))
}





NDVI<-c(NDVI[[1]],NDVI[[2]],NDVI[[3]],NDVI[[4]], NDVI[[5]], NDVI[[6]], 
            NDVI[[7]], NDVI[[8]], NDVI[[9]], NDVI[[10]]) #posar tots els elements de NDVI.

PCA<-rasterPCA(NDVI, nComp=3)
#PCA<-prcomp(NDVI, nComp=3)

writeRaster(PCA$map$PC1, "../../SENTINEL/PCA_R/JUNTPC1.TIF", overwrite=T)
writeRaster(PCA$map$PC2, "../../SENTINEL/PCA_R/JUNTPC2.TIF", overwrite=T)
writeRaster(PCA$map$PC3, "../../SENTINEL/PCA_R/JUNTPC3.TIF", overwrite=T)

### MODEL I RETALL EN PARCELES AFECTADES  ------------------------------------------------------

PC1<-rast("../../SENTINEL/PCA_R/JUNTPC1.TIF")
PC2<-rast("../../SENTINEL/PCA_R/JUNTPC2.TIF")
PC3<-rast("../../SENTINEL/PCA_R/JUNTPC3.TIF")
PC<-c(PC1,PC2,PC3)

danys2022 <- (1/(1+exp(-(2.5760*PC[[1]] - 0.3788*PC[[2]] + 0.8904*PC[[3]] - 4.1929))))
writeRaster(danys2022, "../../model/danys_2022_tot.tif", overwrite=T)

################ Mascara del model en les parceles afectades
SIGPAC_DANYS<-vect("../../SIGPAC/parceles_amb_danys/SIGPAC_DANYS.shp")
SIGPAC_DANYS<-terra::project(SIGPAC_DANYS, crs(PC1))
SIGPAC_DANYS<- buffer(SIGPAC_DANYS,width = -7)  #Creem un buffer negatiu de 7 metres, aixi nomes ens #quedem amb els pixels que estiguin dins dels poligons.


model_retallat<-terra::mask(danys2022, SIGPAC_DANYS, touches=F)
writeRaster(model_retallat, "../../MODEL/danys_2022_retall.tif", overwrite=T)

### SELECCIO DE QUARTILS  ------------------------------------------------------
#SIGPAC_DANYS<-readOGR("SIGPAC/parceles_amb_danys/SIGPAC_DANYS.shp")
#SIGPAC_DANYS<-vect(SIGPAC_DANYS)
dfSIGPAC<-as.data.frame(SIGPAC_DANYS)

#model_retallat<-raster("model/danys_2022_retall.tif")
#model_retallat<-rast(model_retallat)

q25 <- function(x, p=0.25, na.rm = TRUE) { quantile(x, p, na.rm = na.rm) }
q10 <- function(x, p=0.10, na.rm = TRUE) { quantile(x, p, na.rm = na.rm) }
q05 <- function(x, p=0.05, na.rm = TRUE) { quantile(x, p, na.rm = na.rm) }

model_q25<-terra::extract(x = model_retallat, y = SIGPAC_DANYS, fun = "q25")
model_q10<-terra::extract(x = model_retallat, y = SIGPAC_DANYS, fun = "q10")
model_q05<-terra::extract(x = model_retallat, y = SIGPAC_DANYS, fun = "q05")





# Modified q25 function to work with terra::extract
q25 <- function(x, na.rm = TRUE) { quantile(x, probs = 0.25, na.rm = T)}
q10 <- function(x, na.rm = TRUE) { quantile(x, probs = 0.1, na.rm = T)}
q05 <- function(x, na.rm = TRUE) { quantile(x, probs = 0.05, na.rm = T)}

# Use the modified q25 function with terra::extract
model_q25 <- extract(x = model_retallat, y = SIGPAC_DANYS, fun = q25)
model_q10 <- extract(x = model_retallat, y = SIGPAC_DANYS, fun = q10)
model_q05 <- extract(x = model_retallat, y = SIGPAC_DANYS, fun = q05)





#Ho ajuntem tot en una taula i canviem noms. 
quartils<-inner_join(model_q05, model_q10, by="ID")
quartils<-inner_join(quartils, model_q25, by="ID")
quartils$ID<-dfSIGPAC$COD_PAR
names(quartils)<-c("COD_PAR","Q05","Q10","Q25")
COD_PAR<-quartils[,1]
write.csv(quartils, "../../model/quartils_poligons_parceles.csv", sep=";", dec = ",",row.names = TRUE,col.names = TRUE)

### AREA AFECTADA PER CADA QUARTIL EN CADA PARCELA ------------------------------------------------------
quartils<-read.csv("../../MODEL/quartils_poligons_parceles.csv")
qquartils<-quartils[,3:5]
model_retallat<-rast("../../model/danys_2022_retall.tif")
SIGPAC_DANYS<-vect("../../SIGPAC/parceles_amb_danys/SIGPAC_DANYS.shp")
crs(SIGPAC_DANYS)<-crs(model_retallat)

df <- data.frame(matrix(nrow = nrow(dfSIGPAC), ncol = 3))  #Generem la matriu on s'emmagatzemaran els resultats.

for (i in 1:nrow(df)){  #fem una mascara per cada poligon amb els resultats del model. 
  a <- assign(paste("pol", i, sep = ""), SIGPAC_DANYS[i,])
  mascara<-mask(model_retallat, a)
  mascara<-crop(mascara, a)
  c <- vector()
  
  for(r in 1:ncol(df)) { #Els valors superiors a cadascun dels llindars es reclassifiquen a 0. Els de sota, es reclassifiquen a 
                         #1 i es compten.
    b <- mascara
    b[b[] >	qquartils[i,r]  ] = NA 
    b[!is.na(b)]<-1
    fre<-freq(b, value=1)
    c <- c(c, fre)
  }
  df[i,] <- c #Es posa tot a un dataframe. df son els pixels
}

#Dividim entre 100 per tenir el num d'Ha i fem la suma de les columnes amb el mateix COD_PAR. 
tauladanys<-df/100
tauladanys <- cbind(tauladanys, new_col = SIGPAC_DANYS$COD_PAR)     
names(tauladanys)<-c("Q05", "Q10", "Q25","COD_PAR")
tauladanys<-aggregate(cbind(Q05,Q10,Q25) ~ COD_PAR, data = tauladanys, FUN = sum, na.rm = TRUE) #Sumem els polígons de cada parcela. 

Comprovacio<-setdiff(trameses$COD_PAR, tauladanys$COD_PAR) #Així mirem que no hi hagi cap parcela en que no s'hagi calculat el dany. 

write.csv(tauladanys, "model/tauladanys_parcela.csv")

### AJUNTEM DANYS PER EXPEDIENT  ------------------------------------------------------

tauladanys<-dplyr::inner_join(tauladanys, trameses, by="COD_PAR") #Ajuntem la taula inicial amb els resultats
tauladanys<-aggregate(cbind(Q05,Q10,Q25) ~ NUM_EXPEDIENT, data = tauladanys, FUN = sum, na.rm = TRUE) #Sumem les parceles de cada expedient. 

trameses$Ha<-as.numeric(trameses$Ha) #Convertim les hectàrees declarades de les trameses en numeric.
danys_declarats <- trameses %>%  #I sumem els danys declarats per parcel·les en expedients. 
  group_by(NUM_EXPEDIENT) %>% 
  summarise(danys_declarats = sum(Ha))

tauladanys<-dplyr::inner_join(tauladanys, danys_declarats, by="NUM_EXPEDIENT") #I ho ajuntem per expedient.

tauladanys$danys_sentinel <- apply(tauladanys[, -1], 1, function(x) { #Funció que tria quin quartil s'assembla més als danys declarats.
  abs_diff <- abs(x[4] - c(x[1], x[2], x[3]))
  return(x[which.min(abs_diff)])
})

tauladanys$guany_agricultors<-tauladanys$danys_sentinel-tauladanys$danys_declarats #Afegim columna de guany dels agricultors.
#tauladanys$ponderacio_guany_agricultors<-tauladanys$guany_agricultors/tauladanys$danys_declarats #I si vulguéssim, el guany relatiu.

#(tenint en compte ja els expedients que tenen més d'una parcela.)
write.csv(tauladanys, "model/tauladanys_expedient.csv")

