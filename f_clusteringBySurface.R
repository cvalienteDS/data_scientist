f_clusterBySurface <- function(trainYears = c(2010:2019),
                               numSurfClusters = 6,
                               distanceMethod = 'ward.D',
                               data = dataIN,
                               minReg = 100,
                               desarrollando = F, 
                               generar_clusters = F){

  suppressMessages(require(tidyr))
  suppressMessages(library(dplyr))
  suppressMessages(require(fpc))
  suppressMessages(require(factoextra))
  suppressMessages(require(openxlsx))
  suppressMessages(library(caret))
  
  
############### Resumen
## datosunion -> data2: Se incluye altitud
## data2 -> xxx
##############
tryCatch(
  source('C:/Users/Carlos/Google Drive/scripts/scripts_pfp/f_comprobaciones_torneos_y_traer_altitud.R'),
  error = function(e)
  {source('C:/Users/cvaliente/Google Drive/scripts/scripts_pfp/f_comprobaciones_torneos_y_traer_altitud.R')}
)
  aux= duplicated(data %>% dplyr::select(match_id, fecha))
  data <- data %>% dplyr::filter(!aux)
  data2 <- f_comprobaciones_torneos_y_traer_altitud(data)
  
if(generar_clusters == T){  
x <- data2 %>%
      dplyr::filter(year %in% trainYears & multiplicador.importancia.del.partido == 1) %>%
      dplyr::filter(!grepl("Davis",substring(Name.tournamet.independientemnte.de.si.es.Q.o.main.draw,1,5)) &
                  substring(tourney_id,6) != '0000' &
                  independat.surface != 'Carpet' &
                  substring(id_tourney_NO_year,1,1) != 'D') %>%
      # left_join(distinct(dplyr::select(torneos_tabla_maestra, tourney_id_indepe, tourney_id_and_draw, altitud)),
      #           c('id_tourney_NO_year' = 'tourney_id_and_draw')) %>%
      group_by(tourney_id_indepe, independat.surface) %>%
      dplyr::summarise(
                AceRate = sum(w_ace,na.rm=TRUE)/sum(w_svpt,na.rm=TRUE),
                aceNA = sum(is.na(w_ace)),
                n = dplyr::n(),
                diff_1st_2nd = ( sum(w_1stWon,na.rm=TRUE) - sum(w_2ndWon,na.rm=TRUE) ) / sum(w_svpt,na.rm=TRUE),
                  # mean(J1.1st...won - J1.2nd...won, na.rm = T),
                diffNA = sum(is.na(J1.1st...won)),
                Name.tournamet.indepe = first(Name.tournamet.independientemnte.de.si.es.Q.o.main.draw),
                altitud = first(altitud)
                )


x %>%
    dplyr::filter(n-aceNA > minReg) %>%
    dplyr::filter(n-diffNA > minReg)  -> xx


xxx <- dplyr::select(xx, c('tourney_id_indepe','Name.tournamet.indepe', 'AceRate', 'diff_1st_2nd', 'altitud', 'independat.surface'))

# ¿eliminar la altitud en los indoor? Según cree Adri de Mbit (que es físico) igual aplica en indoor que en outdoor
# xxx$altitud[xxx$independat.surface =='Indoors'] <- NA

# a <- subset(xxx, select=-c(independat.surface, id_tourney_NO_year, tourney_name_js_db))
# b <- a[complete.cases(a[,"altitud"]),]
# cor(b)
# hay bastante correlacion entre las variables 1 y 2

dataNum <- as.data.frame(xxx[,c("AceRate","diff_1st_2nd")])
dataCat <- as.data.frame(xxx[,c("tourney_id_indepe", "independat.surface")])

suppressMessages(require(clusterSim))
# antes de hacer data.Normalization hay que quitar NAs, le pongo la mediana
dataNumNoNa <- apply(dataNum,2,function(x) {
  if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x})
scaler <- data.Normalization(dataNumNoNa, "n4") # esta normalizacion es peligrosa de usar si hay algun elemento muy outlier, en ese caso habria que usar la n1.
# pero no me interesa porque la n1 te deja numeros mas grandes y se me estropea la relacion con altitud que va de 0 a 2.
data_norm <- as.data.frame(scaler)

data_norm <- cbind(data_norm, dataCat[,'independat.surface']) # pongo 2 veces surface para darle más peso
colnames(data_norm)[ncol(data_norm)] <- "independat.surface"
data_norm$independat.surface <- as.character(data_norm$independat.surface)

# hay que hacer dummy con las columnas de texto (surface)
#################################### Otra aproximación alternativa a dummies
## Le está dando más importancia de la que quiero a la superficie así que voy a hacer una variable numérica que codifique las diferentes superficies,
## en lugar del one hot encoding que estoy haciendo hasta ahora
########################################
dumy = F
if(dumy == F){
  dummydf <- data_norm
  dummydf$surface_char <- dummydf$independat.surface
  
  dummydf=within(dummydf,{
    surface_Segm=NA
    surface_Segm[surface_char == 'Clay']=0.25
    surface_Segm[surface_char == 'Hard']=0.55
    surface_Segm[surface_char == 'Indoors']=0.65
    surface_Segm[surface_char == 'Grass']=1.1
  }) 
  dummydf$surface_char <- NULL
  dummydf$independat.surface <- NULL
  # COMPROBADO QUE ASIGNA BIEN CON LAS SIGUIENTES CONSULTAS
  # dummydf %>% dplyr::select('surface', 'surface_char') %>% head(20)
  # dummydf %>% dplyr::select('surface', 'surface_char') %>% filter(surface_char == "Hard") %>% head(20)
}

if(dumy == T){
  suppressMessages(library(dummies))
  dummydf <- dummy.data.frame(data_norm, sep = "_", verbose = T, omit.constants = T)
  # dummydf$surface_Hard <- NULL
}


# Tras observar veo que tiene poco en cuenta la altitud asi que la voy a hacer en grupos
# Y lo hago despues de normalizar aposta con intención de que tenga más importancia.
# El resto de variables iran de 0 a 1, y altitud toma valores 0,1,2
dummydf$altitud_Segm<- as.integer(
  cut(xxx$altitud, breaks = c(-100,250,800,9999),
      labels = c('0','1','2'),
      include.lowest = TRUE)
)
dummydf$altitud_Segm <- dummydf$altitud_Segm -1
dummydf$altitud_Segm[is.na(dummydf$altitud_Segm)] <- 0

########################################################
## funcion visualizar resultados
########################################################
visualizar <- function(tourneysCluster, results = xx, exportTable = F, complete_info = F, plot = F){
  if(nrow(xx)!= length(tourneysCluster)){stop('results y el vector de clusters no tienen la misma longitud')}
  # tourneysCluster = cutree(model, numSurfClusters)
  
  if(plot){
  # windows()
  plot(dummydf, col = tourneysCluster)
  }
  # le ponemos los nombres al resultado para visualizar
  results$tourneysCluster <- tourneysCluster
  
  
  
  
  results_by_group <- results %>% group_by(tourneysCluster) %>% dplyr::summarise(n = dplyr::n(),
                                                                          AceRate=mean(AceRate),
                                                                          diff_1st_2nd= mean(diff_1st_2nd),
                                                                          altitud=mean(altitud, na.rm = T),
                                                                          numClay = sum(independat.surface == "Clay"),
                                                                          numGrass = sum(independat.surface == "Grass"),
                                                                          numIndoors = sum(independat.surface == "Indoors"),
                                                                          numHard = sum(independat.surface == "Hard")) %>%
    mutate(freq_clay = percent(numClay / n),
           freq_Grass = percent(numGrass / n),
           freq_Indoors = percent(numIndoors / n),
           freq_Hard = percent(numHard / n))
  
  if(complete_info == F){print(results_by_group)}
  
  results_list <- list(results_by_group)
  
  for (i in unique(tourneysCluster)) {
    # print(paste0( "Tournaments cluster:", i))
    # print(xxx$Name.tournamet.indepe[tourneysCluster == i])
    # cat('\n')
    # cat('\n')
    nombre <- paste0('resulstEachGroup',i) 
    info <- results[results$tourneysCluster == i, c( "n","AceRate", "diff_1st_2nd", "altitud", "independat.surface", "Name.tournamet.indepe")]
    info <- arrange(info, -diff_1st_2nd)
    nom <- assign(nombre, info)
    results_list[[i+1]] <- nom
  }
  
  mastersTV <- results %>%
    inner_join(distinct(dplyr::select(data2, tourney_id_indepe, level)), by = c('tourney_id_indepe' = 'tourney_id_indepe')) %>%
    filter(level == '1000m' | tourney_id_indepe == 605)
  
  OUT <- createWorkbook()
  of="Cluster_de_torneos_results.xlsx"
  
  if(complete_info == T | exportTable == T){
  for (j in seq_along(results_list)) {
    print(results_list[[j]], n = 200)
  }}
  if(exportTable){
    
    for (j in seq_along(results_list)) {
      
        mdf<-results_list[[j]]
        sname<-paste("Cluster_",j-1,sep="")
        addWorksheet(OUT, sname)
        writeData(OUT, sheet = sname, x = mdf)
      }
      
      addWorksheet(OUT, 'Masters_tennisTV')
      writeData(OUT, sheet = 'Masters_tennisTV', x = mastersTV)
      tryCatch(
        setwd("C:/Users/Carlos/Google Drive/mbit data science/TFM/docs"),
        error = function(e)
        {setwd("C:/Users/cvaliente/Google Drive/mbit data science/TFM/docs")}
      )
    
    saveWorkbook(OUT,of, overwrite = T)
  }
  
}
########################################################
## funcion percent
########################################################
percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
#################### hierarchical
distance <- dist(dummydf)
model <- hclust(distance, method = distanceMethod) #mcquitty, ave es el q mas me convence, despues ward.D y complete
# plot(model, labels = xxx$tourney_name_js_db, hang = 0.1, cex = 0.6)
# rect.hclust(model, numSurfClusters)
# tourneysClustered <- cbind(data_norm, tourneysCluster01 = cutree(model, numSurfClusters))
tourneysCluster01 = cutree(model, numSurfClusters)
visualizar(tourneysCluster01, exportTable = F, complete_info = F, plot = F)


####################  kmeans
km=kmeans(dummydf, numSurfClusters)
tourneysCluster02 <- km$cluster
visualizar(tourneysCluster02, exportTable = F, complete_info = F, plot = F)

# visualizar(tourneysCluster02)


# plotcluster(dummydf, tourneysCluster02)
# fviz_cluster(km, dummydf,
#              ellipse.type = "norm")

####################  k medoids
# pamk.result <- pamk(dummydf, krange = 2:30)
# layout(matrix(c(1,2), 1, 2))
# plot(pamk.result$pamobject)
# layout(matrix(1))
# tourneysCluster03 <- pamk.result$pamobject$clustering
# plot(dummydf, col = tourneysCluster03)

# ####################  expectation maximization
# suppressMessages(library(mclust))
# mc <- Mclust(dummydf, numSurfClusters)
# # plot(mc, dimens=c(3,2), what="classification")
# # plot(mc, dimens=c(3,2), what="density")
# # plot(mc, dimens=c(3,2), what="uncertainty")
# tourneysCluster04 <- mc$classification
# # visualizar(tourneysCluster04)
# 
# ####################dbscan
# suppressMessages(library(dbscan))
# # kNNdistplot(dummydf, k =  3)
# scan <- dbscan::dbscan(dummydf, minPts = 15, eps = 0.01) 
# # plot(dummydf, col=scan$cluster+1)
# # plotcluster(dummydf, scan$cluster)
# tourneysCluster05 <- scan$cluster
# # visualizar(tourneysCluster05)


##############################
tourneysClustered <- xxx
tourneysClustered$tourneysCluster <- tourneysCluster02

if(length(tourneysCluster01) == nrow(dummydf)){
dummydf$tourneysCluster <- tourneysCluster02}



# csv_con_cluster <- left_join(data2, 
#                              distinct(tourneysClustered[,c("tourney_id_indepe", 'tourneysCluster')]) , 
#                              by = c("tourney_id_indepe"= 'tourney_id_indepe'))
# if(nrow(data2) != nrow(csv_con_cluster)){stop("Hay algun tourney_id_indepe repetido o alguna mierda")}
# tourneysClustered[duplicated(tourneysClustered$tourney_id_indepe),]
# dif <- setdiff(dplyr::select(csv_con_cluster, -tourneysCluster), data2)
# if(nrow(dif)>0){warning("Warning en dif L471")}
# AHORA MISMOS ES IMPOSIBLE QUE NO DE ERROR PORQUE EL CSV GRANDE TIENE INCONSISTENCIAS PERO YA TENGO EL NUEVO, QUE SE LO METERÉ CUANDO ESTE DUPLICADO
# ESTÁ STUTTGART 2 VECES. otra vezzzz. HABRÁ QUE VER PORQUE

} # end generar clusters

# quiza hay que hacer predict en lugar de join, para ello hacemos un Test a imagen de dummydf, que es con lo que se hizo el modelo y partiendo desde data2,
# porque tiene todas las filas y a diferencia de data, tiene el tourney id indepe.
# ES IMPORTANTE QUE SE ENTRENE EL RF CON LAS VARIABLES EN EL MISMO ESTADO QUE SE VA A PREDECIR . train y csvTest3


csvTest <- data2 %>% arrange(year, semana , tourney_id_indepe, rondas_num, as.Date(fecha, "%m/%d/%Y"), as.Date(fecha.exacta, "%m/%d/%Y"))

csvTest$surface_char <- csvTest$independat.surface
csvTest=within(csvTest,{
  surface_Segm=NA
  surface_Segm[surface_char == 'Clay']=0.25
  surface_Segm[surface_char == 'Hard']=0.55
  surface_Segm[surface_char == 'Indoors']=0.65
  surface_Segm[surface_char == 'Grass']=1.1
})
csvTest$surface_char <- NULL

csvTest$altitud_Segm<- as.integer(
  cut(csvTest$altitud, breaks = c(-100,250,800,9999),
      labels = c('0','1','2'),
      include.lowest = TRUE)
)
csvTest$altitud_Segm <- csvTest$altitud_Segm -1
csvTest$altitud_Segm[is.na(csvTest$altitud_Segm)] <- as.integer(mean(csvTest$altitud_Segm, na.rm = T)) # Lo normal es darle cero, pero por si cambiase la escala en un momento dado, pues le doy la media redondeada hacia abajo

csvTest2 <- csvTest %>%
  dplyr::filter(multiplicador.importancia.del.partido == 1 | grepl("_predecir", match_id)) %>%
  dplyr::transmute(
    match_id = match_id,
    tourney_id_indepe = tourney_id_indepe,
    sumAce = ifelse(is.na(w_ace) | is.na(w_svpt) | w_svpt == 0 | w_svpt == "", NA, w_ace),
    sum_w_svpt = ifelse(is.na(w_ace) | is.na(w_svpt) | w_svpt == 0 | w_svpt == "", NA, w_svpt),
    sum_w_1stWon = ifelse(is.na(w_1stWon) | is.na(w_2ndWon) | w_svpt == 0 | w_svpt == "", NA, w_1stWon),
    sum_w_2ndWon = ifelse(is.na(w_1stWon) | is.na(w_2ndWon) | w_svpt == 0 | w_svpt == "", NA, w_2ndWon),
    # diff_1st_2nd = J1.1st...won - J1.2nd...won,
    altitud_Segm = altitud_Segm,
    surface_Segm = surface_Segm
  )

summary(csvTest2)
cumroll <- function(x) {
  x <- head(x, -1)
  # c(NA, ifelse(cumsum(!is.na(x))<40,NA, cumsum(replace_na(x,0)) / cumsum(!is.na(x))) )
  c(NA, ifelse(cumsum(!is.na(x))<40,NA, cumsum(replace_na(x,0)) ) )
  
} # ESTA FUNCION HAY QUE APLICARLA CON EL DATASET ORDENADO POR AÑO, SEMANA...
# HE PUESTO 30 EN EL MINIMO QUE ES COMO 15, PORQUE AL ESTAR DUPLICADO, CADA PARTIDO SON 2 FILAS

csvTest3 <- csvTest2 %>%
  group_by(tourney_id_indepe) %>% 
  mutate_at(vars(colnames(csvTest2)[-which(colnames(csvTest2) %in% c("tourney_id_indepe", 'match_id', 'altitud_Segm', 'surface_Segm'))]), cumroll)
if(desarrollando){colMeans(is.na(csvTest3))}

csvTest3 <- csvTest3 %>%
  dplyr::transmute(
    match_id = match_id,
    AceRate = sumAce / sum_w_svpt,
    diff_1st_2nd = ( sum_w_1stWon - sum_w_2ndWon ) / sum_w_svpt,
    altitud_Segm = altitud_Segm,
    surface_Segm = surface_Segm
  ) # no pongo tourney_id_indepe porque es grouping variable asi q no se puede modificar. Aunque no la pongas la mantiene


if(generar_clusters == T){
# desnormalizamos dummydf y lo guardamos en train antes de hacer random forest, porque para RF no es necesario normalizar, pero sí normlizamos antes para el cluster.
train <- dummydf
train$AceRate <- train$AceRate * attr(scaler, "normalized:scale")[1] + attr(scaler, "normalized:shift")[1]
train$diff_1st_2nd <- train$diff_1st_2nd * attr(scaler, "normalized:scale")[2] + attr(scaler, "normalized:shift")[2]

train$tourneysCluster <- as.character(train$tourneysCluster)

control <- trainControl("cv",10)
metric <- "Accuracy"
mtry <- sqrt(ncol(train))
tunegrid <- expand.grid(.mtry=mtry)

# Por decisión de negocio sustituimos los NA de altitud por ceros, pero al predecir. Al entrenar solo quiero datos "buenos"
train <- train %>% dplyr::filter(!is.na(altitud_Segm))
fit <- train(tourneysCluster~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
# fit <- train(tourneysCluster~.,data=train, method = "C5.0",
#              metric=metric,  trControl=control)
print(fit)

pred <- predict(fit, train)
confusionMatrix((table(pred, train$tourneysCluster)))
# salvamos el modelo para poder usarlo en produccion. Importante "generar" las features del modelo, porque altitud está segmentada y surface es un valor numerico
tryCatch(
  setwd("C:/Users/Carlos/Google Drive/mbit data science/TFM/models/matchratings_34_from_v84_190915_desde_inicio.csv"),
  error = function(e)
  {setwd("C:/Users/cvaliente/Google Drive/mbit data science/TFM/models/matchratings_34_from_v84_190915_desde_inicio.csv")}
)

saveRDS(fit, "RF_model_cluster_by_surface.rds")
}

if(generar_clusters == F){
  tryCatch(
    setwd("C:/Users/Carlos/Google Drive/mbit data science/TFM/models/matchratings_34_from_v84_190915_desde_inicio.csv"),
    error = function(e)
    {setwd("C:/Users/cvaliente/Google Drive/mbit data science/TFM/models/matchratings_34_from_v84_190915_desde_inicio.csv")}
  )
  
  fit <- readRDS("RF_model_cluster_by_surface.rds")
  }
# d <- preProcess(csvTest3, "medianImpute")
# test <- predict(d, csvTest3)
csvTest3[is.na(csvTest3$altitud_Segm) ,"altitud_Segm"] <- 0
# csvTest3$pred <- ifelse(is.na(rowMeans(csvTest3 %>% data.frame() %>% dplyr::select(-match_id, -tourney_id_indepe))),NA, predict(fit, csvTest3)) ESTO NO FUNCIONA BIEN, creo que porque ifelse no es vectorial
pred <- predict(fit, csvTest3)

# cl_predict(km, csvTest3[, c("AceRate", "diff_1st_2nd", "altitud_Segm", "surface_Segm")])

csvTest3[!is.na(rowMeans(csvTest3 %>% data.frame() %>% dplyr::select(-match_id, -tourney_id_indepe))), 'pred'] <- pred



# Meterlo en csv completo
csv_con_cluster <- left_join(data2, 
                             csvTest3 %>% dplyr::select(match_id, pred, tourney_id_indepe) , 
                             by = c('match_id' = 'match_id', 'tourney_id_indepe' = 'tourney_id_indepe')) %>%
                    dplyr::rename(pred_tourneysCluster = pred)
if(nrow(data2) != nrow(csv_con_cluster)){stop("Hay algun tourney_id_indepe repetido o alguna mierda L376. Esto siempre es porque hay match_id repetidos")}

return(csv_con_cluster)


# comprobar si va bien el predict
if(desarrollando){
# csv_con_cluster %>% dplyr::select(tourneysCluster,tourney_id_indepe) %>% inner_join(csvTest2, by = c('tourney_id_indepe' = 'tourney_id_indepe')) %>% head(60) %>% View() 
csv_con_cluster %>% dplyr::select(pred_tourneysCluster,tourney_id_indepe, match_id, fecha, Name.tournamet.independientemnte.de.si.es.Q.o.main.draw) %>% 
  # dplyr::filter(Name.tournamet.independientemnte.de.si.es.Q.o.main.draw == csv_con_cluster[12000,'Name.tournamet.independientemnte.de.si.es.Q.o.main.draw']) %>% 
  # dplyr::filter(grepl()  Name.tournamet.independientemnte.de.si.es.Q.o.main.draw == 'Roland Garros') %>% 
  dplyr::filter(grepl('Garros',  Name.tournamet.independientemnte.de.si.es.Q.o.main.draw, ignore.case = T)) %>% 
  inner_join(csvTest3, by = c('match_id' = 'match_id')) %>%
  dplyr::rename(Name = Name.tournamet.independientemnte.de.si.es.Q.o.main.draw) %>%
  dplyr::select(pred_tourneysCluster, Name, fecha, AceRate, diff_1st_2nd, altitud_Segm, surface_Segm) %>%
  # View()
  mutate(fecha = as.Date(fecha, format = "%m/%d/%Y")) %>%
  arrange(fecha) %>%
  ggplot(aes(x=fecha, y=pred_tourneysCluster)) +
  geom_line()


# table(csv_con_cluster$tourneysCluster05)
cat('Esto es para comparar las proporciones que hay en el clustering original (agrupado por torneos) con la prediccion (a nivel partido), y son bastante parecidas')
prop.table(table(csv_con_cluster$pred_tourneysCluster))
prop.table(table(dummydf$tourneysCluster))


  ggplot(dummydf, aes(x = xxx$diff_1st_2nd , y = xxx$AceRate ,color = as.factor(tourneysCluster))) + 
    geom_point() + 
    scale_color_brewer(palette="Dark2")
  
}
} # end function




# unique(csv_con_cluster$tourney_name[csv_con_cluster$tourneysCluster == 1])
# write.table(csv_con_cluster, file = "C:/Users/Carlos/Google Drive/alex_vicens_carlos_valiente/comparar CSVs/csv_33_63_190105_invertido_conCluster.csv",  dec=".",sep=';')


