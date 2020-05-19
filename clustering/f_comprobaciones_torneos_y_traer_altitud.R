f_comprobaciones_torneos_y_traer_altitud <- function(data){
  
  require(dplyr)
  require(fpc)
  require(factoextra)
  require(openxlsx)
  require(caret)
  require(tidyr)
  # para que cada vez que pasa por esta funcion no añada nuevas columnas de cluster, aqui le quito las que pueda tener
  desarrollando <- F
  data <- dplyr::select(data, -c(starts_with("tourneysCluster")))
  
  data$id_tourney_NO_year <- substring(data$tourney_id, 6)
  
  # ## Esto es para probar a ver como apañar lo de los torneos.
  # # Lo mejor es hacerlo por id, pero antes hay que ponerle el mismo id a los main y las qualys. 
  # # para ello haré un join con name.tournament.indep... y traigo el id siempre del main
  # str_sort(unique(data$Name.tournamet.independientemnte.de.si.es.Q.o.main.draw))

  ########################################
  ## Construcción tabla maestra de torneos
  ########################################
  # torneos_unicos <- as.data.frame(unique(data[,"Name.tournamet.independientemnte.de.si.es.Q.o.main.draw"]))
  # colnames(torneos_unicos) <- 'Name.tournamet.independientemnte.de.si.es.Q.o.main.draw'
  # torneos_unicos <- arrange(left_join(torneos_unicos,
  #           torneos[,c('tourney_name_cv_db','tourney_name_js_db', 'tourney_id')],
  #           by = c("Name.tournamet.independientemnte.de.si.es.Q.o.main.draw" = "tourney_name_js_db")
  # ), Name.tournamet.independientemnte.de.si.es.Q.o.main.draw)
  # # he editado manualmente los ids que faltaban así que lo que tengo que hacer es leer el csv
  
  # torneos_unicos <- left_join(torneos_unicos, 
  #                               data[, c('id_tourney_NO_year', 'Name.tournamet.independientemnte.de.si.es.Q.o.main.draw', 'tourney_name')],
  #                               by = 'Name.tournamet.independientemnte.de.si.es.Q.o.main.draw')
  # torneos_unicos <- distinct(torneos_unicos)
  # torneos_tabla_maestra <- torneos_unicos %>% dplyr::rename(tourney_id_and_draw = id_tourney_NO_year,
  #                                               tourney_id_indepe = tourney_id)
  # 
  # setwd("C:/Users/Carlos/Google Drive/tenis_pfp/pfp/tablas")
  # write.table(torneos_tabla_maestra,"torneos_tabla_maestra.csv", row.names=F, na="", sep = ";")
  
  torneos_tabla_maestra <- read.csv( paste0(getwd(),"/torneos_tabla_maestra.csv"), 
              header=T , 
              dec=".",
              sep=';', 
              encoding = "UTF-8",
              stringsAsFactors = F,
              na.strings=c("#N/D", "#VALOR","#¡VALOR!", "#VALOR!", "#DIV/0!", "#NOMBRE?", "#REF", "Err:522"))
    
  torneos_tabla_maestra$tourney_id_indepe <- as.character(torneos_tabla_maestra$tourney_id_indepe)
  
  ################ COMPROBACIONES #################### OBLIGATORIO EJECUTAR PORQUE CREA data2
    
    ##############################################################################
    ## comprobamos consistencia de torneos en torneos tabla maestra
    ##############################################################################
    a <- distinct(dplyr::select(torneos_tabla_maestra, tourney_id_indepe, tourney_id_and_draw, altitud))
    b <- distinct(dplyr::select(torneos_tabla_maestra, tourney_id_indepe, tourney_id_and_draw))
    # g <- distinct(dplyr::select(torneos_tabla_maestra, tourney_id_and_draw))
    # which(duplicated(g))
    d <- a[duplicated(a$tourney_id_and_draw),'tourney_id_and_draw'] # este es el torneo o torneos con un mismo id pero distinta altitud
    if(length(d)>0){
      f <- paste0(d, collapse = ', ')
      warning("Estos torneos están repetidos o son incoherentes, y su tourney_id_and_draw es: \n", d)
    }
    
    # Para encontrar torneos repetidos o con incoherencias en la tabla maestra
    pru <- distinct(dplyr::select(torneos_tabla_maestra, tourney_id_indepe, tourney_id_and_draw))
    e <- pru[duplicated(pru$tourney_id_and_draw),]
    ifelse(nrow(e)>0,{
           print('Los siguientes torneos muestra incoherencias en su integridad referencial:')
           print(tibble(e))
           warning("Hay alguno torneo repetido o algo")
           
    }
           ,print('Integridad referencial correcta en todos los torneos de la tabla maestra')
           )
   
    
    
    ##############################################################################
    ## comprobamos consistencia de data (csv grande)
    ##############################################################################
    # intentando traerme al csv grande el id de torneo que no discierne segun main draw/qualy desde la maestra de torneos
    data$tourney_id_indepe <- NULL
    data$altitud <- NULL
    data2 <- left_join(data,
                       distinct(dplyr::select(torneos_tabla_maestra, tourney_id_indepe, tourney_id_and_draw, altitud)),
                       by = c('id_tourney_NO_year' = 'tourney_id_and_draw'))
    if(nrow(data) != nrow(data2)){stop("Al hacer join desde la tabla de partidos a la de torneos se ha traido de más")}
    if(desarrollando){data2 %>% dplyr::select(tourney_id_indepe, Name.tournamet.independientemnte.de.si.es.Q.o.main.draw) %>% 
        dplyr::filter(is.na(tourney_id_indepe) & !grepl("^Davis*", Name.tournamet.independientemnte.de.si.es.Q.o.main.draw))%>%
        dplyr::distinct() %>% View()}
    if(desarrollando){ # buscar los que salen en la anterior consulta. Puede ser que no estén en maestra
      data %>%  dplyr::select(id_tourney_NO_year, Name.tournamet.independientemnte.de.si.es.Q.o.main.draw) %>% 
        dplyr::filter(Name.tournamet.independientemnte.de.si.es.Q.o.main.draw == "Nur-Sultan2 CH") %>%
        View()
    }
      
    data2 %>% dplyr::select(tourney_id_indepe, Name.tournamet.independientemnte.de.si.es.Q.o.main.draw) %>% 
      dplyr::filter(is.na(tourney_id_indepe) & !grepl("^Davis*", Name.tournamet.independientemnte.de.si.es.Q.o.main.draw)) %>% nrow() -> num
    if(num >0){warning('Hay ',num ,' partidos sin tourney_id_indepe')}
    
    # Puede que ya haya una columna altitud, asi que al hacer join nos quedariamos con dos. Si son iguales eliminamos una
    if(dplyr::select(data2, starts_with("altitud")) %>% ncol() > 1){data2 %>% mutate(altitud = altitud.x) %>% dplyr::select(-altitud.x, -altitud.y) -> data2}
    
    # Para encontrar torneos repetidos o con incoherencias en el csv grande
    pru <- data2 %>%
      dplyr::filter(!grepl("Davis",substring(Name.tournamet.independientemnte.de.si.es.Q.o.main.draw,1,5)) &
                      substring(tourney_id,6) != '0000' &
                      independat.surface != 'Carpet' &
                      substring(id_tourney_NO_year,1,1) != 'D') %>%
      dplyr::select(tourney_id_indepe, id_tourney_NO_year, altitud, independat.surface, surface) %>%
      distinct()
    
    data2 %>% dplyr::filter(id_tourney_NO_year %in% pru[duplicated(pru$id_tourney_NO_year),"id_tourney_NO_year"]) %>%
            dplyr::select(Name.tournamet.independientemnte.de.si.es.Q.o.main.draw, year,tourney_id_indepe, id_tourney_NO_year, altitud, independat.surface, surface) %>%
            dplyr::rename(name=Name.tournamet.independientemnte.de.si.es.Q.o.main.draw) %>%
            arrange(name) %>%
            distinct() -> e
    
    ifelse(nrow(e)>0,{
      print('Los siguientes torneos muestra incoherencias en su integridad referencial:')
      print(tibble(e))
      warning("Hay alguno torneo repetido o algo")
      
    }
    ,print('Integridad referencial correcta en todos los torneos de la tabla de hechos (partidos)')
    )

    
    pruebaDuplicados <- pru[duplicated(pru$id_tourney_NO_year),"id_tourney_NO_year"]
    if(length(pruebaDuplicados)!= 0){stop("Hay alguno torneo repetido o algo")}
    
    ##### problemas astana
    # data2 %>% filter(tourney_id_indepe %in% c(3618,7071,8285)) %>%
    #   select(Name.tournamet.independientemnte.de.si.es.Q.o.main.draw, year,tourney_id_indepe, id_tourney_NO_year, altitud, independat.surface, surface) %>%
    #   dplyr::rename(name=Name.tournamet.independientemnte.de.si.es.Q.o.main.draw) %>%
    #   arrange(name) %>%
    #   distinct()
    
    # #### problemas Stuttgart, intuyo que en csv grande hay un id 321 con distintas surface
    # data2[which(data2$tourney_id_indepe == "981"), "surface"]
    # data2[which(data2$tourney_id_indepe == "321"), "surface"]
    
    #################### prueba consistencia con id_tourney_NO_year
    pru <- data2 %>%
      dplyr::filter(!grepl("Davis",substring(Name.tournamet.independientemnte.de.si.es.Q.o.main.draw,1,5)) &
                      substring(tourney_id,6) != '0000' &
                      independat.surface != 'Carpet' &
                      substring(id_tourney_NO_year,1,1) != 'D') %>%
      dplyr::select(tourney_id_indepe, id_tourney_NO_year, altitud, independat.surface, surface) %>%
      distinct()
    
    pruebaDuplicados <-pru %>% filter(id_tourney_NO_year %in% pru[duplicated(pru$id_tourney_NO_year),"id_tourney_NO_year"]) %>%
      dplyr::select(tourney_id_indepe, id_tourney_NO_year, altitud, independat.surface, surface) %>%
      arrange(tourney_id_indepe) %>%
      distinct()
    if(nrow(pruebaDuplicados)!= 0){stop("Hay algun tourney_id_indepe repetido o algo")}
    
    #################### prueba consistencia con tourney_id_indepe
    pru <- data2 %>%
      dplyr::filter(!grepl("Davis",substring(Name.tournamet.independientemnte.de.si.es.Q.o.main.draw,1,5)) &
                      substring(tourney_id,6) != '0000' &
                      independat.surface != 'Carpet' &
                      substring(id_tourney_NO_year,1,1) != 'D') %>%
      dplyr::select(tourney_id_indepe, altitud, independat.surface, surface) %>%
      distinct()
    
    pruebaDuplicados <- pru %>% filter(tourney_id_indepe %in% pru[duplicated(pru$tourney_id_indepe),"tourney_id_indepe"]) %>%
      dplyr::select(tourney_id_indepe, altitud, independat.surface, surface) %>%
      arrange(tourney_id_indepe) %>%
      distinct()
    if(nrow(pruebaDuplicados)!= 0){stop("Hay algun tourney_id_indepe repetido o algo")}
    
    ######################## Probamos que todos los torneos de datos esten en torneos
    pru <- data %>%
      dplyr::filter(!grepl("Davis",substring(Name.tournamet.independientemnte.de.si.es.Q.o.main.draw,1,5)) &
                      substring(tourney_id,6) != '0000' &
                      independat.surface != 'Carpet' &
                      substring(id_tourney_NO_year,1,1) != 'D') %>%
      dplyr::select(id_tourney_NO_year) %>%
      distinct() %>%
      pull(id_tourney_NO_year)
    
    pru2 <- torneos_tabla_maestra %>% 
      dplyr::filter(!grepl("Davis",substring(Name.tournamet.independientemnte.de.si.es.Q.o.main.draw,1,5)) &
                      substring(tourney_id_and_draw,1,1) != 'D') %>%
      dplyr::select(tourney_id_and_draw) %>%
      distinct() %>%
      pull(tourney_id_and_draw)
    
    if(!desarrollando){
      if(length(setdiff(pru, pru2))!= 0){stop("Hay algun id_tourney_NO_year en datos que no está en torneos")}
      # if(length(setdiff(pru2, pru))!=0 ){stop("Hay algun tourney_id_and_draw en torneos que no está en datos")}
      ## Pongo 2 porque en la tabla torneos hay 2 nuevos "Zhuzhai" y hasta que estén en el csv, darían error.
    }
    

  return(data2)
  ###################################################
}