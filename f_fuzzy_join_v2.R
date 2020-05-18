library(dplyr)
require(fuzzyjoin)
require(gtools)

############################
## PARA MEJORAR ESTE SCRIPT HABRIA QUE EXIGIR QUE SE PAREZCAN ADEMAS EL LARGO DE LAS PALABRAS PORQUE POR EJEMPLO ENTRE VERDAD Y VERDASCO HAY UNA SUSTITUCION ASI QUE LO DA POR VALIDO
## otra opcion es añadir la nacionalidad para hacer join como un criterio mas a exigir
############################
desarrollando = F

###########################################################
## otro codigo mejor pero no encuentra nada parecido, solo cambiar orden. Muy posiblemente es lo q se necesita
#############################################################
nombres_y_ids_csv <- data[c("winner_name","winner_id")] # como esta duplicado no hace falta coger el loser
nombres_y_ids_csv <- unique(nombres_y_ids_csv)
nombres_y_ids_csv$winner_id <- as.character(nombres_y_ids_csv$winner_id)
# ids_tenistas17 <- readClipboard()
# birth_tenistas17 <- readClipboard()
tryCatch(
  setwd("C:/Users/Carlos/Google Drive/scripts/scripts_pfp"),
  error = function(e)
  {setwd("C:/Users/cvaliente/Google Drive/scripts/scripts_pfp")}
)
ids_tenistas17 <- readRDS("ids_tenistas17.rds")
birth_tenistas17 <- readRDS("birth_tenistas17.rds")

birth_tenistas17_v2 <- as.Date(birth_tenistas17, format = "%m/%d/%y")
ids_y_birth17 <- data.frame(ids_tenistas17, birth_tenistas17_v2, stringsAsFactors = F)
x <- inner_join(nombres_y_ids_csv, ids_y_birth17, by= c('winner_id' = "ids_tenistas17"))
x <- dplyr::rename(x, birth = birth_tenistas17_v2)
x$winner_name_orig <- x$winner_name
x$winner_name <- gsub("[(|)]", "", x$winner_name) 
x$winner_name <- gsub("-", " ", x$winner_name) 
listado <- x$winner_name
# parece que en listado hay 2 elementos vacios asi que los eliminamos. POrque se paró al no poder iterar sobre los elementos del nombre
listado <- listado[listado != ""]


if(desarrollando){
#buscar algunos nombres problematicos para las pruebas
  listado <- c()
  listado <- c(listado, (grep('Mayer|Nadal|Ratiwa|Watanu|Sousa|Souza', x$winner_name, value = T)))
  listado <- gsub("[(|)]", "", listado) # No necesario
  listado <- gsub("-", " ", listado)
}
df2$jugador_orig <- df2$jugador
df2$jugador <- gsub("[(|)]", "", df2$jugador) 
df2$jugador <- gsub("-", " ", df2$jugador) 
names <- df2$jugador

# drop parentheses, if any
# names <- gsub("[(|)]", "", names) # No necesario
names <- gsub("-", " ", names)
## comprobacion de las sustituciones
# cbind(names[names != names2], names2[names != names2])

# split those names and get all ways to write that name into a list of same length
split_names <- lapply(
  names,
  FUN = function(x){
    print(x);
    # split by a space
    c_split = unlist(x = strsplit(x = x, split = "\\s{1,}"));
    # get all permutations of c_split to compensate for order
    n <- r <- length(c_split)
    c_splits <- list(permutations(n=n, r=r, v=c_split, set = F, repeats.allowed = FALSE))
    # return c_splits
    c_splits;
  }
)

head(split_names)

tenistasDF <- data.frame()
ini = Sys.time()

# suppose we're looking for this name
# for (search_for in x$winner_name) {
for (search_for in listado[1804:3586]) {
  
  cat("Buscando coincidencias para el tenista: ", search_for , " ...")
  cat("\n  Tiempo transcurrido", Sys.time()-ini)
  
  # split it by " " and then find all ways to write that name
search_for_split <- unlist(x = strsplit(x = search_for, split = "\\s{1,}"));
# permutations over search_for_split seem redundant

# initialize a vector containing if search_for was matched in names
match_statuses <- c();
confianzas <- c()

# for each name that's been split. Para cada nombre le va a asignar un status para decir que es el mismo o no.
for(i in 1:length(names)){
  if(i %% 5000 == 0)print(i)
  # the match status for the current name
  match_status <- FALSE;
  
  # the current split name
  c_split_name <- as.data.frame(split_names[[i]]);
  
  # for each element in c_split_name
  for(j in 1:nrow(c_split_name)){
    
    # the current permutation of current split name
    c_c_split_name <- as.matrix(c_split_name[j,]);
    
    # will receive hits in name's words, one by one, in sequence
    hits <- rep(0, 20) # length 20 should always be above max number of words in names
    
    # for each element in search_for_split
    for(k in 1:length(search_for_split)){
      
      distsNomcompleto <- c()
      
      # the current permutation of name
      c_search_for_split <- search_for_split[[k]];
      
      # L first hits will receive hit counts
      L <- min(ncol(c_c_split_name), length(search_for_split)); # devuelve la longitud de lo q tenga menos elementos: lo q buscas o donde lo buscas
      
      # will match as many words as the shortest current pair of names  
      for(l in 1:L){
        
        # if there's a match, the length of grep is greater than zero
        if(
          # is c_search_for_split in c_c_split_name's lth element
          length(
            agrep(
              pattern = c_search_for_split,
              x = as.character(c_c_split_name[l]),
              ignore.case = T,
              max.distance=list(cost=round(nchar(c_search_for_split)*0.15))
            )
          ) > 0 ||
          # or, is c_c_split_name's lth element in c_search_for_split
          length(
            agrep(
              pattern = c_c_split_name[l],
              x = c_search_for_split,
              ignore.case = T,
              max.distance=list(cost=round(nchar(c_c_split_name[l])*0.15))
            )
          ) > 0
          
          # if this is the case, record a hit    
        ){
          hits[l] <- 1;
        } else {
          # otherwise, don't update hit
        }
        distPalabra <- adist(c_search_for_split, c_c_split_name[l]) / nchar(c_search_for_split)
        distsNomcompleto <- c(distsNomcompleto, distPalabra)
      }
      confianza <- 1 - min(distsNomcompleto)
    }
    
    # take L first elements
    hits <- hits[1:L]
    
    # if hits vector has all ones for this permutation, update match status to TRUE
    if(
      sum(hits)/length(hits)==1 # <- can/should be made more flexible (agrep, or sum/length<1)
    ){
      match_status <- TRUE;
    } else {
      # otherwise, don't update match status
    }
  }
  
  # append match_status to the match_statuses list
  match_statuses <- c(match_statuses, match_status);
  confianzas <- c(confianzas, confianza)
}


tenistadf <- cbind(search_for, names, match_statuses, confianzas) 
tenistadf <- tenistadf[order(tenistadf[,"match_statuses"],tenistadf[,"confianzas"],decreasing=TRUE),] %>% head(5)

tenistasDF <- rbind(tenistasDF, tenistadf)
tenistasDF$confianzas <- as.numeric(as.character(tenistasDF$confianzas))
}

tenistasDF_2 <- inner_join(tenistasDF, x, by = c("search_for" = "winner_name" ))
tenistasDF_2 <- unique(tenistasDF_2) # elimino duplicados completos

# 1 nos quedamos con los que son TRUE y coinciden en fecha nacimiento. Hay algun caso en que coincide la fecha asi que nos quedamos ademas con el de mayor confianza
tenistasDF_2 %>%
  dplyr::filter(match_statuses == TRUE) %>%
  inner_join(dplyr::select(df2, jugador, birth_date, player_id), by = c("names" = "jugador", 'birth' = 'birth_date' )) %>%
  group_by(search_for) %>%
  dplyr::filter(confianzas == max(confianzas)) -> porValidacion1
# Y eliminamos los jugadores que ya han pasado el "filtro"
aux = tenistasDF_2$search_for %in% unique(porValidacion1$search_for)
tenistasDF_2 <- tenistasDF_2[!aux, ]



# 2 por cada tenista nos quedamos con aquel que tiene mayor confianza. Como ya se hizo join, significa que coinciden en fecha todos
tenistasDF_2 %>%
  inner_join(dplyr::select(df2, jugador, birth_date, player_id), by = c("names" = "jugador", 'birth' = 'birth_date' )) %>%
  group_by(search_for) %>%
  dplyr::filter(confianzas == max(confianzas)) -> porValidacion2

aux = tenistasDF_2$search_for %in% unique(porValidacion2$search_for)
tenistasDF_2 <- tenistasDF_2[!aux, ]

# tras observacion manual elimino algunos
porValidacion2 <- porValidacion2[-c(10,11,14),]

# 3 Ya de los que quedan cogemos de los TRUE y que tengan 1 de confi, el que más de cada grupo

tenistasDF_2 %>%
  dplyr::filter(match_statuses == TRUE & confianzas ==  1) %>%
  inner_join(dplyr::select(df2, jugador, player_id), by = c("names" = "jugador")) %>%
  group_by(search_for) %>%
  dplyr::filter(confianzas == max(confianzas)) %>%
  dplyr::filter(n() == 1) -> porValidacion3 # esta ultima es para que no entren repetidos, porque hay algunos tenistas que han encontrado varios con confianza 1

aux = tenistasDF_2$search_for %in% unique(porValidacion3$search_for)
tenistasDF_2 <- tenistasDF_2[!aux, ]

if(desarrollando){ 
# LOS QUE QUEDAN 
tenistasDF_2 %>%  
  group_by(search_for) %>%
  dplyr::filter(confianzas == max(confianzas)) %>%
  arrange(confianzas) %>%
  View() 
}

tenistasDF_3 <- as.data.frame(plyr::rbind.fill(porValidacion1, porValidacion2, porValidacion3)) %>%
  dplyr::rename(winner_name = search_for)

tenistasDF_3$winner_id <- as.numeric(tenistasDF_3$winner_id)
x$winner_id <- as.numeric(x$winner_id)

cols_coincidentes <- colnames(tenistasDF_3) %in% colnames(x)
if(nrow(tenistasDF_3) != nrow(x)){
  View(x[x$winner_id %in% setdiff(x$winner_id, tenistasDF_3$winner_id), ])
  View(tenistasDF_3[tenistasDF_3$winner_id %in% setdiff(x$winner_id, tenistasDF_3$winner_id), ])
  warning("Hay alguno de más o de menos")
}
tenistas_joined <- tenistasDF_3
paraSql <- tenistas_joined
paraSql$birth <- as.character(paraSql$birth)
connect <- odbcConnect("mysqltenis")
sqlSave(connect, paraSql, 'atptour.tenistas_joined_v2', rownames = F)

# duplicados?
if(desarrollando){ 
tenistas_joined %>%
  group_by(winner_name) %>%
  dplyr::filter(n()>1) %>%
  View()
}
