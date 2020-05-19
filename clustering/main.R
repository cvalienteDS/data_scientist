# please set your working directory

dataIN <- read.csv('toy_sample.csv', 
                       header=T , 
                       dec=".",
                       sep=';', 
                       encoding = "UTF-8",
                       stringsAsFactors = F,
                       na.strings=c("#N/D", "#VALOR","#¡VALOR!", "#VALOR!", "#DIV/0!", "#NOMBRE?", "#REF", "Err:522"))
                       
source(paste0(getwd(),'/f_clusteringBySurface_for_github.R'))

out <- f_clusterBySurface(trainYears = c(2010:2019), 
                                  numSurfClusters = 6,
                                  clusterMethod = 'kmeans',
                                  distanceMethod = 'ward.D',
                                  data = dataIN,
                                  minReg = 40,
                                  generar_clusters = T
)
