# code_portfolio
Some pieces of code for data portfolio

### Feature engineering, clustering y random forest. "f_clusteringBySurface.R"
- Data preparation and normalization for clustering
- Print and plot results
- Save random forest predictive model in order to predict new observations

### Ranking chart
Visualization, plot.
https://public.flourish.studio/visualisation/585459/

### Keras neural network with custom loss function. "CustomLossFunction.py"
Based on: https://medium.com/@media_73863/machine-learning-for-sports-betting-not-a-basic-classification-problem-b42ae4900782

### Integridad referencial y join. "f_comprobaciones_torneos_y_traer_altitud.R"
Comprueba la integridad referencial entre dos CSVs y si no existe la crea. data representa una tabla de hechos, y torneos representa una dimensión. Una vez validada la integridad importa la variable altitud a la tabla de hechos y la deja preparada para análisis y machine learning.

### "EDA.md"
Exploración de datos simple con dplyr

### Fuzzy search. "f_fuzzy_join_v2.R"
Une 2 bases de datos de distintas fuentes y por tanto con IDs distintos, a través de similitud en el nombre del tenista (aplicable a cualquier otro tipo de entidad)
It joins 2 databases from different sources and therefore with different IDs, through similarity in the tennis player's name + last name (applicable to any other kind of entity)
