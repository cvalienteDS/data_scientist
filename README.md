# code_portfolio
Some pieces of code for data portfolio

## Ranking chart
Visualization, plot.
https://public.flourish.studio/visualisation/585459/

## Clustering
In order to reproduce it yourself feel free to ask me for some sources via linkedin: https://www.linkedin.com/in/carlos-valiente-casas-a270592a/ 
Later you can execute it from clustering/main.R
### Feature engineering, clustering y random forest. "f_clusteringBySurface_for_github.R"
- Data preparation and normalization for clustering
- Print and plot results
- Save random forest predictive model in order to predict new observations
### Integridad referencial y join. "f_comprobaciones_torneos_y_traer_altitud.R"
Función que comprueba la integridad referencial entre dos CSVs y si no existe la crea. 'data' representa una tabla de hechos, y torneos representa una dimensión. Una vez validada la integridad importa la variable altitud a la tabla de hechos y la deja preparada para análisis y machine learning.

## Keras neural network with custom loss function. "CustomLossFunction.py"
Based on: https://medium.com/@media_73863/machine-learning-for-sports-betting-not-a-basic-classification-problem-b42ae4900782

## "EDA.md"
Simple data exploration with dplyr
