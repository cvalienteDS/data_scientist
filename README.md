# code_portfolio
Some pieces of code for data portfolio

## Ranking chart
Visualization, [plot](https://public.flourish.studio/visualisation/585459/).

![image](https://user-images.githubusercontent.com/65561135/178552534-c7d62134-733f-4c57-9445-16954b919a16.png)

## Clustering
In order to reproduce it yourself feel free to ask me for some sources via linkedin: https://www.linkedin.com/in/carlos-valiente-casas-a270592a/ 
Then you can execute it from clustering/main.R

### Feature engineering, clustering y random forest. "f_clusteringBySurface_for_github.R"
- Data preparation and normalization for clustering
- Print and plot results
- Save random forest predictive model in order to predict new observations

![cluster by player03_kmeans4_ok](https://user-images.githubusercontent.com/65561135/178553409-ab0877af-0514-4d51-a597-0617f4c2f11a.png)

### Integridad referencial y join. "f_comprobaciones_torneos_y_traer_altitud.R"
Función que comprueba la integridad referencial entre dos CSVs y si no existe la crea. 'data' representa una tabla de hechos, y torneos representa una dimensión. Una vez validada la integridad importa la variable altitud a la tabla de hechos y la deja preparada para análisis y machine learning.

## Keras neural network with custom loss function. "CustomLossFunction.py"
- Train neural network

![history_keras](https://user-images.githubusercontent.com/65561135/178553733-a471a7f8-2e46-4ef0-9f85-ffe408eece5c.PNG)
- Hyper parameter tuning 

<img width="346" alt="heatmap_talos" src="https://user-images.githubusercontent.com/65561135/178554054-a8ea9dcb-18b4-4949-ab0f-4b8cc9cf2300.png">

- Predictions explainability based on Shap values

![shap_from_keras_predictions](https://user-images.githubusercontent.com/65561135/178553798-11ef0314-0ef6-44be-ba70-9e3d19957075.png)
- Real world simulations

![umbral_same_tour01](https://user-images.githubusercontent.com/65561135/178553943-662eb43a-41c9-4ee7-87e4-9e0395a5be9f.png)	

Based on: https://medium.com/@media_73863/machine-learning-for-sports-betting-not-a-basic-classification-problem-b42ae4900782

## "EDA.md"
Simple [data exploration](https://github.com/cvalienteDS/data_scientist/blob/master/EDA.md) with dplyr
