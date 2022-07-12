# code_portfolio
Some pieces of code for data portfolio

## Ranking chart
Visuals from tennis players evolution since 2014, [plot animation](https://public.flourish.studio/visualisation/585459/).

![image](https://user-images.githubusercontent.com/65561135/178552534-c7d62134-733f-4c57-9445-16954b919a16.png)

## Clustering
Get tennis tournaments groups based on weather, court, elevation, location and playing style.

![image](https://user-images.githubusercontent.com/65561135/178557288-85a1a20f-a90f-409f-ac23-0338a76f7bbf.png)

![cluster by player03_kmeans4_ok](https://user-images.githubusercontent.com/65561135/178553409-ab0877af-0514-4d51-a597-0617f4c2f11a.png)

### Feature engineering, clustering y random forest. "f_clusteringBySurface_for_github.R"
- Data preparation and normalization for clustering
- Print and plot results
- Save random forest predictive model in order to predict new observations

Meaningful conclusion:

![image](https://user-images.githubusercontent.com/65561135/178557201-8551d8f7-6f67-4558-91aa-ecd9c3773697.png)

To reproduce it yourself feel free to ask me for some sources via [linkedin](https://www.linkedin.com/in/carlos-valiente-casas-a270592a/ )
Then you can run it from clustering/main.R

### Referential integrity and join. "f_comprobaciones_torneos_y_traer_altitud.R"

Function that checks the referential integrity between two CSV files and then if it does not exist it creates it. 'data' represents a fact table, and tournaments represents a dimension table from my Data Warehouse. Once validations are passed, it add the extra variable elevation to the fact table. Then it will be ready for analysis and machine learning.

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
