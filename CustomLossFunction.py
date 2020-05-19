#!/usr/bin/env python
# coding: utf-8


# In[ ]:

def predictKeras(df):

    import os
    import pandas as pd
    import numpy as np
    
    import tensorflow as tf
    
    import random as rn
    
    os.environ['PYTHONHASHSEED'] = '0'
    
    # Setting the seed for numpy-generated random numbers
    np.random.seed(37)
    
    # Setting the seed for python random numbers
    rn.seed(1254)
    
    # Setting the graph-level random seed.
    tf.set_random_seed(89)
    
    from keras import backend as K
    K.clear_session()
    K.set_learning_phase(0)
    
    session_conf = tf.ConfigProto(
          intra_op_parallelism_threads=1,
          inter_op_parallelism_threads=1)
    
    #Force Tensorflow to use a single thread
    sess = tf.Session(graph=tf.get_default_graph(), config=session_conf)
    
    K.set_session(sess)
    sess.run(tf.global_variables_initializer())
    
    from keras.models import Sequential, load_model
    from keras.activations import relu, elu, softmax
    
    pd.set_option('display.max_columns', 500)
    pd.options.display.max_rows = 999
    
    df.rename(columns={'Q.1': 'Q1', 'Q.2': 'Q2','partidos.j1': 'partidosj1','partidos.j2': 'partidosj2',
                   'main.qualy_v2': 'mainqualy_v2', 'J1.R1.' : 'J1R1' , 'J2.R1.' : 'J2R1', 'partido.completado.':'partidocompletado',
                   'multiplicador.importancia.del.partido':'multiplicadorimportanciadelpartido'}, inplace=True)
    
    df[['match_id', 'Q1']].head(5)
    
#    df.sort_values(["match_id","odds_j1_pinnacle"], inplace=True)
#    df.drop_duplicates(subset="match_id", keep="first", inplace=True)
#    df.reset_index(drop=True, inplace=True)
    

#    rondas_susceptibles = ['Q-SF', 'F', 'SF','QF', 'Q-2R', 'Q-3R','R64','2R','3R']
#    df = df[df['ronda'].isin(rondas_susceptibles)]
    # vamos a presuponer que todos son ronda 2 y por eso queremos predecirlos
#    df.loc[:,'J1R1'] = 2
#    df.loc[:,'J2R1'] = 2
    
    minPartidosTrain = 14
    all_years = list(range(2013,2019+1,1))
    trainYears = [2013,2015,2017]
    #trainYears = [2015]
    testYears = [2012,2014,2016,2017] # no hay test years porque voy a hacer cross validation
    #simulYears = list(set(all_years) - set(trainYears+testYears))
    simulYears = all_years
    minOdds = 1.2
    maxOdds = 1.55
    
    data = df[(df.partidosj1 >= minPartidosTrain) & (df.partidosj2 >= minPartidosTrain)  & (df.Q1 >= minOdds) & (df.Q1 <= maxOdds) & (df.odds_j1_pinnacle >= minOdds) & (df.J1R1 == 2) & (df.J2R1 == 2)].copy()

    data = df[(df.partidosj1 >= minPartidosTrain) & (df.partidosj2 >= minPartidosTrain) & (df.multiplicadorimportanciadelpartido != 0) & (df.Q1 >= minOdds) & (df.Q1 <= maxOdds) & (df.odds_j1_pinnacle >= minOdds) & (df.J1R1 == 2) & (df.J2R1 == 2)].copy()
    data = data.dropna(subset=['odds_j1_pinnacle', 'odds_j2_pinnacle'])
    data=data.reset_index(drop = True)
    #    data.reset_index(drop=True, inplace=True)
    
    # In[ ]:
    # En esta celda se hace one hot enconding para las variabes categoricas.
    # En los datos que vayamos a predecir es posible que no estén todos los tipos de level asi que hay que considerarlo
    if os.path.exists('C:/.../models/custom_loss_function'):
        os.chdir('C:/.../models/custom_loss_function')
    else:
       os.chdir('C:/.../models/custom_loss_function') 
    
    
    level_dummies = pd.read_pickle('level_dummies.pkl')
    main_qualy_dummies = pd.read_pickle('main_qualy_dummies.pkl')
    
    df_b = pd.get_dummies(data['level'])
    df_c = pd.get_dummies(data['mainqualy_v2'])
    
    df_b = df_b.reindex(columns = level_dummies.columns, fill_value=0)
    df_c = df_c.reindex(columns = main_qualy_dummies.columns, fill_value=0)
    
    
    df_b.columns
    df_c.columns
    # df2 = pd.concat([df.reset_index(drop=False), df_b, df_c], axis=1, join='inner')
    data = pd.concat([data, 
                        df_b, 
                        df_c.drop(['main'], axis = 1)], axis=1) # concateno las dummies generadas y borro las categoricas originales
    data.rename(columns={'1000m': 'm1000', '250atp': 'atp250','500atp': 'atp500'}, inplace=True)
    data.columns
    
    # In[ ]:
    columnas_contextos = [
            "partidosj1","partidosj2", 
    #              'serve.points.won.J1.all.surf' ,
    #              'J1.return.points.won.opponents.quality' ,
    ##              'Average.surface.return.j1.matches' ,
    #              'return.points.won.J1.all.surf' ,
    #              'J1.serve.points.won.opponents.quality'  ,
    ##              'Average.surface.serve.j1.matches' ,
    #              'serve.points.won.J2.all.surf',          
    #              'j2.return.points.won.opponents.quality',
    ##              'Average.surface.return.j2.matches' ,    
    #              'return.points.won.J2.all.surf'  ,       
    #              'j2.serve.points.won.opponents.quality' ,
    ##              'Average.surface.serve.j2.matches',
    ##               "Same.tournament.past.years.j1.games", 
    ##                "Same.tournament.past.years.j1", 
    ##                "Same.tournament.past.years.j2.games", 
    ##                "Same.tournament.past.years.j2",
    #               "Same.cluster.past.years.j1.games",
    #               "Same.cluster.past.years.j1",
    #               "Same.cluster.past.years.j2.games", 
    #               "Same.cluster.past.years.j2",
    #                "VS_playerCluster_j1.games",
    #                "VS_playerCluster_j1",
    #                "VS_playerCluster_j2.games", 
    #                "VS_playerCluster_j2", 
                    'embudo',
                    'valueJ1', 'valueJ2',
                    'Q1',
#                    'Q2',
                    'V1','V2','V3', 'V4',
                    'm1000', 'atp500', 'atp250', 'challenger', 'GS', 'qualy']
    columnas_contextos = list(set(data.columns).intersection(columnas_contextos))
    columnas_contextos.sort()
    
    # In[ ]:
    
    def get_data():
        X = data[columnas_contextos].values # Todas las columnas excepto Class. Aun no hago values porque necesito hacer mas transformaciones. No selecciono columnas contextos porque necesitaré el año para hacer train y test
        y = data['Class'].values # la columna con el resultado del partido. En el ejemplo es 1/2, en mi caso es win/loss. No veo necesario cambiarlo de momento
        y_full = np.zeros((X.shape[0], 3)) # one hot encoding codificando los posibles resultados del partido. Y en las dos ultimas las cuotas
        for i, y_i in enumerate(y):
            if y_i == "win":
                y_full[i, 0] = 1.0 # si en y pone win, entonces en la columna cero les pongo 1. Que son ganar
    #        if y_i == "loss":
    #            y_full[i, 1] = 1.0
    #         se deja libre la columna 1, que son todo ceros y representa el 'no bet'
            y_full[i, 2] = data.iloc[i, data.columns.get_loc('odds_j1_pinnacle')] # las cuotas
        return X, y_full, y
    X, y, outcome = get_data() # se llama a la funcion recien creada y se generan 3 objetos: X que son las predictoras, 'y' que es el target. outcome es una matriz para la funcion de pérdida 
    # The above code gives the X and y for the odds loss.
    
    unique_elements, counts_elements = np.unique(outcome, return_counts=True)
    print(np.asarray((unique_elements, counts_elements)))
    
    print('Las medias de ganados, no bets y cuota del tenista susceptible de apostar son:')
    print(np.mean(y, axis = 0))
    # In[ ]:
    # SPLIT THE DATA IN TRAIN AND TEST DATASET. And convert to numpy arrays
    
    simuldata_x_df = pd.DataFrame(X[(data.Q1 >= minOdds) & (data.Q1 <= maxOdds) & (data.J1R1 == 2) & (data.J2R1 == 2)], columns = columnas_contextos).copy()
    simuldata_y = y[(data.Q1 >= minOdds) & (data.Q1 <= maxOdds) & (data.J1R1 == 2) & (data.J2R1 == 2)].copy()
    #    simuldata_outcome_df = outcome[(data.Q1 >= minOdds) & (data.Q1 <= maxOdds) & (data.J1R1 == 2) & (data.J2R1 == 2)].copy()
    
    simuldata_others = data[(data.Q1 >= minOdds) & (data.Q1 <= maxOdds) & (data.J1R1 == 2) & (data.J2R1 == 2)].copy()
    simuldata_others = simuldata_others.drop(columnas_contextos, axis=1)
    simuldata_others.reset_index(drop=True, inplace=True)
    
    unique_elements, counts_elements = np.unique(simuldata_others['Class'], return_counts=True)
    print(np.asarray((unique_elements, counts_elements)))
    
    # In[ ]:
    estandarizado = True # mejor en True
    if estandarizado == True:
        
        ###########
        ## PRUEBAS STANDARIZAR Y INVERSE
        ###########
     
        
        cols0 = ["partidosj1","partidosj2", 
                   "Same.tournament.past.years.j1.games",  "Same.tournament.past.years.j2.games", 
                   "Same.cluster.past.years.j1.games","Same.cluster.past.years.j2.games",
                    "VS_playerCluster_j1.games","VS_playerCluster_j2.games"] #columnas_a_standarizar
        cols0 = [
            "partidosj1","partidosj2", 
                  'serve.points.won.J1.all.surf' ,
                  'J1.return.points.won.opponents.quality' ,
    #              'Average.surface.return.j1.matches' ,
                  'return.points.won.J1.all.surf' ,
                  'J1.serve.points.won.opponents.quality'  ,
    #              'Average.surface.serve.j1.matches' ,
                  'serve.points.won.J2.all.surf',          
                  'j2.return.points.won.opponents.quality',
    #              'Average.surface.return.j2.matches' ,    
                  'return.points.won.J2.all.surf'  ,       
                  'j2.serve.points.won.opponents.quality' ,
    #              'Average.surface.serve.j2.matches',
                   "Same.tournament.past.years.j1.games", 
                    "Same.tournament.past.years.j1", 
                    "Same.tournament.past.years.j2.games", 
                    "Same.tournament.past.years.j2",
                   "Same.cluster.past.years.j1.games",
                   "Same.cluster.past.years.j1",
                   "Same.cluster.past.years.j2.games", 
                   "Same.cluster.past.years.j2",
                    "VS_playerCluster_j1.games",
                    "VS_playerCluster_j1",
                    "VS_playerCluster_j2.games", 
                    "VS_playerCluster_j2", 
                    'embudo',
                    'valueJ1',
                    'valueJ2',
                    'Q1', 
                    'Q2',
                    'V1','V2','V3', 'V4',
                    'm1000', 'atp500', 'atp250', 'challenger', 'GS', 'qualy']
        
        cols = []
        def list_contains(List1, List2): 
          
            # Iterate in the 1st list 
            for m in List1: 
          
                # Iterate in the 2nd list 
                for n in List2: 
            
                    # if there is a match
                    if m == n: 
                        cols.append(m)
            return cols  
                    
        cols = list_contains(cols0, columnas_contextos)
        cols.sort()
        
        from sklearn.externals import joblib
        if os.path.exists('C:/.../custom_loss_function'):
            os.chdir('C:/.../custom_loss_function')
        else:
           os.chdir('C:/.../custom_loss_function')
        
        
        scaler_filename = "scaler_predecir.save"
        scaler = joblib.load(scaler_filename) 
            
        simuldata_x_scaled =  pd.DataFrame(simuldata_x_df).copy() # copia del df de origen  
        
        simuldata_std = scaler.transform(simuldata_x_scaled[cols]) # se lo aplicas a la copia solo sobre las columnas que interese estandarizar
        simuldata_x_scaled[cols] = simuldata_std # a scaled le metes las columnas estandarizadas, dejando el resto tal y como estaban de origen
        simuldata_x = simuldata_x_scaled.values
        
    #    restored_test_x = test_x_scaled.copy() # te creas una copia del estandarizado
    #    x_unStd = scaler.inverse_transform(test_x_scaled[cols]) # usando la instancia que se creó cuando standarizamos, aplicas la invertida
    #    restored_test_x[cols] = x_unStd # a la copa le metes las columnas estandarizadas, dejando el resto tal y como estaban de origen
               
    #    assert np.allclose(restored_test_x[cols].values, test_x[cols].values)  
        
    #elif estandarizado == 'soloNumPartidos':
    #    train_x["partidosj1"] = train_x["partidosj1"] / 100
    #    train_x["partidosj2"] = train_x["partidosj2"] / 100
    #    test_x["partidosj1"] = test_x["partidosj1"] / 100
    #    test_x["partidosj2"] = test_x["partidosj2"] / 100
    # In[ ]:
    
    
    def odds_loss(y_true, y_pred):
        """
        The function implements the custom loss function mentioned in info.pdf
        
        Inputs
        true : a vector of dimension batch_size, 7. A label encoded version of the output and the backp1_a and backp1_b
        pred : a vector of probabilities of dimension batch_size , 5.
        
        Returns 
        the loss value
        """
        win_home_team = y_true[:, 0:1]
    #    win_away = y_true[:, 1:2]
        no_bet = y_true[:, 1:2]
        odds_a = y_true[:, 2:3]
        gain_loss_vector = K.concatenate([win_home_team * (odds_a - 1) + (1 - win_home_team) * -1,
                                          K.zeros_like(odds_a)], axis=1) # Returns: A Keras variable with the shape of x filled with zeros. Así que en nuestro caso es un cero
        return -1 * K.mean(K.sum(gain_loss_vector * y_pred, axis=1)) # se multiplican cada uno de los elementos de los 2 vectores (predicho vs sucedido) y se suma toda la fila. Despues la media de cada columna
     
    
    
    true = K.variable(np.array([[0, 0, 1.3]]), dtype='float32')
    pred = K.variable(np.array([[0.6, 0.4]]), dtype='float32')
    
#    K.eval(odds_loss(true, pred))
    
    
    # It measures the profit or loss average over the input for a unit stake.
    
    
    # In[ ]:
    K.clear_session()
    
    from keras.backend import manual_variable_initialization
    manual_variable_initialization(True)

    loaded_model = load_model("detalle_predicciones_Dtrain2013_2015_2017Dtest2012_2014_2016_2017_v5.h5", custom_objects={'odds_loss': odds_loss}, compile=False)    
    
    
    from keras.layers import BatchNormalization, Dense, Input, Dropout
    from keras.models import Model
    from keras.optimizers import SGD
    def get_model(input_dim, output_dim, base=1000, multiplier=0.25, p=0.2):
         inputs = Input(shape=(input_dim,))
         l = BatchNormalization()(inputs)
         l = Dropout(p)(l)
         n = base
         l = Dense(n, activation='relu')(l)
         l = BatchNormalization()(l)
         l = Dropout(p)(l)
         n = int(n * multiplier)
         l = Dense(n, activation='relu')(l)
         l = BatchNormalization()(l)
         l = Dropout(p)(l)
         n = int(n * multiplier)
         l = Dense(n, activation='relu')(l)
         outputs = Dense(output_dim, activation='softmax')(l)
         model = Model(inputs=inputs, outputs=outputs)
         opt = SGD(lr=0.005 , momentum=0.9)
         model.compile(optimizer=opt, loss=odds_loss)
         return model
    loaded_model = get_model(input_dim=simuldata_x.shape[1], output_dim= 2,base=len(columnas_contextos)*6, multiplier=0.5,p= 0.5)
    

    loaded_model.load_weights("weights")
    
    print(loaded_model.summary())
    print('simuldata Loss : {}'.format(loaded_model.evaluate(simuldata_x, simuldata_y)))
    
    # In[ ]:
    # SIMULACION
    
    simul_y_pred = loaded_model.predict(simuldata_x)
    
    simul_y_pred = np.argmax(simul_y_pred, axis = 1)
    uq_output = [None] * len(simul_y_pred)
    for i in range(len(simul_y_pred)):
        if simul_y_pred[i] == 0:
            uq_output[i] = "win"
        elif simul_y_pred[i] == 1:
            uq_output[i] = "no bet"
    
    def PLfunc(row):
        if (row['Class'] == 'win') & (row['predAction'] == 'win'):
            return (row['odds_j1_pinnacle']-1) * row['stakeProporcional']
        elif (row['Class'] == 'loss') & (row['predAction'] == 'win'):
            return row['stakeProporcional'] * -1 
        else:
            return 0
    
    uq_output_df = pd.DataFrame(uq_output, columns = ['predAction'])
    simuldata = pd.concat([simuldata_x_df,simuldata_others,uq_output_df],axis=1)
    
    simuldata['stake'] = 2
    simuldata['stakeProporcional']  = simuldata['stake'] / simuldata['odds_j1_pinnacle']
    simuldata['PL'] = simuldata.apply(PLfunc, axis=1)
    simuldata['stake'] = np.where(simuldata['PL']!= 0, 2, 0)
    simuldata['stakeProporcional']  = simuldata['stake'] / simuldata['odds_j1_pinnacle']
    simuldata['yield']  = simuldata['PL'] / simuldata['stakeProporcional']
    simuldata['win'] = np.where( (simuldata['PL']> 0), 1, 0)
    simuldata['bet'] = np.where( (simuldata['PL']!= 0), 1, 0)
    simuldata['loss'] = np.where( (simuldata['PL']< 0), 1, 0)
    
    d = {'PL':'PL', 'yield':'yield', 'win':'wins', 'loss':'loses', 'bet':'bets'}
    results = simuldata.groupby('year').agg({'PL':'sum', 'yield':'mean', 'win':'sum', 'loss':'sum', 'bet':'sum'}).rename(columns=d).copy()
    results =results.append(results.agg(['sum', 'mean']))
    results.index.name = 'year'
    results.reset_index(inplace=True)
    output = results.to_string(formatters={
        'PL': '{:,.1f}'.format,
        'yield': '{:,.1%}'.format,
        'wins': '{:,.0f}'.format,
        'loses': '{:,.0f}'.format,
        'bets': '{:,.0f}'.format,
    })
    #    trainYears_column = pd.DataFrame(trainYears) 
    #    results.merge(trainYears_column, left_index=True, right_index = True)
    print(output)
    
    print('PL: ') 
    print(sum(simuldata['PL']))
    print('wins: ')
    print(len(simuldata.loc[simuldata['PL'] > 0]))
    print('loses: ')
    print(len(simuldata.loc[simuldata['PL'] < 0]))
    print('bets: ')
    print(len(simuldata.loc[simuldata['PL'] > 0]) + len(simuldata.loc[simuldata['PL'] < 0]))
    print('Yield: ')
    print(sum(simuldata['PL']) / simuldata.loc[simuldata['PL'] != 0, 'stakeProporcional'].sum())
    
    # In[ ]:   
#    export = pd.concat([simuldata_x_df[columnas_contextos], simuldata['predAction'], simuldata_others], axis = 1)
#    simuldata.to_csv(r'C:/Users/Carlos/Google Drive/alex_vicens_carlos_valiente/comparar CSVs/keras_predicciones.csv')

    # In[ ]:        
    return (simuldata, results)
   



