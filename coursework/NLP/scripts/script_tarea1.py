# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py:percent
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.17.3
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# %%
# imports
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import seaborn as sns
import pandas as pd
import scipy

# %%
# load data
data_raw = pd.read_csv("primary_results.csv")
print(data_raw)
print(data_raw.shape)
# null counts
print(data_raw.isna().sum())

# %% [markdown]
# 1. Carge este archivo en un dataframe. Descríbalo (columnas, número de observaciones, valores nulos) ¿Qué filas/columnas le sirven para el problema?
#
# El archivo tiene 24611 observaciones y 8 variables, todos los valores nulos corresponden a fips.
# El problema considerara como variable dependiente "candidate", las restantes columnas seran usadas como predictores, excepto "fips" que es la variable id.
# Sin embargo podemos eliminar las variables state-state_abbreviation en tanto aportan la misma informacion y
# la variable party en tanto aporta informacion directa del candidato, dependiendo de como se considere el problema.
# En tanto estas son votaciones de una primaria podriamos determinar (1) los resultados de las votaciones por 'party' of bien considerar al ganador simplemente como quien recibio la mayor cantidad de votos.
# Para este trabajo considerare la votacion como si fuese una votacion libre (no tipo primaria), por lo tanto se observaran los votos netos obtenidos.

# %% [markdown]
# 2. Cree una variable binaria que indique si en dicho county Trump tuvo o no la mayoría de los votos. Esa será su variable dependiente para la clasificación. Explore el balance de dicha variable.


# %%
data_proc = data_raw

# esto no considera que esta sea una votacion tipo primaria
# por lo tanto busca el indice por county que tuvo la mayor cantidad de votos netos
idx = data_raw.groupby('county')['votes'].idxmax()

# seleccionar esas filas y cortar el max
# simplemente interesa la fila que corresponde al candidato con mas votas netos
data_proc = data_raw.loc[idx]

# crear la dummy
data_proc['trump_dummy'] = np.where(data_proc['candidate'] == 'Donald Trump', 1, 0)
data_proc['trump_dummy_cat'] = np.where(data_proc['trump_dummy'] == 1, "Win", "Loss")
print(data_proc.sort_values(by = 'trump_dummy', ascending=False))

# proporcion de victorias y derrotas de trump
prop_of_trump = round(data_proc['trump_dummy'].mean(), 2)
print(f"Proporcion de victorias: {prop_of_trump}")
print(f"Proporcion de derrotas: {round(1 - prop_of_trump, 2)}")

# representacion grafica
plt.hist(data_proc['trump_dummy_cat'])
plt.xlabel("Results")
plt.ylabel("Frequency (across county)")
plt.title("Win-Loss record for Trump")
plt.show()

# %% [markdown]
# 3. Ahora necesitamos un conjunto de regresores. Para ello cargue el archivo 'county_facts.csv'. Explore dicho dataframe. Recuerde que tiene a disposición el libro de códigos.

# %%
predictors_raw = pd.read_csv("county_facts.csv")
print(predictors_raw.info())

# %% [markdown]
#
# 4. Revisando el libro de códigos, vamos a seleccionar un subconjunto de columnas:
#
# 'AGE775214' : Persons 65 years and over, percent, 2014
# 'RHI725214' : Hispanic or Latino, percent, 2014
# 'EDU685213' : Bachelor's degree or higher, percent of persons age 25+, 2009-2013
# 'HSG445213' : Homeownership rate, 2009-2013
# 'PVY020213' : Persons below poverty level, percent, 2009-2013
# 'SBO015207' : Women-owned firms, percent, 2007
# 'BZA010213' : Private nonfarm establishments, 2013
# 'LND110210' : Land area in square miles, 2010
#
# Siéntase libre de escoger otro conjunto de columnas. Esto es solo para no demorarnos tanto.
#
# Describa las variables de interés. ¿Hay valores nulos? ¿Hay outliers? Para esto último puede visualizar histogramas o usar la función describe() para ver la estadística descriptiva.

# %%
# para determinar las variables usare como metrica
# la desviacion estandar, generare un ranking
# y tomare arbitrariamente las 10 primeras variables
# filtrare variables no numericas y el id
features_to_rank = predictors_raw.drop(columns=['fips']).select_dtypes(include='number')
std_devs = features_to_rank.std()
rank = std_devs.rank(ascending=False)
rank_col = pd.DataFrame({
    'std': std_devs,
    'rank': rank
}).sort_values(by='rank')
# seleccionar el top 10
final_features = rank_col.head(10)
predictors_proc = predictors_raw[final_features.index.tolist() + ['fips']]
# distribucion
print(predictors_proc.describe())
# valores nulos
print(predictors_proc.isna().sum())

# representacion grafica para distribucion/outliers
# generamos zscores para que sean comparables
tmp = predictors_proc.drop(columns='fips')
zscores = tmp.apply(scipy.stats.zscore)
predictors_zscore = zscores

sns.boxplot(data = predictors_zscore)
plt.xticks(rotation=90, ha='right')
plt.title("Predictor variables distribution")
plt.ylabel("Zscore")
plt.show()

# el grafico indica que variables relacionadas con ventas y crecimiento poblacional
# cuentan con valores por encima de las 50 desviaciones estandar del promedio
# valores muy por encima de 3 std probablemente no hacen sentido
# usare una estrategia conservadora para eliminarlos en base a
# el rango intercuartilico, cambiando el pivot de media a mediana para 
# no ser influenciado por los valores extremos
Q1 = tmp.quantile(0.25)
Q3 = tmp.quantile(0.75)
IQR = Q3 - Q1
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR
outliers = (tmp < lower_bound) | (tmp > upper_bound)
print(outliers)

# construir la base de features final
predictors_final = tmp[~outliers.any(axis=1)].join(predictors_proc[['fips']])
print(predictors_final)


# %% [markdown]
#
# 5. Vemos que para algunas variables tenemos outliers. Vamos a eliminar cualquier instancia mayor que 5
#
# . Esto puede no ser la solución más ideal, sobretodo porque hay muchos algoritmos que son sensibles a los outliers. Para esto usamos la función zscore(). También podemos escalar la data con RobustScaler()
#
# (modifique esto en base a las variable seleccionadas y el metodo de outliers fue usando IQR, luego de observacion via zscores)

# %% [markdown]
# 6. Finalmente, haga un merge entre ambas bases de datos para tener todo lo necesario para entrenar los clasificadores.

# %%
data_merged = pd.merge(data_proc, predictors_final, on='fips')
print(data_merged.info())

# %% [markdown]
# 7. Especificar un random forest.
# Para encontrar un RF con el numero optimo de arboles, es necesario especificar el resto de hiper-parameteros, de otra manera, podemos encontrar el numero optimo de arboles en cuanto encontramos la especificacion optima de todo los hiper-parametros.

# %%
from sklearn.model_selection import cross_val_score
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import RandomizedSearchCV, StratifiedKFold, cross_validate
from scipy.stats import randint, uniform
from sklearn.datasets import make_classification
from sklearn.ensemble import RandomForestRegressor
from sklearn.inspection import PartialDependenceDisplay


# %%
# el primer paso es optimizar los hyper-parametros del RF
# la estrategia para hacerlo mas rapido es hacer uso 
# de la computacion interna del algoritmo de un error out-of-bag (aqui computaremos el oob accuracy score, para mantener los mejores valores en positivo)
# y probar una grilla aleatoria de paramateros, aunque lo ideal seria 
# una busqueda en grillas uniformes of algun algoritmo de busqueda como los de evolucion

# funcion para extraer el OOB accuracy score
def oob_scorer(estimator, X, y):
    return estimator.oob_score_

# ordenar variables dependendiente e independientes
# remuevo county, ya que en este dataset se puede considerar
# como un indice y llevar a overfitting
X = data_merged.drop(columns=['trump_dummy', 'trump_dummy_cat',
                             'state_abbreviation', 'party', 'fips',
                             'fraction_votes', 'votes', 'candidate', 'county'])
X['state'] = pd.factorize(X['state'])[0]
y = data_merged['trump_dummy_cat']

param_dist = {
    # parametro a observar, numero de arboles
    'n_estimators': randint(10, 500),
    'max_depth': [10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    'min_samples_leaf': randint(1, 11),
    'min_samples_split': randint(2, 21),
    'max_features': uniform(0.1, 0.9),
    'bootstrap': [True],
    'criterion': ['gini']
}

# iniciar el clasificador
# n_jobs=-1 es para usar todos los cpu!
rf = RandomForestClassifier(oob_score=True, n_jobs=-1, random_state=42)

random_search = RandomizedSearchCV(
    estimator=rf,
    param_distributions=param_dist,
    # pocas iteracion para prueba de concepto
    n_iter=100,
    # sklearn require almenos 1 particion, tecnicamente esto no es necesario
    # pero computacionalmente no es problematico, en tanto tomamos el OOB the ambas
    # y se obtiene un promedio simple, ganando un poco mas de estabilidad en el
    # estimado final
    cv=2,
    scoring=oob_scorer,
    verbose=3,
    random_state=42,
    n_jobs=-1
)

random_search.fit(X, y)
best_params = random_search.best_params_
print(best_params)

# pora la representacion grafica construire un grafico de dependencia parcial
# asi poder determinar la calidad del modelo en base al numero de arboles
# mientras se considera el resto de hiper-parametros
optim_results = pd.DataFrame(random_search.cv_results_)
mm_features = [
    'param_n_estimators',
    'param_max_features',
    'param_max_depth',
    'param_min_samples_leaf'
]
X_mm = optim_results[mm_features]
y_mm = optim_results['mean_test_score']

# fit del metamodelo
meta_model = RandomForestRegressor(
    n_estimators=100,
    random_state=42,
    oob_score=True
)
meta_model.fit(X_mm, y_mm)

# representacion grafica
fig, ax = plt.subplots(
    2, 2, figsize=(15,12), constrained_layout=True
)
fig.suptitle('Partial Dependence of Hyper-parameters (RF meta-model)')
display = PartialDependenceDisplay.from_estimator(
    meta_model,
    X_mm,
    features=mm_features,
    ax=ax
)
plt.show()

# %% [markdown]
# La funcion de dependencia parcial (el eje y es su output), estima el efecto marginal de una 'feature' $S$ en la prediccion de un modelo $f$, computado de la siguiente forma:
# $$\hat{f}_S(x_S) = \frac{1}{n} \sum_{i=1}^{n} f(x_S, x_{C,i})$$
# Donde:
# - $\hat{f}_S(x_S)$ es la funcion de dependencia parcial para la 'feature' $S$ evaluado en el valor $x_S$
# - $n$ es el numero de instancias en el dataset
# - $x_S$ es el valor espefico de la 'feature' de interes
# - $x_{C,i}$ representa los valores de todas las otras 'features' para la $i-esima$ instancia en el dataset
#
# Idealmente, se buscaria obtener algun 'bootstrap' de las metricas presentadas para determinar el nivel de incertidumbre. Sin embargo, y considerando una unica corrida del meta-modelo. Se observa que cercano a los ~100-200 arboles (param_n_estimators), se obtiene un 'codo', luego del cual el rendimiento es considerablemente estable. En terminos teoricos el punto optimo del arbol es referente a la demanda computacional, en tanto una mayor cantidad de arbol solo deberia tener el efecto de hacer menos variable la prediccion, sin afectar el sesgo. Por lo tanto ~100-200 arboles parece ser el minimo necesario para que el modelo sea computacionalmente estable.

# %% [markdown]
# Para determinar el desempeno del modelo utilizamos los parametros optimizados y con ellos corremos una 'cross-validation' para obtener la metrica de accuracy promedio.

# %%
# en base a estos parametros especificamos el modelo
print(best_params)
optimal_model = RandomForestClassifier(
    **best_params,
    random_state=42,
    n_jobs=-1
)

# asumimos una especie de jerarquia donde las observaciones
# pertenencen a un 'state' particular
cv_strat = StratifiedKFold(
    n_splits=10,
    shuffle=True,
    random_state=42
)
scoring_metrics = ['accuracy']

# correr la crossvalidation
cv_results = cross_validate(
    optimal_model,
    X, y,
    cv=cv_strat,
    scoring=scoring_metrics
)

# rendimiento obtenido
mean_score = round(cv_results['test_accuracy'].mean(), 2)
std_score = round(cv_results['test_accuracy'].std(), 2)
print(f"Mean accuracy: {mean_score}, std: {std_score}")

# %% [markdown]
# 8. Especificar un MLP, optimizar hiper-parametros y reportar resultados.

# %%
from sklearn.neural_network import MLPClassifier
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline

# establecer el pipeline de optimizacion
param_dist = {
    'mlp__hidden_layer_sizes': [
        # 1 layer
        (50,), (100,),
        # 2 layer
        (50, 50), (100, 50),
        # 3 layer
        (100, 100), (150, 100),
        # 4 layer
        (50, 50, 50), (100, 100, 50), (100, 100, 100),
        # 5 layer
        (50, 50, 50, 50)],
    # fijando los parametros categoricos
    # para observar los parametros mas cuantitativos
    'mlp__activation': ['relu'],
    'mlp__solver': ['sgd'],
    'mlp__alpha': uniform(0.0001, 0.1),
    'mlp__learning_rate_init': uniform(0.001, 0.1)
}

pipeline = Pipeline([
    ('scales', StandardScaler()),
    ('mlp', MLPClassifier(max_iter=1000, random_state=42))
])

# busqueda aleatoria de la grilla
random_search_mlp = RandomizedSearchCV(
    estimator=pipeline,
    param_distributions=param_dist,
    n_iter=100,
    cv=5,
    scoring='accuracy',
    verbose=3,
    random_state=42,
    n_jobs=-1
)

random_search_mlp.fit(X, y)
results_df_mlp = pd.DataFrame(random_search_mlp.cv_results_)
# best params
print(random_search_mlp.best_params_)

# data para el metamodelo
results_df_processed_mlp = results_df_mlp.copy()
results_df_processed_mlp['param_mlp__num_layers'] = results_df_processed_mlp['param_mlp__hidden_layer_sizes'].apply(lambda x: len(x))
results_df_processed_mlp['param_mlp__neurons_per_layer'] = results_df_processed_mlp['param_mlp__hidden_layer_sizes'].apply(lambda x: x[0])
results_df_processed_mlp = pd.get_dummies(results_df_processed_mlp, columns=['param_mlp__activation', 'param_mlp__solver'])

# entrenar el metamodelo para general los partial-dependence-plots
features_for_pdp = [
    'param_mlp__alpha',
    'param_mlp__learning_rate_init',
    'param_mlp__num_layers',
    'param_mlp__neurons_per_layer'
]
X_mm_mlp = results_df_processed_mlp[features_for_pdp]
y_mm_mlp = results_df_processed_mlp['mean_test_score']

meta_model_mlp = RandomForestRegressor(n_estimators=100, random_state=42)
meta_model_mlp.fit(X_mm_mlp, y_mm_mlp)

# representacion grafica
fig, ax = plt.subplots(
    2, 2,
    figsize=(15, 12),
    constrained_layout=True
)
fig.suptitle('PDP de accuracy para los hiper-parametros del MLP')
display = PartialDependenceDisplay.from_estimator(
    meta_model_mlp,
    X_mm_mlp,
    features=features_for_pdp,
    ax=ax)

plt.show()

# %% [markdown]
# Los parametros optimos del MLP quedan descritos en 'random_search_mlp.best_params_'. En terminos generales los PDPs nos indican que una arquitectura relativamente simple y poco profunda parece ser la optima, favoreciendo tasas de aprendizaje mas bajas ('param_mlp__learning_rate_init') y regularizacion mas suave ('param_mlp_alpha'). Para el numero de capas ('param_mlp__num_layers') muestra mejores resultados en rangos bajos, probablemente indicando que la funcion subyacente en los datos no es de gran complejidad, en el numero de neuronas se ve algo similar pero menos pronunciado, probablemente relacionado con la baja dimensionalidad (al seleccionar el top 10 de variables por desviacion standard). Abajo se encuentran anotados los valores de accuracy para el modelo optimizado.

# %%
mean_cv_accuracy_mlp = random_search.best_score_
best_model_index = random_search.best_index_
std_cv_accuracy_mlp = random_search.cv_results_['std_test_score'][best_model_index]


print(f"Hiper-parametros optimos: {random_search.best_params_}\n")
print(f"Accuracy promedio en crossvalidation: {mean_cv_accuracy_mlp}")
print(f"Desviacion estandar de la accuracy: {std_cv_accuracy_mlp}")
