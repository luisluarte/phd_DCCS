# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.16.1
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# %% [markdown]
# # Evaluación análisis espacial

# %%
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import seaborn as sns
from IPython.display import display, Markdown
from libpysal.weights import Queen, KNN
from esda.moran import Moran, Moran_Local
from splot.esda import plot_moran, plot_moran_simulation, lisa_cluster

# config plots
plt.style.use('seaborn-v0_8-whitegrid')
# %matplotlib inline

# %% [markdown]
# ## Instrucción 1

# %% [markdown]
# ### Carga de datos
# %%
# definición de los paths, la estructura es código en /scripts
# data en /data y /data/shapes
PATH_SHP_POLICE = 'data/shapes/cuartelesGS.shp'
PATH_SHP_CRIME = 'data/shapes/delitos.shp'
PATH_SHP_CENSO = 'data/shapes/zonasGS.shp'
PATH_CSV_CENSO = 'data/censo17_zona.csv'
PATH_CSV_VIF = 'data/delitos_zona.csv'

# cargar data homicidios y robos con fuerza o intimidación
# columna 'key' es DMCS delitos de mayor connotación social
gdf_crime = gpd.read_file(PATH_SHP_CRIME).to_crs(32719)
print(gdf_crime.head())
# tipos de delitos
print(gdf_crime['Delito'].unique())

# cargar cuarteles de carabineros
gdf_police = gpd.read_file(PATH_SHP_POLICE).to_crs(32719)
print(gdf_police.head())
# tipos de comisarías
print(gdf_police['TIPO_DE_UN'].unique())
# cambiar dos comisaría con y sin tilde (normalizar)
gdf_police['TIPO_DE_UN'] = gdf_police['TIPO_DE_UN'].str.replace('COMISARÍA', 'COMISARIA')
print(gdf_police['TIPO_DE_UN'].unique())
# aplicar filtro a comisaría tal como sugiere instrucción 1
gdf_comisarias = gdf_police[gdf_police['TIPO_DE_UN'] == 'COMISARIA']
gdf_comisarias = gdf_comisarias.to_crs(32719)
print(gdf_police.shape)
print(gdf_comisarias.shape)

# %% [markdown]
# ### Construcción de buffers
# %%
# 1000 metros
geom_1000 = gdf_comisarias.geometry.buffer(1000)
gdf_buffer_1000 = gpd.GeoDataFrame(geometry=geom_1000, crs=gdf_comisarias.crs)
# 2000 metros
geom_2000 = gdf_comisarias.geometry.buffer(2000)
gdf_buffer_2000 = gpd.GeoDataFrame(geometry=geom_2000, crs=gdf_comisarias.crs)
# 3000 metros
geom_3000 = gdf_comisarias.geometry.buffer(3000)
gdf_buffer_3000 = gpd.GeoDataFrame(geometry=geom_3000, crs=gdf_comisarias.crs)

# plotear 2000 metros para verificar
fig, ax = plt.subplots(figsize=(10, 10))
gdf_buffer_2000.plot(ax=ax, edgecolor='black', linewidth=1.5)
gdf_comisarias.plot(ax=ax, edgecolor='white')
plt.title("Verificación 2000 metros")
plt.axis('off')
plt.show()

# %% [markdown]
# ### Crímenes dentro del área
# %%
# generar buffer unificado para evitar conteo múltiple
unified_1000 = geom_1000.union_all()
gdf_uni_1000 = gpd.GeoDataFrame(geometry=[unified_1000], crs=gdf_comisarias.crs)
unified_2000 = geom_2000.union_all()
gdf_uni_2000 = gpd.GeoDataFrame(geometry=[unified_2000], crs=gdf_comisarias.crs)
unified_3000 = geom_3000.union_all()
gdf_uni_3000 = gpd.GeoDataFrame(geometry=[unified_3000], crs=gdf_comisarias.crs)

# contar delitos dentro del área

def count_points_in_buffer(unified_buffer_gdf, points_gdf):
    # puntos dentro del 'gran' buffer
    joined = gpd.sjoin(points_gdf, unified_buffer_gdf, how='inner',
                       predicate='within')
    return(len(joined))

count_1000 = count_points_in_buffer(gdf_uni_1000, gdf_crime)
count_2000 = count_points_in_buffer(gdf_uni_2000, gdf_crime)
count_3000 = count_points_in_buffer(gdf_uni_3000, gdf_crime)

# data resultados
data_res_1 = {
        'Crimenes acumulados' : [count_1000, count_2000, count_3000]
        }

df_results_1 = pd.DataFrame(
        data_res_1,
        index=['0-1000m', '0-2000m', '0-3000m']
        )
# cálculo para los anillos o áreas no traslapadas
ring_1 = df_results_1.iloc[0, 0]
ring_2 = df_results_1.iloc[1, 0] - ring_1
ring_3 = df_results_1.iloc[2, 0] - ring_2

df_results_1['Crimenes anillos'] = [ring_1, ring_2, ring_3]
df_results_1['Anillos'] = ['0 - 1000m', '1000m - 2000m', '2000m - 3000m']
df_results_1 = df_results_1.set_index('Anillos')
# tabla resumen
md_table = df_results_1.to_markdown()
print(df_results_1)


# verificar con 2000 metros
fig, ax = plt.subplots(figsize=(10, 10))
gdf_uni_2000.plot(ax=ax, edgecolor='black', linewidth=1.5)
gdf_comisarias.plot(ax=ax, edgecolor='white')
plt.title("Verificación 2000 metros")
plt.axis('off')
plt.show()

# %% [markdown]
# ### Tabla resumen con las frecuencias de delitos

# %%
display(Markdown(md_table))

# %% [markdown]
# Como se observa en la tabla, la mayor cantidad de delitos sucede
# en el anillo más externo, entre 2000 y 3000 metros.

# %% [markdown]
# ## Instrucción 2

# %% [markdown]
# ### Preparación de datos

# %%
data_VIF = pd.read_csv(PATH_CSV_VIF)
data_censo = pd.read_csv(PATH_CSV_CENSO)
gdf_censo = gpd.read_file(PATH_SHP_CENSO)
print(data_VIF.head())
print(data_censo.head())
print(gdf_censo.head())

data_merge = data_VIF.merge(data_censo, on='zona', how='inner')
gdf_final = gdf_censo.merge(data_merge, left_on='GEOCODIGO',
                            right_on='zona', how='inner')

# construcción de indicadores
gdf_final['tasa_poblacion'] = (gdf_final['VIFmujer'] / gdf_final['poblacion'])* 100000

gdf_final['tasa_adultos'] = (gdf_final['VIFmujer'] / gdf_final['adultos'])* 100000

gdf_final['tasa_mujeres'] = (gdf_final['VIFmujer'] / gdf_final['adultosM'])* 100000

# lidiar con eventual división por cero
gdf_final = gdf_final.replace([float('inf'), -float('-inf')], 0)

# %% [markdown]
# ### Evaluación autocorrelación espacial

# %%
# Moran global

# crear los pesos
w = Queen.from_dataframe(gdf_final)
w.transform = 'r'

# definir columnas para el análisis
rate_columns = ['tasa_poblacion', 'tasa_adultos', 'tasa_mujeres']

moran_results = {}

for col in rate_columns:
    # calcular Moran
    mi = Moran(gdf_final[col], w)
    moran_results[col] = mi

    # revisar significancia al 5%
    is_significant = mi.p_sim < 0.05
    sig_str = "*" if is_significant else "NS."

    print(f"\nMoran global: {col}")
    print(f"Indice (I): {mi.I:.4f}")
    print(f"P-value: {mi.p_sim:.4f} ({sig_str})")

# Moran local
for col in rate_columns:
    lisa = Moran_Local(gdf_final[col], w)

    # plots
    fig, ax = plt.subplots(figsize=(10, 10))
    lisa_cluster(lisa, gdf_final, p=0.05, ax=ax)
    plt.title(f"Clusters (LISA) significativos: {col}")
    plt.show()

# %% [markdown]
# ## Conclusiones
# En primer lugar, observamos que en las 3 construcciones de las tasas se mostró
# un agrupamiento (clustering) significativo. Lo anterior indica que la distribución de la
# violencia intrafamiliar (VIF) se encuentra concentrada en áreas específicas.
# Luego, en el examen individual de las tasas, vemos que la `tasa_adultos`
# presenta el cluster más fuerte, mientras que la `tasa_mujeres` el más débil.
# La caída del índice cuando se computa para la población en riesgo (mujeres),
# versus cuando se computa para la población general, puede estar indicando que
# la VIF podría estar más distribuida de lo esperado. En tanto un índice I alto
# nos indica que existen zonas más delimitadas donde este fenómeno ocurre
# y otras donde no, la reducción del índice I, por otro lado, indica que
# el fenómeno tiene una distribución más aleatoria. Observando los clusters
# locales, en términos generales hay un 'Low-Low' de clusters de baja VIF
# agrupados en la zona noreste (Providencia, Las Condes, Vitacura, etc.).
# Por otro lado, los 'High-High' se tienden a agrupar en las periferias
# sur y oeste. Adicionalmente, en relación a la reducción del Moran global para
# el cálculo de la `tasa_mujeres`, el índice de Moran local nos revela una
# interpretación más directa: existe una clara partición norte/sur
# probablemente relacionada con los niveles de pobreza/riqueza que
# son muy fuertes. Sin embargo, cuando consideramos la `tasa_mujeres`
# se nos revelan 'micro' contextos donde, por ejemplo, en zonas
# suroeste comienzan a desaparecer clusters y aparecen zonas grises
# no significativas. En conclusión, el fenómeno de VIF parece tener una
# clara estructura espacial global; sin embargo, también presenta
# discontinuidades locales que son más evidentes al considerar la
# población de riesgo. Lo anterior podría ser relevante en cómo llevar
# los esfuerzos para la reducción de la VIF: un acercamiento
# jerárquico podría capturar de mejor manera el fenómeno, reportando
# niveles macro-zonales norte-sur en base a una tasa global, y para determinar
# discontinuidades más finas utilizar la `tasa_mujeres`, o bien simplemente
# utilizar la `tasa_mujeres` en tanto es más específica para el fenómeno
# y captura igualmente la clusterización micro y macro.
