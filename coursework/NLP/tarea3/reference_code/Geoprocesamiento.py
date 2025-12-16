# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.18.1
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# %%
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
# %matplotlib inline
import geopandas as gpd
from shapely.geometry import Point
from shapely.geometry import Polygon
from geopandas.tools import overlay
import scipy.stats as stats
import seaborn as sns
import pysal
import os

# Ruta de trabajo
os.getcwd()
os.listdir(os.getcwd())

# %% [markdown]
# Cuando trabajemos con dos o más coberturas, es un requisito que tengan el mismo sistema de referencia. Estas coberturas, deben tener al menos el mismo datum cuando se requiera realizar una visualización o un geoprocesamiento que no implique el calculo de distancias. En este ultimo caso, es se suma importancia que estén proyectadas, pues son un requisito para los geoprocesamientos, sobretodo si en ellos se estiman en unidades del sistema métrico (metros, kilometros).
#
# Ante esto, en cada proceso o análisis, lo primero a evaluar y corregir, de ser necesario, siempre es el sistema de referencia de las coberturas con las cuales se va a trabajar.

# %% [markdown]
# ## 1. Herramientas de superposición: intersección  
#

# %% [markdown]
# La intersección corresponde a una intersección geométrica de capas de entrada, de cualquier tipo de geometría, siendo el resultado una nueva capa que contiene solo los elementos o partes de elementos que intersecan todas las capas de entrada y sus atributos.
#
# En este geoprocesamiento, al combinar capas que poseen elementos con distinta geometría, la geometría de la capa resultante será por defecto la misma de la capa de entrada con menor dimensión.
#
# Para este ejemplo, trabajaremos con las coberturas de las unidades vecinales proporcionadas por el INE. Estas son:
#
# 1. Superficie de áreas verdes (polígonos)
# 2. Equipamientos (puntos)
# 3. Unidades vecinales (polígonos)
#
# Como verá, las tres coberturas tienen el mismo sistema de referencia, SIRGAS 2000, por lo que se hace necesario transformarlo y proyectarlo.
#
# Al revisar las tablas de atributos de cada capa vemos que:
#
# 1. La superficie de áreas verdes solo muestra la superficie total de un polígono, que no está asociado a una unidad vecinal.
# 2. El tipo de equipamiento, representado por un punto, que no está asociado a una unidad vecinal.
# 3. La unidad vecinal (nombre, comuna y región, más otros atributos).
#    
# Ante esto, intersectaremos las coberturas, por separado, de la superficie de pareas verdes y equipamientos con las unidades vecinales, con el propósito de agregarles la información referida a la unidad vecinal en a cual se ubican.

# %%
areas = gpd.read_file('shapes/Areas_Verdes_Unidades_Vecinales.shp')
areas.head()
#areas.plot(facecolor='white', edgecolor='black')

# %%
equipamiento = gpd.read_file('shapes/Equpamiento_Unidades_Vecinales.shp')
equipamiento.head()
#equipamiento.plot(facecolor='white', edgecolor='black', markersize=5)

# %%
uv = gpd.read_file('shapes/Unidades_Vecinales.shp')
uv.head()
#uv.plot(facecolor='white', edgecolor='black')

# %% [markdown]
# Ahora revisamos y cambiamos y proyectamos el sistema de referencia de las tres capas a WGS 1984, UTM 19S.

# %%
areas.crs
equipamiento.crs
uv.crs

areas = areas.to_crs(32719)
equipamiento = equipamiento.to_crs(32719)
uv = uv.to_crs(32719)

areas.crs
equipamiento.crs
uv.crs

# %% [markdown]
# Para facilitar el trabajo y visualización del resultado, tanto en el mapa como en la tabla de atributos, la intersección de las áreas verdes y del equipamiento lo realizaremos en dos comunas: Puente Alto y Macul. Para esto, primero debemos generar, a partir de la cobertura de unidades vecinales, los dos objetos que almacenes dichas comunas. 

# %%
uv_puente = uv[(uv['NOMBRE_COM'] == 'PUENTE ALTO')]
uv_puente.plot(facecolor='white', edgecolor='black')
#uv_puente.head()

# %%
uv_concepcion = uv[(uv['NOMBRE_COM'] == 'PUDAHUEL')]
uv_concepcion.plot(facecolor='white', edgecolor='black')
#uv_concepcion.head()

# %% [markdown]
# La inersección de ambas coberturas la realizaremos con la función ‘overlay’, en cuyo argumento consideraremos el parámetro ‘how='intersection'’ con las coberturas que queremos intersectar. En este ejemplo, intersectaremos las áreas verdes de la comuna de Puente Alto con las unidades vecinales.
#
# Lo importante en este geoprocesamiento, es la tabla de atributos resultantes, pues en ella se unieron ambas coberturas. Observe y analice la tabla de atributos resultante, la cual ahora tiene el área verde y la información de la unidad vecinal en la cual se ubican.

# %%
areas_puente = gpd.overlay(areas, uv_puente, how='intersection')
areas_puente.head()
areas_puente.plot(facecolor='white', edgecolor='black')

# %%
areas_puente.head()

# %%
equip_concepcion = gpd.overlay(equipamiento, uv_concepcion, how='intersection')
equip_concepcion.head()
equip_concepcion.plot(facecolor='white', edgecolor='black', markersize=5)

# %%
equip_concepcion.head()

# %% [markdown]
# Al igual que el caso anterior, la tabla de atributos resultante ahora contiene el equipamiento con la información de la unidad vecinal en la cual se ubican.

# %% [markdown]
# ## 2. Funciones de distancia: Buffers
#
# Este geoprocesamiento genera área cuyos puntos se encuentran a una distancia menor o igual a aquella que se especifique respecto a otro elemento. El resultado son polígonos que rodean a los objetos sobre los que se realiza este proceso.
#
# En un primer ejemplo, crearemos buffers o áreas de incidencia alrededor de los cuarteles de Carabineros de la ciudad de Santiago a una distancia de 1.000 metros.
#
# Si bien solo necesitamos la cobertura de los cuarteles para crear los corredores también consideraremos la de comunas para poder observarlos en su posición comunal.
#
# Para este proceso ambas coberturas difieren en sus sistemas de referencia, por lo que se hace necesario igualarlos. Los cuarteles están en WGS 1984 y las comunas en SIRGAS 2000, ambas sin proyección. 

# %%
cuarteles  = gpd.read_file('shapes/cuartelesGS.shp')
santiago = gpd.read_file('shapes/ComunasGS.shp')

# %%
cuarteles.crs

# %%
santiago.crs

# %%
cuarteles = cuarteles.to_crs(32719)
santiago =santiago.to_crs(32719)

cuarteles.crs
santiago.crs

# %%
cuarteles['TIPO_DE_UN'].value_counts()

# %%
stgo = santiago.plot(facecolor='white', edgecolor='black')
ax = cuarteles.plot(ax=stgo, color='red', markersize=5)
ax.set_axis_off()

# %% [markdown]
# Los buffers los crearemos con la función ‘geometry.buffer’, en cuyo argumento consideraremos la distancia a la cual se desea crear.

# %%
cuarteles_buf = cuarteles.geometry.buffer(1000)

# %%
cuarteles_buf.plot(facecolor='white', edgecolor='black')

# %%
stgo = santiago.plot(facecolor='white', edgecolor='black')
ax = cuarteles_buf.plot(ax=stgo, facecolor='green', edgecolor='black')
ax = cuarteles.plot(ax=stgo, color='red', markersize=5)
ax.set_axis_off()

# %% [markdown]
# Como segundo ejemplo, crearemos buffers o áreas de incidencia alrededor de las líneas férreas de la región de O'Higgins a una distancia de 3.000 metros.
#
# Si bien solo necesitamos la cobertura de las líneas férreas para crear los corredores también consideraremos la de comunas para poder observarlos en su posición comunal.
#
# Para este proceso ambas coberturas no tienen un sistema de referencia, por lo que debemos creárselos (Averigüe cómo).

# %%
comunas06  = gpd.read_file('shapes/lim_admi_06.shp')
ferreas06 = gpd.read_file('shapes/ferreas06.shp')

# %%
comunas06.crs

# %%
ferreas06.crs

# %%
r06 = comunas06.plot(facecolor='white', edgecolor='black')
ax = ferreas06.plot(ax=r06, color='red')
ax.set_axis_off()

# %% [markdown]
# Creamos el buffer de 3.000 metros alrededor de las líneas férreas.

# %%
ferreas06_buf = ferreas06.geometry.buffer(3000)

# %%
r06 = comunas06.plot(facecolor='white', edgecolor='black')
ax = ferreas06_buf.plot(ax=r06, facecolor='red')
ax = ferreas06.plot(ax=r06, color='yellow', markersize=5)
ax.set_axis_off()
