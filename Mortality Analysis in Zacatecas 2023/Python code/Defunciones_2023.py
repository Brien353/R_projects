import math
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import matplotlib.ticker as ticker
import textwrap 
import sys
import os


#Definir la ruta del archivo que contiene la base de datos de defunciones
path = r"C:\Users\DELL\Downloads\Defunciones_2023\Defunciones.csv"
#Definir la ruta del archivo que contiene la base de datos de diagnosticos
path_diag = r"C:\Users\DELL\Downloads\Defunciones_2023\Diagnosticos.xlsx"
#Importar la base de datos completa
mortalidad = pd.read_csv(path)
# Filtrando la base de datos por entidad federativa = Zacatecas
mortalidad_zacatecas = mortalidad[mortalidad.ent_regis == 32]
#Filtrando la base de datos por mes de ocurrencia
mortalidad_zacatecas_mes= mortalidad_zacatecas[['ent_regis','mes_ocurr']]
#Contando la cantidad de defunciones por mes
mortalidad_zacatecas_mes = mortalidad_zacatecas_mes.groupby('mes_ocurr').size().reset_index(name='n')
#Eliminando valores que son erroneos o fueron mal capturados
mortalidad_zacatecas_mes = mortalidad_zacatecas_mes[mortalidad_zacatecas_mes.mes_ocurr != 99]
#Cambiando las etiquetas de los meses
meses = {
    1: 'Enero',
    2: 'Febrero',
    3: 'Marzo',
    4: 'Abril',
    5: 'Mayo',
    6: 'Junio',
    7: 'Julio',
    8: 'Agosto',
    9: 'Septiembre',
    10: 'Octubre',
    11: 'Noviembre',
    12: 'Diciembre'
}

mortalidad_zacatecas_mes['mes_ocurr'] = mortalidad_zacatecas_mes['mes_ocurr'].replace(meses)
#Ordenar creciente
mortalidad_zacatecas_mes = mortalidad_zacatecas_mes.sort_values('n',ascending=False)

# Filtrando la base de datos por sexo
mortalidad_zacatecas_sexo = mortalidad_zacatecas[['ent_regis','sexo']]
#Eliminando valores que no estan definidos
mortalidad_zacatecas_sexo= mortalidad_zacatecas_sexo
# Eliminando valores que no son 1 0 2, ya que pueden representar errores de medición.
mortalidad_zacatecas_sexo = mortalidad_zacatecas_sexo[(mortalidad_zacatecas_sexo['sexo'] == 1) | (mortalidad_zacatecas_sexo['sexo'] == 2)]
# Contando la frecuencia de Hombres= sexo 1 y Mujeres = Sexo 2
mortalidad_zacatecas_sexo= mortalidad_zacatecas_sexo. groupby('sexo').size().reset_index(name='n')
#Cambiar las etiquetas
mortalidad_zacatecas_sexo['sexo'] = mortalidad_zacatecas_sexo['sexo'].replace({1: 'Hombre', 2: 'Mujer'})
#Filtrando la base de datos para mostrar la causa de defunción
mortalidad_zacatecas_causa_def = mortalidad_zacatecas [['ent_regis','causa_def']]
#Creando la tabla de frecuencia por causa de defunución
mortalidad_zacatecas_causa_def['n'] = mortalidad_zacatecas_causa_def['causa_def'].map(mortalidad_zacatecas_causa_def['causa_def'].value_counts())
#Agrupando datos por causa de defunción
mortalidad_zacatecas_causa_def = mortalidad_zacatecas_causa_def.groupby('causa_def').agg({'n': 'first'}).reset_index()
#Ordenando las columnas de manera creciente
mortalidad_zacatecas_causa_def = mortalidad_zacatecas_causa_def.sort_values('n',ascending=False)
#Traduciendo cada causa de defunción en lenguaje comprensible
diagnosticos = pd.read_excel(path_diag)
#Haciendo un inner join de las tablas en los valores en los que coinciden.
Inner_join = pd.merge(mortalidad_zacatecas_causa_def, diagnosticos, left_on='causa_def', right_on='CATALOG_KEY',how='inner')
#Extrayendo solo las columnas de interés de la unión de ambas tablas en los valores en los que coinciden
mortalidad_zacatecas_causa_def = Inner_join[['NOMBRE','n']]
#Extrayendo las diez primeras causas de mortalidad en Zacatecas
principales_causas_de_mortalidad = mortalidad_zacatecas_causa_def[0:10]
#Extrayendo las siguientes 10 causas de mortalidad en Zacatecas
principales_causas_de_mortalidad_second = mortalidad_zacatecas_causa_def[10:20]
#Extrayendo las siguientes 10 causas de mortalidad en Zacatecas
principales_causas_de_mortalidad_third = mortalidad_zacatecas_causa_def[20:30]
#Extrayendo las causas de mortalidad menos prevalentes en Zacatecas
menos_prevalentes_causas_de_mortalidad = mortalidad_zacatecas_causa_def[::-1][0:10]

#Graficando las causas de mortalidad más comúnes en Zacatecas junto con su frecuencia.

#Definiendo un tema para la gráfica

# Definir el tema
sns.set_theme(style="whitegrid", context="notebook")
# Definir el tamaño de la figura
plt.figure(figsize=(14, 10))  # Adjust the figure size for better proportions

# Partir el texto para mejor lectura
principales_causas_de_mortalidad['wrapped_names'] = principales_causas_de_mortalidad['NOMBRE'].apply(lambda x: '\n'.join(textwrap.wrap(x, width=35)))

# Voltear las etiquetas para que sea más fácil la legibilidad
sns.barplot(y='wrapped_names', x='n', data=principales_causas_de_mortalidad)

# Etiquetas y títulos
plt.ylabel('Nombre del padecimiento', fontsize=10)
plt.xlabel('Cantidad de casos registrados', fontsize=10)
plt.title('Principales causas de muerte en Zacatecas', fontsize=16)

#Dar vuelta en el eje x
plt.xticks(rotation=45, ha='right', fontsize=12)

# Ajustar fondo para que no se traslape nada
plt.tight_layout()

# Show the plot
plt.show()

#Graficando las siguientes 10 causas de muerte más comúnes en Zacatecas.

# Definir el tema
sns.set_theme(style="whitegrid", context="notebook")
# Definir el tamaño de la figura
plt.figure(figsize=(14, 10))  # Adjust the figure size for better proportions

# Partir el texto para mejor lectura
principales_causas_de_mortalidad_second['wrapped_names'] = principales_causas_de_mortalidad_second['NOMBRE'].apply(lambda x: '\n'.join(textwrap.wrap(x, width=35)))

# Voltear las etiquetas para que sea más fácil la legibilidad
sns.barplot(y='wrapped_names', x='n', data=principales_causas_de_mortalidad_second)

# Etiquetas y títulos
plt.ylabel('Nombre del padecimiento', fontsize=10)
plt.xlabel('Cantidad de casos registrados', fontsize=10)
plt.title('Causas de muerte en Zacatecas: Posiciones 11 a 20', fontsize=16)

#Dar vuelta en el eje x
plt.xticks(rotation=45, ha='right', fontsize=12)

# Ajustar fondo para que no se traslape nada
plt.tight_layout()

# Show the plot
plt.show()

#Graficando las siguientes 10 causas de muerte en Zacatecas

# Definir el tema
sns.set_theme(style="whitegrid", context="notebook")
# Definir el tamaño de la figura
plt.figure(figsize=(14, 10))  # Adjust the figure size for better proportions

# Partir el texto para mejor lectura
principales_causas_de_mortalidad_third['wrapped_names'] = principales_causas_de_mortalidad_third['NOMBRE'].apply(lambda x: '\n'.join(textwrap.wrap(x, width=35)))

# Voltear las etiquetas para que sea más fácil la legibilidad
sns.barplot(y='wrapped_names', x='n', data=principales_causas_de_mortalidad_third)

# Etiquetas y títulos
plt.ylabel('Nombre del padecimiento', fontsize=10)
plt.xlabel('Cantidad de casos registrados', fontsize=10)
plt.title('Causas de muerte en Zacatecas: Posiciones 21 a 30', fontsize=16)

#Dar vuelta en el eje x
plt.xticks(rotation=45, ha='right', fontsize=12)

# Ajustar fondo para que no se traslape nada
plt.tight_layout()

# Show the plot
plt.show()


#Graficando las causas de mortalidad más raras en Zacatecas junto con su frecuencia.

# Definir el tema
sns.set_theme(style="whitegrid", context="notebook")
# Definir el tamaño de la figura
plt.figure(figsize=(14, 10))  # Adjust the figure size for better proportions

# Partir el texto para mejor lectura
menos_prevalentes_causas_de_mortalidad['wrapped_names'] = menos_prevalentes_causas_de_mortalidad['NOMBRE'].apply(lambda x: '\n'.join(textwrap.wrap(x, width=35)))

# Voltear las etiquetas para que sea más fácil la legibilidad
sns.barplot(y='wrapped_names', x='n', data=menos_prevalentes_causas_de_mortalidad)

# Etiquetas y títulos
plt.ylabel('Nombre del padecimiento', fontsize=10)
plt.xlabel('Cantidad de casos registrados', fontsize=10)
plt.title('Las 10 Causas Menos Frecuentes de Mortalidad en Zacatecas', fontsize=16)

#Dar vuelta en el eje x
plt.xticks(rotation=45, ha='right', fontsize=12)

# Ajustar fondo para que no se traslape nada
plt.tight_layout()

# Show the plot
plt.show()

#Distribución de Defunciones por Sexo en Zacatecas 2023
color_palette = {'Hombre': 'blue', 'Mujer': 'pink'}

# Adjust the size of the figure
plt.figure(figsize=(10, 8))

# Create the barplot and apply the custom color palette
sns.barplot(data=mortalidad_zacatecas_sexo, x='sexo', y='n', palette=color_palette)

# Set the labels and title
plt.xlabel('Sexo')
plt.ylabel('Número de defunciones')
plt.title('Distribución de Defunciones por Sexo en Zacatecas 2023')

# Display the plot
plt.show()

#Distribución de Defunciones por mes en Zacatecas 2023
# Paleta de colores para los meses (usando nombres de meses)

# Paleta de colores para los meses (usando nombres de meses)
mes_color_palette = {
    'Enero': 'lightblue',
    'Febrero': 'lightgreen',
    'Marzo': 'lightcoral',
    'Abril': 'lightskyblue',
    'Mayo': 'lightgoldenrodyellow',
    'Junio': 'lightpink',
    'Julio': 'lightseagreen',
    'Agosto': 'lightyellow',
    'Septiembre': 'lightsteelblue',
    'Octubre': 'lightgray',
    'Noviembre': 'lightcyan',
    'Diciembre': 'lightpink'
}

# Crear gráfico de barras
plt.figure(figsize=(10, 6))
sns.barplot(x='mes_ocurr', y='n', data=mortalidad_zacatecas_mes, palette=mes_color_palette)

# Calcular el promedio mensual de defunciones
promedio_mensual = mortalidad_zacatecas_mes['n'].mean()

# Dibujar una línea horizontal con el valor del promedio mensual
plt.axhline(y=promedio_mensual, color='red', linestyle='--', label=f'Promedio mensual de defunciones: {promedio_mensual:.2f}')

# Personalizar título y etiquetas
plt.xlabel('Mes')
plt.ylabel('Número de defunciones')
plt.title('Distribución de defunciones por mes')

# Mostrar la leyenda para la línea del promedio
plt.legend()

# Mostrar el gráfico
plt.show()

