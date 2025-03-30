#Analizando la incidencia delictiva de fuero común por entidad federativa en los meses de Enero y Febrero de 2025.

#Instalando y cargando los paquetes necesarios para poder hacer el análisis
install.packages("sqldf")
library(sqldf)
library(ggplot2)
library(psych)
library(dplyr)
library(readxl)
library(readr)


#Entendiendo cual es la codificación de la base de datos presentada
guess_encoding("C:/Users/DELL/Downloads/ID.csv")

#Importando la base de datos completa de datos abiertos proporcionada por el gobierno de mexico
base_de_datos <- read.csv("C:/Users/DELL/Downloads/ID.csv", fileEncoding = "ISO-8859-1")

#Filtrando por año los datos contenidos en la base de datos
 base_de_datos_2025 <- base_de_datos[base_de_datos$Año==2025, ]
 
 #########################################Delito de homicidio doloso###################################################################################
 #Filtrando por delito, con interés en homicidio
 base_de_datos_2025_homicidio <- base_de_datos_2025[base_de_datos_2025$Tipo.de.delito=='Homicidio', ]
 
 #Filtrando por subtipo de delito con interés en homicidio doloso
 base_de_datos_2025_homicidio_doloso <- base_de_datos_2025_homicidio[base_de_datos_2025_homicidio$Subtipo.de.delito=='Homicidio doloso',0:9]
 
 #Contando el total de homicidios dolosos registrados en Enero a nivel nacional
 Total_de_homicidios_dolosos_enero <- sum(base_de_datos_2025_homicidio_doloso$Enero)
 #Contando el total de homicidios dolosos registrados en Febrero a nivel nacional
 Total_de_homicidios_dolosos_febrero <- sum(base_de_datos_2025_homicidio_doloso$Febrero)
 
 #Registrando para cada entidad federativa el total de homicidios dolosos ocurridos en los meses de enero y febrero de 2025
 #El total de homicidios por entidad federativa se registra en la columna conteo_total_enero y conteo_total_febrero, respectivamente.
 Total_de_homicidios_dolosos_por_entidad <- sqldf("
  SELECT Entidad,
         SUM(Enero) AS conteo_total_enero, 
         SUM(Febrero) AS conteo_total_febrero
  FROM base_de_datos_2025_homicidio_doloso
  GROUP BY Entidad
")
 
 #Para cada entidad federativa:
 #Conociendo que total_de_homicidios_dolosos_enero=1988, queremos calcular cual es el porcentaje con respecto al total de homicidios dolosos en la Republica que representa cada estado.
 #Usamos la regla de tres simple, para saber que %homicidios_enero_estado= #homicidios_dolosos_estado_enero*100/total_de_homicidios_dolosos_enero.
 #Similarmente en Febrero, usamos un razonamiento análogo para obtener una nueva tabla
 
 Porcentaje_de_homicidios_dolosos_por_entidad <- sqldf("SELECT Entidad, conteo_total_enero, conteo_total_febrero, 
                           ROUND((conteo_total_enero*100)/(1988.0),2) AS porcentaje_enero, 
                           ROUND((conteo_total_febrero*100)/(1747.0),2) AS porcentaje_febrero
                    FROM Total_de_homicidios_dolosos_por_entidad")
 
 #Calculando la media nacional de homicidios dolosos para el mes de Enero
 media_nacional_enero <- mean(Porcentaje_de_homicidios_dolosos_por_entidad$conteo_total_enero)
 
 #Calculando la media nacional de homicidios dolosos para el mes de Febrero
 media_nacional_febrero <- mean(Porcentaje_de_homicidios_dolosos_por_entidad$conteo_total_febrero)
 
 # Ordenando las etiquetas en función de la cantidad de homicidios dolosos ocurridos en cada entidad federativa en orden decreciente
 Porcentaje_de_homicidios_dolosos_por_entidad$Entidad <- factor(Porcentaje_de_homicidios_dolosos_por_entidad$Entidad, levels = Porcentaje_de_homicidios_dolosos_por_entidad$Entidad[order(Porcentaje_de_homicidios_dolosos_por_entidad$conteo_total_enero, decreasing = TRUE)])
 
 #Graficando la cantidad de homicidios dolosos por entidad federativa en orden decreciente, junto con el promedio nacional así como en cada etiqueta el porcentaje que representa en el mes de Enero
 ggplot(Porcentaje_de_homicidios_dolosos_por_entidad, aes(x = Entidad, y = conteo_total_enero, fill = Entidad)) +
   geom_bar(stat = "identity", show.legend = FALSE) +  # Eliminar leyenda
   coord_flip() +
   geom_text(aes(label = paste(porcentaje_enero, "%")), 
             hjust = -0.2, color = "black", size = 4, fontface = "bold") +  # Etiquetas en barras
   geom_hline(yintercept = media_nacional_enero, linetype = "dashed", color = "red", size = 1.5) +  # Línea de la media nacional
   labs(title = "Cantidad de Homicidios Dolosos por Entidad Federativa - Enero", 
        x = "Entidad Federativa", y = "Cantidad de Homicidios Dolosos") +
   theme_minimal() +
   theme(
     plot.title = element_text(size = 16, face = "bold", color = "darkblue", hjust = 0.5),  # Título centrado y en negrita
     axis.title = element_text(size = 12, face = "bold", color = "black"),
     axis.text = element_text(size = 10, color = "black"),  # Tamaño de texto en los ejes
     panel.grid.major = element_blank(),  # Eliminar las líneas de la cuadrícula mayor
     panel.grid.minor = element_blank(),  # Eliminar las líneas de la cuadrícula menor
     plot.margin = margin(1, 1, 2, 1, "cm")  # Añadir más margen en la parte superior
   ) +
   annotate("text", 
            x = 10, 
            y = media_nacional_enero + 100,  # Ajustar la posición de la etiqueta de la media
            label = paste("Media Nacional: ", round(media_nacional_enero, 0)), 
            color = "red", size = 5, fontface = "bold") +
   scale_y_continuous(limits = c(0, max(Porcentaje_de_homicidios_dolosos_por_entidad$conteo_total_enero) * 1.1))
 
 
 
 #Ordenando las etiquetas en función de la cantidad de homicidios dolosos ocurridos en cada entidad federativa en orden decreciente en Febrero
 Porcentaje_de_homicidios_dolosos_por_entidad$Entidad <- factor(
   Porcentaje_de_homicidios_dolosos_por_entidad$Entidad,
   levels = Porcentaje_de_homicidios_dolosos_por_entidad$Entidad[order(Porcentaje_de_homicidios_dolosos_por_entidad$conteo_total_febrero, decreasing = TRUE)]
 )
 
 
 
 #Grafica de la cantidad de homicidios dolosos por entidad federativa en el mes de febrero
 
 ggplot(Porcentaje_de_homicidios_dolosos_por_entidad, aes(x = Entidad, y = conteo_total_febrero, fill = Entidad)) +
   geom_bar(stat = "identity", show.legend = FALSE) +  # Eliminar leyenda
   coord_flip() +
   geom_text(aes(label = paste(porcentaje_febrero, "%")), 
             hjust = -0.2, color = "black", size = 4, fontface = "bold") +  # Etiquetas en barras
   geom_hline(yintercept = media_nacional_febrero, linetype = "dashed", color = "red", size = 1.5) +  # Línea de la media nacional
   labs(title = "Cantidad de Homicidios Dolosos por Entidad Federativa - Febrero", 
        x = "Entidad Federativa", y = "Cantidad de Homicidios Dolosos") +
   theme_minimal() +
   theme(
     plot.title = element_text(size = 16, face = "bold", color = "darkblue", hjust = 0.5),  # Título centrado y en negrita
     axis.title = element_text(size = 12, face = "bold", color = "black"),
     axis.text = element_text(size = 10, color = "black"),  # Tamaño de texto en los ejes
     panel.grid.major = element_blank(),  # Eliminar las líneas de la cuadrícula mayor
     panel.grid.minor = element_blank(),  # Eliminar las líneas de la cuadrícula menor
     plot.margin = margin(1, 1, 2, 1, "cm")  # Añadir más margen en la parte superior
   ) +
   annotate("text", 
            x = 10, 
            y = media_nacional_febrero + 100,  # Ajustar la posición de la etiqueta de la media
            label = paste("Media Nacional: ", round(media_nacional_febrero, 0)), 
            color = "red", size = 5, fontface = "bold") +
   scale_y_continuous(limits = c(0, max(Porcentaje_de_homicidios_dolosos_por_entidad$conteo_total_febrero) * 1.1))
 
 #####################################################Delito de feminicidio#########################################################################
 # Filtrando por delito, con interés en feminicidio
 base_de_datos_2025_feminicidio <- base_de_datos_2025[base_de_datos_2025$Tipo.de.delito == 'Feminicidio', ]
 
 # Filtrando por subtipo de delito con interés en feminicidio
 base_de_datos_2025_feminicidio_feminicidio <- base_de_datos_2025_feminicidio[base_de_datos_2025_feminicidio$Subtipo.de.delito == 'Feminicidio', 0:9]
 
 # Contando el total de feminicidios registrados en Enero a nivel nacional
 Total_de_feminicidios_enero <- sum(base_de_datos_2025_feminicidio_feminicidio$Enero)
 
 # Contando el total de feminicidios registrados en Febrero a nivel nacional
 Total_de_feminicidios_febrero <- sum(base_de_datos_2025_feminicidio_feminicidio$Febrero)
 
 # Registrando para cada entidad federativa el total de feminicidios ocurridos en los meses de enero y febrero de 2025
 # El total de feminicidios por entidad federativa se registra en la columna conteo_total_enero y conteo_total_febrero, respectivamente.
 Total_de_feminicidios_por_entidad <- sqldf("
  SELECT Entidad,
         SUM(Enero) AS conteo_total_enero, 
         SUM(Febrero) AS conteo_total_febrero
  FROM base_de_datos_2025_feminicidio_feminicidio
  GROUP BY Entidad
")
 
 # Para cada entidad federativa:
 # Calculando el porcentaje con respecto al total de feminicidios en la República
 Porcentaje_de_feminicidios_por_entidad <- sqldf("
  SELECT Entidad, conteo_total_enero, conteo_total_febrero, 
         ROUND((conteo_total_enero*100)/(53.0),2) AS porcentaje_enero, 
         ROUND((conteo_total_febrero*100)/(45.0),2) AS porcentaje_febrero
  FROM Total_de_feminicidios_por_entidad
")
 
 # Calculando la media nacional de feminicidios para el mes de Enero
 media_nacional_feminicidio_enero <- mean(Porcentaje_de_feminicidios_por_entidad$conteo_total_enero)
 
 # Calculando la media nacional de feminicidios para el mes de Febrero
 media_nacional_feminicidio_febrero <- mean(Porcentaje_de_feminicidios_por_entidad$conteo_total_febrero)
 
 # Ordenando las etiquetas en función de la cantidad de feminicidios ocurridos en cada entidad federativa en orden decreciente para Enero
 Porcentaje_de_feminicidios_por_entidad$Entidad <- factor(Porcentaje_de_feminicidios_por_entidad$Entidad, levels = Porcentaje_de_feminicidios_por_entidad$Entidad[order(Porcentaje_de_feminicidios_por_entidad$conteo_total_enero, decreasing = TRUE)])
 
 # Graficando la cantidad de feminicidios por entidad federativa en orden decreciente, junto con el promedio nacional, así como en cada etiqueta el porcentaje que representa en el mes de Enero
 ggplot(Porcentaje_de_feminicidios_por_entidad, aes(x = Entidad, y = conteo_total_enero, fill = Entidad)) +
   geom_bar(stat = "identity", show.legend = FALSE) +  # Eliminar leyenda
   coord_flip() +
   geom_text(aes(label = paste(porcentaje_enero, "%")), 
             hjust = -0.2, color = "black", size = 4, fontface = "bold") +  # Etiquetas en barras
   geom_hline(yintercept = media_nacional_feminicidio_enero, linetype = "dashed", color = "red", size = 1.5) +  # Línea de la media nacional
   labs(title = "Cantidad de Feminicidios por Entidad Federativa - Enero", 
        x = "Entidad Federativa", y = "Cantidad de Feminicidios") +
   theme_minimal() +
   theme(
     plot.title = element_text(size = 16, face = "bold", color = "darkblue", hjust = 0.5),  # Título centrado y en negrita
     axis.title = element_text(size = 12, face = "bold", color = "black"),
     axis.text = element_text(size = 10, color = "black"),  # Tamaño de texto en los ejes
     panel.grid.major = element_blank(),  # Eliminar las líneas de la cuadrícula mayor
     panel.grid.minor = element_blank(),  # Eliminar las líneas de la cuadrícula menor
     plot.margin = margin(1, 1, 2, 1, "cm")  # Añadir más margen en la parte superior
   ) +
   annotate("text", 
            x = 10, 
            y = media_nacional_feminicidio_enero + 100,  # Ajustar la posición de la etiqueta de la media
            label = paste("Media Nacional: ", round(media_nacional_feminicidio_enero, 0)), 
            color = "red", size = 5, fontface = "bold") +
   scale_y_continuous(limits = c(0, max(Porcentaje_de_feminicidios_por_entidad$conteo_total_enero) * 1.1))
 
 # Ordenando las etiquetas en función de la cantidad de feminicidios ocurridos en cada entidad federativa en orden decreciente en Febrero
 Porcentaje_de_feminicidios_por_entidad$Entidad <- factor(
   Porcentaje_de_feminicidios_por_entidad$Entidad,
   levels = Porcentaje_de_feminicidios_por_entidad$Entidad[order(Porcentaje_de_feminicidios_por_entidad$conteo_total_febrero, decreasing = TRUE)]
 )
 
 # Graficando la cantidad de feminicidios por entidad federativa en el mes de febrero
 ggplot(Porcentaje_de_feminicidios_por_entidad, aes(x = Entidad, y = conteo_total_febrero, fill = Entidad)) +
   geom_bar(stat = "identity", show.legend = FALSE) +  # Eliminar leyenda
   coord_flip() +
   geom_text(aes(label = paste(porcentaje_febrero, "%")), 
             hjust = -0.2, color = "black", size = 4, fontface = "bold") +  # Etiquetas en barras
   geom_hline(yintercept = media_nacional_feminicidio_febrero, linetype = "dashed", color = "red", size = 1.5) +  # Línea de la media nacional
   labs(title = "Cantidad de Feminicidios por Entidad Federativa - Febrero", 
        x = "Entidad Federativa", y = "Cantidad de Feminicidios") +
   theme_minimal() +
   theme(
     plot.title = element_text(size = 16, face = "bold", color = "darkblue", hjust = 0.5),  # Título centrado y en negrita
     axis.title = element_text(size = 12, face = "bold", color = "black"),
     axis.text = element_text(size = 10, color = "black"),  # Tamaño de texto en los ejes
     panel.grid.major = element_blank(),  # Eliminar las líneas de la cuadrícula mayor
     panel.grid.minor = element_blank(),  # Eliminar las líneas de la cuadrícula menor
     plot.margin = margin(1, 1, 2, 1, "cm")  # Añadir más margen en la parte superior
   ) +
   annotate("text", 
            x = 10, 
            y = media_nacional_feminicidio_febrero + 100,  # Ajustar la posición de la etiqueta de la media
            label = paste("Media Nacional: ", round(media_nacional_feminicidio_febrero, 0)), 
            color = "red", size = 5, fontface = "bold") +
   scale_y_continuous(limits = c(0, max(Porcentaje_de_feminicidios_por_entidad$conteo_total_febrero) * 1.1))
 
 ######################Abuso sexual#################################################################################
 
 # Filtrando por delito, con interés en abuso sexual
 base_de_datos_2025_abuso_sexual <- base_de_datos_2025[base_de_datos_2025$Tipo.de.delito == 'Abuso sexual', ]
 
 # Filtrando por subtipo de delito con interés en abuso sexual
 base_de_datos_2025_abuso_sexual_abuso_sexual <- base_de_datos_2025_abuso_sexual[base_de_datos_2025_abuso_sexual$Subtipo.de.delito == 'Abuso sexual', 0:9]
 
 # Contando el total de abuso sexual registrado en Enero a nivel nacional
 Total_de_abuso_sexual_enero <- sum(base_de_datos_2025_abuso_sexual_abuso_sexual$Enero)
 
 # Contando el total de abuso sexual registrado en Febrero a nivel nacional
 Total_de_abuso_sexual_febrero <- sum(base_de_datos_2025_abuso_sexual_abuso_sexual$Febrero)
 
 # Registrando para cada entidad federativa el total de abuso sexual ocurrido en los meses de enero y febrero de 2025
 # El total de abuso sexual por entidad federativa se registra en la columna conteo_total_enero y conteo_total_febrero, respectivamente.
 Total_de_abuso_sexual_por_entidad <- sqldf("
  SELECT Entidad,
         SUM(Enero) AS conteo_total_enero, 
         SUM(Febrero) AS conteo_total_febrero
  FROM base_de_datos_2025_abuso_sexual_abuso_sexual
  GROUP BY Entidad
")
 
 # Calculando el porcentaje de abuso sexual respecto al total de abuso sexual a nivel nacional
 Porcentaje_de_abuso_sexual_por_entidad <- sqldf("
  SELECT Entidad, conteo_total_enero, conteo_total_febrero, 
         ROUND((conteo_total_enero*100)/(2413.0),2) AS porcentaje_enero, 
         ROUND((conteo_total_febrero*100)/(2620.0),2) AS porcentaje_febrero
  FROM Total_de_abuso_sexual_por_entidad
")
 
 # Calculando la media nacional de abuso sexual para el mes de Enero
 media_nacional_abuso_sexual_enero <- mean(Porcentaje_de_abuso_sexual_por_entidad$conteo_total_enero)
 
 # Calculando la media nacional de abuso sexual para el mes de Febrero
 media_nacional_abuso_sexual_febrero <- mean(Porcentaje_de_abuso_sexual_por_entidad$conteo_total_febrero)
 
 # Ordenando las etiquetas en función de la cantidad de abuso sexual ocurrido en cada entidad federativa en orden decreciente para Enero
 Porcentaje_de_abuso_sexual_por_entidad$Entidad <- factor(Porcentaje_de_abuso_sexual_por_entidad$Entidad, levels = Porcentaje_de_abuso_sexual_por_entidad$Entidad[order(Porcentaje_de_abuso_sexual_por_entidad$conteo_total_enero, decreasing = TRUE)])
 
 # Graficando la cantidad de abuso sexual por entidad federativa en orden decreciente, junto con el promedio nacional, así como en cada etiqueta el porcentaje que representa en el mes de Enero
 ggplot(Porcentaje_de_abuso_sexual_por_entidad, aes(x = Entidad, y = conteo_total_enero, fill = Entidad)) +
   geom_bar(stat = "identity", show.legend = FALSE) +  # Eliminar leyenda
   coord_flip() +
   geom_text(aes(label = paste(porcentaje_enero, "%")), 
             hjust = -0.2, color = "black", size = 4, fontface = "bold") +  # Etiquetas en barras
   geom_hline(yintercept = media_nacional_abuso_sexual_enero, linetype = "dashed", color = "red", size = 1.5) +  # Línea de la media nacional
   labs(title = "Cantidad de Abuso Sexual por Entidad Federativa - Enero", 
        x = "Entidad Federativa", y = "Cantidad de Abuso Sexual") +
   theme_minimal() +
   theme(
     plot.title = element_text(size = 16, face = "bold", color = "darkblue", hjust = 0.5),  # Título centrado y en negrita
     axis.title = element_text(size = 12, face = "bold", color = "black"),
     axis.text = element_text(size = 10, color = "black"),  # Tamaño de texto en los ejes
     panel.grid.major = element_blank(),  # Eliminar las líneas de la cuadrícula mayor
     panel.grid.minor = element_blank(),  # Eliminar las líneas de la cuadrícula menor
     plot.margin = margin(1, 1, 2, 1, "cm")  # Añadir más margen en la parte superior
   ) +
   annotate("text", 
            x = 10, 
            y = media_nacional_abuso_sexual_enero + 100,  # Ajustar la posición de la etiqueta de la media
            label = paste("Media Nacional: ", round(media_nacional_abuso_sexual_enero, 0)), 
            color = "red", size = 5, fontface = "bold") +
   scale_y_continuous(limits = c(0, max(Porcentaje_de_abuso_sexual_por_entidad$conteo_total_enero) * 1.1))
 
 # Ordenando las etiquetas en función de la cantidad de abuso sexual ocurrido en cada entidad federativa en orden decreciente en Febrero
 Porcentaje_de_abuso_sexual_por_entidad$Entidad <- factor(
   Porcentaje_de_abuso_sexual_por_entidad$Entidad,
   levels = Porcentaje_de_abuso_sexual_por_entidad$Entidad[order(Porcentaje_de_abuso_sexual_por_entidad$conteo_total_febrero, decreasing = TRUE)]
 )
 
 # Graficando la cantidad de abuso sexual por entidad federativa en el mes de febrero
 ggplot(Porcentaje_de_abuso_sexual_por_entidad, aes(x = Entidad, y = conteo_total_febrero, fill = Entidad)) +
   geom_bar(stat = "identity", show.legend = FALSE) +  # Eliminar leyenda
   coord_flip() +
   geom_text(aes(label = paste(porcentaje_febrero, "%")), 
             hjust = -0.2, color = "black", size = 4, fontface = "bold") +  # Etiquetas en barras
   geom_hline(yintercept = media_nacional_abuso_sexual_febrero, linetype = "dashed", color = "red", size = 1.5) +  # Línea de la media nacional
   labs(title = "Cantidad de Abuso Sexual por Entidad Federativa - Febrero", 
        x = "Entidad Federativa", y = "Cantidad de Abuso Sexual") +
   theme_minimal() +
   theme(
     plot.title = element_text(size = 16, face = "bold", color = "darkblue", hjust = 0.5),  # Título centrado y en negrita
     axis.title = element_text(size = 12, face = "bold", color = "black"),
     axis.text = element_text(size = 10, color = "black"),  # Tamaño de texto en los ejes
     panel.grid.major = element_blank(),  # Eliminar las líneas de la cuadrícula mayor
     panel.grid.minor = element_blank(),  # Eliminar las líneas de la cuadrícula menor
     plot.margin = margin(1, 1, 2, 1, "cm")  # Añadir más margen en la parte superior
   ) +
   annotate("text", 
            x = 10, 
            y = media_nacional_abuso_sexual_febrero + 100,  # Ajustar la posición de la etiqueta de la media
            label = paste("Media Nacional: ", round(media_nacional_abuso_sexual_febrero, 0)), 
            color = "red", size = 5, fontface = "bold") +
   scale_y_continuous(limits = c(0, max(Porcentaje_de_abuso_sexual_por_entidad$conteo_total_febrero) * 1.1))
 
 
 
 