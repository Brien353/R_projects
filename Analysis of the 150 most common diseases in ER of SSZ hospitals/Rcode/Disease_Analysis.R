#Análisis de los 150 padecimientos más comunes atendidos en el servicio de Urgencias de la Secretaría de Salud de Zacatecas durante el año 2023"

#Cargando las librerias necesarias para hacer la importación de las bases de datos.
install.packages("readxl")
library(readxl)
library(dplyr)
library(ggplot2)
library(psych)
#Cargando las bases de datos necesarias para el proyecto.
diseases_data <- read.table("C:/Users/DELL/Downloads/Proyecto_Urgencias/Urgencias_2023/afecciones.txt",
                   sep = "|", 
                   header = TRUE, 
                   stringsAsFactors = FALSE)
clues_data <- read_excel("C:/Users/DELL/Downloads/Proyecto_Urgencias/Urgencias_2023/CLUES.xlsx")

diagnosticos_data <- read_excel("C:/Users/DELL/Downloads/Proyecto_Urgencias/Urgencias_2023/DIAGNOSTICOS.xlsx")

#Filtrando las bases de datos de las CLUES para la entidad de Zacatecas.
clues_zacatecas <- clues_data[clues_data$ENTIDAD=="ZACATECAS", ]
#Filtrando la base de datos anterior para enfocarnos solo en las CLUES de la Secretaria de Salud
clues_Secretaria_de_salud <- clues_zacatecas[clues_zacatecas$`NOMBRE DE LA INSTITUCION`=='SECRETARIA DE SALUD', ]
#Filtrando la base de datos anterior para enfocarnos específicamente en las CLUES De SSZ con cede en el Estado de Zacatecas
clues_Secretaria_de_salud_zac <- clues_Secretaria_de_salud[clues_Secretaria_de_salud$MUNICIPIO=='ZACATECAS', ]
#Filtrar la base de de datos general para permitir solo las observaciones de las afecciones que fueron atendidas en la SSZ con sede en Zacatecas
diseases_filtered <- diseases_data %>% filter(CLUES %in% clues_Secretaria_de_salud_zac$CLUES)
#Utilizando la base de datos proporcionada, crear una nueva tabla con la frecuencia absoluta de cada afección atendida en SSZ con dede en Zacatecas
Frequency_disease_zac <- diseases_filtered %>%
  count(AFEC)

#Ordenar de manera ascendente las frecuencias absolutas de las afecciones atendidas por la SSZ con sede en el Estado de Zacatecas.
Frequency_disease_zac <- Frequency_disease_zac %>%
                                    arrange(desc(n))
#Haciendo un LEFT JOIN() de la tabla que contiene los codigos de las enfermedades con la tabla de diagnosticos que contiene las llaves asignadas a cada codigo.
Frequency_disease_zac <- Frequency_disease_zac %>%
  left_join(diagnosticos_data, by = c("AFEC" = "CATALOG_KEY"))
#Remplazando la columna de códigos con la columna que contiene los nombres asociados a los padecimientos.
Frequency_disease_zac <- Frequency_disease_zac %>%
  mutate(AFEC = NOMBRE) %>%
  select(-NOMBRE) 
#Eliminando la información proporcionada por el LEFT JOIN () que no es relevante ni tiene interpretación clara
Frequency_disease_zac <- Frequency_disease_zac[,1:2]
#Filtrando los 10 padecimientos más comunes atendidos en el servicio de urgencias de la SSZ en el año de 2023.
Padecimientos_mas_comunes <- Frequency_disease_zac %>% slice_head(n=10)
#Filtrando los 11:20 padecimientos más comunes atendidos en el servicio de urgencias de la SSZ en el año de 2023.
Padecimientos_11_20 <- Frequency_disease_zac %>% slice(11:20)
#Filtrando los 21:30 padecimientos más comunes atendidos en el servicio de urgencias de la SSZ en el año de 2023.
Padecimientos_21_30 <- Frequency_disease_zac %>% slice(21:30)
#Filtrando los 31:40 padecimientos más comunes atendidos en el servicio de urgencias de la SSZ en el año de 2023.
Padecimientos_31_40 <- Frequency_disease_zac %>% slice(31:40)
#Filtrando los 41:50 padecimientos más comunes atendidos en el servicio de urgencias de la SSZ en el año de 2023.
Padecimientos_41_50 <- Frequency_disease_zac %>% slice(41:50)
#Filtrando los 51:60 padecimientos más comunes atendidos en el servicio de urgencias de la SSZ en el año de 2023.
Padecimientos_51_60 <- Frequency_disease_zac %>% slice(51:60)
#Filtrando los 61:70 padecimientos más comunes atendidos en el servicio de urgencias de la SSZ en el año de 2023.
Padecimientos_61_70 <- Frequency_disease_zac %>% slice(61:70)
#Filtrando los 71:80 padecimientos más comunes atendidos en el servicio de urgencias de la SSZ en el año de 2023.
Padecimientos_71_80 <- Frequency_disease_zac %>% slice(71:80)
#Filtrando los 81:90 padecimientos más comunes atendidos en el servicio de urgencias de la SSZ en el año de 2023.
Padecimientos_81_90 <- Frequency_disease_zac %>% slice(81:90)
#Filtrando los 91:100 padecimientos más comunes atendidos en el servicio de urgencias de la SSZ en el año de 2023.
Padecimientos_91_100 <- Frequency_disease_zac %>% slice(91:100)
#Filtrando los 101:110 padecimientos más comunes atendidos en el servicio de urgencias de la SSZ en el año de 2023.
Padecimientos_101_110 <- Frequency_disease_zac %>% slice(101:110)
#Filtrando los 111:120 padecimientos más comunes atendidos en el servicio de urgencias de la SSZ en el año de 2023.
Padecimientos_111_120 <- Frequency_disease_zac %>% slice(111:120)
#Filtrando los 121:130 padecimientos más comunes atendidos en el servicio de urgencias de la SSZ en el año de 2023.
Padecimientos_121_130 <- Frequency_disease_zac %>% slice(121:130)
#Filtrando los 131:140 padecimientos más comunes atendidos en el servicio de urgencias de la SSZ en el año de 2023.
Padecimientos_131_140 <- Frequency_disease_zac %>% slice(131:140)
#Filtrando los 141:150 padecimientos más comunes atendidos en el servicio de urgencias de la SSZ en el año de 2023.
Padecimientos_141_150 <- Frequency_disease_zac %>% slice(141:150)

#Construyendo una gráfica de barras para presentar la frecuencia de las 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023
Padecimientos_mas_comunes <- Padecimientos_mas_comunes %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_mas_comunes, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "10 principales afecciones atendidas en Urgencias (SSZ, 2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low ="#E06666",  # light blue
    high = "red"  # dark blue
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 10)  # space below title
    ),
    legend.position = "none"
  )

#Construyendo una gráfica de barras para presentar la frecuencia de las siguinetes 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023

Padecimientos_11_20 <- Padecimientos_11_20 %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_11_20, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Afecciones comunes Atendidas en Urgencias #11–20 (SSZ 2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#A6CEE3",  # light blue
    high = "#1F78B4"  # dark blue
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 10)  # space below title
    ),
    legend.position = "none"
  )

#Construyendo una gráfica de barras para presentar la frecuencia de las siguinetes 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023

Padecimientos_21_30 <- Padecimientos_21_30 %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_21_30, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Afecciones comunes Atendidas en Urgencias #21–30(SSZ,2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#A9DFBF",  # light blue
    high = "green"  # dark blue
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 10)  # space below title
    ),
    legend.position = "none"
  )

#Construyendo una gráfica de barras para presentar la frecuencia de las siguinetes 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023

Padecimientos_21_30 <- Padecimientos_21_30 %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_21_30, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Afecciones comunes Atendidas en Urgencias #21–30(SSZ,2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#A9DFBF",  # light blue
    high = "green"  # dark blue
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 10)  # space below title
    ),
    legend.position = "none"
  )

#Construyendo una gráfica de barras para presentar la frecuencia de las siguinetes 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023

Padecimientos_31_40 <- Padecimientos_31_40 %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_31_40, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Afecciones comunes Atendidas en Urgencias #31–40 (SSZ 2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#D2B4DE",  # light blue
    high = "purple"  # dark blue
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 10)  # space below title
    ),
    legend.position = "none"
  )


#Construyendo una gráfica de barras para presentar la frecuencia de las siguinetes 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023

Padecimientos_41_50 <- Padecimientos_41_50 %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_41_50, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Afecciones comunes Atendidas en Urgencias #41–50 (SSZ 2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#FADBD8",,  
    high = "#C0392B"  
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 10)  # space below title
    ),
    legend.position = "none"
  )

#Construyendo una gráfica de barras para presentar la frecuencia de las siguinetes 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023

Padecimientos_51_60 <- Padecimientos_51_60 %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_51_60, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Afecciones comunes Atendidas en Urgencias #51–60 (SSZ 2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#A9DFBF",,  
    high = "#196F3D"  
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 10)  # space below title
    ),
    legend.position = "none"
  )
#Construyendo una gráfica de barras para presentar la frecuencia de las siguinetes 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023

Padecimientos_61_70 <- Padecimientos_61_70 %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_61_70, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Afecciones comunes Atendidas en Urgencias #61–70 (SSZ 2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#A3E4D7",,  
    high = "#117864"  
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 10)  # space below title
    ),
    legend.position = "none"
  )

#Construyendo una gráfica de barras para presentar la frecuencia de las siguinetes 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023

Padecimientos_71_80 <- Padecimientos_71_80 %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_71_80, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Afecciones comunes Atendidas en Urgencias #71–80 (SSZ,2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#D2B4DE",,  
    high = "#6C3483"  
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 10)  # space below title
    ),
    legend.position = "none"
  )
#Construyendo una gráfica de barras para presentar la frecuencia de las siguinetes 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023

Padecimientos_81_90 <- Padecimientos_81_90 %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_81_90, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Afecciones comunes Atendidas en Urgencias #81–90 (SSZ,2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#BB8FCE",,  
    high = "#512E5F"  
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 10)  # space below title
    ),
    legend.position = "none"
  )
#Construyendo una gráfica de barras para presentar la frecuencia de las siguinetes 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023

Padecimientos_91_100 <- Padecimientos_91_100 %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_91_100, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Afecciones comunes Atendidas en Urgencias #91–100 (SSZ,2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#F8C471",,  
    high = "#CA6F1E"  
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 10)  # space below title
    ),
    legend.position = "none"
  )
#Construyendo una gráfica de barras para presentar la frecuencia de las siguinetes 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023


Padecimientos_101_110 <- Padecimientos_101_110 %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_101_110, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Afecciones comunes Atendidas en Urgencias #101–110 (SSZ,2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#F0B27A",,  
    high = "#A04000"  
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 10)  # space below title
    ),
    legend.position = "none"
  )
#Construyendo una gráfica de barras para presentar la frecuencia de las siguinetes 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023

Padecimientos_111_120 <- Padecimientos_111_120 %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_111_120, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Afecciones comunes Atendidas en Urgencias #111–120 (SSZ,2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#E6B0AA",,  
    high = "#943126"  
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 10)  # space below title
    ),
    legend.position = "none"
  )

#Construyendo una gráfica de barras para presentar la frecuencia de las siguinetes 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023


Padecimientos_121_130 <- Padecimientos_121_130 %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_121_130, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Afecciones comunes Atendidas en Urgencias #121–130 (SSZ,2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#DC7633",,  
    high = "#6E2C00"  
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 10)  # space below title
    ),
    legend.position = "none"
  )
#Construyendo una gráfica de barras para presentar la frecuencia de las siguinetes 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023

Padecimientos_131_140 <- Padecimientos_131_140 %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_131_140, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Afecciones comunes Atendidas en Urgencias #131–140 (SSZ,2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#CD6155",,  
    high = "#641E16"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold",
      margin = margin(b = 10)  # space below title
    ),
    legend.position = "none"
  )
#Construyendo una gráfica de barras para presentar la frecuencia de las siguinetes 10 principales afecciones atendidas en el servicio de urgencia de la SSZ en el año de 2023

Padecimientos_141_150 <- Padecimientos_141_150 %>%
  arrange(n) %>%
  mutate(AFEC = factor(AFEC, levels = AFEC))
ggplot(Padecimientos_141_150, aes(x = AFEC, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, fontface = "bold") +
  labs(
    title = "Afecciones comunes Atendidas en Urgencias #141–150 (SSZ,2023)",
    x = "Nombre de la afección",
    y = "Número de veces que la afección fue atendida"
  ) +
  coord_flip() +
  scale_fill_gradient(
    low = "#7FB3D5",,  
    high = "#1B4F72"
  ) +
  theme_minimal(base_size = 12) +
  theme


#Discusión acerca de la distribución de los datos.
describe(Frequency_disease_zac$n)

#Medidas de tendencia central
Frequency_disease_zac_mean<-mean(Frequency_disease_zac$n)
Frequency_disease_zac_median <- median(Frequency_disease_zac$n)

#Medidas de dispersión
Frequency_disease_zac_SD <- sd(Frequency_disease_zac$n)
Frequency_disease_zac_SD_percentage <- (Frequency_disease_zac_SD/ Frequency_disease_zac_mean)*100
Frequency_disease_zac_IQR <- IQR(Frequency_disease_zac$n)
#Como la desviación estandar de la muestra es alta, esto significa que

#1.Variaciones extremas en la frecuencia de enfermedades.

#2.Algunos padecimientos que se atienden en urgencias son muy comunes, mientras que otros son raros.

#3.Posibles errores en la recolección de datos o la presencia de valores atípicos.

#Vamos a confirmar con un diagrama de caja que existen valores atípicos, es decir que 3 es verdadera

bp <- boxplot(Frequency_disease_zac$n,
              main = "Diagrama de caja de Padecimientos comunes atendidos en Urgencias en SSZ en 2023",
              ylab = "Frecuencia",
              col = "orange",
              border = "darkred",
              notch = TRUE,
              boxwex = 0.5,
              outline = TRUE,
              las = 1)

# Add median label
text(x = 1.2, y = bp$stats[3], labels = paste("Mediana:", bp$stats[3]), pos = 4, col = "blue")

#Gracias al diagrama de caja podemos concluir que las mediciones varian de manera extrema gracias a la presencia de valores atípicos.
#Por lo tanto, podemos tomar como valores referencia de las medidas de tendencia central la mediana y como medida de variancia el IQR.
#El IQR=42 sugiere que hay variabilidad moderada a alta en los datos, aún descartando el caso de los valores atípicos.
#La mediana de 43 implica que la mitad de las observaciones tienen una frequencia de aparición en la unidad de urgencias menor a 43
#Aunque las 10 enfermedades más frecuentes se identifican como valores atípicos estadísticos según el rango intercuartílico (IQR), representan afecciones comunes que suelen ser atendidas en los servicios de urgencias. Su alta frecuencia es esperada y resalta la gran carga que estas condiciones imponen en la atención médica aguda. Por lo tanto, estos valores se consideran válidos y significativos para el análisis.

#Viendo si los datos siguien una distribución normal
shapiro.test(Frequency_disease_zac$n)
#Puesto que el p-valor < 0.05, entonces los datos no siguien una distribución normal


sum(Frequency_disease_zac$n)
