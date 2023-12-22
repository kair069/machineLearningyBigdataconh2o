# Instala y carga el paquete "tidyverse" si aún no lo has hecho
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
#

library(skimr)
library(DataExplorer)
library(ggpubr)
library(mosaicData)
library(rsample)
library(tidymodels)
#h2o.init(
#  nthreads = -1,
#  max_mem_size = "4g"
#)
########################IMPORTACION DE DATOS-TRAIN
datos_train <- read.csv("C:/Users/Acer/Downloads/Machine_Learning_Online-master/Machine_Learning_Online-master/adult/adult.data", header=FALSE)
colnames(datos_train) <- c("age", "workclass", "final_weight", "education",
                           "education_number", "marital_status", "occupation",
                           "relationship", "race", "sex", "capital_gain",
                           "capital_loss", "hours_per_week", "native_country",
                           "salario")

########################IMPORTACION DE DATOS-TEST
datos_test <- read.csv("C:/Users/Acer/Downloads/Machine_Learning_Online-master/Machine_Learning_Online-master/adult/adult.test", header=FALSE)
colnames(datos_test) <- c("age", "workclass", "final_weight", "education",
                          "education_number", "marital_status", "occupation",
                          "relationship", "race", "sex", "capital_gain",
                          "capital_loss", "hours_per_week", "native_country",
                          "salario")
head(datos_train)
head(datos_test)
#count(datos_train)
#count(datos_test)

datos <- bind_rows(datos_train, datos_test)
#count(datos)



remplazar <- function(x){
  x[x == "?"] <- NA
  return(x)
}


datos <- datos %>% map_df(.f = remplazar)

datos <- drop_na(datos)

# Recodificación de niveles.
datos <- datos %>% mutate(workclass = recode(datos$workclass,
                                             "Without_pay" = "desempleado",
                                             "Self_emp_inc" = "autonomo",
                                             "Self_emp_not_inc" = "autonomo",
                                             "Federal_gov" = "funcionario",
                                             "Local_gov" = "funcionario",
                                             "State_gov" = "funcionario"))



datos <- datos %>% mutate(marital_status = recode(datos$marital_status,
                                                  "Married-AF-spouse" = "casado",
                                                  "Married-civ-spouse" = "casado",
                                                  "Married-spouse-absent"= "casado",
                                                  "Widowed" = "separado",
                                                  "Never-married" = "soltero",
                                                  "Separated" = "separado",
                                                  "Divorced" = "separado")
)


norte_america <- c("Canada", "Cuba", "Dominican-Republic", "El-Salvador",
                   "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico",
                   "Nicaragua", "Outlying-US(Guam-USVI-etc)", "Puerto-Rico",
                   "Trinadad&Tobago", "United-States")



europa <- c("England", "France", "Germany", "Greece", "Holand-Netherlan
ds",
            "Hungary", "Ireland", "Italy", "Poland", "Portugal", "Scotland",
            "Yugoslavia")



asia <- c("Cambodia", "China", "Hong", "India", "Iran", "Japan", "Laos",
          "Philippines", "Taiwan", "Thailand", "Vietnam")


sud_america <- c("Columbia", "Ecuador", "Peru")

otros <- c("South")

datos <- datos %>% mutate(native_country = case_when(
  native_country %in% norte_america ~ "Norte America",
  native_country %in% asia ~ "Asia",
  native_country %in% sud_america ~ "Sud America",
  native_country %in% europa ~ "Europa",
  native_country %in% otros ~ "Otros",
  TRUE ~ as.character(native_country)
))




primaria <- c("Preschool", "1st-4th", "5th-6th", "7th-8th", "9th", "10th",
              "11th","12th")
bachillerato <- c("Some-college", "Assoc-acdm", "Assoc-voc")
master <- c("Masters", "Prof-school")



datos <- datos %>% mutate(education = case_when(
  education %in% primaria ~ "primaria",
  education == "HS-grad" ~ "secundaria",
  education %in% bachillerato ~ "bachillerato",
  education == "Bachelors" ~ "universidad",
  education %in% master ~ "master",
  education == "Doctorate" ~ "doctorado",
  TRUE ~ as.character(education)
))


datos <- datos %>% mutate(salario = case_when(
  salario == "<=50K." ~ "<=50K",
  salario == ">50K." ~ ">50K",
  TRUE ~ as.character(salario)
))
# Se agrupan las observaciones cuya variable occupation == "Armed-Forces"
#con
# "Other-service" ya que solo hay 14 observaciones (0.03% del total).
datos <- datos %>% mutate(occupation = case_when(
  occupation == "Armed-Forces" ~ "Other-service",
  TRUE ~ as.character(occupation)
))
write_csv(x = datos, path = "adult_custom.csv", col_names = TRUE)


######################Iniciación de H2O######################


library(h2o)
library(tidyverse)
# Creación de un cluster local con todos los cores disponibles.
h2o.init(
  ip = "localhost",
  # -1 indica que se empleen todos los cores disponibles.
  nthreads = -1,
  # Máxima memoria disponible para el cluster.
  max_mem_size = "6g"
)



# Se eliminan los datos del cluster por si ya había sido iniciado.
#h2o.removeAll()
# Para que no se muestre la barra de progreso.
#h2o.no_progress()

# Ejemplo de conexión a un cluster remoto
h2o.init(ip = "localhost", port = 54321)


# Carga de datos en el cluster H2O desde url.
url_path <- "https://github.com/JoaquinAmatRodrigo/Estadistica-con-R/raw/master/datos/adult_custom.csv"


datos_h2o <- h2o.importFile(
  path = url_path,
  header = TRUE,
  sep = ",",
  destination_frame = "datos_h2o"
)

#########################################################
# Carga de datos en el cluster H2O desde local.
datos_h2o <- h2o.importFile(
  path = "./adult_custom.csv",
  header = TRUE,
  sep = ",",
  destination_frame = "datos_h2o"
)
datos_R <- read.csv(file = url_path, header = TRUE)
datos_h2o <- as.h2o(x = datos_R, destination_frame = "datos_h2o")
##Exploración de los datos
# Dimensiones del set de datos

h2o.dim(datos_h2o)


# Nombre de las columnas
h2o.colnames(datos_h2o)


h2o.describe(datos_h2o)



# Índices
indices <- h2o.columns_by_type(object = datos_h2o, coltype = "numeric")
indices


# Nombres
h2o.colnames(x = datos_h2o)[indices]



indices <- h2o.columns_by_type(object = datos_h2o, coltype = "numeric")
h2o.cor(x = datos_h2o[, indices], y = NULL, method = "Pearson", na.rm = TRUE)

# Convertir la columna "salario" a factor
datos_h2o$salario <- as.factor(datos_h2o$salario)

# Crear la tabla de frecuencias con h2o.table
tabla_muestra <- as.data.frame(h2o.table(datos_h2o$salario))
print(tabla_muestra)




# Una vez creada la tabla, se carga en el entorno de R para poder graficar.
ggplot(
  data = tabla_muestra,
  aes(x = salario, y = Count, fill = salario)) +
  geom_col() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  theme_bw() +
  labs(
    x = "Salario", y = "Número de observaciones",
    title = "Distribución de la variable Salario") +
  theme(legend.position = "none")



#######Separación de training, validación y test
# Separación de las observaciones en conjunto de entrenamiento y test.
# En los ejemplos de GBM y deep learning se repetirá la separación, pero en
# tres conjuntos en lugar de dos.
particiones <- h2o.splitFrame(data = datos_h2o, ratios = c(0.8), seed = 123
)
datos_train_h2o <- h2o.assign(data = particiones[[1]], key = "datos_train_H2O")
datos_test_h2o <- h2o.assign(data = particiones[[2]], key = "datos_test_H2O")

h2o.table(datos_train_h2o$salario)



h2o.table(datos_test_h2o$salario)



h2o.table(datos_train_h2o$salario)/h2o.nrow(datos_train_h2o)



h2o.table(datos_test_h2o$salario)/h2o.nrow(datos_test_h2o)