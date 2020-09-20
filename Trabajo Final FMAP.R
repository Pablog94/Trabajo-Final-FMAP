# Trabajo Final FMAP ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Librerias
library(tidyverse)
library(readxl)
library(stringi)
library(jsonlite)
library(data.table)

# Directorio 

directorio <- "C:/Users/Pabca/OneDrive - Facultad de Ciencias Económicas - Universidad de Buenos Aires/Maestria/Metodos analiticos predictivos/Trabajo Final"
setwd(directorio)

# Base --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

autos_meli <- read_excel("C:/Users/Pabca/OneDrive - Facultad de Ciencias Económicas - Universidad de Buenos Aires/Maestria/Metodos analiticos predictivos/Trabajo Final/autos_mercadolibre.xlsx")
colnames(autos_meli) <-stri_trans_general(tolower(colnames(autos_meli)), "Latin-ASCII") #Pongo colnames en minusculas
autos_meli <- mutate(autos_meli, titulo = stri_trans_general(tolower(titulo), "Latin-ASCII")) #Pongo titulo en minuscula

# Transformo variables en numericas y borro columnas vacias  --------------------------------------------------------------------------------------------------------------------------------------------------

# Borro columnas que son todo na
autos_meli <- autos_meli[,colSums(is.na(autos_meli))<nrow(autos_meli)]
autos_meli <- autos_meli %>% select(-cilindros)

# Vuelvo numerica las colummna kilometros, potencia, altura, largo, ancho, distancia entre ejes y capacidad del tanque
autos_meli <- autos_meli %>% 
  mutate(kilometros = as.numeric(stri_replace_all_regex(kilometros,"[:alpha:]|[:punct:]","")),
         potencia = as.numeric(stri_replace_all_regex(potencia, "hp| ","")),
         altura = as.numeric(stri_replace_all_regex(altura, "mm| ", "")),
         largo = as.numeric(stri_replace_all_regex(largo, "mm| ", "")),
         ancho = as.numeric(stri_replace_all_regex(ancho, "mm| ", "")),
         `distancia entre ejes` = as.numeric(stri_replace_all_regex(`distancia entre ejes`, "mm| ", "")),
         `capacidad del tanque` = as.numeric(stri_replace_all_regex(`capacidad del tanque`, "L| ", "")))

# Vuelvo numerica la columna motor
motores <- as.character(seq(1.0, 6.0, by = 0.1))
for (i in 1:6) {
  motores[motores[] == paste0(i)] <- paste0(i,".0")  
}

motores <- stri_replace_all_regex(motores, "\\.","\\\\.")
autos_meli <- autos_meli %>% mutate(motor = as.numeric(stri_extract_first_regex(motor,paste(motores, collapse = "|"))))

# Vuelvo numerica la columna cilindrada y pongo todo en la misma unidad (cc)
autos_meli <- autos_meli %>% mutate(cilindrada_litros = ifelse(str_detect(cilindrada, "L"),1,0),
                                    cilindrada = as.numeric(stri_replace_all_regex(cilindrada,"cc|L| ","")),
                                    cilindrada = ifelse(cilindrada_litros == 1, cilindrada*1000,cilindrada))

autos_meli <- autos_meli %>% select(!cilindrada_litros)

# Convierto todas las columnas de que dicen si o NA en dummies 
for (i in 26:ncol(autos_meli)) {
  print(i)  
  autos_meli[,i] <- as.numeric(ifelse(is.na(autos_meli[,i]),"0","1"))
}

# Creo variables marca y modelo ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Remplazo "-" " " del titulo ya que algunos describen el modelo de auto asi.
autos_meli <- autos_meli %>% mutate(titulo = str_replace_all(titulo, "\\-"," "))

# Creo tabla con marcas y modelo uniendo informacion de dos fuentes y agregando las faltantes ----------------

#back4apps
modelos_autos_back4apps <- fromJSON("C:/Users/Pabca/OneDrive - Facultad de Ciencias Económicas - Universidad de Buenos Aires/Maestria/Metodos analiticos predictivos/Trabajo Final/Car_Model_List.json")
modelos_autos_back4apps <- as.data.frame(modelos_autos_back4apps[[1]])

modelos_autos_back4apps <- modelos_autos_back4apps %>% 
  group_by(Make,Model) %>% 
  summarise()

colnames(modelos_autos_back4apps) <- c("marca","modelo")
modelos_autos_back4apps <- as.data.frame(modelos_autos_back4apps)

#rananegra
marcas_autos_rananegra <- read_excel("C:/Users/Pabca/OneDrive - Facultad de Ciencias Económicas - Universidad de Buenos Aires/Maestria/Metodos analiticos predictivos/Trabajo Final/Marcas y modelos coches - base de datos - rananegra.es/marcas.xls")
modelos_autos_rananegra <- read_excel("C:/Users/Pabca/OneDrive - Facultad de Ciencias Económicas - Universidad de Buenos Aires/Maestria/Metodos analiticos predictivos/Trabajo Final/Marcas y modelos coches - base de datos - rananegra.es/modelos.xls")

colnames(marcas_autos_rananegra)[1] <- "id_marca"
modelos_autos_rananegra <- left_join(modelos_autos_rananegra,marcas_autos_rananegra[,c(1,2)], by="id_marca")

modelos_autos_rananegra <- modelos_autos_rananegra[,c(5,3)]
colnames(modelos_autos_rananegra) <- c("marca","modelo")
modelos_autos_rananegra <- as.data.frame(modelos_autos_rananegra)

#agregados
agregados <- data.frame(marca  = c("Chery","Chery","Chery","Chery","Chery","Chevrolet","Fiat","Fiat","Fiat","Fiat","Fiat","Fiat","Fiat","Fiat","Chevrolet","Chevrolet","Chevrolet","Chevrolet","Chevrolet","Chevrolet","Chevrolet","Chevrolet","Chevrolet","Chevrolet","Chevrolet","Chevrolet","Ford","Ford","Ford","Ford","Ford","Toyota","Toyota","Toyota","Toyota","Toyota","Volkswagen","Volkswagen","Volkswagen","Volkswagen","Volkswagen","Volkswagen","Volkswagen","Volkswagen","Volkswagen","Volkswagen","Hyundai","Hyundai","Hyundai","Hyundai","Citroen","Citroen","Citroen","Renault","Renault","Renault","Renault","Renault","Peugeot","Peugeot","Peugeot","Peugeot","Peugeot","Chrysler","Chrysler","Honda","Jeep","Jeep","Audi","Dodge","Mercedes Benz","Mercedes Benz","Mercedes Benz","Mercedes Benz","Mercedes Benz","Mercedes Benz","Nissan","Nissan","Nissan","Nissan","Rover","Rover","Rover","Suzuki","Asia","Asia","Land Rover","Land Rover","Daewoo","Daewoo","Daewoo","Daihatsu","Daihatsu","Daihatsu","Fiat","Fiat","Fiat","Fiat","Chevrolet","Chevrolet","Chevrolet","Chevrolet","Chevrolet","Chevrolet","Chevrolet","Chevrolet","Chevrolet","Chery","Ford","Ford","Ford","Toyota","Toyota","Fiat","Fiat","Ford","Ford","Ford","Ford","Ford","Ford","Chevrolet","Chevrolet","Chevrolet","Chevrolet","volkswagen","Volskwagen","Citroen","Renault","Renault","Renault","Renault","Renault","Renault","Renault","Nissan","Nissan","Nissan","Nissan","Ram","Ram","Dodge","Dodge","Dodge","Mecedes Benz","Rover","Kia","Kia","Mg","Volkswagen","Volkswagen","Volkswagen","Volkswagen","Volkswagen","Mitsubishi","Mitsubishi","Mitsubishi"),
                        modelo = c("Tiggo","Fulwin","QQ","Face","Arrizo","Corsa","Siena","Palio","Cronos","Toro","Mobi","Argo","Duna","147","Prisma","Astra","Spin","Celta","Veraneio","Agile","Onix","Vectra","Luv","Meriva","S10","Zafira","F 100","Falcon","F 150","Taunus","Econoline","Etios","SW4","Hiace","Innova","Prado","Gol Trend","Gol","Suran","Senda","Voyage","Virtus","Saveiro","Golf","Quantum","2009","Creta","Galloper","Santamo","Aircross","Mehari","Aircross","Elysee","Logan","Sandero","Duster","Kwid","Symbol","504","408","301","2008","Expert","Caravan","Journey","City","IKA","Potro","Q2","Ram","MI","MB","CLK","1215","250","350","March","Frontier","X Terra","X Trail","416","420","214","Fun","Towner","Topic","Evoque","420","Tico","Espero","Musso","Cuore","Wide","Move","Vivace","Regatta","Multicarga","Spazio","Silverado","Montana","Grand Vitara","Ipanema","Apache","Omega","City","Wagon","Kadett","Skin","F100","Max","F350","Corona","Xei","128","125","F 400","Currier","Titanium","Eco Sport","Duty","Windsatar","Monza","Chevette","C 20","C 10","Pointer","T Cross","Picasso","12","21","11","Rodeo","18","19","Captur","Pick up","Cadenero","NP300","D22","2500","1500","1500","200","100","ml ","414","Besta","K 2700","Emgrand","Pointer","T cross","Gol","Trend","Gacel","i300","Nativa","Protom")) 

#General
marca_modelo_autos <- rbind(modelos_autos_back4apps, modelos_autos_rananegra,agregados)
marca_modelo_autos <- marca_modelo_autos %>% mutate(marca = tolower(str_replace_all(marca, "\\-" , " ")),
                                                    modelo = tolower(str_replace_all(modelo, "\\-" , " ")))
marca_modelo_autos <- marca_modelo_autos[duplicated(marca_modelo_autos)== FALSE,]
rm(modelos_autos_back4apps,modelos_autos_rananegra,agregados,marcas_autos_rananegra)

# -----------------------------------------------------------------------------------------------------------------

# Busco la marca en el titulo, separo los que no encontre
vec_marc_autos <- unique(marca_modelo_autos$marca)
autos_meli <- autos_meli %>% mutate(marca = as.character(stri_extract_first_regex(titulo, paste(vec_marc_autos, collapse = "|"))))
autos_meli <- as.data.frame(autos_meli)

autos_marca <- autos_meli %>% filter(!is.na(marca))
autos_sin_marca <- autos_meli %>% filter(is.na(marca))

autos_marca[autos_marca[,"marca"] == "volskwagen" , "marca"] <- "volkswagen" #remplazo marca (errores de tipeo en las publicaciones)
marca_modelo_autos[marca_modelo_autos[,"marca"] == "volskwagen","marca"] <- "volkswagen" 
marca_modelo_autos <- marca_modelo_autos[duplicated(marca_modelo_autos)== FALSE,]

# Busco el modelo de los autos con marca
marcas_existentes <- unique(autos_marca$marca)
autos_meli <- autos_marca[0,]
for (i in marcas_existentes) {
  print(i)  
  
  autos_temp <- autos_marca %>% filter(marca == i)  
  modelos_temp <- marca_modelo_autos %>% filter(marca == i) 
  
  autos_temp <- autos_temp %>% mutate(modelo = as.character(stri_extract_first_regex(titulo, paste(modelos_temp$modelo, collapse = "|"))))
  autos_meli <-  rbind(autos_meli, autos_temp)
}
rm(autos_temp,modelos_temp)

# Busco el modelo de los autos sin marca
marca_modelo_autos_2 <- marca_modelo_autos %>% 
  filter(marca_modelo_autos$marca %in% marcas_existentes)
marca_modelo_autos_2 <- marca_modelo_autos_2 %>% 
  mutate(letras_modelo = stri_count_regex(modelo, ".")) %>% 
  filter(letras_modelo > 2)

autos_sin_marca <- autos_sin_marca %>% mutate(modelo = as.character(stri_extract_first_regex(titulo, paste(marca_modelo_autos_2$modelo, collapse = "|")))) 

# Borro de los autos sin marca aquellos que no pude encontrar el modelo, luego pongo en marca aquella que le corresponde al modelo
autos_sin_marca <- autos_sin_marca %>% filter(!is.na(modelo))

for (i in 1:nrow(autos_sin_marca)) {
  print(i)
  
  marca_temp <- marca_modelo_autos_2 %>% filter(modelo == as.character(autos_sin_marca[i,"modelo"]))
  autos_sin_marca[i,"marca"] <- marca_temp[1,"marca"]
}
rm(marca_temp,marca_modelo_autos_2)

# Vuelvo a juntar los casos en la base y elimino los que no tienen modelo (muchos de ellos no son autos)
autos_meli <- rbind(autos_meli, autos_sin_marca)
autos_meli <- autos_meli %>% filter(!is.na(modelo))
rm(autos_marca,autos_sin_marca)

# remplazo marca ram por dodge y en modelo pongo ram. Tambien cooper por mini en modelo
autos_meli[autos_meli[,"marca"] == "ram", "modelo"] <- "ram"
autos_meli[autos_meli[,"marca"] == "ram", "marca"] <- "dodge"
autos_meli[autos_meli[,"marca"] == "mini", "modelo"] <- "cooper"

# Analisis exploratorio de las variables, eliminacion de outliers y accion ante missings -------------------------------------------------------------------------------------------------------------------------

# Acomodo variables en la base por comodidad
autos_meli <- autos_meli[,c(1:5,84,85,6:83)]

# LLeno NA segun marca modelo y año ------------------------------------------------------------------------

# Creo varaible marca _modelo_año
autos_meli <- autos_meli %>% 
              mutate(marca_modelo_ano = paste0(marca,"_",modelo,"_",ano))

na_cols <- c(10:15,17:27)
for (j in na_cols ) {
  print(j)
  
  for (i in 1:nrow(autos_meli)) {
    if(is.na(autos_meli[i,j])){
      
      temp <- filter(autos_meli, marca_modelo_ano == as.character(autos_meli$marca_modelo_ano[i]))
      temp <- temp[!is.na(temp[,j]),]
      if (nrow(temp) > 0){
        autos_meli[i,j] <- sample(temp[,j],1)
      }
    }
  }
}
rm(temp)

# Borro los registros que quedaron con na (sin tener en cuenta las columnas de medida y la capacidad del tanque que no se usaran)

na_cols <- na_cols[-seq(18:22)]
for (i in na_cols ) {
  print(i)
  autos_meli <- autos_meli[is.na(autos_meli[,i]) == F,]
}

# Año -----------------------------------------------------------------------------------------------------
summary(autos_meli$ano) #No hay NA
hist(autos_meli$ano)

# Elimino autos del 2020 y menores a 2000
autos_meli <- autos_meli %>% 
              filter(ano < 2020, ano >= 2000)

# Kilometros ----------------------------------------------------------------------------------------------
summary(autos_meli$kilometros) #No hay NA

# Filtro aquellos autos que el precio es igual a los kilometros (alguno de ambos esta mal)
autos_meli <- autos_meli %>% 
              filter(precio != kilometros)

# Me quedo con los autos con kilometraje mayor a 5.000 y menor a 900.000 (casi 0km y cualquier cosa en el kilometraje)
autos_meli <- autos_meli %>% 
              filter(kilometros > 5000, kilometros < 900000)

# Puertas --------------------------------------------------------------------------------------------------
summary(autos_meli$puertas)

# Precio ------------------------------------------------------------------------------------------------
summary(autos_meli$precio)

# Los autos cuyos digitos de precio son todos 9 no tienen ese precio 
vect_9 <- c(9,99,999,9999,99999,999999,9999999,99999999,999999999,9999999999)
autos_meli <- autos_meli %>% filter(!(precio%in%vect_9)) 
rm(vect_9)

# Me quedo con autos entre 5 millones y 30 mil pesos ya que por fuera de esto son errores en la publicaicon o no son autos o son pocos autos demasiado expensivos
autos_meli <- autos_meli %>% filter(precio > 30000, precio < 5000000) 

# Outliers en precio 
ggplot(autos_meli) + 
  geom_boxplot(aes(x = marca, y = precio, fill = marca)) +
  xlab("marca") +
  theme(legend.position="none") +
  xlab("") +
  xlab("")

# Voy a eliminar los outliers, agrupando por marca, modelo y año , filtrandolos por el metodo hampel
marca_modelo_ano <- unique(autos_meli$marca_modelo_ano)
autos_meli_outliers <- autos_meli[0,]

elemento <- 1
for (i in marca_modelo_ano) {
  print(round(100*elemento/length(marca_modelo_ano)),0)
  
  autos_meli_temp <- autos_meli %>% 
                     filter(marca_modelo_ano == i)
  
  autos_meli_temp <- autos_meli_temp %>% 
                     mutate(outlier = ifelse((precio < (median(autos_meli_temp$precio) - 3*mad(autos_meli_temp$precio)) |
                            precio > (median(autos_meli_temp$precio) +  3*mad(autos_meli_temp$precio))),
                            1,0))
  
  autos_meli_outliers <- rbind(autos_meli_outliers, autos_meli_temp)
  elemento <- elemento +1
}

autos_meli <- left_join(autos_meli, autos_meli_outliers[,c("id","outlier")], by = "id")
rm(autos_meli_outliers,autos_meli_temp)

#borro los outliers
autos_meli <- autos_meli %>% 
              filter(outlier == 0) %>% 
              select(!outlier)

# Marca y Modelo ------------------------------------------------------------------------------------------

# Cantidad de modelos en la base de datos
cant_autos_marca_modelo <- autos_meli %>% 
                           group_by(marca,modelo) %>% 
                           tally()

# Creo variable marca_modelo
autos_meli <- autos_meli %>% 
              mutate(marca_modelo = paste0(marca,"_",modelo))

# Voy a eliminar aquellos modelos que tengan menos de 10 autos, ya que pueden ser errores y son autos pocos comunes de vender en ese caso.
cant_autos_marca_modelo <- cant_autos_marca_modelo %>% 
                           filter(n < 10)

cant_autos_marca_modelo <- cant_autos_marca_modelo %>% 
                           mutate(marca_modelo = paste0(marca,"_",modelo))

autos_meli <- autos_meli %>% 
              mutate(marca_modelo = paste0(marca,"_",modelo))

autos_meli <- autos_meli %>% 
              filter(!(marca_modelo %in% cant_autos_marca_modelo$marca_modelo))

# Arreglos varios -------------------------------------------------------------------------------------------

#Elimino dos registros con muchas puertas y uno con solo una y las de 6 tambien
autos_meli <- autos_meli %>% 
  filter(puertas <= 5, puertas > 1)

#Me quedo solo con los autos con capacidad de hasta 5 personas 
autos_meli <- autos_meli %>% 
  filter(`capacidad de personas` <=5, `capacidad de personas`> 1)

#Elimino los automoviles con menos de 3 velocidades y mas de 8
autos_meli <- autos_meli %>% 
  filter(`numero de velocidades` > 3, `numero de velocidades`< 9)

# Nuevas variables ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Creo variable de rango de antiguedad.(agrupo cada 5) ---------------------------------------------------
autos_meli <- autos_meli %>% 
  mutate(rango_antiguedad = ifelse(ano <= 2019 & ano > 2014,5,
                            ifelse(ano <= 2014 & ano > 2009,10,
                            ifelse(ano <= 2009 & ano > 2004,15,
                            ifelse(ano <= 2004 & ano > 1999,20,
                            ifelse(ano <= 1999 & ano > 1994,25,30))))))

# Precio de valuacion del DRNAP ---------------------------------------------------------------------------
# La base es un pdf convertido a excel

dnrap_1 <- read_excel("C:/Users/Pabca/OneDrive - Facultad de Ciencias Económicas - Universidad de Buenos Aires/Maestria/Metodos analiticos predictivos/Trabajo Final/dnrpa_1.xlsx")
dnrap_2 <- read_excel("C:/Users/Pabca/OneDrive - Facultad de Ciencias Económicas - Universidad de Buenos Aires/Maestria/Metodos analiticos predictivos/Trabajo Final/dnrpa_2.xlsx")

DNRAP <- rbind(dnrap_1,dnrap_2)

# Arreglo la base 

DNRAP <- DNRAP %>%  
         filter(DNRAP$`Desc. Tipo` %in% c("TODO TERRENO","SEDAN 3 PUERTAS","SEDAN 4 PUERTAS","SEDAN 5 PUERTAS",
                                          "COUPE","DESCAPOTABLE","RURAL 5 PUERTAS","SEDAN 2 PUERTAS","PICK-UP",
                                          "FAMILIAR","CONVERTIBLE","UTILITARIO","PICK-UP CABINA SIMPLE",
                                          "PICK-UP CARROZADA","PICK-UP CABINA DOBLE","RURAL 3 PUERTAS"))

DNRAP <- DNRAP[,-c(2:7,11)]
DNRAP <- melt(DNRAP, id.vars = as.character(colnames(DNRAP)[1:4]),variable.name = "anio")
DNRAP <- DNRAP %>% 
         filter(!is.na(value))

DNRAP <- DNRAP %>% 
         mutate(`Desc. marca` = tolower(`Desc. marca`),
                `Desc. Modelo` = stri_replace_all_regex(tolower(`Desc. Modelo`), "-"," "),
                puertas = stri_extract_all_regex(`Desc. Tipo`,"[:digit:]" ))

# Pongo el modelo como lo tengo en la base

marcas_existentes <- unique(DNRAP$`Desc. marca`)
DNRAP_2 <- DNRAP[0,]
for (i in marcas_existentes) {
  print(i)  
  
  autos_temp <- DNRAP %>% filter(`Desc. marca` == i)  
  modelos_temp <- marca_modelo_autos %>% filter(marca == i) 
  
  autos_temp <-  autos_temp %>% mutate(modelo = as.character(stri_extract_first_regex(`Desc. Modelo`, paste(modelos_temp$modelo, collapse = "|"))))
  DNRAP_2 <-  rbind(DNRAP_2, autos_temp)
}
DNRAP <- DNRAP_2
rm(autos_temp,modelos_temp,DNRAP_2)

# Arreglo c3 aircross por aircross
DNRAP <- mutate(DNRAP,temp = stri_detect_fixed(`Desc. Modelo`, "c3 aircross"))
DNRAP[DNRAP[,"temp"] == T, "modelo"] <- "aircross"
DNRAP <- select(DNRAP,-temp)

# Meto el precio en la base. 

DNRAP$anio <- as.numeric(as.character(DNRAP$anio))
autos_meli <- mutate(autos_meli, precio_dnrpa = -1, importado_nacional = "nd", otro_anio_dnrpa = 0)

for (i in 1:nrow(autos_meli)) {
print(i)
  
  var.marca <- autos_meli$marca[i]
  var.modelo <- autos_meli$modelo[i]
  var.anio <- autos_meli$ano[i]
  var.puertas <- autos_meli$puertas[i]
  
  DNRAP_TEMP <- DNRAP %>% 
                filter(`Desc. marca` == var.marca, modelo == var.modelo, anio == var.anio)
  
  if(nrow(DNRAP_TEMP) == 1){
    autos_meli[i,"precio_dnrpa"] <- DNRAP_TEMP[1,"value"]
    autos_meli[i,"importado_nacional"] <- DNRAP_TEMP[1,"I/N"]
  
    } else if(nrow(DNRAP_TEMP) > 1){
    
    DNRAP_TEMP <- filter(DNRAP_TEMP, puertas == var.puertas)
    
    if (nrow(DNRAP_TEMP) == 1){
      autos_meli[i,"precio_dnrpa"] <- DNRAP_TEMP[1,"value"]
      autos_meli[i,"importado_nacional"] <- DNRAP_TEMP[1,"I/N"]

    } else if (nrow(DNRAP_TEMP) > 1){
      
      DNRAP_TEMP <- mutate(DNRAP_TEMP, en_titulo = stri_detect_fixed(autos_meli$titulo[i], paste0(DNRAP_TEMP$`Desc. Modelo`)))
      
      if(sum(DNRAP_TEMP$en_titulo)>0){
        DNRAP_TEMP <- filter(DNRAP_TEMP, en_titulo == TRUE)
        autos_meli[i,"precio_dnrpa"] <- DNRAP_TEMP[1,"value"]
        autos_meli[i,"importado_nacional"] <- DNRAP_TEMP[1,"I/N"]
  
      } else {
        autos_meli[i,"precio_dnrpa"] <- mean(DNRAP_TEMP$value)
        autos_meli[i,"importado_nacional"] <- DNRAP_TEMP[1,"I/N"]

      }
    } else { 
      
      DNRAP_TEMP <- DNRAP %>% 
        filter(`Desc. marca` == var.marca, modelo == var.modelo, anio == var.anio)
      
      if (nrow(DNRAP_TEMP) == 1){
        autos_meli[i,"precio_dnrpa"] <- DNRAP_TEMP[1,"value"]
        autos_meli[i,"importado_nacional"] <- DNRAP_TEMP[1,"I/N"]
  
      } else if (nrow(DNRAP_TEMP) > 1){
        
        DNRAP_TEMP <- mutate(DNRAP_TEMP, en_titulo = stri_detect_fixed(autos_meli$titulo[i], paste0(DNRAP_TEMP$`Desc. Modelo`)))
        
        if(sum(DNRAP_TEMP$en_titulo)>0){
          DNRAP_TEMP <- filter(DNRAP_TEMP, en_titulo == TRUE)
          autos_meli[i,"precio_dnrpa"] <- DNRAP_TEMP[1,"value"]
          autos_meli[i,"importado_nacional"] <- DNRAP_TEMP[1,"I/N"]
     
        } else {
          autos_meli[i,"precio_dnrpa"] <- mean(DNRAP_TEMP$value)
          autos_meli[i,"importado_nacional"] <- DNRAP_TEMP[1,"I/N"]
       
        }
      }
    }
    } else {
      
      DNRAP_TEMP <- DNRAP %>% 
                    filter(`Desc. marca` == var.marca, modelo == var.modelo)
      
      if (nrow(DNRAP_TEMP) == 1){
        autos_meli[i,"precio_dnrpa"] <- DNRAP_TEMP[1,"value"]
        autos_meli[i,"importado_nacional"] <- DNRAP_TEMP[1,"I/N"]
        autos_meli[i, "otro_anio_dnrpa"] <- 1
        
      } else if (nrow(DNRAP_TEMP) > 1){
        
        DNRAP_TEMP <- mutate(DNRAP_TEMP, en_titulo = stri_detect_fixed(autos_meli$titulo[i], paste0(DNRAP_TEMP$`Desc. Modelo`)))
        
        if(sum(DNRAP_TEMP$en_titulo)>0){
          DNRAP_TEMP <- filter(DNRAP_TEMP, en_titulo == TRUE)
          DNRAP_TEMP <- filter(DNRAP_TEMP, anio == max(DNRAP_TEMP$anio))
          autos_meli[i,"precio_dnrpa"] <- DNRAP_TEMP[1,"value"]
          autos_meli[i,"importado_nacional"] <- DNRAP_TEMP[1,"I/N"]
          autos_meli[i, "otro_anio_dnrpa"] <- 1
          
        } else {
          
          DNRAP_TEMP <- filter(DNRAP_TEMP, anio == max(DNRAP_TEMP$anio))
          autos_meli[i,"precio_dnrpa"] <- mean(DNRAP_TEMP$value)
          autos_meli[i,"importado_nacional"] <- DNRAP_TEMP[1,"I/N"]
          autos_meli[i, "otro_anio_dnrpa"] <- 1
        }
      }
    } 
}

# hago dummie la variable importado 

autos_meli <- autos_meli %>% 
              mutate(importado = ifelse(importado_nacional == "I",1,0)) %>% 
              select(!importado_nacional)

# Variable cantidad de modelos (marca_modelo) y (marca_modelo_anio) --------------------------------------
marca_modelo_autos <- autos_meli %>% 
                      group_by(marca_modelo) %>% 
                      tally()
colnames(marca_modelo_autos)[2] <- "cantidad_modelos_iguales"
autos_meli <- left_join(autos_meli,marca_modelo_autos, by ="marca_modelo")


marca_modelo_ano_autos <- autos_meli %>% 
                          group_by(marca_modelo_ano) %>% 
                          tally()

colnames(marca_modelo_ano_autos)[2] <- "cantidad_modelos_anio_iguales"
autos_meli <- left_join(autos_meli, marca_modelo_ano_autos, by= "marca_modelo_ano")

# Creo una variable que indica las marcas de alta gamma  --------------------------------------------------

marcas_alta_gamma = c("bmw","chrysler","jeep","audi","mercedes benz","alfa romeo","mini","smart")

autos_meli <- autos_meli %>% 
              mutate(alta_gamma = ifelse(marca %in% marcas_alta_gamma,1,0))

# kilometros por anio -------------------------------------------------------------------------------------

autos_meli <- autos_meli %>% 
              mutate(kilometros_por_anio = kilometros/(2020 - ano))

#----------------------------------------------------------------------------------------------------------------------------------

# Elimino variables que no se van a utilizar ---------------------------------------------- 
autos_meli <- autos_meli %>% 
              select(-url,-titulo,-descripcion,-marca_modelo_ano,-marca_modelo,-color,-altura,-largo, -ancho,-`distancia entre ejes`,-`capacidad del tanque`)
autos_meli <- autos_meli %>% filter(!is.na(motor))
write.csv2(autos_meli, "autos_meli_2.csv", row.names = F,na="")
