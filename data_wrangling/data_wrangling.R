library(tidyverse)
library(sf)
library(lubridate)


##################
# Database Setup #
##################

# OBJECTIVE: In this script we are going to work on building the database for machine learning.
#            The topic will be insecurity and will focus on crimes committed
#            with motorcycles in the City of Buenos Aires.

# Our main source of information will be the crime databases of CABA. You can find them at https://data.buenosaires.gob.ar/dataset/delitos


# To start loading the data, let's create an auxiliary function that takes
# a path, applies the read_excel function to it, and iterates over a vector
# of sheets to read.
read_many_csv <- function(x) {
  
  map(x,
      ~ read_csv(x))
  
}

# Now, let's create a list of files to load.
archivos <- list.files("data/") %>% 
  tibble(archivos = .) %>% 
  filter(str_detect(archivos, "delitos")) %>% 
  mutate(ruta = paste0("data/", archivos))

# Load the databases
delito <- archivos %>% 
  mutate(anios = map(.x = ruta, # Vector over which the following function will iterate.
                     .f = ~ read_csv(ruta,
                                     na = "NULL")) # Fixed arguments.
  )

# Join into a single dataset
delito <- bind_rows(delito$anios)

# Look at the data
# delito %>% head(10) %>% view()

# Remove columns we will not use.
delito <- delito %>% 
  select(-cantidad)

# Keep only crimes that were committed on a motorcycle and have a georeferenced coordinate.
delito <- delito %>% 
  filter(uso_moto == "SI") %>% 
  drop_na(latitud, longitud) #581499

# Create a couple of extra variables.
delito <- delito %>% 
  mutate(comuna = paste("Comuna", comuna), # Paste the label "Comuna" to the Comuna number.
         uso_arma = if_else(uso_arma == "SI", # Translate weapon use
                            "yes",
                            "no"),
         subtipo = case_when(subtipo == "Robo total" ~ "Theft", # Translate robbery types.
                             subtipo == "Robo automotor" ~ "Car Theft",
                             T ~ subtipo),
         franja = as.numeric(franja), # Transform the franja variable to numeric to perform calculations.
         # Create the "Business cycle" variable
         ciclo_laboral = case_when(mes %in% c("ENERO", "FEBRERO", "DICIEMBRE") ~ "Holidays",
                                   T ~ "Workdays"),
         # Create the "Weekend" variable
         fin_de_semana = case_when(dia %in% c("SABADO", "DOMINGO") ~ "yes",
                                   T ~ "no"),
         # Create the "Night" variable
         noche = case_when(franja < 7 | franja >= 18 ~ "yes",
                           T ~ "no"),
         # Create the "Banking hours" variable
         horario_bancario = case_when((franja >= 10 & franja < 18) & fin_de_semana == "no" ~ "yes",
                                      T ~ "no"),
         fecha = ymd(fecha), # Transform the fecha variable into a date.
         day_month = day(fecha), # Extract the day of the month.
         # Create a variable that determines the first day of the month.
         principo_mes = if_else(day_month <= 7, 
                                "yes",
                                "no"),
         # Create a variable that determines the last week of the month
         fin_de_mes = if_else(day_month >= 24,
                              "yes",
                              "no"),
         mes = recode(str_to_lower(mes),
                      "enero"      = "January",
                      "febrero"    = "February",
                      "marzo"      = "March",
                      "abril"      = "April",
                      "mayo"       = "May",
                      "junio"      = "June",
                      "julio"      = "July",
                      "agosto"     = "August",
                      "septiembre" = "September",
                      "octubre"    = "October",
                      "noviembre"  = "November",
                      "diciembre"  = "December"),
         dia = recode(str_to_lower(dia),
                      "lunes"     = "Monday",
                      "martes"    = "Tuesday",
                      "miercoles" = "Wednesday",  
                      "jueves"    = "Thursday",
                      "viernes"   = "Friday",
                      "sabado"    = "Saturday",   
                      "domingo"   = "Sunday"))


# Transform our dataset into spatial
delito <- delito %>%
  st_as_sf(coords = c("longitud", "latitud"),
           crs = "4326")

# Set the coordinate system 4326 to perform spatial operations.
delito <- delito %>%
  st_set_crs(4326)

# Load the geometries of CABA's neighborhoods
caba <- read_sf("data/barrios.geojson") %>% 
  st_transform(st_crs(delito)) %>% 
  st_make_valid()

# Keep only cases that fall within CABA
delito <- st_filter(delito,
                    caba,
                    .predicate = st_intersects) #581265

# Ensure unique records
# Keep only unique records.
delito <- delito %>%
  distinct() #64585

# Set CABA's coordinate system.
crs_ba <- 5347 # Posgar 2007 - Argentina 5.

# Convert our CRS of the crimes to apply precise spatial calculations.
delito <- delito %>% 
  st_transform(st_crs(crs_ba))

# Now, we are going to enrich our data with other sources of information.

# Police stations
# Source: BA DATA
# Can be found at https://data.buenosaires.gob.ar/dataset/comisarias-policia-ciudad
comisarias <- read_sf("data/comisarias_policia.geojson") %>% 
  st_transform(st_crs(delito)) %>% 
  select(1:2)

# Load the city street directory
# Source: BA DATA
# Can be found at https://data.buenosaires.gob.ar/dataset/calles
callejero <- read_sf("data/callejero.geojson") %>% 
  select(nomoficial, tipo_c) %>% 
  st_transform(st_crs(delito)) %>% 
  # Reduce types
  mutate(tipo_c = case_when(str_detect(tipo_c, "AUTOPISTA") ~ "Highway",
                            str_detect(tipo_c, "PASAJE") ~ "Street",
                            str_detect(tipo_c, "CALLE") ~ "Street",
                            tipo_c == "BOULEVARD" ~ "Avenue",
                            tipo_c == "AVENIDA" ~ "Avenue",
                            tipo_c == "SENDERO" ~ "Path",
                            tipo_c == "PUENTE" ~ "Other",
                            tipo_c == "TÚNEL" ~ "Other",
                            T ~ tipo_c))

# summary(as.factor(callejero$tipo_c))

# ATMs
# Source: BA DATA
# Can be found at https://data.buenosaires.gob.ar/dataset/cajeros-automaticos
cajeros <- read.csv("data/cajeros-automaticos.csv") %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) %>% 
  st_transform(st_crs(delito)) %>% 
  filter(localidad == "CABA") %>% 
  select(banco, red, terminales)

# Banks
# Source: BA DATA
# Can be found at https://data.buenosaires.gob.ar/dataset/bancos
bancos <- read.csv("data/bancos.csv", 
                   sep = ";",
                   encoding = "latin1") %>% 
  # We have to correct its coordinates.
  mutate(across(.cols = c("long", "lat"),
                ~ . %>%
                  str_remove_all("[:punct:]")))

str_sub(bancos$lat, end = 2, omit_na = FALSE) <- "34."; bancos$lat
str_sub(bancos$long, end = 2, omit_na = FALSE) <- "58."; bancos$long

bancos <- bancos %>% 
  mutate(across(.cols = c("long", "lat"),
                ~ . %>%
                  as.numeric()*-1)) %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326)

bancos <- bancos %>% 
  st_transform(st_crs(delito))

# We are going to calculate distances to different key infrastructures.
# Police stations
idx <- st_nearest_feature(delito, comisarias)

distancias <- st_distance(delito, comisarias[idx, ], by_element = TRUE)

delito$dist_comisaria <- distancias    


# ATMs
idx <- st_nearest_feature(delito, cajeros)

distancias <- st_distance(delito, cajeros[idx, ], by_element = TRUE)

delito$dist_cajeros <- distancias


# Banks
idx <- st_nearest_feature(delito, bancos)

distancias <- st_distance(delito, bancos[idx, ], by_element = TRUE)

delito$dist_bancos <- distancias

# Add info from the nearest ATM
delito <- 
  st_join(
    delito,
    cajeros,
    join = st_nearest_feature,
    suffix = c("", "_nearest")
  )

# Add info about the type of street where the crime occurred 
delito <- st_join(
  delito,
  callejero,
  join = st_nearest_feature,
  suffix = c("", "_nearest")
)

# Generate some distance thresholds
delito <- delito %>% 
  # First, transform our variables into numeric.
  mutate(across(.cols = c("dist_cajeros", "dist_comisaria", "dist_bancos"), 
                ~ . %>% as.numeric()),
         # Crime within 100m of an ATM
         cajeros_a_100 = if_else(dist_cajeros <= 100,
                                 "yes",
                                 "no"),
         # Crime within 200m of an ATM
         cajeros_a_200 = if_else(dist_cajeros <= 200,
                                 "yes",
                                 "no"),
         # Crime within 100m of a police station
         comisarias_a_100 = if_else(dist_comisaria <= 100,
                                    "yes",
                                    "no"),
         # Crime within 200m of a police station
         comisarias_a_200 = if_else(dist_comisaria <= 200,
                                    "yes",
                                    "no"),
         # Crime within 100m of a bank
         bancos_a_100 = if_else(dist_bancos <= 100,
                                "yes",
                                "no"),
         # Crime within 200m of a bank
         bancos_a_200 = if_else(dist_bancos <= 200,
                                "yes",
                                "no"))

# Finally, we load our dataset of the square meter price of apartment sales by neighborhood. Source: https://www.lanacion.com.ar/propiedades/casas-y-departamentos/mapa-interactivo-los-precios-de-venta-y-de-alquiler-de-las-propiedades-en-cada-barrio-porteno-nid22022026/ 
precio <- read.csv("data/precios_m2_caba.csv") %>% 
  mutate(barrio = str_to_upper(barrio),
         barrio = herramientas::remover_tildes(barrio)) %>% 
  select(-unit)

# Apply some changes to the crime dataset to be able to make the join
delito <- delito %>% 
  mutate(barrio = str_remove_all(barrio, "[:punct:]"),
         barrio = case_when(barrio == "BOCA" ~ "LA BOCA",
                            barrio == "VILLA LUGANO" ~ "LUGANO",
                            barrio == "VILLA SANTA RITA" ~ "SANTA RITA",
                            T ~ barrio)) %>% 
  # and Join
  left_join(precio, by = "barrio")

# Load the 2010 census tract dataset.
# Source: BA DATA
# Can be found at https://data.buenosaires.gob.ar/dataset/informacion-censal-por-radio
radios <- read_sf("data/informacion-censal-por-radio-2010/informacion_censal_por_radio_2010_wgs84.shp") %>% 
  st_make_valid() %>% 
  st_transform(st_crs(delito))

# Keep the density and number of households with unmet basic needs.
radios <- radios %>% 
  mutate(area = as.numeric(st_area(.))/1000000,
         densidad_pob = TOTAL_POB/area,
         pct_hogares_nbi = H_CON_NBI/T_HOGAR,
         pct_hogares_nbi = if_else(TOTAL_POB == 0,
                                   0,
                                   pct_hogares_nbi)) %>% 
  select(densidad_pob, pct_hogares_nbi)

# Join each crime with its corresponding census tract.
delito <- delito %>% 
  st_join(radios) # It is not a perfect join. Check.

# Keep only the variables of interest and translate them to English.
delito <- delito %>% 
  select("year" = anio, 
         "month" = mes, 
         "business_cycle" = ciclo_laboral,
         "day" = day_month, 
         "day_of_week" = dia,
         "weekend" = fin_de_semana,
         "start_of_month" = principo_mes,
         "end_of_month" = fin_de_mes,
         "hour" = franja,
         "night" = noche,
         "banking_hours" = horario_bancario,
         "crime_type" = subtipo,
         "weapon_use" = uso_arma,
         "street_type" = tipo_c,
         "atms_within_100m" = cajeros_a_100,
         "atms_within_200m" = cajeros_a_200,
         "atm_bank" = banco,
         "atm_network" = red,
         "banks_within_100m" = bancos_a_100,
        "banks_within_200m" = bancos_a_200,
         "police_stations_within_100m" =comisarias_a_100,
         "police_stations_within_200m" = comisarias_a_200,
         "price_per_sqm_usd" = value,
         "population_density" = densidad_pob,
         "pct_households_nbi" =  pct_hogares_nbi,
        "neighbourhood"  = barrio,
         "comune" = comuna)


# Save.
delito %>% 
  st_drop_geometry() %>% 
  write_csv("data/crime_data.csv")


