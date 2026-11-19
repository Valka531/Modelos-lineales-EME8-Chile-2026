# ==========================================
# PREGUNTA 1: AUDITORIA DE LA EXPOSICION
# VIII ENCUESTA DE MICROEMPRENDIMIENTO
# ==========================================

# Cargar librería
library(tidyr)
library(dplyr)
library(ggplot2)
library(lmtest)
library(car)
library(sandwich)

# Cargar base de datos original
base <- read.csv(
  "C:/Users/ferna/OneDrive/Desktop/Fila_A/03_Datos_EME8/EME8_base_full_original.csv",
  header = TRUE,
  check.names = FALSE
)

# Cargar archivo de particiones
particiones <- read.csv(
  "C:/Users/ferna/OneDrive/Desktop/Fila_A/03_Datos_EME8/ids_particiones_pliegues.csv",
  header = TRUE
)

# Revisar nombres de las columnas
names(base)
names(particiones)

# Eliminar primera columna con nombre vacío
base <- base %>%
  select(-1)

# Asegurar mismo tipo de identificador
base$id <- as.character(base$id)
particiones$id <- as.character(particiones$id)

# Verificar coincidencia de identificadores
sum(base$id %in% particiones$id)
sum(!base$id %in% particiones$id)

# Verificar identificadores duplicados en particiones
anyDuplicated(particiones$id)

# ------------------------------------------
# A) UNIVERSO, CLAVES Y MUESTRA COMUN
# ------------------------------------------

# Dimenciones base original
dim(base)

# Dimeciones particiones
dim(particiones)

# ID duplicados base:
sum(duplicated(base$id))

#ID duplicados particiones:
sum(duplicated(particiones$id))

# Verificar identificadores sin correspondencia
# ID particiones no encontrados:
sum(!particiones$id %in% base$id)

# ID base no incluidos en particiones:
sum(!base$id %in% particiones$id)

# Construir muestra común
muestra <- inner_join(
  particiones,
  base,
  by = "id"
)

# Dimensiones muestra comun:
dim(muestra)

# Distribución de particiones
print(table(muestra$particion))

# Distribución de pliegues
print(table(muestra$fold, useNA = "ifany"))

# Revisar tipos de variables
variables <- c(
  "id", "region", "sexo",
  "m2_1", "m2_2", "m2_3",
  "m2_4", "m2_5", "ganancia_final"
)

print(sapply(muestra[variables], class))

# ------------------------------------------
# B) RECODIFICACION AMENAZAS NATURALES
# ------------------------------------------

amenazas <- c("m2_1", "m2_2", "m2_3", "m2_4")

muestra <- muestra %>%
  mutate(
    across(
      all_of(amenazas),
      ~ case_when(
        .x == 1 ~ 1L,
        .x == 2 ~ 0L,
        TRUE ~ NA_integer_
      ),
      .names = "{.col}_bin"
    )
  )

amenazas_bin <- paste0(amenazas, "_bin")

# Frecuencias de variables recodificadas
for (v in amenazas_bin) {
  print(paste("Variable:", v))
  print(table(muestra[[v]], useNA = "ifany"))
}

# Tabla de porcentajes de amenazas declaradas
for (v in amenazas_bin) {
  print(paste("Porcentajes:", v))
  print(round(prop.table(
    table(muestra[[v]], useNA = "ifany")
  ) * 100, 2))
}

# ------------------------------------------
# C) ALGUNA AMENAZA
# ------------------------------------------

muestra <- muestra %>%
  mutate(
    alguna_amenaza = case_when(
      if_any(all_of(amenazas_bin), ~ .x == 1) ~ 1L,
      if_all(all_of(amenazas_bin), ~ .x == 0) ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# Frecuencias
print(table(muestra$alguna_amenaza, useNA = "ifany"))

# Cantidad de amenazas declaradas
muestra$n_amenazas <- rowSums(
  muestra[, amenazas_bin],
  na.rm = FALSE
)

print(table(muestra$n_amenazas, useNA = "ifany"))

# Revisar consistencia de m2_5
print(table(
  muestra$m2_5,
  muestra$alguna_amenaza,
  useNA = "ifany"
))

# Identificar inconsistencias
inconsistencia_1 <- sum(
  muestra$m2_5 == 1 &
    muestra$alguna_amenaza == 1,
  na.rm = TRUE
)

inconsistencia_2 <- sum(
  muestra$m2_5 == 2 &
    muestra$alguna_amenaza == 0,
  na.rm = TRUE
)

print(paste("Ninguno = Si, pero declaro amenazas:", inconsistencia_1))
print(paste("Ninguno = No, pero no declaro amenazas:", inconsistencia_2))

# Revisar indicadores de no respuesta
print(table(muestra$m2_88, useNA = "ifany"))
print(table(muestra$m2_99, useNA = "ifany"))

# ------------------------------------------
# REVISION DE GANANCIA FINAL
# ------------------------------------------

summary(muestra$ganancia_final)

# Ganancias negativas:
sum(muestra$ganancia_final < 0, na.rm = TRUE)

# Ganancias iguales a cero:
sum(muestra$ganancia_final == 0, na.rm = TRUE)

# Ganancias positivas:
sum(muestra$ganancia_final > 0, na.rm = TRUE)

# Valores faltantes:
sum(is.na(muestra$ganancia_final))

# ==========================================
# PREGUNTA 2: EDA DEL PROBLEMA AMBIENTAL
# ==========================================

# Preparar identificadores
base$id <- as.character(base$id)
particiones$id <- as.character(particiones$id)

# Construir muestra comun
muestra <- inner_join(
  particiones,
  base,
  by = "id"
)

# ------------------------------------------
# PREPARACION DE VARIABLES
# ------------------------------------------

amenazas <- c("m2_1", "m2_2", "m2_3", "m2_4")

# Recodificar amenazas
muestra <- muestra %>%
  mutate(
    across(
      all_of(amenazas),
      ~ case_when(
        .x == 1 ~ 1L,
        .x == 2 ~ 0L,
        TRUE ~ NA_integer_
      ),
      .names = "{.col}_bin"
    )
  )

amenazas_bin <- paste0(amenazas, "_bin")

# Construir alguna_amenaza
muestra <- muestra %>%
  mutate(
    alguna_amenaza = case_when(
      if_any(all_of(amenazas_bin), ~ .x == 1) ~ 1L,
      if_all(all_of(amenazas_bin), ~ .x == 0) ~ 0L,
      TRUE ~ NA_integer_
    )
  )

muestra <- muestra %>%
  mutate(
    grupo_amenaza = factor(
      alguna_amenaza,
      levels = c(0, 1),
      labels = c("Sin amenazas", "Con amenazas")
    )
  )

# ==========================================
# A) DISTRIBUCION DE GANANCIAS
# ==========================================

# Estadisticos descriptivos
tabla_ganancias <- muestra %>%
  group_by(grupo_amenaza) %>%
  summarise(
    n = sum(!is.na(ganancia_final)),
    media = mean(ganancia_final, na.rm = TRUE),
    mediana = median(ganancia_final, na.rm = TRUE),
    Q1 = quantile(ganancia_final, 0.25, na.rm = TRUE),
    Q3 = quantile(ganancia_final, 0.75, na.rm = TRUE),
    minimo = min(ganancia_final, na.rm = TRUE),
    maximo = max(ganancia_final, na.rm = TRUE),
    .groups = "drop"
  )

print(tabla_ganancias)

# Diagrama de cajas
ggplot(
  muestra,
  aes(x = grupo_amenaza, y = ganancia_final)
) +
  geom_boxplot(
    fill = "#800BF4",
    color = "#000000"
  ) +
  labs(
    title = "Distribucion de ganancias mensuales",
    subtitle = "Comparacion entre negocios con y sin amenazas",
    x = "Declaracion de amenazas naturales",
    y = "Ganancia mensual (CLP)"
  ) +
  theme_minimal()

# Version ampliada para observar mejor la zona central
# No elimina observaciones: solamente limita la ventana visual.
limites <- quantile(
  muestra$ganancia_final,
  c(0.01, 0.99),
  na.rm = TRUE
)

ggplot(
  muestra,
  aes(x = grupo_amenaza, y = ganancia_final)
) +
  geom_boxplot(fill = "#800BF4") +
  coord_cartesian(ylim = limites) +
  labs(
    title = "Distribucion central de las ganancias",
    subtitle = "Ventana visual entre los percentiles 1 y 99",
    x = "Grupo",
    y = "Ganancia mensual (CLP)"
  ) +
  theme_minimal()


# ==========================================
# B) FRECUENCIAS DE AMENAZAS
# ==========================================

# Etiquetas de amenazas
etiquetas <- c(
  m2_1_bin = "Sequias y heladas",
  m2_2_bin = "Inundaciones y aluviones",
  m2_3_bin = "Incendios forestales",
  m2_4_bin = "Tormentas, lluvias y vientos"
)

# Crear tabla larga
amenazas_largas <- muestra %>%
  select(all_of(amenazas_bin)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "amenaza",
    values_to = "declaracion"
  ) %>%
  filter(!is.na(declaracion)) %>%
  mutate(
    amenaza = dplyr::recode(amenaza, !!!etiquetas)
  )

# Tabla de frecuencias
frecuencias <- amenazas_largas %>%
  group_by(amenaza) %>%
  summarise(
    frecuencia = sum(declaracion == 1),
    total = n(),
    porcentaje = mean(declaracion == 1) * 100,
    .groups = "drop"
  )

print(frecuencias)

# Grafico de frecuencias
ggplot(
  frecuencias,
  aes(x = reorder(amenaza, frecuencia),
      y = frecuencia)
) +
  geom_col(fill = "#0BF4A6") +
  coord_flip() +
  labs(
    title = "Frecuencia de amenazas naturales",
    x = "Amenaza natural",
    y = "Numero de declaraciones"
  ) +
  theme_minimal()

# ==========================================
# CRUCE POR SECTOR ECONOMICO
# ==========================================

# Etiquetas de los sectores
muestra <- muestra %>%
  mutate(
    sector = factor(
      c1_b,
      levels = 1:6,
      labels = c(
        "Agropecuario y forestal",
        "Manufactura",
        "Construccion",
        "Comercio",
        "Transporte",
        "Otros"
      )
    )
  )

# Frecuencia de alguna amenaza por sector
tabla_sector <- muestra %>%
  group_by(sector) %>%
  summarise(
    total = n(),
    con_amenazas = sum(alguna_amenaza == 1),
    porcentaje = mean(alguna_amenaza == 1) * 100,
    .groups = "drop"
  )

print(tabla_sector)

# Grafico de porcentaje por sector
ggplot(
  tabla_sector,
  aes(x = reorder(sector, porcentaje),
      y = porcentaje)
) +
  geom_col(fill = "#0BF4A6") +
  coord_flip() +
  labs(
    title = "Negocios con amenazas por sector economico",
    x = "Sector economico",
    y = "Negocios con amenazas (%)"
  ) +
  theme_minimal()

# ------------------------------------------
# CADA AMENAZA SEGUN SECTOR ECONOMICO
# ------------------------------------------

# Preparar datos
amenazas_sector <- muestra %>%
  select(sector, all_of(amenazas_bin)) %>%
  tidyr::pivot_longer(
    cols = all_of(amenazas_bin),
    names_to = "amenaza",
    values_to = "declaracion"
  ) %>%
  filter(!is.na(declaracion)) %>%
  mutate(
    amenaza = dplyr::recode(amenaza, !!!etiquetas)
  ) %>%
  group_by(sector, amenaza) %>%
  summarise(
    frecuencia = sum(declaracion == 1),
    porcentaje = mean(declaracion == 1) * 100,
    .groups = "drop"
  )

# Revisar resultados
print(amenazas_sector)

# Grafico de amenazas por sector
ggplot(
  amenazas_sector,
  aes(x = sector, y = porcentaje,
      fill = amenaza)
) +
  geom_col(position = "dodge") +
  labs(
    title = "Amenazas naturales segun sector economico",
    x = "Sector economico",
    y = "Porcentaje de declaraciones (%)",
    fill = "Amenaza"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 35,
      hjust = 1
    )
  )


# ==========================================
# C) VARIACION REGIONAL
# ==========================================

# Etiquetas de las regiones
nombres_regiones <- c(
  "Tarapaca",
  "Antofagasta",
  "Atacama",
  "Coquimbo",
  "Valparaiso",
  "O'Higgins",
  "Maule",
  "Biobio",
  "Araucania",
  "Los Lagos",
  "Aysen",
  "Magallanes",
  "Metropolitana",
  "Los Rios",
  "Arica y Parinacota",
  "Nuble"
)

# Asignar nombres a las regiones
muestra <- muestra %>%
  mutate(
    region_nombre = factor(
      region,
      levels = 1:16,
      labels = nombres_regiones
    )
  )

# ------------------------------------------
# TABLA DE VARIACION REGIONAL
# ------------------------------------------

tabla_region <- muestra %>%
  group_by(region_nombre) %>%
  summarise(
    negocios = n(),
    con_amenazas = sum(alguna_amenaza == 1),
    porcentaje_amenazas =
      mean(alguna_amenaza == 1) * 100,
    mediana_ganancia =
      median(ganancia_final, na.rm = TRUE),
    media_ganancia =
      mean(ganancia_final, na.rm = TRUE),
    .groups = "drop"
  )

print(tabla_region)

# ------------------------------------------
# GRAFICO DE AMENAZAS POR REGION
# ------------------------------------------

ggplot(
  tabla_region,
  aes(
    x = reorder(region_nombre, porcentaje_amenazas),
    y = porcentaje_amenazas
  )
) +
  geom_col(fill = "#F40B55") +
  coord_flip() +
  labs(
    title = "Declaracion de amenazas por region",
    x = "Region",
    y = "Negocios con amenazas (%)"
  ) +
  theme_minimal()

# ------------------------------------------
# GRAFICO DE MEDIANAS DE GANANCIAS
# ------------------------------------------

ggplot(
  tabla_region,
  aes(
    x = reorder(region_nombre, mediana_ganancia),
    y = mediana_ganancia
  )
) +
  geom_col(fill = "#079C4F") +
  coord_flip() +
  labs(
    title = "Mediana de ganancias mensuales por region",
    x = "Region",
    y = "Mediana de ganancias (CLP)"
  ) +
  theme_minimal()

# ------------------------------------------
# COMPARACION DE DISTRIBUCIONES REGIONALES
# ------------------------------------------

ggplot(
  muestra,
  aes(
    x = region_nombre,
    y = ganancia_final
  )
) +
  geom_boxplot(fill = "#0BA6F4") +
  coord_flip(
    ylim = quantile(
      muestra$ganancia_final,
      c(0.01, 0.99),
      na.rm = TRUE
    )
  ) +
  labs(
    title = "Distribucion de ganancias por region",
    x = "Region",
    y = "Ganancia mensual (CLP)"
  ) +
  theme_minimal()


# ============================================================
# PREGUNTA 3: MODELO OLS Y DIAGNOSTICO DE SUPUESTOS
# ============================================================

# ------------------------------------------------------------
# 1. LECTURA DE LOS DATOS
# ------------------------------------------------------------

# Unir la base con las particiones mediante el identificador
muestra <- inner_join(particiones, base, by = "id")

# Conservar exclusivamente las observaciones de entrenamiento
train <- muestra %>%
  filter(particion == "entrenamiento")

nrow(train)

# ------------------------------------------------------------
# 2. RECODIFICACION DE LAS VARIABLES DE AMENAZAS
# ------------------------------------------------------------
# En la encuesta:
# 1 = Si
# 2 = No
# Otros valores = No disponibles / no clasificables

amenazas <- c("m2_1", "m2_2", "m2_3", "m2_4")

train <- train %>%
  mutate(
    across(
      all_of(amenazas),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      .names = "{.col}_bin"
    )
  )

# Variables creadas:
# m2_1_bin = Sequias y heladas
# m2_2_bin = Inundaciones y aluviones
# m2_3_bin = Incendios forestales
# m2_4_bin = Tormentas, lluvias y vientos


# ------------------------------------------------------------
# 3. PREPARACION DE VARIABLES
# ------------------------------------------------------------

# Las variables categoricas se convierten en factores.
# Las variables numericas permanecen como numericas.

train <- train %>%
  mutate(
    c1_b = factor(c1_b),
    sexo = factor(sexo),
    region = factor(region),
    informalidad = factor(informalidad)
  )

# Variables que se utilizaran en el modelo
vars_modelo <- c(
  "ganancia_final",
  "m2_1_bin",
  "m2_2_bin",
  "m2_3_bin",
  "m2_4_bin",
  "c6_3",
  "c7",
  "total_trabajadores",
  "informalidad",
  "c1_b",
  "sexo",
  "region"
)

# Revisar cantidad de datos completos
sum(complete.cases(train[, vars_modelo]))

# Trabajar con observaciones completas
train_model <- train %>%
  filter(complete.cases(across(all_of(vars_modelo))))

# Observaciones usadas en los modelos
nrow(train_model)

# ------------------------------------------------------------
# 4. ESTIMACION DEL MODELO OLS COMPLETO
# ------------------------------------------------------------

modelo_ols <- lm(
  ganancia_final ~
    m2_1_bin +
    m2_2_bin +
    m2_3_bin +
    m2_4_bin +
    c6_3 +
    c7 +
    total_trabajadores +
    informalidad +
    c1_b +
    sexo +
    region,
  data = train_model
)

# Resumen del modelo
summary(modelo_ols)


# ------------------------------------------------------------
# 5. ERRORES ESTANDAR ROBUSTOS HC3
# ------------------------------------------------------------

# Se utilizan como complemento debido a la posible
# presencia de heterocedasticidad.

errores_robustos <- coeftest(
  modelo_ols,
  vcov. = vcovHC(modelo_ols, type = "HC3")
)

print(errores_robustos)

# ------------------------------------------------------------
# 6. GRAFICOS DE DIAGNOSTICO
# ------------------------------------------------------------

# 6.1 Residuos versus valores ajustados
plot(modelo_ols, which = 1)


# 6.2 Grafico Q-Q de residuos
plot(modelo_ols, which = 2)


# 6.3 Grafico escala-localizacion
plot(modelo_ols, which = 3)


# 6.4 Histograma de residuos
hist(
  residuals(modelo_ols),
  breaks = 50,
  main = "Histograma de residuos del modelo OLS",
  xlab = "Residuos (CLP)",
  ylab = "Frecuencia"
)

# ------------------------------------------------------------
# 7. PRUEBAS DE SUPUESTOS DEL MODELO
# ------------------------------------------------------------

# 7.1 Prueba de Breusch-Pagan
# H0: Homocedasticidad
# H1: Heterocedasticidad

bp <- bptest(modelo_ols)

# PRUEBA DE BREUSCH-PAGAN
print(bp)

# 7.2 PRUEBA DE JARQUE-BERA
# H0: Los residuos siguen una distribución normal
# H1: Los residuos no siguen una distribución normal

# Función Jarque-Bera (no requiere tseries)

jarque_bera <- function(residuos) {
  
  residuos <- residuos[is.finite(residuos)]
  
  n <- length(residuos)
  
  # Estandarizar residuos
  z <- (residuos - mean(residuos)) / sd(residuos)
  
  # Asimetría
  S <- mean(z^3)
  
  # Curtosis en exceso
  K <- mean(z^4) - 3
  
  # Estadístico Jarque-Bera
  JB <- (n / 6) * (S^2 + (K^2 / 4))
  
  # Valor p
  p_value <- pchisq(JB, df = 2, lower.tail = FALSE)
  
  resultado <- list(
    statistic = JB,
    p.value = p_value,
    skewness = S,
    excess_kurtosis = K,
    method = "Prueba de Jarque-Bera"
  )
  
  return(resultado)
}

# MODELO DE SOLO CONTROLES
modelo_controles <- lm(
  ganancia_final ~ c6_3 + c7 + total_trabajadores +
    informalidad + c1_b + sexo + region,
  data = train_model
)

# PRUEBA JARQUE-BERA: MODELO COMPLETO
jb_completo <- jarque_bera(residuals(modelo_ols))
print(jb_completo)

# PRUEBA JARQUE-BERA: MODELO SOLO CONTROLES
jb_controles <- jarque_bera(residuals(modelo_controles))
print(jb_controles)

# 7.3 Prueba RESET de Ramsey
# H0: El modelo esta correctamente especificado
# H1: Existe evidencia de mala especificacion

reset <- resettest(
  modelo_ols,
  power = 2,
  type = "fitted"
)

# PRUEBA RESET DE RAMSEY
print(reset)


# ------------------------------------------------------------
# 8. MULTICOLINEALIDAD: FACTOR DE INFLACION DE VARIANZA
# ------------------------------------------------------------

# Para variables categoricas, car::vif entrega GVIF,
# grados de libertad y GVIF ajustado.

vif_resultado <- vif(modelo_ols)

# RESULTADOS VIF / GVIF
print(vif_resultado)

# ------------------------------------------------------------
# 9. MODELO QUE INCLUYE SOLO VARIABLES DE CONTROL
# ------------------------------------------------------------

# Se estima un segundo modelo sin las variables de amenazas
# naturales para comparar su capacidad explicativa.

modelo_controles <- lm(
  ganancia_final ~
    c6_3 +
    c7 +
    total_trabajadores +
    informalidad +
    c1_b +
    sexo +
    region,
  data = train_model
)

# RESUMEN DEL MODELO SOLO CONTROLES
summary(modelo_controles)

# ------------------------------------------------------------
# 10. COMPARACION DE LOS DOS MODELOS
# ------------------------------------------------------------

# MODELO COMPLETO: AMENAZAS + CONTROLES

print(summary(modelo_ols))
print(bptest(modelo_ols))
print(jarque_bera(residuals(modelo_ols)))
print(resettest(modelo_ols, power = 2, type = "fitted"))

# MODELO SOLO CONTROLES

print(summary(modelo_controles))
print(bptest(modelo_controles))
print(jarque_bera(residuals(modelo_controles)))
print(resettest(modelo_controles, power = 2, type = "fitted"))

# Tabla resumen de comparacion
comparacion <- data.frame(
  Modelo = c(
    "Amenazas + controles",
    "Solo controles"
  ),
  R2 = c(
    summary(modelo_ols)$r.squared,
    summary(modelo_controles)$r.squared
  ),
  R2_ajustado = c(
    summary(modelo_ols)$adj.r.squared,
    summary(modelo_controles)$adj.r.squared
  ),
  AIC = c(
    AIC(modelo_ols),
    AIC(modelo_controles)
  ),
  BIC = c(
    BIC(modelo_ols),
    BIC(modelo_controles)
  )
)

# TABLA DE COMPARACION
print(comparacion)

# ------------------------------------------------------------
# 11. GRAFICO COMPARATIVO DE RESIDUOS
# ------------------------------------------------------------

par(mfrow = c(1, 2))

plot(
  fitted(modelo_ols),
  residuals(modelo_ols),
  main = "Amenazas + controles",
  xlab = "Valores ajustados",
  ylab = "Residuos"
)

abline(h = 0, lty = 2)

plot(
  fitted(modelo_controles),
  residuals(modelo_controles),
  main = "Solo controles",
  xlab = "Valores ajustados",
  ylab = "Residuos"
)

abline(h = 0, lty = 2)

# Restaurar configuracion grafica
par(mfrow = c(1, 1))


# ============================================================
# PREGUNTA 4 - TRANSFORMACIONES DE LA GANANCIA
# ============================================================

# ------------------------------------------------------------
# 1. PREPARACION DE LOS DATOS
# ------------------------------------------------------------

# Variables ambientales
amenazas <- c("m2_1", "m2_2", "m2_3", "m2_4")

amenazas_bin <- paste0(amenazas, "_bin")

# Recodificar amenazas si todavia no existen
for (v in amenazas) {
  
  nombre <- paste0(v, "_bin")
  
  if (!nombre %in% names(muestra)) {
    
    muestra[[nombre]] <- ifelse(
      muestra[[v]] == 1, 1,
      ifelse(muestra[[v]] == 2, 0, NA)
    )
    
  }
}


# Variables del modelo
vars_modelo <- c(
  "ganancia_final",
  amenazas_bin,
  "c6_3",
  "c7",
  "total_trabajadores",
  "informalidad",
  "c1_b",
  "sexo",
  "region"
)


# Separar entrenamiento y prueba
train <- muestra %>%
  filter(particion == "entrenamiento")

test <- muestra %>%
  filter(particion == "prueba")

# Observaciones de entrenamiento
nrow(train)
# Obsevaciones de prueba 
nrow(test)

# Convertir variables categoricas en factores
variables_factor <- c(
  "informalidad",
  "c1_b",
  "sexo",
  "region"
)

for (v in variables_factor) {
  
  train[[v]] <- factor(train[[v]])
  
  # Mantener los mismos niveles del entrenamiento
  test[[v]] <- factor(
    test[[v]],
    levels = levels(train[[v]])
  )
  
}


# Conservar observaciones completas
train_model <- train %>%
  filter(complete.cases(across(all_of(vars_modelo))))

test_model <- test %>%
  filter(complete.cases(across(all_of(vars_modelo))))

# Observaciones completas de entrenamientos
nrow(train_model)
# Observaciones completas de prueba
nrow(test_model)

# ------------------------------------------------------------
# 2. INCISO A: DOCUMENTAR PERDIDAS Y CEROS
# ------------------------------------------------------------

Y <- train_model$ganancia_final

# Resumen de la variable original
# DISTRIBUCION DE GANANCIA ORIGINAL
print(summary(Y))


# Contar ganancias negativas, cero y positivas
clasificacion_ganancia <- data.frame(
  Categoria = c(
    "Ganancias negativas",
    "Ganancias iguales a cero",
    "Ganancias positivas"
  ),
  Frecuencia = c(
    sum(Y < 0),
    sum(Y == 0),
    sum(Y > 0)
  )
)

clasificacion_ganancia$Porcentaje <-
  clasificacion_ganancia$Frecuencia /
  length(Y) * 100

print(clasificacion_ganancia)

# Porcentaje de observaciones que no admiten log(Y)
no_admitidas <- sum(Y <= 0)

print(paste(
  "Observaciones que impiden aplicar log(Y):",
  no_admitidas
))

print(paste(
  "Porcentaje que no admite log(Y):",
  round(no_admitidas / length(Y) * 100, 2),
  "%"
))


# Tabla descriptiva
tabla_ganancia <- data.frame(
  Estadistico = c(
    "Minimo",
    "Primer cuartil",
    "Mediana",
    "Media",
    "Tercer cuartil",
    "Maximo",
    "Desviacion estandar"
  ),
  Valor = c(
    min(Y),
    quantile(Y, 0.25),
    median(Y),
    mean(Y),
    quantile(Y, 0.75),
    max(Y),
    sd(Y)
  )
)

print(tabla_ganancia)


# Grafico de distribucion original
hist(
  Y,
  breaks = 60,
  main = "Distribucion de ganancias mensuales",
  xlab = "Ganancia mensual (CLP)",
  ylab = "Frecuencia"
)

abline(v = 0, lty = 2)

# ------------------------------------------------------------
# 3. INCISO B: CONSTRUIR TRANSFORMACIONES
# ------------------------------------------------------------

# Escala de referencia
escala <- 100000


# 3.1 Respuesta original
train_model$Y_original <- train_model$ganancia_final

test_model$Y_original <- test_model$ganancia_final

# 3.2 Transformacion asinh
train_model$Y_asinh <- asinh(
  train_model$ganancia_final / escala
)

test_model$Y_asinh <- asinh(
  test_model$ganancia_final / escala
)


# 3.3 Transformacion logaritmica con signo
# sign(Y) * log(1 + abs(Y)/escala)

train_model$Y_log_signo <-
  sign(train_model$ganancia_final) *
  log1p(abs(train_model$ganancia_final) / escala)

test_model$Y_log_signo <-
  sign(test_model$ganancia_final) *
  log1p(abs(test_model$ganancia_final) / escala)


# Comparar estadisticos de las transformaciones
tabla_transformaciones <- data.frame(
  Variable = c(
    "Ganancia original",
    "Transformacion asinh",
    "Logaritmica con signo"
  ),
  Media = c(
    mean(train_model$Y_original),
    mean(train_model$Y_asinh),
    mean(train_model$Y_log_signo)
  ),
  Mediana = c(
    median(train_model$Y_original),
    median(train_model$Y_asinh),
    median(train_model$Y_log_signo)
  ),
  Desviacion = c(
    sd(train_model$Y_original),
    sd(train_model$Y_asinh),
    sd(train_model$Y_log_signo)
  ),
  Minimo = c(
    min(train_model$Y_original),
    min(train_model$Y_asinh),
    min(train_model$Y_log_signo)
  ),
  Maximo = c(
    max(train_model$Y_original),
    max(train_model$Y_asinh),
    max(train_model$Y_log_signo)
  )
)

print(tabla_transformaciones)

# ------------------------------------------------------------
# 4. GRAFICOS COMPARATIVOS DE TRANSFORMACIONES
# ------------------------------------------------------------

datos_graficos <- data.frame(
  Original = train_model$Y_original,
  Asinh = train_model$Y_asinh,
  Log_signo = train_model$Y_log_signo
)

datos_largos <- datos_graficos %>%
  pivot_longer(
    cols = everything(),
    names_to = "Transformacion",
    values_to = "Valor"
  )

grafico_transformaciones <- ggplot(
  datos_largos,
  aes(x = Valor)
) +
  geom_histogram(
    bins = 50,
    fill = "#AD27F5",
    color = "#ffffff"
  ) +
  facet_wrap(
    ~ Transformacion,
    scales = "free"
  ) +
  labs(
    title = "Comparacion de distribuciones de ganancias",
    x = "Valor de la respuesta",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Mostrar grafico en pantalla
print(grafico_transformaciones)


# ------------------------------------------------------------
# 5. ESTIMAR LOS TRES MODELOS OLS
# ------------------------------------------------------------

# Predictores comunes
formula_predictores <-
  paste(
    c(
      amenazas_bin,
      "c6_3",
      "c7",
      "total_trabajadores",
      "informalidad",
      "c1_b",
      "sexo",
      "region"
    ),
    collapse = " + "
  )


# Modelo 1: respuesta original
formula_original <- as.formula(
  paste("Y_original ~", formula_predictores)
)

modelo_original <- lm(
  formula_original,
  data = train_model
)


# Modelo 2: respuesta asinh
formula_asinh <- as.formula(
  paste("Y_asinh ~", formula_predictores)
)

modelo_asinh <- lm(
  formula_asinh,
  data = train_model
)


# Modelo 3: respuesta logaritmica con signo
formula_log_signo <- as.formula(
  paste("Y_log_signo ~", formula_predictores)
)

modelo_log_signo <- lm(
  formula_log_signo,
  data = train_model
)


# Resumen de los modelos
# MODELO ORIGINAL
print(summary(modelo_original))

# MODELO ASINH
print(summary(modelo_asinh))

# MODELO LOGARITMICO CON SIGNO
print(summary(modelo_log_signo))


# ------------------------------------------------------------
# 6. COMPARACION DEL AJUSTE DE LOS MODELOS
# ------------------------------------------------------------

comparacion_ajuste <- data.frame(
  Modelo = c(
    "Original",
    "Asinh",
    "Logaritmica con signo"
  ),
  R2 = c(
    summary(modelo_original)$r.squared,
    summary(modelo_asinh)$r.squared,
    summary(modelo_log_signo)$r.squared
  ),
  R2_ajustado = c(
    summary(modelo_original)$adj.r.squared,
    summary(modelo_asinh)$adj.r.squared,
    summary(modelo_log_signo)$adj.r.squared
  ),
  RMSE_entrenamiento = c(
    sqrt(mean(residuals(modelo_original)^2)),
    sqrt(mean(residuals(modelo_asinh)^2)),
    sqrt(mean(residuals(modelo_log_signo)^2))
  )
)

print(comparacion_ajuste)

# ------------------------------------------------------------
# 7. DIAGNOSTICO GRAFICO DE LOS MODELOS
# ------------------------------------------------------------

par(mfrow = c(3, 2))

# Residuos: modelo original
plot(
  fitted(modelo_original),
  residuals(modelo_original),
  main = "Residuos: original",
  xlab = "Valores ajustados",
  ylab = "Residuos"
)
abline(h = 0, lty = 2)

# Residuos: modelo asinh
plot(
  fitted(modelo_asinh),
  residuals(modelo_asinh),
  main = "Residuos: asinh",
  xlab = "Valores ajustados",
  ylab = "Residuos"
)
abline(h = 0, lty = 2)

# Residuos: modelo log con signo
plot(
  fitted(modelo_log_signo),
  residuals(modelo_log_signo),
  main = "Residuos: log con signo",
  xlab = "Valores ajustados",
  ylab = "Residuos"
)
abline(h = 0, lty = 2)

# Q-Q: modelo original
qqnorm(
  residuals(modelo_original),
  main = "Q-Q original"
)
qqline(residuals(modelo_original))

# Q-Q: modelo asinh
qqnorm(
  residuals(modelo_asinh),
  main = "Q-Q asinh"
)
qqline(residuals(modelo_asinh))

# Q-Q: modelo log con signo
qqnorm(
  residuals(modelo_log_signo),
  main = "Q-Q log con signo"
)
qqline(residuals(modelo_log_signo))

# Restaurar configuracion grafica
par(mfrow = c(1, 1))

# ------------------------------------------------------------
# 8. INCISO C: RETRANSFORMACION
# ------------------------------------------------------------

# Predicciones en escala original
pred_original <- predict(
  modelo_original,
  newdata = test_model
)


# Predicciones transformadas asinh
pred_asinh_transformada <- predict(
  modelo_asinh,
  newdata = test_model
)

# Funcion inversa asinh
pred_asinh <- escala * sinh(
  pred_asinh_transformada
)


# Predicciones transformadas logaritmica con signo
pred_log_transformada <- predict(
  modelo_log_signo,
  newdata = test_model
)

# Funcion inversa logaritmica con signo
pred_log_signo <-
  sign(pred_log_transformada) *
  escala *
  expm1(abs(pred_log_transformada))


# ------------------------------------------------------------
# 9. CALCULAR MAE Y RMSE EN PESOS CHILENOS
# ------------------------------------------------------------

# Funcion de metricas
calcular_metricas <- function(real, predicho) {
  
  valido <- is.finite(real) & is.finite(predicho)
  
  real <- real[valido]
  predicho <- predicho[valido]
  
  MAE <- mean(abs(real - predicho))
  
  RMSE <- sqrt(
    mean((real - predicho)^2)
  )
  
  return(
    data.frame(
      MAE = MAE,
      RMSE = RMSE,
      Observaciones = length(real)
    )
  )
}


# Valores reales en CLP
ganancia_real <- test_model$ganancia_final


# Calcular metricas
metricas_original <- calcular_metricas(
  ganancia_real,
  pred_original
)

metricas_asinh <- calcular_metricas(
  ganancia_real,
  pred_asinh
)

metricas_log_signo <- calcular_metricas(
  ganancia_real,
  pred_log_signo
)


# Tabla final comparativa
metricas_finales <- data.frame(
  Modelo = c(
    "Respuesta original",
    "Asinh(Y/100000)",
    "Logaritmica con signo"
  ),
  MAE = c(
    metricas_original$MAE,
    metricas_asinh$MAE,
    metricas_log_signo$MAE
  ),
  RMSE = c(
    metricas_original$RMSE,
    metricas_asinh$RMSE,
    metricas_log_signo$RMSE
  ),
  Observaciones = c(
    metricas_original$Observaciones,
    metricas_asinh$Observaciones,
    metricas_log_signo$Observaciones
  )
)

# COMPARACION PREDICTIVA EN CLP
print(metricas_finales)


# ------------------------------------------------------------
# 10. GRAFICO COMPARATIVO DE MAE Y RMSE
# ------------------------------------------------------------

metricas_largas <- metricas_finales %>%
  pivot_longer(
    cols = c(MAE, RMSE),
    names_to = "Metrica",
    values_to = "Error"
  )

grafico_metricas <- ggplot(
  metricas_largas,
  aes(
    x = Modelo,
    y = Error,
    fill = Modelo
  )
) +
  geom_col() +
  facet_wrap(
    ~ Metrica,
    scales = "free_y"
  ) +
  labs(
    title = "Comparacion de errores predictivos",
    x = "Modelo",
    y = "Error (CLP)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    ),
    legend.position = "none"
  )

# Mostrar grafico en pantalla
print(grafico_metricas)


# ------------------------------------------------------------
# 11. IDENTIFICAR EL MEJOR MODELO PREDICTIVO
# ------------------------------------------------------------

mejor_MAE <- metricas_finales$Modelo[
  which.min(metricas_finales$MAE)
]

mejor_RMSE <- metricas_finales$Modelo[
  which.min(metricas_finales$RMSE)
]

# CONCLUSION PREDICTIVA
# Mejor modelo segun MAE
mejor_MAE
# Mejor modelo según RMSE
mejor_RMSE

# ============================================================
# PREGUNTA 5: FORMAS FUNCIONALES E INTERACCIONES
# ============================================================

library(sandwich)

# RECONSTRUIR VARIABLES AMBIENTALES
# Recodificar las cuatro amenazas:
# 1 = Sí
# 2 = No
# Otros valores = NA

variables_amenazas <- c("m2_1", "m2_2", "m2_3", "m2_4")

muestra <- muestra %>%
  mutate(
    across(
      all_of(variables_amenazas),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      .names = "{.col}_bin"
    )
  ) %>%
  mutate(
    alguna_amenaza = case_when(
      if_any(
        ends_with("_bin"),
        ~ .x == 1
      ) ~ 1,
      
      if_all(
        ends_with("_bin"),
        ~ .x == 0
      ) ~ 0,
      
      TRUE ~ NA_real_
    )
  )

# Verificar que se haya creado correctamente
print(table(muestra$alguna_amenaza, useNA = "ifany"))

# 1. Preparar muestra de entrenamiento
train_q5 <- muestra %>%
  filter(particion == "entrenamiento") %>%
  mutate(
    c6_3 = as.numeric(c6_3),
    c7 = as.numeric(c7),
    total_trabajadores = as.numeric(total_trabajadores),
    
    # Variable de amenaza: 0 = ninguna, 1 = alguna
    alguna_amenaza = as.numeric(alguna_amenaza),
    
    # Variables categóricas
    informalidad = factor(informalidad),
    c1_b = factor(c1_b),
    sexo = factor(sexo),
    region = factor(region)
  )

# Revisar codificación y valores
print(table(train_q5$alguna_amenaza, useNA = "ifany"))
print(table(train_q5$informalidad, useNA = "ifany"))

summary(train_q5$c6_3)
summary(train_q5$total_trabajadores)
summary(train_q5$c7)

# Verificar valores negativos antes de aplicar log1p()
# valores negativos en c6_3
sum(train_q5$c6_3 < 0, na.rm = TRUE)

# Valores negativos en total_trabajadores
sum(train_q5$total_trabajadores < 0, na.rm = TRUE)

# ----------------------------------------
# a) Comparación de formas funcionales
# ---------------------------------------
# Crear transformaciones logarítmicas
train_q5 <- train_q5 %>%
  mutate(
    log_c6_3 = log1p(c6_3),
    log_total_trabajadores = log1p(total_trabajadores)
  )

# Variables necesarias para ambos modelos
vars_a <- c(
  "ganancia_final",
  "c6_3",
  "m2_1_bin",
  "m2_2_bin",
  "m2_3_bin",
  "m2_4_bin",
  "total_trabajadores",
  "log_c6_3",
  "log_total_trabajadores",
  "c7",
  "alguna_amenaza",
  "informalidad",
  "c1_b",
  "sexo",
  "region"
)

datos_a <- train_q5 %>%
  select(all_of(vars_a)) %>%
  filter(if_all(everything(), ~ !is.na(.)))

# Observaciones utilizadas en inciso a)
nrow(datos_a)

# Modelo lineal y logaritmico
modelo_a_lineal <- lm(
  ganancia_final ~
    m2_1_bin + m2_2_bin + m2_3_bin + m2_4_bin +
    c6_3 + total_trabajadores + c7 +
    informalidad + c1_b + sexo + region,
  data = datos_a
)

modelo_a_log <- lm(
  ganancia_final ~
    m2_1_bin + m2_2_bin + m2_3_bin + m2_4_bin +
    log_c6_3 + log_total_trabajadores + c7 +
    informalidad + c1_b + sexo + region,
  data = datos_a
)

coeftest(
  modelo_a_lineal,
  vcov. = vcovHC(modelo_a_lineal, type = "HC3")
)

coeftest(
  modelo_a_log,
  vcov. = vcovHC(modelo_a_log, type = "HC3")
)

# Comparación de ajuste
comparacion_a <- data.frame(
  Modelo = c("Variables originales", "Variables logarítmicas"),
  R2 = c(summary(modelo_a_lineal)$r.squared,
         summary(modelo_a_log)$r.squared),
  R2_ajustado = c(summary(modelo_a_lineal)$adj.r.squared,
                  summary(modelo_a_log)$adj.r.squared),
  AIC = c(AIC(modelo_a_lineal), AIC(modelo_a_log)),
  BIC = c(BIC(modelo_a_lineal), BIC(modelo_a_log)),
  Observaciones = c(nobs(modelo_a_lineal),
                    nobs(modelo_a_log))
)

print(comparacion_a)

#-------------------------------
# b) Témico cuadráticp para c7
#-----------------------------------
# Crear variable centrada y término cuadrático
datos_b <- datos_a %>%
  mutate(
    c7_c = c7 - mean(c7, na.rm = TRUE),
    c7_c2 = c7_c^2
  )

# Modelo lineal
modelo_b_lineal <- lm(
  ganancia_final ~
    alguna_amenaza +
    c6_3 +
    total_trabajadores +
    c7_c +
    informalidad +
    c1_b +
    sexo +
    region,
  data = datos_b
)

# Modelo cuadrático: mantiene c7_c y agrega c7_c2
modelo_b_cuadratico <- lm(
  ganancia_final ~
    alguna_amenaza +
    c6_3 +
    total_trabajadores +
    c7_c +
    c7_c2 +
    informalidad +
    c1_b +
    sexo +
    region,
  data = datos_b
)

# Coeficientes con errores robustos HC3
# Modelo cuadrático
print(coeftest(
  modelo_b_cuadratico,
  vcov. = vcovHC(modelo_b_cuadratico, type = "HC3")
))

# Prueba robusta de significancia del término cuadrático
prueba_cuadratica <- linearHypothesis(
  modelo_b_cuadratico,
  "c7_c2 = 0",
  vcov. = vcovHC(modelo_b_cuadratico, type = "HC3"),
  test = "F"
)

# Prueba del temino cuadratico
print(prueba_cuadratica)

# Comparación de ajuste
comparacion_b <- data.frame(
  Modelo = c("Lineal", "Cuadrático"),
  R2 = c(summary(modelo_b_lineal)$r.squared,
         summary(modelo_b_cuadratico)$r.squared),
  R2_ajustado = c(summary(modelo_b_lineal)$adj.r.squared,
                  summary(modelo_b_cuadratico)$adj.r.squared),
  AIC = c(AIC(modelo_b_lineal), AIC(modelo_b_cuadratico)),
  BIC = c(BIC(modelo_b_lineal), BIC(modelo_b_cuadratico)),
  Observaciones = c(nobs(modelo_b_lineal),
                    nobs(modelo_b_cuadratico))
)

print(comparacion_b)

#--------------------------------------------------------
# c) Interacción entre amenazasas naturales e informalidad
#-------------------------------------------------------
# Utilizar las mismas observaciones para ambos modelos
datos_c <- datos_a

# Modelo sin interacción
modelo_c_sin_interaccion <- lm(
  ganancia_final ~
    alguna_amenaza +
    c6_3 +
    total_trabajadores +
    c7 +
    informalidad +
    c1_b +
    sexo +
    region,
  data = datos_c
)

# Modelo con interacción
# El operador * incluye los efectos principales y la interacción
modelo_c_interaccion <- lm(
  ganancia_final ~
    alguna_amenaza * informalidad +
    c6_3 +
    total_trabajadores +
    c7 +
    c1_b +
    sexo +
    region,
  data = datos_c
)

# Resultados robustos HC3
#Modelo con interacción
resultados_c <- coeftest(
  modelo_c_interaccion,
  vcov. = vcovHC(modelo_c_interaccion, type = "HC3")
)

print(resultados_c)

# Identificar los coeficientes de interacción
terminos_interaccion <- grep(
  ":",
  names(coef(modelo_c_interaccion)),
  value = TRUE
)

# Términos de interacción identificados:
print(terminos_interaccion)

# Prueba conjunta robusta de los términos de interacción
if (length(terminos_interaccion) > 0) {
  
  prueba_interaccion <- linearHypothesis(
    modelo_c_interaccion,
    terminos_interaccion,
    vcov. = vcovHC(modelo_c_interaccion, type = "HC3"),
    test = "F"
  )
  
  # Prueba conjunta de interacción
  print(prueba_interaccion)
  
} else {
  print("No se identificaron términos de interacción.")
}

# Comparación de ajuste
comparacion_c <- data.frame(
  Modelo = c("Sin interacción", "Con interacción"),
  R2 = c(summary(modelo_c_sin_interaccion)$r.squared,
         summary(modelo_c_interaccion)$r.squared),
  R2_ajustado = c(summary(modelo_c_sin_interaccion)$adj.r.squared,
                  summary(modelo_c_interaccion)$adj.r.squared),
  AIC = c(AIC(modelo_c_sin_interaccion),
          AIC(modelo_c_interaccion)),
  BIC = c(BIC(modelo_c_sin_interaccion),
          BIC(modelo_c_interaccion)),
  Observaciones = c(nobs(modelo_c_sin_interaccion),
                    nobs(modelo_c_interaccion))
)

print(comparacion_c)

# ----------------------------------------
# PREGUNTA 6: INFLUENCIA Y ESTABILIDAD
# ----------------------------------------

# Base de entrenamiento con las variables del modelo original
datos_q6 <- muestra %>%
  filter(particion == "entrenamiento") %>%
  mutate(
    c6_3 = as.numeric(c6_3),
    c7 = as.numeric(c7),
    total_trabajadores = as.numeric(total_trabajadores),
    informalidad = factor(informalidad),
    c1_b = factor(c1_b),
    sexo = factor(sexo),
    region = factor(region)
  ) %>%
  select(
    id, ganancia_final,
    m2_1_bin, m2_2_bin, m2_3_bin, m2_4_bin,
    c6_3, c7, total_trabajadores,
    informalidad, c1_b, sexo, region
  ) %>%
  filter(if_all(-id, ~ !is.na(.)))

# Modelo original
modelo_q6 <- lm(
  ganancia_final ~
    m2_1_bin + m2_2_bin + m2_3_bin + m2_4_bin +
    c6_3 + c7 + total_trabajadores +
    informalidad + c1_b + sexo + region,
  data = datos_q6
)

# Tamaño muestral y número de parámetros
n_q6 <- nobs(modelo_q6)
p_q6 <- length(coef(modelo_q6))

#Obsevaciuones
n_q6
# Parámetros estimados (incluido intercepto)
p_q6

# ----------------------------------------
# a) Medidas de influencia
# ----------------------------------------

diagnosticos_q6 <- datos_q6 %>%
  mutate(
    residuo_estudentizado = rstudent(modelo_q6),
    leverage = hatvalues(modelo_q6),
    cook = cooks.distance(modelo_q6)
  ) %>%
  mutate(
    alto_residuo = abs(residuo_estudentizado) > 3,
    alto_leverage = leverage > 2 * p_q6 / n_q6,
    alta_cook = cook > 4 / n_q6
  )

# Umbrales utilizados
umbral_leverage <- 2 * p_q6 / n_q6
umbral_cook <- 4 / n_q6

# Umbral leverage:
umbral_leverage
# Umbral Cook:
umbral_cook

# Cantidad de observaciones que supera cada criterio
# Residuos estudentizados |r| > 3:
sum(diagnosticos_q6$alto_residuo)

# Leverage alto:
sum(diagnosticos_q6$alto_leverage)

# Distancia de Cook alta:
sum(diagnosticos_q6$alta_cook)

# Resumen de los diagnósticos
summary(diagnosticos_q6$residuo_estudentizado)
summary(diagnosticos_q6$leverage)
summary(diagnosticos_q6$cook)

# ----------------------------------------
# b) Cinco observaciones más influyentes
# ----------------------------------------

cinco_influyentes <- diagnosticos_q6 %>%
  arrange(desc(cook)) %>%
  slice_head(n = 5) %>%
  select(
    id,
    residuo_estudentizado,
    leverage,
    cook,
    alto_residuo,
    alto_leverage,
    alta_cook
  )

print(cinco_influyentes)

# ----------------------------------------
# c) Análisis de sensibilidad
# ----------------------------------------

# Identificadores de las cinco observaciones más influyentes
ids_excluir <- cinco_influyentes$id

# Base temporal sin esas cinco observaciones
datos_q6_sin_influyentes <- datos_q6 %>%
  filter(!id %in% ids_excluir)

# Reestimar el mismo modelo
modelo_q6_sin_influyentes <- lm(
  ganancia_final ~
    m2_1_bin + m2_2_bin + m2_3_bin + m2_4_bin +
    c6_3 + c7 + total_trabajadores +
    informalidad + c1_b + sexo + region,
  data = datos_q6_sin_influyentes
)

# Coeficientes originales y con exclusión temporal
resultados_originales <- coeftest(
  modelo_q6,
  vcov. = vcovHC(modelo_q6, type = "HC3")
)

resultados_sin_influyentes <- coeftest(
  modelo_q6_sin_influyentes,
  vcov. = vcovHC(modelo_q6_sin_influyentes, type = "HC3")
)

# Variables ambientales
variables_ambientales <- c(
  "m2_1_bin", "m2_2_bin", "m2_3_bin", "m2_4_bin"
)

# Tabla comparativa
comparacion_ambiental <- data.frame(
  Variable = variables_ambientales,
  Coef_original = resultados_originales[
    variables_ambientales, "Estimate"
  ],
  p_original = resultados_originales[
    variables_ambientales, "Pr(>|t|)"
  ],
  Coef_sin_influyentes = resultados_sin_influyentes[
    variables_ambientales, "Estimate"
  ],
  p_sin_influyentes = resultados_sin_influyentes[
    variables_ambientales, "Pr(>|t|)"
  ]
) %>%
  mutate(
    Diferencia = Coef_sin_influyentes - Coef_original,
    Cambio_absoluto = abs(Diferencia),
    Cambio_signo = sign(Coef_original) != sign(Coef_sin_influyentes)
  )

print(comparacion_ambiental)

# Tamaños muestrales
# Observaciones modelo original:
nobs(modelo_q6)
#Observaciones tras exclusión temporal:
nobs(modelo_q6_sin_influyentes)

# Identificadores excluidos temporalmente
# IDs excluidos temporalmente:
print(ids_excluir)

# ----------------------------------------
# PREGUNTA 7: SELECCIÓN CLÁSICA
# ----------------------------------------

# Base de entrenamiento con las variables candidatas
datos_q7 <- muestra %>%
  filter(particion == "entrenamiento") %>%
  mutate(
    c6_3 = as.numeric(c6_3),
    c7 = as.numeric(c7),
    total_trabajadores = as.numeric(total_trabajadores),
    informalidad = factor(informalidad),
    c1_b = factor(c1_b),
    sexo = factor(sexo),
    region = factor(region)
  ) %>%
  select(
    ganancia_final,
    m2_1_bin, m2_2_bin, m2_3_bin, m2_4_bin,
    c6_3, c7, total_trabajadores,
    informalidad, c1_b, sexo, region
  ) %>%
  filter(if_all(everything(), ~ !is.na(.)))

# Verificar tamaño de muestra
nrow(datos_q7)

# Modelo nulo: solo intercepto
modelo_nulo_q7 <- lm(
  ganancia_final ~ 1,
  data = datos_q7
)

# Modelo completo: cuatro amenazas y todas las variables de control
modelo_completo_q7 <- lm(
  ganancia_final ~
    m2_1_bin + m2_2_bin + m2_3_bin + m2_4_bin +
    c6_3 + c7 + total_trabajadores +
    informalidad + c1_b + sexo + region,
  data = datos_q7
)

# Alcance de variables para los procedimientos de selección
alcance_q7 <- list(
  lower = formula(modelo_nulo_q7),
  upper = formula(modelo_completo_q7)
)

# Selección Forward
modelo_forward <- MASS::stepAIC(
  modelo_nulo_q7,
  scope = alcance_q7,
  direction = "forward",
  trace = 0
)

# Selección Backward
modelo_backward <- MASS::stepAIC(
  modelo_completo_q7,
  direction = "backward",
  trace = 0
)

# Selección Stepwise bidireccional
modelo_stepwise <- MASS::stepAIC(
  modelo_completo_q7,
  scope = alcance_q7,
  direction = "both",
  trace = 0
)

# Mostrar las fórmulas seleccionadas
# MODELO FORWARD
print(formula(modelo_forward))

# MODELO BACKWARD
print(formula(modelo_backward))

# MODELO STEPWISE BIDIRECCIONAL
print(formula(modelo_stepwise))

# MODELO COMPLETO
print(formula(modelo_completo_q7))

# ----------------------------------------
# b) Comparación AIC, BIC y Cp
# ----------------------------------------

# Lista de modelos
modelos_q7 <- list(
  "Forward" = modelo_forward,
  "Backward" = modelo_backward,
  "Stepwise" = modelo_stepwise,
  "Modelo completo" = modelo_completo_q7
)

# Varianza residual del modelo completo
sigma2_completo <- sigma(modelo_completo_q7)^2

# Función para calcular Cp de Mallows
calcular_cp <- function(modelo, sigma2_referencia, n) {
  
  rss <- sum(residuals(modelo)^2)
  p <- length(coef(modelo))
  
  cp <- rss / sigma2_referencia - n + 2 * p
  
  return(cp)
}

# Tabla comparativa
comparacion_q7 <- data.frame(
  Modelo = names(modelos_q7),
  AIC = sapply(modelos_q7, AIC),
  BIC = sapply(modelos_q7, BIC),
  Cp = sapply(
    modelos_q7,
    calcular_cp,
    sigma2_referencia = sigma2_completo,
    n = nrow(datos_q7)
  ),
  R2 = sapply(
    modelos_q7,
    function(modelo) summary(modelo)$r.squared
  ),
  R2_ajustado = sapply(
    modelos_q7,
    function(modelo) summary(modelo)$adj.r.squared
  ),
  Parametros = sapply(
    modelos_q7,
    function(modelo) length(coef(modelo))
  ),
  Observaciones = sapply(
    modelos_q7,
    nobs
  )
)

print(comparacion_q7)

# ----------------------------------------
# 7.c) Amenazas ambientales seleccionadas
# ----------------------------------------

amenazas_q7 <- c(
  "m2_1_bin", "m2_2_bin",
  "m2_3_bin", "m2_4_bin"
)

# Identificar amenazas presentes en cada modelo
amenazas_retenidas <- lapply(
  modelos_q7,
  function(modelo) {
    intersect(
      amenazas_q7,
      attr(terms(modelo), "term.labels")
    )
  }
)

print(amenazas_retenidas)

# Coeficientes ambientales de cada modelo
for (nombre in names(modelos_q7)) {
  
  print(paste("MODELO:", nombre))
  
  modelo <- modelos_q7[[nombre]]
  
  coeficientes <- coeftest(
    modelo,
    vcov. = vcovHC(modelo, type = "HC3")
  )
  
  filas_ambientales <- intersect(
    amenazas_q7,
    rownames(coeficientes)
  )
  
  if (length(filas_ambientales) > 0) {
    print(coeficientes[filas_ambientales, , drop = FALSE])
  } else {
    print("El procedimiento no retuvo amenazas ambientales.")
  }
}

# ==========================================
# PREGUNTA 8: VALIDACION CRUZADA
# ==========================================

# b) Compara MAE y RMSE
# Revisar distribución de los pliegues
print(table(
  muestra$particion,
  muestra$fold,
  useNA = "ifany"
))

# Base exclusiva de entrenamiento
datos_cv <- muestra %>%
  filter(particion == "entrenamiento")

# Verificar que existan cinco pliegues
folds_cv <- sort(unique(as.character(datos_cv$fold)))

print(folds_cv)
print(table(datos_cv$fold))

stopifnot(length(folds_cv) == 5)

# Variables explicativas
amenazas_cv <- c(
  "m2_1_bin",
  "m2_2_bin",
  "m2_3_bin",
  "m2_4_bin"
)

controles_cv <- c(
  "c6_3",
  "c7",
  "total_trabajadores",
  "informalidad",
  "c1_b",
  "sexo",
  "region"
)

variables_cv <- c(
  "ganancia_final",
  amenazas_cv,
  controles_cv
)

# Preparar tipos de variables
datos_cv <- datos_cv %>%
  mutate(
    c6_3 = as.numeric(c6_3),
    c7 = as.numeric(c7),
    total_trabajadores = as.numeric(total_trabajadores),
    informalidad = factor(informalidad),
    c1_b = factor(c1_b),
    sexo = factor(sexo),
    region = factor(region)
  )

# Mantener la misma muestra para ambos modelos
datos_cv <- datos_cv %>%
  filter(if_all(all_of(variables_cv), ~ !is.na(.)))

# Verificar observaciones disponibles
print(nrow(datos_cv))
print(table(datos_cv$fold))

# Fórmula sin amenazas ambientales
formula_sin_amenazas <- as.formula(
  paste(
    "ganancia_final ~",
    paste(controles_cv, collapse = " + ")
  )
)

# Fórmula con amenazas ambientales
formula_con_amenazas <- as.formula(
  paste(
    "ganancia_final ~",
    paste(c(amenazas_cv, controles_cv), collapse = " + ")
  )
)

# Guardar resultados
resultados_folds <- list()
predicciones_cv <- list()

# Ejecutar exclusivamente los cinco pliegues predeterminados
for (f in folds_cv) {
  
  # Separar entrenamiento y validación
  entrenamiento_fold <- datos_cv %>%
    filter(as.character(fold) != f)
  
  validacion_fold <- datos_cv %>%
    filter(as.character(fold) == f)
  
  # Definir niveles factoriales usando solo el entrenamiento
  variables_factor <- c(
    "informalidad", "c1_b", "sexo", "region"
  )
  
  for (v in variables_factor) {
    
    entrenamiento_fold[[v]] <- droplevels(
      factor(entrenamiento_fold[[v]])
    )
    
    validacion_fold[[v]] <- factor(
      as.character(validacion_fold[[v]]),
      levels = levels(entrenamiento_fold[[v]])
    )
  }
  
  # Excluir categorías no observadas en el entrenamiento,
  # manteniendo las mismas observaciones para ambos modelos
  entrenamiento_fold <- entrenamiento_fold %>%
    filter(if_all(all_of(variables_cv), ~ !is.na(.)))
  
  validacion_fold <- validacion_fold %>%
    filter(if_all(all_of(variables_cv), ~ !is.na(.)))
  
  # Modelo sin indicadores ambientales
  modelo_sin <- lm(
    formula_sin_amenazas,
    data = entrenamiento_fold
  )
  
  # Modelo con los cuatro indicadores ambientales
  modelo_con <- lm(
    formula_con_amenazas,
    data = entrenamiento_fold
  )
  
  # Predicciones sobre el pliegue reservado
  pred_sin <- predict(
    modelo_sin,
    newdata = validacion_fold
  )
  
  pred_con <- predict(
    modelo_con,
    newdata = validacion_fold
  )
  
  # Guardar errores de predicción
  predicciones_cv[[f]] <- data.frame(
    fold = f,
    observacion = validacion_fold$ganancia_final,
    pred_sin_amenazas = pred_sin,
    pred_con_amenazas = pred_con
  )
  
  # Métricas del pliegue
  resultados_folds[[f]] <- data.frame(
    Fold = f,
    Observaciones = nrow(validacion_fold),
    
    MAE_sin = mean(
      abs(validacion_fold$ganancia_final - pred_sin)
    ),
    
    RMSE_sin = sqrt(mean(
      (validacion_fold$ganancia_final - pred_sin)^2
    )),
    
    MAE_con = mean(
      abs(validacion_fold$ganancia_final - pred_con)
    ),
    
    RMSE_con = sqrt(mean(
      (validacion_fold$ganancia_final - pred_con)^2
    ))
  )
}

# Unir resultados
tabla_folds <- bind_rows(resultados_folds)

predicciones_cv_final <- bind_rows(predicciones_cv)

# Mostrar resultados por pliegue
print(tabla_folds)

# Métricas globales
comparacion_cv <- data.frame(
  Modelo = c(
    "Sin indicadores ambientales",
    "Con indicadores ambientales"
  ),
  
  MAE = c(
    mean(abs(
      predicciones_cv_final$observacion -
        predicciones_cv_final$pred_sin_amenazas
    )),
    mean(abs(
      predicciones_cv_final$observacion -
        predicciones_cv_final$pred_con_amenazas
    ))
  ),
  
  RMSE = c(
    sqrt(mean(
      (predicciones_cv_final$observacion -
         predicciones_cv_final$pred_sin_amenazas)^2
    )),
    sqrt(mean(
      (predicciones_cv_final$observacion -
         predicciones_cv_final$pred_con_amenazas)^2
    ))
  )
)

print(comparacion_cv)

# ----------------------------
# Pregunta 9 - Regularización
# ----------------------------
library(glmnet)

# a) Lasso, Ridge y Elastic Net
# ------------------------------------------------------------
# 1. Preparar datos de entrenamiento
# ------------------------------------------------------------

variables_modelo <- c(
  "ganancia_final",
  "m2_1_bin", "m2_2_bin", "m2_3_bin", "m2_4_bin",
  "c6_3", "c7", "total_trabajadores",
  "informalidad", "c1_b", "sexo", "region",
  "fold"
)

datos_reg <- datos_cv %>%
  select(all_of(variables_modelo)) %>%
  mutate(
    across(c(informalidad, c1_b, sexo, region), as.factor)
  ) %>%
  filter(complete.cases(.))

# Verificar que se mantienen los cinco folds
print(table(datos_reg$fold))

# Cada fold debe contener 1.072 observaciones
stopifnot(all(table(datos_reg$fold) == 1072))

# ------------------------------------------------------------
# 2. Crear matriz de diseño
# ------------------------------------------------------------

formula_reg <- ganancia_final ~
  m2_1_bin + m2_2_bin + m2_3_bin + m2_4_bin +
  c6_3 + c7 + total_trabajadores +
  informalidad + c1_b + sexo + region

# Codificación de variables categóricas mediante dummies
X <- model.matrix(formula_reg, data = datos_reg)[, -1]

# Respuesta en escala original: pesos chilenos
Y <- datos_reg$ganancia_final

# Identificador de los cinco folds predefinidos
foldid <- as.integer(as.character(datos_reg$fold))

# Comprobar dimensiones
print(dim(X))
print(length(Y))
print(table(foldid))

# ------------------------------------------------------------
# 3. Estimar Ridge y LASSO
# ------------------------------------------------------------

set.seed(123)

modelo_ridge <- cv.glmnet(
  x = X,
  y = Y,
  alpha = 0,
  family = "gaussian",
  foldid = foldid,
  type.measure = "mse",
  standardize = TRUE
)

set.seed(123)

modelo_lasso <- cv.glmnet(
  x = X,
  y = Y,
  alpha = 1,
  family = "gaussian",
  foldid = foldid,
  type.measure = "mse",
  standardize = TRUE
)

# ------------------------------------------------------------
# 4. Ajustar Elastic Net y seleccionar alpha
# ------------------------------------------------------------

valores_alpha <- seq(0.1, 0.9, by = 0.1)

modelos_en <- vector("list", length(valores_alpha))
resultados_en <- data.frame(
  alpha = valores_alpha,
  lambda_min = NA_real_,
  mse_cv = NA_real_
)

for (i in seq_along(valores_alpha)) {
  
  set.seed(123)
  
  modelos_en[[i]] <- cv.glmnet(
    x = X,
    y = Y,
    alpha = valores_alpha[i],
    family = "gaussian",
    foldid = foldid,
    type.measure = "mse",
    standardize = TRUE
  )
  
  resultados_en$lambda_min[i] <- modelos_en[[i]]$lambda.min
  
  resultados_en$mse_cv[i] <- min(modelos_en[[i]]$cvm)
}

# Elegir alpha con menor error cuadrático medio de CV
mejor_en <- which.min(resultados_en$mse_cv)

alpha_optimo <- valores_alpha[mejor_en]
modelo_elastic <- modelos_en[[mejor_en]]

print(resultados_en)
print(alpha_optimo)

# ------------------------------------------------------------
# 5. Comparación de los modelos
# ------------------------------------------------------------

resultados_regularizacion <- data.frame(
  Modelo = c("Ridge", "LASSO", "Elastic Net"),
  Alpha = c(
    0,
    1,
    alpha_optimo
  ),
  Lambda_min = c(
    modelo_ridge$lambda.min,
    modelo_lasso$lambda.min,
    modelo_elastic$lambda.min
  ),
  MSE_CV = c(
    min(modelo_ridge$cvm),
    min(modelo_lasso$cvm),
    min(modelo_elastic$cvm)
  )
) %>%
  mutate(
    RMSE_CV = sqrt(MSE_CV)
  )

print(resultados_regularizacion)

par(mfrow = c(1, 3))

plot(modelo_ridge, main = "Ridge")
plot(modelo_lasso, main = "LASSO")
plot(modelo_elastic, main = "Elastic Net")

par(mfrow = c(1, 1))

# c) 
# ------------------------------------------------------------
# 6. Extraer coeficientes de cada modelo
# ------------------------------------------------------------

coef_ridge <- as.matrix(
  coef(modelo_ridge, s = "lambda.min")
)

coef_lasso <- as.matrix(
  coef(modelo_lasso, s = "lambda.min")
)

coef_elastic <- as.matrix(
  coef(modelo_elastic, s = "lambda.min")
)

# Indicadores ambientales
amenazas <- c(
  "m2_1_bin",
  "m2_2_bin",
  "m2_3_bin",
  "m2_4_bin"
)

# Tabla comparativa
comparacion_amenazas <- data.frame(
  Amenaza = amenazas,
  Ridge = coef_ridge[amenazas, 1],
  LASSO = coef_lasso[amenazas, 1],
  Elastic_Net = coef_elastic[amenazas, 1]
)

# Identificar si LASSO y Elastic Net conservaron cada indicador
comparacion_amenazas <- comparacion_amenazas %>%
  mutate(
    Seleccion_LASSO = LASSO != 0,
    Seleccion_Elastic_Net = Elastic_Net != 0
  )

print(comparacion_amenazas)



library(lmtest)
library(sandwich)

resultados_interaccion <- lapply(1:5, function(k) {
  
  datos_fold <- subset(train_q5, fold != k)
  
  modelo_fold <- lm(
    ganancia_final ~
      alguna_amenaza * informalidad +
      m2_1_bin + m2_2_bin + m2_3_bin + m2_4_bin +
      c6_3 + c7 + total_trabajadores +
      c1_b + sexo + region,
    data = datos_fold
  )
  
  robustos <- coeftest(
    modelo_fold,
    vcov. = vcovHC(modelo_fold, type = "HC3")
  )
  
  nombre <- "alguna_amenaza:informalidad1"
  
  data.frame(
    Pliegue_excluido = k,
    Coeficiente = robustos[nombre, 1],
    Error_HC3 = robustos[nombre, 2],
    Valor_p = robustos[nombre, 4]
  )
})

tabla_estabilidad <- do.call(rbind, resultados_interaccion)

print(tabla_estabilidad)
