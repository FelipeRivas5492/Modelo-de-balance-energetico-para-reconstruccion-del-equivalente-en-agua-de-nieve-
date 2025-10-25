

require(gridExtra)
require(patchwork)
require(readxl)
require(ggplot2)
require(dplyr)
require(plotly)
require(ggplot2)
require(reshape2)

# FELIPE RIVAS 25/10/2025 13:10 pm
# :D

#### DATOS METEO 1 AÑO HIDROLOGICO #### 


ruta_1h = ".../meteo_1h.csv"

meteo_1h = read.csv(ruta_1h)

head(meteo_1h)


#### PLOT METEO #### 



ruta_1h <- ".../meteo_1h.csv"
meteo_1h <- read.csv(ruta_1h)

# Convertir la columna Fecha a tipo POSIXct
meteo_1h$Fecha <- as.POSIXct(meteo_1h$Fecha, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Crear gráficos individuales

# Velocidad del viento
plot_viento <- ggplot(meteo_1h, aes(x = Fecha, y = viento_ms)) +
  geom_line(color = "black", size = 1) +
  labs(title = "Velocidad del Viento", x = "Fecha", y = "Viento [m/s]") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

# Radiación de onda corta
plot_rad_sw <- ggplot(meteo_1h, aes(x = Fecha, y = SW_W_m2)) +
  geom_line(color = "black", size = 1) +
  labs(title = "Radiación de Onda Corta", x = "Fecha", y = "Radiación [W/m²]") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

# Snow Water Equivalent (SWE)
plot_swe <- ggplot(meteo_1h, aes(x = Fecha, y = SWE_mm)) +
  geom_line(color ="black", size = 1) +
  labs(title = "Snow Water Equivalent (SWE)", x = "Fecha", y = "SWE [mm]") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

# Humedad relativa
plot_humedad <- ggplot(meteo_1h, aes(x = Fecha, y = RH)) +
  geom_line(color = "black", size = 1) +
  labs(title = "Humedad Relativa", x = "Fecha", y = "Humedad [%]") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

# Temperatura
plot_temperatura <- ggplot(meteo_1h, aes(x = Fecha, y = Ta_C)) +
  geom_line(color = "black", size = 1) +
  labs(title = "Temperatura", x = "Fecha", y = "Temperatura [°C]") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

# Crear el panel de gráficos
library(patchwork)

panel <- plot_viento / plot_rad_sw / plot_swe / plot_humedad / plot_temperatura +
  plot_layout(ncol = 1)  # Una columna

# Mostrar el panel
print(panel)














#### FECHA SWEmax y SWE = 0 #### 


SWE <- meteo_1h$SWE_mm
fecha <- as.POSIXct(meteo_1h$Fecha, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

max_SWE <- max(SWE, na.rm = TRUE)
fecha_max_SWE <- fecha[which.max(SWE)]

# Encontrar la primera fecha donde SWE es cero después del valor máximo
SWE_after_max <- SWE[fecha > fecha_max_SWE]
fecha_after_max <- fecha[fecha > fecha_max_SWE]
fecha_cero_SWE <- fecha_after_max[which(SWE_after_max == 0)[1]]

# Resultados de SWE
cat("Máximo SWE:", max_SWE, "\n")
cat("Fecha del máximo SWE:", format(fecha_max_SWE, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Primera fecha con SWE igual a cero después del máximo:", format(fecha_cero_SWE, "%Y-%m-%d %H:%M:%S"), "\n")


#### DECAIMIENTO ALBEDO #### 




calcular_albedo <- function(edad_horas, albedo_inicial = 0.9, C = 0.0473, beta = 0.1) {
  # Convertir la edad de horas a días
  edad_dias <- edad_horas / 24
  
  # Fórmula para el decaimiento del albedo
  albedo <- albedo_inicial - C * (edad_dias^beta)
  
  # El albedo no puede ser menor que 0
  albedo <- pmax(albedo, 0)
  
  return(albedo)
}


total_intervalos <- 1057  # Número total de pasos de tiempo (horas)
intervalo_horas <- 1  # Intervalo horario


albedos <- numeric(total_intervalos)
tiempo <- seq(0, by = intervalo_horas, length.out = total_intervalos)  # Tiempo en horas desde el inicio


for (i in 1:total_intervalos) {
  edad_horas <- (i - 1) * intervalo_horas  # Edad en horas
  albedos[i] <- calcular_albedo(edad_horas)  # Calcular albedo para esta edad
}


resultados_albedo <- data.frame(
  Tiempo = tiempo,  # Tiempo en horas
  Albedo = albedos
)

# Mostrar las primeras filas del dataframe con los resultados
print(head(resultados_albedo))


library(ggplot2)
ggplot(resultados_albedo, aes(x = Tiempo, y = Albedo)) +
  geom_line(color = "black", size = 1) +
  labs(title = "Decaimiento del Albedo",
       x = "Tiempo (horas)", y = "Albedo") +
  theme_minimal()






#### OBTENER LIMITES DE FECHAS PARA FUNCION #### 



fecha_max_SWE 
fecha_cero_SWE 

subset_meteo <- subset(meteo_1h, as.POSIXct(Fecha, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") >= fecha_max_SWE & 
                         as.POSIXct(Fecha, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") <= fecha_cero_SWE)


t_snow <- 0
t_air <- subset_meteo$Ta_C
k_down <- subset_meteo$SW_W_m2

emisividad_atmos <- subset_meteo$emisividad_atmosf

# T_air_kelvin = t_air +273.15 

l_down <- subset_meteo$Rlw

 # l_down<- 0.575* (ea_ma^(1/7)) * sigma * (T_air_kelvin^4)
# 
# l_down_1 <- emisividad_atmos *sigma * (T_air_kelvin^4)
# 
# emisividad_snow = 0.95 
# 
# l_up <- emisividad_snow * sigma * 273.15^4 + (1 - emisividad_snow) * l_down
# 
# l_up_1 <- emisividad_snow * sigma * 273.15^4 + (1 - emisividad_snow) * l_down_1
# 
# 
# Qnl <- l_down - l_up
# 
# 
# Qnl_1 <- l_down - l_up_1




fecha_max_SWE 
fecha_cero_SWE 

subset_meteo <- subset(meteo_1h, as.POSIXct(Fecha, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") >= fecha_max_SWE & 
                         as.POSIXct(Fecha, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") <= fecha_cero_SWE)


t_snow <- 0
t_air <- subset_meteo$Ta_C
k_down <- subset_meteo$SW_W_m2
emisividad_atmos <- subset_meteo$emisividad_atmosf
l_down <- subset_meteo$Rlw
RH = subset_meteo$RH
ea_ma <- subset_meteo$e_hPa
SWE <- subset_meteo$SWE_mm
u_a <- subset_meteo$viento_ms
emisividad <- subset_meteo$emisividad_atmosf
albedo_1 = resultados_albedo$Albedo




#### BALANCE #### 


ruta_1h = ".../meteo_1h.csv"

meteo_1h = read.csv(ruta_1h)

head(meteo_1h)



ruta_sca <- ".../SCA_Fecha_Hora.csv"

sca <- read.csv(ruta_sca)

head(sca)


#### SIN /CON CORRECCION #### 

#The roughness lengths for ice and snow were assumed equal and also constant in time during both melt seasons (z 0w = 10 mm). 
#The roughness lengths of temperature and vapour pressure were assumed to be two orders of magnitude smaller 
# than that of wind speed according to Reference HolmgrenHolmgren (1971) and Reference AmbachAmbach (1986).

#Z_0 SNOW
# Za REF
# Zw = 


# ANDREAS rugosidad snow. No sirvio para la zona de estudio

calcular_zs <- function(u_a, z_a, z_0, tipo = "vapor") {
  # Constantes
  k <- 0.4  # Constante de von Kármán
  nu <- 1.5e-5  # Viscosidad cinemática del aire (m²/s)
  
  # Cálculo de la velocidad de fricción (u_fric)
  u_fric <- (u_a * k) / log(z_a / z_0)
  
  # Cálculo del Reynolds de rugosidad (R_fric)
  R_fric <- (u_fric * z_0) / nu
  
  # Definir los coeficientes según la tabla para temperatura y vapor de agua
  coeficientes <- list(
    temperatura = list(
      rango1 = c(b0 = 1.250, b1 = NA, b2 = NA),
      rango2 = c(b0 = 0.149, b1 = -0.550, b2 = NA),
      rango3 = c(b0 = 0.317, b1 = -0.565, b2 = -0.183)
    ),
    vapor = list(
      rango1 = c(b0 = 1.610, b1 = NA, b2 = NA),
      rango2 = c(b0 = 0.351, b1 = -0.628, b2 = NA),
      rango3 = c(b0 = 0.396, b1 = -0.512, b2 = -0.180)
    )
  )
  
  # Seleccionar el tipo de cálculo
  tipo_coef <- coeficientes[[tipo]]
  
  # Determinar el rango de R_fric
  if (R_fric <= 0.135) {
    b <- tipo_coef$rango1
  } else if (R_fric > 0.135 && R_fric < 2.5) {
    b <- tipo_coef$rango2
  } else if (R_fric >= 2.5 && R_fric <= 1000) {
    b <- tipo_coef$rango3
  } else {
    stop("R_fric fuera de rango (0.135 <= R^* <= 1000)")
  }
  
  # Calcular ln(z_s / z_0)
  ln_zs_z0 <- b["b0"] + (if (!is.na(b["b1"])) b["b1"] * log(R_fric) else 0) +
    (if (!is.na(b["b2"])) b["b2"] * (log(R_fric)^2) else 0)
  
  # Calcular z_s
  z_s <- z_0 * exp(ln_zs_z0)
  
  # Retornar resultados
  return(list(
    u_fric = u_fric,
    R_fric = R_fric,
    z_s = z_s
  ))
}


resultado <- calcular_zs(u_a = 100, z_a = 2, z_0 = 0.001, tipo = "vapor")





calcular_balance_energia_sc <- function(k_down, albedo, t_snow, t_air, ea_ma, emisividad, l_down, u_a, z_a, z_0,  QG = 0, QR = 0, fsca, RH, z_est) {
  
  t_snow <- rep(t_snow, length(t_air))
  emisividad <- rep(emisividad, length(t_air))
  
  # Constantes
  sigma <- 5.67e-8  # Constante de Stefan-Boltzmann (W/m² K⁴) OK
  g <- 9.81         # Aceleración de la gravedad (m/s²) OK 
  rho_a <- 1.225    # Densidad del aire (kg/m³) OK 
  cp <- 1005        # Calor específico del aire (J/kg K) OK 
  k <- 0.4          # Constante de von Kármán (adimensional) OK
  P_0 <- 101.325      # Presión atmosférica estándar (Pa)
  L_v <- 2.5e6      # Calor latente de vaporización (J/kg) OK
  L_f <- 3.34e5     # Calor latente de fusión (J/kg) OK 
  L_s <- 2.834e6    # Calor latente de sublimacion (J/kg) OK 
  rho_w <- 1000     # Densidad del agua (kg/m³) OK 
  
  
  # Inicializar listas para almacenar resultados
  balance_energia_neta_list <- vector("numeric", length(k_down))
  Qnk_list <- vector("numeric", length(k_down))
  Qnl_list <- vector("numeric", length(k_down))
  QH_list <- vector("numeric", length(k_down))
  QE_list <- vector("numeric", length(k_down))
  Ce_list <- vector("numeric", length(k_down))
  Ch_list <- vector("numeric", length(k_down))
  Chn_list <- vector("numeric", length(k_down))
  Ri_list <- vector("numeric", length(k_down)) 
  
  Ts_list <- vector("numeric", length(k_down)) 
  Ta_list <- vector("numeric", length(k_down)) 
  Tm_list <- vector("numeric", length(k_down)) 
  Pa_list <- vector("numeric", length(k_down)) 
  
  # Bucle sobre cada punto en el tiempo (o cada posición)
  for (i in 1:length(k_down)) {
    
    # Conversión de temperaturas a Kelvin
    Ts <- t_snow[i] + 273.15  # Superficie de nieve en Kelvin (K) OK
    Ta <- t_air[i] + 273.15   # Temperatura del aire en Kelvin (K) OK
    
    
    print(paste("Iteración:", i))
    print(paste("z_est:", z_est))
    print(paste("Ta:", Ta))
    print(paste("ea_ma[i]:", ea_ma[i]))
    
    ### CORRECCION PRESION 
    
    Pa <- 101.325 * (1 - ((0.0065 * z_est) / Ta))^5.26
    
    print(paste("Pa:", Pa))
    
    #### RADIACIÓN DE ONDA CORTA ####
    
    k_up <- albedo[i] * k_down[i] # OK 
    
    Qnk <- k_down[i] - k_up # OK
    
    print(paste("k_up:", k_up))
    print(paste("Qnk: ", Qnk ))
    
    
    #### RADIACIÓN DE ONDA LARGA ####
    
    
    l_up <- emisividad[i] * sigma * (Ts^4) + (1 - emisividad[i]) * l_down[i] # OK
    
    
    Qnl <- l_down[i] - l_up # OK
    
    print(paste("l_up:" , l_up))
    
    print(paste(" Qnl: " , Qnl))
    
    
    #### FLUJO DE CALOR SENSIBLE ####
    
    Tm <- (Ta + Ts) / 2 # OK 
    
    u_a_i <- max(u_a[i], 0.1) #OK 
    
    Ri_B <- (g * (Tm^-1) * z_a * (Ta - Ts) )/ u_a_i^2  # Número de Richardson EN KELVIN OK 
    
    
    
    zw = calcular_zs(u_a_i, z_a, z_0,tipo = "vapor")
    zt = calcular_zs(u_a_i, z_a, z_0,tipo = "temperatura")
    
    
    
    Chn <- (k^2) / ((log(z_a / z_0)*log(z_a /0.1* z_0))) # OK
    
    
    # u_fric = (u_a_i*k)/log(z_a/z_0)
    # 
    # R_fric = (u_fric * z_0)/1.5e-5


    
    # 
    #     Ch <- ifelse(Ri_B < 0.01,(Chn * (1 - 16 * Ri_B)^0.75)# INESTABLE  OK
    #                  , (Chn * (1 - 5 * Ri_B)^2)) # ESTABLE OK
    # 
    #     Ch <- ifelse(is.finite(Ch), Ch, Chn)
    
    
    
    QH <- rho_a * cp * Chn * u_a_i * (Ta - Ts) # OK 
    
    print(paste("Ch:", Chn))
    print(paste(" QH: ",  QH ))
    
    
    #### FLUJO DE CALOR LATENTE ####
    
    
    Ps <- 0.6112 * exp((22.46 * t_snow[i]) / (272.62 + t_snow[i])) # EN kPA 
    
    # e_0 <- RH[i] * Ps / 100 # PA 
    
    # # # 
    # Ce <- ifelse(Ri_B < -0.01, Chn * (1 - 10 * Ri_B)^(-1/2), Chn *(1 + 10 * Ri_B))
    
    Ce <- (k^2) / ((log(z_a / z_0) * log(z_a / 0.1*z_0))) 
    
    
    
    QE <- rho_a * L_s * Ce * u_a_i * 0.622 * ((Ps - ea_ma[i])/P_0)   # E_o es la actual de la nieve y se necesita la saturacion 
    
    print(paste("Ce:", Ce))
    print(paste(" QE: ",  QE ))
    # print(paste("e_0:", e_0 ))
    print(paste("Ri_B: ",  Ri_B ))
    print(paste("Ri_B: ",  Ps ))
    
    
    #### BALANCE DE ENERGÍA TOTAL ####
    
    
    balance_energia <- Qnk + Qnl + QH + QE + QG + QR
    balance_energia_neta <- balance_energia * fsca[i]
    
    
    print(paste("balance_energia_neta:", balance_energia_neta ))
    
    #### Almacenar resultados ####
    
    
    balance_energia_neta_list[i] <- balance_energia
    Qnk_list[i] <- Qnk
    Qnl_list[i] <- Qnl
    QH_list[i] <- QH
    QE_list[i] <- QE
    Ce_list[i] <- Ce
    # Ch_list[i] <- Ch
    Pa_list[i] <- Pa
    
    
    Chn_list[i] <- Chn
    Ri_list[i] <- Ri_B
    Ts_list[i] <- Ts
    Ta_list[i] <- Ta
    Tm_list[i] <- Tm
  }
  
  # Crear un data frame con los resultados
  df_resultados <- data.frame(
    Tiempo = 1:length(k_down),
    Balance_Energia_Neta = balance_energia_neta_list,
    Radiacion_Onda_Corta_Neta = Qnk_list,
    Radiacion_Onda_Larga_Neta = Qnl_list,
    Calor_Sensible = QH_list,
    Calor_Latente = QE_list,
    Pa = Pa_list,
    # Ch = Ch_list,
    # Ce = Ce_list,  # Coeficiente de transferencia latente
    Chn = Chn_list,  # Coeficiente de transferencia sensible
    Ri = Ri_list ,
    Ts = Ts_list ,
    Ta = Ta_list ,
    Tm = Tm_list 
    
  )
  
  # Retornar el data frame con los resultados
  return(df_resultados)
}


calcular_balance_energia_cc <- function(k_down, albedo, t_snow, t_air, ea_ma, emisividad, l_down, u_a, z_a, z_0, QG = 0, QR = 0, fsca, RH, z_est) {
  
  t_snow <- rep(t_snow, length(t_air))
  emisividad <- rep(emisividad, length(t_air))
  
  # Constantes
  sigma <- 5.67e-8  # Constante de Stefan-Boltzmann (W/m² K⁴) OK
  g <- 9.81         # Aceleración de la gravedad (m/s²) OK 
  rho_a <- 1.225    # Densidad del aire (kg/m³) OK 
  cp <- 1005        # Calor específico del aire (J/kg K) OK 
  k <- 0.4          # Constante de von Kármán (adimensional) OK
  P_0 <- 101.325      # Presión atmosférica estándar (Pa)
  L_v <- 2.5e6      # Calor latente de vaporización (J/kg) OK
  L_f <- 3.34e5     # Calor latente de fusión (J/kg) OK 
  L_s <- 2.834e6    # Calor latente de sublimacion (J/kg) OK 
  rho_w <- 1000     # Densidad del agua (kg/m³) OK 
  
  
  # Inicializar listas para almacenar resultados
  balance_energia_neta_list <- vector("numeric", length(k_down))
  Qnk_list <- vector("numeric", length(k_down))
  Qnl_list <- vector("numeric", length(k_down))
  QH_list <- vector("numeric", length(k_down))
  QE_list <- vector("numeric", length(k_down))
  Ce_list <- vector("numeric", length(k_down))
  Ch_list <- vector("numeric", length(k_down))
  Chn_list <- vector("numeric", length(k_down))
  Ri_list <- vector("numeric", length(k_down)) 
  
  Ts_list <- vector("numeric", length(k_down)) 
  Ta_list <- vector("numeric", length(k_down)) 
  Tm_list <- vector("numeric", length(k_down)) 
  Pa_list <- vector("numeric", length(k_down)) 
  
  # Bucle sobre cada punto en el tiempo (o cada posición)
  for (i in 1:length(k_down)) {
    
    # Conversión de temperaturas a Kelvin
    Ts <- t_snow[i] + 273.15  # Superficie de nieve en Kelvin (K) OK
    Ta <- t_air[i] + 273.15   # Temperatura del aire en Kelvin (K) OK
    
    
    print(paste("Iteración:", i))
    print(paste("z_est:", z_est))
    print(paste("Ta:", Ta))
    print(paste("ea_ma[i]:", ea_ma[i]))
    
    ### CORRECCION PRESION 
    
    Pa <- 101.325 * (1 - ((0.0065 * z_est) / Ta))^5.26
    
    print(paste("Pa:", Pa))
    
    #### RADIACIÓN DE ONDA CORTA ####
    
    k_up <- albedo[i] * k_down[i] # OK 
    
    Qnk <- k_down[i] - k_up # OK
    
    print(paste("k_up:", k_up))
    print(paste("Qnk: ", Qnk ))
    
    
    #### RADIACIÓN DE ONDA LARGA ####
    
    
    l_up <- emisividad[i] * sigma * (Ts^4) + (1 - emisividad[i]) * l_down[i] # OK
    
    
    Qnl <- l_down[i] - l_up # OK
    
    print(paste("l_up:" , l_up))
    
    print(paste(" Qnl: " , Qnl))
    
    
    #### FLUJO DE CALOR SENSIBLE ####
    
    Tm <- (Ta + Ts) / 2 # OK 
    
    u_a_i <- max(u_a[i], 0.1) #OK 
    
    Ri_B <- (g * (Tm^-1) * z_a * (Ta - Ts) )/ u_a_i^2  # Número de Richardson EN KELVIN OK 
    
    
    
    zw = calcular_zs(u_a_i, z_a, z_0,tipo = "vapor")
    zt = calcular_zs(u_a_i, z_a, z_0,tipo = "temperatura")
    
    
    
    Chn <- (k^2) / ((log(z_a / z_0) * log(z_a / 0.1 * z_0))) 
    
    # 
    #     Ch <- ifelse(Ri_B < 0.01,(Chn * (1 - 16 * Ri_B)^0.75)# INESTABLE  OK
    #                  , (Chn * (1 - 5 * Ri_B)^2)) # ESTABLE OK
    # 
    #     Ch <- ifelse(is.finite(Ch), Ch, Chn)
    
    
    QH <- rho_a * cp * Chn * u_a_i * (Ta - Ts) # OK 
    
    print(paste("Ch:", Chn))
    print(paste(" QH: ",  QH ))
    
    
    #### FLUJO DE CALOR LATENTE ####
    
    
    Ps <- 0.6112 * exp((22.46 * t_snow[i]) / (272.62 + t_snow[i])) # EN kPA 
    
    # e_0 <- RH[i] * Ps / 100 # PA 
    
    # # # 
    # Ce <- ifelse(Ri_B < -0.01, Chn * (1 - 10 * Ri_B)^(-1/2), Chn *(1 + 10 * Ri_B))
    
    Ce <- (k^2) / ((log(z_a / z_0) * log(z_a / 0.1*z_0))) 
    
    
    
    QE <- rho_a * L_s * Ce * u_a_i * 0.622 * ((Ps - ea_ma[i])/Pa)   # E_o es la actual de la nieve y se necesita la saturacion 
    
    print(paste("Ce:", Ce))
    print(paste(" QE: ",  QE ))
    # print(paste("e_0:", e_0 ))
    print(paste("Ri_B: ",  Ri_B ))
    print(paste("Ri_B: ",  Ps ))
    
    
    #### BALANCE DE ENERGÍA TOTAL ####
    
    
    balance_energia <- Qnk + Qnl + QH + QE + QG + QR
    balance_energia_neta <- balance_energia * fsca[i]
    
    
    print(paste("balance_energia_neta:", balance_energia_neta ))
    
    #### Almacenar resultados ####
    
    
    balance_energia_neta_list[i] <- balance_energia
    Qnk_list[i] <- Qnk
    Qnl_list[i] <- Qnl
    QH_list[i] <- QH
    QE_list[i] <- QE
    Ce_list[i] <- Ce
    # Ch_list[i] <- Ch
    Pa_list[i] <- Pa
    
    
    Chn_list[i] <- Chn
    Ri_list[i] <- Ri_B
    Ts_list[i] <- Ts
    Ta_list[i] <- Ta
    Tm_list[i] <- Tm
  }
  
  # Crear un data frame con los resultados
  df_resultados <- data.frame(
    Tiempo = 1:length(k_down),
    Balance_Energia_Neta = balance_energia_neta_list,
    Radiacion_Onda_Corta_Neta = Qnk_list,
    Radiacion_Onda_Larga_Neta = Qnl_list,
    Calor_Sensible = QH_list,
    Calor_Latente = QE_list,
    Pa = Pa_list,
    # Ch = Ch_list,
    # Ce = Ce_list,  # Coeficiente de transferencia latente
    Chn = Chn_list,  # Coeficiente de transferencia sensible
    Ri = Ri_list ,
    Ts = Ts_list ,
    Ta = Ta_list ,
    Tm = Tm_list 
    
  )
  
  # Retornar el data frame con los resultados
  return(df_resultados)
}


sca = rep(1, length(t_air))

t_snow= 0




# GENERAR RESULTADO SC


resultado_balance <- calcular_balance_energia_sc(
  k_down, albedo_1, t_snow, t_air, ea_ma, emisividad = 0.95, l_down, u_a, z_a = 2 , z_0 = 0.001,  QG = 0, QR = 0, fsca = sca, RH, z_est = 3500
)



resultado_balance_reversed <- resultado_balance[nrow(resultado_balance):1, ]
print(resultado_balance_reversed)


# GENERAR RESULTADO CC 



resultado_balance_cc <- calcular_balance_energia_cc(
  k_down, albedo_1, t_snow, t_air, ea_ma, emisividad = 0.95, l_down, u_a, z_a = 2 , z_0 = 0.001, QG = 0, QR = 0, fsca = sca, RH, z_est = 3500
)


resultado_balance_reversed_cc<- resultado_balance_cc[nrow(resultado_balance_cc):1, ]
print(resultado_balance_reversed_cc)


# GENERAR DERRETIMIENTO 


calcular_derretimiento_y_SWE <- function(resultado, L_f = 3.34e5, rho_w = 1000, delta_t = 3600) {
  
  
  # L_f: Calor latente de fusión (J/kg)
  # rho_w: Densidad del agua (kg/m³)
  # delta_t: Intervalo de tiempo en segundos (por defecto, 1 hora)
  
  # Inicializar variables para el SWE acumulado y el derretimiento potencial
  
  SWE_acumulado_mm <- 0
  derretimiento_mm_h_list <- vector("numeric", length(resultado$Balance_Energia_Neta))
  derretimiento_m_s_list <- vector("numeric", length(resultado$Balance_Energia_Neta))
  SWE_acumulado_list <- vector("numeric", length(resultado$Balance_Energia_Neta))
  
  for (i in 1:nrow(resultado)) {
    # Balance de energía neto para la posición i (W/m² = J/(s·m²))
    balance_energia_neta <- resultado$Balance_Energia[i]
    
    # Derretimiento potencial en m/s (sin conversión adicional)
    derretimiento_m_s <- ifelse(
      balance_energia_neta > 0,
      balance_energia_neta / (L_f * rho_w), # m/s
      0
    )
    
    # Derretimiento en mm/h (multiplicando por 3600 s/h y 1000 mm/m)
    derretimiento_mm_h <- derretimiento_m_s * delta_t * 1000 # mm/h
    
    # Acumulado del SWE en mm
    SWE_acumulado_mm <- SWE_acumulado_mm + derretimiento_mm_h
    
    # Guardar los valores en las listas
    derretimiento_mm_h_list[i] <- derretimiento_mm_h
    derretimiento_m_s_list[i] <- derretimiento_m_s
    SWE_acumulado_list[i] <- SWE_acumulado_mm
  }
  
  # Crear un nuevo data frame con los resultados
  df_derretimiento_SWE <- data.frame(
    Tiempo = resultado$Tiempo,
    Derretimiento_mm_h = derretimiento_mm_h_list,
    Derretimiento_m_s = derretimiento_m_s_list,
    SWE_Acumulado_mm = SWE_acumulado_list
  )
  
  # Retornar el nuevo data frame
  return(df_derretimiento_SWE)
}


resultado_derretimiento_SWE_r <- calcular_derretimiento_y_SWE(resultado_balance_reversed)
print(resultado_derretimiento_SWE_r)



resultado_derretimiento_SWE_cc <- calcular_derretimiento_y_SWE(resultado_balance_reversed_cc)
print(resultado_derretimiento_SWE_cc)



#### PANEL GRAFICOS BALANCE ####



df_grafico <- data.frame(
  Fecha = as.POSIXct(subset_meteo$Fecha, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  SWE_Observado = SWE,
  SWE_Retrospectivo = rev(resultado_derretimiento_SWE_r$SWE_Acumulado_mm),
  SWE_Corregido = rev(resultado_derretimiento_SWE_cc$SWE_Acumulado_mm)
)

# Crear el gráfico con un marco
grafico_SWE <- ggplot(df_grafico, aes(x = Fecha)) +
  geom_line(aes(y = SWE_Observado, color = "SWE Observado"), size = 1) +
  geom_line(aes(y = SWE_Retrospectivo, color = "SWE s/c atm"), size = 1) +
  geom_line(aes(y = SWE_Corregido, color = "SWE c/c atm"), size = 1) +
  
  labs(
    title = "SWE Observado y retrospectivo",
    x = NULL,  # Eliminar etiqueta del eje x para simplicidad
    y = "SWE [mm]"
  ) +
  scale_color_manual(values = c(
    "SWE Observado" = "black",
    "SWE s/c atm" = "red",
    "SWE c/c atm" = "blue"
  )) +
  scale_x_datetime(date_labels = "%d-%b", date_breaks = "1 day") +  # Graduación diaria
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text     = element_text(size = 12, face = "bold"),
    plot.title      = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5, size = 14, face = "italic"),
    panel.border    = element_rect(color = "black", fill = NA, size = 2),
    axis.text.x     = element_text(face = "bold", size = 12),
    axis.text.y     = element_text(face = "bold", size = 16),
    axis.title.y    = element_text(face = "bold", size = 16),
    axis.title.x    = element_text(face = "bold", size = 16),
    strip.text      = element_text(size = 12, face = "bold")
  )







# Imprimir el gráfico
print(grafico_SWE)






#### ENERGIA NETA 



start_time <- as.POSIXct(fecha_max_SWE, tz = "UTC")  # Fecha inicial asociada al tiempo 298
fecha_final <- as.POSIXct(fecha_cero_SWE, tz = "UTC")  # Fecha final


balance_df <- resultado_balance_reversed_cc %>%
  dplyr::mutate(Fecha = seq(from = start_time, by = "1 hour", length.out = nrow(resultado_balance_reversed_cc)))

balance_sc_df <- resultado_balance_reversed %>%
  dplyr::mutate(Fecha = seq(from = start_time, by = "1 hour", length.out = nrow(resultado_balance_reversed)))


cat("Verificación de tiempo 298:\n")
print(balance_df %>% filter(Tiempo == 298))
print(balance_sc_df %>% filter(Tiempo == 298))


balance_df_oct <- balance_df %>%
  filter(Fecha >= start_time & Fecha <= fecha_final)

balance_sc_df_oct <- balance_sc_df %>%
  filter(Fecha >= start_time & Fecha <= fecha_final)


cat("\nDatos filtrados - balance_df_oct:\n")
print(head(balance_df_oct))

cat("\nDatos filtrados - balance_sc_df_oct:\n")
print(head(balance_sc_df_oct))


grafico_calor_latente <- ggplot() +
  geom_line(data = balance_df_oct, aes(x = Fecha, y = Calor_Latente, color = "Con Corrección", group = 1), size = 1) +
  geom_line(data = balance_sc_df_oct, aes(x = Fecha, y = Calor_Latente, color = "Sin Corrección", group = 1), size = 1) +
  labs(
    title = "Flujo de Calor Latente",
    x = "Fecha",
    y = "Calor Latente [W/m²]"
  ) +
  scale_color_manual(values = c(
    "Con Corrección" = "black",
    "Sin Corrección" = "red"
  )) +
  scale_x_datetime(
    limits = c(start_time, fecha_final),
    date_labels = "%d-%b",
    date_breaks = "2 days"
  ) +
  theme_minimal() +
   theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text     = element_text(size = 12, face = "bold"),
    plot.title      = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5, size = 14, face = "italic"),
    panel.border    = element_rect(color = "black", fill = NA, size = 2),
    axis.text.x     = element_text(face = "bold", size = 12),
    axis.text.y     = element_text(face = "bold", size = 16),
    axis.title.y    = element_text(face = "bold", size = 16),
    axis.title.x    = element_text(face = "bold", size = 16),
    strip.text      = element_text(size = 12, face = "bold")
  )


grafico_balance_energia <- ggplot() +
  geom_line(data = balance_df_oct, aes(x = Fecha, y = Balance_Energia_Neta, color = "Con Corrección", group = 1), size = 1) +
  geom_line(data = balance_sc_df_oct, aes(x = Fecha, y = Balance_Energia_Neta, color = "Sin Corrección", group = 1), size = 1) +
  labs(
    title = "Balance Neto de Energía",
    x = "Fecha",
    y = "Balance Energía Neta [W/m²]"
  ) +
  scale_color_manual(values = c(
    "Con Corrección" = "black",
    "Sin Corrección" = "red"
  )) +
  scale_x_datetime(
    limits = c(start_time, fecha_final),
    date_labels = "%d-%b",
    date_breaks = "2 days"
  ) +
  theme_minimal() +
   theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text     = element_text(size = 12, face = "bold"),
    plot.title      = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5, size = 14, face = "italic"),
    panel.border    = element_rect(color = "black", fill = NA, size = 2),
    axis.text.x     = element_text(face = "bold", size = 12),
    axis.text.y     = element_text(face = "bold", size = 16),
    axis.title.y    = element_text(face = "bold", size = 16),
    axis.title.x    = element_text(face = "bold", size = 16),
    strip.text      = element_text(size = 12, face = "bold")
  )



panel_E <- grafico_balance_energia / grafico_calor_latente +
  plot_layout(ncol = 1, heights = c(1, 1))

print(panel_E)


panel_energia <- grafico_balance_energia / grafico_calor_latente +
  plot_layout(ncol = 1, heights = c(1, 1))

panel_definitivo <- grafico_SWE | panel_energia

print(panel_definitivo)
