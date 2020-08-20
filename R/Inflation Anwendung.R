#-----------------------------------------------------------------------------------------------------------#
#   Anwendung des R-Pakets TimeSeries
#   
#   Datensatz: monatlich erfasste Inflationsraten in Deutschland zwischen 2017 und 2019 (36 Datenpunkte)
#-----------------------------------------------------------------------------------------------------------#

library("readxl")
library("dplyr")
library("tidyr")
library("tseries")
library("TimeSeries")
library("ggplot2")


### Vorbereitung

# Import des Datensatzes
ts_inf <- read_excel("C:\\Users\\Benjamin Riesch\\Mirror\\Universität Heidelberg Semesterübersicht\\Semester 7 SS 2020\\RKursHD20\\Inflation_Germany.xlsx")


# Reduktion des Datensatzes
ts_inf %>% filter(Year>2016) -> ts_inf


# Anpassung des Datensatzes
ts_inf %>% mutate(Day="1") %>%  
  unite("Time",Year, Month, Day, sep = "-") %>% 
  mutate(d.ts= c(0,diff(Inflation))) %>% 
  mutate(mean = mean(d.ts))-> ts_data

ts_data%>% select(d.ts) %>% pull() -> diff_ts

diff_ts[1:36] -> diff_ts_red



# Erstellen von Übersichtsgrafiken
ggplot(data = ts_data) +
  geom_point(mapping = aes(x = as.Date(Time), y = d.ts)) + 
  labs(title="Verteilung der Inflationsratendifferenzen",
    x="", y = "Differenz der Inflationsrate \n im Vergleich zum vorherigen Monat") +
  geom_line(mapping = aes(x = as.Date(Time), y = d.ts)) + 
  geom_line(mapping = aes(x = as.Date(Time), y = mean), lwd = 0.7, color="red") +
  theme_classic()


# Test auf Stationarität
adf.test(diff_ts, alternative = "stationary", k=0)



### Anwendung des Pakets TimeSeries

# Durbin-Levinson Algorithm
DL_Out <-  function(ts, n) {
  ts_ak <- ts
  coeff <-  double(36)
  for (i in 1:3) {
    coeff[(36-n+i):(36+i-1)] <- DL(ts_ak,n)
    ts_ak[36+i] <- sum(ts_ak*coeff)
  }
  ts_ak[1:36] <- NA
  ts_ak
}

ggplot(data = ts_data) +
  geom_point(mapping = aes(x = as.Date(Time), y = d.ts)) + 
  labs(title="Verteilung der Inflationsratendifferenzen",
       x="", y = "Differenz der Inflationsrate \n im Vergleich zum vorherigen Monat") +
  geom_line(mapping = aes(x = as.Date(Time), y = d.ts))+
  geom_line(mapping = aes(x = as.Date(Time), y = 0), lwd=0.7, color = "grey")+
  
  geom_line(mapping = aes(x = as.Date(Time), y = DL_Out(diff_ts_red,12L)), lwd = 1, color="blue")+
  geom_point(mapping = aes(x = as.Date(Time), y = DL_Out(diff_ts_red,12L)), lwd = 1, color="blue")+
  geom_line(mapping = aes(x = as.Date(Time), y = DL_Out(diff_ts_red,24L)), lwd = 1, color="red")+
  geom_point(mapping = aes(x = as.Date(Time), y = DL_Out(diff_ts_red,24L)), lwd = 1, color="red")+
  theme_classic()



# Innovation Algorithm
IA_Out <-  function(ts, n) {
  ts_ak <- ts
  for (i in 1:3) {
    ts_ak[36+i]<- innovation_prediction(ts_ak,1L+i,n)
  }
  ts_ak[1:36] <- NA
  ts_ak
}

ggplot(data = ts_data) +
  geom_point(mapping = aes(x = as.Date(Time), y = d.ts)) + 
  labs(title="Verteilung der Inflationsratendifferenzen",
       x="", y = "Differenz der Inflationsrate \n im Vergleich zum vorherigen Monat") +
  geom_line(mapping = aes(x = as.Date(Time), y = d.ts))+
  geom_line(mapping = aes(x = as.Date(Time), y = 0), lwd=0.7, color = "grey")+
  
  geom_line(mapping = aes(x = as.Date(Time), y = IA_Out(diff_ts_red,12L)), lwd = 1, color="blue")+
  geom_point(mapping = aes(x = as.Date(Time), y = IA_Out(diff_ts_red,12L)), lwd = 1, color="blue")+
  geom_line(mapping = aes(x = as.Date(Time), y = IA_Out(diff_ts_red,24L)), lwd = 1, color="red")+
  geom_point(mapping = aes(x = as.Date(Time), y = IA_Out(diff_ts_red,24L)), lwd = 1, color="red")+
  theme_classic()


# Periodogram
peri <- periodogram(diff_ts_red) 

plot_periodogram(peri)
