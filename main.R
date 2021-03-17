# Title     : Projekt MS
# Objective : NO
# Created by: Jan Staniszewski, Jerzy Balcer, Konrad Krawczyk, Daniel Karnacz, Marcin Rak
# Created on: 13.03.2021

#install.packages("psych")
library("psych")

# sciezka janek
#dane <- read.csv("C:/Users/wyga/IdeaProjects/Projekt MS/Exasens.csv", sep=",", header = TRUE) #Pobieranie danych z pliku CSV do programy

# sciezka jerzy
dane <- read.csv("C:/Users/Jerzy/PycharmProjects/ProjektMS/ProjektMS/Exasens.csv", sep=",", header = TRUE) #Pobieranie danych z pliku CSV do programy

# wczytanie danych do ramki
dane_grupy <- split(dane, dane$Diagnosis)

# ZAD 1

wartosci <- c(Asthma = summary(dane_grupy$Asthma$Age), COPD = summary(dane_grupy$COPD$Age), HC = summary(dane_grupy$HC$Age), Infected = summary(dane_grupy$Infected$Age))

# Miary przecietne
srednie <- c(Asthma = mean(dane_grupy$Asthma$Age), COPD = mean(dane_grupy$COPD$Age), HC = mean(dane_grupy$HC$Age), Infected = mean(dane_grupy$Infected$Age))
srednie_geo <- c(Asthma = geometric.mean(dane_grupy$Asthma$Age), COPD = geometric.mean(dane_grupy$COPD$Age), HC = geometric.mean(dane_grupy$HC$Age), Infected = geometric.mean(dane_grupy$Infected$Age))
srednie_harm <- c(Asthma = harmonic.mean(dane_grupy$Asthma$Age), COPD = harmonic.mean(dane_grupy$COPD$Age), HC = harmonic.mean(dane_grupy$HC$Age), Infected = harmonic.mean(dane_grupy$Infected$Age))
srednie_kwadratowa <- c(Asthma = sqrt(mean(dane_grupy$Asthma$Age)^2), COPD = sqrt(mean(dane_grupy$COPD$Age)^2), HC = sqrt(mean(dane_grupy$HC$Age)^2), Infected = sqrt(mean(dane_grupy$Infected$Age)^2))
kwartyl1 <- c(Asthma = wartosci["Asthma.1st Qu."], COPD = wartosci["COPD.1st Qu."], HC = wartosci["HC.1st Qu."], Infected = wartosci["Infected.1st Qu."])
kwartyl2 <- c(Asthma = wartosci["Asthma.Median"], COPD = wartosci["COPD.Median"], HC = wartosci["HC.Median"], Infected = wartosci["Infected.Median"])
kwartyl3 <- c(Asthma = wartosci["Asthma.3rd Qu."], COPD = wartosci["COPD.3rd Qu."], HC = wartosci["HC.3rd Qu."], Infected = wartosci["Infected.3rd Qu."])
obliczModalna <- function(vector) { wartosc <- unique(vector)
                                    wartosc[which.max(tabulate(match(vector, wartosc)))] }
moda <- c(Asthma = obliczModalna(dane_grupy$Asthma$Age), COPD = obliczModalna(dane_grupy$COPD$Age), HC = obliczModalna(dane_grupy$HC$Age), Infected = obliczModalna(dane_grupy$Infected$Age))


# Miary Zroznicowania
rozstepy_cwiartkowe <- c(Asthma = IQR(dane_grupy$Asthma$Age), COPD = IQR(dane_grupy$COPD$Age), HC = IQR(dane_grupy$HC$Age), Infected = IQR(dane_grupy$Infected$Age))
rozstepy <- c(Asthma = max(dane_grupy$Asthma$Age) - min(dane_grupy$Asthma$Age), COPD = max(dane_grupy$COPD$Age) - min(dane_grupy$COPD$Age), HC = max(dane_grupy$HC$Age) - min(dane_grupy$HC$Age), Infected = max(dane_grupy$Infected$Age) - min(dane_grupy$Infected$Age))
odchylenia <- c(Asthma = sd(dane_grupy$Asthma$Age), COPD = sd(dane_grupy$COPD$Age), HC = sd(dane_grupy$HC$Age), Infected = sd(dane_grupy$Infected$Age))
wariancje <- c(Asthma = var(dane_grupy$Asthma$Age), COPD = var(dane_grupy$COPD$Age), HC = var(dane_grupy$HC$Age), Infected = var(dane_grupy$Infected$Age))
odchylenie <- c(Asthma = mad(dane_grupy$Asthma$Age), COPD = mad(dane_grupy$COPD$Age), HC = mad(dane_grupy$HC$Age), Infected = mad(dane_grupy$Infected$Age))
zmiennosc <- c(Asthma = sd(dane_grupy$Asthma$Age)/mean(dane_grupy$Asthma$Age), COPD = sd(dane_grupy$COPD$Age)/mean(dane_grupy$COPD$Age), HC = sd(dane_grupy$HC$Age)/mean(dane_grupy$HC$Age), Infected = sd(dane_grupy$Infected$Age)/mean(dane_grupy$Infected$Age))
odch_cwiartkowe <- c(Asthma = rozstepy_cwiartkowe["Asthma"]/2 , COPD = rozstepy_cwiartkowe["COPD"]/2, HC = rozstepy_cwiartkowe["HC"]/2, Infected = rozstepy_cwiartkowe["Infected"]/2)

# Miary Asymetrii

