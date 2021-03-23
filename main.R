# Title     : Projekt MS
# Objective : NO
# Created by: Jan Staniszewski, Jerzy Balcer, Konrad Krawczyk, Daniel Karnacz, Marcin Rak
# Created on: 13.03.2021

# Konieczne jest doinstalowanie ponizszych bibliotek
library("psych")
library("moments")
library("ineq")

# Sciezka Janek
#dane <- read.csv("C:/Users/wyga/IdeaProjects/Projekt MS/Exasens.csv", sep=",", header = TRUE) #Pobieranie danych z pliku CSV do programy

# Sciezka jerzy
dane <- read.csv("C:/Users/Jerzy/PycharmProjects/ProjektMS/ProjektMS/Exasens.csv", sep=",", header = TRUE) #Pobieranie danych z pliku CSV do programy

# Wczytanie danych do ramki
dane_grupy <- split(dane, dane$Diagnosis)

# ZAD 1 - w raporcie
# ZAD 2

wartosci <- c(Asthma = summary(dane_grupy$Asthma$Age), COPD = summary(dane_grupy$COPD$Age), HC = summary(dane_grupy$HC$Age), Infected = summary(dane_grupy$Infected$Age))

# Miary przecietne
srednia <- c(Asthma = mean(dane_grupy$Asthma$Age), COPD = mean(dane_grupy$COPD$Age), HC = mean(dane_grupy$HC$Age), Infected = mean(dane_grupy$Infected$Age))
kwartyl1 <- c(Asthma = wartosci["Asthma.1st Qu."], COPD = wartosci["COPD.1st Qu."], HC = wartosci["HC.1st Qu."], Infected = wartosci["Infected.1st Qu."])
kwartyl2 <- c(Asthma = wartosci["Asthma.Median"], COPD = wartosci["COPD.Median"], HC = wartosci["HC.Median"], Infected = wartosci["Infected.Median"])
kwartyl3 <- c(Asthma = wartosci["Asthma.3rd Qu."], COPD = wartosci["COPD.3rd Qu."], HC = wartosci["HC.3rd Qu."], Infected = wartosci["Infected.3rd Qu."])
obliczModalna <- function(vector) { wartosc <- unique(vector)
                                    wartosc[which.max(tabulate(match(vector, wartosc)))] }
moda <- c(Asthma = obliczModalna(dane_grupy$Asthma$Age), COPD = obliczModalna(dane_grupy$COPD$Age), HC = obliczModalna(dane_grupy$HC$Age), Infected = obliczModalna(dane_grupy$Infected$Age))

# Miary Zroznicowania
rozstepy_miedzykwartylowy <- c(Asthma = IQR(dane_grupy$Asthma$Age), COPD = IQR(dane_grupy$COPD$Age), HC = IQR(dane_grupy$HC$Age), Infected = IQR(dane_grupy$Infected$Age))
rozstep_wynikow <- c(Asthma = max(dane_grupy$Asthma$Age) - min(dane_grupy$Asthma$Age), COPD = max(dane_grupy$COPD$Age) - min(dane_grupy$COPD$Age), HC = max(dane_grupy$HC$Age) - min(dane_grupy$HC$Age), Infected = max(dane_grupy$Infected$Age) - min(dane_grupy$Infected$Age))
odchylenie_standardowe <- c(Asthma = sd(dane_grupy$Asthma$Age), COPD = sd(dane_grupy$COPD$Age), HC = sd(dane_grupy$HC$Age), Infected = sd(dane_grupy$Infected$Age))
wariancje <- c(Asthma = var(dane_grupy$Asthma$Age), COPD = var(dane_grupy$COPD$Age), HC = var(dane_grupy$HC$Age), Infected = var(dane_grupy$Infected$Age))
wspolczynnik_zmiennosci <- c(Asthma = sd(dane_grupy$Asthma$Age)/mean(dane_grupy$Asthma$Age), COPD = sd(dane_grupy$COPD$Age)/mean(dane_grupy$COPD$Age), HC = sd(dane_grupy$HC$Age)/mean(dane_grupy$HC$Age), Infected = sd(dane_grupy$Infected$Age)/mean(dane_grupy$Infected$Age))

# Miary Asymetrii
skosnosc <- c(Asthma = skew(dane_grupy$Asthma$Age), COPD = skew(dane_grupy$COPD$Age), HC = skew(dane_grupy$HC$Age), Infected = skew(dane_grupy$Infected$Age))

# Miary Koncentracji
kurtoza <- c(Asthma = kurtosis(dane_grupy$Asthma$Age), COPD = kurtosis(dane_grupy$COPD$Age), HC = kurtosis(dane_grupy$HC$Age), Infected = kurtosis(dane_grupy$Infected$Age))

# Histogram
histAsthma <- hist(dane_grupy$Asthma$Age, probability = F, main = "Histogram dla grupy Asthma", xlab = "Grupa Wiekowa", ylab = "Liczebnosc", xlim=range(0,100) ,ylim = range(0,20) )
histCOPD <- hist(dane_grupy$COPD$Age, probability = F, main = "Histogram dla grupy COPD", xlab = "Grupa Wiekowa", ylab = "Liczebnosc", xlim=range(0,100) ,ylim = range(0,20) )
histHC <- hist(dane_grupy$HC$Age, probability = F, main = "Histogram dla grupy HC", xlab = "Grupa Wiekowa", ylab = "Liczebnosc", xlim=range(0,100) ,ylim = range(0,20) )
histInfected <- hist(dane_grupy$Infected$Age, probability = F, main = "Histogram dla grupy Infected", xlab = "Grupa Wiekowa", ylab = "Liczebnosc", xlim=range(0,100) ,ylim = range(0,20) )

boxplot(dane_grupy$Asthma$Age, dane_grupy$COPD$Age,dane_grupy$HC$Age,dane_grupy$Infected$Age, ylab = "Wiek", xlab = "Grupa", main = "Rozkład danych na wykresie pudełkowym", names = c("Asthma", "COPD", "HC", "Infected"))

# ZAD 3

# Asthma
length_bez_na_asthma <- length(dane_grupy$Asthma$Real.part.Average[!is.na(dane_grupy$Asthma$Real.part.Average)])
wart_statystyki_testowej_asthma <- ((mean(dane_grupy$Asthma$Real.part.Average, na.rm = "true")-(-470))/(sd(dane_grupy$Asthma$Real.part.Average, na.rm = "true")/sqrt(length_bez_na_asthma)))
kwantyl_rozkladu_asthma <- qnorm(0.975)
wynik_testu_asthma_zad3 <- "przecietna wartosc moze byc rowna -470 na danym poziomie istotnosci"

if(wart_statystyki_testowej_asthma<(0-kwantyl_rozkladu_asthma) || wart_statystyki_testowej_asthma>kwantyl_rozkladu_asthma)
  wynik_testu_asthma_zad3 = "przecietna wartosc jest rozna od -470"

# Infected
length_bez_na_infected <- length(dane_grupy$Infected$Real.part.Average[!is.na(dane_grupy$Infected$Real.part.Average)])
wart_statystyki_testowej_infected <- ((mean(dane_grupy$Infected$Real.part.Average, na.rm = "true")-(-470))/(sd(dane_grupy$Infected$Real.part.Average, na.rm = "true")/sqrt(length_bez_na_infected)))
kwantyl_rozkladu_infected <- qnorm(0.975)
wynik_testu_infected_zad3 <- "przecietna wartosc moze byc rowna -470 na danym poziomie istotnosci"

if(wart_statystyki_testowej_infected<(0-kwantyl_rozkladu_infected) || wart_statystyki_testowej_infected>kwantyl_rozkladu_infected)
  wynik_testu_infected_zad3 = "przecietna wartosc jest rozna od -470"

# ZAD 4

# a)

# Zakomentowane aby nie losowac nowych probek za kazdym razem
# zbiorDanychAsthma <- sample(dane_grupy$Asthma$Age,20)
# zbiorDanychCOPD <- sample(dane_grupy$Asthma$Age,20)
# zbiorDanychHC <- sample(dane_grupy$Asthma$Age,20)
# zbiorDanychInfected <- sample(dane_grupy$Asthma$Age,20)

# Wynik jednego z losowan
zbiorDanychAsthma <- c(75, 64 ,53 ,55, 78, 76, 61, 66 ,50 ,21 ,21 ,58, 63 ,43, 49 ,73 ,57 ,79, 20 ,33)
zbiorDanychCOPD <- c(68, 49, 81 ,35 ,61, 28, 76, 64, 17, 49, 29, 57, 61, 53, 63, 76, 65, 53, 63, 22)
zbiorDanychHC <- c(57, 48, 49, 51, 66, 57, 38, 43, 67, 49, 20, 65, 22, 53 ,76, 58, 63, 51, 80, 61)
zbiorDanychInfected <- c(53, 64, 63, 80, 88, 53, 52, 60, 67, 51, 65, 80, 51, 55, 73, 19, 37, 49, 40, 66)

testAsthma <- shapiro.test(zbiorDanychAsthma)$p.value
testCOPD <- shapiro.test(zbiorDanychCOPD)$p.value
testHC <- shapiro.test(zbiorDanychHC)$p.value
testInfected <- shapiro.test(zbiorDanychInfected)$p.value

wynik_testAsthma_zad4a <- "wiek w danej probie ma rozklad normalny"
wynik_testCOPD_zad4a <- "wiek w danej probie ma rozklad normalny"
wynik_testHC_zad4a <- "wiek w danej probie ma rozklad normalny"
wynik_testInfected_zad4a <- "wiek w danej probie ma rozklad normalny"

if(testAsthma<=0.05) wynik_testAsthma_zad4a = "rozklad wieku nie jest rozkladem normalnym"
if(testCOPD<=0.05) wynik_testAsthma_zad4a = "rozklad wieku nie jest rozkladem normalnym"
if(testHC<=0.05) wynik_testHC_zad4a = "rozklad wieku nie jest rozkladem normalnym"
if(testInfected<=0.05) wynik_testInfected_zad4a = "rozklad wieku nie jest rozkladem normalnym"

# b)




