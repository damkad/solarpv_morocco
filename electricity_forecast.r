library(greybox)
?greybox
library(forecast)
library(tidyverse)

#https://www.iea.org/countries/morocco
electricity_cons_data <-  c(9, 9, 10, 11, 11, 12, 12, 13, 13, 13,
                            14, 15, 16, 17, 18, 19, 21, 22, 23, 
                            24, 25, 27, 29, 29, 30, 31, 32, 33)



electricity_cons_data
mmm <- as.data.frame(electricity_cons_data)
mm <- cbind(1990:2017, mmm)
colnames(mm) <- c("Year", "Electricity Consumption in Morocco (TWh)") 

ggplot(mm)+geom_line(aes(x=Year, y= `Electricity Consumption in Morocco (TWh)`)) +
scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

length(electricity_cons_data)
z1 <- ts(matrix(electricity_cons_data), start = c(1990, 1), frequency = 1)

plot(z1, plot.type = "single")

md <- auto.arima(z1, d= 1, D=1, trace = TRUE)
summary(md)
checkresiduals(md)
f_1<- forecast(md, h= 13)
autoplot(f_1, include = 20) + xlab("Year")+ylab("Electricity Consumption in Morocco (TWh)") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
f_1
print(f_1)

out <- read.table(text=capture.output(f_1))

write.csv(out, "prediction.csv")
#f = 45TWh
list.files()


data <- read.csv("n/global_power_plant_database.csv")
data <- subset(data, country_long=="Morocco")
data_renewable <- data %>% subset(primary_fuel == "Hydro" | primary_fuel == "Wind" |
                                    primary_fuel == "Solar")
summary(data_renewable)
#power plant-under deve 18-21

unique(data_renewable$primary_fuel)
#1-The integrated wind power programme — First phase: Taza wind farm project of 150 MW(e) (IPP project);

#2-The integrated wind power programme — Second phase: 5 wind farm projects of 850 MW(e) (IPP project);

#3-The repowering of the Abdelkhalek Torres wind park from 50 MW to 100 MW and an extension of 200 MW;

#4-Three pumped storage power plant projects: Abdelmoumen (350 MW), Ifahsa (300 MW) and El Menzel (300 MW);

#5-The Noor Tafilalt, Noor Atlas and Noor Argana photovoltaic (PV) solar power plants with a total of 570 MW(e) installed capacity (managed by ONEE-BE);

#6-The Noor Ouarzazate, Noor Layoune, Noor Boujdour and Noor Midelt solar power plants (concentrated solar power (CSP) and PV) with a total of 1320 MW(e) installed capacity (managed by MASEN);

#7-Renewable energy projects developed by private companies in the framework of Law No. 13-09 (currently, up to 740 MW(e) of solar PV, 220 MW of wind power and 126 MW(e) of microhydraulic power plants are developed in the framework of this law).



#1 and 2
data_renewable$status <- "Existing"
new_entry <- cbind("MAR", "Morocco", "Taza", "", "1000.0", "34.2239", "-4.0066", "Wind", "", "", "", "NA", "", "", "https://www-pub.iaea.org/MTCD/Publications/PDF/cnpp2018/countryprofiles/Morocco/Morocco.htm", "", "", "", "", "", "", "", "", "", "Planned")
new_entry <- as.data.frame(new_entry) 
colnames(new_entry) <- colnames(data_renewable)
data_renewable <- rbind(data_renewable, new_entry)
#3
new_entry <- cbind("MAR", "Morocco", "Abdelkhalek Torres wind park", "", "250.0", "35.8184", "-5.4538", "Wind", "", "", "", "NA", "", "", "https://www-pub.iaea.org/MTCD/Publications/PDF/cnpp2018/countryprofiles/Morocco/Morocco.htm", "", "", "", "", "", "", "", "", "", "Planned")
new_entry <- as.data.frame(new_entry) 
colnames(new_entry) <- colnames(data_renewable)
data_renewable <- rbind(data_renewable, new_entry)
#4
new_entry <- cbind("MAR", "Morocco", "Abdelmoumen", "", "350.0", "33.5735", "-7.6259", "Hydro", "", "", "", "NA", "", "", "https://www-pub.iaea.org/MTCD/Publications/PDF/cnpp2018/countryprofiles/Morocco/Morocco.htm", "", "", "", "", "", "", "", "", "", "Planned")
new_entry <- as.data.frame(new_entry) 
colnames(new_entry) <- colnames(data_renewable)
data_renewable <- rbind(data_renewable, new_entry)


new_entry <- cbind("MAR", "Morocco", "Ifahsa", "", "300.0", "35.2965", "-5.2306", "Hydro", "", "", "", "NA", "", "", "https://www-pub.iaea.org/MTCD/Publications/PDF/cnpp2018/countryprofiles/Morocco/Morocco.htm", "", "", "", "", "", "", "", "", "", "Planned")
new_entry <- as.data.frame(new_entry) 
colnames(new_entry) <- colnames(data_renewable)
data_renewable <- rbind(data_renewable, new_entry)


new_entry <- cbind("MAR", "Morocco", "El Menzel", "", "300.0", "34.9167", "-8.35", "Hydro", "", "", "", "NA", "", "", "https://www-pub.iaea.org/MTCD/Publications/PDF/cnpp2018/countryprofiles/Morocco/Morocco.htm", "", "", "", "", "", "", "", "", "", "Planned")
new_entry <- as.data.frame(new_entry) 
colnames(new_entry) <- colnames(data_renewable)
data_renewable <- rbind(data_renewable, new_entry)

#5
new_entry <- cbind("MAR", "Morocco", "Noor Tafilalt", "", "120.0", "31.3", "-4.3", "Solar", "", "", "", "NA", "", "", "https://www-pub.iaea.org/MTCD/Publications/PDF/cnpp2018/countryprofiles/Morocco/Morocco.htm", "", "", "", "", "", "", "", "", "", "Planned")
new_entry <- as.data.frame(new_entry) 
colnames(new_entry) <- colnames(data_renewable)
data_renewable <- rbind(data_renewable, new_entry)


new_entry <- cbind("MAR", "Morocco", "Noor Atlas", "", "200.0", "31.0404", "-6.8452", "Solar", "", "", "", "NA", "", "", "https://www-pub.iaea.org/MTCD/Publications/PDF/cnpp2018/countryprofiles/Morocco/Morocco.htm", "", "", "", "", "", "", "", "", "", "Planned")
new_entry <- as.data.frame(new_entry) 
colnames(new_entry) <- colnames(data_renewable)
data_renewable <- rbind(data_renewable, new_entry)


new_entry <- cbind("MAR", "Morocco", "Noor Agana", "", "200.0", "31.3", "-4.5", "Solar", "", "", "", "NA", "", "", "https://www-pub.iaea.org/MTCD/Publications/PDF/cnpp2018/countryprofiles/Morocco/Morocco.htm", "", "", "", "", "", "", "", "", "", "Planned")
new_entry <- as.data.frame(new_entry) 
colnames(new_entry) <- colnames(data_renewable)
data_renewable <- rbind(data_renewable, new_entry)




#6  The Noor Ouarzazate, Noor Layoune, Noor Boujdour and Noor Midelt
new_entry <- cbind("MAR", "Morocco", "Centrale Solaire Noor Ouarzazate 4", "", "72.0", "31.0404", "-6.8449", "Solar", "", "", "", "NA", "", "", "https://www-pub.iaea.org/MTCD/Publications/PDF/cnpp2018/countryprofiles/Morocco/Morocco.htm", "", "", "", "", "", "", "", "", "", "Planned")
new_entry <- as.data.frame(new_entry) 
colnames(new_entry) <- colnames(data_renewable)
data_renewable <- rbind(data_renewable, new_entry)


new_entry <- cbind("MAR", "Morocco", "Noor Layoune", "", "85.0", "26.9941", "-12.9543", "Solar", "", "", "", "NA", "", "", "https://www-pub.iaea.org/MTCD/Publications/PDF/cnpp2018/countryprofiles/Morocco/Morocco.htm", "", "", "", "", "", "", "", "", "", "Planned")
new_entry <- as.data.frame(new_entry) 
colnames(new_entry) <- colnames(data_renewable)
data_renewable <- rbind(data_renewable, new_entry)


new_entry <- cbind("MAR", "Morocco", "Noor Boujdour", "", "100.0", "26.1649", "-14.3636", "Solar", "", "", "", "NA", "", "", "https://www-pub.iaea.org/MTCD/Publications/PDF/cnpp2018/countryprofiles/Morocco/Morocco.htm", "", "", "", "", "", "", "", "", "", "Planned")
new_entry <- as.data.frame(new_entry) 
colnames(new_entry) <- colnames(data_renewable)
data_renewable <- rbind(data_renewable, new_entry)


new_entry <- cbind("MAR", "Morocco", "Noor Midelt", "", "1063.0", "32.6816", "-4.7923", "Solar", "", "", "", "NA", "", "", "https://www-pub.iaea.org/MTCD/Publications/PDF/cnpp2018/countryprofiles/Morocco/Morocco.htm", "", "", "", "", "", "", "", "", "", "Planned")
new_entry <- as.data.frame(new_entry) 
colnames(new_entry) <- colnames(data_renewable)
data_renewable <- rbind(data_renewable, new_entry)



#7 1086Mw
new_entry <- cbind("MAR", "Morocco", "Private companies", "", "740.0", "31.0404", "-6.8452", "Solar", "", "", "", "NA", "", "", "https://www-pub.iaea.org/MTCD/Publications/PDF/cnpp2018/countryprofiles/Morocco/Morocco.htm", "", "", "", "", "", "", "", "", "", "Planned")
new_entry <- as.data.frame(new_entry) 
colnames(new_entry) <- colnames(data_renewable)
data_renewable <- rbind(data_renewable, new_entry)
new_entry <- cbind("MAR", "Morocco", "Private companies", "", "220.0", "31.0404", "-6.8453", "Wind", "", "", "", "NA", "", "", "https://www-pub.iaea.org/MTCD/Publications/PDF/cnpp2018/countryprofiles/Morocco/Morocco.htm", "", "", "", "", "", "", "", "", "", "Planned")
new_entry <- as.data.frame(new_entry) 
colnames(new_entry) <- colnames(data_renewable)
data_renewable <- rbind(data_renewable, new_entry)
new_entry <- cbind("MAR", "Morocco", "Private companies", "", "126.0", "31.0404", "-6.8454", "Hydro", "", "", "", "NA", "", "", "https://www-pub.iaea.org/MTCD/Publications/PDF/cnpp2018/countryprofiles/Morocco/Morocco.htm", "", "", "", "", "", "", "", "", "", "Planned")
new_entry <- as.data.frame(new_entry) 
colnames(new_entry) <- colnames(data_renewable)
data_renewable <- rbind(data_renewable, new_entry)

data_renewable <- droplevels(data_renewable)


write.csv(data_renewable, "power_plant_morocco.csv")


colnames(data_renewable)
data_renewable_hydro <- data_renewable %>% subset(primary_fuel == "Hydro")
data_renewable_solar <- data_renewable %>% subset(primary_fuel == "Solar")
data_renewable_wind <-data_renewable %>% subset(primary_fuel == "Wind")
data_renewable$estimated_generation_gwh <- as.integer(data_renewable$estimated_generation_gwh)

data_renewable$capacity_mw <- as.integer(data_renewable$capacity_mw)
unique(data_renewable$primary_fuel)
sum(data_renewable$capacity_mw)

sum(data_renewable_solar$capacity_mw)
a <- data_renewable_hydro[which(data_renewable_hydro$status == "Planned"), c(5)]
sum(a)
data_renewable_solar$status == "Planned"
sum(data_renewable_wind$capacity_mw)
sum(data_renewable_hydro$capacity_mw)








