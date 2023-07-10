setwd("C:\\Users\\Clarissa\\Desktop\\Masterarbeit\\data")

library(ggplot2)
library(tidyverse)
library(dplyr)
library(FSA)


data <- read_csv("export_gephi_nodes.csv", col_names = TRUE)
view(data)

data_long <- data %>%
  pivot_longer(cols = c("Mitglieder_22", "Mitglieder_23"), names_to = "date", values_to = "members")



################################################################################

## Verteilung ##

# Nicht normalverteilt, aber unimodal
x11()
ggplot(data = data_long) + 
  geom_point(mapping = aes(x = date, y = members)) + 
  geom_boxplot(mapping = aes(x = date, y = members))

x11()  
ggplot(data_long, aes(x= members, fill=date)) +     
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', binwidth=1000) 

x11()  
ggplot(data, aes(x= Mitglieder_23)) +     
  geom_histogram(position = 'identity', binwidth=2000) + 
  labs(x = "Mitgliederzahl 2023", y = "Anzahl der Telegramkanäle")


################################################################################

## Paarvergleich ##

# T test
t.test(x = data$Mitglieder_22, y = data$Mitglieder_23, paired = TRUE, alternative = "two.sided")

group_by(data_long, date) %>%
  summarise(
    count = n(),
    mean = mean(members, na.rm = TRUE),
    median = median(members, na.rm = TRUE),
    sd = sd(members, na.rm = TRUE)
  )


# Paired Samples Wilcoxon Test 
wilcox.test(x = data$Mitglieder_22, y = data$Mitglieder_23, paired = TRUE, alternative = "less")


################################################################################

## Vergleich der Cluster ##

data$`Modularity Class` <- as.factor(data$`Modularity Class`)
data$growth <- data$Mitglieder_23 - data$Mitglieder_22
summary(data)


kruskal <- kruskal.test(growth~`Modularity Class`, data = data)
kruskal

x11()  
ggplot(data, aes(x=`Modularity Class`, y=growth)) +     
  geom_point() +
  geom_boxplot()

dunnTest(growth~`Modularity Class`, data = data,
         method="bonferroni")

# Unterschiede zwischen 1 (Influencer Verschwörung) - 5 (Identitäre Bewegung)
summary(data%>% filter(`Modularity Class`==1))
summary(data%>% filter(`Modularity Class`==5))
