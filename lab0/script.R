# Lab 01

library(tidyverse)
library(maps)

#load data
data("USArrests")
stateCoord <- read.table("~/git/STAT-215A-Fall-2017/week1/data/stateCoord.txt", header=TRUE)

#merge datasets
merged <- merge(USArrests, stateCoord, by=0)

#plot murder vs. assault
ggplot(merged) +
  geom_point(aes(x=Murder, y=Assault))

#plot rape vs urban pop, mark outlier
ggplot(merged) +
  geom_point(aes(x=UrbanPop, y=Rape, color=(Rape>4*IQR(Rape) & UrbanPop < 4*IQR(UrbanPop))))+
  guides(color=FALSE)

#plot where each point is a state: rape vs. assault
ggplot(merged) +
  geom_text(aes(x=Murder, y=Assault, label=State))

# rape vs urban pop: Alaska is scary
ggplot(merged) +
  geom_text(aes(x=UrbanPop, y=Rape, label=State, color=(Rape>4*IQR(Rape) & UrbanPop < 4*IQR(UrbanPop))))+
  guides(color=FALSE)

# map of US and murder rate
states <- map_data("state")
merged["State"][,] <- tolower(merged["State"][,])
states2 <- full_join(states, merged, by=c("region"="State"))

ggplot(states2) +
  geom_polygon(aes(x=long.x, y=lat, fill=Murder/UrbanPop, group=group), color="white")


#linear regression: urban pop vs. rape; residual plot
linearMod <- lm(Rape~UrbanPop, merged)
ggplot() + 
  geom_point(aes(x=linearMod$fitted.values,y=linearMod$residuals))

#plot regression line on top of urbanpop vs rape
ggplot(merged, aes(x=UrbanPop, y=Rape)) +
  geom_point() +
  geom_abline(intercept=linearMod$coefficients[1], slope=linearMod$coefficients[2], color="blue")


#refit without outlier
filtered = filter(merged, Rape<4*IQR(Rape) | UrbanPop>4*IQR(UrbanPop))
linearMod2 <- lm(Rape~UrbanPop, filtered)


getRegLine <- function(x,m,b){
  sapply(x, function(x) m*x + b)
}

getRegLine(merged$UrbanPop, linearMod$coefficients[1], linearMod$coefficients[2])

ggplot(merged) +
  geom_point(aes(x=UrbanPop, y=Rape)) +
  geom_abline(intercept=linearMod$coefficients[1], slope=linearMod$coefficients[2], color="blue") +
  geom_abline(intercept=linearMod2$coefficients[1], slope=linearMod2$coefficients[2], color="red") +
  labs(x='Percent urban population',y='Rape arrests per 100,000',title="Relationship between urban population percent and rape arrests")+
  scale_color_manual()
  
