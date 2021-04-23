# R code used to create figures in Data Analysis of Fisheries Science paper
# Packages required
```{r setup, results='hide', warning=FALSE, message=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning=FALSE)
library("tidyverse")
library("ggmap")
library("ggplot2")
library("knitr")
library("formatR")
library("tikzDevice")
library("Hmisc")
library("dplyr")
library(latticeExtra)
library("hrbrthemes")
library("quantreg")
opts_chunk$set(tidy.opts=list(width.cutoff=66),tidy=TRUE)
```

# Import data
```{r, warning=FALSE}
PredPrey <- read.csv("Predator_and_prey_body_sizes_in_marine_food_webs_vsn4.csv")
```

# Making sure data is accurate 
```{r}
PredPrey$Prey.mass[PredPrey$Prey.mass.unit == "mg"] <- (PredPrey$Prey.mass[PredPrey$Prey.mass.unit == "mg"])/1000
PredPrey$Prey.mass.unit[PredPrey$Prey.mass.unit == "mg"] <- "g"

PredPrey$Predator.mass[PredPrey$Predator.mass.unit == "mg"] <- (PredPrey$Predator.mass[PredPrey$Predator.mass.unit == "mg"])/1000
PredPrey$Predator.mass.unit[PredPrey$Predator.mass.unit == "mg"] <- "g"
```

# Relationship between predator and prey lengths


# Relationship between predator and prey masses 
## Figure 13
```{r}
ggplot(data = PredPrey, aes(x = Predator.mass, y = Prey.mass)) + geom_point(size = 0.2) + scale_x_log10() + scale_y_log10() + xlab("log(predator mass)") + ylab("log(prey mass)") + geom_smooth(colour = 'red', fill = 'blue')
```

## Figure 14
```{r}
PredPrey$logpredmass <- PredPrey$Predator.mass
PredPrey$logpreylength <- PredPrey$Prey.mass

PredPrey[,c(61, 62)] <- log10(PredPrey[,c(61, 62)])

PredPrey$Predator.mass.cat <- cut(PredPrey$logpredmass, breaks = 15)
ggplot(data=PredPrey) + geom_boxplot(mapping=aes(x=logpredmass, y=Predator.mass.cat)) + xlab("log(Prey Mass)") + ylab("Categories of log(Predator Mass)")
```

## Figure 15
I merged the below graphs together to make the figure shown in the project.
```{r}
PredPrey$massratio <- (PredPrey$Prey.mass)/(PredPrey$Predator.mass)

ggplot(data = PredPrey, aes(x=massratio)) + geom_histogram(aes(y=stat(count) / sum(count)), binwidth = 0.05, colour="black", fill="white") + scale_y_continuous(labels = scales::percent, limits=c(0,0.1)) + ylab("Relative Frequency (%)") + xlab("Prey Mass/Predator Mass") + xlim(0,3)

ggplot(PredPrey, aes(massratio)) + stat_ecdf(size = 1.25, color = "red") + scale_y_continuous(labels = scales::percent) + xlab("Prey Mass/Predator Mass") + ylab("Cumulative Frequency (%)") + xlim(0,3)
```

## Figure 16
```{r}
model.rq <- rq(massratio ~ Predator.mass, PredPrey, tau=c(0.1, 0.9))
quantile.regressions <- data.frame(t(coef(model.rq)))
colnames(quantile.regressions) <- c("intercept", "slope")
quantile.regressions$quantile <- rownames(quantile.regressions)
quantile.regressions

scatterplot <- qplot(x=Predator.mass, y=massratio, data=PredPrey, size=I(0.2))
scatterplot + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions, size=1.5) + xlab("Predator Mass (g)") + ylab("Prey Mass/Predator Mass")  + scale_x_log10() + ylim(0,3)
```

### Alternative code for Figure 16
```{r}
model.rq <- rq(massratio ~ Predator.mass, PredPrey, tau=c(0.1, 0.9))
quantile.regressions <- data.frame(t(coef(model.rq)))
colnames(quantile.regressions) <- c("intercept", "slope")
quantile.regressions$quantile <- rownames(quantile.regressions)
quantile.regressions

scatterplot <- qplot(x=Predator.mass, y=massratio, data=PredPrey, size=I(0.2))
scatterplot + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions, size=1.5) + xlab("Predator Mass (g)") + ylab("Prey Mass/Predator Mass")

model.rq1 <- rq(massratio ~ Predator.mass, PredPrey, tau=c(0.1))
quantile.regressions1 <- data.frame(t(coef(model.rq1)))
colnames(quantile.regressions1) <- c("intercept", "slope")
quantile.regressions1$quantile <- rownames(quantile.regressions1)
quantile.regressions1
ressq1 <- (model.rq1$residuals)^2
sum1 <- sum(ressq1)

model.rq2 <- rq(massratio ~ Predator.mass, PredPrey, tau=c(0.9))
quantile.regressions2 <- data.frame(t(coef(model.rq2)))
colnames(quantile.regressions2) <- c("intercept", "slope")
quantile.regressions2$quantile <- rownames(quantile.regressions2)
quantile.regressions2
ressq2 <- (model.rq2$residuals)^2
sum2 <- sum(ressq2) 
```

## Figure 18
```{r}
PredPrey5 <- PredPrey[c(4, 21, 28, 41, 64)]
PredPrey5 <- filter(PredPrey5, Predator == c("Lampanyctus crocodilus", "Merluccius bilinearis", "Thunnus alalunga ", "Gadus morhua", "Thunnus thynnus"))

ggplot(data = PredPrey5, aes(x=Predator.mass, y=massratio)) + geom_point(size=0.2) + ylab("Prey Mass/Predator Mass") + xlab("Predator Mass (g)") + geom_quantile(quantiles = c(0.1, 0.9), colour = "red", size = 1) + facet_wrap(~ Predator) + xlim(0,3000)
```
Decided this wasn't clear enough and I'd do the graphs for each species separately.

### Lampanyctus crocodilus
```{r, warning=FALSE}
lamp <- filter(PredPrey5, Predator == "Lampanyctus crocodilus")
model.rq.lamp <- rq(massratio ~ Predator.mass, lamp, tau=c(0.1, 0.9))
quantile.regressions.lamp <- data.frame(t(coef(model.rq.lamp)))
colnames(quantile.regressions.lamp) <- c("intercept", "slope")
quantile.regressions.lamp$quantile <- rownames(quantile.regressions.lamp)
quantile.regressions.lamp

scatterplot.lamp <- qplot(x=Predator.mass, y=massratio, data=lamp, size=I(0.2))
scatterplot.lamp + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions.lamp, size=1.5) + xlab("Predator Mass (g)") + ylab("Prey Mass/Predator Mass")

model.rq1.lamp <- rq(massratio ~ Predator.mass, lamp, tau=c(0.1))
quantile.regressions1.lamp <- data.frame(t(coef(model.rq1.lamp)))
colnames(quantile.regressions1.lamp) <- c("intercept", "slope")
quantile.regressions1.lamp$quantile <- rownames(quantile.regressions1.lamp)
quantile.regressions1.lamp
ressq1.lamp <- (model.rq1.lamp$residuals)^2
sum1.lamp <- sum(ressq1.lamp)

model.rq2.lamp <- rq(massratio ~ Predator.mass, lamp, tau=c(0.9))
quantile.regressions2.lamp <- data.frame(t(coef(model.rq2.lamp)))
colnames(quantile.regressions2.lamp) <- c("intercept", "slope")
quantile.regressions2.lamp$quantile <- rownames(quantile.regressions2.lamp)
quantile.regressions2.lamp
ressq2.lamp <- (model.rq2.lamp$residuals)^2
sum2.lamp <- sum(ressq2.lamp) 
```

### Merluccius bilinearis
```{r}
mer <- filter(PredPrey5, Predator == "Merluccius bilinearis")
model.rq.mer <- rq(massratio ~ Predator.mass, mer, tau=c(0.1, 0.9))
quantile.regressions.mer <- data.frame(t(coef(model.rq.mer)))
colnames(quantile.regressions.mer) <- c("intercept", "slope")
quantile.regressions.mer$quantile <- rownames(quantile.regressions.mer)
quantile.regressions.mer

scatterplot.mer <- qplot(x=Predator.mass, y=massratio, data=mer, size=I(0.2))
scatterplot.mer + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions.mer, size=1.5) + xlab("Predator Mass (g)") + ylab("Prey Mass/Predator Mass")

model.rq1.mer <- rq(massratio ~ Predator.mass, mer, tau=c(0.1))
quantile.regressions1.mer <- data.frame(t(coef(model.rq1.mer)))
colnames(quantile.regressions1.mer) <- c("intercept", "slope")
quantile.regressions1.mer$quantile <- rownames(quantile.regressions1.mer)
quantile.regressions1.mer
ressq1.mer <- (model.rq1.mer$residuals)^2
sum1.mer <- sum(ressq1.mer)

model.rq2.mer <- rq(massratio ~ Predator.mass, mer, tau=c(0.9))
quantile.regressions2.mer <- data.frame(t(coef(model.rq2.mer)))
colnames(quantile.regressions2.mer) <- c("intercept", "slope")
quantile.regressions2.mer$quantile <- rownames(quantile.regressions2.mer)
quantile.regressions2.mer
ressq2.mer <- (model.rq2.mer$residuals)^2
sum2.mer <- sum(ressq2.mer) 
```

### Thunnus alalunga
```{r}
al <- filter(PredPrey5, Predator == "Thunnus alalunga ")
model.rq.al <- rq(massratio ~ Predator.mass, al, tau=c(0.1, 0.9))
quantile.regressions.al <- data.frame(t(coef(model.rq.al)))
colnames(quantile.regressions.al) <- c("intercept", "slope")
quantile.regressions.al$quantile <- rownames(quantile.regressions.al)
quantile.regressions.al

scatterplot.al <- qplot(x=Predator.mass, y=massratio, data=al, size=I(0.2))
scatterplot.al + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions.al, size=1.5) + xlab("Predator Mass (g)") + ylab("Prey Mass/Predator Mass")

model.rq1.al <- rq(massratio ~ Predator.mass, al, tau=c(0.1))
quantile.regressions1.al <- data.frame(t(coef(model.rq1.al)))
colnames(quantile.regressions1.al) <- c("intercept", "slope")
quantile.regressions1.al$quantile <- rownames(quantile.regressions1.al)
quantile.regressions1.al
ressq1.al <- (model.rq1.al$residuals)^2
sum1.al <- sum(ressq1.al)

model.rq2.al <- rq(massratio ~ Predator.mass, al, tau=c(0.9))
quantile.regressions2.al <- data.frame(t(coef(model.rq2.al)))
colnames(quantile.regressions2.al) <- c("intercept", "slope")
quantile.regressions2.al$quantile <- rownames(quantile.regressions2.al)
quantile.regressions2.al
ressq2.al <- (model.rq2.al$residuals)^2
sum2.al <- sum(ressq2.al) 
```

### Gaddus morhua
```{r}
gad <- filter(PredPrey5, Predator == "Gadus morhua")
model.rq.gad <- rq(massratio ~ Predator.mass, gad, tau=c(0.1, 0.9))
quantile.regressions.gad <- data.frame(t(coef(model.rq.gad)))
colnames(quantile.regressions.gad) <- c("intercept", "slope")
quantile.regressions.gad$quantile <- rownames(quantile.regressions.gad)
quantile.regressions.gad

scatterplot.gad <- qplot(x=Predator.mass, y=massratio, data=gad, size=I(0.2))
scatterplot.gad + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions.gad, size=1.5) + xlab("Predator Mass (g)") + ylab("Prey Mass/Predator Mass")

model.rq1.gad <- rq(massratio ~ Predator.mass, gad, tau=c(0.1))
quantile.regressions1.gad <- data.frame(t(coef(model.rq1.gad)))
colnames(quantile.regressions1.gad) <- c("intercept", "slope")
quantile.regressions1.gad$quantile <- rownames(quantile.regressions1.gad)
quantile.regressions1.gad
ressq1.gad <- (model.rq1.gad$residuals)^2
sum1.gad <- sum(ressq1.gad)

model.rq2.gad <- rq(massratio ~ Predator.mass, gad, tau=c(0.9))
quantile.regressions2.gad <- data.frame(t(coef(model.rq2.gad)))
colnames(quantile.regressions2.gad) <- c("intercept", "slope")
quantile.regressions2.gad$quantile <- rownames(quantile.regressions2.gad)
quantile.regressions2.gad
ressq2.gad <- (model.rq2.gad$residuals)^2
sum2.gad <- sum(ressq2.gad) 
```

### Thunnus thynnus
```{r}
thy <- filter(PredPrey5, Predator == "Thunnus thynnus")
model.rq.thy <- rq(massratio ~ Predator.mass, thy, tau=c(0.1, 0.9))
quantile.regressions.thy <- data.frame(t(coef(model.rq.thy)))
colnames(quantile.regressions.thy) <- c("intercept", "slope")
quantile.regressions.thy$quantile <- rownames(quantile.regressions.thy)
quantile.regressions.thy

scatterplot.thy <- qplot(x=Predator.mass, y=massratio, data=thy, size=I(0.2))
scatterplot.thy + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions.thy, size=1.5) + xlab("Predator Mass (g)") + ylab("Prey Mass/Predator Mass")

model.rq1.thy <- rq(massratio ~ Predator.mass, thy, tau=c(0.1))
quantile.regressions1.thy <- data.frame(t(coef(model.rq1.thy)))
colnames(quantile.regressions1.thy) <- c("intercept", "slope")
quantile.regressions1.thy$quantile <- rownames(quantile.regressions1.thy)
quantile.regressions1.thy
ressq1.thy <- (model.rq1.thy$residuals)^2
sum1.thy <- sum(ressq1.thy)

model.rq2.thy <- rq(massratio ~ Predator.mass, thy, tau=c(0.9))
quantile.regressions2.thy <- data.frame(t(coef(model.rq2.thy)))
colnames(quantile.regressions2.thy) <- c("intercept", "slope")
quantile.regressions2.thy$quantile <- rownames(quantile.regressions2.thy)
quantile.regressions2.thy
ressq2.thy <- (model.rq2.thy$residuals)^2
sum2.thy <- sum(ressq2.thy) 
```
I used the tables showing the intercept, slope and quantile for each species to fill out the table shown in figure 17.


## Figure 20
I combined this graphs like I did for figure 15
```{r}
ggplot(data = PredPrey5, aes(x=massratio)) + geom_histogram(aes(y=stat(count) / sum(count)), binwidth = 0.05, colour="black", fill="white") + scale_y_continuous(labels = scales::percent) + ylab("Relative Frequency (%)") + xlab("Prey Mass/Predator Mass") + facet_wrap(~ Predator)
ggplot(PredPrey5, aes(massratio)) + stat_ecdf(size = 1.25, color = "red") + scale_y_continuous(labels = scales::percent) + xlab("Prey Mass/Predator Mass") + ylab("Cumulative Frequency (%)") + facet_wrap(~ Predator)
