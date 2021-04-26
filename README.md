# R code used to create figures in Data Analysis of Fisheries Science paper

# Packages required
```{r setup, results='hide', warning=FALSE, message=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning=FALSE)
library("olsrr")
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



# Preparing the Data
## Figure 1
```{r}
PredPrey <- PredPrey[-c(31711, 31717), ]
PredPrey$logpredlength <- PredPrey$Predator.length
PredPrey$logpreylength <- PredPrey$Prey.length
PredPrey[,c(61, 62)] <- log10(PredPrey[,c(61, 62)])

ggplot(data = PredPrey, aes(x = logpredlength, y = logpreylength)) + geom_point(size = 0.2) + xlab("log(Predator Length (mm))") + ylab("log(Prey Length (mm))") + geom_smooth(colour = 'red', fill = 'blue')
```

## Making sure data is accurate 
```{r}
PredPrey$Prey.mass[PredPrey$Prey.mass.unit == "mg"] <- (PredPrey$Prey.mass[PredPrey$Prey.mass.unit == "mg"])/1000
PredPrey$Prey.mass.unit[PredPrey$Prey.mass.unit == "mg"] <- "g"

PredPrey$Predator.mass[PredPrey$Predator.mass.unit == "mg"] <- (PredPrey$Predator.mass[PredPrey$Predator.mass.unit == "mg"])/1000
PredPrey$Predator.mass.unit[PredPrey$Predator.mass.unit == "mg"] <- "g"

PredPrey$Prey.length[PredPrey$Prey.length.unit == "Âµm"] <- (PredPrey$Prey.length[PredPrey$Prey.length.unit == "Âµm"])/1000
PredPrey$Prey.length.unit[PredPrey$Prey.length.unit == "Âµm"] <- "mm"

PredPrey$Prey.length[PredPrey$Prey.length.unit == "cm"] <- (PredPrey$Prey.length[PredPrey$Prey.length.unit == "cm"])*10
PredPrey$Prey.length.unit[PredPrey$Prey.length.unit == "cm"] <- "mm"

PredPrey$Predator.length[PredPrey$Predator.length.unit == "Âµm"] <- (PredPrey$Predator.length[PredPrey$Predator.length.unit == "Âµm"])/1000
PredPrey$Predator.length.unit[PredPrey$Predator.length.unit == "Âµm"] <- "mm"

PredPrey$Predator.length[PredPrey$Predator.length.unit == "cm"] <- (PredPrey$Predator.length[PredPrey$Predator.length.unit == "cm"])*10
PredPrey$Predator.length.unit[PredPrey$Predator.length.unit == "cm"] <- "mm"
```

## Figure 2
```{r}
PredPrey$logpredlength <- PredPrey$Predator.length
PredPrey$logpreylength <- PredPrey$Prey.length
PredPrey[,c(61, 62)] <- log10(PredPrey[,c(61, 62)])

ggplot(data = PredPrey, aes(x = logpredlength, y = logpreylength)) + geom_point(size = 0.2) + xlab("log(Predator Length (mm))") + ylab("log(Prey Length (mm))") + geom_smooth(colour = 'red', fill = 'blue')
```

## Figure 3
```{r}
PredPrey$logpredmass <- PredPrey$Predator.mass
PredPrey$logpreymass <- PredPrey$Prey.mass
PredPrey[,c(63, 64)] <- log10(PredPrey[,c(63, 64)])

ggplot(data = PredPrey, aes(x = logpredmass, y = logpreymass)) + geom_point(size = 0.2) + xlab("log(Predator Mass (g))") + ylab("log(Prey Mass (g))") + geom_smooth(colour = 'red', fill = 'blue')
```

#Results
## Lengths of Marine Animals
### Figure 4
```{r}
PredPrey$Predator.length.cat <- cut(PredPrey$logpredlength, breaks = 15)
ggplot(data=PredPrey) + geom_boxplot(mapping=aes(x=logpreylength, y=Predator.length.cat)) + xlab("log(Prey Length (mm))") + ylab("Categories of log(Predator Length (mm))") 
```

### Figure 5
I merged the below graphs together to make the figure shown in the project.
```{r}
PredPrey$ratio <- (PredPrey$Prey.length)/(PredPrey$Predator.length)
PredPrey$logratio <- log10(PredPrey$ratio)
ggplot(data = PredPrey, aes(x=logratio)) + geom_histogram(aes(y=stat(count) / sum(count)), binwidth = 0.05, colour="black", fill="white") + scale_y_continuous(labels = scales::percent) + ylab("Relative Frequency (%)") + xlab("log(Prey Length/Predator Length)")
ggplot(PredPrey, aes(logratio)) + stat_ecdf(size = 1.25, color = "red") + scale_y_continuous(labels = scales::percent) + xlab("log(Prey Length/Predator Length)") + ylab("Cumulative Frequency (%)")
```

### Figure 6
```{r}
model.rq <- rq(logratio ~ logpredlength, PredPrey, tau=c(0.1, 0.9))
quantile.regressions <- data.frame(t(coef(model.rq)))
colnames(quantile.regressions) <- c("intercept", "slope")
quantile.regressions$quantile <- rownames(quantile.regressions)
quantile.regressions

scatterplot <- qplot(x=logpredlength, y=logratio, data=PredPrey, size=I(0.2))
scatterplot + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions, size=1.5) + xlab("log(Predator Length (mm))") + ylab("log(Prey Length/Predator Length)")
```
### Figure 7
```{r}
model <- lm(logratio ~ logpredlength, data = PredPrey)
ols_plot_resid_qq(model)

model.res = resid(model)
scatterplot <- qplot(x=PredPrey$logpredlength, y=model.res,  size=I(0.2))
scatterplot + xlab("log(Predator Length (mm))") + ylab("Residuals") + geom_hline(yintercept=0, color="red", size=1.2)
```

### Figure 11
```{r}
PredPrey4 <- PredPrey[c(4, 9, 28, 31, 64)]
PredPrey4 <- filter(PredPrey4, Predator == c("Lampanyctus crocodilus", "Merluccius bilinearis", "Thunnus alalunga ", "Gadus morhua", "Thunnus thynnus"))
PredPrey4$logpredlength <- PredPrey4$Predator.length
PredPrey4$logpreylength <- PredPrey4$Prey.length
PredPrey4[,c(6, 7)] <- log10(PredPrey4[,c(6, 7)])

PredPrey4$ratio <- (PredPrey4$Prey.length)/(PredPrey4$Predator.length)
PredPrey4$logratio <- log10(PredPrey4$ratio)

ggplot(data = PredPrey4, aes(x=logpredlength, y=logratio)) + geom_point(size=0.2) + ylab("log(Prey Length/Predator Length)") + xlab("log(Predator Length (mm))") + geom_quantile(quantiles = c(0.1, 0.9), colour = "red", size = 1) + facet_wrap(~ Predator)
```
Decided this wasn't clear enough and I'd do the graphs for each species separately.

#### Lampanyctus crocodilus
```{r}
lamp <- filter(PredPrey4, Predator == "Lampanyctus crocodilus")
model.rq.lamp <- rq(logratio ~ logpredlength, lamp, tau=c(0.1, 0.9))
quantile.regressions.lamp <- data.frame(t(coef(model.rq.lamp)))
colnames(quantile.regressions.lamp) <- c("intercept", "slope")
quantile.regressions.lamp$quantile <- rownames(quantile.regressions.lamp)
quantile.regressions.lamp

scatterplot.lamp <- qplot(x=logpredlength, y=logratio, data=lamp, size=I(0.2))
scatterplot.lamp + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions.lamp, size=1.5) + xlab("log(Predator Length (mm))") + ylab("log(Prey Length/Predator Length)")

model.rq1.lamp <- rq(logratio ~ logpredlength, lamp, tau=c(0.1))
quantile.regressions1.lamp <- data.frame(t(coef(model.rq1.lamp)))
colnames(quantile.regressions1.lamp) <- c("intercept", "slope")
quantile.regressions1.lamp$quantile <- rownames(quantile.regressions1.lamp)
quantile.regressions1.lamp
ressq1.lamp <- (model.rq1.lamp$residuals)^2
sum1.lamp <- sum(ressq1.lamp)

model.rq2.lamp <- rq(logratio ~ logpredlength, lamp, tau=c(0.9))
quantile.regressions2.lamp <- data.frame(t(coef(model.rq2.lamp)))
colnames(quantile.regressions2.lamp) <- c("intercept", "slope")
quantile.regressions2.lamp$quantile <- rownames(quantile.regressions2.lamp)
quantile.regressions2.lamp
ressq2.lamp <- (model.rq2.lamp$residuals)^2
sum2.lamp <- sum(ressq2.lamp) 
```

#### Merluccius Bilinearis
```{r}
mer <- filter(PredPrey4, Predator == "Merluccius bilinearis")
model.rq.mer <- rq(logratio ~ logpredlength, mer, tau=c(0.1, 0.9))
quantile.regressions.mer <- data.frame(t(coef(model.rq.mer)))
colnames(quantile.regressions.mer) <- c("intercept", "slope")
quantile.regressions.mer$quantile <- rownames(quantile.regressions.mer)
quantile.regressions.mer

scatterplot.mer <- qplot(x=logpredlength, y=logratio, data=mer, size=I(0.2))
scatterplot.mer + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions.mer, size=1.5) + xlab("log(Predator Length (mm))") + ylab("log(Prey Length/Predator Length)")

model.rq1.mer <- rq(logratio ~ logpredlength, mer, tau=c(0.1))
quantile.regressions1.mer <- data.frame(t(coef(model.rq1.mer)))
colnames(quantile.regressions1.mer) <- c("intercept", "slope")
quantile.regressions1.mer$quantile <- rownames(quantile.regressions1.mer)
quantile.regressions1.mer
ressq1.mer <- (model.rq1.mer$residuals)^2
sum1.mer <- sum(ressq1.mer)

model.rq2.mer <- rq(logratio ~ logpredlength, mer, tau=c(0.9))
quantile.regressions2.mer <- data.frame(t(coef(model.rq2.mer)))
colnames(quantile.regressions2.mer) <- c("intercept", "slope")
quantile.regressions2.mer$quantile <- rownames(quantile.regressions2.mer)
quantile.regressions2.mer
ressq2.mer <- (model.rq2.mer$residuals)^2
sum2.mer <- sum(ressq2.mer) 
```

#### Thunnus alalunga
```{r}
al <- filter(PredPrey4, Predator == "Thunnus alalunga ")
model.rq.al <- rq(logratio ~ logpredlength, al, tau=c(0.1, 0.9))
quantile.regressions.al <- data.frame(t(coef(model.rq.al)))
colnames(quantile.regressions.al) <- c("intercept", "slope")
quantile.regressions.al$quantile <- rownames(quantile.regressions.al)
quantile.regressions.al

scatterplot.al <- qplot(x=logpredlength, y=logratio, data=al, size=I(0.2))
scatterplot.al + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions.al, size=1.5) + xlab("log(Predator Length (mm))") + ylab("log(Prey Length/Predator Length)")

model.rq1.al <- rq(logratio ~ logpredlength, al, tau=c(0.1))
quantile.regressions1.al <- data.frame(t(coef(model.rq1.al)))
colnames(quantile.regressions1.al) <- c("intercept", "slope")
quantile.regressions1.al$quantile <- rownames(quantile.regressions1.al)
quantile.regressions1.al
ressq1.al <- (model.rq1.al$residuals)^2
sum1.al <- sum(ressq1.al)

model.rq2.al <- rq(logratio ~ logpredlength, al, tau=c(0.9))
quantile.regressions2.al <- data.frame(t(coef(model.rq2.al)))
colnames(quantile.regressions2.al) <- c("intercept", "slope")
quantile.regressions2.al$quantile <- rownames(quantile.regressions2.al)
quantile.regressions2.al
ressq2.al <- (model.rq2.al$residuals)^2
sum2.al <- sum(ressq2.al) 
```

#### Gadus morhua
```{r}
gad <- filter(PredPrey4, Predator == "Gadus morhua")
model.rq.gad <- rq(logratio ~ logpredlength, gad, tau=c(0.1, 0.9))
quantile.regressions.gad <- data.frame(t(coef(model.rq.gad)))
colnames(quantile.regressions.gad) <- c("intercept", "slope")
quantile.regressions.gad$quantile <- rownames(quantile.regressions.gad)
quantile.regressions.gad

scatterplot.gad <- qplot(x=logpredlength, y=logratio, data=gad, size=I(0.2))
scatterplot.gad + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions.gad, size=1.5) + xlab("log(Predator Length (mm))") + ylab("log(Prey Length/Predator Length)")

model.rq1.gad <- rq(logratio ~ logpredlength, gad, tau=c(0.1))
quantile.regressions1.gad <- data.frame(t(coef(model.rq1.gad)))
colnames(quantile.regressions1.gad) <- c("intercept", "slope")
quantile.regressions1.gad$quantile <- rownames(quantile.regressions1.gad)
quantile.regressions1.gad
ressq1.gad <- (model.rq1.gad$residuals)^2
sum1.gad <- sum(ressq1.gad)

model.rq2.gad <- rq(logratio ~ logpredlength, gad, tau=c(0.9))
quantile.regressions2.gad <- data.frame(t(coef(model.rq2.gad)))
colnames(quantile.regressions2.gad) <- c("intercept", "slope")
quantile.regressions2.gad$quantile <- rownames(quantile.regressions2.gad)
quantile.regressions2.gad
ressq2.gad <- (model.rq2.gad$residuals)^2
sum2.gad <- sum(ressq2.gad) 
```

#### Thunnus thynnus
```{r}
thy <- filter(PredPrey4, Predator == "Thunnus thynnus")
model.rq.thy <- rq(logratio ~ logpredlength, thy, tau=c(0.1, 0.9))
quantile.regressions.thy <- data.frame(t(coef(model.rq.thy)))
colnames(quantile.regressions.thy) <- c("intercept", "slope")
quantile.regressions.thy$quantile <- rownames(quantile.regressions.thy)
quantile.regressions.thy

scatterplot.thy <- qplot(x=logpredlength, y=logratio, data=thy, size=I(0.2))
scatterplot.thy + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions.thy, size=1.5) + xlab("log(Predator Length (mm))") + ylab("log(Prey Length/Predator Length)")

model.rq1.thy <- rq(logratio ~ logpredlength, thy, tau=c(0.1))
quantile.regressions1.thy <- data.frame(t(coef(model.rq1.thy)))
colnames(quantile.regressions1.thy) <- c("intercept", "slope")
quantile.regressions1.thy$quantile <- rownames(quantile.regressions1.thy)
quantile.regressions1.thy
ressq1.thy <- (model.rq1.thy$residuals)^2
sum1.thy <- sum(ressq1.thy)

model.rq2.thy <- rq(logratio ~ logpredlength, thy, tau=c(0.9))
quantile.regressions2.thy <- data.frame(t(coef(model.rq2.thy)))
colnames(quantile.regressions2.thy) <- c("intercept", "slope")
quantile.regressions2.thy$quantile <- rownames(quantile.regressions2.thy)
quantile.regressions2.thy
ressq2.thy <- (model.rq2.thy$residuals)^2
sum2.thy <- sum(ressq2.thy) 
```
I used the tables showing the intercept, slope and quantile for each species to fill out the table shown in figure 9.

### Figure 13
I combined this graphs like I did for figure 15
```{r}
ggplot(data = PredPrey4, aes(x=logratio)) + geom_histogram(aes(y=stat(count) / sum(count)), binwidth = 0.05, colour="black", fill="white") + scale_y_continuous(labels = scales::percent) + ylab("Relative Frequency (%)") + xlab("log(Prey Length/Predator Length)") + facet_wrap(~ Predator)
ggplot(PredPrey4, aes(logratio)) + stat_ecdf(size = 1.25, color = "red") + scale_y_continuous(labels = scales::percent) + xlab("log(Prey Length/Predator Length)") + ylab("Cumulative Frequency (%)") + facet_wrap(~ Predator)
```


## Masses of Marine Animals
### Figure 14
```{r}
PredPrey$Predator.mass.cat <- cut(PredPrey$logpredmass, breaks = 15)
ggplot(data=PredPrey) + geom_boxplot(mapping=aes(x=logpredmass, y=Predator.mass.cat)) + xlab("log(Prey Mass (g))") + ylab("Categories of log(Predator Mass (g))")
```

### Figure 15
I merged the below graphs together to make the figure shown in the project.
```{r}
PredPrey$massratio <- (PredPrey$Prey.mass)/(PredPrey$Predator.mass)
PredPrey$logmassratio <- log10(PredPrey$massratio)

ggplot(data = PredPrey, aes(x=logmassratio)) + geom_histogram(aes(y=stat(count) / sum(count)), binwidth = 0.2, colour="black", fill="white") + scale_y_continuous(labels = scales::percent) + ylab("Relative Frequency (%)") + xlab("log(Prey Mass/Predator Mass)")

ggplot(PredPrey, aes(logmassratio)) + stat_ecdf(size = 1.25, color = "red") + scale_y_continuous(labels = scales::percent) + xlab("Prey Mass/Predator Mass") + ylab("Cumulative Frequency (%)")
```
### Figure 16
```{r}
model.rq <- rq(logmassratio ~ logpredmass, PredPrey, tau=c(0.1, 0.9))
quantile.regressions <- data.frame(t(coef(model.rq)))
colnames(quantile.regressions) <- c("intercept", "slope")
quantile.regressions$quantile <- rownames(quantile.regressions)
quantile.regressions

scatterplot <- qplot(x=logpredmass, y=logmassratio, data=PredPrey, size=I(0.2))
scatterplot + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions, size=1.5) + xlab("log(Predator Mass (g))") + ylab("log(Prey Mass/Predator Mass)")
```

#### Alternative code for Figure 16
```{r}
model.rq <- rq(logmassratio ~ logpredmass, PredPrey, tau=c(0.1, 0.9))
quantile.regressions <- data.frame(t(coef(model.rq)))
colnames(quantile.regressions) <- c("intercept", "slope")
quantile.regressions$quantile <- rownames(quantile.regressions)
quantile.regressions

scatterplot <- qplot(x=logpredmass, y=logmassratio, data=PredPrey, size=I(0.2))
scatterplot + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions, size=1.5) + xlab("log(Predator Mass (g))") + ylab("log(Prey Mass/Predator Mass)")

model.rq1 <- rq(logmassratio ~ logpredmass, PredPrey, tau=c(0.1))
quantile.regressions1 <- data.frame(t(coef(model.rq1)))
colnames(quantile.regressions1) <- c("intercept", "slope")
quantile.regressions1$quantile <- rownames(quantile.regressions1)
quantile.regressions1
ressq1 <- (model.rq1$residuals)^2
sum1 <- sum(ressq1)

model.rq2 <- rq(logmassratio ~ logpredmass, PredPrey, tau=c(0.9))
quantile.regressions2 <- data.frame(t(coef(model.rq2)))
colnames(quantile.regressions2) <- c("intercept", "slope")
quantile.regressions2$quantile <- rownames(quantile.regressions2)
quantile.regressions2
ressq2 <- (model.rq2$residuals)^2
sum2 <- sum(ressq2) 
```

### Figure 17
```{r}
model2 <- lm(logmassratio ~ logpredmass, data = PredPrey)
ols_plot_resid_qq(model2)

model2.res = resid(model2)
scatterplot <- qplot(x=PredPrey$logpredmass, y=model2.res,  size=I(0.2))
scatterplot + xlab("log(Predator Mass (g))") + ylab("Residuals") + geom_hline(yintercept=0, color="red", size=1.2)
```


### Figure 19
```{r}
PredPrey5 <- PredPrey[c(4, 21, 28, 41, 63, 64)]
PredPrey5 <- filter(PredPrey5, Predator == c("Lampanyctus crocodilus", "Merluccius bilinearis", "Thunnus alalunga ", "Gadus morhua", "Thunnus thynnus"))

PredPrey5$massratio <- (PredPrey5$Prey.mass)/(PredPrey5$Predator.mass)
PredPrey5$logmassratio <- log10(PredPrey5$massratio)

ggplot(data = PredPrey5, aes(x=logpredmass, y=logmassratio)) + geom_point(size=0.2) + ylab("log(Prey Mass/Predator Mass)") + xlab("log(Predator Mass (g))") + geom_quantile(quantiles = c(0.1, 0.9), colour = "red", size = 1) + facet_wrap(~ Predator)
```
Decided this wasn't clear enough and I'd do the graphs for each species separately.

#### Lampanyctus crocodilus
```{r, warning=FALSE}
lamp <- filter(PredPrey5, Predator == "Lampanyctus crocodilus")
model.rq.lamp <- rq(logmassratio ~ logpredmass, lamp, tau=c(0.1, 0.9))
quantile.regressions.lamp <- data.frame(t(coef(model.rq.lamp)))
colnames(quantile.regressions.lamp) <- c("intercept", "slope")
quantile.regressions.lamp$quantile <- rownames(quantile.regressions.lamp)
quantile.regressions.lamp

scatterplot.lamp <- qplot(x=logpredmass, y=logmassratio, data=lamp, size=I(0.2))
scatterplot.lamp + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions.lamp, size=1.5) + xlab("log(Predator Mass (g))") + ylab("log(Prey Mass/Predator Mass)")

model.rq1.lamp <- rq(logmassratio ~ logpredmass, lamp, tau=c(0.1))
quantile.regressions1.lamp <- data.frame(t(coef(model.rq1.lamp)))
colnames(quantile.regressions1.lamp) <- c("intercept", "slope")
quantile.regressions1.lamp$quantile <- rownames(quantile.regressions1.lamp)
quantile.regressions1.lamp
ressq1.lamp <- (model.rq1.lamp$residuals)^2
sum1.lamp <- sum(ressq1.lamp)

model.rq2.lamp <- rq(logmassratio ~ logpredmass, lamp, tau=c(0.9))
quantile.regressions2.lamp <- data.frame(t(coef(model.rq2.lamp)))
colnames(quantile.regressions2.lamp) <- c("intercept", "slope")
quantile.regressions2.lamp$quantile <- rownames(quantile.regressions2.lamp)
quantile.regressions2.lamp
ressq2.lamp <- (model.rq2.lamp$residuals)^2
sum2.lamp <- sum(ressq2.lamp) 
```

#### Merluccius bilinearis
```{r}
mer <- filter(PredPrey5, Predator == "Merluccius bilinearis")
model.rq.mer <- rq(logmassratio ~ logpredmass, mer, tau=c(0.1, 0.9))
quantile.regressions.mer <- data.frame(t(coef(model.rq.mer)))
colnames(quantile.regressions.mer) <- c("intercept", "slope")
quantile.regressions.mer$quantile <- rownames(quantile.regressions.mer)
quantile.regressions.mer

scatterplot.mer <- qplot(x=logpredmass, y=logmassratio, data=mer, size=I(0.2))
scatterplot.mer + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions.mer, size=1.5) + xlab("log(Predator Mass (g))") + ylab("log(Prey Mass/Predator Mass)")

model.rq1.mer <- rq(logmassratio ~ logpredmass, mer, tau=c(0.1))
quantile.regressions1.mer <- data.frame(t(coef(model.rq1.mer)))
colnames(quantile.regressions1.mer) <- c("intercept", "slope")
quantile.regressions1.mer$quantile <- rownames(quantile.regressions1.mer)
quantile.regressions1.mer
ressq1.mer <- (model.rq1.mer$residuals)^2
sum1.mer <- sum(ressq1.mer)

model.rq2.mer <- rq(logmassratio ~ logpredmass, mer, tau=c(0.9))
quantile.regressions2.mer <- data.frame(t(coef(model.rq2.mer)))
colnames(quantile.regressions2.mer) <- c("intercept", "slope")
quantile.regressions2.mer$quantile <- rownames(quantile.regressions2.mer)
quantile.regressions2.mer
ressq2.mer <- (model.rq2.mer$residuals)^2
sum2.mer <- sum(ressq2.mer) 
```

#### Thunnus alalunga
```{r}
al <- filter(PredPrey5, Predator == "Thunnus alalunga ")
model.rq.al <- rq(logmassratio ~ logpredmass, al, tau=c(0.1, 0.9))
quantile.regressions.al <- data.frame(t(coef(model.rq.al)))
colnames(quantile.regressions.al) <- c("intercept", "slope")
quantile.regressions.al$quantile <- rownames(quantile.regressions.al)
quantile.regressions.al

scatterplot.al <- qplot(x=logpredmass, y=logmassratio, data=al, size=I(0.2))
scatterplot.al + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions.al, size=1.5) + xlab("log(Predator Mass (g))") + ylab("log(Prey Mass/Predator Mass)")

model.rq1.al <- rq(logmassratio ~ logpredmass, al, tau=c(0.1))
quantile.regressions1.al <- data.frame(t(coef(model.rq1.al)))
colnames(quantile.regressions1.al) <- c("intercept", "slope")
quantile.regressions1.al$quantile <- rownames(quantile.regressions1.al)
quantile.regressions1.al
ressq1.al <- (model.rq1.al$residuals)^2
sum1.al <- sum(ressq1.al)

model.rq2.al <- rq(logmassratio ~ logpredmass, al, tau=c(0.9))
quantile.regressions2.al <- data.frame(t(coef(model.rq2.al)))
colnames(quantile.regressions2.al) <- c("intercept", "slope")
quantile.regressions2.al$quantile <- rownames(quantile.regressions2.al)
quantile.regressions2.al
ressq2.al <- (model.rq2.al$residuals)^2
sum2.al <- sum(ressq2.al) 
```

#### Gaddus morhua
```{r}
gad <- filter(PredPrey5, Predator == "Gadus morhua")
model.rq.gad <- rq(logmassratio ~ logpredmass, gad, tau=c(0.1, 0.9))
quantile.regressions.gad <- data.frame(t(coef(model.rq.gad)))
colnames(quantile.regressions.gad) <- c("intercept", "slope")
quantile.regressions.gad$quantile <- rownames(quantile.regressions.gad)
quantile.regressions.gad

scatterplot.gad <- qplot(x=logpredmass, y=logmassratio, data=gad, size=I(0.2))
scatterplot.gad + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions.gad, size=1.5) + xlab("log(Predator Mass (g))") + ylab("log(Prey Mass/Predator Mass)")

model.rq1.gad <- rq(logmassratio ~ logpredmass, gad, tau=c(0.1))
quantile.regressions1.gad <- data.frame(t(coef(model.rq1.gad)))
colnames(quantile.regressions1.gad) <- c("intercept", "slope")
quantile.regressions1.gad$quantile <- rownames(quantile.regressions1.gad)
quantile.regressions1.gad
ressq1.gad <- (model.rq1.gad$residuals)^2
sum1.gad <- sum(ressq1.gad)

model.rq2.gad <- rq(logmassratio ~ logpredmass, gad, tau=c(0.9))
quantile.regressions2.gad <- data.frame(t(coef(model.rq2.gad)))
colnames(quantile.regressions2.gad) <- c("intercept", "slope")
quantile.regressions2.gad$quantile <- rownames(quantile.regressions2.gad)
quantile.regressions2.gad
ressq2.gad <- (model.rq2.gad$residuals)^2
sum2.gad <- sum(ressq2.gad) 
```

#### Thunnus thynnus
```{r}
thy <- filter(PredPrey5, Predator == "Thunnus thynnus")
model.rq.thy <- rq(logmassratio ~ logpredmass, thy, tau=c(0.1, 0.9))
quantile.regressions.thy <- data.frame(t(coef(model.rq.thy)))
colnames(quantile.regressions.thy) <- c("intercept", "slope")
quantile.regressions.thy$quantile <- rownames(quantile.regressions.thy)
quantile.regressions.thy

scatterplot.thy <- qplot(x=logpredmass, y=logmassratio, data=thy, size=I(0.2))
scatterplot.thy + geom_abline(aes(intercept=intercept, slope=slope,
  colour=quantile), data=quantile.regressions.thy, size=1.5) + xlab("log(Predator Mass (g))") + ylab("log(Prey Mass/Predator Mass)")

model.rq1.thy <- rq(logmassratio ~ logpredmass, thy, tau=c(0.1))
quantile.regressions1.thy <- data.frame(t(coef(model.rq1.thy)))
colnames(quantile.regressions1.thy) <- c("intercept", "slope")
quantile.regressions1.thy$quantile <- rownames(quantile.regressions1.thy)
quantile.regressions1.thy
ressq1.thy <- (model.rq1.thy$residuals)^2
sum1.thy <- sum(ressq1.thy)

model.rq2.thy <- rq(logmassratio ~ logpredmass, thy, tau=c(0.9))
quantile.regressions2.thy <- data.frame(t(coef(model.rq2.thy)))
colnames(quantile.regressions2.thy) <- c("intercept", "slope")
quantile.regressions2.thy$quantile <- rownames(quantile.regressions2.thy)
quantile.regressions2.thy
ressq2.thy <- (model.rq2.thy$residuals)^2
sum2.thy <- sum(ressq2.thy) 
```
I used the tables showing the intercept, slope and quantile for each species to fill out the table shown in figure 17.


### Figure 21
I combined this graphs like I did for figure 15
```{r}
ggplot(data = PredPrey5, aes(x=logmassratio)) + geom_histogram(aes(y=stat(count) / sum(count)), binwidth = 0.2, colour="black", fill="white") + scale_y_continuous(labels = scales::percent) + ylab("Relative Frequency (%)") + xlab("log(Prey Mass/Predator Mass)") + facet_wrap(~ Predator)
ggplot(PredPrey5, aes(logmassratio)) + stat_ecdf(size = 1.25, color = "red") + scale_y_continuous(labels = scales::percent) + xlab("log(Prey Mass/Predator Mass)") + ylab("Cumulative Frequency (%)") + facet_wrap(~ Predator)
```
