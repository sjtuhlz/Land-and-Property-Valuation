
### RESIDENTIAL 

library(foreign)
library(np)

# Non-Parametric Project
rm(list=ls())
setwd("C:/Users/wenyun/Desktop/NP project")
residential <- read.dta("D:/summer, 2011/Third Project/Final_data/residential_six_final_3category_unix.dta")
single <- subset(residential, lu_08 >= 1110 & lu_08 < 1120)
single.bk <- single

# obtain a 1% sample 
single$x <- runif(length(single$sale_price))
single <- subset(single, x < 0.01)

attach(single)

single$totvalue07[totvalue07 == 0] <- NA
single$sale_price[sale_price == 0] <- NA
single$saleyr[saleyr == 0 | saleyr == 1899] <- NA
single$floor_area[floor_area  == 0] <- NA
# single$lgvsq  <- log(single$sale_price/ single$floor_area) 
single$lgp <- log(single$sale_price)
single$lgf <- log(single$floor_area)
single$lgl <- log(single$shape_area)
single$city.name <- single$city
single$city <- NULL
detach()





# problem dealing with missing data
ava.single <- subset(single, !is.na(lgp)  & !is.na(lgf)  & !is.na(lgl)  & !is.na(saleyr)) 
ava.single <- subset(ava.single, fwy != 0 & ocean != 0)
ava.single <- subset(ava.single, county != "la")

####### parametric version ##############

lreg <- lm(formula = lgp ~ lgf + lgl + fwy + ocean + fsub + cbd + factor(saleyr) + factor(city.name) + factor(lu_08), 
data = ava.single, na.action = na.omit)


# log of accessibilities
lreg.log <- lm(formula =lgp ~ lgf + lgl +  I(log(fwy)) + I(log(ocean)) + I(log(fsub)) + I(log(cbd)) + factor(saleyr) + factor(city.name) + factor(lu_08), 
data = ava.single, na.action = na.omit)

lreg.log <- lm(formula =lgp ~ lgf + lgl +  I(log(fwy)) + I(log(ocean)) + I(log(fsub)) + I(log(cbd)), 
data = ava.single, na.action = na.omit)


####### Non- parametric version ##############

ava.single$saleyr.f <- factor(saleyr)
ava.single$city.name.f <- factor(city.name)
ava.single$lu08.f <- factor(lu_08)

ava.single$lgfwy <- log(fwy)
ava.single$lgocean <- log(ocean)
ava.single$lgfsub <- log(fsub)
ava.single$lgcbd <- log(cbd)



### keep only saleyr dummies 

lreg.log <- lm(formula =lgp ~ lgf + lgl +  I(log(fwy)) + I(log(ocean)) + I(log(fsub)) + I(log(cbd)) + factor(saleyr), 
data = ava.single, na.action = na.omit)

# SP on each of the four accessibilities

bw.fwy <- npplregbw(formula = lgp ~ lgf + lgl + lgfsub + lgocean + lgcbd + saleyr.f | fwy, 
data = ava.single, na.action = na.omit, ftol = 0.01, tol = 0.01)
beta.fwy <- npplreg(bws = bw.fwy)


bw.fsub <- npplregbw(formula = lgp ~ lgf + lgl + lgfwy + lgocean + lgcbd + saleyr.f | fsub, 
data = ava.single, na.action = na.omit, ftol = 0.01, tol = 0.01)
beta.fsub <- npplreg(bws = bw.fsub)


bw.cbd <- npplregbw(formula = lgp ~ lgf + lgl + lgfwy + lgocean + lgfsub + saleyr.f | cbd, 
data = ava.single, na.action = na.omit, ftol = 0.01, tol = 0.01)
beta.cbd <- npplreg(bws = bw.cbd)

bw.ocean <- npplregbw(formula = lgp ~ lgf + lgl + lgfwy + lgcbd + lgfsub + saleyr.f | ocean, 
data = ava.single, na.action = na.omit, ftol = 0.01, tol = 0.01)
beta.ocean <- npplreg(bws = bw.ocean)


## plot the NP part of SP regression
x.fwy <- data.frame(lgf, lgl, lgfsub, lgocean, lgcbd, saleyr.f)
ava.single$y.fwy <- lgp - predict(beta.fwy, x.fwy, se.fit = FALSE)
bw2.fwy <- npregbw(formula = y.fwy ~ fwy, data = ava.single, na.action = na.omit, ftol = 0.01, tol = 0.01)
dev.new()
npplot(bw2.fwy, gradients = TRUE)

x.fsub <- data.frame(lgf, lgl, lgfwy, lgocean, lgcbd, saleyr.f)
ava.single$y.fsub <- lgp - predict(beta.fsub, x.fsub, se.fit = FALSE)
bw2.fsub <- npregbw(formula = y.fsub ~ fsub, data = ava.single, na.action = na.omit, ftol = 0.01, tol = 0.01)
dev.new()
npplot(bw2.fsub, gradients = TRUE)

x.cbd <- data.frame(lgf, lgl, lgfwy, lgocean, lgfsub, saleyr.f)
ava.single$y.cbd <- lgp - predict(beta.cbd, x.cbd, se.fit = FALSE)
bw2.cbd <- npregbw(formula = y.cbd ~ cbd, data = ava.single, na.action = na.omit, ftol = 0.01, tol = 0.01)
dev.new()
npplot(bw2.cbd, gradients = TRUE)

x.ocean <- data.frame(lgf, lgl, lgfwy, lgcbd, lgfsub, saleyr.f)
ava.single$y.ocean <- lgp - predict(beta.ocean, x.ocean, se.fit = FALSE)
dev.new()
bw2.ocean <- npregbw(formula = y.ocean ~ ocean, data = ava.single, na.action = na.omit, ftol = 0.01, tol = 0.01)
npplot(bw2.ocean, gradients = TRUE)

### plot fitted values
npplot(bw2.fwy, 
plot.errors.method="bootstrap", 
plot.errors.boot.num=300)

npplot(bw2.fsub, 
plot.errors.method="bootstrap", 
plot.errors.boot.num=300)

npplot(bw2.cbd, 
plot.errors.method="bootstrap", 
plot.errors.boot.num=300)

npplot(bw2.ocean, 
plot.errors.method="bootstrap", 
plot.errors.boot.num=300)


### plot gradients
dev.new()
npplot(bw2.fwy, 
plot.errors.method="bootstrap", 
plot.errors.boot.num=300,
gradients=TRUE)

dev.new()
npplot(bw2.fsub, 
plot.errors.method="bootstrap", 
plot.errors.boot.num=300,
gradients=TRUE)

dev.new()
npplot(bw2.cbd, 
plot.errors.method="bootstrap", 
plot.errors.boot.num=300,
gradients=TRUE)

dev.new()
npplot(bw2.ocean, 
plot.errors.method="bootstrap", 
plot.errors.boot.num=300,
gradients=TRUE)




# NP on all of the four accessibilities


bwnp.all <- npregbw(formula = lgpf ~  fsub + cbd + fwy + ocean,
data = ava.single, na.action = na.omit, ftol = 0.01, tol = 0.01)
betanp.all <- npreg(bws = bwnp.all)
npplot(bws = bwnp.all)
npplot(bws = bwnp.all, gradients = TRUE)



# NP on each of the four accessibilities

bwnp.fwy <- npregbw(formula = lgpf ~   fwy ,
data = ava.single, na.action = na.omit, ftol = 0.01, tol = 0.01)
npplot(bws = bwnp.fwy)

bwnp.ocean <- npregbw(formula = lgpf ~   ocean ,
data = ava.single, na.action = na.omit, ftol = 0.01, tol = 0.01)
npplot(bws = bwnp.ocean, gradients = TRUE)


bwnp.fsub <- npregbw(formula = lgpf ~  fsub ,
data = ava.single, na.action = na.omit, ftol = 0.01, tol = 0.01)
npplot(bws = bwnp.fsub, gradients = TRUE)


bwnp.cbd <- npregbw(formula = lgpf ~  cbd ,
data = ava.single, na.action = na.omit, ftol = 0.01, tol = 0.01)
npplot(bws = bwnp.cbd, gradients = TRUE)


