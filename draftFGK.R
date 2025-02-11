# Install the released version from CRAN:
#install.packages("agridat")

# Install the development version from GitHub:
install.packages("devtools")
devtools::install_github("kwstat/agridat")

library(agridat)
?agridat # list all datasets with keywords

data(durban.rowcol)
dat <- durban.rowcol

libs(desplot)
desplot(dat, yield~bed*row,
        out1=rep, num=gen, # aspect unknown
        main="durban.rowcol")


# Durban 2003 Figure 1
m10 <- lm(yield~gen, data=dat)
dat$resid <- m10$resid
## libs(lattice)
## xyplot(resid~row, dat, type=c('p','smooth'), main="durban.rowcol")
## xyplot(resid~bed, dat, type=c('p','smooth'), main="durban.rowcol")

# Figure 3
libs(lattice)
xyplot(resid ~ bed|factor(row), data=dat,
       main="durban.rowcol",
       type=c('p','smooth'))



# Figure 5 - field trend
# note, Durban used gam package like this
# m1lo <- gam(yield ~ gen + lo(row, span=10/16) + lo(bed, span=9/34), data=dat)
libs(mgcv)
m1lo <- gam(yield ~ gen + s(row) + s(bed, k=5), data=dat)
new1 <- expand.grid(row=unique(dat$row),bed=unique(dat$bed))
new1 <- cbind(new1, gen="G001")
p1lo <- predict(m1lo, newdata=new1)
libs(lattice)
wireframe(p1lo~row+bed, new1, aspect=c(1,.5), main="Field trend")


if(require("asreml", quietly=TRUE)) {
  libs(asreml)
  
  dat <- transform(dat, rowf=factor(row), bedf=factor(bed))
  dat <- dat[order(dat$rowf, dat$bedf),]
  
  m1a1 <- asreml(yield~gen + lin(rowf) + lin(bedf), data=dat,
                 random=~spl(rowf) + spl(bedf) + units,
                 family=asr_gaussian(dispersion=1))
  m1a2 <- asreml(yield~gen + lin(rowf) + lin(bedf), data=dat,
                 random=~spl(rowf) + spl(bedf) + units,
                 resid = ~ar1(rowf):ar1(bedf))
  m1a2 <- update(m1a2)
  m1a3 <- asreml(yield~gen, data=dat, random=~units,
                 resid = ~ar1(rowf):ar1(bedf))
  
  # Figure 7
  libs(lattice)
  v7a <- asr_varioGram(x=dat$bedf, y=dat$rowf, z=m1a3$residuals)
  wireframe(gamma ~ x*y, v7a, aspect=c(1,.5)) # Fig 7a
  
  v7b <- asr_varioGram(x=dat$bedf, y=dat$rowf, z=m1a2$residuals)
  wireframe(gamma ~ x*y, v7b, aspect=c(1,.5)) # Fig 7b
  
  v7c <- asr_varioGram(x=dat$bedf, y=dat$rowf, z=m1lo$residuals)
  wireframe(gamma ~ x*y, v7c, aspect=c(1,.5)) # Fig 7c
}




#Notes ggplot2

ggplot(, aes()) + 
  geom_point() + 
  geom_smooth() + 
  scale_colour_continuous() + 
  facet_grid(n ~s ) + 
  coord_fixed() + 
  theme()


# Install and load necessary packages
#install.packages("ggeffects")
library(ggeffects)
library(ggplot2)

# Fit a regression model
data(mtcars)
model <- lm(mpg ~ wt + hp, data = mtcars)

# Generate marginal effects
marginal_effects <- ggpredict(model, terms = c("wt", "hp"))

# Plot the marginal effects
plot(marginal_effects) +
  theme_minimal() +
  labs(title = "Marginal Effects of Weight and Horsepower on MPG",
       x = "Predictors",
       y = "Predicted MPG")
