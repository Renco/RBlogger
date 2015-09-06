#http://www.magesblog.com/2015/08/generalised-linear-models-in-r.html


icecream <- data.frame(
  temp=c(11.9, 14.2, 15.2, 16.4, 17.2, 18.1, 
         18.5, 19.4, 22.1, 22.6, 23.4, 25.1),
  units=c(185L, 215L, 332L, 325L, 408L, 421L, 
          406L, 412L, 522L, 445L, 544L, 614L)
)


basicPlot <- function(...){
  plot(units ~ temp, data=icecream, bty="n", lwd=2,
       main="Number of ice creams sold", col="#00526D", 
       xlab="Temperature (Celsius)", 
       ylab="Units sold", ...)
  axis(side = 1, col="grey")
  axis(side = 2, col="grey")
}
basicPlot()
#bty is the box type "n" means we don't want a box 

#OLS
lsq.mod <- lsfit(icecream$temp, icecream$units)
basicPlot()
abline(lsq.mod, col="orange", lwd=2)
legend(x="topleft", bty="n", lwd=c(2,2), lty=c(NA,1),
       legend=c("observation", "linear least square"),
       col=c("#00526D","orange"),  pch=c(1,NA))

#GLM way of writing OLS
lin.mod <- glm(units ~ temp, data=icecream, 
               family=gaussian(link="identity"))
library(arm) # for 'display' function only
display(lin.mod)

#GLM lognormral 
log.lin.mod <- glm(log(units) ~ temp, data=icecream, 
                   family=gaussian(link="identity"))
display(log.lin.mod)
## glm(formula = log(units) ~ temp, family = gaussian(link = "identity"), 
##     data = icecream)
##             coef.est coef.se
## (Intercept) 4.40     0.20   
## temp        0.08     0.01   
## ---
##   n = 12, k = 2
##   residual deviance = 0.2, null deviance = 1.4 (difference = 1.2)
##   overdispersion parameter = 0.0
##   residual sd is sqrt(overdispersion) = 0.14
log.lin.sig <- summary(log.lin.mod)$dispersion
log.lin.pred <- exp(predict(log.lin.mod) + 0.5*log.lin.sig)
basicPlot()
lines(icecream$temp, log.lin.pred, col="red", lwd=2)
legend(x="topleft", bty="n", lwd=c(2,2), lty=c(NA,1),
       legend=c("observation", "log-transformed LM"),
       col=c("#00526D","red"), pch=c(1,NA))