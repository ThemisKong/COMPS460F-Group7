library(splines)

x      <- my_data$Daily_Screen_Time.hrs     
y      <- my_data$Happiness_Index.1.10      
sort.x <- sort(x)                           
x.lab  <- "Daily Screen Time (hours)"
y.lab  <- "Happiness Index (1-10)"

# Create two plots side by side
par(mfrow = c(1,2))

#Quadratic B-spline(degree = 2, df = 6)
spline.bs3 <- lm(y ~ bs(x, degree = 2, df = 6))

pred.bs3     <- predict(spline.bs3, newdata = list(x = sort.x), se.fit = TRUE)
pred.bs3_y   <- pred.bs3$fit
se.bs3       <- pred.bs3$se.fit
se.bands.bs3 <- cbind(pred.bs3_y + 2*se.bs3, pred.bs3_y - 2*se.bs3)

plot(x, y, cex.lab = 1.1, col = "darkgrey", 
     xlab = x.lab, ylab = y.lab, 
     main = "Quadratic Spline", bty = 'l')
lines(sort.x, pred.bs3_y, lwd = 2, col = "darkgreen")
matlines(sort.x, se.bands.bs3, lwd = 2, col = "darkgreen", lty = 3)


#Cubic B-spline(degree = 3, df = 7)
spline.bs4 <- lm(y ~ bs(x, degree = 3, df = 7))

pred.bs4     <- predict(spline.bs4, newdata = list(x = sort.x), se.fit = TRUE)
pred.bs4_y   <- pred.bs4$fit
se.bs4       <- pred.bs4$se.fit
se.bands.bs4 <- cbind(pred.bs4_y + 2*se.bs4, pred.bs4_y - 2*se.bs4)

plot(x, y, cex.lab = 1.1, col = "darkgrey", 
     xlab = x.lab, ylab = y.lab, 
     main = "Cubic Spline", bty = 'l')
lines(sort.x, pred.bs4_y, lwd = 2, col = "darkblue")
matlines(sort.x, se.bands.bs4, lwd = 2, col = "darkblue", lty = 3)