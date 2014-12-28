# data(co2)
# require(graphics)
# 
# myCO2 <- function(bins) {
#   plot(co2, ylab = expression("Atmospheric concentration of CO"[2]),
#        las = 1, xlim = c(1959, bins))
#   title(main = "co2 data set")
# }

col <- 70
myData <- data.frame(matrix(0, 0, col))
# seasons <- c("myApp//93-94", "myApp/94-95/", "myApp//95-96", "myApp//96-97",
#              "myApp//97-98", "myApp//98-99", "myApp//99-00", "myApp//00-01",
#              "myApp//01-02", "myApp//02-03", "myApp//03-04", "myApp//04-05",
#              "myApp//05-06", "myApp//06-07", "myApp//07-08", "myApp//08-09",
#              "myApp//09-10", "myApp//10-11", "myApp//11-12", "myApp//12-13",
#              "myApp//13-14", "myApp//14-15");
# seasons <- c("myApp//93-94", "myApp/94-95/", "myApp//95-96");
seasons <- c("myApp//10-11");
divisions <- c("D1.csv");
# divisions <- c("D1.csv", "D2.csv");
seasons <- rev(seasons)

for (season in seasons) {
    for (division in divisions) {
        this <- read.csv(file = paste(season, division, sep = "/"))
#         print(c(season, division, dim(this)))
        if (dim(this)[2] < col) {
            this <- cbind(this, matrix(0.0, dim(this)[1], col-dim(this)[2]))
            print(c(season, division, dim(this)))
        }
        myData <- rbind(myData, this)
    }
}


# k <- read.csv(file = "myApp/93-94/D1.csv")
# l <- read.csv(file = "myApp/00-01/D2.csv")[,1:4]
# test <- rbind(k, l)
# test <- cbind(mySet, k)