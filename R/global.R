createDirs <- function(mypath = "data", folders = "1415", warnings = FALSE) {
    for (folder in folders) {
        dir.create(path = paste(mypath, folder, sep = "/"), showWarnings = warnings)
    }
}

downloadFiles <- function(myurl = "http://www.football-data.co.uk/mmz4281",
                          folders = "1415", files = "D1.csv", mydest = "data") {
    for (folder in folders) {
        for (file in files) {
            download.file(url = paste(myurl, folder, file, sep = "/"),
                           destfile = paste(mydest, folder, file, sep = "/"))
        }
    }
}

initData <- function() {
    seasons <- c("9394", "9495", "9596", "9697",
                 "9798", "9899", "9900", "0001",
                 "0102", "0203", "0304", "0405",
                 "0506", "0607", "0708", "0809",
                 "0910", "1011", "1112", "1213",
                 "1314", "1415")
    divisions <- c("D1.csv", "D2.csv")
    createDirs(folders = seasons)
    downloadFiles(folders = seasons, files = divisions)
}
# 
# # col <- 70
# # myData <- data.frame(matrix(0, 0, col))
# # seasons <- c("9394", "9495", "9596", "9697",
# #              "9798", "9899", "9900", "0001",
# #              "0102", "0203", "0304", "0405",
# #              "0506", "0607", "0708", "0809",
# #              "0910", "1011", "1112", "1213",
# #              "1314", "1415");
# # # seasons <- c("93-94", "94-95", "95-96");
# # seasons <- c("10-11");
# # divisions <- c("D1.csv");
# # # divisions <- c("D1.csv", "D2.csv");
# # seasons <- rev(seasons)
# # 
# # for (season in seasons) {
# #     for (division in divisions) {
# #         this <- read.csv(file = paste(season, division, sep = "/"))
# # #         print(c(season, division, dim(this)))
# #         if (dim(this)[2] < col) {
# #             this <- cbind(this, matrix(0.0, dim(this)[1], col-dim(this)[2]))
# #             print(c(season, division, dim(this)))
# #         }
# #         myData <- rbind(myData, this)
# #     }
# # }