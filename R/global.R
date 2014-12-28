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

colNames <- function(mypath = "data", filename = "colNames.csv") {
    return (read.csv(paste(mypath, filename, sep = "/")))
}

loadData <- function(path = "data") {
    seasons <- c("9394", "9495", "9596", "9697",
                 "9798", "9899", "9900", "0001",
                 "0102", "0203", "0304", "0405",
                 "0506", "0607", "0708", "0809",
                 "0910", "1011", "1112", "1213",
                 "1314", "1415")
    divisions <- c("D1.csv", "D2.csv")
    createDirs(mypath = path, folders = seasons)
    downloadFiles(folders = seasons, files = divisions)
}

cleanDataFiles <- function(path = "data") {
    seasons <- c("9394", "9495", "9596", "9697",
                 "9798", "9899", "9900", "0001",
                 "0102", "0203", "0304", "0405",
                 "0506", "0607", "0708", "0809",
                 "0910", "1011", "1112", "1213",
                 "1314", "1415")
    divisions <- c("D1.csv", "D2.csv")
    names <- as.matrix(colNames())
    names <- as.character(names[,1])
    col <- length(names)
    for (season in seasons) {
        for (division in divisions) {
            mainData <- read.csv(paste(path, season, division, sep = "/"))
            dim <- dim(mainData)
            if (dim[2] < col) {
                newData <- cbind(mainData, matrix(0, ncol = col-dim[2], nrow = dim[1]))
                colnames(newData) <- names
                write.csv(newData, paste(path, season, division, sep = "/"))
            } else if (dim[2] == col) {
                colnames(mainData) <- names
                write.csv(mainData, paste(path, season, division, sep = "/"))
            } else {
                newData <- mainData[,1:col]
                colnames(newData) <- names
                write.csv(newData, paste(path, season, division, sep = "/"))
            }
        }
    }
}

pasteData <- function(path = "data", filename = "complete.csv") {
    seasons <- c("9394", "9495", "9596", "9697",
                 "9798", "9899", "9900", "0001",
                 "0102", "0203", "0304", "0405",
                 "0506", "0607", "0708", "0809",
                 "0910", "1011", "1112", "1213",
                 "1314", "1415")
    divisions <- c("D1.csv", "D2.csv")
    names <- as.matrix(colNames())
    names <- as.character(names[,1])
    row <- 0
    col <- length(names)
    mainData <- data.frame(matrix(data = 0, nrow = row, ncol = col))
    colnames(mainData) <- names
    for (season in seasons) {
        for (division in divisions) {
            tmp <- read.csv(file = paste(path, season, division, sep = "/"))
            mainData <- rbind(mainData, tmp)
            write.csv(mainData[,1:28], paste(path, filename, sep = "/"))
        }
    }
}

init <- function(path = "data", filename = "complete.csv") {
    loadData(path)
    cleanDataFiles(path)
    pasteData(path, filename)
#     out <- read.csv(paste(path, filename, sep = "/"))
#     return (out)
}