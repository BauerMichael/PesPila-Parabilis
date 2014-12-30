createDirs <- function(path = "data", league = "bundesliga", folders = "", warnings = FALSE) {
    dir.create(paste(path, league, sep = "/"), showWarnings = warnings)
    for (folder in folders) {
        dir.create(paste(path, league, folder, sep = "/"), showWarnings = warnings)
    }
}

downloadFiles <- function(url = "http://www.football-data.co.uk/mmz4281", league = "bundesliga",
                          path = "data", folders = "", files = "") {
    for (folder in folders) {
        for (file in files) {
            download.file(paste(url, folder, file, sep = "/"),
                           destfile = paste(path, league, folder, file, sep = "/"))
        }
    }
}

extractData <- function(path = "data", folders = "", files = "", league = "bundesliga",
                        analysisType = "statistics.csv") {
    names <- scan(file = paste(path, analysisType, sep = "/"), what = "character")
    col <- length(names)
    
    for (folder in folders) {
        for (file in files) {
            mainData <- read.csv(paste(path, league, folder, file, sep = "/"))
            row <- dim(mainData)[1]
            cleanData <- data.frame(matrix(data = 0, nrow = row, ncol = col))
            colnames(cleanData) <- names
            for (name in names) {
                if (name %in% colnames(mainData)) {
                    cleanData[, name] <- mainData[, name]
                } else {
                    cleanData[, name] <- 0
                }
            }
            cleanData <- subset(cleanData, Div != "")
            write.csv(cleanData, paste(path, league, folder, file, sep = "/"),
                      row.names = FALSE)
        }
    }
}

combineAllData <- function(path = "data", folders = "", files = "", league = "bundesliga",
                        filename = "complete.csv", analysisType = "statistics.csv") {
    names <- scan(file = paste(path, analysisType, sep = "/"), what = "character")
    col <- length(names)
    mainData <- data.frame(matrix(data = 0, nrow = 0, ncol = col))
    colnames(mainData) <- names
    for (folder in folders) {
        for (file in files) {
            tmp <- read.csv(paste(path, league, folder, file, sep = "/"))
            mainData <- rbind(mainData, tmp)
        }
    }
    D1 <- subset(mainData, Div == "D1")
    D2 <- subset(mainData, Div == "D2")
    write.csv(mainData, paste(path, league, filename, sep = "/"), row.names = FALSE)
    write.csv(D1, paste(path, league, "D1.csv", sep = "/"), row.names = FALSE)
    write.csv(D2, paste(path, league, "D2.csv", sep = "/"), row.names = FALSE)
}

createTeamFolder <- function(path = "data", league = "bundesliga", filename = "teams.csv",
                             warnings = FALSE) {
    teams <- scan(file = paste(path, filename, sep = "/"), what = "character", sep = "\n")
    for (team in teams) {
        dir.create(paste(path, league, team, sep = "/"), showWarnings = warnings)
    }
}

# updateSeason <- function(path = "data", league = "bundesliga", season = "1415",
#                          filename = "complete.csv") {
#     D1 <- read.csv(paste(path, league, season, "D1", sep = "/"))
#     D2 <- read.csv(paste(path, league, season, "D2", sep = "/"))
#     names <- scan(file = paste(path, analysisType, sep = "/"), what = "character")
#     col <- length(names)
#     mainData <- data.frame(matrix(data = 0, nrow = 0, ncol = col))
#     colnames(mainData) <- names
#     for (folder in folders) {
#         for (file in files) {
#             tmp <- read.csv(paste(path, league, folder, file, sep = "/"))
#             mainData <- rbind(mainData, tmp)
#             write.csv(mainData, paste(path, filename, sep = "/"), row.names = FALSE)
#         }
#     }
# }

init <- function(path = "data", filename = "complete.csv") {
    seasons <- c("9394", "9495", "9596", "9697",
                 "9798", "9899", "9900", "0001",
                 "0102", "0203", "0304", "0405",
                 "0506", "0607", "0708", "0809",
                 "0910", "1011", "1112", "1213",
                 "1314", "1415")
    divisions <- c("D1.csv", "D2.csv")
    createDirs(folders = seasons)
    downloadFiles(folders = seasons, files = divisions)
    extractData(folders = seasons, files = divisions)
    combineAllData(folders = seasons, files = divisions)
    createTeamFolder()
}