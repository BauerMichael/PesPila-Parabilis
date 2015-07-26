loadAllContent <- function(path = "data", league = "bundesliga", filename = "complete.csv") {
    return (read.csv(file = paste(path, league, filename, sep = "/")))
}

loadLeagueContent <- function(path = "data", league = "bundesliga", filename = "D1.csv") {
    return (read.csv(file = paste(path, league, filename, sep = "/")))
}

loadTeams <- function(path = "data", filename = "teams.csv") {
    return (scan(file = paste(path, filename, sep = "/"), what = "character", sep = "\n"))
}

loadPreferedStatistics <- function(path = "data", filename = "statistics.csv") {
    return (scan(file = paste(path, filename, sep = "/"), what = "character", sep = "\n"))
}

extractTeamData <- function(path = "data", league = "bundesliga", filename = "data.csv") {
    teams <- loadTeams()
    data <- loadContent()
    for (team in teams) {
        home <- subset(data, HomeTeam == team)
        away <- subset(data, AwayTeam == team)
        whole <- rbind(home, away)
        write.csv(whole, paste(path, league, team, filename, sep = "/"), row.names = FALSE)
    }
}

overallData <- function(path = "data", league = "bundesliga", filename = "leagueData.csv") {
    data <- loadLeagueContent()
    results <- matrix(0, 10, 10)
    vec <- as.vector(data[, "FTR"])
    sums <- c(0, 0, 0)
    for (val in vec) {
        if (val == "H") {
            sums[1] <- sums[1] + 1
        } else if (val == "D") {
            sums[2] <- sums[2] + 1
        } else {
            sums[3] <- sums[3] + 1
        }
    }
    sums <- c(sums, sum(data[, "FTHG"]))
    sums <- c(sums, sum(data[, "FTAG"]))
    sums <- matrix(sums)
    
    outcomes <- 0:9
    rownames(results) <- outcomes
    colnames(results) <- outcomes
    home <- as.character(data[, "FTHG"])
    away <- as.character(data[, "FTAG"])
    for (i in 1:length(home)) {
        results[home[i], away[i]] <- results[home[i], away[i]] + 1
    }
    
    difference <- matrix(0, 1, 9)
    colnames(difference) <- as.character(0:8)
    
    home <- data[, "FTHG"]
    away <- data[, "FTAG"]
    for (i in 1:length(home)) {
        diff <- as.character(abs(home[i]-away[i]))
        difference[1, diff] <- difference[1, diff] + 1
    }
    
    goalSumsPerMatch <- matrix(0, 12, 1)
    rownames(goalSumsPerMatch) <- as.character(0:11)
    
    for (i in 1:length(home)) {
        gspmSums <- as.character(home[i] + away[i])
        goalSumsPerMatch[gspmSums, 1] <- goalSumsPerMatch[gspmSums, 1] + 1
    }
    
    out <- list(sums, results, difference, goalSumsPerMatch)
    return (out)
}

allGamesComparison <- function(home = "", away = "") {
    data <- loadAllContent()
    compareTable <- data[which(data$HomeTeam == home
                               & data$AwayTeam == away), ]
    compareTable <- rbind(compareTable, data[which(data$HomeTeam == away
                               & data$AwayTeam == home), ])
    return (compareTable)
}

teamComparison <- function(home = "", away = "") {
    data <- loadAllContent()
    compareTable <- data[which(data$HomeTeam == home
                               & data$AwayTeam == away), ]
    return (compareTable)
}

sumUpComparison <- function(home = "", away = "") {
    specData <- teamComparison(home, away)
    column.names <- colnames(specData)
    colNames <- c("Home Team", "Away Team", "Home Win in %", "Draw in %", "Away Win in %",
                  "Sum of home goals", "Sum of away goals", "Average home goals",
                  "Average away goals")
    sumUpData <- c(home, away, 0, 0, 0, 0, 0, 0, 0)
    len <- dim(specData)[1]
    for (i in 1:len) {
        if (specData[i, "FTR"] == "H") {
            sumUpData[3] <- as.integer(sumUpData[3]) + 1
        } else if (specData[i, "FTR"] == "D") {
            sumUpData[4] <- as.integer(sumUpData[4]) + 1
        } else {
            sumUpData[5] <- as.integer(sumUpData[5]) + 1
        }
    }
    sumUpData[6] <- sum(specData[, "FTHG"])
    sumUpData[7] <- sum(specData[, "FTAG"])
    sumUpData <- matrix(sumUpData)
    sumUpData <- t(sumUpData)
    colnames(sumUpData) <- colNames
    # colnames(sumUpData) <- column.names
    
    return (sumUpData)
}

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