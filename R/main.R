loadAllContent <- function(path = "data", filename = "complete.csv") {
    return (read.csv(file = paste(path, filename, sep = "/")))
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
    teams <- loadTeams()
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