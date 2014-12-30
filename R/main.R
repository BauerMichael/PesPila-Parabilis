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
#     stats <- loadPreferedStatistics()
#     sums <- c()
#     for (stat in stats) {
#         sums <- c(sums, sum(as.vector(data[, stat])))
#     }
    return (sums)
}