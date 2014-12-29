loadContent <- function() {
    return (read.csv(file = "data//complete.csv"))
}

returnTeams <- function(path = "data", filename = "teams.csv") {
    teams <- scan(file = paste(path, filename, sep = "/"), what = "character", sep = "\n")
    return (teams)
}

extractTeamData <- function(path = "data", league = "bundesliga") {
    teams <- returnTeams()
    data <- loadContent()
    for (team in teams) {
        home <- subset(data, HomeTeam == team)
        away <- subset(data, AwayTeam == team)
        whole <- rbind(home, away)
        write.csv(whole, paste(path, league, team, "data.csv", sep = "/"), row.names = FALSE)
    }
}