# load libraries
library(ggplot2)
library(RColorBrewer)
library(dplyr)

# load data
raw_data <- read.csv("./baseballdatabank-master/Teams.csv", na.strings = "")

# select data
modern_teams = c("KCR", "MIN", "CLE", "CHW", "DET",
                  "TOR", "NYY", "BAL", "TBD", "BOS",
                  "TEX", "HOU", "ANA", "SEA", "OAK",
                  "STL", "PIT", "CHC", "MIL", "CIN",
                  "NYM", "WSN", "FLA", "ATL", "PHI",
                  "LAD", "SFG", "ARI", "SDP", "COL")

# select team, ask user for input
#team_select <- as.character(readline("Please enter a MLB team: "))
#while (team_select %in% modern_teams == FALSE) {
#    team_select <- as.character(readline("Please enter a MLB team: "))
#}
team_select <- "CHC"

# modern teams and modern era
data <- subset(raw_data, franchID %in% modern_teams)
data <- subset(data, yearID > 1900)

# select only necessary columns
data <- data[c("yearID","franchID","W","L",
                   "DivWin","WCWin","LgWin","WSWin")]

# make win percentage column
data$WinPercent <- data$W / (data$W + data$L)

# subset for team
data_2 <- subset(data, franchID == team_select)

# subset for playoffs
data_3 <- subset(data_2, DivWin == "Y")
data_4 <- subset(data_2, WCWin == "Y")
data_5 <- subset(data_2, LgWin == "Y")
data_6 <- subset(data_2, WSWin == "Y")

# plot all teams
g0 = ggplot(data = data, aes(x=yearID, y=WinPercent)) +
     theme_bw() +
     geom_point(alpha = 0.4) +
     geom_line(alpha = 0.2)

# calculate winning percent quantiles
data_tbl <- tbl_df(data)
data_tbl_year <- group_by(data_tbl, yearID)
data_tbl_year_wp <- summarise(data_tbl_year,
                              w_mean = mean(WinPercent),
                              w_median = median(WinPercent),
                              w_1q = quantile(WinPercent, 0.25),
                              w_2q = quantile(WinPercent, 0.50),
                              w_3q = quantile(WinPercent, 0.75))

# add quantiles to plot
g1 = g0 + geom_line(data = data_tbl_year_wp,
                 aes(x = yearID, y = w_2q),
                 stat="smooth",
                 method = "loess", span = 0.5,
                 linetype = "dashed",
                 se=FALSE,
                 color = "black",
                 size = 0.5, alpha = 0.75) +
    geom_line(data = data_tbl_year_wp,
                aes(x = yearID, y = w_1q),
                stat="smooth",
                method = "loess", span = 0.5,
                linetype = "dashed",
                se=FALSE,
                color = "black",
                size = 0.5, alpha = 0.75) +
    geom_line(data = data_tbl_year_wp,
                aes(x = yearID, y = w_3q),
                stat="smooth",
                method = "loess", span = 0.5,
                linetype = "dashed",
                se=FALSE,
                color = "black",
                size = 0.5, alpha = 0.75)

# add team of interest to plot
g2 = g1 + geom_line(data=data_2, aes(x=yearID, y=WinPercent),
                    color="blue4", size=1.5, alpha = 0.9) +
          geom_point(data=data_2, aes(x=yearID, y=WinPercent),
                     color="blue4", size=3) +
          geom_smooth(data=data_2, aes(x=yearID, y=WinPercent),
                     method = "loess", span = 0.5,
                     color="red3", alpha = 0.1, fill="red4", size = 1)

# colors for playoffs
cols <- c("3 - Division Winner"="gold3",
          "4 - Wild Card Winner"="hotpink4",
          "2 - League Champion"="darkorange3",
          "1 - World Series Champion"="green4")

# label order for playoffs
labs <- c("3 - Division Winner"="Division Winner",
          "4 - Wild Card Winner"="Wild Card WInner",
          "2 - League Champion"="League Champion",
          "1 - World Series Champion"="World Series Champion")

# division win
g3 = g2 + geom_point(data=data_3,
                     aes(x=yearID, y=WinPercent,
                         color="3 - Division Winner"),
                     size = 4, shape = 16)

# wild card win
g4= g3 + geom_point(data=data_4,
                    aes(x=yearID, y=WinPercent,
                        color="4 - Wild Card Winner"),
                    size = 4, shape = 16)

# league win
g5 = g4 + geom_point(data=data_5,
                     aes(x=yearID, y=WinPercent,
                         color="2 - League Champion"),
                     size = 4, shape = 16)

# world series win
g6 = g5 + geom_point(data=data_6,
                     aes(x=yearID, y=WinPercent,
                         color="1 - World Series Champion"),
                     size = 4, shape = 16)

# plot labels and axis
g7 = g6  + geom_abline(intercept = 0.5, slope = 0,
                  color = "black", alpha = 0.75) +
    ggtitle(paste(team_select,
                  "Winning Percentage and Playoff History (1901-2015)")) +
    xlab("Year") +
    ylab("Winning Percentage Ratio, W/(W+L)") +
    scale_color_manual(name=c(""),
                      values=(cols),
                      labels=labs) +
    scale_x_continuous(limits = c(1900,2020),
                       breaks = seq(1900, 2020, by = 10)) +
    scale_y_continuous(limits = c(0.2,0.8),
                       breaks = seq(0.2, 0.8, by = 0.05))

# print final plot
g7

# explort TSV file for dimple.js visualization -----------------------

# replace missing values
data$DivWin[is.na(data$DivWin)] <- "N"
data$WCWin[is.na(data$WCWin)] <- "N"
data$LgWin[is.na(data$LgWin)] <- "N"
data$WSWin[is.na(data$WSWin)] <- "N"

# make new postseason column, melt data manually
data$PostSeason <- NA
for ( x in 1:dim(data)[1] ) {
    if ( data$WSWin[x] == "Y" ) {
        data$PostSeason[x] <- "World Series Champion"
    } else if ( data$LgWin[x] == "Y" ) {
        data$PostSeason[x] <- "League Champion"
    } else if ( data$WCWin[x] == "Y" ) {
        data$PostSeason[x] <- "Wild Card Winner"
    } else if ( data$DivWin[x] == "Y" ) {
        data$PostSeason[x] <- "Division Winner"
    } else { }
}

## select only necessary data
data_export <- data[c("yearID","franchID","WinPercent","PostSeason")]

# export data to tsv file
write.table(data_export,
            file='Teams_Edited.tsv',
            quote=FALSE,
            sep='\t',
            row.names = FALSE)
