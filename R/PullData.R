library(readr)
library(lubridate)
library(forcats)
library(dplyr)
library(tidyr)

"%+%" <- function(...) paste0(...)

WrangleData <- function(yr) {
    link <- .CreateLink(yr)
    df <- .ReadData(link) %>%
        .AddGW()
}

.ReadData <- function(link) {
    data <- readr::read_csv(
        "http://www.football-data.co.uk/mmz4281/1920/E0.csv"
    )

    data <- data[, 1:24]

    data <- data %>%
        select(-Div) %>%
        mutate(timestamp = Date %+% " " %+% Time) %>%
        mutate(timestamp = gsub("/", "-", timestamp)) %>%
        mutate(timestamp = dmy_hms(timestamp)) %>%
        select(-Date, -Time, -HTHG, -HTAG, -HTR, -Referee) %>%
        select(timestamp, HomeTeam:AR) %>%
        mutate(season = max(year(timestamp))) %>%
        mutate(HomeTeam = as_factor(HomeTeam)) %>%
        mutate(AwayTeam = as_factor(AwayTeam))

    return(data)
}

.CreateLink <- function(yr) {
    part1 <- "http://www.football-data.co.uk/mmz4281/"
    part2 <- "/E0.csv"

    link <- part1 %+% (yr - 1) %+% yr %+% part2

    return(link)
}

.AddGW <- function(data) {

    gameweeks <- 1:38
    gwperteam <- rep(gameweeks, 10)
    gwdata <- tibble(
        gw = gwperteam
    ) %>%
    arrange(gw)

    data$gw <- gwdata$gw

    data <- data %>%
        mutate(gw = as_factor(gw))
    return(data)
}

.CreateTable <- function(data) {
    teams <- unique(data$HomeTeam)

    base_table <- tibble(
        team = teams,
        points = 0,
        gw = 0,
        position = 0
    )
    table <- base_table
    weeks <- 1:38

    for (week in weeks) {
        dat <- data %>% filter(gw == week)

        tab <- base_table
        tab$gw <- week

        for (game in 1:nrow(dat)) {
            ht <- dat$HomeTeam[game]
            at <- dat$AwayTeam[game]

            if (dat$FTR[game] == "H") {
                htp <- 3
                atp <- 0
            }
            if (dat$FTR[game] == "A") {
                htp <- 0
                atp <- 3
            }
            if (dat$FTR[game] == "D") {
                htp <- 1
                atp <- 1
            } 

            tab$points[tab$team == ht] <- htp + table$points[table$team == ht & table$gw == (week - 1)] 
            tab$points[tab$team == at] <- atp + table$points[table$team == at & table$gw == (week - 1)]
        }

        table <- rbind(table, tab)
    }

    table <- table[-which(table$gw == 0), ]
}

df <- WrangleData(20)