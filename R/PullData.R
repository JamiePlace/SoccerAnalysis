library(readr)
library(lubridate)
library(forcats)
library(dplyr)
library(tidyr)
library(ggplot2)

"%+%" <- function(...) paste0(...)

WrangleData <- function(yr) {
    link <- .CreateLink(yr)

    df <- .ReadData(link) %>%
        .AddGW()
}

.ReadData <- function(link) {
    data <- readr::read_csv(
        link
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

        tab <- table[table$gw == (week - 1), ]
        tab$gw <- week

        for (game in 1:nrow(dat)) {


            ht <- as.character(dat$HomeTeam[game])
            at <- as.character(dat$AwayTeam[game])


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

            previous_hp <- table %>%
                filter(team == ht) %>%
                filter(gw == week - 1) %>%
                select(points) %>%
                pull()

            previous_ap <- table %>%
                filter(team == at) %>%
                filter(gw == week - 1) %>%
                select(points) %>%
                pull()

            tab$points[tab$team == ht & tab$gw == week] <- htp + previous_hp
            tab$points[tab$team == at & tab$gw == week] <- atp + previous_ap
        }
        table <- rbind(table, tab)
    }

    #table <- table %>%
    #    filter(gw != 0)

    return(table)
}

df <- WrangleData(19)
fpl_table <- .CreateTable(df)


plt <- ggplot(fpl_table) +
    geom_line(aes(x = gw, y = points, colour = team))