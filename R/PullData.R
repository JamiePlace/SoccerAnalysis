library(readr)
library(lubridate)
library(forcats)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

"%+%" <- function(...) paste0(...)

WrangleData <- function(yr) {
    link <- .CreateLink(yr)

    df <- .ReadData(link) %>%
        .AddGW() %>%
        mutate(
            hgw = NA,
            agw = NA,
            gnum = 1:n()
        )

    #pl_table <- .CreateTable(df)
}

.ReadData <- function(link) {
    data <- readr::read_csv(
        link
    )

    cols_2_keep <- c(
        "timestamp",
        "HomeTeam",
        "AwayTeam",
        "FTHG",
        "FTAG",
        "FTR",
        "HS",
        "AS",
        "HST",
        "AST",
        "HF",
        "AF",
        "HC",
        "AC"
    )

    data <- data %>%
        .ParseDates() %>%
        select(all_of(cols_2_keep)) %>%
        mutate(season = max(year(timestamp))) %>%
        mutate(HomeTeam = as_factor(HomeTeam)) %>%
        mutate(AwayTeam = as_factor(AwayTeam))

    names(data) <- tolower(names(data))

    return(data)
}

.CreateLink <- function(yr) {
    part1 <- "http://www.football-data.co.uk/mmz4281/"
    part2 <- "/E0.csv"

    prev_yr <- stringr::str_pad((as.numeric(yr) - 1), 2, "left", 0)

    link <- part1 %+% prev_yr %+% yr %+% part2

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
        group_by(season) %>%
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

    return(table)
}

.ParseDates <- function(data) {
    data <- data %>%
        mutate(timestamp = lubridate::dmy(Date))

    return(data)
}


df <- WrangleData(20)

View(head(df %>% filter(HomeTeam == "Liverpool" | AwayTeam == "Liverpool")))

fpl_table <- .CreateTable(df)


plt <- ggplot(fpl_table) +
    geom_line(aes(x = gw, y = points, colour = team))