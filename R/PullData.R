library(readr)
library(lubridate)
library(forcats)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(curl)

"%+%" <- function(...) paste0(...)

WrangleData <- function(yr) {
    link <- .CreateLink(yr)

    df <- .ReadData(link) %>%
        .AddGW() %>%
        mutate(
            hgw = NA,
            agw = NA,
            hpos = NA,
            apos = NA,
            hpoints = NA,
            apoints = NA,
            hgoalscored = NA,
            agoalscored = NA,
            hgoalconceded = NA,
            agoalconceded = NA,
            hshots = NA,
            ashots = NA,
            hshotstarget = NA,
            ashotstarget = NA,
            hfouls = NA,
            afouls = NA,
            hcorners = NA,
            acorners = NA,
            gnum = 1:n()
        ) %>%
        .GetTeamGW() %>%
        .GetTeamPos()
    
    
    
    return(df)
}

.GetTeamPos <- function(df) {
    teams <- unique(
        c(
            as.character(df$hometeam), 
            as.character(df$awayteam)
        )
    )
    
    fpl_table <- .CreateTable(df)
    
    
    data <- NULL
    for (week in as.numeric(unique(df$gw))) {
        
        sub_dat <- df %>%
            filter(gw == as.character(week))
        
        for (game in seq_len(nrow(sub_dat))) {
            ###############
            # position
            hpos <- fpl_table$position[
                which(
                    fpl_table$team == sub_dat$hometeam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            apos <- fpl_table$position[
                which(
                    fpl_table$team == sub_dat$awayteam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            ###############
            # points
            hpoints <- fpl_table$points[
                which(
                    fpl_table$team == sub_dat$hometeam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            apoints <- fpl_table$points[
                which(
                    fpl_table$team == sub_dat$awayteam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            ###############
            # goal scored
            hgoalscored <- fpl_table$goalscored[
                which(
                    fpl_table$team == sub_dat$hometeam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            agoalscored <- fpl_table$goalscored[
                which(
                    fpl_table$team == sub_dat$awayteam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            ###############
            # goal conceded
            hgoalconceded <- fpl_table$goalconceded[
                which(
                    fpl_table$team == sub_dat$hometeam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            agoalconceded <- fpl_table$goalconceded[
                which(
                    fpl_table$team == sub_dat$awayteam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            ###############
            # shots
            hshots <- fpl_table$shots[
                which(
                    fpl_table$team == sub_dat$hometeam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            ashots <- fpl_table$shots[
                which(
                    fpl_table$team == sub_dat$awayteam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            ###############
            # shots target
            hshotstarget <- fpl_table$shotstarget[
                which(
                    fpl_table$team == sub_dat$hometeam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            ashotstarget <- fpl_table$shotstarget[
                which(
                    fpl_table$team == sub_dat$awayteam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            ###############
            # fouls
            hfouls <- fpl_table$fouls[
                which(
                    fpl_table$team == sub_dat$hometeam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            afouls <- fpl_table$fouls[
                which(
                    fpl_table$team == sub_dat$awayteam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            ###############
            # corners
            hcorners <- fpl_table$corners[
                which(
                    fpl_table$team == sub_dat$hometeam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            acorners <- fpl_table$corners[
                which(
                    fpl_table$team == sub_dat$awayteam[game] &
                        fpl_table$gw == (week - 1)
                )
            ]
            
            ###############
            sub_dat$hpos[game] <- hpos
            sub_dat$apos[game] <- apos
            
            sub_dat$hpoints[game] <- hpoints
            sub_dat$apoints[game] <- apoints
            
            sub_dat$hgoalscored[game] <- hgoalscored
            sub_dat$agoalscored[game] <- agoalscored
            
            sub_dat$hgoalconceded[game] <- hgoalconceded
            sub_dat$agoalconceded[game] <- agoalconceded
            
            sub_dat$hshots[game] <- hshots
            sub_dat$ashots[game] <- ashots
            
            sub_dat$hshotstarget[game] <- hshotstarget
            sub_dat$ashotstarget[game] <- ashotstarget
            
            sub_dat$hfouls[game] <- hfouls
            sub_dat$afouls[game] <- afouls
            
            sub_dat$hcorners[game] <- hcorners
            sub_dat$acorners[game] <- acorners
            
        }
        
        if (is.null(data)) {
            data <- sub_dat
        } else {
            data <- rbind(data, sub_dat)
        }
    }
    
    return(data)
}

.GetTeamGW <- function(df) {
    data <- NULL # empty dataframe to append to
    
    unique_teams <- unique(c(as.character(df$hometeam), as.character(df$awayteam)))
    
    for (team in unique_teams) {
        
        sub_df <- df %>% filter(hometeam == team | awayteam == team)
        
        for (game in seq_len(nrow(sub_df))) {
            if (sub_df$hometeam[game] == team) {
                sub_df$hgw[game] <- game
            }
            if (sub_df$awayteam[game] == team) {
                sub_df$agw[game] <- game
            }
        }
        
        if (is.null(data)) {
            data <- sub_df %>% select(gnum, hgw, agw)
        } else {
            data <- rbind(data, sub_df %>% select(gnum, hgw, agw))
        }
    }
    
    data <- data %>% ungroup()
    
    
    gw_data <- NULL
    for (game in unique(df$gnum)) {
        sub_df <- data %>% filter(gnum == game) %>% select(-season)
        df1 <- sub_df[1,] %>% select_if(~ !any(is.na(.)))
        df2 <- sub_df[2,] %>% select_if(~ !any(is.na(.)))
        
        sub_df <- left_join(df1, df2, by = "gnum")
        
        if (is.null(gw_data)) {
            gw_data <- sub_df
        } else {
            gw_data <- rbind(gw_data, sub_df)
        }
    }
    
    df <- df %>% 
        select(-hgw, -agw) %>%
        left_join(gw_data, by = "gnum")
    
    return(df)
}

.ReadData <- function(link) {
    data <- readr::read_csv(
        link
    )
    
    
    # remove last rows of the 14 15 season
    # something weird with the data
    if (link == "http://www.football-data.co.uk/mmz4281/1415/E0.csv") {
        data <- data[-nrow(data), ]
    }
    
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
    
    curr_yr <- stringr::str_pad(yr, width = 2, side = "left", 0)
    prev_yr <- stringr::str_pad((as.numeric(yr) - 1), 2, "left", 0)

    link <- part1 %+% prev_yr %+% curr_yr %+% part2

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
    teams <- unique(c(as.character(data$hometeam), as.character(data$awayteam)))

    base_table <- tibble(
        team = teams,
        points = 0,
        gw = 0,
        position = 0,
        goalscored = 0,
        goalconceded = 0,
        shots = 0,
        shotstarget = 0,
        fouls = 0,
        corners = 0
    )
    
    table <- base_table
    weeks <- 1:38

    for (week in weeks) {
        dat <- data %>% filter(gw == week)

        tab <- table[table$gw == (week - 1), ]
        tab$gw <- week

        for (game in 1:nrow(dat)) {

            ht <- as.character(dat$hometeam[game])
            at <- as.character(dat$awayteam[game])

            if (dat$ftr[game] == "H") {
                htp <- 3
                atp <- 0
            }
            if (dat$ftr[game] == "A") {
                htp <- 0
                atp <- 3
            }
            if (dat$ftr[game] == "D") {
                htp <- 1
                atp <- 1
            } 
            
            htgs <- dat$fthg[game]
            atgs <- dat$ftag[game]
            
            htgcd <- dat$ftag[game]
            atgcd <- dat$fthg[game]
            
            htshots <- dat$hs[game]
            atshots <- dat$as[game]
            
            htshotstarget <- dat$hst[game]
            atshotstarget <- dat$ast[game]
            
            htfouls <- dat$hf[game]
            atfouls <- dat$af[game]
            
            htcorners <- dat$hc[game]
            atcorners <- dat$ac[game]
            
            ###########
            # points
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
            
            ###########
            # goals
            previous_goalscored_h <- table %>%
                filter(team == ht) %>%
                filter(gw == week - 1) %>%
                select(goalscored) %>%
                pull()
            
            previous_goalscored_a <- table %>%
                filter(team == at) %>%
                filter(gw == week - 1) %>%
                select(goalscored) %>%
                pull()
            
            ###########
            # goals conceded
            previous_goalconceded_h <- table %>%
                filter(team == ht) %>%
                filter(gw == week - 1) %>%
                select(goalconceded) %>%
                pull()
            
            previous_goalconceded_a <- table %>%
                filter(team == at) %>%
                filter(gw == week - 1) %>%
                select(goalconceded) %>%
                pull()
            
            ###########
            # shots
            previous_shots_h <- table %>%
                filter(team == ht) %>%
                filter(gw == week - 1) %>%
                select(shots) %>%
                pull()
            
            previous_shots_a <- table %>%
                filter(team == at) %>%
                filter(gw == week - 1) %>%
                select(shots) %>%
                pull()
            ###########
            # shots target
            previous_shotstarget_h <- table %>%
                filter(team == ht) %>%
                filter(gw == week - 1) %>%
                select(shotstarget) %>%
                pull()
            
            previous_shotstarget_a <- table %>%
                filter(team == at) %>%
                filter(gw == week - 1) %>%
                select(shotstarget) %>%
                pull()
            ###########
            # fouls
            previous_fouls_h <- table %>%
                filter(team == ht) %>%
                filter(gw == week - 1) %>%
                select(fouls) %>%
                pull()
            
            previous_fouls_a <- table %>%
                filter(team == at) %>%
                filter(gw == week - 1) %>%
                select(fouls) %>%
                pull()
            ###########
            # corners
            previous_corners_h <- table %>%
                filter(team == ht) %>%
                filter(gw == week - 1) %>%
                select(corners) %>%
                pull()
            
            previous_corners_a <- table %>%
                filter(team == at) %>%
                filter(gw == week - 1) %>%
                select(corners) %>%
                pull()
            ###########
            
            tab$points[tab$team == ht & tab$gw == week] <- htp + previous_hp
            tab$points[tab$team == at & tab$gw == week] <- atp + previous_ap
            
            tab$goalscored[tab$team == ht & tab$gw == week] <- htgs + previous_goalscored_h
            tab$goalscored[tab$team == at & tab$gw == week] <- atgs + previous_goalscored_a
            
            tab$goalconceded[tab$team == ht & tab$gw == week] <- htgcd + previous_goalconceded_h
            tab$goalconceded[tab$team == at & tab$gw == week] <- atgcd + previous_goalconceded_a
            
            tab$shots[tab$team == ht & tab$gw == week] <- htshots + previous_shots_h
            tab$shots[tab$team == at & tab$gw == week] <- atshots + previous_shots_a
            
            tab$shotstarget[tab$team == ht & tab$gw == week] <- htshotstarget + previous_shotstarget_h
            tab$shotstarget[tab$team == at & tab$gw == week] <- atshotstarget + previous_shotstarget_a
            
            tab$fouls[tab$team == ht & tab$gw == week] <- htfouls + previous_fouls_h
            tab$fouls[tab$team == at & tab$gw == week] <- atfouls + previous_fouls_a
            
            tab$corners[tab$team == ht & tab$gw == week] <- htcorners + previous_corners_h
            tab$corners[tab$team == at & tab$gw == week] <- atcorners + previous_corners_a
        }
        
        if (sum(tab$gw) > 0) {
            position <- 1:20
            pos_dat <- tab %>% arrange(desc(points), as.character(team))
            
            pos_dat$position <- position
            
            tab_data <- pos_dat
        }
        
        table <- rbind(table, tab_data)
    }

    return(table)
}

.ParseDates <- function(data) {
    data <- data %>%
        mutate(timestamp = lubridate::dmy(Date))

    return(data)
}

# all years from 2001 - 2020
years_of_data <- 1:20
# remove the 2004/2005 season (some issue)
# remove the 2003/2004 season (some issue)
# will fix later
# pad the string.... just to be sure
years_of_data <- str_pad(years_of_data, width = 2, side = "left", pad = 0)
# rbind all data
df <- map_dfr(years_of_data, WrangleData)

# save the data to disk
write_csv(df, ".\\data\\csv\\premierleague_data.csv")
