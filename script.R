setwd("/Users/Carl/Google Drive/women soccer")
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(stringr)
library(readxl)
# library(caret)
# library(doMC)

source("functions.R")

soccer = read.csv("data/wsoccer.csv")
injury = read_excel("data/injury.xlsx", col_names = TRUE)[, -1]
injury = mutate(injury, `Injury Date` = ymd(`Injury Date`))

soccer = mutate(soccer, Date = mdy(Date), Start.Time = hms(Start.Time),
                End.Time = hms(End.Time), week = ifelse(week(Date) > 30, week(Date) - 30, week(Date) - 30 + 52),
                Activity.Length = seconds(End.Time - Start.Time))
soccer = filter(soccer, week <= 16)
names = unique(soccer$Player.Name)
dlist = list()
for (name in names) {
  dlist[[name]] = filter(soccer, Player.Name == name)
}

session = list() # obs with period name "session" are sums of detailed data
detail = list()
for (name in names) {
  session[[name]] = filter(dlist[[name]], Period.Name == "Session")
  detail[[name]] = filter(dlist[[name]], Period.Name != "Session")
}

weekly.load = list()
for (name in names) {
  weekly.load[[name]] = session[[name]] %>% right_join(data.frame(week = 1:16), by = "week") %>%
    group_by(week) %>% summarise(Total.Load = sum(Total.Player.Load)) %>%
    replace_na(replace = list(Total.Load = 0))
}

daily.load = list()
for (name in names) {
  daily.load[[name]] = session[[name]] %>% right_join(data.frame(Date = seq(ymd("2016-08-03"), ymd("2016-11-12"), by = 1)), by = "Date") %>%
    group_by(Date) %>% summarise(Total.Load = sum(Total.Player.Load), Length = sum(Activity.Length) / 60, IMA = sum(Total.IMA), IMA.High = sum(High.IMA)) %>%
    mutate(load.per.minite = Total.Load / Length) %>%
    replace_na(replace = list(Total.Load = 0, Length = 0, IMA = 0, IMA.High = 0, load.per.minite = 0))
}

p = lapply(1:length(names), function(i) {
  ggplot(weekly.load[[i]], aes(week, Total.Load)) + geom_line() +
    theme(axis.title = element_blank())
})

# do.call("grid.arrange", args = c(p, ncol = 4))

records = split(injury, seq(nrow(injury)))

pre.injury.load.1 = bind_rows(lapply(records, recent_load))
pre.injury.load.2 = bind_rows(lapply(records, function(lis) recent_load(lis, period = 14)))
pre.injury.load.4 = bind_rows(lapply(records, function(lis) recent_load(lis, period = 28)))

injured.names = unique(injury$`Student Name`)

q = lapply(injured.names, function(name) {
  load_plot(name)
})
# do.call("grid.arrange", args = c(q, ncol = 5))

dat.class = prep_data(7, 14)

# boxplot
p.box = lapply(names(dat.class)[-ncol(dat.class)], function(var) {
  ggplot(dat.class, aes(injured, dat.class[[var]], color = injured)) + geom_boxplot() +
    theme(legend.position = "none", axis.title = element_blank()) + ggtitle(var)
})
# do.call("grid.arrange", args = c(p.box, ncol = ceiling(sqrt(ncol(dat.class) - 1))))
# no significant difference between injured and not injured players

# models
# grid = expand.grid(n.trees = (1:6) * 50, interaction.depth = 1:6, shrinkage = 0.025 * (1:4), n.minobsinnode = c(2, 5, 10))
# model.gbm = train_wrap(dat.class, "gbm", sampling = "smote", cv = TRUE, numcore = 4, tuneGrid = grid)
# grid = expand.grid(iter = (1:6) * 50, maxdepth = 1:6, nu = 0.025 * (1:4))
# model.ada = train_wrap(dat.class, "ada", sampling = "smote", cv = TRUE, tuneGrid = grid)
# 
# 
# model.logis = train_wrap(dat.class, "glm", sampling = "smote", cv = TRUE, numcore = 4, family = "binomial")

# 17 16 15
# load_plot("WS16")
