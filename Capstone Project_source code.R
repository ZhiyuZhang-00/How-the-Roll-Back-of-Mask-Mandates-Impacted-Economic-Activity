library(xlsx)
library(tidyverse)
library(stargazer)

setwd("D:/DPSS/Capstone/task5")

retail <- read.csv("retail_data_export_w_fips.csv")
vote2020 <- read.xlsx("vote2020.xlsx",1)
vaccine <- read.xlsx("county_week26_data_fixed.xlsx",1)

### Time series Plot of average foot traffic (by day) in Walmart stores during the 28 day period

task2 <- retail %>%
  group_by(dayofmonth) %>%
  summarise(mean_visit = mean(daily_visitors, na.rm = T))

plot1 <- ggplot(data = task2) +
  geom_line(aes(x = dayofmonth, y=mean_visit))+
  geom_vline(aes(xintercept=18), color="#E22828",  linetype = "dashed") +
  scale_x_continuous(limits = c(4, 31), breaks = c(4, 11, 18, 24, 31)) +
  theme_bw() + 
  theme(panel.grid=element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  labs(title = "Daily Average Visit in Walmart Stores Before and After the Policy Change")  +
  xlab("Date of May") + 
  ylab("Daily Average Visit")

plot1

### Time series Plot of average foot traffic (by day) for each state where a Walmart is located

task3 <- retail %>%
  group_by(dayofmonth, STATEFP) %>%
  summarise(state_visit = mean(daily_visitors, na.rm = T)) %>%
  left_join(task2, by = 'dayofmonth')

plot2 <- ggplot(task3) +
  geom_line(aes(x = dayofmonth, y=state_visit, group=STATEFP), color = "#278BDE", size = 0.1, alpha = 0.3)+
  geom_line(aes(x = dayofmonth, y=mean_visit), color = "#E22828", size = 0.1, alpha = 0.8)+ 
  geom_vline(aes(xintercept=18), color="#E22828",  linetype = "dashed") +
  geom_text(data = data.frame(x = 31, y = 160), aes( x=x, y=y, label = "Mean"), size = 4, color = "#E22828")+
  scale_x_continuous(limits = c(4, 31), breaks = c(4, 11, 18, 24, 31))+
  theme_bw() + 
  theme(panel.grid=element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  labs(title = "Daily Average Visit in Walmart Stores for Each State Before and After the Policy Change")  +
  xlab("Date of May") + 
  ylab("Daily Average Visit")
plot2

# Create a new variable called post. Set this variable equal to 0 before the policy change and equal to 1 after the policy change. And create a new variable to identify the day of week.

task4 <- retail %>%
  mutate(post = ifelse(dayofmonth >= 18, 1, 0)) %>%
  mutate(dayofweek = case_when(
    dayofmonth == 10 | dayofmonth == 17 | dayofmonth == 24 | dayofmonth == 31   ~ "Mon",
    dayofmonth == 4 | dayofmonth == 11 | dayofmonth == 18 | dayofmonth == 25   ~ "Tue",
    dayofmonth == 5 | dayofmonth == 12 | dayofmonth == 19 | dayofmonth == 26   ~ "Wed",
    dayofmonth == 6 | dayofmonth == 13 | dayofmonth == 20 | dayofmonth == 27   ~ "Thu",
    dayofmonth == 7 | dayofmonth == 14 | dayofmonth == 21 | dayofmonth == 28   ~ "Fri",
    dayofmonth == 8 | dayofmonth == 15 | dayofmonth == 22 | dayofmonth == 29   ~ "Sat",
    dayofmonth == 9 | dayofmonth == 16 | dayofmonth == 23 | dayofmonth == 30   ~ "Sun")) %>%
  filter(!is.na(dayofweek), !is.na(daily_visitors), )

# basic linear model
model1 <- lm(daily_visitors ~ post, data = task4)

# with fixed effects of states
model2 <- lm(
  daily_visitors ~ post + factor(STATEFP),
  data = task4)

# with fixed effects of day of week
model3 <- lm(
  daily_visitors ~ post + factor(dayofweek),
  data = task4)

# with fixed effects of both states and day of week
model4 <- lm(
  daily_visitors ~ post + factor(dayofweek) + factor(STATEFP),
  data = task4)


stargazer(model1, model2, model3, model4, title = "results", align = F, type = "text", omit = "factor(dayofweek)Mon", no.space = TRUE, out = "task4.html")

# Using the county fips code for each store, use a join to merge information about Republican vote share in 2020 and vaccine hesitancy. Now run three regressions. 
# Use the same dependent variable except for regression (1) use post and post ¡Á trump_vs. 
# For (2) use post and post ¡Á vaccine_hes. 
# For (3) use post, post ¡Á trump_vs, and post ¡Á vaccine_hes.

visit <- retail %>%
  mutate(post = ifelse(dayofmonth >= 18, 1 , 0)) %>%
  mutate(foot_traffic = daily_visitors) %>%
  filter(!is.na(foot_traffic),
         dayofmonth >= 4) %>%
  select(fips, post, foot_traffic, STATEFP)

vac <- vaccine %>%
  mutate(fips = FIPS.Code) %>%
  mutate(vaccine_hes = Estimated.hesitant) %>%
  select(fips, vaccine_hes)

vote <- vote2020 %>%
  mutate(fips = county_fips) %>%
  mutate(trump_vs = per_gop) %>%
  select(fips, trump_vs)

task5 <- visit %>%
  left_join(vac, by = "fips") %>%
  left_join(vote, by = "fips") 

model1 <- lm(
  foot_traffic ~ post + post:trump_vs + factor(STATEFP),
  data = task5)
summary(model1)
stargazer(model1, title = "results", align = F, type = "text", no.space = TRUE, out = "m1.html")

model2 <- lm(
  foot_traffic ~ post + post:vaccine_hes + factor(STATEFP),
  data = task5)
summary(model2)
stargazer(model2, title = "results", align = F, type = "text", no.space = TRUE, out = "m2.html")

model3 <- lm(
  foot_traffic ~ post + post:trump_vs + post:vaccine_hes + factor(STATEFP),
  data = task5)
summary(model3)
stargazer(model3, title = "results", align = F, type = "text", no.space = TRUE, out = "m3.html")

stargazer(model1, model2, model3, title = "results", align = F, type = "text", no.space = TRUE, out = "model.html")
