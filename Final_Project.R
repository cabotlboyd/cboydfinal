# To load in our .csv, we need to load in the readr package
library (readr)
library (tidyverse)
library (dslabs)
# Now we will pull our data from github and make our data set under the name
# 'baseball'
urlfile="https://vincentarelbundock.github.io/Rdatasets/csv/plyr/baseball.csv"
baseball<-read_csv(url(urlfile))

head(baseball)
str(baseball)

# We need to compare a few offensive stats
# The first is batting average. Because there is no batting average variable in
# this data set, we are going to have to create one using the mutate function

avg <- baseball %>% filter(id == "sheffga01" |  id == "griffke02") %>% 
  mutate(ba = (h)/ab) %>% select(id,year,h,ab,ba)
# This creates a data frame that filters out the two players we want to examine
# and create a new column that creates the batting average statistic
batavg <- avg %>% ggplot() + geom_line(aes(year,ba,color=id),size=1.2) + 
  labs(title = "Career Batting Averages", 
       subtitle = "Ken Griffey Jr. v. Gary Sheffield", x = "Year", 
       y = "Batting Average") + 
  scale_colour_discrete(name = "Players",labels = c("Griffey Jr.","Sheffield")) +
  geom_hline(yintercept = 0.25) + 
  geom_text(aes(1999,0.245),label = "League Average",size = 4)
batavg


# Now we will compare Sheffield's home run numbers to another HOFer and teammate,
# Tony Gwynn. 20+ home runs in a season would be considered elite.

hits <- baseball %>% filter(id == "sheffga01" | id == "gwynnto01") %>% 
  select(id,year,h,X2b,X3b,hr)
# This creates a data frame that filters out just the two players we want to 
# examine
homeruns <- ggplot(hits,aes(fill=id, y=hr, x=year)) + 
  geom_bar(position="identity", stat = "identity", alpha = 0.6) + 
  labs(title = "Career Home Runs", subtitle = "Gary Sheffield v. Tony Gwynn", 
       x = "Year", y = "Homeruns") + geom_hline(yintercept = 20) +
  geom_text(aes(1985,22),label = "Elite Level",size = 4)
homeruns
# How to change title of legend in this plot????

# Finally we will look at OPS, or On base Plus Slugging, which represents the 
# offensive production of a player. An elite level for OPS is 0.450+. We will
# compare Sheffield to HOFer Rickey Henderson.

plus <- baseball %>% mutate(ops=((h+X2b+X3b+hr+bb+hbp)/(ab+hbp+sf))) %>% 
  filter(id == "sheffga01" | id == "henderi01")
# We have to establish a data frame that adds a column for OPS and filter for
# the two players we are looking at
onbaseps <- plus %>% ggplot() + geom_line(aes(year,ops,color=id),size=1.2) +
  labs(title = "Career Onbase Plus Slugging", 
       subtitle = "Gary Sheffield v. Rickey Henderson", x = "Year", y = "OPS") +
  scale_colour_discrete(name = "Players",labels = c("Henderson","Sheffield")) +
  geom_hline(yintercept = 0.450) + 
  geom_text(aes(1985,0.465),label = "Elite Level",size = 4)
onbaseps

