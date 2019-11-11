###Setting up Working Environment 💡
# Install and load necessary packages you will be working with!

# install.packages("dplyr", repos = "http://cran.us.r-project.org")
# install.packages("ggplot2", repos = "http://cran.us.r-project.org")
# install.packages("DT", repos = "http://cran.us.r-project.org")

library(dplyr)
library(ggplot2)

# read csv file (data)
exyu <- read.csv("data/exyu_olympic.csv")

# Find the number of medals per each team? 

exyu %>% 
  filter(!is.na(Medal)) %>% 
  group_by(Team, Medal) %>% 
  summarize(cases=n()) %>% 
  DT::datatable()

# Find the number of medals per each team for the last Rio games?

exyu %>% 
  filter(!is.na(Medal) & Year == 2016) %>% 
  group_by(Team, Medal) %>% 
  summarize(cases=n()) %>% 
  DT::datatable()

# Visualise data about number of female and male athletes 
# from ex YU countries available in the data set:
# 1) identify data to be poloted
# 2) visualise data

# 1: identify data

exyu_mf <- exyu %>% 
  group_by(Team, Sex) %>% 
  summarize(total = n()) #<< 
exyu_mf

# 2: How do we plot this? 
# we need a bar chart with each team on the x axis 
# and na umber of male and female athlethes on the y axis.

ggplot(data = exyu_mf, aes(x = Team, y = total, fill = Sex)) +
  geom_bar(stat="identity",  position="dodge", col = "black") +
  # to make it read easier flip x & y coordinates
  coord_flip() +
  # add description for x and y axies and title and subtitle  
  labs(x="ex YU country", y="No of athletes", 
       title = "Comparisons of M and F representatives in exYU Teams",
       subtitle = "for klikR workshop",
       caption = "Data from: kaggle - 120 years of Olympic history") +
  # add the border on the graph  
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) +
  # remove the grid lines
  theme(plot.title = element_text(size = 14, vjust = 2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank())