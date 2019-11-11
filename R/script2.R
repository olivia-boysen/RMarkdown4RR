
# read csv file (data)
olympic <- read.csv("data/athlete_events.csv")
olympic[1:5,]

##Setting up Working Environment 💡
# Install necessary packages you will be working with!

# install.packages("dplyr", repos = "http://cran.us.r-project.org")
# install.packages("ggplot2", repos = "http://cran.us.r-project.org")
# install.packages("gapminder", repos = "http://cran.us.r-project.org")
# install.packages("DT", repos = "http://cran.us.r-project.org")

# check your data
dim(olympic)
head(olympic, n = 3)

# check the structure of your data
str(olympic) 

# upload package dplyr
# take another look at the data
suppressPackageStartupMessages(library(dplyr))
glimpse(olympic) 


## Select your variables 
# Use `olympic df` to select the variable(s)
#
# 1) that ends with letter `t`
# 
# 2) starts with letter `S`. Try to do this selection using base R.  

##Solutions:

end_t <- select(olympic, ends_with("t"))
head(end_t, n = 1)
beg_S <- select(olympic, starts_with("S"))
head(beg_S, n = 1)

# using base R:
beg_S_base <- olympic[c("Sex", "Season", "Sport")]
head(beg_S_base, n = 1) 

# mutate(): create BMI variable
olympic <- mutate(olympic, BMI = Weight / (Height/100)^2) 
head(olympic, n = 1)

## Filter your data:
#  
# Use `olympic df` to filter:
#  
# 1) only Serbian teams and save it as olympicSR
#
# 2) only Serbian teams from 2000 onward and save it as olympicSR21c
#
# 3) athletes whos wight is bigger then 100kg and height is over 2m.

## Solutions:

olympicSR <- filter(olympic, Team == "Serbia") 
dim(olympicSR)

olympicSR21c <- filter(olympicSR,  Year >= 2000)
dim(olympicSR21c)

big_athlete <- filter(olympic, Weight > 100 & Height > 200)
dim(big_athlete)

# arrange()
## Arranging your data
# 1) Arrange Serbian athletes in `olympicSR21c` `df` by `Height` in ascending and descending order.
#
# 2) Using `olympicSR df`

## Solution 1):

olympicSR21c_hs <- arrange(olympicSR21c, Height)
head(olympicSR21c_hs, 2)
olympicSR21c_ht <- arrange(olympicSR21c, desc(Height)) 
head(olympicSR21c_ht, 2)

## Solution 2):

head(arrange(olympicSR, Age), 5)

head(arrange(olympicSR, desc(Weight)), 5)


##Collapse many values down to a single summary: summarise()

# Use `summarise()`:
#  
# 1) to print out a summary of `olypicSR` `df` containing two variables: max_Age and max_BMI.
#
# 2) to print out a summary of `olypicSR` `df` containing two variables: mean_Age and mean_BMI.

## Solution: Summarise your data
summarise(olympicSR, max_Age = max(Age), max_BMI = max(BMI))

summarise(olympicSR, mean_Age = mean(Age), mean_BMI = mean(BMI))

## Let's `%>%` all up!

# Do you know what this code does?
olympicSR_pipe <- olympic %>%
  filter(Team == "Serbia" & Year > 2000) %>%
  mutate(BMI = Weight / (Height/100)^2)

plot(olympicSR_pipe$Age, olympicSR_pipe$Height, cex = 0.5, col = "red")

# ggplot()
library(ggplot2)
ggplot(olympicSR_pipe, aes(x = Age, y = Height)) +
  geom_point(col ="red")

# ggplot() gallery
# Run the following code to see what graphs it's going to produce.
       
       ggplot(data = olympic, mapping = aes(x = Height), binwidth = 10) +
       geom_histogram()
#
       ggplot(data = olympic, mapping = aes(x = Height)) +
       geom_density()
#
       ggplot(data = olympic, mapping = aes(x = Season, color = Sex)) +
       geom_bar()
#
       ggplot(data = olympic, mapping = aes(x = Sex, fill = Season)) +
       geom_bar()
       
##Confer with your neighbours:
# regression model
m1 <- lm(olympic$BMI ~ olympic$Age)
#
summary(m1)

## Your turn!
#       
#       Use `olympic` data.
#       
#       **Does the Weight depend upon Age?**
#       
#       1) Data set is big, hence let us use a sample of 10,000 athletes (tip: `sample_n(df, n)`)
#       
#       2) Produce a scattep plot: what does it tell you?
#       
#       3) Fit a regression model: is there a relationship? How strong is it?
#       Is the relationship linear? What conclusion(s) can you draw?
#       
#       4) What are the other questions you could ask; could you provide the answers to them?
#     
## Possible Solution Q1 & Q2: sample and scatter plot
       
       sam_olymp <- sample_n(olympic, 10000) 
       ggplot(sam_olymp, aes(x = Age, y = Weight)) +
       geom_point(alpha = 0.2, shape = 21, fill = "blue", colour="black", size = 5) +
       geom_smooth(method = "lm", se = F, col = "maroon3") +
       geom_smooth(method = "loess", se = F, col = "limegreen") 
    
## Possible Solution Q3: simple regression model

       my.model <- lm(sam_olymp$Weight ~ sam_olymp$Age)
       summary(my.model)

## Adding layers to your <span style="color:blue">`ggplot()`</span>

## Voila
       ggplot(sam_olymp, aes(x = Age, y = Weight, col = "red")) +
       geom_point(alpha = 0.2, shape = 21, fill = "blue", colour="black", size = 5) +
       geom_smooth(method = "lm", se = F, col = "maroon3") +
       geom_smooth(method = "loess", se = F, col = "limegreen") +
       labs (title= "Age vs Weight", 
       x = "Age", y = "Weight") +
       theme(legend.position = "none", 
       panel.border = element_rect(fill = NA, 
       colour = "black",
       size = .75),
       plot.title=element_text(hjust=0.5)) +
       geom_text(x = 80000, y = 125, label = "regression line", col = "maroon3") +
       geom_text(x = 90000, y = 75, label = "smooth line", col = "limegreen")

## **There is a challenge:** 
#       
#       - `dplyr`'s `group_by()` function enables you to group your data. It allows you to create a separate df that splits the original df by a variable.
#       - `datatable()` from `DT` package enables you to display as table on HTML page an R data object that could be filtered, arranged etc.
#       - `boxplot()` function produces boxplot(s) of the given (grouped) values.
       
# Knowing about `group_by()` and `DT::datatable()` functions, coud we find out number of medals per each team?
        
       olympic %>% 
         filter(!is.na(Medal)) %>% 
         group_by(Team, Medal) %>% 
         summarize(cases=n()) %>% 
         DT::datatable()
       
# Could you find the number of medals per each team for the last Rio games?

       olympic_med <- olympic %>% 
         filter(!is.na(Medal)) %>% 
         group_by(Team, Medal) %>% 
         summarize(cases=n()) 
    
  dt1 <- DT::datatable(olympic_med)     
  
# First we would need to get the data we want to be presented on a graph.

       exyu <- olympic %>% 
         filter(Team %in% c("Bosnia and Herzegovina", "Croatia", "Serbia", "Serbia and Montenegro", "Montenegro", "Slovenia")) %>% 
         group_by(Team, Sex) %>% 
         summarize(total = n()) #<< 
 
   dt2 <- DT::datatable(exyu)

# How do we plot this? �
# we need a bar chart with each team on the x axis and number of male and female athlethes on the y axis.
 my_plot <- ggplot(data = exyu, aes(x = Team, y = total, fill = Sex)) +
         geom_bar(stat="identity",  position="dodge", col = "black") +
         # to make it read easier we will flip x & y coordinates
         coord_flip() +
         # we will add description for x and y axies and title and subtitle  
         labs(x="ex YU country", y="No of athletes", 
              title = "Comparisons of M and F representatives in exYU Teams",
              subtitle = "for klikR workshop",
              caption = "Data from: kaggle - 120 years of Olympic history") +
         # add the border on the graph  
         theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) +
         #remove the grid lines
         theme(plot.title = element_text(size = 14, vjust = 2),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), 
               axis.line = element_blank())
     

       
