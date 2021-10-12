
library(ggplot2)
library(lubridate) 
anytime::anydate(D)



ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

str(ny)

head(wash)

str(wash)

head(chi)

str(chi)

wash$Gender <- NA
wash$Birth.Year <- NA
wash$City <- 'Washington'
ny$City <- 'New York'
chi$City <- 'Chicago'

concatenation <- function(d1, d2)
{
  return(rbind(d1, d2))
}

df <- concatenation(ny,wash)   #df <- rbind(ny, wash)
df <- concatenation(df,chi)    #df <- rbind(df, chi)
head(df)

# Count of users in City
total_df = sort(table(df$df))
print(total_df)

# percentage of users in City
round((total_df / sum(total_df) * 100), digits = 2)

# Visualizing data with ggplot
ggplot(aes(x = City, y = Trip.Duration), data = df) +
    geom_bar(position = 'dodge', stat = "summary", fun.y = "mean", fill = "yellow", colour="black") + 
    ggtitle('The average travel time for users in different cities') +
    labs(y = 'Average Trip Duration', x = 'City') +
    coord_flip()

my.summary <- with(df, aggregate(list(Trip.Duration), by = list(City), 
                   FUN = function(x) { mon.mean = mean(x, na.rm = TRUE) } ))

colnames(my.summary) <- c('City', 'Average.Trip.Duration')
my.summary

From the results shown, Washington has more average travel duration of users , followed by
Chicago and lastly New York.These also could be influenced by seasonal changes. people travel more when it is summer 

# Creating new data frame by binding 'New York City' and 'Chicago' data
# washington is omitted due to the lack of information (birth year and Gender)

df2 <- concatenation(chi,ny)      #city2 <- rbind(chi, ny)

# Count of Gender (Male and Female)
Gen_total = sort(table(df2$Gender))
print(Gen_total)

# percentage of Gender (Male and Female)
round((Gen_total / length(df2$Gender) * 100), digits = 2)



# Visualizing data with ggplot
ggplot(aes(x = Gender, fill = City), data = df2) +
    geom_bar(position = 'dodge', colour="black") +
    ggtitle('Counts of each gender') +
    scale_x_discrete(labels = c('Not mentioned', 'Female', 'Male')) +
    labs(y = 'Number of Riders', x = 'Gender') +
    scale_fill_manual("legend", values = c("Chicago" = "green", "New York" = "yellow"))

# Count of Gender(Male and Female) in Chicago
total_chi = sort(table(df2$Gender[df2$City == 'Chicago']))
print(total_chi)

# percentage of Gender(Male and Female) in Chicago
round((total_chi / length(df2$Gender[df2$City == 'Chicago']) * 100), digits = 2)

# Count of Gender(Male and Female) in New York City
total_ny = sort(table(df2$Gender[df2$City == 'New York']))
print(total_ny)

# percentage of Gender(Male and Female) in Chicago
round((total_ny / length(df2$Gender[df2$City == 'New York']) * 100), digits = 2)

# Re-formatting of date columns
df$Start.Time <- ymd_hms(df$Start.Time)
df$End.Time <- ymd_hms(df$End.Time)

# Creating new column 'Month' extracting from Start.Time
df$Month <- month(df$Start.Time)

# Visualizing data with ggplot
ggplot(aes(x = Month, fill = City), data = df) +
    geom_bar(position = 'dodge', colour="black") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6), labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
    ggtitle('Number of Rides in different Months') +
    labs(y = 'Number of Rides', x = 'Month') +
    scale_fill_manual("legend", values = c("Chicago" = "black", "New York" = "grey", "Washington" = "yellow"))

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

crosstab(df, row.vars = "Month")

# Count of users per month by grouped by cities
crosstab(df, row.vars = "Month", col.vars = "City")

# Percentage of users per month by grouped by cities
crosstab(df, row.vars = "Month", col.vars = "City", type = "r")

system('python -m nbconvert Explore_bikeshare_data.ipynb')
