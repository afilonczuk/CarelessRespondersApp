#install.packages("rsconnect")
library(rsconnect)
rsconnect::setAccountInfo(name='careless-statistics',
                          token='068E0A5E253EF4A266E25CC89DD04396',
                          secret='<SECRET>')


rsconnect::deployApp('/Users/audreyf/Desktop/CarelessStatistics')

