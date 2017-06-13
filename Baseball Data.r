
library('RSQLite')

db_conn <- dbConnect(SQLite(), dbname="baseball.sqlite")
dbListTables(conn = db_conn)

# Using the aggregate function
salaries <- dbReadTable(conn=db_conn, "Salaries")
salary2010 <-salaries[ salaries$yearID == 2010, ]
payroll <- aggregate(salary ~ teamID,salary2010, sum)

# Order by payroll descending
ordered <- payroll[order(-payroll$salary),]
head(ordered)

# library('ggplot2')
# # Very basic bar graph
# print("Now let's see a barplot of these salaries")
# ggplot(data=ordered[1:5,], aes(x=teamID, y=salary, label=salary)) +
#     geom_bar(stat="identity")

query <-"SELECT teamID, 
                SUM(Salary) as payroll 
        FROM Salaries 
        WHERE yearID = 2010
        GROUP BY teamID 
        ORDER BY payroll desc 
        LIMIT 6"
salaries <- dbGetQuery(conn=db_conn, query)
salaries

dbListFields(conn=db_conn, "Salaries")

query <-"SELECT teamID, yearID,
                SUM(Salary) as payroll 
        FROM Salaries 
        GROUP BY teamID, yearID 
        ORDER BY payroll desc"
salaries <- dbGetQuery(conn=db_conn, query)

library("reshape2")
dcast(salaries, yearID ~ teamID, value.var = "payroll")

library(ggplot2)

# Read in the CPIAUCSL 
cpi <- read.csv('./CPIAUCSL.csv', header = TRUE, sep = ",")

# Filter to after 1985
cpi <- cpi[as.Date(cpi$DATE) >= as.Date("1985-01-01"), ]

# Filter to January
cpi <- cpi[months(as.Date(cpi$DATE)) == "January", ] 

ggplot(data=cpi, aes(x=DATE, y=CPIAUCSL, group=1)) + geom_line(color="red")

library(lubridate)

cpi$yearID <- year(cpi$DATE)
salaries <- merge(salaries,cpi[,c('CPIAUCSL','yearID')],by="yearID")

latest_cpi <- cpi[nrow(cpi),'CPIAUCSL']

salaries$adjusted_2017 <- salaries$payroll*latest_cpi/salaries$CPIAUCSL
ggplot(salaries, aes(x=yearID, y=adjusted_2017)) + geom_line() + facet_wrap(~teamID)

salaries$adjusted_log <- log10(salaries$adjusted_2017)
ggplot(salaries, aes(x=yearID, y=adjusted_log)) + geom_line() + facet_wrap(~teamID)

ggplot(salaries, aes(x=yearID, y=adjusted_2017)) + geom_line(aes(group = teamID, color= teamID))

query <-"SELECT CAST(W AS FLOAT)/G  AS WIN_PCT,
        yearID,
        teamID
        FROM Teams 
        GROUP BY yearID, teamID"
performance <- dbGetQuery(conn=db_conn, query)
combined = merge(performance, salaries, by=c("yearID","teamID"))
ggplot(combined, aes(x=adjusted_log, y=WIN_PCT)) + geom_point(color='red', size=2, alpha=0.2) + facet_wrap(~teamID)


ggplot(combined, aes(x=payroll, y=WIN_PCT)) + geom_point(color='red', size=2, alpha=0.2)
