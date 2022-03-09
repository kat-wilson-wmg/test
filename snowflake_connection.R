# install.packages(c("DBI", "dplyr","dbplyr","odbc"))
library(DBI)
library(dplyr)
library(dbplyr)
library(odbc)
# install.packages("RJDBC")
library(RJDBC)
library(rJava)


jdbcDriver <- JDBC(
  driverClass = "net.snowflake.client.jdbc.SnowflakeDriver",
  classPath = "snowflake-jdbc-3.9.0.jar"
) # ← give path to where you downloaded the jar file

# create a connection, with Okta SSO
con <- dbConnect(
  jdbcDriver,
  "jdbc:snowflake://wmg-datalab.snowflakecomputing.com/?authenticator=https://wmg.okta.com/",
  "kat.wilson@wmg.com",
  "2R@nd0mW0rds")
mydata <- DBI::dbGetQuery(myconn,"SELECT * FROM EMP")
head(mydata)