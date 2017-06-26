## AEA_DB setup:

library(RMySQL)

# system("mysql.server start")
# system("mysql -u root -p")   # Then type: qwertz

# Create a database with the right columns:
  con = dbConnect(MySQL(), host = "localhost", 
                  user = 'root', password = "qwertz")
  dbSendQuery(con, "CREATE DATABASE basecamp;")
  
  dbSendQuery(con, "USE basecamp")
  
  dbSendQuery(con, "CREATE TABLE aea_links (
              art_id VARCHAR(100),
              art_url VARCHAR(150));")
  dbSendQuery(con, "ALTER TABLE aea_links
              ADD PRIMARY KEY (art_id);")


  dbSendQuery(con, "CREATE TABLE abstract_set (
              art_id VARCHAR(100),
              abstract VARCHAR(3000),
              labels VARCHAR(30));")
  dbSendQuery(con, "ALTER TABLE abstract_set
              ADD PRIMARY KEY (art_id);")
  
  dbSendQuery(con, "CREATE TABLE aea_links_ext (
              oid VARCHAR(50),
              art_links VARCHAR(100));")
  dbSendQuery(con, "ALTER TABLE aea_links_ext 
              ADD PRIMARY KEY (oid);")
  