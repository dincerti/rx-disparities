
# SET UP -----------------------------------------------------------------------
con <- dbConnect(MySQL(), dbname="meps", host="localhost",
                 default.file = "C:/Program Files/MySQL/MySQL Server 5.5/my.ini")

# LOAD DATA --------------------------------------------------------------------
# meps files
fyc <- data.table(dbGetQuery(conn = con, "SELECT * FROM fyc"))
pm <- data.table(dbGetQuery(conn = con, "SELECT * FROM pm"))
mc <- data.table(dbGetQuery(conn = con, "SELECT * FROM mc"))
vartsl <- data.table(dbGetQuery(conn = con, "SELECT dupersid, panel, stra9612, psu9612
                    FROM vartsl"))

# bls cpi data
cpi <- read.csv("data/bls_cpi_urban1996-2014.csv")

# rxcat lookup
lookup.rxcat <- read.csv("data/lookup_rxcat.csv")