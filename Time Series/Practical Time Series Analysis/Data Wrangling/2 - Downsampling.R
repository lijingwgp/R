require(zoo)
require(data.table)

unemp <- fread("data/UNRATE.csv")
unemp[, DATE := as.Date(DATE)]
setkey(unemp, DATE)

## downsampling - focus on a particular portion
unemp[seq.int(from = 1, to = nrow(unemp), by = 12)]

## downsampling - aggregation
unemp[, mean(UNRATE), by=format(DATE, '%Y')]

## upsampling - irregular time series
rand.unemp.idx <- sample(1:nrow(unemp), .1*nrow(unemp))
rand.unemp <- unemp[-rand.unemp.idx]

all.dates <- seq(from=unemp$DATE[1], to=tail(unemp$DATE, 1), by='months')
rand.unemp = rand.unemp[J(all.dates), roll=0]
rand.unemp[, rpt := is.na(UNRATE)]