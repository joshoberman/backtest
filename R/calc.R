library(base64enc)
library(jsonlite)
library(data.table)
library(vars)

r_script_dir <- "R"
keep_backtest_data <- F

load(paste(r_script_dir,"runCalc.RData",sep="/"))
res<-runCalc(local_dir = r_script_dir,base64Json  = base64outputJson, keep_backtest = keep_backtest_data)

rm(list="base64outputJson")
OUTPUT<-list("resultJson"=res)
