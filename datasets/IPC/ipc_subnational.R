required.packages <- c("data.table","jsonlite","httr", "rstudioapi")
lapply(required.packages, require, character.only=T)

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")
setwd("..")

isos <- fread("datasets/Countrynames/isos.csv", encoding = "UTF-8", na.strings = "")

## Food insecurity ##
ipc <- data.table(fromJSON("https://fsr2av3qi2.execute-api.us-east-1.amazonaws.com/ch/country?key=bac2a4d1-1274-4526-9065-0502ce9d4d5e"))
ipc[, date := (as.Date(paste0("01 ", to), "%d %b %Y")-as.Date(paste0("01 ", from), "%d %b %Y"))/2 + as.Date(paste0("01 ", from), "%d %b %Y")]
#ipc <- ipc[ipc[, .I[which.min(abs(as.Date("2020-06-30")-date))], by = country]$V1]

ipc_national <- ipc[, rbindlist(lapply(phases, function(x) lapply(x, function(y) as.numeric(y)))), by = .(title, country, condition, id, estimated_population, from, to, period, year)]
ipc_national <- merge(isos[, .(iso3, iso2)], ipc_national, by.x = "iso2", by.y = "country", all.y = T)

fwrite(ipc_national, "datasets/IPC/ipc_national.csv")

ipc_sub <- list()
for(i in 1:nrow(ipc)){
  id <- ipc[i]$id
  cc <- ipc[i]$country
  link <- paste0("https://map.ipcinfo.org/api/export/geojson/", id, "/?country=", cc, "&condition=A")
  temp <- fromJSON((link))$features$properties
  temp$icons <- NULL
  ipc_sub[[i]] <- as.data.table(temp)
}

ipc_sub <- rbindlist(ipc_sub, fill = T)

fwrite(ipc_sub, "datasets/IPC/ipc_subnational.csv")
