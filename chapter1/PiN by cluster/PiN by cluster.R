suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))
invisible(lapply(c("https://raw.githubusercontent.com/devinit/gha_automation/main/PiN/hpc_caseload_api.R",
                   "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_api_appeals.R")
                 , source))

setwd(dirname(getActiveDocumentContext()$path))

appeals <- fts_get_appeals(2018:2021)

appeals_dt <- appeals[, .(iso3 = unlist(lapply(locations, function(x) as.character(x$iso3[x$adminLevel == 0]))), year = unlist(lapply(years, function(x) x$year)), type = unlist(lapply(categories, function(x) x$name))), by = .(id, planVersion.name)]

pin <- list()
for(i in 1:nrow(appeals_dt)){
  
  id = appeals_dt[i]$id
  temp <- hpc_api_all(id, by_sector = F, disaggregations = F)
  pin[[i]] <- cbind(appeals_dt[i, .(id, plan_name = planVersion.name, iso3, year)], temp)
}

pin <- rbindlist(pin, fill = T)

fwrite(pin, "PiN_total.csv")
