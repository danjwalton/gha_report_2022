suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))
invisible(lapply(c("https://raw.githubusercontent.com/devinit/gha_automation/main/PiN/hpc_caseload_api.R")
       , source))

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

plans <- data.table(fromJSON("https://api.hpc.tools/v2/public/plan")$data)
plans[, year := lapply(years, function(x) x$year), by = id]
plans[, type := lapply(categories, function(x) x$name), by = id]

plans <- plans[year %in% 2018:2021]

plan_caseloads <- list()
pb <- txtProgressBar(0, nrow(plans), style = 3)
for(i in 1:nrow(plans)){
  plan_id <- plans$id[[i]]
  plan_name <- plans$planVersion.name[[i]]
  location <- paste0(plans$locations[[i]][["name"]][plans$locations[[i]]$adminLevel == 0], collapse = "; ")
  iso <- paste0(plans$locations[[i]][["iso3"]][plans$locations[[i]]$adminLevel == 0], collapse = "; ")
  year <- paste0(plans$years[[i]][["year"]], collapse = "; ")
  
  metadata <- cbind(plan_id, plan_name, location, iso, year)
  
  caseload_temp <- hpc_api_all(plan_id, data_type = "caseLoad", by_sector = F, disaggregations = T)
  
  if(!is.null(caseload_temp)) plan_caseloads[[i]] <- data.table(cbind(metadata, caseload_temp))
  rm(plan_id)
  setTxtProgressBar(pb, i)
}
plan_caseloads <- rbindlist(plan_caseloads, fill = T)

plan_caseloads <- plan_caseloads[metric_id %in% c("inNeed", "target", "expectedReach")]

#

splits <- plan_caseloads[, .(cat_splits =  paste0(unique(category_name), collapse = ", "), loc_splits = paste0(unique(location_name), collapse = ", ")), by = .(plan_id, plan_name, year)]

#Gender
gender_f <- c("women", "girl", "female", "fille", "femme", "féminin", "feminin", "niña", "nina", "mujere")
gender_m <- c("\\bmen\\b", "boy", "\\bmale\\b", "garçon", "garcon", "homme", "masculin", "niño", "\\bnino\\b", "hombre")
gender_terms <- paste0(paste0(gender_f, collapse = "|"), "|", paste0(gender_m, collapse = "|"))

splits[grepl(gender_terms, cat_splits, ignore.case = T), gender := T]

#IDPs
idps <- c("\\bidp", "\\bpdi", "displaced")
refugees <- c("refugee", "réfugié.s", "refugie.s")
refugee_terms <- paste0(paste0(idps, collapse = "|"), "|", paste0(refugees, collapse = "|"))

splits[grepl(refugee_terms, cat_splits, ignore.case = T), refugees := T]

#Children
children <- c("child", "girl", "boy", "fille", "garçon", "garcon", "niña", "niño", "\\b5\\b", "19", "17", "\\b1\\b")
children_terms <- paste0(children, collapse = "|")

splits[grepl(children_terms, cat_splits, ignore.case = T), children := T]

#Elderly
elderly <- c("elderly", "60")
elderly_terms <- paste0(elderly, collapse = "|")

splits[grepl(elderly_terms, cat_splits, ignore.case = T), elderly := T]

#Subnational
splits[loc_splits != "NA", subnational := T]
