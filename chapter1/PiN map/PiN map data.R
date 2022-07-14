required.packages <- c("data.table","jsonlite","httr","readxl", "rstudioapi")
lapply(required.packages, require, character.only=T)

invisible(lapply(c("https://raw.githubusercontent.com/devinit/gha_automation/main/general/emdatData.R",
         "https://raw.githubusercontent.com/devinit/gha_automation/main/general/hiikData.R",
         "https://raw.githubusercontent.com/devinit/gha_automation/main/general/informAPI.R",
         "https://raw.githubusercontent.com/devinit/gha_automation/main/general/wupData.R",
         "https://raw.githubusercontent.com/devinit/gha_automation/main/general/acapsAPI.R",
         "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_api_appeals.R",
         "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_appeals_data.R"
), source))

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")
setwd("..")

isos <- fread("datasets/Countrynames/isos.csv", encoding = "UTF-8")
###This map shows:###
#- The total number of PiN by country
#- The types of crises affecting each country (displacement, conflict, natural/technological hazard)
#- The ACAPS/INFORM severity score
#- Climate risk
#- Food insecurity
#- Vaccine rollout
#- Protracted crisis countries
#- HRP requirements
#- RRP requirements

##Total PiN##
hpc_pin <- fread("datasets/People in need - HPC/PiN_total_2021.csv")
hpc_pin <- hpc_pin[metric_id == "inNeed", .(iso3, hpc_pin = value/1000000)]
acaps_pin <- fread("datasets/ACAPS/acaps_pin.csv")[year == 2021, .(iso3, acaps_pin)]
all_pin <- merge(hpc_pin, acaps_pin, all = T)[, .(pin = max(c(acaps_pin, hpc_pin), na.rm = T)), by = iso3]

##Crises types##
crisis_types <- fread("datasets/Crisis types/crisis_types.csv")[year == 2021]

##ACAPS severity##
acaps <- fread("datasets/ACAPS/ACAPS_20220427.csv")
severity <- acaps[!is.na(`Concentration of conditions`) & year == 2021, .(severity_max = max(as.numeric(`Concentration of conditions`), na.rm = T)), by = .(iso3)][, iso3 := gsub("[|]", "; ", iso3)]

##INFORM severity##
inform_risk <- inform_get(2021, "INFORM")
inform_risk <- inform_risk[, .(iso3 = Iso3, risk_score = IndicatorScore, risk_class = ifelse(IndicatorScore >= 2, ifelse(IndicatorScore >= 3.5, ifelse(IndicatorScore >= 5, ifelse(IndicatorScore >= 6.5, "Very High", "High"), "Medium"), "Low"), "Very Low"))]

##Vaccine rollout##
vac_share <- fread("datasets/OWID/vaccination_shares.csv")
vac_share <- vac_share[, c("iso3", "vac_share")]

## Climate vuln ##
ndgain <- fread("datasets/ND-GAIN/vulnerability/vulnerability.csv", header = T)
ndgain <- ndgain[, .(iso3 = ISO3, vuln = `2019`)]
vuln_thresholds <- data.table(threshold = quantile(ndgain$vuln, seq(0,0.8,0.2), na.rm = T), climate_vulnerability = c("very resilient", "resilient", "slightly vulnerable", "vulnerable", "very vulnerable"))
ndgain[, climate_vulnerability := ifelse(vuln < vuln_thresholds[2]$threshold, vuln_thresholds[1]$climate_vulnerability, ifelse(vuln < vuln_thresholds[3]$threshold, vuln_thresholds[2]$climate_vulnerability, ifelse(vuln < vuln_thresholds[4]$threshold, vuln_thresholds[3]$climate_vulnerability, ifelse(vuln < vuln_thresholds[5]$threshold, vuln_thresholds[4]$climate_vulnerability, vuln_thresholds[5]$climate_vulnerability))))]

## Food insecurity ##
ipc <- data.table(fromJSON("https://fsr2av3qi2.execute-api.us-east-1.amazonaws.com/ch/country?key=bac2a4d1-1274-4526-9065-0502ce9d4d5e"))
ipc[, date := (as.Date(paste0("01 ", to), "%d %b %Y")-as.Date(paste0("01 ", from), "%d %b %Y"))/2 + as.Date(paste0("01 ", from), "%d %b %Y")]
ipc <- ipc[ipc[, .I[which.min(abs(as.Date("2021-06-30")-date))], by = country]$V1]
ipc <- ipc[, rbindlist(lapply(phases, function(x) lapply(x, function(y) as.numeric(y)))), by = .(title, country, condition, id, estimated_population, from, to, period, year)]
ipc <- ipc[, .(ipc_crisis_share = sum(population[phase >= 3])/1000000, ipc_phase = max(phase[percentage >= 0.2])), by = country]
ipc <- merge(ipc[, .(iso2 = country, ipc_crisis_share, ipc_phase)], isos[, .(iso3, iso2)], by = "iso2", all.x = T)
ipc <- ipc[, .(iso3, ipc_crisis_share, ipc_phase)]

##Protracted crisis countries##
pccs <- fread("datasets/Protracted Crisis/protracted_crisis_classifications.csv")
pccs <- pccs[year == 2021]
pccs <- pccs[, .(iso3, consecutive_crisis, crisis_class)]

##HRPs##
hrps <- fts_get_appeals(2021)
hrps[, `:=` (iso3 = unlist(lapply(locations, function(x) paste0(x$iso3[x$adminLevel == 0], collapse = ";"))), type = unlist(lapply(categories, function(x) x$name)))]
hrps <- hrps[type != "Regional response plan"]

hrps_list <- list()
for(i in 1:nrow(hrps)){
  id <- hrps[i]$id
  hrps_list[[i]] <- fts_get_appeal_totals(id, 2021)
}
hrps_dt <- rbindlist(hrps_list, fill = T)

hrps <- merge(hrps[, .(id, iso3)], hrps_dt[, .(id = appeal_id, HRP_funding = `Total incoming funding:`, HRP_requirements = `Total current requirements:`)], by = "id", all = T)
hrps <- hrps[, .(HRP_funding = sum(HRP_funding, na.rm = T), HRP_requirements = sum(HRP_requirements, na.rm = T)), by = iso3]

##RRPs##
combined_rrps <- fread("datasets/UNHCR RRPs/combined_rrps.csv", encoding = "UTF-8")

unhcr_rrps <- fread("datasets/UNHCR RRPs/rrp_data.csv", encoding = "UTF-8")
unhcr_rrps[Country == "Iran, Islamic Republic of", Country := "Iran (Islamic Republic of)"]
unhcr_rrps[Country == "Burma", Country := "Myanmar"]
unhcr_rrps[Country == "Czech Republic", Country := "Czechia"]
unhcr_rrps[Country == "Moldova", Country := "Republic of Moldova"]
unhcr_rrps[Country == "Slovak Republic", Country := "Slovakia"]
unhcr_rrps[Country == "Curaçao", Country := "Curacao"]
unhcr_rrps[Country == "United States", Country := "United States of America"]

unhcr_rrps <- merge(unhcr_rrps[Year == 2021], isos[, .(iso3, Country = countryname_unhcr)], by = "Country", all.x = T)

unhcr_rrps <- unhcr_rrps[!(RRP %in% combined_rrps[`Use?` == "UNOCHA"]$`UNHCR name`)]
unhcr_rrps <- unhcr_rrps[, .(country = Country, RRP, req = as.numeric(gsub("[$]|,", "", `Funds Requested`)), funds = as.numeric(gsub("[$]|,", "", `Funds Received`)))]

unocha_rrps <- fts_get_appeals(2021)
unocha_rrps <- unocha_rrps[planVersion.name %in% combined_rrps[`Use?` == "UNOCHA"]$`UNOCHA name`]

unocha_rrps_list <- list()
for(i in 1:nrow(unocha_rrps)){
  id <- unocha_rrps[i]$id
  unocha_rrps_list[[i]] <- fts_get_appeal_locations(id, 2021)
}
unocha_rrps <- rbindlist(unocha_rrps_list, fill = T)
unocha_rrps[plan_name == "Bangladesh: Rohingya Refugee Crisis Joint Response Plan 2021", Location := "Bangladesh"]

unocha_rrps <- unocha_rrps[, .(country = Location, RRP = plan_name, req = as.numeric(gsub(",", "", `Current requirements US$`)), funds = as.numeric(gsub(",", "", `Funding US$`)))]

rrps <- rbind(unhcr_rrps, unocha_rrps)
rrps <- rrps[, .(RRP_requirements = sum(req, na.rm = T), RRP_funding = sum(funds, na.rm = T)), by = country]

rrps[country == "Bolivia, Plurinational State of", country := "Bolivia"]
rrps[country == "Curaçao", country := "Curacao"]

rrps <- merge(rrps, isos[, .(iso3, country = countryname_unhcr)], by = "country", all.x = T)
rrps <- rrps[, .(iso3, RRP_requirements, RRP_funding)]

###Join all
crises_map <- merge(all_pin[!grepl(";", iso3)], crisis_types, by = "iso3", all.x = T)
crises_map <- merge(crises_map, severity, by = "iso3", all.x = T)
crises_map <- merge(crises_map, inform_risk, by = "iso3", all.x = T)
crises_map <- merge(crises_map, vac_share, by = "iso3", all.x = T)
crises_map <- merge(crises_map, ndgain, by = "iso3", all.x = T)
crises_map <- merge(crises_map, ipc, by = "iso3", all.x = T)
crises_map <- merge(crises_map, pccs, by = "iso3", all.x = T)
crises_map <- merge(crises_map, hrps, by = "iso3", all.x = T)
crises_map <- merge(crises_map, rrps, by = "iso3", all.x = T)

fwrite(crises_map, "chapter1/PiN map/crises_map.csv")
