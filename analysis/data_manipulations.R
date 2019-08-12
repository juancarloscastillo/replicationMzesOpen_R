# Generate object with dependent variables for all ISSP waves

## Packages

if (!require("pacman")) install.packages("pacman")
<<<<<<< HEAD
pacman::p_load(haven, purrr, tibble, dplyr,readxl, countrycode, rvest, stringr)


## Read the five individual-level datasets

path <- "data"

archives <- list.files(path, pattern = "^Z.*\\.dta$", full.names = T)
issp <- lapply(archives, read_dta)
names(issp) <- c("issp85", "issp90","issp96", "issp06", "issp16") # name dfs

## Get variable labels

makeVlist <- function(dta) {
        labels <- sapply(dta, function(x) attr(x, "label"))
        tibble(name = names(labels),
               label = labels)
}
issp.labels <- lapply(issp, makeVlist) # list of variables in each df

## Find the six dependent variables
ids <- lapply(issp.labels, function(x)
        x[grep("resp.*numb|numb.*res", x$label,
               ignore.case = T),])

jobs <- lapply(issp.labels, function(x)
        x[grep("resp.*job", x$label,
               ignore.case = T),])
jobs.k <- tibble(data = names(jobs), # Key of same dv variable in each ISSP
                 job.vars = unlist(sapply(jobs, function(x) x[,"name"])),
                 id = unlist(sapply(ids, function(x) x[,"name"])))

unemp <- lapply(issp.labels, function(x)
        x[grep("resp.*une", x$label,
               ignore.case = T),])
unemp.k <- tibble(data = names(unemp),
                  unemp.vars = unlist(sapply(unemp, function(x) x[,"name"])),
                  id = unlist(sapply(ids, function(x) x[,"name"])))

income <- lapply(issp.labels, function(x)
        x[grep("resp.*red.*inc", x$label,
               ignore.case = T),])
income.k <- tibble(data = names(income),
                   income.vars = unlist(sapply(income, function(x) x[,"name"])),
                   id = unlist(sapply(ids, function(x) x[,"name"])))

retirement <- lapply(issp.labels, function(x)
        x[grep("elde.*|resp.*old.*|18d.*old", x$label,
               ignore.case = T),])
retirement.k <- tibble(data = names(retirement),
                       retirement.vars = unlist(sapply(retirement, function(x) x[,"name"])),
                       id = unlist(sapply(ids, function(x) x[,"name"])))

healthcare <- lapply(issp.labels, function(x)
        x[grep("resp.*health", x$label,
               ignore.case = T),])
healthcare.k <- tibble(data = names(healthcare),
                       healthcare.vars = unlist(sapply(healthcare, function(x) x[,"name"])),
                       id = unlist(sapply(ids, function(x) x[,"name"])))

housing <- lapply(issp.labels, function(x) ## Not asked in 1985
        x[grep("dece.*", x$label,
               ignore.case = T),])
housing.k <- tibble(data = names(housing)[names(housing) != "issp85"],
                    housing.vars = unlist(sapply(housing, function(x) x[,"name"])),
                    id = unlist(sapply(ids[-1], function(x) x[,"name"]))) # rm 85

## Build df with dependent variables

### Function to create merge of same dv in different dataframes with id

merge.dv.vars <- function(list, key, var.name, new.name){
        map2_df(.x = list,
                .y = names(list),
                .f = ~ {
                        temp1 <- key[[var.name]][key[['data']] == .y]
                        temp2 <- key[["id"]][key[['data']] == .y]
                        tibble(data = .y,
                               !!new.name := .x[[temp1]],
                               id = .x[[temp2]])
                })
}

jobs.dv <- merge.dv.vars(list = issp, key = jobs.k,
                         var.name = "job.vars", new.name = "jobs")
unemp.dv <- merge.dv.vars(list = issp, key = unemp.k,
                          var.name = "unemp.vars", new.name = "unemployed")
income.dv <- merge.dv.vars(list = issp, key = income.k,
                           var.name = "income.vars", new.name = "income")
retirement.dv <- merge.dv.vars(list = issp, key = retirement.k,
                               var.name = "retirement.vars", new.name = "retirement")
healthcare.dv <- merge.dv.vars(list = issp, key = healthcare.k,
                               var.name = "healthcare.vars", new.name = "healthcare")
housing.dv <- merge.dv.vars(list = issp[-1], key = housing.k, # rm 85
                            var.name = "housing.vars", new.name = "housing")

## Create tibble with all the dv -housing
dv.final <- bind_cols(jobs.dv[,c(3,1,2)],
                      unemp.dv[,2], income.dv[,2],
                      retirement.dv[,2], healthcare.dv[,2])

### There are less unique ids than nrows in datasets, fast solution

dv.final$id2 <- as.numeric(paste0(gsub("issp", "", dv.final$data),
                                  dv.final$id))
housing.dv$id2 <- as.numeric(paste0(gsub("issp", "", housing.dv$data),
                                    housing.dv$id))
dv.final$housing <- ifelse(dv.final$id2 %in% housing.dv$id2,
                           housing.dv$housing, NA)

dv.final[,3:8][dv.final[,3:8] < 1 | dv.final[,3:8] > 4] <- NA

### Country
#### International vehicle registration code
if(!file.exists(paste0(path,"country_code_vehicle_reg.RData"))){
        url <- "https://en.wikipedia.org/wiki/International_vehicle_registration_code"
        library(rvest)
        aut.id <- url %>%
                read_html() %>%
                html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
                html_table(header = T, fill=T) %>%
                as.data.frame()
        aut.id <- aut.id[,1:2]
        aut.id$Code <- tolower(aut.id$Code)
        names(aut.id) <- tolower(names(aut.id))
        save(aut.id,
             file = paste0(path, "country_code_vehicle_reg.RData"))
} else { load(paste0(path,"country_code_vehicle_reg.RData")) }
plain.ctry <- function(x){
        tolower(as.character(as_factor(x)))
}

country <- tibble(ctry.reg = c(plain.ctry(issp$issp85$V3),
                               plain.ctry(issp$issp90$v3),
                               plain.ctry(issp$issp96$v3)))
country <- merge(country, aut.id, by.x = "ctry.reg", by.y = "code", all.x = T)

## Some cases by hand
country$country[country$ctry.reg == "d-e" | country$ctry.reg == "d-w" ] <- "Germany"
country$country[country$ctry.reg == "il-a" | country$ctry.reg == "il-j" ] <- "Israel"
country$country[country$ctry.reg == "nirl" | country$ctry.reg == "il-j" ] <- "United Kingdom"
country$country[country$country == "United Kingdom (of Great Britain and Northern Ireland)"] <-
        "United Kingdom"
country$country <- tolower(country$country)
country[sample(nrow(country), 30),] ## Check countries from 85, 90 & 96

### ISSP 06
country06 <- gsub("^.*-", "", plain.ctry(issp$issp06$V3a))
country06[country06 == "Great Britain"] <- "united kingdom" # Following labels in l2
country06[sample(length(country06), 30)]

### ISSP 16
issp$issp16$c_alphan[issp$issp16$c_alphan == "GB-GBN"] <- "GB"
country16 <- tolower(countrycode::countrycode(issp$issp16$c_alphan,
                                              "iso2c", "country.name"))
country.all <- stringr::str_to_title(c(country$country,
                                       country06, country16)) ## length == all issp

dv.final$country <- country.all ## Add country to tibble of dv
dv.final$country[dv.final$country == "Great Britain"] <- "United Kingdom"
dv.final$country[dv.final$country == "Czechia"] <- "Czech Republic"


sum(table(dv.final$country)) ## Check
sum(is.na(dv.final$country)) ## Check

### Save the data with 6 dv in 5 ISSP waves (except for housing, not in 85)

# Save object
save(dv.final, file = "data/dv_5issp.rda")

## Add level 2 data
l2 <- readxl::read_excel(paste0(path, "/cri_macro1.xlsx"),
                         na = c(".", ".."))
l2$country[l2$country == "The Netherlands"] <- "Netherlands"
l2$country[l2$country == "Korea, South"] <- "South Korea"
l2$country[l2$country == "Czechia"] <- "Czech Republic"

## Missing l2 data for:
## Dominican Republic, Georgia, Philippines, Suriname, Thailand, Uruguay, Venezuela
issp.rep <- dv.final
issp.rep$year <- ifelse(issp.rep$data == "issp85", "1985",
                        ifelse(issp.rep$data == "issp90", "1990",
                               ifelse(issp.rep$data == "issp96", "1996",
                                      ifelse(issp.rep$data == "issp06", "2006",
                                             "2016"))))

issp.rep <- merge(issp.rep, l2, by = c("country", "year"), all.x = T)
save(issp.rep,
     file = paste0(path, "/issp_l2.RData"))

## Level 1 independent variables IV
sex <- lapply(issp.labels, function(x)
        x[grep("r.*sex|sex.*resp|^\\s+sex\\s+$", x$label,
               ignore.case = T),])
sex.k <- tibble(data = names(sex),
                sex.vars = unlist(sapply(sex, function(x) x[,"name"])),
                id = unlist(sapply(ids, function(x) x[,"name"])))

sex.iv <- merge.dv.vars(list = issp, key = sex.k,
                        var.name = "sex.vars", new.name = "sex")
sex.iv$sex <- factor(ifelse(sex.iv$sex == 1, "Male",
                            ifelse(sex.iv$sex == 2, "Female", NA)))

age <- lapply(issp.labels, function(x)
        x[grep("age.*dent$|age.*dent\\s+$|r: age|^\\s+age \\s+$", x$label,
               ignore.case = T),])
age.k <- tibble(data = names(age),
                age.vars = unlist(sapply(age, function(x) x[,"name"])),
                id = unlist(sapply(ids, function(x) x[,"name"])))
age.iv <- merge.dv.vars(list = issp, key = age.k,
                        var.name = "age.vars", new.name = "age")
age.iv$age[age.iv$age == 999] <- NA

## Employment status independent variables
lapply(issp.labels, function(x)
        x[grep("r:.*curr.*empl|*curr.*empl|^\\scurr.*empl.*\\s+$|^empl.*ship$", x$label,
               ignore.case = T),])

empl.k <- tibble(data = c("issp85", "issp90", "issp96", "issp06", "issp16"),
                 empl.vars = c("V109", "v63", "v206", "wrkst", "WORK"),
                 id = unlist(sapply(ids, function(x) x[,"name"])))

## ISSP 85 only differentiates between employed & unemployed
empl.iv85 <- tibble(data = rep("issp85", nrow(issp$issp85)),
                    empl = ifelse(issp$issp85$V109 == 1, "Unemployed", "Not unemployed"))
empl.iv90 <- tibble(data = rep("issp90", nrow(issp$issp90)),
                    empl = ifelse(issp$issp90$v63 == 5, "Unemployed", "Not unemployed"))
empl.iv96 <- tibble(data = rep("issp96", nrow(issp$issp96)),
                    empl = ifelse(issp$issp96$v206 == 5, "Unemployed", "Not unemployed"))
empl.iv06 <- tibble(data = rep("issp06", nrow(issp$issp06)),
                    empl = ifelse(issp$issp06$wrkst == 5, "Unemployed", "Not unemployed"))
empl.iv16 <- tibble(data = rep("issp16", nrow(issp$issp16)),
                    empl = ifelse(issp$issp16$WORK == 2, "Unemployed", "Not unemployed"))
empl.iv <- bind_rows(empl.iv85, empl.iv90, empl.iv96, empl.iv06, empl.iv16)

## Tibble with all the data except education (problems for harmonizing with certainty)
issp.rep$sex  <- sex.iv$sex
issp.rep$age  <- age.iv$age
issp.rep$empl <- empl.iv$empl

### country/year variable (survey)
issp.rep$survey <- paste0(issp.rep$country, ".", issp.rep$year)

## Dichotomous dependent variables
issp.rep <- issp.rep %>% 
        mutate_at(vars(c("jobs", "unemployed", "income",
                         "retirement", "housing", "healthcare")),
                  funs(factor(recode(.,`1`="No", `2`="No", `3`="Yes", `4`="Yes"))))

## Countries considered in original analysis
brady.ctry <- c("Australia", "Canada", "Denmark", "Finland", "France", "Germany",
                "Ireland", "Japan", "Netherlands", "New Zealand", "Norway", 
                "Portugal", "Spain", "Sweden", "Switzerland", "United Kingdom",
                "United States")
issp.rep$brady.ctry <- factor(ifelse(issp.rep$country %in% brady.ctry, 1, 0))

### Country-year centered variables at grand mean
l2.vars <- names(issp.rep[,13:32])
issp.rep <- issp.rep %>% 
        mutate_at(vars(l2.vars), funs(. - mean(., na.rm = T)))

### l1 centered variables
issp.rep$sex.cwc <- as.character(issp.rep$sex)
issp.rep$sex.cwc <- ifelse(issp.rep$sex.cwc == "Female", 1, 0)
issp.rep$empl.cwc <- as.character(issp.rep$empl)
issp.rep$empl.cwc <- ifelse(issp.rep$empl.cwc == "Unemployed", 1, 0)

issp.rep <- issp.rep %>% 
        group_by(country, year) %>% 
        mutate(sex.cwc = sex.cwc - mean(sex.cwc, na.rm=T),
               empl.cwc = empl.cwc - mean(empl.cwc, na.rm=T),
               age.cwc = age - mean(age, na.rm=T))

## Save waves with independent variables at l1 (without education) and l2
save(issp.rep,
     file = paste0(path, "/issp_l2.RData"))
