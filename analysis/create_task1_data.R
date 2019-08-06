# Packages

pacman::p_load(tidyverse, 
                haven,
                rvest, # List of automobile identification codes
                kableExtra,
                sjlabelled,
                descr,
                sjmisc,
                car,
                countrycode
                ) 
 
# Raw data
path <- "C:/Users/CM/Dropbox/2017 - Doctorado PUC/replicationMzesOpen/data/"
l2 <- read_dta(paste0(path, "L2data.dta"))
names(l2)
issp96 <- read_dta(paste0(path, "ZA2900.dta"))
issp06 <- read_dta(paste0(path, "ZA4700_2.dta")) ## Different archive from original script
names(issp06) <- tolower(names(issp06))

# Find variables (create labels vector for next analysis)
issp96.labs <- issp96 %>% 
                    map_chr(~attributes(.)$label)
issp06.labs <- issp06 %>% 
        map_chr(~attributes(.)$label)

## Individual level variables

## 1996

names(issp96)

#' 
#' *Dependent variables*
#' 
#' "Instructions: Then there is a module of questions from which we draw variables in the social welfare related domains of, 
#' - “... provide a decent standard of living for the old” we label this Old Age Care, 
#' - “... provide a decent standard of living for the unemployed” labeled Unemployed, 
#' - “... reduce income differences between the rich and the poor” labeled Reduce Income Differences, and 
#' - “... provide a job for everyone who wants one” labeled Jobs. 
#' 
#' Respondents chose among ordinal categories of definitely should be,
#' probably should be, probably should not be, and definitely should not be for each. These are collapsed into a dichotomous variable where affirmative answers =1."
#' 
#' Identify variables
#' 
## ------------------------------------------------------------------------
issp96.labs[grep("provide", issp96.labs)] # v39 Old Age Care & v41 unemployed
issp96.labs[grep("duce", issp96.labs)] # v42 Reduce Income Differences
issp96.labs[grep("job", issp96.labs)] # v36 Jobs

#' 
#' Select & recode
#' 
## ------------------------------------------------------------------------
dep.96 <- issp96 %>% 
                select("v39", "v41", "v42", "v36") %>% 
                rename(old.age = v39, unemploy = v41, red.inc = v42, jobs = v36)

get_labels(dep.96)
freq(to_label(dep.96$old.age))
summary(dep.96)

#' 
#' Missings already recoded to NAs in dataset; now recode according to instruccions values 1 & 2 to 1 and 3 & 4 to 0.
#' 
## ------------------------------------------------------------------------
dep.96 <- as_tibble(ifelse(dep.96 <= 2, 1, ifelse(dep.96 <= 4, 0, NA)))
freq(to_label(dep.96$old.age)) # Check
summary(dep.96) # Check

#' 
#' 
#' *Independent variables*
#' 
#' Select & recodes
#' 
## ------------------------------------------------------------------------
issp96.labs[grep("sex", issp96.labs)] # v200 sex
issp96.labs[grep(" [Aa]ge ", issp96.labs)] #  v201 age
issp96.labs[grep("ducat", issp96.labs)] #  v205 R: Education II: categories
issp96.labs[grep("yment", issp96.labs)] #  v206 R: Current employment status
issp96.labs[grep("ountry", issp96.labs)] # v3

ind.96 <- issp96 %>% 
                select("v2","v3", "v200", "v201", "v205", "v206") %>% 
                rename(id.r = v2, country=v3, sex = v200, age = v201, educ =
                         v205, empl = v206)

# Sex
  get_labels(ind.96)
  freq(ind.96$sex)
  ind.96$sex= Recode(ind.96$sex,"1=0 ; 2=1")

# Educ (nota: secondary incomplete here as primary,could also be secondary, not clear from instructions)
  freq(ind.96$educ)
  ind.96$educ <- ifelse(ind.96$educ <= 4, "Primary or less", 
                        ifelse(ind.96$educ == 5 | ind.96$educ == 6, "Secondary", ifelse(ind.96$educ == 7, "University", NA)))
  ind.96$educ <- relevel(as_factor(ind.96$educ), ref = "Secondary")
  freq(ind.96$educ)  
  get_labels(ind.96$educ)
  lm(sex ~ educ, data = ind.96) # check reference

# Employment  
  # Note: It's not clear in what category recode 'Helping family member', now as 'active unemployed'. Also 'housewife' could be multiple categories  

  get_labels(ind.96$empl)
  freq(ind.96$empl)
  ind.96$empl <- ifelse(ind.96$empl == 1, "Full-time", 
                 ifelse(ind.96$empl == 2 | ind.96$empl == 3, "Part-time",
                 ifelse(ind.96$empl == 4 | ind.96$empl == 5, "Active unemployed",
                 ifelse(ind.96$empl >= 6, "Not active", NA))))

  ind.96$empl <- relevel(as.factor(ind.96$empl), ref = "Full-time")
  freq(ind.96$empl)
  
# Year  
  ind.96$year <- rep(1996, n = nrow(ind.96))

#' 
#' 
#' ### 2006
#' 
#' *Dependent variables*
#' 
## ------------------------------------------------------------------------
issp06.labs[grep("rovide", issp06.labs)] # v28 Old Age Care & v30 unemployed
issp06.labs[grep("duce", issp06.labs)] # v31 Reduce Income Differences
issp06.labs[grep("job", issp06.labs)] # v25 Jobs

dep.06 <- issp06 %>% select ("v28", "v30", "v31", "v25") %>% 
                rename(old.age = v28, unemploy = v30, red.inc = v31, jobs = v25)

summary(dep.06)

get_labels(dep.06)
freq(to_label(dep.06$old.age))

dep.06 <- as_tibble(ifelse(dep.06 <= 2, 1, ifelse(dep.06 <= 4, 0, NA)))
summary(dep.06) # Check

#' 
#' *Independent variables*
#' 
## ------------------------------------------------------------------------
issp06.labs[grep("[Ss]ex", issp06.labs)] # sex sex
issp06.labs[grep("[Aa]ge", issp06.labs)] #  age age
issp06.labs[grep("ducat", issp06.labs)] #  degree "R: Education II-highest education level"
issp06.labs[grep("yment", issp06.labs)] #  wrkst "R: Current employment status" 

ind.06 <- issp06 %>% 
                select("v2","v3", "sex", "age", "degree", "wrkst") %>% 
                rename(id.r = v2, country=v3, educ = degree, empl = wrkst)

  get_labels(ind.06)

# Sex
  freq(ind.06$sex)
  ind.06$sex= Recode(ind.06$sex,"1=0 ; 2=1")

# Educ  
  get_labels(ind.06$educ)
  freq(ind.06$educ)
  freq(to_label(ind.06$educ))
  ind.06$educ <- ifelse(ind.06$educ <= 2, "Primary or less", 
                        ifelse(ind.06$educ == 3 | ind.06$educ == 4, "Secondary", 
                        ifelse(ind.06$educ == 5, "University", NA)))
  freq(ind.06$educ)  
  ind.06$educ <- relevel(as_factor(ind.06$educ), ref = "Secondary")

# Employment  
  freq(ind.06$empl)
  freq(to_label(ind.06$empl))
  ind.06$empl <- ifelse(ind.06$empl == 1, "Full-time", 
                      ifelse(ind.06$empl == 2 | ind.06$empl == 3, "Part-time",
                             ifelse(ind.06$empl == 4 | ind.06$empl == 5, "Active unemployed",
                                    ifelse(ind.06$empl >= 6 , "Not active", NA))))
  freq(to_label(ind.06$empl))
  ind.06$empl <- relevel(as.factor(ind.06$empl), ref = "Full-time")

# Year  
  ind.06$year <- rep(2006, n = nrow(ind.06))

#' 
#'   
#' ### Select common 13 countries
#' 
#' ISSP 2006 countries 
#' 
## ------------------------------------------------------------------------
unique(ind.06$country) # casi códigos iso, pero con "."; eliminar el valor posterior al punto
ind.06$countryiso <- as.character(as_factor(ind.06$country))
ind.06$countryiso <- gsub("//.[0-9]", "", ind.06$country)
ind.06$countryiso=as.numeric(ind.06$countryiso)
unique(ind.06$countryiso)

#' 
#' ISSP 1996 countries
#' 
## ------------------------------------------------------------------------
get_labels(ind.96$country) # country vehicle registration codes (the old ISSP way ...)
unique(ind.96$country)

# Recode 1996 to ISO 3  
ind.96$countryiso=Recode(ind.96$country,
"1=036;
2=276;
3=276;
4=826;
5=826;
6=840;
7=040;
8=348;
9=380;
10=372;
11=528;
12=578;
13=752;
14=203;
15=705;
16=616;
17=100;
18=643;
19=554;
20=124;
21=608;
22=376;
23=376;
24=392;
25=724;
26=428;
27=250;
28=196;
30=756")

ind.96$countryisoc   <- countrycode(ind.96$countryiso, "iso3n", "iso3c")
get_labels(ind.96$countryisoc)

ind.96 %>% 
    group_by(country) %>% 
    select(country,countryiso,countryisoc) %>%  
    summarise(countryiso=mean(countryiso),countryisoc=first(countryisoc))


#' 
#' *Individuals' general dataframe*
#' 
## ------------------------------------------------------------------------

issp <- bind_rows(bind_cols(dep.06, ind.06), 
                  bind_cols(dep.96, ind.96))
names(issp)
issp$country <- NULL
issp = rename(issp, country = countryiso)

#' 
#' Filter by common rich countries
#' 
## ------------------------------------------------------------------------
com.cntr <- intersect(unique(ind.06$countryiso), unique(ind.96$countryiso)) # common coutries in both datasets
length(com.cntr)

# Differentiate not rich countries according to 2006 GDP per capita (World Bank)

# not.rich = "Czech Republic" (203), "Hungary" (348), "Israel" (376), "Latvia"(428), "Philippines"(608), "Poland" (616), "Slovenia"(705), "Spain" (724))

not.rich=c(203,348,376,428,608,616,705,643) ## Correction for resubmission

coun.anlys <- com.cntr[!(com.cntr %in% not.rich)]
length(coun.anlys)

#### Filter issp data set by 13 countries
issp <- filter(issp, country  %in% coun.anlys)
unique(issp$country)
summary(issp)

## Add level 2 data
names(l2)
l2=rename(l2, country=cntry, country_lab=country)
unique(l2$country)
issp <- merge(issp, l2, by = c("country", "year"))
names(issp)

summary(issp)
length(unique(issp$country))

### Checks
issp %>% 
    group_by(country,year) %>% 
    summarise_if(is.numeric, mean,na.rm = TRUE)

issp %>% 
    group_by(country,year) %>% 
    summarise(total.count=n())

table(issp$country_lab)

## Save dataframe
save(issp, file = paste0(path, "issp_tidy_task1_s2.RData"))
load(file = paste0(path, "issp_tidy_task1_s2.RData"))
names(issp) <- gsub("\\.", "_", names(issp))
write_dta(issp, path = paste0(path, "issp_tidy_task1_s2.dta"))
