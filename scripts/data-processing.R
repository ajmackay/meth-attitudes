library(tidyverse)
library(janitor)
library(gt)


# Import ------------------------------------------------------------------
#### TODO - Automate process to take latest file ####

survey.files <- list.files("data/", pattern = "*.csv")
survey.file <- survey.files[length(survey.files)]

if(length(survey.file) > 1){
  stop("Error with reading files")
}

# Headers
survey.headers <- read_csv(str_c("data/", survey.file), n_max = 1) %>% 
  names() %>% 
  tolower() %>% 
  str_replace("\\.\\.\\.", "\\.") %>% 
  str_replace("\\_", "\\.") %>% 
  str_replace("duration \\(in seconds\\)", "duration")

survey.raw <- read_csv(str_c("data/", survey.file), skip = 2)

colnames(survey.raw) <- survey.headers

survey.raw <- survey.raw %>% 
  mutate(id = row_number())


# Cleaning ----------------------------------------------------------------
#### Demographics - screened ####
dems.df <- survey.raw %>% 
  filter(status == "IP Address",
         q126.13 %in% c("Full license (current/valid)", "Full license (expired/revoked)")) %>% 
  transmute(id = id,
            license.screen = as_factor(q126.13),
            drugs.other.alcohol = q27,
            age = q19,
            sex = as_factor(q13),
            ethnicity = fct_recode(as_factor(q14),
                                   "Eastern European" = "Southern and Eastern European (e.g. Albania, Bosnia and Herzegovina, Bulgaria, Croatia, Greece)",
                                   "Oceanian"= "Oceanian (Australian, Aboriginal, Torres Strait Islander)",
                                   "Undisclosed" = "Prefer not to say",
                                   "North-West European" = "North-West European (e.g.  UK, Ireland, France, Germany, Spain, Belgium, Portugal, Austria, Andorra, Czechia)",
                                   "Central Asian" = "Southern and Central Asian (e.g. Afghanistan, Bangladesh, Bhutan, India, Kyrgyzstan, Kazakhstan, Maldives, Nepal, Pakistan, Sri Lanka)",
                                   "Asian" = "North-East Asian (e.g. China, Japan, North Korea, South Korea, Mongolia)",
                                   "American" = "People of the Americas (e.g. indigenous peoples of the Americas are the pre-Columbian peoples of North, Central and South America)",
                                   "Middle-East" = "North African and Middle Eastern (e.g. Egypt, and Turkey)",
                                   "South-East Asian" = "South-East Asian (e.g. Cambodia, Indonesia, Laos, Malaysia, the Philippines, Singapore, Thailand and Vietnam)",
                                   "African" = "Sub-Saharan African (e.g. Botswana,Cameroon, Central African Republic, Congo, Ethiopia, Ghana, Guinea, Kenya, Liberia, Namibia, Nigeria)"
            ),
            marital.status = as_factor(q15),
            education = fct_recode(as_factor(q20),
                                   "Did not finish high school" = "Attended high school but did not finish",
                                   "High School Diploma" = "High School Diploma",
                                   "Vocational/Technical degree or certificate" = "Vocational/Technical degree or certificate",
                                   "Did not finish University" = "Attended University but did not finish",
                                   "Bachelor Degree" = "Bachelor Degree",
                                   "Postgraduate Degree" = "Master’s Degree",
                                   "Postgraduate Degree" = "Postgraduate Degree"
            ),
            employment.status = as_factor(q17),
            area.live = q21,
            license.status = as_factor(q22),
            alcohol.ever = as_factor(q28),
            ma.ingest = fct_recode(as_factor(q47)) %>% replace_na("No") #### TODO Change to logical ####
  )

#### AUDIT ####
audit.df <- survey.raw %>% 
  filter(status == "IP Address",
         q126.13 %in% c("Full license (current/valid)", "Full license (expired/revoked)")) %>% 
  transmute(
    id = id,
    audit.freq = as.numeric(as.character(fct_recode(as_factor(q33.40),
                                                    "0" = "Never",
                                                    "1" = "Monthly or less",
                                                    "2" = "Two to four times a month",
                                                    "3" = "Two to three times per week",
                                                    "4" = "Four or more times a week"))
    ),
    audit.typical = as.numeric(as.character(fct_recode(as_factor(q31),
                                                       "0" = "1 or 2",
                                                       "1" = "3 or 4",
                                                       "2" = "5 or 6",
                                                       "3" = "7 to 9",
                                                       "4" = "10 or more"))
    ),
    audit.six = as.numeric(as.character(fct_recode(as_factor(q32),
                                                   "0" = "Never",
                                                   "1" = "Less than Monthly",
                                                   "2" = "Monthly",
                                                   "3" = "Two to three times per week",
                                                   "4" = "Four or more times a week"))
    ),
    audit.total = audit.freq + audit.typical + audit.six,
    audit.risky = ifelse((q13 == "Male" & audit.total > 2 | 
                            q13 == "Female" & audit.total > 1), "TRUE", "FALSE")
  )

#### Drug Use ####
drug.df <- survey.raw %>% 
  filter(status == "IP Address",
         q126.13 %in% c("Full license (current/valid)", "Full license (expired/revoked)")) %>% 
  transmute(
    id = id,
    cocaine = fct_recode(as_factor(q63.1),
                                  "Never" = "Neither/not used",
                                  "Before 12 months" = "BEFORE 12 MONTHS AGO",
                                  "Past 12 months" = "PAST 12 MONTHS",
                                  "Both" = "PAST 12 MONTHS,BEFORE 12 MONTHS AGO"
    ),
    cannabis = fct_recode(as_factor(q63.2),
                                   "Never" = "Neither/not used",
                                   "Before 12 months" = "BEFORE 12 MONTHS AGO",
                                   "Past 12 months" = "PAST 12 MONTHS",
                                   "Past 12 months" = "PAST 12 MONTHS,Neither/not used",
                                   "Both" = "PAST 12 MONTHS,BEFORE 12 MONTHS AGO"
    ),
    club.drugs = fct_recode(as_factor(q63.3),
                                     "Never" = "Neither/not used",
                                     "Before 12 months" = "BEFORE 12 MONTHS AGO",
                                     "Past 12 months" = "PAST 12 MONTHS",
                                     "Both" = "PAST 12 MONTHS,BEFORE 12 MONTHS AGO"
    ),
    hallucinogens = fct_recode(as_factor(q63.4),
                                        "Never" = "Neither/not used",
                                        "Before 12 months" = "BEFORE 12 MONTHS AGO",
                                        "Before 12 months" = "BEFORE 12 MONTHS AGO,Neither/not used",
                                        "Past 12 months" = "PAST 12 MONTHS",
                                        "Both" = "PAST 12 MONTHS,BEFORE 12 MONTHS AGO"
    ),
    inhalants = fct_recode(as_factor(q63.5),
                                    "Never" = "Neither/not used",
                                    "Before 12 months" = "BEFORE 12 MONTHS AGO",
                                    "Past 12 months" = "PAST 12 MONTHS",
                                    "Both" = "PAST 12 MONTHS,BEFORE 12 MONTHS AGO"
    ),
    heroin = fct_recode(as_factor(q63.6),
                                 "Before 12 months" = "BEFORE 12 MONTHS AGO",
                                 "Never" = "Neither/not used",
                                 "Past 12 months" = "PAST 12 MONTHS"
    ),
    sedatives = fct_recode(as_factor(q63.7),
                                    "Never" = "Neither/not used",
                                    "Before 12 months" = "BEFORE 12 MONTHS AGO",
                                    "Past 12 months" = "PAST 12 MONTHS",
                                    "Both" = "PAST 12 MONTHS,BEFORE 12 MONTHS AGO"
    ),
    new.psychoactive = fct_recode(as_factor(q63.8),
                                           "Never" = "Neither/not used",
                                           "Before 12 months" = "BEFORE 12 MONTHS AGO",
                                           "Before 12 months" = "BEFORE 12 MONTHS AGO,Neither/not used",
                                           "Past 12 months" = "PAST 12 MONTHS",
                                           "Both" = "PAST 12 MONTHS,BEFORE 12 MONTHS AGO"
    ),
    pres.drugs = as_factor(q64), # Prescription drugs
    pres.drug.use = as_factor(q144), # Will need to consolidate levels
    pres.pain.killer = as_factor(q125.1), # Off-label prescription drug use, prescription pain killer
    pres.pain.otc = as_factor(q125.2), # Off-label prescription drug use, over the counter
    pres.sleeping = as_factor(q125.3),
    pres.methadone = as_factor(q125.4),
    pres.ritalin = as_factor(q125.5)
  )

#### MA Use ####
ma.df <- survey.raw %>% 
  filter(status == "IP Address",
         q126.13 %in% c("Full license (current/valid)", "Full license (expired/revoked)"),
         q47 == "Yes") %>% 
  transmute(
    id = id,
    ma.most.common = as_factor(q48),
    ma.use.peak = as_factor(q49),
    ma.12.months = as_factor(q50),
    ma.before.last.year = as_factor(q51),
    ma.recent.use = as_factor(q52),
    ma.use.age = as.numeric(as.character(fct_recode(as_factor(q53),
                                                      "25" = "25 years old.",
                                                      "19" = "19 years old",
                                                      "18" = "18 years of age",
                                                      "20" = "Twenty years of age"))),
    ma.use.ways = q54, # Think about how to organise factors
    # SDS Scale (Re-scored to not use zero as R is weird with zeros)
    sds.1 = as.numeric(fct_recode(as_factor(q55),
                                                "1" = "Never or almost never",
                                                "2" = "Sometimes",
                                                "3" = "Often",
                                                "4" = "Always")
    ),
    sds.2 = as.numeric(fct_recode(as_factor(q56),
                                                       "1" = "Never or almost never",
                                                       "2" = "Sometimes",
                                                       "3" = "Often",
                                                       "4" = "Always")
    ),
    sds.3 = as.numeric(fct_recode(as_factor(q57),
                                                 "1" = "Not at all",
                                                 "2" = "A little",
                                                 "3" = "Often",
                                                 "4" = "Always or nearly always")
    ),
    sds.4 = as.numeric(fct_recode(as_factor(q58),
                                           "1" = "Never or almost never",
                                           "2" = "Sometimes",
                                           "3" = "Often",
                                           "4" = "Always")
    ),
    sds.5 = as.numeric(fct_recode(as_factor(q60),
                                                     "1" = "Not difficult at all",
                                                     "2" = "Quite difficult",
                                                     "3" = "Very difficult",
                                                     "4" = "Impossible")
    ),
    sds.total = ((sds.1 - 1) + (sds.2 - 1) + (sds.3 - 1) + 
                  (sds.4 - 1) + (sds.5 - 1)),
    ma.type = ifelse(sds.total > 4, "MUD", "Recreational"),
    ma.want.to.change = as_factor(q61),
    ma.could.change = as_factor(q62)
  ) %>% 
  filter(!is.na(sds.total))

ma.id <- ma.df %>% pull(id)

#### K6 ####
k6.df <- survey.raw %>% 
  filter(status == "IP Address",
         q126.13 %in% c("Full license (current/valid)", "Full license (expired/revoked)")) %>% 
  transmute(
    id = id,
    k6.nervous = as.numeric(fct_recode(as_factor(q33.58),
                                       "1" = "None of the time",
                                       "2" = "A little of the time",
                                       "3" = "Some of the time",
                                       "4" = "Most of the time",
                                       "5" = "All of the time")
    ),
    k6.hopeless = as.numeric(fct_recode(as_factor(q34),
                                        "1" = "None of the time",
                                        "2" = "A little of the time",
                                        "3" = "Some of the time",
                                        "4" = "Most of the time",
                                        "5" = "All of the time")
    ),
    k6.restless = as.numeric(fct_recode(as_factor(q35),
                                        "1" = "None of the time",
                                        "2" = "A little of the time",
                                        "3" = "Some of the time",
                                        "4" = "Most of the time",
                                        "5" = "All of the time")
    ),
    k6.depressed = as.numeric(fct_recode(as_factor(q44),
                                         "1" = "None of the time",
                                         "3" = "Some of the time",
                                         "4" = "Most of the time",
                                         "5" = "All of the time")
    ),
    k6.effort = as.numeric(fct_recode(as_factor(q36),
                                      "1" = "None of the time",
                                      "2" = "A little of the time",
                                      "3" = "Some of the time",
                                      "4" = "Most of the time",
                                      "5" = "All of the time")
    ),
    k6.worthless = as.numeric(fct_recode(as_factor(q37),
                                         "1" = "None of the time",
                                         "2" = "A little of the time",
                                         "3" = "Some of the time",
                                         "4" = "Most of the time",
                                         "5" = "All of the time")
    ),
    k6.physical.cause = as.numeric(fct_recode(as_factor(q43),
                                              "1" = "None of the time",
                                              "2" = "A little of the time", # Doesn't contain this option
                                              "3" = "Some of the time",
                                              "4" = "Most of the time",
                                              "5" = "All of the time")
    ),
    k6.total = k6.nervous + k6.hopeless + k6.restless + k6.depressed + k6.effort + k6.worthless,
  )


#### STAXI ####


#### Summary ####
# Key dems and totals of assessments


#### DDDI ####
dd.df <- survey.raw %>% 
  filter(status == "IP Address",
         q126.13 %in% c("Full license (current/valid)", "Full license (expired/revoked)")) %>% 
  transmute(
    id = id,
    dd.ne.1 = as.numeric(fct_recode(as_factor(q65),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.ne.2 = as.numeric(fct_recode(as_factor(q66),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.ne.3 = as.numeric(fct_recode(as_factor(q67),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.ad.1 = as.numeric(fct_recode(as_factor(q68),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.ad.2 = as.numeric(fct_recode(as_factor(q69),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.ad.3 = as.numeric(fct_recode(as_factor(q70),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.ad.4 = as.numeric(fct_recode(as_factor(q73),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.rd.1 = as.numeric(fct_recode(as_factor(q74),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.ad.5 = as.numeric(fct_recode(as_factor(q75),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.rd.2 = as.numeric(fct_recode(as_factor(q76),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.ad.6 = as.numeric(fct_recode(as_factor(q77),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.ne.4 = as.numeric(fct_recode(as_factor(q79),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.rd.3 = as.numeric(fct_recode(as_factor(q80),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.rd.4 = as.numeric(fct_recode(as_factor(q81),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.ad.7 = as.numeric(fct_recode(as_factor(q82),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.ne.5 = as.numeric(fct_recode(as_factor(q83),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.ne.6 = as.numeric(fct_recode(as_factor(q84),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.ne.7 = as.numeric(fct_recode(as_factor(q85),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.rd.5 = as.numeric(fct_recode(as_factor(q86),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.rd.6 = as.numeric(fct_recode(as_factor(q87),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.ne.8 = as.numeric(fct_recode(as_factor(q88),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.rd.7 = as.numeric(fct_recode(as_factor(q89),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.rd.8 = as.numeric(fct_recode(as_factor(q90),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.rd.9 = as.numeric(fct_recode(as_factor(q91),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.ne.9 = as.numeric(fct_recode(as_factor(q92),
                                    "1" = "Never",
                                    "2" = "Rarely",
                                    "3" = "Sometimes",
                                    "4" = "Often",
                                    "5" = "Always")
    ),
    dd.rd.10 = as.numeric(fct_recode(as_factor(q93),
                                     "1" = "Never",
                                     "2" = "Rarely",
                                     "3" = "Sometimes",
                                     "4" = "Often",
                                     "5" = "Always")
    ),
    dd.rd.11 = as.numeric(fct_recode(as_factor(q94),
                                     "1" = "Never",
                                     "2" = "Rarely",
                                     "3" = "Sometimes",
                                     "4" = "Often",
                                     "5" = "Always")
    ),
    ## DDDI summing up scores
    dd.ne.total = dd.ne.1 + dd.ne.2 + dd.ne.3 + dd.ne.4 + dd.ne.5 + dd.ne.6 + dd.ne.7 + dd.ne.8 + dd.ne.9,
    dd.ad.total = dd.ad.1 + dd.ad.2 + dd.ad.3 + dd.ad.4 + dd.ad.5 + dd.ad.6 + dd.ad.7,
    dd.rd.total = dd.rd.1 + dd.rd.2 + dd.rd.3 + dd.rd.4 + dd.rd.5 + dd.rd.6 + dd.rd.7 + dd.rd.8 + dd.rd.9 + dd.rd.10 + 
      dd.rd.11,
    dd.total = dd.ne.total + dd.ad.total + dd.rd.total,
    keep.dd = !is.na(dd.total))





# survey.df <- survey.raw %>% 
#   select(-c(status, recordeddate, userlanguage, q.recaptchascore, responseid)) %>% 
#   filter(str_detect(q126.13, "Full license"), finished == FALSE) %>% 
#   count(finished)
  # Add ID for each

save.image(file = "objects/all-objects.RData")


