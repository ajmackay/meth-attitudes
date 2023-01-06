# source("scripts/load-packages.R")


# Import ------------------------------------------------------------------
survey.files <- list.files("data/", pattern = "*Meth-Survey.csv")
survey.file <- survey.files[length(survey.files)]

#### File moving ####
to.move <- survey.files[!survey.files == survey.file]
old.path <- str_c("data/", to.move)
new.path <- str_c("data/archive/", to.move)

if(!is_empty(to.move)){
  
  if(file.copy(old.path, new.path)){
    file.remove(old.path)
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
    mutate(id = row_number(),
           .before = 1)
  
  saveRDS(survey.raw, file = "objects/survey-raw.RData")
  
}else survey.raw <- readRDS("objects/survey-raw.RData")

# Contains all responses (cleaned - not filtered)
survey.df <- survey.raw %>% 
  mutate(ma.ingest = fct_recode(as_factor(q47)),
            ma.ingest = ma.ingest == "Yes",
            ma.ingest = replace_na(ma.ingest, FALSE),
            ma.most.common = q48 == "Yes",
         consent = q25 == "Yes",
         age = q19,
         
         .keep = "unused")

survey.df %>% 
  filter(ma.ingest, status!= "Spam", ma.most.common, finished)

# Data Processing -------------------------------------------------------------------
#### Screened ####
# survey.screened <- survey.raw %>% 
#   filter(status == "IP Address",
#          q126.13 %in% c("Full license (current/valid)", "Full license (expired/revoked)"),
#          !is.na(q19)) %>% # Age (forced entry) 
#   mutate(ma.ingest = fct_recode(as_factor(q47)) %>% replace_na("No"),
#          ma.ingest = ma.ingest == "Yes",
#          ma.most.common = q48 == "Yes") %>% 
#   filter(!ma.ingest | ma.ingest & ma.most.common)


survey.screened <- survey.raw %>% 
  filter(status == "IP Address",
         q126.13 %in% c("Full license (current/valid)", "Full license (expired/revoked)"),
         !is.na(q19)) %>% # Age (forced entry) 
  mutate(ma.ingest = fct_recode(as_factor(q47)),
         ma.ingest = ma.ingest == "Yes",
         ma.ingest = replace_na(ma.ingest, FALSE),
         ma.most.common = q48 == "Yes") %>% 
  filter(!ma.ingest | ma.ingest & ma.most.common)


#### Demographics ####
dems.df <- survey.screened %>% 
  transmute(id = id,
            ma.ingest = ma.ingest,
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
            education = fct_collapse(as_factor(q20),
                                   "University Degree" = c("Bachelor Degree", "Master’s Degree", "Postgraduate Degree"),
                                   "Highschool/Technical Degree" = c("High School Diploma", "Vocational/Technical degree or certificate"),
                                   "Did not finish University" = "Attended University but did not finish",
                                   "Did not finish High School" = "Attended high school but did not finish"
                                   # "High School Diploma" = "High School Diploma",
                                   # "Vocational/Technical degree or certificate" = "Vocational/Technical degree or certificate",
                                   
                                  
                                   # "Bachelor Degree" = "Bachelor Degree",
                                   # "Postgraduate Degree" = "Master’s Degree",
                                   # "Postgraduate Degree" = "Postgraduate Degree",
                                   
            ),
            employment.status = as_factor(q17),
            area.live = fct_collapse(as_factor(q21),
                                     "Rural/Suburban" = c("Rural", "Suburban"),
                                     "Urban/Inner-City" = "Urban/Inner-city"
            ),
            license.status = as_factor(q22),
            alcohol.ever = as_factor(q28) == "Yes", # Convert to logical
            dems.full = if_else(is.na(age) | is.na(sex) | is.na(education) | is.na(area.live), FALSE, TRUE) # Removed Ethnicity as it's vague
            )



#### AUDIT ####
alcohol.ids <- dems.df %>% 
  filter(alcohol.ever) %>% pull(id)

audit.df <- survey.screened %>% 
  transmute(
    id = id,
    ma.ingest = ma.ingest,
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
    audit.risky = ifelse((q13 == "Male" & audit.total > 4 | 
                            q13 == "Female" & audit.total > 2), TRUE, FALSE), # https://www.hepatitis.va.gov/alcohol/treatment/audit-c.asp (also see protocol)
    audit.full = !(id %in% alcohol.ids & is.na(audit.total)),
    audit.total = if_else(audit.full & is.na(audit.total), 0, audit.total))

#### Drug Use ####
druguse.df <- survey.screened %>% 
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
ma.df <- survey.screened %>% 
  filter(q47 == "Yes") %>% 
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
    # ma.use.ways = q54,
    ma.use.ways = case_when(
      str_detect(q54, "Injection") ~ "Injection",
      str_detect(q54, "IV") ~ "Injection",
      str_detect(q54, "Smoking") ~ "Smoking",
      str_detect(q54, "Snorting") ~ "Snorting",
      str_detect(q54, "mouth") ~ "Oral",
      TRUE ~ NA_character_
    ),
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
    sds.full = !is.na(sds.total),
    ma.type = ifelse(sds.total > 4, "MUD", "Recreational"),
    ma.want.to.change = as_factor(q61),
    ma.could.change = as_factor(q62)
  )

ma.id <- ma.df %>% pull(id)

#### K6 ####
k6.df <- survey.screened %>% 
  transmute(
    id = id,
    ma.ingest = ma.ingest,
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
                                              # "2" = "A little of the time", # Doesn't contain this option
                                              "3" = "Some of the time",
                                              "4" = "Most of the time",
                                              "5" = "All of the time")
    ),
    k6.total = k6.nervous + k6.hopeless + k6.restless + k6.depressed + k6.effort + k6.worthless,
    k6.full = !is.na(k6.total),
    
    psychiatric.diagnosis = q46
  )


#### STAXI ####
##### State #####
state.df <- 
  survey.screened %>% 
  rename_with(~str_c("state.", seq(1, 14)),
              .cols = q146.1:q146.14) %>% 
  mutate(across(starts_with("state."), ~case_when(
    .x == "Not at all" ~ 1,
    .x == "Somewhat" ~ 2,
    .x == "Moderately so" ~ 3,
    .x == "Very much so" ~ 4))) %>% 
  mutate(state.total = select(., state.1:state.14) %>% 
           rowSums(),
         state.full = !is.na(state.total)) %>% 
  select(id, ma.ingest, starts_with("state"))

##### Trait #####
trait.df <- survey.screened %>% 
  rename_with(~str_c("trait.", seq(1, 10)),
              .cols = q147.1:q147.10) %>% 
  mutate(across(starts_with("trait."), ~case_when(
    .x == "Almost never" ~ 1,
    .x == "Sometimes" ~ 2,
    .x == "Often" ~ 3,
    .x == "Almost always" ~ 4
  ))) %>% 
  mutate(trait.total = select(., starts_with("trait.")) %>% 
                                rowSums(),
                              trait.full = !is.na(trait.total)) %>% 
           select(id, ma.ingest, starts_with("trait"))


# STAXI
staxi.df <- left_join(state.df, trait.df)


#### DDDI ####
dd.df <- survey.screened %>% 
  transmute(
    id = id,
    ma.ingest = ma.ingest,
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
    ## DDDI summing up scores - TODO make neater
    dd.ne.total = dd.ne.1 + dd.ne.2 + dd.ne.3 + dd.ne.4 + dd.ne.5 + dd.ne.6 + dd.ne.7 + dd.ne.8 + dd.ne.9,
    dd.ad.total = dd.ad.1 + dd.ad.2 + dd.ad.3 + dd.ad.4 + dd.ad.5 + dd.ad.6 + dd.ad.7,
    dd.rd.total = dd.rd.1 + dd.rd.2 + dd.rd.3 + dd.rd.4 + dd.rd.5 + dd.rd.6 + dd.rd.7 + dd.rd.8 + dd.rd.9 + dd.rd.10 + 
      dd.rd.11,
    dd.total = dd.ne.total + dd.ad.total + dd.rd.total,
    dd.full = !is.na(dd.total))


#### Instances/Attitudes/Strategies ####
##### DUI Instance and Attitude #####
dui.inst.att.df <- survey.screened %>% 
  # Instances
  rename_with(~str_c("dui.inst.", c("revoked", "hurt")),
              .cols = c(q95, q96)) %>% 
  mutate(dui.inst.revoked = dui.inst.revoked == "Yes",
         dui.inst.hurt = dui.inst.hurt == "Yes") %>% 
  
  # Attitudes
  rename_with(~str_c("dui.att.", c("friends", "drunk", "jail", "strict", "police", 
                                  "caught", "once.while", "dumb", "overrated", "lose")),
              .cols = q97:q106) %>% 
  mutate(# Strongly agree = 1
         across(.cols = str_c("dui.att.", c("jail", "strict", "police", "dumb", "lose")), ~case_when(
           .x == "Strongly Agree" ~ 1,
           .x == "Agree" ~ 2,
           .x == "Somewhat agree" ~ 3,
           .x == "Neither agree nor disagree" ~ 4,
           .x == "Somewhat disagree" ~ 5,
           .x == "Disagree" ~ 6,
           .x == "Strongly disagree" ~ 7)),
         
         #Reverse Scoring
         across(.cols = str_c("dui.att.", c("friends", "drunk", "caught", "once.while", "overrated")), ~case_when(
           .x == "Strongly Agree" ~ 7,
           .x == "Agree" ~ 6,
           .x == "Somewhat agree" ~ 5,
           .x == "Neither agree nor disagree" ~ 4,
           .x == "Somewhat disagree" ~ 3,
           .x == "Disagree" ~ 2,
           .x == "Strongly disagree" ~ 1))
  ) %>% 
  mutate(dui.att.total = select(., starts_with("dui.att")) %>% 
           rowSums(na.rm = FALSE),
         dui.att.full = !is.na(dui.att.total)) %>% 
  select(id, ma.ingest, starts_with("dui"))

dui.att.df <- dui.inst.att.df %>% 
  select(id, ma.ingest, starts_with("dui.att"))

dui.inst.df <- dui.inst.att.df %>% 
  select(id, ma.ingest, starts_with("dui.inst"))


##### DUI Strat #####
dui.strat.df <- survey.screened %>% 
  rename_with(~str_c("dui.strat.", c("leave", "light", "driver", "use", "taxi", "track", "overnight")),
              .cols = q107:q113) %>% 
  mutate(across(.cols = starts_with("dui.strat"), ~case_when(
    .x == "Definitely yes" ~ 1,
    .x == "Probably yes" ~ 2,
    .x == "Might or might not" ~ 3,
    .x == "Probably not" ~ 4,
    .x == "Definitely not" ~5,
    
    .x == "Extremely likely" ~ 1,
    .x == "Moderately likely" ~ 2, 
    .x == "Slightly likely" ~ 2,
    .x == "Neither likely nor unlikely" ~ 3,
    .x == "Slightly unlikely" ~ 4,
    .x == "Moderately unlikely" ~ 4,
    .x == "Extremely unlikely" ~5)),
    
  ) %>% 
  mutate(dui.strat.total = select(., starts_with("dui.strat")) %>% 
           rowSums(na.rm = FALSE),
         dui.strat.full = !is.na(dui.strat.total)) %>% 
  select(id, ma.ingest, starts_with("dui.strat"))

###### DUI Strat Dichotomous ######
dui.strat.dich.df <- survey.screened %>% 
  rename_with(~str_c("dui.strat.dich.", c("leave", "light", "driver", "use", "taxi", "track", "overnight")),
              .cols = q107:q113) %>% 
  mutate(across(.cols = starts_with("dui.strat.dich"), ~case_when(
    .x == "Definitely yes" | 
      .x == "Probably yes" |
      .x == "Extremely likely" |
      .x == "Moderately likely" |
      .x == "Slightly likely" ~ TRUE,
    .x == "Might or might not" | 
      .x == "Probably not" |
      .x == "Definitely not" |
      .x == "Neither likely nor unlikely" |
      .x == "Slightly unlikely" |
      .x == "Moderately unlikely" |
      .x == "Extremely unlikely" ~ FALSE))
  ) %>% 
  select(id, ma.ingest, starts_with("dui.strat"))


##### DUID Instances and Attitudes #####
duid.inst.att.df <- survey.screened %>% 
  # filter(id %in% ma.id) %>% 
  #Instances
  rename_with(~str_c("duid.inst.", c("ever", "last12months", "before12months", 
                                        "revoked", "hurt")),
              .cols = c(q114, q118, q120, q122, q123)) %>% 
  rename(drugs.last.12 = q119,
         drugs.before.12 = q121,
         drugs.revoked = q124,
         drugs.hurt = q125) %>% 
  rename_with(~str_c("duid.att.", c("friends", "high", "jail", "strict", "police", 
                                    "caught", "once.while", "dumb", "overrated", "lose")),
              .cols = c(q126.75, q128:q136)) %>% 
  
  mutate(
    # Instances
    across(.cols = starts_with("duid.inst"), ~case_when(
    .x == "Yes" ~ TRUE,
    .x == "No" ~ FALSE
  )),
  # Attitudes
  # Strongly agree = 1
  across(.cols = str_c("duid.att.", c("jail", "strict", "police", "dumb", "lose")), ~case_when(
    .x == "Strongly Agree" ~ 1,
    .x == "Agree" ~ 2,
    .x == "Somewhat agree" ~ 3,
    .x == "Neither agree nor disagree" ~ 4,
    .x == "Somewhat disagree" ~ 5,
    .x == "Disagree" ~ 6,
    .x == "Strongly disagree" ~ 7)),
  
  #Reverse Scoring
  across(.cols = str_c("duid.att.", c("friends", "high", "caught", "once.while", "overrated")), ~case_when(
    .x == "Strongly Agree" ~ 7,
    .x == "Agree" ~ 6,
    .x == "Somewhat agree" ~ 5,
    .x == "Neither agree nor disagree" ~ 4,
    .x == "Somewhat disagree" ~ 3,
    .x == "Disagree" ~ 2,
    .x == "Strongly disagree" ~ 1)),
  ) %>%
  mutate(
    duid.inst.total = select(., starts_with("duid.ins")) %>% 
      rowSums(na.rm = FALSE),
    duid.inst.full = !is.na(duid.inst.total),
         duid.att.total = select(., duid.att.friends:duid.att.lose) %>% rowSums(na.rm = FALSE),
         duid.att.full = !is.na(duid.att.total)) %>% 
  select(id, ma.ingest, starts_with("duid"), duid.att.total)




duid.att.df <- duid.inst.att.df %>% 
  select(id, ma.ingest, starts_with("duid.att"))

duid.inst.df <- duid.inst.att.df %>% 
  select(id, ma.ingest, starts_with("duid.inst"))

##### DUID Strat #####
duid.strat.df <- survey.screened %>% 
  filter(ma.ingest) %>% 
  rename_with(~str_c("duid.strat.", c("leave", "less", "driver", "use", "taxi", "track", "overnight")),
              .cols = q137:q143) %>% 
  mutate(across(.cols = starts_with("duid.strat"), ~case_when(
    .x == "Definitely yes" ~ 1,
    .x == "Probably yes" ~ 2,
    .x == "Might or might not" ~ 3,
    .x == "Probably not" ~ 4,
    .x == "Definitely not" ~5,
    
    .x == "Extremely likely" ~ 1,
    .x == "Moderately likely" ~ 2, 
    .x == "Slightly likely" ~ 2,
    .x == "Neither likely nor unlikely" ~ 3,
    .x == "Slightly unlikely" ~ 4,
    .x == "Moderately unlikely" ~ 4,
    .x == "Extremely unlikely" ~5)),
    
  ) %>% 
  mutate(duid.strat.total = select(., starts_with("duid.strat")) %>% rowSums(na.rm = FALSE),
         duid.strat.full = !is.na(duid.strat.total)) %>% 
  select(id, ma.ingest, starts_with("duid.strat"))

# Might be worth checking this
duid.strat.dich.df <- duid.strat.df %>%
  select(-c(duid.strat.total, duid.strat.full)) %>% 
  mutate(across(.cols = starts_with("duid.strat"), ~case_when(
    .x >= 3 ~ TRUE,
    .x < 3 ~ FALSE
  ))) %>% 
  mutate(duid.strat.total = select(., starts_with("duid.strat")) %>% 
           rowSums(na.rm = FALSE))


#### Summary ####
# Key dems and totals of assessments
summ.df <- dems.df %>% 
  left_join(select(audit.df, id, audit.total, audit.risky, audit.full)) %>% 
  left_join(select(ma.df, id, ma.use.peak, sds.total, sds.full, ma.type, ma.use.age)) %>% 
  left_join(select(k6.df, id, k6.total, k6.full, psychiatric.diagnosis)) %>% 
  left_join(select(staxi.df, id, state.total, state.full, trait.total, trait.full)) %>% 
  left_join(select(dd.df, id, dd.ne.total, dd.ad.total, dd.rd.total, dd.total, dd.full)) %>% 
  left_join(select(dui.inst.att.df, id, dui.inst.revoked, dui.att.total, dui.att.full)) %>% 
  left_join(select(dui.strat.df, id, dui.strat.total, dui.strat.full)) %>%
  left_join(select(duid.inst.att.df, id, duid.inst.revoked, duid.att.total, duid.att.full)) %>% 
  left_join(select(duid.strat.df, id, duid.strat.total, duid.strat.full))


# Final Data for Analyses -------------------------------------------------
#### MA group ####

ma.id <- summ.df %>% 
  filter(ma.ingest,
         dems.full,
         audit.full,
         sds.full,
         k6.full,
         trait.full,
         dd.full,
         # dui.strat.full,
         # duid.strat.full,
         # dui.att.full,
         # duid.att.full
  ) %>% pull(id)


ma.final <- summ.df %>% 
  filter(id %in% ma.id) %>% 
  select(id, age, sex, education, area.live,
         audit.total, sds.total, k6.total, trait.total, 
         dd.ne.total, dd.ad.total, dd.rd.total, dd.total)

ma.final <- ma.final %>% 
  mutate(sex = factor(sex, levels = c("Male", "Female")))


# ma.final <- ma.final %>% 
#   mutate(education = fct_collapse(education,
#                                   "University Degree" = c("Bachelor Degree", "Postgraduate Degree"),
#                                   "Highschool/Technical Degree" = c("Vocational/Technical degree or certificate",
#                                                                     "High School Diploma")),
#          area.live = fct_collapse(area.live,
#                                   "Rural/Suburban" = c("Rural", "Suburban")))

ma.dems <- ma.df %>% 
  filter(id %in% ma.id) %>% 
  left_join(dems.df) %>% 
  select(id, age, sex, education, area.live,
         ma.use.peak, ma.use.age, sds.total, dependent = ma.type) %>% 
  mutate(dependent = dependent == "MUD")


#### Save Image ####
save.image(file = "objects/all-objects.RData")



