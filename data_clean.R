# Packages
library(dplyr)
library(tidyr)
library(table1)
library(data.table)
library(lubridate)
library(stringr)
library(scales)
library(fuzzyjoin)
library(stringr)
library(ggplot2)
library(eye)


#############
# BASELINE INFO 
# dimpat
dimpat <- as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/190915_dimpat.csv", header = TRUE, stringsAsFactors = FALSE)) %>%
  mutate(patkey = as.character(PatKey),
         dob = ymd(DOB),
         gender = case_when(
           SexName == "Male" ~ "m",
           SexName == "Female" ~ "f"),
         ethnicity = case_when(
           EthnicityName == "Indian" ~ "se_asian",
           EthnicityName == "British" ~ "caucasian",
           EthnicityName == "Caribbean" ~ "afrocarribean",
           EthnicityName == "African" ~ "afrocarribean",
           EthnicityName == "Pakistani" ~ "se_asian",
           EthnicityName == "Chinese" ~ "chinese",
           EthnicityName == "Bangladeshi" ~ "se_asian",
           EthnicityName == "Irish" ~ "caucasian",
           EthnicityName == "Any other White background" ~ "caucasian",
           EthnicityName == "Any other Black background" ~ "afrocarribean",
           EthnicityName == "Any other Asian background" ~ "se_asian",
           EthnicityName == "White and Black African" ~ "mixed",
           EthnicityName == "White and Black Caribbean" ~ "mixed",
           EthnicityName == "Any other mixed background" ~ "mixed",
           T~ "unknown"),
         postcode = PostCode) %>%
  select(patkey, gender, dob, ethnicity, postcode) %>%
  distinct(patkey, .keep_all = T)


################################
# Collecting VA
################################
# Inj amd -VA
va_injamd <- as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/AMD/version_20181121.csv", header = TRUE, stringsAsFactors = FALSE)) %>%
  mutate(
    patkey = as.character(PatKey),
    eye = case_when(
      Eye == "Right" ~ "r", 
      Eye == "Left" ~ "l"),
    patkey = as.character(PatKey),
    app_dat = ymd(AppointmentDate),
    va = as.integer(VA_ETDRS)) %>% 
  select(patkey, eye, app_dat, va, PriorAvastin, PriorMacugen, PriorPDT, PDT, PriorLaser) %>%
  filter(!is.na(PriorAvastin) | 
           !is.na(PriorMacugen) | 
           !is.na(PriorPDT) | 
           !is.na(PDT) | 
           !is.na(PriorLaser)) %>%
  filter(!is.na(va)) %>%
  select(patkey, eye, app_dat, va) 

# other inj va
va_inj <-  as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/20200211_injva.csv", header = TRUE, stringsAsFactors = FALSE)) %>%
  mutate(
    patkey = as.character(Patkey),
    app_dat = ymd(ExaminationDateKeyN),
    l = va(LvalueEtdrs, to = "etdrs"),
    r = va(RvalueEtdrs, to = "etdrs")) %>%
  select(patkey, app_dat, l, r)  %>%
  gather(l, r, key = "eye", value = 'va' )%>%
  filter(!is.na(va)) 


# non_inj VA
va_noninj <-  as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/20200211_noninjva.csv", header = TRUE, stringsAsFactors = FALSE))  %>%
  mutate(
    patkey = as.character(Patkey),
    app_dat = ymd(ExaminationDateKeyN),
    l = va(LvalueEtdrs, to = "etdrs"),
    r = va(RvalueEtdrs, to = "etdrs")) %>%
  select(patkey, app_dat, l, r)  %>%
  gather(l, r, key = "eye", value = 'va' )%>%
  filter(!is.na(va)) 


# manual VAs from daniela

meh_match <-  as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/210121_meh_tobeannotatedd_abspath.csv", header = TRUE, stringsAsFactors = FALSE)) %>%
  mutate(sdb = str_extract(filepath, ".........sdb"),
         app_dat = dmy(dat_scan)) %>%
  select(patkey, app_dat, sdb, eye)

va_manual <-  as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/210129_meh_va.csv", header = TRUE, stringsAsFactors = FALSE)) %>%
  left_join(meh_match, by = "sdb") %>%
  mutate(va = va(VA, to = "etdrs")) %>%
  select(patkey, eye, app_dat, va)


####
all_va <- rbind(va_inj, va_injamd,va_noninj) %>%
  rbind(va_manual) %>%
  filter(!is.na(va)) %>%
  distinct(patkey, app_dat, eye, va)


#########################################
# MEH dataset 
#########################################
d_meh <-  as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/210202_meh_annotated.csv", header = T, stringsAsFactors = FALSE))  %>%
  mutate(
    patkey = as.character(patkey),
    dat_scan = dmy(dat_scan),
    pathology = case_when(
      GA == "No" ~ "not_ga",
      T ~"ga")) %>%
  filter(gradable == "Yes") %>%
  select(patkey, eye, dat_scan, pathology, sdb, pat) %>%
  left_join(all_va, by = c("patkey", "eye")) %>% # join VAs < 90
  filter(abs(dat_scan - app_dat) < 90) %>%
  group_by(patkey, eye) %>%
  slice(which.min(abs(dat_scan - app_dat))) %>%
  ungroup()  %>%
  left_join(dimpat, by = "patkey") %>%
  mutate(age = interval(dob, dat_scan) / years(1),
         cohort = "meh") %>%
  group_by(patkey,eye) %>% # Allowing for 2 eyes per patient
  arrange(pathology) %>% # preferencing GA over Non-GA
  slice_head() %>%
  ungroup() %>%
  filter(pathology == "ga") %>%
  mutate(Study.Fellow = "")


#########################################
# Apellis dataset
#########################################
# Ethnicity
d_apel_ethnicity <- as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/210131_apellis_events_with_scans.csv", header = T, stringsAsFactors = FALSE)) %>%
  mutate(patkey = as.character(Subject)) %>%
  mutate(ethnicity = case_when(
    ASIAN == "1"~ "asian",
    AMINDIAN == "1"~ "indian",
    BLACK == "1"~ "black",
    HAWAIIAN == "1"~ "hawaiian",
    ETHNIC == "Hispanic or Latino" ~ "hispanic or latino",
    WHITE == "1"~ "caucasian",
    OTHER == "1" ~ "other")) %>%
  distinct(patkey, ethnicity)  %>%
  filter(!is.na(ethnicity))


d_apel_event <-  as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/210131_apellis_events_with_scans.csv", header = T, stringsAsFactors = FALSE)) %>%
  mutate(patkey = as.character(Subject), 
         dat_app = dmy(RecordDay),
         oct = case_when(SID >= 1 ~ "1",
                         T ~ "0"),
         eye = case_when(
           BCEYE =="O.S." ~ "l",
           BCEYE =="O.D." ~ "r"), 
         va = TVAS_bcva,
         time = TargetDays,
         dob = dmy_hms(BRTHDAT),
         sdb = paste0((str_pad(SID, 8, pad = "0")),".sdb" ),
         pat = paste0((str_pad(PID, 8, pad = "0")),".pat" )) %>%
  mutate(dat_app = date(dat_app),
         gender = case_when(
           SEX == "Female" ~ "f",
           SEX == "Male" ~ "m")) %>%
  select(49:59) %>%
  mutate(cohort = "filly2") %>%
  left_join(d_apel_ethnicity, by = "patkey")


# VA
d_apel_va <- d_apel_event %>%
  filter(!is.na(va)) %>%
  mutate(dat_va = dat_app) %>%
  select(patkey, dat_va, eye, va)

# OCT
d_apel_oct <- d_apel_event %>%
  filter(oct == "1") %>%
  mutate(dat_scan = dat_app)  %>%
  select(patkey, eye, dat_scan, pat, sdb)  
  
# Apellis baseline
d_apel_base <- d_apel_event %>%
  filter(
    Study.Fellow == "S"
         &
      time <= 0) %>%
  select(-va, -sdb, -pat) %>%
  mutate(age = interval(dob, dat_app) / years(1)) %>% # this is baseline age
  select(-dob) %>%
  # integrate OCT & VA
  # left_join(d_apel_oct, by = c("patkey", "eye")) %>%
  # filter(abs(dat_app - dat_scan) < 30) %>%
  left_join(d_apel_va, by = c("patkey", "eye")) %>%  
  filter(abs(dat_app - dat_va) < 30) %>%
  group_by(patkey, eye) %>%
  slice(which.min(time)) %>%
  ungroup() %>%
  mutate(pathology = "ga")


fellow <- d_apel_event %>%
  filter(cohort == "filly2"
         & Study.Fellow =="S") %>%
  mutate(
    Study.Fellow = "F",
    eye = case_when(
      eye =="r" ~ "l",
      eye =="l" ~ "r"), 
    status = "missing",
    fovea = "no_annotation") %>% 
  distinct(patkey, eye ,.keep_all = T) %>%
  select(-va , -pat, -sdb,-time)

  
#########################################
# DATAFRAME Merge
#########################################
# All ethnicity
all_ethnicity <- dimpat %>%
  select(patkey, ethnicity) %>%
  rbind(d_apel_ethnicity)

#
df_all <-  d_meh %>%
  mutate(dat_app = ymd(app_dat),
         dat_scan = ymd(dat_scan),
         time = 0) %>%
  select(-postcode, -dob, -app_dat, -dat_scan, -sdb, -pat) %>%
  rbind((d_apel_base %>%
           select(-oct, -dat_va)))
  

# What Sophie has so far
df <- as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/210204_df_complete_v1.csv", header = T, stringsAsFactors = FALSE))  %>%
  mutate(
    patkey = as.character(patkey),
    dat_app = ymd(dat_app),
    dat_scan = ymd(dat_scan),
    sdb = paste0((str_pad(sdb, 8, pad = "0")),".sdb" ),
    pat = paste0((str_pad(pat, 8, pad = "0")),".pat" )) %>%
  select(-X) %>%
  # filter(!Study.Fellow == "F") %>%
  # Adding on OCTs that are missing but should be there
  # bind_rows is basically rbind.fill()
  bind_rows(( df_all %>%
                anti_join(df, by = c("patkey", "eye"))%>%
                mutate(va = as.numeric(va)))) %>%
  select(-ethnicity) %>%
  left_join(all_ethnicity, by = "patkey") 



######

##### All FILLY scans so far #######
d_filly_v1 <- as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/210131_apellis_events_with_scans.csv", header = T, stringsAsFactors = FALSE)) %>%
  mutate(patkey = as.character(Subject), 
         dat_app = dmy(RecordDay),
         oct = case_when(SID >= 1 ~ "1",
                         T ~ "0"),
         eye = case_when(
           BCEYE =="O.S." ~ "l",
           BCEYE =="O.D." ~ "r"), 
         va = TVAS_bcva,
         time = TargetDays,
         dob = dmy_hms(BRTHDAT),
         sdb = paste0((str_pad(SID, 8, pad = "0")),".sdb" ),
         pat = paste0((str_pad(PID, 8, pad = "0")),".pat" )) %>%
  mutate(dat_app = date(dat_app),
         gender = case_when(
           SEX == "Female" ~ "f",
           SEX == "Male" ~ "m")) %>%
  select(49:59) %>%
  filter(Study.Fellow == "S") %>%
  distinct(patkey,.keep_all = T)
  

d_filly_v2 <-  as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/210216_fillyOCTs_processing.csv", header = T, stringsAsFactors = FALSE))  %>%
  mutate(
    patkey = as.character(Subject),
    dat_app = dmy(RecordDay),
    dat_scan = dmy(RecordDay),
    sdb = paste0((str_pad(SID, 8, pad = "0")),".sdb" ),
    pat = paste0((str_pad(PID, 8, pad = "0")),".pat" )) %>%
  select(-X) %>%
  filter(Study.Fellow == "S") %>%
  distinct(PID,.keep_all = T)




write.csv(filly, file="210217_filly2_annotation2.csv")







  semi_join(df_all, by = "pat")

x <- df_all %>%
  filter(cohort == "filly2") %>%
  anti_join(filly, by = c("patkey", "Study.Fellow"))


x1 <- filly %>%
  anti_join(df_all, by = c("patkey", "Study.Fellow"))
# %>%
#   bind_rows(x) %>%
#   mutate(
#     fovea = case_when(
#       status =="interpolated" ~ "annotated_interpolated",
#       T ~ "no_annotation"),
#     status = case_when(
#       status =="interpolated" ~ "segmented",
#       T ~ "missing")
#   )
# 
# 
# write.csv(df, file = "210208_dataframe.csv")
