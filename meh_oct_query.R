
#  THIS IS HOW WE ARRIVED AT WHAT WENT FOR FOVEAL ANNOTATION

#########################################
# MEH dataset from Report 1
#########################################
### ### From Report 1 ### ###
# those graded & with pathology annotated per 
d_meh_report1_pathology <-  as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/dataset_meh_meta.csv", header = T, stringsAsFactors = FALSE)) %>%
  select(patkey, eye, pathology, X2.meta.exam.scan_datetime) %>%
  mutate(
    patkey = as.character(patkey),
    pathology = case_when(
      pathology == "GA" ~ "ga", 
      pathology == "ga other" ~ "ga", 
      pathology == "ga amn other" ~ "ga", 
      pathology == "ERM" ~ "erm", 
      pathology == "PED" ~ "ped", 
      pathology == "PED and other" ~ "ped", 
      pathology == "ga ped other" ~ "ped", 
      pathology == "ped other" ~ "ped", 
      pathology == "ped , other" ~ "ped", 
      pathology == "other" ~ "namd", 
      pathology == "IRF SRF" ~ "namd", 
      T ~ "ga"),
    dat_scan = dmy_hm(X2.meta.exam.scan_datetime )) %>%
  mutate(dat_scan = date(dat_scan))  %>%
  select(-X2.meta.exam.scan_datetime)

# filepaths of all that were originally extracted 
# They only graded 192 out of 195 - maybe because of quality
# Patients 110 Eyes 171
d_meh_report1 <-  as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/dataset_meh_patkey.csv", header = T, stringsAsFactors = FALSE)) %>%
  mutate(patkey = as.character(patkey), 
         dat_scan = dmy_hm(dat_scan)) %>%
  mutate(dat_scan = date(dat_scan))  %>%
  select(patkey, eye, dat_scan, filepath) %>%
  inner_join(d_meh_report1_pathology, by = c("patkey", "eye", "dat_scan"))


### ### Compile a dataset of 1 scan per eye with preference for GAs
# GA: Pt 91 Eyes 142
d_meh_report1_ga <- d_meh_report1 %>%
  filter(pathology == "ga") %>%
  distinct(patkey, eye,.keep_all = T) %>%
  ungroup()

count(d_meh_report1_ga %>% distinct(patkey))
count(d_meh_report1_ga %>% distinct(patkey,eye))

# Non-GA: Eyes 29
d_meh_report1_nonga <- d_meh_report1 %>%
  anti_join(d_meh_report1_ga, by = c("patkey", "eye")) %>%
  distinct(patkey, eye,.keep_all = T) %>%
  ungroup()


d_meh_report1 <- rbind(d_meh_report1_ga, d_meh_report1_nonga ) 

# Join on VAs within 90 days
d_meh_report1_va_match <- d_meh_report1 %>%
  left_join(all_va, by = c("patkey", "eye")) %>%
  filter(abs(dat_scan - app_dat) < 90) %>%
  group_by(patkey, eye) %>%
  slice(which.min(abs(dat_scan - app_dat))) %>%
  ungroup()


# take this forward for NOW , but keep in mind that # 46 with VAs ; ;125 withOUT
d_meh_report1 <- d_meh_report1 %>%
  left_join(d_meh_report1_va_match) 


#########################################
# MEH dataset outwidth Report 1
#########################################

d_meh <-  as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/210120_meh_ga_octs_query.csv", header = T, stringsAsFactors = FALSE)) %>%
  mutate(
    patkey = as.character(PatKey),
    eye = case_when(
      laterality == "R" ~ "r", 
      laterality == "L" ~ "l"),
    dob = ymd(DOB),
    dat_scan = ymd_hms(examDateTime)) %>%
  mutate(dat_scan = date(dat_scan)) %>%
  select(patkey, eye, dob, dat_scan, directoryUncPath, fileName) %>%
  filter(directoryUncPath == "//MEHHEYEX.moorfields.nhs.uk/heyex-new/patients" 
         | directoryUncPath == "//MEHHEYEX.moorfields.nhs.uk/heyex/patients") #These were the only subfolders we had access to


count(d_meh %>% distinct(patkey)) #2580
count(d_meh %>% distinct(patkey,eye)) #4991
count(d_meh %>% distinct(patkey,eye, dat_scan)) #15,268 i.e. about 3 scans per person

multi_scan <- d_meh %>%
  distinct(patkey, eye, dat_scan) %>% # i.e. want the scans to be on different dates
  count(patkey,eye) %>%
  filter(n >= 3)


count(multi_scan %>% distinct(patkey)) # 421
count(multi_scan %>% distinct(patkey,eye)) # 776

# These would be a selection of MEH OCTs that are likely to be GA & with VAs
# but do NOT have Foveas annotated
d_meh_foveal_annotated <- d_meh %>%
  semi_join(multi_scan, by = c("patkey", "eye")) %>% # more than 6 scans on 6 different dates
  anti_join(d_meh_report1, by = "patkey") %>%
  anti_join(va_injamd, by = c("patkey", "eye")) %>% # ie not been injected
  
  left_join(all_va, by = c("patkey", "eye")) %>%
  filter( dat_scan == app_dat) %>% # its a direct match here but we can go more loose of course
  group_by(patkey, eye) %>%
  slice(which.min(abs(dat_scan - app_dat))) %>%
  ungroup() %>%
  # filter(dat_scan < "2018-01-01") %>%
  distinct(patkey, .keep_all = T) %>% # 1 eye per patient
  head(200)




d_meh_all <- rbind(
  (d_meh_report1 %>%
     select(patkey, eye, dat_scan, filepath, va) %>%
     mutate(directoryUncPath = NA)),
  (d_meh_foveal_annotated %>%
     mutate(filepath = fileName) %>% 
     select(patkey, eye, dat_scan, filepath, va, directoryUncPath)
  ))


########## Now complete ##########
# Export for foveal annotation
#  forfovealannotate <- d_meh_all
#  write.csv(forfovealannotate, file = "210121_meh_tobeannotated.csv")
########## ########## ##########


