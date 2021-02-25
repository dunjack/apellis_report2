

folder <- setwd("/Users/Dunistrator/Documents/MEH_data/apellis/fovea/")
files = list.files(path = "/Users/Dunistrator/Documents/MEH_data/apellis/fovea/", 
                   pattern="*.csv")


d_fovea <- do.call(bind_rows, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))  %>%
  mutate(
    pat = case_when(
      is.na(pat) ~ pid, 
      T ~ pat),
    sdb = case_when(
      is.na(sdb) ~ sid, 
      T ~ sdb)) %>%
  filter(gradable == "Yes")


 setwd("/Users/dunistrator/Desktop/Dropbox/dj_desktop/Research/")

x <- df %>% 
  # filter(cohort =="filly2") %>%
  anti_join(d_fovea, by = c("pat", "sdb")) %>% 
  left_join(filepaths, by = c("pat", "sdb"))


filepaths <- as.data.frame(read.csv("/Users/Dunistrator/Documents/MEH_data/apellis/trial_cohort/210222_df_complete.csv", header = T, stringsAsFactors = FALSE)) %>%
  select(pat, sdb, filepath)
  

write.csv(x, file = "210225_fillyremaining.csv")