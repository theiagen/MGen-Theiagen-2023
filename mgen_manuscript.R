################################################################
# this R code summarizes Theiagen workflows' data from Terra.bio
#
#James R. Otieno
################################################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  skimr,
  data.table,
  lubridate,
  janitor,
  aweek,
  zoo,
  jsonlite,
  ggplot2,
  tidyverse,
  gtsummary,
  rstatix,
  flextable,
  readxl,
  openxlsx,
  plotly,
  reticulate,
  dashCoreComponents,
  dashHtmlComponents,
  scales
)

#Import Supplementay Table
metadata<-loadWorkbook("Supplementary Tables.xlsx") %>% 
  sheets() %>% 
  set_names() %>% 
  map_df(~ read_excel(path = "Supplementary Tables.xlsx", sheet = .x), .id = "sheet")

monthly_data<-metadata %>%
  filter(sheet == "Table S9") %>%
  select_if(~ !all(is.na(.))) %>%
  select(-c(sheet)) %>% 
  mutate(month = as.Date(month),
         month = as.yearmon(month)) %>%
  pivot_longer(cols = c(workflow_count,submission_count,user_count),
               names_to = "metrics",
               values_to = "counts") %>%
  mutate(workflow = stringr::str_trim(sapply(strsplit(extracted_method, split = '/' ), '[',6 ))) %>%
  mutate(metrics = ifelse(grepl("workflow_count",metrics)==TRUE,"Number of Samples", metrics)) %>%
  mutate(metrics = ifelse(grepl("submission_count",metrics)==TRUE,"Number of Submissions", metrics)) %>%
  mutate(metrics = ifelse(grepl("user_count",metrics)==TRUE,"Number of Users", metrics))

alltime_data<-metadata %>%
  filter(sheet == "Table S8") %>%
  select_if(~ !all(is.na(.))) %>%
  select(-c(sheet)) %>% 
  pivot_longer(cols = c(workflow_count,submission_count,user_count),
               names_to = "metrics",
               values_to = "counts") %>%
  mutate(workflow = stringr::str_trim(sapply(strsplit(extracted_method, split = '/' ), '[',6 ))) %>%
  mutate(metrics = ifelse(grepl("workflow_count",metrics)==TRUE,"Number of Samples", metrics)) %>%
  mutate(metrics = ifelse(grepl("submission_count",metrics)==TRUE,"Number of Submissions", metrics)) %>%
  mutate(metrics = ifelse(grepl("user_count",metrics)==TRUE,"Number of Users", metrics))

wf_groups<-metadata %>%
  filter(sheet == "Table S10") %>%
  select_if(~ !all(is.na(.))) %>%
  select(-c(sheet))

#Merge with the curated groupings
monthly_data<-merge(monthly_data,wf_groups,by="workflow",all=T) %>%
  filter(!(grp3 %in% c('Development'))) %>%
  mutate(title = "") %>%
  mutate(title = ifelse(grepl("Number of Samples",metrics)==TRUE,paste0("Number of Sample Analyses Per Month Using ",grp4," Workflows"),title)) %>%
  mutate(title = ifelse(grepl("Number of Submissions",metrics)==TRUE,paste0("Number of Submissions Per Month Using ",grp4," Workflows"),title)) %>%
  mutate(title = ifelse(grepl("Number of Users",metrics)==TRUE,paste0("Number of Users Per Month Using ",grp4," Workflows"),title))

alltime_data<-merge(alltime_data,wf_groups,by="workflow",all=T) %>%
  filter(!is.na(counts)) %>%
  filter(!(grp3 %in% c('Development'))) %>%
  mutate(title = "") %>%
  mutate(title = ifelse(grepl("Number of Samples",metrics)==TRUE,paste0("Number of Sample Analyses Per Month Using ",grp4," Workflows"),title)) %>%
  mutate(title = ifelse(grepl("Number of Submissions",metrics)==TRUE,paste0("Number of Submissions Per Month Using ",grp4," Workflows"),title)) %>%
  mutate(title = ifelse(grepl("Number of Users",metrics)==TRUE,paste0("Number of Users Per Month Using ",grp4," Workflows"),title))

#Summarise sample amnalyses, submissions and users
alltime_data %>%
  filter(metrics == "Number of Samples" & grp4 == "Sample-Level") %>%
  summarise(sum(counts))

alltime_data %>%
  filter(metrics == "Number of Submissions") %>%
  summarise(sum(counts))

#Tabulate monthly data by the samples run
monthly_avgs<-monthly_data %>%
  select(grp3,metrics,counts) %>%
  rename("Workflow Type"= grp3) %>%
  group_by(`Workflow Type`,metrics) %>%
  summarise(counts = sum(counts)) %>%
  ungroup() %>%
  pivot_wider(                                      
    values_from = counts,                     
    names_from = metrics) %>%
  arrange(desc(`Number of Samples`))
write.table(monthly_avgs,file="samples_submissions_users1.tsv",sep="\t",row.names=F)

#Tabulate alltime data by the samples run
alltime_avgs<-alltime_data %>%
  select(grp3,metrics,counts) %>%
  rename("Workflow Type" = grp3) %>%
  group_by(`Workflow Type`,metrics) %>%
  summarise(counts = sum(counts)) %>%
  ungroup() %>%
  pivot_wider(                                      
    values_from = counts,                     
    names_from = metrics) %>%
  arrange(desc(`Number of Samples`))
write.table(alltime_avgs,file="samples_submissions_users2.tsv",sep="\t",row.names=F)

#Plotting:
#1. Number of sample analyses per month using sample-level workflows
#2. Number of submissions per month using sample-level workflows
#3. Number of submissions per month using set-level workflows
monthly_data%>%
  filter(!(grp3 %in% c('Development'))) %>%
  filter(!(metrics %in% c('Number of Users'))) %>%
  filter(!(metrics == "Number of Samples" & grp4 == "Set-Level")) %>%
  group_by(month,grp3,title) %>%
  summarise(counts = sum(counts)) %>%
  ggplot(aes(x=factor(month), y=counts, fill=grp3)) + theme_minimal() + 
  facet_wrap(factor(title) ~ ., scales="free", ncol=1) + 
  expand_limits(y = 0) +
  scale_fill_manual(values=c("#50B594","#7983C1","#C355B6","#CA9D32","#BD523D","#674DB5","#B54E78","#858A49","#73B94A","grey"),
                    breaks=c("Updates","TheiaCoV","Submission","Phylogenetics","BaseSpace_Fetch","Utility","TheiaProk","Freyja","SRA_Fetch","Other")) +
  geom_bar(stat = "identity") + labs(x="Month", y="Counts", fill="Workflow") +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  theme(axis.text.y=element_text(size=10), axis.text.x=element_text(size=10, angle=45, hjust=1.0), 
        title=element_text(size=16), legend.position="bottom",
        strip.text.x=element_text(size=14, margin=margin(2, 0, 2, 0)),
        strip.background=element_rect(color="black", fill="grey90", linetype="solid"),
        panel.spacing=unit(3, "lines"))
