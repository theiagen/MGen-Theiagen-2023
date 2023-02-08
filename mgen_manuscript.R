setwd("/Volumes/Macintosh HD/Users/jotieno/Google Drive/Shared drives/terra_workflow_launches_data/")

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

#Import whole workbook
metadata1<-loadWorkbook("theiagen_wf_sub_users_by_month.xlsx") %>% 
  sheets() %>% 
  set_names() %>% 
  map_df(~ read_excel(path = "theiagen_wf_sub_users_by_month.xlsx", sheet = .x), .id = "sheet") %>%
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

metadata2<-loadWorkbook("theiagen_wf_sub_users_all_time.xlsx") %>% 
  sheets() %>% 
  set_names() %>% 
  map_df(~ read_excel(path = "theiagen_wf_sub_users_all_time.xlsx", sheet = .x), .id = "sheet") %>%
  select(-c(sheet)) %>% 
  pivot_longer(cols = c(workflow_count,submission_count,user_count),
               names_to = "metrics",
               values_to = "counts") %>%
  mutate(workflow = stringr::str_trim(sapply(strsplit(extracted_method, split = '/' ), '[',6 ))) %>%
  mutate(metrics = ifelse(grepl("workflow_count",metrics)==TRUE,"Number of Samples", metrics)) %>%
  mutate(metrics = ifelse(grepl("submission_count",metrics)==TRUE,"Number of Submissions", metrics)) %>%
  mutate(metrics = ifelse(grepl("user_count",metrics)==TRUE,"Number of Users", metrics))

metadata3<-loadWorkbook("workflow_groupings.xlsx") %>% 
  sheets() %>% 
  set_names() %>% 
  map_df(~ read_excel(path = "workflow_groupings.xlsx", sheet = .x), .id = "sheet") %>%
  filter(sheet != "backup") %>%
  select(-c(sheet,grp1,grp2,key))

#Merge with the curated groupings
metadata1<-merge(metadata1,metadata3,by="workflow",all=T) %>%
  filter(!(grp3 %in% c('Development'))) %>%
  mutate(title = "") %>%
  mutate(title = ifelse(grepl("Number of Samples",metrics)==TRUE,paste0("Number of Samples Analysed Per Month Using ",grp4," Workflows"),title)) %>%
  mutate(title = ifelse(grepl("Number of Submissions",metrics)==TRUE,paste0("Number of Submissions Per Month Using ",grp4," Workflows"),title)) %>%
  mutate(title = ifelse(grepl("Number of Users",metrics)==TRUE,paste0("Number of Users Per Month Using ",grp4," Workflows"),title))

metadata2<-merge(metadata2,metadata3,by="workflow",all=T) %>%
  filter(!is.na(counts)) %>%
  filter(!(grp3 %in% c('Development'))) %>%
  mutate(title = "") %>%
  mutate(title = ifelse(grepl("Number of Samples",metrics)==TRUE,paste0("# of Samples Analysed Per Month Using ",grp4," Workflows"),title)) %>%
  mutate(title = ifelse(grepl("Number of Submissions",metrics)==TRUE,paste0("# of Submissions Per Month Using ",grp4," Workflows"),title)) %>%
  mutate(title = ifelse(grepl("Number of Users",metrics)==TRUE,paste0("# of Users Per Month Using ",grp4," Workflows"),title))

sort(unique(metadata1$grp3))
sort(unique(metadata2$grp3))

#Sum and order by the samples run
all_workflows<-metadata2 %>%
  rename("Workflow Type"= grp3) %>%
  group_by(`Workflow Type`) %>%
  summarise(counts = sum(counts)) %>%
  arrange(desc(counts))
all_workflows

#Sum and order by the samples run
all_avgs<-metadata1 %>%
  select(grp3,metrics,counts) %>%
  rename("Workflow Type"= grp3) %>%
  group_by(`Workflow Type`,metrics) %>%
  summarise(counts = sum(counts)) %>%
  ungroup() %>%
  pivot_wider(                                      
    values_from = counts,                     
    names_from = metrics) %>%
  arrange(desc(`Number of Samples`))
all_avgs %>%
  flextable::flextable() %>%
  flextable::hline(part = "body") %>%
  flextable::autofit() %>%
  flextable::bg(bg = "grey", part = "header") %>%
  flextable::theme_vanilla() %>%
  flextable::save_as_image(path="./figures/workflow_averages1.pdf")
write.table(all_avgs,file="./figures/workflow_averages1.tsv",sep="\t",row.names=F)

all_avgs2<-metadata2 %>%
  select(grp3,metrics,counts) %>%
  rename("Workflow Type" = grp3) %>%
  group_by(`Workflow Type`,metrics) %>%
  summarise(counts = sum(counts)) %>%
  ungroup() %>%
  pivot_wider(                                      
    values_from = counts,                     
    names_from = metrics) %>%
  arrange(desc(`Number of Samples`))
all_avgs2 %>%
  flextable::flextable() %>%
  flextable::hline(part = "body") %>%
  flextable::autofit() %>%
  flextable::bg(bg = "grey", part = "header") %>%
  flextable::theme_vanilla() %>%
  flextable::save_as_image(path="./figures/workflow_averages2.pdf")
write.table(all_avgs2,file="./figures/workflow_averages2.tsv",sep="\t",row.names=F)

#Plotting
#all metrics
metadata1%>%
  filter(!(grp3 %in% c('Development'))) %>%
  group_by(month,metrics,grp4) %>%
  summarise(counts = sum(counts)) %>%
  ggplot(aes(x=factor(month), y=counts, fill=grp4)) + theme_minimal() + 
  facet_wrap(. ~ factor(metrics, levels=c("Number of Samples","Number of Submissions","Number of Users")),scales = "free_y") +
  expand_limits(y = 0) + geom_bar(stat = "identity") + 
  labs(x="Month", y="Counts", fill="Level") +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  theme(axis.text.y=element_text(size=10), axis.text.x=element_text(size=10, angle=45, hjust=1.0), 
        title=element_text(size=20),legend.position="bottom",
        strip.text.x=element_text(size=12, margin=margin(2, 0, 2, 0)),
        strip.background=element_rect(color="black", fill="grey90", linetype="solid"))
ggsave("./figures/all_metrics_set.pdf", width=15, height=8.27, units="in")

p<-metadata1%>%
  filter(!(grp3 %in% c('Development'))) %>%
  group_by(month,grp3,title) %>%
  summarise(counts = sum(counts)) %>%
  ggplot(aes(x=factor(month), y=counts, fill=grp3)) + theme_minimal() + 
  facet_wrap(factor(title) ~ ., scales="free", ncol=2) + expand_limits(y = 0) +
  #scale_fill_manual(values=c("red","blue","yellow","#6ab64c","#ff4500","purple","brown","pink","black","gray40","cyan"),
  scale_fill_manual(values=c("#6ab64c","#8f62ca","#ff4500","gray40","#c6793d","#4db598","black","pink","#637c38","cyan"),
                    breaks=key_order) +
  geom_bar(stat = "identity") + labs(x="Month", y="Counts", fill="Workflow") +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  theme(axis.text.y=element_text(size=10), axis.text.x=element_text(size=10, angle=45, hjust=1.0), 
        title=element_text(size=20), legend.position="bottom",
        strip.text.x=element_text(size=12, margin=margin(2, 0, 2, 0)),
        strip.background=element_rect(color="black", fill="grey90", linetype="solid"))
p
ggsave("./figures/all_metrics_top10.pdf", width=16, height=8.27, units="in")

#Select
p<-metadata1%>%
  filter(!(grp3 %in% c('Development'))) %>%
  filter(!(metrics %in% c('Number of Users'))) %>%
  filter(!(metrics == "Number of Samples" & grp4 == "Set-Level")) %>%
  group_by(month,grp3,title) %>%
  summarise(counts = sum(counts)) %>%
  ggplot(aes(x=factor(month), y=counts, fill=grp3)) + theme_minimal() + 
  facet_wrap(factor(title) ~ ., scales="free", ncol=1) + 
  expand_limits(y = 0) +
  scale_fill_manual(values=c("#50B594","#7983C1","#C355B6","#CA9D32","#BD523D","#674DB5","#B54E78","#858A49","#73B94A","grey"),
  #scale_fill_manual(values=alpha(c("#6ab64c","#8f62ca","#ff4500","gray40","#c6793d","#4db598","black","pink","#637c38","cyan"),0.8),
                    breaks=c("Updates","TheiaCoV","Submission","Phylogenetics","BaseSpace_Fetch","Utility","TheiaProk","Freyja","SRA_Fetch","Other")) +
  geom_bar(stat = "identity") + labs(x="Month", y="Counts", fill="Workflow") +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  theme(axis.text.y=element_text(size=10), axis.text.x=element_text(size=10, angle=45, hjust=1.0), 
        title=element_text(size=16), legend.position="bottom",
        strip.text.x=element_text(size=14, margin=margin(2, 0, 2, 0)),
        strip.background=element_rect(color="black", fill="grey90", linetype="solid"),
        panel.spacing=unit(3, "lines"))
p
ggsave("./figures/all_metrics_top10v2.pdf", width=8.27, height=11.69, units="in")

#if you want the plot in the interactive plotly
fig<-ggplotly(p)
fig #You can export this as HTML from the viewer tab
