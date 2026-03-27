# current version using the simpler.grants.gov API

source("r/functions/simpler_grants_gov_api.r")
library(tidyverse)

mq <- create_query(top_level_agency = "HHS-NIH11", opportunity_status = c("forecasted", "posted", "closed"), per_page = 50)
nofos <- search_opps(mq) ## 388 on 18 March 2026; 758 forecasted, posted, or closed

nofos <- nofos %>% 
  mutate(across(ends_with("_date"), as.Date)) %>% 
  mutate(doc_text = paste(opportunity_title, summary.summary_description, sep = ". "))

source("r/functions/vbic_classify.r")
tterms <- read.csv("projects/reference data/nichd_theme_terms_forNOFOs.csv", stringsAsFactors = FALSE, allowEscapes = TRUE)
vbic_out <- vbic_classify(nofos, tterms, concept_threshold = 1, unclassified = TRUE, doc_id_column = "opportunity_id", doc_text_column = "doc_text")
vbic_out$concept_list %>% count(all_concepts)

nofos <- nofos %>% 
  left_join(vbic_out$concept_list[,c(1,3,4)], by = c("opportunity_id" = "doc_id")) %>% 
  select(!doc_text) %>% 
  arrange(desc(summary.post_date), opportunity_number)


nofo_display <- nofos %>% 
  mutate(
    nofo_url = paste0("https://simpler.grants.gov/opportunity/", opportunity_id),
    opportunity_number = paste0("<a href = \"", nofo_url,"\" target = \"_blank\">", opportunity_number, "</a>"),
    nofo_type = case_when(
      grepl("RFA", opportunity_number) ~ "RFA",
      grepl("PA[RS]", opportunity_number) ~ "PAR",
      grepl("PA", opportunity_number) ~ "PA",
      grepl("^OT", opportunity_number) ~ "OTH",
      TRUE ~ "UNK"
    ),
    all_concepts = gsub("theme1", "Developmental Biology", all_concepts),
    all_concepts = gsub("theme2", "Reproductive Health", all_concepts),
    all_concepts = gsub("theme3", "Pregnancy & Lactation", all_concepts),
    all_concepts = gsub("theme4", "Pediatrics", all_concepts),
    all_concepts = gsub("theme5", "Pharmacology & Therapeutics", all_concepts),
    all_concepts = gsub("ncmrr", "Rehabilitation", all_concepts),
    all_concepts = gsub("unclassified", "Unclassified", all_concepts),
    hd_involvement = case_when(
      (grepl("NICHD|Eunice|NCMRR", nofos$summary.summary_description) == TRUE | grepl("-HD-", opportunity_number) | opportunity_assistance_numbers == "93.865" | grepl("PAR-25-11[0-3]", opportunity_number)) & grepl("93.865", opportunity_assistance_numbers) ~ "HD Primary",
      grepl("93.865", opportunity_assistance_numbers) ~ "HD Secondary",
      grepl("93.865", opportunity_assistance_numbers) == FALSE ~ "No HD Involvement"
    ), ## not completely reliable
    opportunity_status = str_to_title(opportunity_status),
    run_date = as.character(Sys.Date())
  ) %>% 
  arrange(desc(summary.post_date))

hd_nofos <- nofo_display %>% 
  filter(grepl("93.865", opportunity_assistance_numbers), grepl("\\(Parent [A-Z]..\\b.*\\)|Parent SBIR|Parent STTR", opportunity_title) == FALSE) %>% 
  select(opportunity_number, nofo_type, opportunity_title, summary.post_date, summary.close_date, opportunity_status, all_concepts, hd_involvement, run_date)
write.csv(hd_nofos, file = "shiny dashboards/hd_nofos/data/hd_nofos_current.csv", row.names = FALSE)
## missed PAR-25-185, PAR-25-110, PAR-25-111, PAR-25-112, PAR-25-113, ...

relevant_nofos <- nofo_display %>% 
  filter(all_concepts != "Unclassified", grepl("93.865", opportunity_assistance_numbers) == FALSE, opportunity_status == "Forecasted") %>% 
  select(opportunity_number, nofo_type, opportunity_title, summary.post_date, all_concepts, run_date)
write.csv(relevant_nofos, file = paste0("projects/foam/nofo_forecast_relevant_", gsub("-", "_", as.character(Sys.Date())), ".csv"), row.names = FALSE)
write.csv(relevant_nofos, file = "shiny dashboards/hd_nofos/data/nofo_forecast_relevant_current.csv", row.names = FALSE)

rsconnect::writeManifest(appDir = "shiny dashboards/hd_nofos/")
##rsconnect::deployApp(appDir = "shiny dashboards/hd_nofos/")