rm(list = ls())

library(tidyverse)
library(readr)
library(janitor)
library(rfishbase)

data_ns1 <- read_csv("IDN_new_species_NS1.csv") %>%
  clean_names() %>%
  filter(issue)

data_asfis <- read_csv("ASFIS_sp_2022_REV1.csv") %>%
  clean_names() %>% 
  select(x3a_code, scientific_name, english_name)

data_fishbase_synonyms <- fb_tbl("synonyms", server = "fishbase") %>% # Get FISHBASE synonyms
  clean_names() %>%
  mutate(scientific_name = paste(syn_genus, syn_species))

data_sealifebase_synonyms <- fb_tbl("synonyms", server = "sealifebase") %>% # Get SEALIFEBASE synonyms
  clean_names() %>%
  mutate(scientific_name = paste(syn_genus, syn_species))

ns1_fishbase_asfis_lookup <- tibble(scientific_name = character(), synonyms = character(), status = character(), valid = logical(), synonymy = character(), combination = character(), misspelling = logical())

for (i in unique(data_ns1$ns1_scientific_name)) {
  
  if (i %in% data_fishbase_synonyms$scientific_name) {
    
    synonym_code <- data_fishbase_synonyms %>%
      filter(scientific_name == i) %>%
      pull(spec_code)
    
    synonyms_names <- data_fishbase_synonyms %>%
      filter(spec_code == synonym_code, tolower(status) != "misapplied name") %>%
      select(scientific_name, status, valid, synonymy, combination, misspelling) %>%
      rename(synonyms = scientific_name) %>%
      mutate(scientific_name = i, status = tolower(status), synonymy = tolower(synonymy), combination = tolower(combination)) %>%
      select(scientific_name, everything())
    
    ns1_fishbase_asfis_lookup <- rbind(ns1_fishbase_asfis_lookup, synonyms_names)
    
    }
  
}

ns1_fishbase_asfis_lookup <- ns1_fishbase_asfis_lookup %>%
  mutate(in_asfis = synonyms %in% data_asfis$scientific_name) %>%
  # filter(in_asfis) %>%
  left_join(data_asfis, by = c("synonyms" = "scientific_name")) %>%
  arrange(desc(in_asfis), scientific_name)

write_csv(ns1_fishbase_asfis_lookup, "results.csv", na = "")
