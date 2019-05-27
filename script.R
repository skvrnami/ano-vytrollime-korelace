library(rvest)
library(dplyr)
library(corrr)

okresy <- "https://volby.cz/opendata/ep2019/EP_nuts.htm"
read_html(okresy) %>%
    html_node("table") %>%
    html_nodes("tr") %>%
    html_text() %>%
    stringr::str_extract(., "CZ[0-9]{4}|CZ[0-9]{3}[A-Z]{1}") %>%
    `[`(., !is.na(.)) -> okresy_codes

# base_url <- "https://volby.cz/pls/ep2019/vysledky_okres?nuts="

scrap_obec <- function(obec_html){
    municipality_id <- obec_html %>% html_attr("cis_obec")
    obec1 <- obec_html %>%
        html_nodes("hlasy_strana")
    
    estrana <- obec1 %>% html_attr("estrana") %>% as.integer       
    votes <- obec1 %>% html_attr("hlasy") %>% as.integer
    pct_votes <- obec1 %>% html_attr("proc_hlasu") %>% as.numeric
    
    tibble(municipality_id = municipality_id, 
           estrana = estrana, 
           votes = votes, 
           pct_votes)
}

scrap_okres_results <- function(code){
    html <- read_html(paste0("https://volby.cz/pls/ep2019/vysledky_okres?nuts=", code))
    okres <- html %>% html_node("okres") %>% html_attr("nuts_okres")
    obce <- html %>% html_nodes("obec")
    
    purrr::map_df(obce, scrap_obec) %>%
        mutate(okres = okres)
}

purrr::map_df(okresy_codes, scrap_okres_results) -> obce_results

write.csv(obce_results, file = "eu_obce_vysledky.csv", 
          row.names = FALSE)

# korelace procentuální výsledek po obcích
obce_results %>%
    select(-votes) %>%
    tidyr::spread(., estrana, pct_votes) -> obce_wide_pct

obce_wide_pct[is.na(obce_wide_pct)] <- 0.0

obce_wide_pct %>%
    select(-c(municipality_id, okres)) %>%
    correlate(., use = "everything") %>%
    stretch() -> cor_obce_pct

cor_obce_pct %>%
    filter(x == 6 & y == 30)

cor_obce_pct %>%
    arrange(desc(r)) %>%
    head()

# korelace procentuálních výsledků po okresech
obce_results %>%
    group_by(okres, estrana) %>%
    summarise(votes = sum(votes)) %>%
    ungroup() %>%
    group_by(okres) %>%
    mutate(pct_votes = votes / sum(votes) * 100) -> okres_votes

okres_votes %>%
    select(-votes) %>%
    tidyr::spread(., estrana, pct_votes) -> okres_wide_pct
    
okres_wide_pct[is.na(okres_wide_pct)] <- 0.0    

okres_wide_pct %>%
    ungroup %>%
    select(-okres) %>%
    correlate %>%
    stretch() -> okres_pct_corr

# ANO + ANO, vytrollíme...
okres_pct_corr %>%
    filter(x == 6 & y == 30)

okres_pct_corr %>%
    arrange(desc(r)) %>% 
    head

# korelace počet hlasů po obcích
obce_results %>%
    select(-pct_votes) %>%
    tidyr::spread(., estrana, votes) -> obce_wide_votes

obce_wide_votes[is.na(obce_wide_votes)] <- 0

obce_wide_votes %>%
    select(-c(municipality_id, okres)) %>%
    correlate(., use = "everything") %>%
    stretch() -> cor_obce_votes

cor_obce_votes %>%
    filter(x == 6 & y == 30)

cor_obce_votes %>%
    arrange(desc(r)) %>%
    head()

# korelace počet hlasů po okresech

obce_results %>%
    group_by(estrana, okres) %>%
    summarise(votes = sum(votes)) %>% 
    tidyr::spread(., estrana, votes) -> okresy_votes

okresy_votes[is.na(okresy_votes)] <- 0

okresy_votes %>%
    select(-okres) %>%
    correlate %>%
    stretch() -> cor_okres_votes

cor_okres_votes %>%
    filter(x == 6 & y == 30)

cor_okres_votes %>%
    arrange(desc(r)) %>%
    head

# simulace
obce_results %>%
    filter(estrana == 6 | estrana == 30) %>%
    select(-pct_votes) %>%
    tidyr::spread(., estrana, votes) %>%
    mutate(`6` = ifelse(is.na(`6`), 0, `6`), 
           `30` = ifelse(is.na(`30`), 0, `30`)) %>%
    mutate(total_votes = `6` + `30`) -> municipalities_ano

simulate_vote_error <- function(mun_size, p_troll = 0.06868){
    # počet hlasů, které by dostalo ANO, vytrollíme europarlament
    # v obci s `mun_size` voliči, pokud pravděpodobnost záměny lístku 
    # je 0.06868 (= podíl ANO, vytrollíme ze zisku ANO+ANO,vytrollíme)
    sum(sample(c(TRUE, FALSE), size = mun_size, 
               replace = TRUE, 
               prob = c(p_troll, 1 - p_troll)))
}

simulate_cor <- function(){
    municipalities_ano %>%
        mutate(expected_troll = purrr::map_int(total_votes, 
                                               function(x) simulate_vote_error(mun_size = x))) %>%
        mutate(expected_ano = total_votes - expected_troll) -> sim_results
    
    cor(sim_results$expected_ano, sim_results$expected_troll)
}

corrs <- replicate(1000, simulate_cor())

summary(corrs)
