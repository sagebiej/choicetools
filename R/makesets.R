createSets <- function(.data, choice, attributes , uniquerow) {
  require("dplyr")
  require("tidyr")
  require("purrr")
  

  
  sets <- .data %>% 
    select({{ attributes }}, {{ choice }}, {{ uniquerow }} ) %>%
    group_by(!!rlang::sym(uniquerow), !!rlang::sym(choice)) %>% 
    add_count() %>% ungroup %>% 
    group_by(!!rlang::sym(uniquerow)) %>% 
    distinct(n, .keep_all=TRUE) %>% 
    mutate(perc = round((n / sum(n) * 100))) %>% 
    arrange({{ uniquerow }}, {{ choice }}) %>%
    group_split() %>% 
    set_names(map(., ~ unique(as.character(.x[[rlang::as_string(uniquerow)]]))))
  

  
  makesets <- function(.data) {

    
    .data %>%
      pivot_wider(
        id_cols = c({{ uniquerow }}, everything()),
        names_from = {{ choice }},
        values_from = c(n, perc),
        names_sep = "."
      )  %>%
      select(- {{ uniquerow }}) %>% 
      rename_with(
        ~ gsub("^(a([12]))_(.*)$", "\\3.\\2", .),
        starts_with("a1") | starts_with("a2")
      ) %>% 
      pivot_longer(
        cols = everything(),
        names_to = c("name", "suffix"),
        names_pattern = "(.*)\\.(.*)"
      ) %>%
      pivot_wider(
        names_from = suffix,
        values_from = value
      )
  }
  
  finalsets <- map(sets, ~makesets(.x ))

  
  
  return(finalsets)
  
}

#finalsets2 <- createFreq(database, choice = "pref1", attributes = ends_with(c("ZEIT","x1")), uniquerow = "UniqueRow")
