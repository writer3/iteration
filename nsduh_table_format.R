```{r}
nsduh_table_format = function(html, table_num, table_name) {
  
  out_table =
    html |> 
    html_table() |> 
    nth(table_num) |> 
    slice(-1) |> 
    mutate(drug = table_name) |> 
    select(-contains("P Value"))
  
  return(out_table)
  
}

bind_rows(
  nsduh_table_format(html = nsduh_html, 1, "marj"),
  nsduh_table_format(html = nsduh_html, 4, "cocaine"),
  nsduh_table_format(html = nsduh_html, 5, "heroin")
)
````