#Creating the table of indicators for LandStewardReports

ind.table.kbl<-proj.indicator.table%>%select(Indicator, Acronym, Description, `Final units`)

Table1<-kbl(ind.table.kbl,
            caption= "Description of carbon indicators",
            col.names = gsub("[.]", " ", x=names(ind.table.kbl)), 
            row.names = FALSE,
            vline = "|",
            linesep = "\\addlinespace",
            align= c('llll'))

if ("BD" %in% ind.table.kbl$Acronym) {
  # Add a footnote marker to BD row (in Acronym column)
  ind.table.kbl$Description <- ifelse(
    ind.table.kbl$Acronym == "BD",
    paste0(ind.table.kbl$Description, footnote_marker_symbol(1)),
    ind.table.kbl$Description
  )
  
  # Rebuild the table with updated Acronym column
  Table1 <- kbl(
    ind.table.kbl,
    caption = "Description of carbon indicators",
    col.names = gsub("[.]", " ", x = names(ind.table.kbl)), 
    row.names = FALSE,
    vline = "|",
    linesep = "\\addlinespace",
    align = c('llll'),
    escape = FALSE
  ) %>%
    footnote(
      symbol  = "Bulk density values are calculated without subtracting rock volume so they can more accurately be used for carbon stock calculations. This may lead to lower “hybrid” BD values than are commonly presented."
    )
}

IndicatorTableFinal<-Table1 %>% 
  kable_styling(position='center', full_width = T) %>%
  column_spec(1, width="1.25in")%>%
  column_spec(2, width="0.5in")%>%
  column_spec(3, width="2.5in")%>%
  column_spec(4, width="1.75in")%>%
  row_spec(0, bold=TRUE)%>%
  collapse_rows(columns = c(1, 3, 4), valign = "middle") #merges adjacent cells with identical values
