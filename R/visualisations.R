categoryCount_chart <- function(data, colours, title, subtitle, source, link){
  highchart() %>%
  hc_xAxis(categories = data$maincategory, title = list(enabled = FALSE)) %>% 
  hc_yAxis(title = list(enabled = FALSE)) %>%
  hc_add_series(data = as.numeric(data$count), type = "column", color = colours, name = "Topic", label = NULL) %>%
  # hchart(data, hcaes(maincategory, as.numeric(count), color = colours), type = "column") %>% 
  hc_plotOptions(column = list(dataLabels = list(enabled = TRUE))) %>% 
  hc_chart(height = "400") %>%
  hc_title(text = paste0("<b>", title, "</b>"), align = "left", margin = 20) %>%
  hc_subtitle(text = subtitle, align = "left") %>%
  hc_credits(enabled = TRUE, text = source, href = link, position = list(align = "left", x = 12))
}

categoryTreemap_chart <- function(data, title, subtitle, source, link){
  pal <- viridis(n = 20, direction = 1)
  
  tm <- treemap(data, index = "maincategory",
                vSize = "count", vColor = "count",
                type = "value", palette = pal)
  
  hctreemap(tm) %>% 
    hc_chart(height = "400") %>%
    hc_title(text = paste0("<b>", title, "</b>"), align = "left", margin = 20) %>%
    hc_subtitle(text = subtitle, align = "left") %>%
    hc_credits(enabled = TRUE, text = source, href = link, position = list(align = "left", x = 12))
}

performerCount_chart <- function(data, colours, title, subtitle, source, link){
  highchart() %>%
    hc_xAxis(categories = data$performer, title = list(enabled = FALSE)) %>% 
    hc_yAxis(title = list(enabled = FALSE)) %>%
    hc_add_series(data = as.numeric(data$count), type = "column", color = colours, name = "Topic", label = NULL) %>%
    # hchart(data, hcaes(maincategory, as.numeric(count), color = colours), type = "column") %>% 
    hc_plotOptions(column = list(dataLabels = list(enabled = TRUE))) %>% 
    hc_chart(height = "400", polar = FALSE) %>%
    hc_title(text = paste0("<b>", title, "</b>"), align = "left", margin = 20) %>%
    hc_subtitle(text = subtitle, align = "left") %>%
    hc_credits(enabled = TRUE, text = source, href = link, position = list(align = "left", x = 12))
}

datesCount_chart <- function(data, colours, title, subtitle, source, link){
  highchart() %>%
    hc_xAxis(categories = data$dates, title = list(enabled = FALSE)) %>% 
    hc_yAxis(title = list(enabled = FALSE)) %>%
    hc_add_series(data = as.numeric(data$count), type = "area", color = colours, name = "Articles", label = NULL) %>%
    # hc_add_series(data = as.numeric(data$rollingAvg), type = "line", color = colours, name = "Rolling Average", label = NULL) %>%
    # hchart(data, hcaes(maincategory, as.numeric(count), color = colours), type = "column") %>% 
    # hc_plotOptions(line = list(dataLabels = list(enabled = TRUE))) %>%
    hc_chart(height = "400") %>%
    hc_title(text = paste0("<b>", title, "</b>"), align = "left", margin = 20) %>%
    hc_subtitle(text = subtitle, align = "left") %>%
    hc_credits(enabled = TRUE, text = source, href = link, position = list(align = "left", x = 12))
}


# hchart(datesAnalysis, hcaes(dates, count), type = "column") %>% hc_plotOptions(bar = list(stacking = "normal"))
