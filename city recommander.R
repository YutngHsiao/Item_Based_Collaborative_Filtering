library(shiny)
library(leaflet)

dfLongLat <- read.csv("dfl.csv")[,-1]
dfLongLat$city <- as.character(dfLongLat$city)
dfLongLat$url <- as.character(dfLongLat$url)
dfLongLat$pic_urls <- as.character(dfLongLat$pic_urls)
ITBC_sim <- read.csv("itbc.csv")[,-1]
rownames(ITBC_sim) <- colnames(ITBC_sim)
share <- 
###########
ui <- bootstrapPage(
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}", HTML(' #sidebar { background-color: #FFFFFF; text-align: center; vertical-align: middle; border:6px solid silver;} ')), 
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, left = 10, width = 360,draggable = F, id = "sidebar",
                h3("City Travel Recommender"),
                div(style="display:inline-block",selectInput(inputId="fav", label="Select Your Favourite City from Below or Click On The Map for more Information",c("-", dfLongLat$city))),
                imageOutput("image1", height = 200),
                uiOutput("tt"),
                p(),
                p("Top 5 recommend cities, you can either click for more info or view them on the map"),
                uiOutput("t1"),
                uiOutput("t1p"),
                uiOutput("t2"),
                uiOutput("t2p"),
                uiOutput("t3"),
                uiOutput("t3p"),
                uiOutput("t4"),
                uiOutput("t4p"),
                uiOutput("t5"),
                uiOutput("t5p"),
                #uiOutput("t6"),
                p(),
                p("How about our recommendation?"),
                actionButton("like", "Like !!"),
                actionButton("dislike", "dislike"),
                uiOutput("text2"),
                uiOutput("text3"),
                uiOutput("text1"),
                
                p()
  )
)



recom <- function(x) {
  tmp <- ITBC_sim[colnames(ITBC_sim) %in% x,]
  tmp <- tmp[order(tmp,decreasing = T)][2:6] 
  return(names(tmp))
}
server <- function(input, output, session) {
    frombtn <- "<button onclick='Shiny.onInputChange(\"button_click_f\",  Math.random())' id='frompop' type='button' class='btn btn-default action-button'>FAV.</button>"
  city_popup <- paste0(dfLongLat$city, "&nbsp;&nbsp;&nbsp;", dfLongLat$pic_urls,p(),frombtn)

  output$map <- renderLeaflet({
    
    city_popup <- paste0(dfLongLat$city, "&nbsp;&nbsp;&nbsp;", dfLongLat$pic_urls,p(),frombtn)
    cityIcon <- makeIcon(iconUrl = "http://icons.iconarchive.com/icons/icons-land/vista-map-markers/48/Map-Marker-Push-Pin-1-Left-Azure-icon.png", iconWidth = 32, iconHeight = 32) 
    
    leaflet(dfLongLat) %>%
      addTiles() %>%
      #addTiles('//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png') %>%
      addMarkers(~lon, ~lat, icon = cityIcon, popup = city_popup, layerId=dfLongLat$city)
  })
  proxy <- leafletProxy("map", data = dfLongLat)

  observe({
    if(input$fav %in% "-" == F){
      cty <- recom(input$fav)
      re_city <- dfLongLat[dfLongLat$city %in% cty,]
      lnk <- paste0("<a target='_blank' href='", dfLongLat[dfLongLat$city == input$fav,4],"'>","Things to do in ", input$fav,"</a>")
      output$tt <- renderUI({ HTML(lnk) })
      
      lnk1 <- paste0("<a target='_blank' href='", dfLongLat[dfLongLat$city == cty[1],4],"'>",cty[1],"</a>")
      output$t1 <- renderUI({ HTML(lnk1) })
      output$t1p <- renderUI({ HTML(dfLongLat$pic_urls[match(cty[1], dfLongLat$city)]) })
      lnk2 <- paste0("<a target='_blank' href='", dfLongLat[dfLongLat$city == cty[2],4],"'>",cty[2],"</a>")
      output$t2 <- renderUI({ HTML(lnk2) })
      output$t2p <- renderUI({ HTML(dfLongLat$pic_urls[match(cty[2], dfLongLat$city)]) })
      lnk3 <- paste0("<a target='_blank' href='", dfLongLat[dfLongLat$city == cty[3],4],"'>",cty[3],"</a>")
      output$t3 <- renderUI({ HTML(lnk3) })
      output$t3p <- renderUI({ HTML(dfLongLat$pic_urls[match(cty[3], dfLongLat$city)]) })
      lnk4 <- paste0("<a target='_blank' href='", dfLongLat[dfLongLat$city == cty[4],4],"'>",cty[4],"</a>")
      output$t4 <- renderUI({ HTML(lnk4) })
      output$t4p <- renderUI({ HTML(dfLongLat$pic_urls[match(cty[4], dfLongLat$city)]) })
      lnk5 <- paste0("<a target='_blank' href='", dfLongLat[dfLongLat$city == cty[5],4],"'>",cty[5],"</a>")
      output$t5 <- renderUI({ HTML(lnk5) })
      output$t5p <- renderUI({ HTML(dfLongLat$pic_urls[match(cty[5], dfLongLat$city)]) })
      #lnk6 <- paste0("<a target='_blank' href='", dfLongLat[dfLongLat$city == cty[6],4],"'>",cty[6],"</a>")
      #output$t6 <- renderUI({ HTML(lnk6) })

      
     # print(re_city)
      proxy <- leafletProxy("map", data = re_city) 
      re_city_popup <- paste0(h5("Recommand Destination :"), re_city$city, re_city$pic_urls,p(), frombtn)
      proxy %>% clearMarkers() %>% clearShapes() %>% addMarkers(~lon, ~lat, popup = re_city_popup, layerId=re_city$city)
    } else{
      leafletProxy("map", data = dfLongLat) %>% clearMarkers() %>% addMarkers(~lon, ~lat+.1, popup = city_popup, layerId=dfLongLat$city)
    }
  })
  vars = reactiveValues(counter = 0)
  click = reactiveValues(counter = 0)
  # increase the counter
  observeEvent(input$like, {
    click$counter <- click$counter + 1
    if(vars$counter<1){
      vars$counter <- vars$counter + 1
      output$text1 <- renderUI({ paste(vars$counter) })
      sumL = read.csv("v.csv")[1,2]
      sumD = read.csv("c.csv")[1,2]
      sumL <- sumL+1
      if (click$counter > 1) {sumD <- sumD-1}
      write.csv(sumL, "v.csv")
      write.csv(sumD, "c.csv")
      output$text2 <- renderUI({ paste("Like : ",read.csv("v.csv")[1,2]) })
      output$text3 <- renderUI({ paste("Dislike : ",read.csv("c.csv")[1,2]) })
      output$text1 <- renderUI({ paste("total : ", sumD + sumL) })
    }
    
  })
  
  observeEvent(input$dislike, {
    click$counter <- click$counter + 1
    if(vars$counter>0){
      
      vars$counter <- vars$counter -1
      
      
      sumL = read.csv("v.csv")[1,2]
      sumD = read.csv("c.csv")[1,2]
      if (click$counter > 1) {sumL <- sumL-1}
      
      sumD <- sumD+1
      write.csv(sumL, "v.csv")
      write.csv(sumD, "c.csv")
      output$text2 <- renderUI({ paste("Like : ",read.csv("v.csv")[1,2]) })
      output$text3 <- renderUI({ paste("Dislike : ",read.csv("c.csv")[1,2]) })
      output$text1 <- renderUI({ paste("total : ", sumD + sumL) })
    }
  })
  output$image1 <- renderImage({
    if (input$fav %in% dfLongLat$city){
      i <- match(input$fav, dfLongLat$city)
      pic <- paste0(i, ".jpg")
      return(list(
        src = pic,
        contentType = "image/png"#,
        #alt = "Face"
      ))
    } else{
      return(list(
        src = "a.jpg",
        contentType = "image/png"
      ))
    }
    
  }, deleteFile = FALSE)
  
  observeEvent(input$button_click_f, {
    updateSelectInput(session, "fav",choices = dfLongLat$city, selected = input$map_marker_click$id)
  })
}

shinyApp(ui, server)
