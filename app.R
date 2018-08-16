

library(dplyr)
library(shiny)
library(ggplot2)
library(DT)
library(tidyr)
library(leaflet)
library(shinythemes)
library(htmlTable)

## data
skoly=read.table("https://raw.githubusercontent.com/SamuelHudec/BBSK/master/skolyapp.txt")




## input filtre
# zdriadovatel
zr = skoly %>% distinct(zriadovateľ) %>% mutate_if(is.factor, as.character)

# skupina odborov
sk = skoly %>% distinct(skupina.odborov) %>% mutate_if(is.factor, as.character)

# odbor nazov
od = skoly %>% distinct(odbor.nazov) %>% mutate_if(is.factor, as.character)

# forma
fr = skoly %>% distinct(Forma) %>% mutate_if(is.factor, as.character)

# druh
dr = skoly %>% distinct(Druh) %>% mutate_if(is.factor, as.character)

# stupeň
st = skoly %>% distinct(stupen.vzdelania) %>% mutate_if(is.factor, as.character)

# dlžka
dl = skoly %>% distinct(Dĺžka) %>% mutate_if(is.factor, as.character)

# jazyk
ja = skoly %>% distinct(Jazyk) %>% mutate_if(is.factor, as.character)



# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
   # Application title
   titlePanel("Školy pre Zuzku (BETA)"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(position = "right",
      sidebarPanel(
  
        ### select inputs ####
        selectInput(inputId="zri",
                     label="Zriadovateľ:",
                     choices=c("Všetky"="",zr), multiple=TRUE),
        
        selectInput(inputId="sku",
                     label="Skupina odborov:",
                     choices=c("Všetky"=""), multiple=TRUE),
        
        selectInput(inputId="odb",
                     label="Odbor názov:",
                     choices=c("Všekty"=""), multiple=TRUE),
        hr(),
        
        selectInput(inputId="frm",
                     label="Forma:",
                     choices=c("Všetky"=""), multiple=TRUE),
        
        selectInput(inputId="dru",
                     label="Druh:",
                     choices=c("Všetky"=""), multiple=TRUE),
        
        selectInput(inputId="stu",
                     label="Stupeň:",
                     choices=c("Všetky"=""), multiple=TRUE),
        
        selectInput(inputId="dlz",
                     label="Dĺžka:",
                     choices=c("Všetky"=""), multiple=TRUE),
        
        selectInput(inputId="jaz",
                     label="Jazyk:",
                     choices=c("Všetky"=""), multiple=TRUE)
      ),
      
    
     
      ### main panel ####
      mainPanel(
        leafletOutput(outputId = "map",height = 600),br(),
        tableOutput(outputId = "table")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({
    nskupina <- if (is.null(input$zri)) sk else {
      filter(skoly, zriadovateľ %in% input$zri) %>%
        `$`('skupina.odborov') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$sku[input$sku %in% nskupina])
    updateSelectInput(session, "sku", choices = nskupina,
      selected = stillSelected)
  })
  
  
  observe({
    nodbor <- if (is.null(input$zri)) {
        filter(skoly,is.null(input$sku) | skupina.odborov %in% input$sku) %>%
        `$`('odbor.nazov') %>%
        unique() %>%
        sort()
    } else {
      skoly %>% 
      filter(zriadovateľ %in% input$zri,
        is.null(input$sku) | skupina.odborov %in% input$sku) %>%
        `$`('odbor.nazov') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$odb[input$odb %in% nodbor])
    updateSelectInput(session, "odb", choices = nodbor,
      selected = stillSelected)
  })
  
  ## Doplnkove filtrovanie
  ## forma 
  observe({
    nforma <- if (is.null(input$zri)) {
      skoly %>%
        filter(is.null(input$sku) | skupina.odborov %in% input$sku,
          is.null(input$odb) | odbor.nazov %in% input$odb) %>%
        `$`('Forma') %>%
        unique() %>%
        sort()
    } else {
      skoly %>% 
      filter(zriadovateľ %in% input$zri,
        is.null(input$sku) | skupina.odborov %in% input$sku,
        is.null(input$odb) | odbor.nazov %in% input$odb) %>%
        `$`('Forma') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$frm[input$frm %in% nforma])
    updateSelectInput(session, "frm", choices = nforma,
      selected = stillSelected)
  })
  
  ## druh
  observe({
    ndruh <- if (is.null(input$zri)) {
      skoly %>%
        filter(is.null(input$sku) | skupina.odborov %in% input$sku,
          is.null(input$odb) | odbor.nazov %in% input$odb) %>%
        `$`('Druh') %>%
        unique() %>%
        sort()
    } else {
      skoly %>% 
      filter(zriadovateľ %in% input$zri,
        is.null(input$sku) | skupina.odborov %in% input$sku,
        is.null(input$odb) | odbor.nazov %in% input$odb) %>%
        `$`('Druh') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$dru[input$dru %in% ndruh])
    updateSelectInput(session, "dru", choices = ndruh,
      selected = stillSelected)
  })
  
  ## stupen 
  observe({
    nstupen <- if (is.null(input$zri)) {
      skoly %>%
        filter(is.null(input$sku) | skupina.odborov %in% input$sku,
          is.null(input$odb) | odbor.nazov %in% input$odb) %>%
        `$`('stupen.vzdelania') %>%
        unique() %>%
        sort()
    } else {
      skoly %>% 
      filter(zriadovateľ %in% input$zri,
        is.null(input$sku) | skupina.odborov %in% input$sku,
        is.null(input$odb) | odbor.nazov %in% input$odb) %>%
        `$`('stupen.vzdelania') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$stu[input$stu %in% nstupen])
    updateSelectInput(session, "stu", choices = nstupen,
      selected = stillSelected)
  })
  
  ## dlzka 
  observe({
    ndlzka <- if (is.null(input$zri)) {
      skoly %>%
        filter(is.null(input$sku) | skupina.odborov %in% input$sku,
          is.null(input$odb) | odbor.nazov %in% input$odb) %>%
        `$`('Dĺžka') %>%
        unique() %>%
        sort()
    } else {
      skoly %>% 
      filter(zriadovateľ %in% input$zri,
        is.null(input$sku) | skupina.odborov %in% input$sku,
        is.null(input$odb) | odbor.nazov %in% input$odb) %>%
        `$`('Dĺžka') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$dlz[input$dlz %in% ndlzka])
    updateSelectInput(session, "dlz", choices = ndlzka,
      selected = stillSelected)
  })
  
  ## Jazyk 
  observe({
    njazyk <- if (is.null(input$zri)) {
      skoly %>%
        filter(is.null(input$sku) | skupina.odborov %in% input$sku,
          is.null(input$odb) | odbor.nazov %in% input$odb) %>%
        `$`('Jazyk') %>%
        unique() %>%
        sort()
    } else {
      skoly %>% 
      filter(zriadovateľ %in% input$zri,
        is.null(input$sku) | skupina.odborov %in% input$sku,
        is.null(input$odb) | odbor.nazov %in% input$odb) %>%
        `$`('Jazyk') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$jaz[input$jaz %in% njazyk])
    updateSelectInput(session, "jaz", choices = njazyk,
      selected = stillSelected)
  })
  
  
  
  ## subset ####
  skoly_subset<- reactive({
    filtered <- skoly
    if (!is.null(input$zri)) {
      filtered <- filtered %>% filter(zriadovateľ %in% input$zri)
    }
    if (!is.null(input$sku)) {
      filtered <- filtered %>% filter(skupina.odborov %in% input$sku)
    }
    if (!is.null(input$odb)) {
      filtered <- filtered %>% filter(odbor.nazov %in% input$odb)
    }
    if (!is.null(input$frm)) {
      filtered <- filtered %>% filter(Forma %in% input$frm)
    }
    if (!is.null(input$dru)) {
      filtered <- filtered %>% filter(Druh %in% input$dru)
    }
    if (!is.null(input$stu)) {
      filtered <- filtered %>% filter(stupen.vzdelania %in% input$stu)
    }
    if (!is.null(input$dlz)) {
      filtered <- filtered %>% filter(Dĺžka %in% input$dlz)
    }
    if (!is.null(input$jaz)) {
      filtered <- filtered %>% filter(Jazyk %in% input$jaz)
    }
    filtered <- filtered %>% group_by(ID,nazov.skoly,lat,lng) %>% summarize(
      absolventi.2017=sum(absolventi.2017),
      žiaci.spolu=sum(žiaci.spolu),
      X1.=sum(X1.),X2.=sum(X2.),X3.=sum(X3.),X4.=sum(X4.),
      X5.=sum(X5.),X6.=sum(X6.),X7.=sum(X7.),X8.=sum(X8.),
      predpokladani.absolventi.máj.2018=sum(predpokladani.absolventi.máj.2018),
      odborný.výcvik.a.prax=sum(odborný.výcvik.a.prax),
      novoprijatí.1..ročník.2017=sum(novoprijatí.1..ročník.2017) # tu mozno bude chyba
    )
    filtered
  })
  
  new <- reactive({
    n = skoly_subset() %>% group_by() %>% summarize(
      "Žiaci spolu"=sum(žiaci.spolu),
      "Novoprijatí prváci" = sum(novoprijatí.1..ročník.2017),
      "Absolventi 2017"=sum(absolventi.2017))
    n
  })
   
  ## table ####
  output$table <- renderTable({new()},width=800)

  
  
  ## map ####
  output$map <- renderLeaflet({
     leaflet(skoly) %>% addProviderTiles("CartoDB.Positron") %>%
      addProviderTiles("OpenMapSurfer.AdminBounds") %>%
      setView(19.5,48.5, zoom = 9)
   })
  

   
  # maintaining part
   observe({
    leafletProxy("map",data=skoly_subset()) %>% clearShapes() %>%
      addCircles(lng = ~lat, lat = ~lng, weight = 1, label=~nazov.skoly,
        radius = ~sqrt(žiaci.spolu)*100, stroke=T, col="red",layerId = ~ID) 
   })
   
   ## popup ####
   
   # Show a popup at the given location
    showPopup <- function(ID, lat, lng) {
      selectedskola <- skoly_subset()[skoly_subset()$ID == ID,]
      
      content <- paste(tagList(
        tags$strong(selectedskola$nazov.skoly),tags$br(),
        paste("Žiaci spolu:", selectedskola$žiaci.spolu),tags$br()),
        htmlTable(selectedskola[,7:14],header=c("1.r &nbsp","2.r &nbsp","3.r &nbsp","4.r &nbsp","5.r &nbsp","6.r &nbsp","7.r &nbsp","8.r"), rnames = F),
        tagList(
        paste("Absolventi:", selectedskola$absolventi.2017),tags$br(),
        paste("Predpokladaný absolventi:", selectedskola$predpokladani.absolventi.máj.2018),tags$br(),
        paste("Odborný výcvik a prax:", selectedskola$odborný.výcvik.a.prax)
        ))
      
      leafletProxy("map") %>% addPopups(lng=lng, lat=lat, content, layerId = ID)
    }
  
    # When map is clicked, show a popup with city info
    observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_shape_click
      if (is.null(event))
        return()
  
      isolate({
        showPopup(event$id, event$lat, event$lng)
      })
    })
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

