library(shiny)
library(wordcloud)
library(RColorBrewer)
library(tm)



                            ###ZONA CÓDIGO GENERAL###
########################################################################################

##Caga de textos##
spechees_list <- read.csv('./speeches/cods.csv', encoding = 'UTF-8', stringsAsFactors = F)
speeches <- spechees_list$COD
names(speeches) <- spechees_list$Speech

##Carga stopwords##
stop_words_en <- stopwords("en")

##Funciones##

leer_texto <- function(speech){
  
  if(!speech %in% speeches)
    stop('speech not found')
  texto <- readLines(sprintf("./speeches/%s.txt", speech), encoding = "UTF-8")
  return(texto)
  
}

preprocesado <- function(lineas_texto){
  
  lineas_texto <- gsub("[[:punct:]]", " ", lineas_texto) # Eliminamos puntuación
  lineas_texto <- gsub("[0-9]", " ", lineas_texto) # Eliminamos números
  lineas_texto <- trimws(lineas_texto) # Elimina espacios por delante y por detrás
  lineas_texto <- unlist(strsplit(lineas_texto, " ")) # Convierto en vector de palabras
  lineas_texto <- lineas_texto[lineas_texto != ""] # elimino los espacios en blanco
  lineas_texto <- tolower(lineas_texto) # Paso a minúsculas
}

matriz_palabras <- function(libro){
  
  texto <- leer_texto(libro)
  corpus <- preprocesado(texto)
  
  #corpus es todo lo que haya en corpus que no esté en el vector de stopwords
  corpus <- corpus[!(corpus %in% stop_words_en)]
  
  #la función table me devuelve un objeto tabla de cada palabra y su frecuencia
  #la convertimos a dataframe para facilitar su pectura y proceamiento
  tabla <- as.data.frame(table(corpus), stringsAsFactors = F)
  
  #se ordena el dataframe por la columna Freq de mayor a menor
  tabla <- tabla[order(tabla$Freq, decreasing = T),]
  
  return(tabla)
}




###Zona Ui###

ui <- fluidPage(
  
  title = "Oscar's speeches 2020",
  
  sidebarLayout(
    
    sidebarPanel(
      
    selectInput(inputId = 'select', label = "Select a speech and press 'Go!:",
                choices = speeches),
    
    actionButton(inputId = 'change', label = 'Go!'),
    
    hr(),
    
    sliderInput(inputId = 'freq', label = 'Minimum frequency: ',
                min = 1, max = 50, value = 15),
    
    sliderInput(inputId = 'num_words', label = 'Number of words to display:',
                min = 1, max = 300, value = 50)
    
    
   ),
  
   mainPanel(
     
       h3('WordCloud'),
       plotOutput('cloud')
     
   )
 )
)

###Zona server###

server <- function(input, output){
  
  libroCorpus <- reactive({
    
    input$change
    
    isolate({
      
      withProgress({
        
        setProgress(message = 'Processing speech ...')
        matriz_palabras(input$select)
        
      })
      
    })
    
  })
  
  output$cloud <- renderPlot({
    
    v <- libroCorpus()
    wordcloud(words = v$corpus, 
              freq = v$Freq,
              min.freq = input$freq,
              max.words = input$num_words,
              colors = brewer.pal(8, "Dark2")
    )
    
  },
  height = 650)
  
}

shinyApp(ui = ui, server = server)

