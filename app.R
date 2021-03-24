library("shiny")
library("tm")
library("wordcloud2")
library("ggfortify")
library("ggplot2")
library("dplyr")
library("genius")
library("RTextTools")
library("shinythemes")
library("OpenImageR")


ui <- shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        textInput("artist", "Enter Artist Name", value = "Mac Miller"),
        textInput("song", "Enter Album Name", value = "Good News"),
        actionButton("submit", "Get Lyrics"),
        fileInput(inputId = "img", label =  "choose an image...", accept = ".png"),
        actionButton('draw', 'Generate WordCloud', class="btn-info"),
        selectInput(inputId ="fontWeight", label = "Choose Weight", choices = c(600, "bold", "normal"), selected = NULL, multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        sliderInput(inputId = "sizeSlider", label = "Word Size", min = 0, max = 2, value = 1, step = .05,
                    round = FALSE, format = NULL, locale = NULL, ticks = TRUE,
                    animate = FALSE, width = NULL, sep = ",", pre = NULL,
                    post = NULL, timeFormat = NULL, timezone = NULL,
                    dragRange = TRUE),
        sliderInput(inputId = "minSlider", label = "Min word Size", min = 0, max = 10, value = 1, step = .05,
                    round = FALSE, format = NULL, locale = NULL, ticks = TRUE,
                    animate = FALSE, width = NULL, sep = ",", pre = NULL,
                    post = NULL, timeFormat = NULL, timezone = NULL,
                    dragRange = TRUE),
        sliderInput(inputId = "gridSize", label = "Grid Size", min = 0, max = 10, value = 1, step = .05,
                    round = FALSE, format = NULL, locale = NULL, ticks = TRUE,
                    animate = FALSE, width = NULL, sep = ",", pre = NULL,
                    post = NULL, timeFormat = NULL, timezone = NULL,
                    dragRange = TRUE),
        sliderInput(inputId = "rotateRatioSlider", label = "Rotate Words", min = 0, max = 1, value = .5, step = .05,
                    round = FALSE, format = NULL, locale = NULL, ticks = TRUE,
                    animate = FALSE, width = NULL, sep = ",", pre = NULL,
                    post = NULL, timeFormat = NULL, timezone = NULL,
                    dragRange = TRUE),
        sliderInput(inputId = "minRotationSlider", label = "Min Rotation", min = -1, max = 1, value = 0, step = .05,
                    round = FALSE, format = NULL, locale = NULL, ticks = TRUE,
                    animate = FALSE, width = NULL, sep = ",", pre = NULL,
                    post = NULL, timeFormat = NULL, timezone = NULL,
                    dragRange = TRUE),
        sliderInput(inputId = "maxRotationSlider", label = "Max Rotation", min = -1, max = 1, value = 0, step = .05,
                    round = FALSE, format = NULL, locale = NULL, ticks = TRUE,
                    animate = FALSE, width = NULL, sep = ",", pre = NULL,
                    post = NULL, timeFormat = NULL, timezone = NULL,
                    dragRange = TRUE),
        sliderInput(inputId = "ellipticitySlider", label = "ellipticity", min = 0, max = 3, value = 1, step = .05,
                    round = FALSE, format = NULL, locale = NULL, ticks = TRUE,
                    animate = FALSE, width = NULL, sep = ",", pre = NULL,
                    post = NULL, timeFormat = NULL, timezone = NULL,
                    dragRange = TRUE)
      ), 
      mainPanel(
        wordcloud2Output("wcplot")
      ), 
      position = c("left", "right"),fluid = TRUE)
  )
)
server <- shinyServer <- function(input,output, session){
 image_upload <- reactive({
   #image <- ifelse(test = is.na(input$img$datapath), yes = input$img$datapath, no = NULL)
   image <- input$img$datapath
   print(image)
   return(image)
 })
  
  wc_data <- reactive({
    input$submit
    input$submitImage
    isolate({
      withProgress({
        setProgress(message = "Generating Song Lyrics...")
        artistName <- tolower(input$artist)
        songName <- tolower(input$song)
        lyrics_og <- genius_lyrics(artist = artistName, song = songName)
        repeated_lines <- lyrics_og %>% group_by(lyric) %>% tally()
        repeated_lines <- data.frame(word =repeated_lines$lyric, freq = repeated_lines$n)
        repeated_lines <- repeated_lines %>% arrange(desc(freq))
        single_words <- as.character(lyrics_og$lyric)
        single_words <- single_words %>%
          removePunctuation() %>%
          removeNumbers() %>%
          stripWhitespace()
        single_wordsCorpus <- Corpus(VectorSource(single_words)) %>%
          TermDocumentMatrix() %>%
          as.matrix()
        single_wordsCorpus <- sort(rowSums(single_wordsCorpus), decreasing=TRUE)
        single_wordsCorpus <- data.frame(word = names(single_wordsCorpus), freq=single_wordsCorpus, row.names = NULL)
        print(paste0(songName, " - ", artistName))
        wc_text2 <- rbind(repeated_lines, single_wordsCorpus)
        wc_text2$freq <- scale(wc_text2$freq)
        wc_text2$freq <- abs(wc_text2$freq)
        
        freq <- wc_text2$freq
        freq <- ifelse(test = freq < 2, 
                       yes = freq, 
                       no = mean(freq) * freq)
        cloud_text <- data.frame(word = wc_text2$word, freq = freq)
        cloud_text
      })
    })
  })
  create_wordcloud <- function(data, 
                               num_words = 500, 
                               background = "rgba(255, 0, 0, 0.01)",
                               #figPath = "/Users/dre/Downloads/test_shiny/bass.png",
                               size = input$sizeSlider, 
                               minSize = input$minSlider, 
                               gridSize =input$gridSize,
                               fontWeight = input$fontWeight,
                               rotateRatio = input$rotateRatioSlider,
                               minRotation = input$minRotationSlider,
                               maxRotation = input$minRotationSlider,
                               ellipticity = input$ellipticitySlider)#, #size=input$sizeSlider, 
  #minSize = input$minSlider, gridSize =input$gridSize,
  #fontWeight = fontWeight)
  #figPath = input$figPath)
  wordcloud_rep <- repeatable(wordcloud2)
  output$wcplot <- renderWordcloud2({
    wordcloud2(wc_data(), figPath = image_upload())
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
