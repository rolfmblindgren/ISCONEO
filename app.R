#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Yrkestilpasningskalkulator:"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("I",
                  "Extraversion:",
                  min = 20,
                  max = 80,
                  value = 50),      
      sliderInput("II",
                  "Agreeablness:",
                  min = 20,
                  max = 80,
                  value = 50),      
      sliderInput("III",
                  "Conscientiousness:",
                  min = 20,
                  max = 80,
                  value = 50),      
      sliderInput("IV",
                  "Emotional stability:",
                  min = 20,
                  max = 80,
                  value = 50),      
      sliderInput("V",
                  "Openness to Experience:",
                  min = 20,
                  max = 80,
                  value = 50)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        DT::dataTableOutput("result"),
        p("Hvis du mener at lista ikke stemmer for deg, kan det ha en rekke årsaker."),
        p("Formlene er basert på amerikansk forskning, noe som gir en viss usikkerhet."),
        p("Videre er det slik at det som bestemmer hva som passer for deg, ikke bare er personlighet, men også hva du har av erfaring, ambisjoner, interesser, og hvor intelligent du er. Av disse faktorene, er personlighet det som er vanskeligst å finne ut av selv."),
        p("Selv om noen av forslagene er langt utenfor hva du selv har erfaring med, og ikke ligner stort på hverandre, så kan samme personlighet godt passe til svært forskjellige yrker. Bare tenk selv på hva folk kan finne på å ha av overraskende hobbier eller fritidsinteresser. "),
        p("Lista er altså forslag som ikke er tatt rett ut av løse lufta, men som du må se i lys av hva du ellers vet om deg selv. Den vil forbedres over tid.")
   
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    library(readr)
    library(readxl)
    library(psych)
    library(DT)

    
    if ( length(getwd()) == 0 ) {
        homedir <- "/srv/shiny-server/STYRK"
    } else { 
        homedir <- paste0(getwd(),"/")
    }

    ISCONEO <- read_delim(paste0(homedir,"share_isco_big5_ratings.csv"),
                         "\t", escape_double = FALSE,
                         col_types = cols(profession = col_character()), 
                         trim_ws = TRUE)
   
   STYRK <- read_delim(paste0(homedir,"STYRK-08.csv"),
                       ";", escape_double = FALSE,
                       col_types = cols(
                         code = col_integer(),
                         parentCode = col_character(),
                         level = col_integer(),
                         name = col_character(),
                         shortName = col_character(),
                         notes = col_character(),
                         validFrom = col_character(),
                         validTo = col_character()),
                       trim_ws = TRUE)
   
    ISCO <- read_excel(paste0(homedir,"index08-draft.xlsx"),
                       col_types = c("numeric", "numeric", "text"))
   
   descriptives <- describe(ISCONEO
                            [c("job_ext","job_agr","job_con","job_sta","job_ope")])

    
    
  output$result <- DT::renderDataTable({
    
    I <- input$I
    II <- input$II
    III <- input$III
    IV <- input$IV
    V <- input$V

   score <- c(I,II,III,IV,V)
    
   
   
     mI <- 4.86
    sdI <- 1.13
               
    mII <- 5.33
   sdII <- 0.98
               
   mIII <- 6.01
  sdIII <- 0.84
               
    mIV <- 4.33
   sdIV <- 1.18
               
     mV <- 4.53
    sdV <- 1.16

   SCONEO[["job_ext"]] <- (ISCONEO[["job_ext"]] - mI)/sdI * 10 + 50
   ISCONEO[["job_agr"]] <- (ISCONEO[["job_agr"]] - mII)/sdII * 10 + 50
   ISCONEO[["job_con"]] <- (ISCONEO[["job_con"]] - mIII)/sdIII * 10 + 50
   ISCONEO[["job_sta"]] <- (ISCONEO[["job_sta"]] - mIV)/sdIV * 10 + 50
   ISCONEO[["job_ope"]] <- (ISCONEO[["job_ope"]] - mV)/sdV * 10 + 50

   b5sim <- function (needle) {
       simscores <-
           cbind(
               ISCONEO[[1]],
               apply
               (ISCONEO[c("job_ext","job_agr","job_con","job_sta","job_ope")],
                   1,
                   function(X){
                       proxy::dist(rbind(needle,X),method="Euclidean")
                   }
               )
           )
       
     yrker <- apply(matrix(simscores[,1]),1,function(X){

         isco88 <- ISCO[which(ISCO[[2]]==X),1][[1]][1]
         
         idx <- which(STYRK[[1]]==isco88)

         ifelse ( length(idx) > 0, STYRK[[idx,4]], NA)
     })

       yrker <- yrker[order(simscores[,2])]
       
       yrker <- cbind(sort(simscores[,2]),yrker)

       yrker[,1] <- format(
           100 - as.numeric(yrker[,1])/as.numeric(yrker[176,1])*100,
           digits=0,
           width=3
       )
       
       colnames(yrker) <- c("Match i prosent","Yrker")
       
       yrker[complete.cases(yrker),]
   }
   
   yrker <- b5sim(score)
   
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
