## ---------------------------
## TITANIC-ORAKEL
## ---------------------------

## Pakete ggf. installieren und importieren
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, tidyverse, e1071)

## SVM-Modell laden
model_svm <- readRDS("titanic-orakel_svm.rds")

## ---------------------------

## User-Interface
ui <- fluidPage(
  
  # UTF-Encoding
  tags$head(
    tags$meta(charset = "UTF-8"),
  ),
  
  # Name der App
  titlePanel("Titanic-Orakel"),
  
  # Layout
  sidebarLayout(
    
    # User-Input
    sidebarPanel(
      
      # Überschrift für Parameter
      tags$label(h3("Dateneingabe", style = "margin-bottom: 20px;")),
      
      # Features eingeben
      sliderInput("pclass", "Passagierklasse:",
                  min = 1, max = 3,
                  value = 2),
      
      radioButtons("sex", inline = TRUE, "Geschlecht:",
                   c("Frau" = 1,
                     "Mann" = 0)),
      
      numericInput("age", "Alter:",
                   value = "0"),
      
      selectInput("embarked", "Einstiegshafen:",
                  c("Cherbourg" = 0,
                    "Southampton" = 1,
                    "Queenstown" = 2)),
      
      # Action-Button löst observeEvent() aus
      actionButton("action", label = "Los geht's!", class = "btn btn-primary"),
      
      # Margin für Sidebar
      style = "margin-top: 20px;"
      
    ), # Ende User-Input
    
    # Output: Prognose
    mainPanel(
      
      # Überschrift für Prognose
      tags$label(h3("Hättest du die Titanic überlebt?", style = "margin-bottom: 20px;")),
      
      # Erklärtext
      HTML("<p>Fülle das Formular aus, um herauszufinden, ob du das Unglück überstehst oder nicht.</p>
            <p>Der Wert <i>p(überlebt)</i> gibt die Überlebenswahrscheinlichkeit an, während der Wert <i>p(verstorben)</i> die Wahrscheinlichkeit ausdrückt, dass du verstirbst.</p>
            <p><strong>Beispiel:</strong> Ein <i>p(überlebt)</i>-Wert von 0.43 sagt aus, dass die Wahrscheinlichkeit des Überlebens bei 43% liegt.</p>"
           ),
      
      # Anzeige der Prognose
      tags$div(
        tableOutput("value1"),
        style = "margin-top: 20px;"
      )
      
    ) # Ende Output
    
  ) # Ende Layout
  
) # Ende User-Interface

## ---------------------------

## Server-Logik
server <- function(input, output, session) {
  
  # observeEvent reagiert auf Action-Button
  observeEvent(input$action, {
    
    # User-Input Variablen zuweisen
    pclass <- as.numeric(input$pclass)
    sex <- as.numeric(input$sex)
    age <- input$age
    embarked <- as.numeric(input$embarked)
    
    # Input in Data Frame speichern
    data <- data.frame(pclass, sex, age, embarked)
    
    # Prognosen auf Basis des SVM-Modells anstellen
    result <- predict(model_svm, data, probability = TRUE)
    my_result <- data.frame(attr(result, "probabilities"))
    
    # Output-Tabelle generieren, wobei Spalten umbenannt werden
    output$value1 <- renderTable(my_result %>%
                                   rename("p(überlebt)" = X1, "p(verstorben)" = X0))
    
  }) # Ende observeEvent
  
} # Ende Server-Logik

## ---------------------------

## User-Interface und Server-Logik zusammenbauen
shinyApp(ui = ui, server = server)

## ---------------------------