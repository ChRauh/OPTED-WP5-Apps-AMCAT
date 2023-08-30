library(shiny)

library(dplyr)
library(magrittr)
library(tidyr)
library(forcats)
library(stringr)
library(readr)

library(feather)

# OPTED blue
# #0063a6

# Autocomplete lists ####
ac_parties <- read_rds("./Data/ac-parties.rds")
ac_laws <- read_rds("./Data/ac-laws.rds")
ac_speakers <- read_rds("./Data/ac-speakers.rds")


# Define UI
ui <- fluidPage(
                # theme = shinytheme("cerulean"),
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "cerulean.OPTED.css") # Edited cerulean scheme with OPTED branding
                ),
                navbarPage(
                  title = "ParlLawSpeech: Extractor",
                  id = "tabs",
                  tabPanel("Main",
                           sidebarLayout(position = "right",
                           sidebarPanel(
                             # tags$h3("Your choices"),
                             selectInput("parl", label = h4("Parliament"),
                                         choices = list("DE: Bundestag" = "DE-Bundestag", 
                                                        "UK: House Of Commons" = "UK-HouseOfCommons"), 
                                         selected = 1),
                             
                             helpText("Choose one of the parliamentary chambers we currently feature here (required). The filters below can be left blank. 
                                      In this case the maximum range of values in the raw data is returned."),
                             
                             dateRangeInput(
                               inputId = "dates", 
                               label = h5("Date range (YYYY-MM-DD)"),
                               start = NA,
                               end = NA),

                             selectizeInput(
                               inputId = 'laws',
                               label = h5('Debated Law'),
                               choices = ac_laws,
                               selected = NULL,
                               multiple = TRUE, # allow for multiple inputs
                               options = list(create = FALSE) # if TRUE, allows newly created inputs
                             ),
                             
                             selectizeInput(
                               inputId = 'party',
                               label = h5('Party (of speaker)'),
                               choices = ac_parties,
                               selected = NULL,
                               multiple = TRUE, # allow for multiple inputs
                               options = list(create = FALSE) # if TRUE, allows newly created inputs
                             ),
                             
                             selectizeInput(
                               inputId = 'speaker',
                               label = h5('Speakers'),
                               choices = ac_speakers,
                               selected = NULL,
                               multiple = TRUE, # allow for multiple inputs
                               options = list(create = FALSE) # if TRUE, allows newly created inputs
                             ),
                             
                             helpText("Once you submit your choices, we collect the data for you. Given the size of the text corpora, this may take a couple of seconds."),
                             actionButton("submit", label = "Submit!", style = "color: #0063a6;")
                             # submitButton(text = "Submit!")
                             
                           ), # sidebarPanel
                           mainPanel(
                             h4("ParlLawSpeech: Extractor"),
                             p(HTML("<i style = \"color: #0063a6\">Quickly extract full-text speech data sets
                               from the political debates of different national parliaments in Europe.</i>")),
                             p(HTML("")),
                             p(HTML("Once you have set and submitted the parliament of interest to you (with optional filters) in the grey box, we summarise your selection below and offer different download options.")),
                             p(HTML("<strong>NOTE:</strong> Currently the app runs on sampled mock data drawn from <a href=\"https://doi.org/10.7910/DVN/L4OAKN\" target=\"_blank\">Rauh and Schwalbach 2020</a> only.")),
                             p(HTML("Before <i>using this material in your work</i>, please consult the 'About' page above.")),
                             p(HTML("This proto-type application has been developed in <a href=\"https://opted.eu/designing-an-infrastructure/wp5-parliamentary-government-and-legal-texts\" target=\"_blank\">Work Package 5</a> 
                                    of the <a href=\"https://opted.eu\" target=\"_blank\">OPTED initiative</a>. 
                                    This project has received funding from the European Union’s Horizon 2020 research & innovation programme under <a href=\"https://cordis.europa.eu/project/id/951832\" target=\"_blank\">grant agreement No 951832</a>.<br>")),
                             p(HTML("<br>")),

                             h4("Your current selection"),
                             verbatimTextOutput("summary"),
                             p(HTML("<br>")),
                             
                             h4("Download options"),
                             p(HTML("<br>")),
                             p(downloadButton("downloadRDS", "Download .rds"),
                               "   ",
                               downloadButton("downloadTSV", "Download .tsv"),
                               "   ",
                               downloadButton("downloadFEATHER", "Download .feather")
                               )
                             
                           ) # mainPanel
                           ) # sidebarLayout

                  ), 
                  tabPanel("About", 
                           h3("Background"),
                           a(img(src='OPTED_logo_transparent.png', style = "float:right; width: 150px; size: contain;", alt = "OPTED initiative"), href="https://opted.eu"),
                           p(HTML("<p>This proto-type application has been developed in the context of the <a href=\"https://opted.eu\" target=\"_blank\">OPTED initiative</a>, aiming to facilitate access to systematic information from political texts.<p>
                           It is part of <a href=\"https://opted.eu/designing-an-infrastructure/wp5-parliamentary-government-and-legal-texts\" target=\"_blank\">Work Package 5</a>, focusssing on parliamentary, government and legal texts, in particular.<p>
                           The project has received funding from the European Union’s Horizon 2020 research & innovation programme under <a href=\"https://cordis.europa.eu/project/id/951832\" target=\"_blank\">grant agreement No 951832</a>.<p>
                           For feedback and questions on this particular application, please contact to <a href=\"http://christian-rauh.eu\" target=\"_blank\">Christian Rauh</a>. <br> ")),
                           p(HTML("<br>")),
                           h3("How to cite"),
                           p(HTML("When using any of the material generated here in your own work, please refer to this application and cite the underlying data sources.<br>
                           At the moment, all parliamentary text data are drawn from <a href=\"https://doi.org/10.7910/DVN/L4OAKN\" target=\"_blank\">Rauh and Schwalbach 2020</a>.")),
                           p(HTML("<br>")),
                           
                           h3("Project partners"),
                           p(HTML("<br>")),
                           tags$table(style = "border-collapse: separate; border-spacing: 50px 0; padding: 10px 0;",
                                      tags$tr(tags$th(a(img(src='WZB_Komb_portrait_Web_engl.png', style = "float:center; height: 100px; size: contain;", alt = "WZB"), href = "https://www.wzb.eu/en/persons/christian-rauh")),
                                              tags$th(a(img(src='pti_logo.png', style = "float:center; height: 100px; size: contain;", alt = "TKPTI"), href = "https://politikatudomany.tk.hu/en/researcher/sebok-miklos")),
                                              tags$th(a(img(src='CCCP_Logo.png', style = "float:center; height: 100px; size: contain;", alt = "CCCP"), href = "https://cccp.uni-koeln.de/en/team/core-faculty/prof-dr-sven-oliver-proksch"))),
                                      tags$tr(tags$td(HTML("<br>")),
                                              tags$td(HTML("<br>")),
                                              tags$td(HTML("<br>"))),
                                      tags$tr(tags$td("Christian Rauh"),
                                              tags$td(HTML("Miklós Sebők<br>Anna Székely<br>Péter Visnovitz")),
                                              tags$td(HTML("Sven-Oliver Proksch<br>Jan Schwalbach<br>Alexander Dalheimer"))))
                           # p(a(img(src='WZB_Komb_portrait_Web_engl.png', style = "float:left; height: 100px; size: contain;", alt = "OPTED initiative"), href="https://opted.eu"),
                           #   a(img(src='pti_logo.png', style = "float:center; height: 100px; size: contain;", alt = "OPTED initiative"), href="https://opted.eu"),
                           #   a(img(src='CCCP_Logo.png', style = "float:rigth; height: 100px; size: contain;", alt = "OPTED initiative"), href="https://opted.eu"))
                           
                  )
                ) # navbarPage
) # fluidPage



# Define server logic 
server <- function(input, output) {
  
  
  # Data selection ####
  # Basis to let the app select the right data based on user parliament selection
  parl.select <- data.frame(parliament = c("UK-HouseOfCommons", "DE-Bundestag"),
                            app.path = c("./Data/hc-mock.feather", "./Data/bt-mock.feather"), # Relative path from app directory
                            nspeeches = c(10000, 10000)) 
  
  # Update user parliament choice upon submit button
  user.parliament <- eventReactive(input$submit, {
    input$parl
  })
  
  # Update user party choice upon submit button
  user.dates <- eventReactive(input$submit, {
    input$dates
  })
  
  # Update user party choice upon submit button
  user.laws <- eventReactive(input$submit, {
    input$laws
  })
  
  # Update user party choice upon submit button
  user.party <- eventReactive(input$submit, {
    input$party
  })
  
  # Update user speaker choice upon submit button
  user.speaker <- eventReactive(input$submit, {
    input$speaker
  })

  # Load data  of selected parliament,
  # and apply optional filters
  out.data <- eventReactive(input$submit, {

    showModal(modalDialog(title = "Just a few seconds ...", "Loading and filtering data", footer=NULL))

    df <- read_feather(parl.select$app.path[parl.select$parliament == user.parliament()])
    
    # Date filter, if present
    if (!is.na(user.dates()[1])) { 
      df <- df %>% 
        filter(date >= user.dates()[1]) %>% 
        filter(date <= user.dates()[2])
    }
    
    # Party filter, if present
    if (!is.null(user.party())) { 
      df <- df %>% 
        filter(party %in% user.party())
    }
    
    # Speaker filter, if present
    if (!is.null(user.speaker())) { 
      df <- df %>% 
        filter(speaker %in% user.speaker())
    }
    
    # Law filter, if present
    if (!is.null(user.laws())) { 
      df <- df %>% 
        filter(law %in% user.laws())
    }
    
    # Return resulting data
    return(df)

    })
  
  # RDS download
  output$downloadRDS <- downloadHandler(
    filename = function() {
      paste0('ParlLawSpeech-Extraction', Sys.Date(), '.rds')
    },
    content = function(con) {
      write_rds(out.data(), con)
    }
  )
  
  # FEATHER download
  output$downloadFEATHER <- downloadHandler(
    filename = function() {
      paste0('ParlLawSpeech-Extraction', Sys.Date(), '.feather')
    },
    content = function(con) {
      write_feather(out.data(), con)
    }
  )
  
  # TSV download
  output$downloadTSV <- downloadHandler(
    filename = function() {
      paste0('ParlLawSpeech-Extraction', Sys.Date(), '.tsv')
    },
    content = function(con) {
      write_tsv(out.data(), con)
    }
  )
  
   

  # Summary output for start page
  output$summary <- renderText({
    
    if(input$submit == 0) {return("Nothing submitted yet!")}
    
    date.range <- ifelse(is.na(user.dates()[1]) & is.na(user.dates()[2]), " ",  paste(user.dates(),collapse = " to "))
    

    summary <- paste0("Parliament:\t\t", user.parliament(), "\n",
                      "Raw data:\t\t", parl.select$app.path[parl.select$parliament == user.parliament()], "\n",
                      "Speeches in raw data:\t",  parl.select$nspeeches[parl.select$parliament == user.parliament()], "\n",
                      "\n",
                      "Date range:\t\t", date.range, "\n",
                      "Parties:\t\t", paste(user.party(), collapse = ", "), "\n",
                      "Laws:\t\t\t", paste(user.laws(), collapse = ", "), "\n",
                      "Speakers:\t\t", paste(user.speaker(), collapse = ", "), "\n",
                      "\n",
                      "Selected speeches:\t", nrow(out.data()))
    
    removeModal() # End waiting dialogue here 
    
    return(summary)
    
  })

  
}




shinyApp(ui, server)