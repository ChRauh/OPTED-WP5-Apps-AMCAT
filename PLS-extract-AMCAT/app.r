library(shiny)
library(tidyverse)
library(feather)

# OPTED blue
# #0063a6

# Establish connection to database (AMCAT server)
library(amcat4r)
# host <- "https://parliaments.opted.eu/api/" # Vienna Server
host <- "https://opted.amcat.nl/amcat" # AMS server
amcat_login(host) # MUST BE DONE ON THE SHINY SERVER AND ENCRYPTED THERE!


# Autocomplete lists ####
ac_parties <- read_rds("./Data/ac-parties.rds")
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
                                         choices = list("AT: Nationalrat" = "speeches_austria",
                                                        "CZ: Poslanecká snemovna" = "speeches_cz",
                                                        "DE: Bundestag" = "speeches_germany", 
                                                        "DK: Folketing" = "speeches_denmark",
                                                        "ES: Congreso de los Diputados" = "speeches_spain", 
                                                        "EU: European Parliament" = "speeches_ep",
                                                        "HR: Hrvatski sabor" = "speeches_croatia",
                                                        "HU: Országgyűlés" = "speeches_hungary"),
                                         selected = 1),
                             
                             helpText(HTML("Choose one of the parliamentary chambers covered in ParlLawSpeech (<i>required</i>).")),
                             helpText("The filters below can be left blank. In this case the maximum range of values in the raw data is returned."),
                             
                             
                             textInput("words", label = h5("Keywords in speech text"), ""),
                             helpText("Filter speeches along keywords appearing in the full text."),
                             helpText(HTML("You may use boolean operators (AND/OR), include wildcards such as ? or *, search for \"exact phrases\", or nest combinations of words and phrases in brackets. More detail for complex queries is available <a href='https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html#query-string-query-notes' target='_blank' style='text-decoration:none'>here</a>.")),
                             
                             
                             dateRangeInput(
                               inputId = "dates", 
                               label = h5("Date range (YYYY-MM-DD)"),
                               start = NA,
                               end = NA),

                             # selectizeInput(
                             #   inputId = 'laws',
                             #   label = h5('Debated Law'),
                             #   choices = ac_laws,
                             #   selected = NULL,
                             #   multiple = TRUE, # allow for multiple inputs
                             #   options = list(create = FALSE) # if TRUE, allows newly created inputs
                             # ),
                             
                             selectizeInput(
                               inputId = 'party',
                               label = h5('Party (of speaker)'),
                               choices = NULL,
                               selected = NULL,
                               multiple = TRUE, # allow for multiple inputs
                               options = list(create = FALSE) # if TRUE, allows newly created inputs
                             ),
                             
                             selectizeInput(
                               inputId = 'speaker',
                               label = h5('Speaker(s)'),
                               choices = NULL,
                               selected = NULL,
                               multiple = TRUE, # allow for multiple inputs
                               options = list(create = FALSE) # if TRUE, allows newly created inputs
                             ),
                             
                             helpText(HTML("Once you submit your choices, we collect the data for you.<br>
                                           <b>Given the size of the text corpora, this may take a couple of seconds!</b><br>")),
                             actionButton("submit", label = "Submit!", style = "color: #0063a6;")
                             # submitButton(text = "Submit!")
                             
                           ), # sidebarPanel
                           mainPanel(
                             h4("ParlLawSpeech: Extractor"),
                             p(HTML("<i style = \"color: #0063a6\">Quickly extract full-text speech data sets
                               from the political debates of different national parliaments in Europe.</i>")),
                             p(HTML("<br>")),
                             p(HTML("Choose the parliament of interest to you and apply filters (optional) on the right-hand side. We then collect the respective text corpora and provide you with different download options below.")),
                             p(HTML("The <i>text data</i> are extracted from the development version of the encompassing <b><a href=\"https://chrauh.github.io/ParlLawSpeechTutorials\" target=\"_blank\">ParlLawSpeech</a></b> collection.<br>
                                    <b>Before using any of the material presented here in your own work, consult the 'about' page above!</b>")),
                             p(HTML("This prototype application has been developed in <a href=\"https://opted.eu/designing-an-infrastructure/wp5-parliamentary-government-and-legal-texts\" target=\"_blank\">Work Package 5</a> 
                                    of the <a href=\"https://opted.eu\" target=\"_blank\">OPTED initiative</a>, a project that received funding from the European Union’s Horizon 2020 research & innovation programme (<a href=\"https://cordis.europa.eu/project/id/951832\" target=\"_blank\">grant agreement No 951832</a>).<br>")),
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
                           p(HTML("<p>This prototype application has been developed in the context of the <a href=\"https://opted.eu\" target=\"_blank\">OPTED initiative</a>, which aims to facilitate the collection of and access to large-scale collections of political texts and the systematic information hidden therein.<p>
                           The application has been developed by <a href=\"https://opted.eu/designing-an-infrastructure/wp5-parliamentary-government-and-legal-texts\" target=\"_blank\">Work Package 5</a> (see the full list of partners below), which focusses on parliamentary, government and legal texts.<br>
                           Johannes Gruber (<a href=\"https://opted.eu/team/wp7-pre-processing-storage-and-data-sharing/\" target=\"_blank\">WP7</a>) and Paul Baluff (<a href=\"https://opted.eu/team/wp3-journalistic-mass-mediated-political-texts/\" target=\"_blank\">WP3</a>) have provided valuable input regarding integration of this app with the <a href = \"https://amcat.nl/book/01._why-amcat\" target=\"_blank\">AMCAT server backend</a>.<p>
                           The project has received funding from the European Union’s Horizon 2020 research & innovation programme under <a href=\"https://cordis.europa.eu/project/id/951832\" target=\"_blank\">grant agreement No 951832</a>.<p>
                           Please contact <a href=\"http://christian-rauh.eu\" target=\"_blank\">Christian Rauh</a> for feedback or questions on this particular application. <br> ")),
                           p(HTML(" ")),
                           h3("How to cite"),
                           p(HTML("When <i>using any of the material generated here in your own work</i>, please refer to this application and cite the underlying data sources.<br>
                           The parliamentary text data offered here draw on a pre-publication version of the <i><a href=\"http://htmlpreview.github.io/?https://github.com/ChRauh/ChRauh.github.io/blob/main/ParlLawSpeechTutorials.html\" target=\"_blank\">ParlLawSpeech</a></i> data collection.<br>
                    For now please use the citation below but check back for updates. Thank you!")),
                           # code("Rauh, Christian, Péter Gelányi, Lukas Hetzer, Sven-Oliver Proksch, Jan Schwalbach, and Miklós Sebők (2023) \'ParlLawSpeech - Public Access Website\', OPTED Deliverable D5.6"), 
                           p(HTML("Rauh, Christian; Péter Gelányi; Lukas Hetzer; Sven-Oliver Proksch; Jan Schwalbach; and Miklós Sebők (2023) \'ParlLawSpeech - Public Access Website\', OPTED Deliverable D5.6"), style="text-align:left;color:#0063a6"),
                           # div(style = "border-style: solid; border-color: #0063a6;",
                           #      p(HTML("Rauh, Christian; Péter Gelányi; Lukas Hetzer; Sven-Oliver Proksch; Jan Schwalbach; and Miklós Sebők (2023) \'ParlLawSpeech - Public Access Website\', OPTED Deliverable D5.6"), style="text-align:center")),
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
                                              tags$td(HTML("Miklós Sebők<br>Anna Székely<br>Péter Visnovitz<br>Péter Gelányi")),
                                              tags$td(HTML("Sven-Oliver Proksch<br>Jan Schwalbach<br>Lukas Hetzer<br>Alexander Dalheimer"))))
                           # p(a(img(src='WZB_Komb_portrait_Web_engl.png', style = "float:left; height: 100px; size: contain;", alt = "OPTED initiative"), href="https://opted.eu"),
                           #   a(img(src='pti_logo.png', style = "float:center; height: 100px; size: contain;", alt = "OPTED initiative"), href="https://opted.eu"),
                           #   a(img(src='CCCP_Logo.png', style = "float:rigth; height: 100px; size: contain;", alt = "OPTED initiative"), href="https://opted.eu"))
                           
                  )
                ) # navbarPage
) # fluidPage



# Define server logic 
server <- function(input, output, session) {
  
  # Auto-complete party field
  updateSelectizeInput(session, 'party', choices = ac_parties, server = TRUE)
  
  # Auto-complete speaker field
  updateSelectizeInput(session, 'speaker', choices = ac_speakers, server = TRUE)

  
  # Data selection ####
  # Basis to let the app select the right data based on user parliament selection
  parl.select <- data.frame(parliament = c("speeches_austria", "speeches_cz", "speeches_germany", "speeches_spain", "speeches_ep", "speeches_croatia", "speeches_denmark", "speeches_hungary"), # Equals the index name in AMCAT database
                            parl.name = c("Nationalrat (Austria)", "Poslanecká snemovna (Czech Republic)", "Bundestag (Germany)", "Congreso de los Diputados (Spain)", "European Parliament (EU)", "Hrvatski sabor (Croatia)", "Folketing (Denmark)", "Országgyűlés (Hungary)"),
                            nspeeches = c(205110, 391306, 188230, 268329, 300168, 405260, 715928, 487877)) 
  
  # Update user parliament choice upon submit button
  user.parliament <- eventReactive(input$submit, {
    input$parl
  })
  
  # Update user query upon submit button
  user.words <- eventReactive(input$submit, {
    string <- paste0("text:(", input$words, ")")
    if(string == "text:()"){string <- "text:()"} # Ensure that empty queries are treated as such
    return(string)
  })
  
  # Update user party choice upon submit button
  user.dates <- eventReactive(input$submit, {
    input$dates
  })
  
  # # Update user law choice upon submit button
  # user.laws <- eventReactive(input$submit, {
  #   input$laws
  # })
  
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
    
    # df <- data.frame(NULL)

    showModal(modalDialog(title = "Give us a minute ...", "We are retrieving lots of text data for you! ", footer=NULL))


    # Construct query
    # Easier to filter dates through there ...
    
    keywords <- user.words()
    if(keywords == "text:()"){keywords = NULL} # Ensure that empty queries are treated as such

    # Extract dates
    mindate = user.dates()[1] %>% as.character()
    maxdate = user.dates()[2] %>% as.character()

    # Adapt empty fields to elastic language
    if(is.na(mindate)){mindate = "*"}
    if(is.na(maxdate)){maxdate = "*"}

    # Date search string (elastic language)
    date = paste0("date:[", mindate, " TO ", maxdate,"]")

    # Add date to text query if keywords field has content
    # otherwise we search for date range only ("* TO*" if no dates are supplied)
    query <- NULL # Empty, the fall-back
    if(!is.null(keywords) & (mindate == "*" & maxdate == "*")){query = keywords} # If  there are keywords but not dates, search for keywords only
    if(is.null(keywords) & (mindate != "*" | maxdate != "*")){query = date} # If there are no keywords but at least one date, search for date string
    if(!is.null(keywords) & (mindate != "*" | maxdate != "*")){query = paste0(keywords, " AND ", date)} # If there are keywords and at least one date, search for both keyowrds and date string
      
    print(query) # For inspection in console
    
    
    # Party and speaker filters
    party = user.party() %>% as.character()
    speaker = user.speaker() %>% as.character()

    filter.list <- list(speaker = speaker, party = party)

    if(is.null(party)){filter.list$party <- NULL}
    if(is.null(speaker)){filter.list$speaker <- NULL}
    if(is.null(speaker) & is.null(party)){filter.list <- NULL}

    # Extract from AMCAT
    df <- amcat4r::query_documents(index = user.parliament(),
                            queries = query,
                            fields = c("date", "speaker", "party", "text", "title", "agenda"),
                            filters = filter.list,
                            per_page = 100,
                            scroll = "10m", # Should suffice, I hope
                            max_pages = Inf)



    
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
      write_feather(out.data() %>%  mutate(date = as.character(date)), con)
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
    
    summary <- paste0("Parliament:\t\t\t\t", parl.select$parl.name[parl.select$parliament == user.parliament()], "\n", "\n",
                      "Server:\t\t\t\t\t", host, "\n", 
                      "Database:\t\t\t\t", user.parliament(), "\n", 
                      "Total number of speeches:\t\t", parl.select$nspeeches[parl.select$parliament == user.parliament()], "\n", "\n",
                      "Keyword filter:\t\t\t\t", user.words() %>% str_remove(fixed("text:()")), "\n", 
                      "Date filter:\t\t\t\t", date.range, "\n", 
                      "Party filter:\t\t\t\t", paste(user.party(), collapse = ", "), "\n", 
                      "Speaker filter:\t\t\t\t", paste(user.speaker(), collapse = ", "), "\n", "\n",
                      "Filtered speeches for download:\t\t", nrow(out.data()))
    
    removeModal() # End waiting dialogue here 
    
    return(summary)
    
  })

  
}




shinyApp(ui, server)