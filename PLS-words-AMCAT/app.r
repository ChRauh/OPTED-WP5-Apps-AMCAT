##################################################################
# Project:  OPTED WP5 Apps based on AMCAT server
# Task:     Establish app to query content of speeches
# Author:   @ChRauh / 23.08.2023
#################################################################



# Packages ####
library(shiny)
library(tidyverse)
# library(dplyr)
# library(magrittr)
# library(tidyr)
# library(forcats)
# library(ggplot2)
# library(stringr)
# library(readr)

library(plotly) # 4.10.0
library(gmodels)
#library(feather)

# Establish connection to database (AMCAT server)
library(amcat4r)
host <- "https://parliaments.opted.eu/api/"
amcat_login(host) # MUST BE DONE ON THE SHINY SERVER AND ENCRYPTED THERE!


# OPTED blue
# #0063a6


# Define UI
ui <- fluidPage(
  # theme = shinytheme("cerulean"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "cerulean.OPTED.css") # Edited cerulean scheme with OPTED branding
  ),
  navbarPage(
    title = "Words in Parliament",
    id = "tabs",
    tabPanel("Overview & Input",
             sidebarLayout(position = "right",
                           sidebarPanel(
                             # tags$h3("Your choices"),
                             selectInput("parl", label = h4("Your parliament"),
                                         choices = list("AT: Nationalrat" = "speeches_austria",
                                                        "CZ: Poslanecká snemovna" = "speeches_cz",
                                                        "DE: Bundestag" = "speeches_germany", 
                                                        "ES: Congreso de los Diputados" = "speeches_spain", 
                                                        "EU: European Parliament" = "speeches_ep",
                                                        "HR: Hrvatski sabor" = "speeches_croatia"), 
                                         selected = 3),
                             helpText("Choose one of the parliamentary chambers featured in ParlLawSpeech."),
                             
                             textInput("words", label = h4("Your query"), "migration* OR flucht*"),
                             helpText("The words you want to track in parliamentary speeches."),
                             helpText(HTML("You may use boolean operators (AND/OR), include wildcards such as ? or *, search for \"exact phrases\", or nest combinations of words and phrases in brackets. More detail for complex queries is available <a href='https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html#query-string-query-notes' target='_blank' style='text-decoration:none'>here</a>.")),
                             helpText("Once you submit your choices, we collect the data for you. Given the size of the text corpora, this may take a couple of seconds."),
                             actionButton("submit", label = "Submit!", style = "color: #0063a6;")
                             # submitButton(text = "Submit!")
                             
                           ), # sidebarPanel
                           mainPanel(
                             h4("Words in Parliament"),
                            p(HTML("<i style = \"color: #0063a6\">Quickly analyze how
                               specific words have featured in political debates of different national parliaments in Europe.</i>")),
                             p(HTML("<br>")),
                             p(HTML("Once you have chosen the parliament and words of interest to you on the right-hand side,
                               we visualize their prominence in all plenary speeches over <i>time</i>, across <i>different parties</i>, and across <i>individual speakers</i>.")),
                             p("The menu on top of this page leads you to these results. All graphics can be customized and saved by hoovering over them. 
                               You may also download the underlying data."),
                             p(HTML("Before <i>using this material in your work</i>, please consult the 'About' page above.")),
                             p(HTML("This proto-type application has been developed in <a href=\"https://opted.eu/designing-an-infrastructure/wp5-parliamentary-government-and-legal-texts\" target=\"_blank\">Work Package 5</a> 
                                    of the <a href=\"https://opted.eu\" target=\"_blank\">OPTED initiative</a>. 
                                    This project has received funding from the European Union’s Horizon 2020 research & innovation programme under <a href=\"https://cordis.europa.eu/project/id/951832\" target=\"_blank\">grant agreement No 951832</a>.<br>")),
                             p(HTML("<br>")),
                             
                             h4("Your current selection"),
                             verbatimTextOutput("summary")
                             
                           ) # mainPanel
             ) # sidebarLayout
             
    ), # Navbar 1, tabPanel
    tabPanel("Time", 
             sidebarLayout(position = "left",
                           sidebarPanel(width = 3, # Out of 12
                                        helpText("This plot illustrates the prominence of your words of interest in the chosen parliament over time."),
                                        helpText("It shows the <i>monthly share of speeches that match your query<i>."),
                                        helpText("Hoover over the plot to show and hide time series, to adapt the scales, or to save the result."),
                                        helpText("You can also download the monthly time series, but note the usage and citation requirements on the 'about' page."),
                                        downloadButton("downloadTime", "Download time series")),
                           mainPanel(plotlyOutput("timeplot"))
             )
    ),
    tabPanel("Parties",
             sidebarLayout(position = "left",
                           sidebarPanel(width = 3, # Out of 12
                                        helpText("This plot illustrates the prominence of your keywords across the parties in the respective parliament."),
                                        helpText(HTML("It shows the <i>share of party speeches that match your query</i> in the respective parliament.")),
                                        helpText("Hoover over the plot to customize it or to save the resulting picture."),
                                        helpText("You can also download the party data, but note the usage and citation requirements on the 'about' page."),
                                        downloadButton("downloadParty", "Download party shares")),
                           mainPanel(plotlyOutput("partyplot"))
             )
    ),
    tabPanel("Speakers",
             sidebarLayout(position = "left",
                           sidebarPanel(width = 3, # Out of 12
                                        helpText("This plot illustrates the prominence of your keywords across the individuals in the respective parliament."),
                                        helpText(HTML("It shows the <i>number of speeches that match your query as a share of all speeches an individual has given in the respective parliament</i> (note that speaking time varies a lot across speakers!), listing only the top-25 speakers along that measure.")),
                                        helpText("Hoover over the plot to customize it or to save the resulting picture."),
                                        helpText("You can also download the values for all speakers, but note the usage and citation requirements on the 'about' page."),
                                        downloadButton("downloadSpeaker", "Download speaker shares")),
                           mainPanel(plotlyOutput("speakerplot"))
             )
    ),
    tabPanel("About", 
             h3("Background"),
             a(img(src='OPTED_logo_transparent.png', style = "float:right; width: 150px; size: contain;", alt = "OPTED initiative"), href="https://opted.eu"),
             p(HTML("<p>This proto-type application has been developed in the context of the <a href=\"https://opted.eu\" target=\"_blank\">OPTED initiative</a>, aiming to facilitate access to systematic information from political texts.<p>
                           It has been developed in <a href=\"https://opted.eu/designing-an-infrastructure/wp5-parliamentary-government-and-legal-texts\" target=\"_blank\">Work Package 5</a>, which focusses on parliamentary, government and legal texts.<br>Johannes Gruber (<a href=\"https://opted.eu/team/wp7-pre-processing-storage-and-data-sharing/\" target=\"_blank\">WP7</a>) and Paul Baluff (<a href=\"https://opted.eu/team/wp3-journalistic-mass-mediated-political-texts/\" target=\"_blank\">WP3</a>) have provided valuable input regarding integration of this app with the AMCAT server backend.<p>
                           The project has received funding from the European Union’s Horizon 2020 research & innovation programme under <a href=\"https://cordis.europa.eu/project/id/951832\" target=\"_blank\">grant agreement No 951832</a>.<p>
                           For feedback and questions on this particular application, please contact to <a href=\"http://christian-rauh.eu\" target=\"_blank\">Christian Rauh</a>. <br> ")),
             p(HTML("<br>")),
             h3("How to cite"),
             p(HTML("When using any of the material generated here in your own work, please refer to this application and cite the underlying data sources.<br>
                           At the moment, the parliamentary text data offered here drawn on a pre-publication version of the <i><a href=\"http://htmlpreview.github.io/?https://github.com/ChRauh/ChRauh.github.io/blob/main/ParlLawSpeechTutorials.html\" target=\"_blank\">ParlLawSpeech</a></i> data collection.")),
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
                                tags$td(HTML("Sven-Oliver Proksch<br>Jan Schwalbach<br>Lukas Hetzer<br>Alexander Dalheimer"))))
             # p(a(img(src='WZB_Komb_portrait_Web_engl.png', style = "float:left; height: 100px; size: contain;", alt = "OPTED initiative"), href="https://opted.eu"),
             #   a(img(src='pti_logo.png', style = "float:center; height: 100px; size: contain;", alt = "OPTED initiative"), href="https://opted.eu"),
             #   a(img(src='CCCP_Logo.png', style = "float:rigth; height: 100px; size: contain;", alt = "OPTED initiative"), href="https://opted.eu"))
             
    )
  ) # navbarPage
) # fluidPage




# Define server logic  ----

server <- function(input, output) {
  
  # Database overview 
  # For plotting summaries and correct dat labelling
  parl.select <- data.frame(parliament = c("speeches_austria", "speeches_cz", "speeches_germany", "speeches_spain", "speeches_ep", "speeches_croatia"), # Equals the index name in AMCAT database
                            parl.name = c("Nationalrat (Austria)", "Poslanecká snemovna (Czech Republic)", "Bundestag (Germany)", "Congreso de los Diputados (Spain)", "European Parliament (EU)", "Hrvatski sabor (Croatia)"),
                            nspeeches = c(205110, 391306, 188230, 268329, 300168, 405273)) 
  
  # Load normalisation data
  monthly.speeches <- read_rds("./Data/monthly.speeches.rds")
  party.speeches <- read_rds("./Data/party.speeches.rds")
  speaker.speeches <- read_rds("./Data/speaker.speeches.rds")


  # Update user parliament choice upon submit button
  user.parliament <- eventReactive(input$submit, {
    input$parl
  })

  # Update user query upon submit button
  user.words <- eventReactive(input$submit, {
    input$words

  })
  
  
  # Time series data
  time.data <- reactive({
    
    if(input$submit == 0) {return(NULL)}
    
    showModal(modalDialog(title = "Just a few seconds.", "Extracting and aggregating data ...", footer=NULL))
    
    # Extract monthly count of query hits from AMCAT server
    hits <-    amcat4r::query_aggregate(index = user.parliament(),  # Define which index on host - user.parliament()
                                        queries = user.words(),      # Query
                                        axes = list(list(name="date",       # Axes for aggregation (note the correct nesting of lists)
                                                         field="date",
                                                         interval="month"))) %>% 
      rename(month = date_month) %>% 
      mutate(month = str_remove(as.character(month), "-[0-9]{2}$")) # AMCAT returns daily date (first) here 
    
    # Add to full range of monthly speeches 
    full <- monthly.speeches %>% 
      filter(index == user.parliament()) %>% # Only for user chosen parliament
      left_join(hits, by = "month")
    full$n[is.na(full$n)] <- 0 # True zeros - no hits in this month
    
    # Calculate share of speeches with query hits
    full <- full %>% 
      mutate(share = n/monthly.speeches)
    full$share[is.na(full$share)] <- 0 # True zeros - no hits in this month
    
    # Calculate moving averages and turn into long shape for plotting
    long <- full %>% 
      select(-c(monthly.speeches, index)) %>% 
      mutate(share.ma = stats::filter(share, rep(1,5), sides = 2)/5) %>% 
      pivot_longer(2:4) %>% 
      mutate(series = ifelse(name == "n", "Monthly Count", 
                             ifelse(name == "share", "Monthly share", "Moving average (5 months)"))) %>% 
      select(-name) %>% 
      arrange(month, series)
    
    # Remove waiting message
    removeModal()
    
    # Output
    return(long)
    
})

    
  # Time series plot
  output$timeplot <- renderPlotly({
    
    time.breaks <- unique(time.data()$month)
    time.breaks <- time.breaks[which(str_detect(time.breaks, "-01"))] # Only January 
    time.labels <- time.breaks %>% str_remove_all("-.*?$") # Label full years only
    
    
    time.gg <- ggplot(time.data() %>% filter(series != "Monthly Count"), 
                      aes(y = value, x = month, color = series, size = series, group = series))+
      geom_line()+
      scale_x_discrete(breaks = time.breaks, labels = time.labels)+
      scale_y_continuous(labels = scales::percent) +
      scale_color_manual(values = c("grey60", "#0063a6"), name = "Time series: ")+
      scale_size_manual(values = c(.5, 1.2), name = "Time series: ")+
      labs(title = "Your words in parliament over time",
           subtitle = paste("Parliament: ", parl.select$parl.name[parl.select$parliament == user.parliament()], ". Query: ", user.words()),
           x = "\nMonth",
           y = "Share of parliamentary speeches\nmatching the query\n",
           caption = "Based on the ParlLawSpeech data collection")+
      theme_bw()+
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, vjust = .5, hjust = -1))
    
    time.pl <- ggplotly(time.gg, tooltip = c("y", "x")) %>%
      layout(title = list(text = paste0('Your words in parliament over time',
                                        '<br>',
                                        '<sup>',
                                        paste0("Parliament: ", parl.select$parl.name[parl.select$parliament == user.parliament()], ". Query: \"", user.words()),'\"</sup>')),
             legend = list(orientation = "h", x = 0.3, y = -0.3))
    
    # Remove waiting message
    removeModal()
    
    # Output
    return(time.pl)
    
    
    
  })
  
  # Time series download
  output$downloadTime <- downloadHandler(
    filename = function() {
      paste('MyWordsInParliament-TimeSeries', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(time.data(), con, row.names = F)
    }
  )
  
  
  # Party data
  party.data <- reactive({
    
    if(input$submit == 0) {return(NULL)}
    
    showModal(modalDialog(title = "Just a few seconds.", "Extracting and aggregating data ...", footer=NULL))
    
    # AMCAT query
    hits <- amcat4r::query_aggregate(index = user.parliament(),  # Define which index on host - user.parliament()
                                     queries = user.words(),      # Query
                                     axes = list(list(name="party",       # Axes for aggregation (note the correct nesting of lists)
                                                      field="party"))) %>% 
         mutate(index = user.parliament())
    hits <- hits %>% 
      filter(!(party %in% c(NA, "-", "fraktionslos", "ohne Klubzugehörigkeit", "independent", "NA")))
    
    
    # # Add normalisation data and calculate share
    # hits <- hits %>%
    #   left_join(party.speeches, by = c("index", "party")) %>%
    #   mutate(share = n/party.speeches) %>%
    #   arrange(share)
    # hits$party2 <- factor(hits$party, levels = hits$party) # Plot ordering
    
    
    # Add normalisation data and calculate share
    # While ensuring that all parties in parliament are included (even if none of the speeches matched the query)
    hits2 <- party.speeches %>% filter(index == user.parliament()) %>% 
      left_join(hits, by = c("index", "party")) %>% 
      filter(!(party %in% c(NA, "-", "fraktionslos", "ohne Klubzugehörigkeit", "independent"))) %>% 
      mutate(n = ifelse(is.na(n), 0, n)) %>% # True zeros
      mutate(share = n/party.speeches) %>%
      arrange(share)
    hits2$party2 <- factor(hits2$party, levels = hits2$party) # Plot ordering
    
    
    # Remove waiting message
    removeModal()
    
    # Output
    return(hits2)
  })
  
  # Party plot
  output$partyplot <- renderPlotly({
    
    parties.gg <- ggplot(party.data(), aes(y = party2, x = share))+
      geom_col(width = .6, fill = "#0063a6") +
      geom_vline(xintercept = mean(party.data()$share, na.rm = T), linetype = "dashed")+
      scale_x_continuous(labels = scales::percent)+
      labs(title = "Keywords by party of speaker",
           subtitle = paste("Parliament: ", user.parliament(), ". Keywords: ", user.words(), sep = ""),
           x = "Share of partisan parliamentary speechesthat match the query\n(vertical line indicates average across parties)",
           y = "")+
      theme_bw()+
      theme(legend.position = "none",
            axis.text = element_text(color = "black"))
    
    party.pl <- ggplotly(parties.gg, tooltip = c("y", "x")) %>%
      layout(title = list(text = paste0('Your words in parliament across parties',
                                        '<br>',
                                        '<sup>',
                                        paste("Parliament: ", parl.select$parl.name[parl.select$parliament == user.parliament()], ". Query: ", user.words(), sep = ""),'</sup>')))
    
    # Output plot
    return(party.pl)
  })
  
  # Party download
  output$downloadParty <- downloadHandler(
    filename = function() {
      paste('MyWordsInParliament-PartyShares', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(party.data(), con, row.names = F)
    }
    
  )
  
  
  # Speaker data 
  speaker.data <- reactive({
    
    if(input$submit == 0) {return(NULL)}
    
    showModal(modalDialog(title = "Just a few seconds.", "Aggregating usage of your key words across speakers ...", footer=NULL))
    
    # AMCAT query
    hits <- amcat4r::query_aggregate(index = user.parliament(),  # Define which index on host - user.parliament()
                                     queries = user.words(),      # Query
                                     axes = list(list(name="speaker",       # Axes for aggregation (note the correct nesting of lists)
                                                      field="speaker"))) %>% 
      mutate(index = user.parliament())
    
    # Add normalisation data and calculate shares
    hits <- hits %>% 
      left_join(speaker.speeches, by = c("index", "speaker")) %>% 
      mutate(share = n/s.speeches) %>% 
      arrange(share)
    hits$speaker2 <- factor(hits$speaker, levels = hits$speaker) # Plot ordering
    
    # Remove waiting message
    removeModal()
    
    # Output
    return(hits)
    
  })
  
  # Speaker plot
  output$speakerplot <- renderPlotly({
    
    speaker.gg <- ggplot(head(speaker.data() %>% arrange(-share), 25), 
                         aes(y = speaker2, x = share))+
      geom_col(fill = "#0063a6", width = .6)+
      # geom_vline(xintercept = mean(speaker.data()$share), linetype = "solid", color = "red")+
      scale_x_continuous(expand = expansion(mult = c(0, 0.1)), labels = scales::percent)+
      labs(title = "Keyword usage by individual speakers (Top 25, in relative terms)",
           subtitle = paste("Parliament: ", user.parliament(), ". Keywords: ", paste(user.words(), collapse = ", "), sep = ""),
           x = "Percent of speeches by individual speakers\nthat match the query",
           y = "")+
      theme_bw()+
      theme(legend.position = "none",
            axis.text = element_text(color = "black"))
    
    speaker.pl <- ggplotly(speaker.gg, tooltip = c("y", "x")) %>%
      layout(title = list(text = paste0('Keyword usage by individual speakers (Top 25)',
                                        '<br>',
                                        '<sup>',
                                        paste("Parliament: ", parl.select$parl.name[parl.select$parliament == user.parliament()], ". Query: ", user.words(), sep = ""),'</sup>')))
    
    # Output
    return(speaker.pl)
    
  })
  
  # Speaker download
  output$downloadSpeaker <- downloadHandler(
    filename = function() {
      paste('MyWordsInParliament-Speakers-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(speaker.data(), con, row.names = F)
    }
  )
  
  # counter <- 0
  
  # Summary output for start page
  output$summary <- renderText({
    
    if(input$submit == 0) {return("Nothing submitted yet!")}
    
    # showModal(modalDialog(title = "Just a few seconds.", "Extracting and aggregating data ...", footer=NULL))
    
    # Would be nice, but is too slow ...
    # counts <- nrow(query_documents(index = user.parliament(),
    #                                queries = user.words(),
    #                                fields = ".id", 
    #                                per_page = 100,
    #                                scroll = "5m",
    #                                max_pages = Inf))
    
    ## "Speeches matching your query:\t\t", counts, "\n"
    
    summary <- paste0("Parliament:\t\t\t\t", parl.select$parl.name[parl.select$parliament == user.parliament()], "\n",
                      "Query:\t\t\t\t\t", user.words(), "\n",
                      "Database:\t\t\t\t", user.parliament(), "\n",
                      "Total number of speeches analysed:\t", parl.select$nspeeches[parl.select$parliament == user.parliament()], "\n")
    
    # removeModal() # End waiting dialogue here 
    
    return(summary)
    
  })
  
  
}




shinyApp(ui, server)