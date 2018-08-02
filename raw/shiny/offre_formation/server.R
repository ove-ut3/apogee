shinyServer(function(input, output) {
  
  library(magrittr)

  formations <- apogee::formations_liste() %>% 
    dplyr::select(-lib_etape_apogee) %>% 
    dplyr::mutate_all(as.factor)
  
  formations_filtrees <- reactive({
    
    if (!is.null(input$composante)){
      formations <- dplyr::filter(formations, lib_composante %in% input$composante)
    }
    
    if (!is.null(input$type_diplome)){
      formations <- dplyr::filter(formations, acronyme_type_diplome %in% input$type_diplome)
    }
    
    if (!is.null(input$mention)){
      formations <- dplyr::filter(formations, lib_mention %in% input$mention)
    }
    
    if (!is.null(input$annee_formation)){
      formations <- dplyr::filter(formations, annee_etap %in% input$annee_formation)
    }
    
    formations
  })
  
  output$composante <- renderUI({
    
    # if (is.null(input$type_diplome) & is.null(input$mention) & is.null(input$annee_formation)) {
    #   choix <- sort(unique(formations$lib_composante))
    # } else {
    #   choix <- sort(unique(formations_filtrees()$lib_composante))
    # }
    
    selectInput("composante", label = "Composante",
                choices = choix <- sort(unique(formations$lib_composante)), multiple = TRUE)
  })
  
  output$type_diplome <- renderUI({
    
    # browser()
    
    # if (is.null(input$composante) & is.null(input$mention) & is.null(input$annee_formation)) {
    #   choix <- sort(unique(formations$acronyme_type_diplome))
    # } else {
    #   choix <- sort(unique(formations_filtrees()$acronyme_type_diplome))
    # }
    
    selectInput("type_diplome", label = "Type diplôme",
                choices = sort(unique(formations$acronyme_type_diplome)), multiple = TRUE)
  })
  
  output$mention <- renderUI({
    selectInput("mention", label = "Mention",
                choices = sort(unique(formations$lib_mention), na.last = TRUE), multiple = TRUE)
  })
  
  output$annee_formation <- renderUI({
    selectInput("annee_formation", label = "Année de formation",
                choices = sort(unique(formations$annee_etape), na.last = TRUE), multiple = TRUE)
  })
  
  output$formations <- DT::renderDataTable({
    
    formations_filtrees() %>% 
      DT::datatable(selection = list(mode = 'single', selected = 1),
                    colnames = c('Composante' = 'lib_composante', 'Type diplôme' = 'acronyme_type_diplome', 'Année formation' = 'annee_etape', 'Code étape' = 'code_etape', 'Libellé' = 'lib_etape', 'Acronyme' = 'acronyme_etape', 'Mention' = 'lib_mention', 'Option' = 'option', 'Particularité' = 'particularite', 'Ville' = 'ville'),
                    rownames = FALSE,
                    filter = 'top',
                    extensions = 'Buttons', 
                    options = list(
                      pageLength = -1,
                      dom = 'Brt',
                      buttons = c('csv', 'excel'),
                      scrollY = '76vh',
                      autoWidth = TRUE,
                      columnDefs = list(list(width = '10%', targets = 0),
                                        list(width = '12%', targets = 1),
                                        list(width = '25%', targets = 4))
                    )
      )

  })
  
})
