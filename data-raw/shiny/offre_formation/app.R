library(shinydashboard)
library(shinyWidgets)
library(magrittr)

# https://groups.google.com/forum/#!topic/shiny-discuss/Q1ZvnjDCzUM

annee <- 2017

formations <- apogee::formations_liste(annee) %>% 
  dplyr::mutate(lib_etape = apogee::lib_etape(code_etape, type_diplome = FALSE, annee = FALSE),
                annee_etape = stringr::str_c("Bac+", annee_etape)) %>% 
  tidyr::replace_na(list(lib_mention = "Pas de mention",
                         lib_domaine = "Pas de domaine",
                         annee_etape = "Pas d'année")) %>% 
  dplyr::select(lib_composante, acronyme_type_diplome, annee_etape, code_etape, lib_etape, acronyme_etape, lib_mention, lib_domaine)

cols_name <- names(formations) %>% 
  dplyr::recode("lib_composante" = "Composante", "acronyme_type_diplome" = "Type diplôme", "lib_mention" = "Mention", "annee_etape" = "Année formation", "lib_etape" = "Libellé formation", "lib_domaine" = "Domaine")

columnFilterUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("filter_container"))
}

columnFilter <- function(input, output, session, df, col_num, choice_filter, reset=F) {
  
  if (reset) {
    
    if (cols_name[col_num] == "Libellé formation") {
      
      updateTextInput(session, "filter_value", value = "", placeholder = "Entrez du texte...")
      
    } else {
      
      updateSelectInput(session, "filter_value",
                        choices = sort(unique(df()[,col_num,drop=TRUE])),
                        selected = NULL)
      
    }
    
  } else {
  
    # This renders a selectInput and only re-renders when the selected data
    # frame changes. (i.e. it doesn't re-render when filters change state.)
    output$filter_container <- renderUI({
      # Don't render if col_num is > actual number of cols
      req(col_num <= ncol(df()))
      
      freezeReactiveValue(input, "filter_value")
  
      if (cols_name[col_num] == "Libellé formation") {
        
        textInput(session$ns("filter_value"), label = "Libellé formation", placeholder = "Entrez du texte...")
        
      } else {
        
        choices <- sort(unique(df()[,col_num,drop=TRUE]))
        
        # names(df())[[col_num]]
        pickerInput(session$ns("filter_value"), cols_name[col_num], multiple = T, 
                    choices = choices, selected = choices,
                    options = list(`actions-box` = T, `select-all-text` = "Tout sélectionner",
                                   `deselect-all-text` = "Tout désélectionner",
                                   `none-selected-text` = "Aucune sélection"))
        
      }
  
    })
    
    # When the other filters change, update this filter to remove rows that
    # are filtered out by the other filters' criteria. (We also add in the
    # currently selected values for this filter, so that changing other
    # filters does not cause this filter's selected values to be unselected;
    # while that behavior might make sense logically, it's a poor user
    # experience.)
    observeEvent(choice_filter(), {
      
      current_values <- input$filter_value
      
      updatePickerInput(session, "filter_value",
                        choices = sort(unique(c(current_values, df()[choice_filter(),col_num,drop=TRUE]))),
                        selected = current_values
      )
    })
  
  }
  
  # Return a reactive that is a row index of selected rows, according to
  # just this filter. If this filter shouldn't be taken into account
  # because its col_num is too high, or if there are no values selected,
  # just return TRUE to accept all rows.
  reactive({
    if (col_num > ncol(df())) {
      TRUE
    } else if (!isTruthy(input$filter_value)) {
      TRUE
    } else if (cols_name[col_num] == "Libellé formation") {
      stringr::str_detect(df()[,col_num,drop=TRUE], stringr::fixed(input$filter_value, ignore_case = TRUE))
    } else {
      # filter_value <- input$filter_value
      # if (!filter_value %in% df()[,col_num,drop=TRUE]) {
      #   filter_value <- iconv(filter_value, from = "UTF-8")
      #   Encoding(filter_value) <- "latin1"
      #   filter_value <- iconv(filter_value, from = "UTF-8")
      # }
      # df()[,col_num,drop=TRUE] %in% filter_value
      
      df()[,col_num,drop=TRUE] %in% input$filter_value
    }
  })
}

columnFilterSetUI <- function(id, cols) {
  ns <- NS(id)
  
  list(
    actionButton(ns("clear_all_filters_button"), "Remise à zéro des filtres"),
    lapply(cols, function(i) {
        columnFilterUI(ns(paste0("col", i)))
    })
  )
  
}

columnFilterSet <- function(input, output, session, df, cols) {
  
  # Each column filter needs to only display the choices that are
  # permitted after all the OTHER filters have had their say. But
  # each column filter must not take its own filter into account
  # (hence we do filter[-col], not filter, in the reactive below).
  create_choice_filter <- function(col) {
    reactive({
      filter_values <- lapply(filters[-col], do.call, args = list())
      Reduce(`&`, filter_values, TRUE)
    })
  }
  
  observeEvent(input$clear_all_filters_button, {
    filters <- lapply(cols, function(i) {
      callModule(columnFilter, paste0("col", i), df= df, col_num =i, create_choice_filter(i), reset=T)
    })
  })

  # filters is a list of reactive expressions, each of which is a
  # logical vector of rows to be selected.
  filters <- lapply(cols, function(i) {
    callModule(columnFilter, paste0("col", i), df, i, create_choice_filter(i))
  })
  
  reactive({
    # Unpack the list of reactive expressions to a list of logical vectors
    filter_values <- lapply(filters, do.call, args = list())
    # Combine all the logical vectors using & operator
    selected_rows <- Reduce(`&`, filter_values, TRUE)
    # Return the data frame, filtered by the selected rows
    df()[selected_rows,]
  })
}

ui <- dashboardPage(
  # tags$script("Shiny.addCustomMessageHandler('launch-modal', function(d) {$('#' + d).modal().focus();})"),
  # tags$script("Shiny.addCustomMessageHandler('remove-modal', function(d) {$('#' + d).modal('hide');})"),
  # 
  # tags$div(
  #   id = "chargement",
  #   class="modal fade", tabindex="-1", `data-backdrop`="static", `data-keyboard`="false",
  #   tags$div(
  #     class="modal-dialog",
  #     tags$div(
  #       class = "modal-content",
  #       tags$div(class="modal-header", tags$h4(class="modal-title", "Chargement des données"))
  #     )
  #   )
  # ),
  dashboardHeader(title = paste("Offre de formation", apogee::annee_u(annee)), titleWidth = 270),
  dashboardSidebar(width = 270, sidebarMenu(
    menuItem(h4("Filtres"), tabName = "formations", startExpanded = TRUE),
    column(12, h5(textOutput("n_formations")), offset = 0.5),
    columnFilterSetUI("filterset", cols = c(1, 2, 7, 8, 3, 5)),
    column(12, h4("")),
    menuItem(h4("Téléchargment"), tabName = "export", startExpanded = TRUE),
    column(4, downloadButton("csv", "CSV")),
    column(4, downloadButton("excel", "Excel"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "formations",
              fixedRow(
                column(width = 12,
                       fluidRow(
                         DT::dataTableOutput("table")
                       )
                )
                
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  selected_data <- reactive({
    formations
  })
  
  filtered_data <- callModule(columnFilterSet, "filterset", df = selected_data, cols = c(1, 2, 7, 8, 3, 5))

  output$n_formations <- reactive({
    if (nrow(formations) == nrow(filtered_data())) {
      paste(nrow(formations), "formations au total")
    } else {
      paste(nrow(filtered_data()), "formations filtrées")
    }
  })
  
  #session$sendCustomMessage(type = 'launch-modal', "chargement") # launch the modal
  
  output$table <- DT::renderDataTable({
    
    filtered_data() %>% 
      DT::datatable(selection = list(mode = 'single', selected = 1),
                    colnames = c('Composante' = 'lib_composante', 'Type diplôme' = 'acronyme_type_diplome', 'Année formation' = 'annee_etape', 'Code étape' = 'code_etape', 'Libellé formation' = 'lib_etape', 'Acronyme' = 'acronyme_etape', 'Mention' = 'lib_mention', 'Domaine' = 'lib_domaine'), #'Option' = 'option', 'Particularité' = 'particularite', 'Ville' = 'ville'
                    rownames = FALSE,
                    extensions = 'Buttons',
                    options = list(
                      pageLength = -1,
                      dom = 'rt',
                      #buttons = c('csv', 'excel'),
                      scrollY = '85vh',
                      autoWidth = TRUE#,
                      # columnDefs = list(list(width = '10%', targets = 0),
                      #                   list(width = '12%', targets = 1),
                      #                   list(width = '25%', targets = 4))
                  )
    )
    
    })
  
  #session$sendCustomMessage(type = 'remove-modal', "chargement") # hide the modal programmatically
  
  output$csv <- downloadHandler(
    filename = function() {
      "formations.csv"
    },
    content = function(con) {
      write.csv2(filtered_data(), con, row.names = FALSE, na = "")
    }
  )
  
  output$excel <- downloadHandler(
    filename = function() {
      "formations.xlsx"
    },
    content = function(con) {
      writexl::write_xlsx(filtered_data(), con)
    }
  )
  
}

shinyApp(ui, server)