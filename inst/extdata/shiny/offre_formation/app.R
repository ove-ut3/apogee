library(shinydashboard)
library(shinyWidgets)
library(magrittr)

# https://groups.google.com/forum/#!topic/shiny-discuss/Q1ZvnjDCzUM

formations <- apogee::formations_liste() %>% 
  dplyr::select(-lib_etape_apogee) %>% 
  dplyr::mutate(annee_etape = stringr::str_c("Bac+", annee_etape)) %>% 
  tidyr::replace_na(list(lib_mention = "Pas de mention",
                         annee_etape = "Pas d'année"))

cols_name <- names(formations) %>% 
  dplyr::recode("lib_composante" = "Composante", "acronyme_type_diplome" = "Type diplôme", "lib_mention" = "Mention", "annee_etape" = "Année formation")

columnFilterUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("filter_container"))
}

columnFilter <- function(input, output, session, df, col_num, choice_filter, reset=F) {
  
  if (reset) {
    
    updateSelectInput(session, "filter_value",
                      choices = sort(unique(df()[,col_num,drop=TRUE])),
                      selected = NULL
    )
    
  } else {
  
    # This renders a selectInput and only re-renders when the selected data
    # frame changes. (i.e. it doesn't re-render when filters change state.)
    output$filter_container <- renderUI({
      # Don't render if col_num is > actual number of cols
      req(col_num <= ncol(df()))
      
      freezeReactiveValue(input, "filter_value")
      
      choices <- sort(unique(df()[,col_num,drop=TRUE]))
  
      # names(df())[[col_num]]
      pickerInput(session$ns("filter_value"), cols_name[col_num], multiple = T, 
                  choices = choices, selected = choices,
                  options = list(`actions-box` = T, `select-all-text` = "Tout sélectionner",
                                 `deselect-all-text` = "Tout désélectionner",
                                 `none-selected-text` = "Aucune sélection"))
  
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
    } else {
      df()[,col_num,drop=TRUE] %in% input$filter_value
    }
  })
}

columnFilterSetUI <- function(id, cols) {
  ns <- NS(id)
  
  list(
    actionButton(ns("clear_all_filters_button"), "Remise à zéro"),
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
    cat("clear", "\n")
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
  dashboardHeader(title = paste("Offre de formation")),
  dashboardSidebar(width = 250, sidebarMenu(
    menuItem("Filtres", tabName = "formations"),
    columnFilterSetUI("filterset", cols = c(1, 2, 7, 3))
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
  
  filtered_data <- callModule(columnFilterSet, "filterset", df = selected_data, cols = c(1, 2, 7, 3))
  
  output$table <- DT::renderDataTable({ 
    
    #browser()
    
    filtered_data() %>% 
      DT::datatable(selection = list(mode = 'single', selected = 1),
                    colnames = c('Composante' = 'lib_composante', 'Type diplôme' = 'acronyme_type_diplome', 'Année formation' = 'annee_etape', 'Code étape' = 'code_etape', 'Libellé' = 'lib_etape', 'Acronyme' = 'acronyme_etape', 'Mention' = 'lib_mention', 'Option' = 'option', 'Particularité' = 'particularite', 'Ville' = 'ville'),
                    rownames = FALSE,
                    extensions = 'Buttons',
                    options = list(
                      pageLength = -1,
                      dom = 'Brt',
                      buttons = c('csv', 'excel'),
                      scrollY = '81vh',
                      autoWidth = TRUE#,
                      # columnDefs = list(list(width = '10%', targets = 0),
                      #                   list(width = '12%', targets = 1),
                      #                   list(width = '25%', targets = 4))
                  )
    )
    
    })
}

shinyApp(ui, server)