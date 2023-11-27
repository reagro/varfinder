
library(semantic.dashboard)
library(htmltools)
library(shiny)
library(shinyjs)
library(shiny.semantic)  # button
#library(plotly)
#library(shinyWidgets)

doshiny <- function(...) {

  #### dataset list ###
	datasetListUI <- function(id) {
		ns <- NS(id)
		uiOutput(ns('dataList'))
	}

	datasetListServer <- function(id, md_list) {
		moduleServer(id,
			function(input, output, session) {
				output$dataList = renderUI({
					selectizeInput(
						inputId = 'dataList',
						label = 'Select a sample',
						choices = md_list,
						options = list(placeholder = '')
					)
				})
			})
	}

	# maximum size of file to upload
	options(shiny.maxRequestSize = 30 * 1024 ^ 2)

	box_color <- "blue"
	box_height <- '100%'
	input_class <- c("ui small icon input", "ui fluid icon input")

#######################################################################
############### UI ####################################################
#######################################################################

	ui <- dashboardPage(
		title = "Varietal Identification",
		dashboardHeader(
			color = "green",
			menu_button_label = "",
			class = "ui top attached header",
			button(
				input_id = "close",
				label = span(icon("close"), "Exit"),
				class = c("tiny", "ui red button", "compact ui button")
			),
		),

		### Sidebar content ###
		dashboardSidebar(
			size = "thin",
			color = "green",
			sidebarMenu(
				menuItem(
					text = span(icon("upload"), "Inputs"), tabName = "inputs_tab"),
				menuItem(
					text = span(icon("map"), "Combine"), tabName = "ref_check"
				),
				menuItem(
					text = span(icon("filter"), "Identification"),
					tabName = "ref_id"
				),
				menuItem(
					text = span(icon("eye"), "Visualisation"),
					tabName = "visualization"
				)
			)
		),

		## Body content
		dashboardBody(
			useShinyjs(),
			extendShinyjs(text = "shinyjs.closeWindow = function() { window.close(); }",
										functions = c("closeWindow")),

############### INPUTS ################################################

			tabItems(
				tabItem(
					tabName = "inputs_tab",
					box(
						title =	"Input files",
						color = box_color,
						width = 16,
						collapsible = FALSE,
						title_side = "top left",
						style = box_height,
						shiny.semantic::fileInput(
							inputId = "reference_file",
							label = "Reference file",
							buttonLabel = "Browse",
							type = input_class,
							placeholder = "reference.csv",
							accept = "csv"
						),
						shiny.semantic::fileInput(
							inputId = "survey_file",
							label = "Survey file",
							buttonLabel = "Browse",
							type = input_class,
							placeholder = "survey.csv",
							accept = "csv"
						),
						shiny.semantic::fileInput(
							inputId = "coordinates_file",
							label = "Coordinates file",
							buttonLabel = "Browse",
							type = input_class,
							placeholder = "coordinates.csv",
							accept = "csv"
						),
					)
				),

############### REFERENCE CHECK #######################################

				tabItem(
					tabName = "ref_check",
					box(
						title = "Combine input",
						width = 16,
						color = box_color,
						collapsible = FALSE,
						title_side = "top left",
						button(
							input_id = "run_check",
							label = span(icon("play"), "RUN"),
							class = "ui green button"
						)
					),
					fluidRow(
						id = "referenceValidationBody",
						title = NULL,
						hidden = FALSE,
						fluidRow(h3("Hello!"))
					)
				),

############### REFERENCE IDENTIFICATION ##############################

				tabItem(
					tabName = "ref_id",
					box(
						title = "Identify varieties",
						width = 16,
						color = box_color,
						collapsible = FALSE,
						title_side = "top left",
						br(),
						button(
							input_id = "run_id",
							label = span(icon("play"), "RUN"),
							class = "ui green button"
						),
						br(),
						
						fluidRow(	
							column(1,
								br(),
								downloadButton('download',"Save results table to csv file"),
								br()
							)
						),
						DT::dataTableOutput("res.ID"),
						style = "height:800px; overflow-y: scroll;overflow-x: scroll;"
					)
				),

############### VISUALIZATION #########################################

				tabItem(
					tabName = "visualization",
					box(
						title = "Map",
						width = 16,
						color = box_color,
						collapsible = FALSE,
						title_side = "top left",
						datasetListUI(id = "dataList"),
							button(
								input_id = "run_pca",
								label = span(icon("play"), "RUN"),
								class = "ui green button"
							),
#						plotlyOutput("plot_pca",height="800px")
					)
				)
			)
		)
	)
	
#######################################################################
############### SERVER ################################################
#######################################################################

	server <- function(input, output, session) {

		options(shiny.maxRequestSize=50*1024^2)
		
		hammingCoReactive <- reactiveVal(NULL)
		ID_res <- reactiveVal(NULL)

		### Close button ###
		observeEvent(input$close, {
		  lapply(names(resourcePaths()), removeResourcePath)
		  js$closeWindow()
		  stopApp()
		})

############### REFERENCE CHECK #######################################

		observeEvent(input$run_check, {
			print("run_check")		
			showModal(modalDialog(
				title = "run_check",
				"This is an important message!"
			))
		
		})
############### REFERENCE IDENTIFICATION ##############################

		observeEvent(input$run_id, {
			print("run_id")		
			showModal(modalDialog(
				title = "run_id",
				"This is an important message!"
			))

		})

		output$download <- downloadHandler(
			filename = function(){"match_results.csv"},
			content = function(fname) {
				write.csv(res_ID$res_summary, fname)
			}
		)

    ############### VISUALIZATION #########################################
		observeEvent(input$run_pca, {
			print("run_pca")		
		})
	} # server end
	
	shinyApp(ui, server, ...)
}

doshiny()

