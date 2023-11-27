
check_files <- function(reffile, surfile) {
	if (is.null(reffile)) return("empty reference file")
	if (is.null(surfile)) return("empty survey file")
	if (!file.exists(reffile)) return("reference file does not exist")
	if (!file.exists(surfile)) return("survey file does not exist")
	return("")
}

doshiny <- function(...) {

  #### dataset list ###
	datasetListUI <- function(id) {
		ns <- shiny::NS(id)
		shiny::uiOutput(ns('dataList'))
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
	options(shiny.maxRequestSize = 50 * 1024 ^ 2)
	box_color <- "blue"
	box_height <- '100%'
	input_class <- c("ui small icon input", "ui fluid icon input")

#######################################################################
############### UI ####################################################
#######################################################################


	ui <- semantic.dashboard::dashboardPage(
		title = "Variety Identification",
		semantic.dashboard::dashboardHeader(
			color = "green",
			menu_button_label = "",
			class = "ui top attached header",
			shiny.semantic::button(
				input_id = "close",
				label = shiny::span(shiny::icon("close"), "Exit"),
				class = c("tiny", "ui red button", "compact ui button")
			),
		),

		### Sidebar content ###
		semantic.dashboard::dashboardSidebar(
			size = "thin",
			color = "green",
			semantic.dashboard::sidebarMenu(
				semantic.dashboard::menuItem(
					text = shiny::span(shiny::icon("upload"), "Inputs"), tabName = "inputs_tab"),
				semantic.dashboard::menuItem(
					text = shiny::span(shiny::icon("map"), "Combine"), tabName = "ref_check"
				),
				semantic.dashboard::menuItem(
					text = shiny::span(shiny::icon("filter"), "Identification"), tabName = "ref_id"
				),
				semantic.dashboard::menuItem(
					text = shiny::span(shiny::icon("eye"), "Visualization"), tabName = "visualization"
				)
			)
		),

		## Body content
		semantic.dashboard::dashboardBody(
			shinyjs::useShinyjs(),
			shinyjs::extendShinyjs(text = "shinyjs.closeWindow = function() { window.close(); }",
										functions = c("closeWindow")),
			shinybusy::add_busy_spinner(spin = "fading-circle"),

			shinybusy::add_busy_gif(
			  src = "https://jeroen.github.io/images/banana.gif",
			  height = 70, width = 70
			),

############### INPUTS ################################################

			semantic.dashboard::tabItems(
				semantic.dashboard::tabItem(
					tabName = "inputs_tab",
					semantic.dashboard::box(
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

				semantic.dashboard::tabItem(
					tabName = "ref_check",
					semantic.dashboard::box(
						title = "Combine input",
						width = 16,
						color = box_color,
						collapsible = FALSE,
						title_side = "top left",
						shiny.semantic::button(
							input_id = "run_ref_check",
							label = shiny::span(shiny::icon("play"), "RUN"),
							class = "ui green button"
						)
					),
					shiny::fluidRow(
						id = "reference",
						shiny::column(3,
							shiny::verbatimTextOutput('rf_read'),
							shiny::verbatimTextOutput('rf_combine'),
							shiny::verbatimTextOutput('rf_recode')
						)
					)
				),

############### REFERENCE IDENTIFICATION ##############################

				semantic.dashboard::tabItem(
					tabName = "ref_id",
					semantic.dashboard::box(
						title = "Identify varieties",
						width = 16,
						color = box_color,
						collapsible = FALSE,
						title_side = "top left",
						shiny::br(),
						shiny.semantic::button(
							input_id = "run_id",
							label = shiny::span(shiny::icon("play"), "RUN"),
							class = "ui green button"
						)
					),
					shiny::fluidRow(	
						shiny::column(2,
							shiny::verbatimTextOutput('rf_id'),
							shiny::downloadButton('download',"Save to .csv"),
						)
					),
					DT::dataTableOutput("res.ID"),
						style = "height:800px; overflow-y: scroll;overflow-x: scroll;"
				),

############### VISUALIZATION #########################################

				semantic.dashboard::tabItem(
					tabName = "visualization",
					semantic.dashboard::box(
						title = "Map",
						width = 16,
						color = box_color,
						collapsible = FALSE,
						title_side = "top left",
						datasetListUI(id = "dataList"),
							shiny.semantic::button(
								input_id = "run_pca",
								label = shiny::span(shiny::icon("play"), "RUN"),
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

		# Close button
		observeEvent(input$close, {
		  lapply(names(resourcePaths()), removeResourcePath)
		  js$closeWindow()
		  stopApp()
		})

############### REFERENCE CHECK #######################################

		observeEvent(input$run_ref_check, {

			reffile = input$reference_file$datapath
			surfile = input$survey_file$datapath
			check = check_files(reffile, surfile)
			if (check != "") {
				output$rf_read <- renderText({check})
				return(NULL)
			}

			ref <- try(data.table::fread(reffile))
			if (inherits(ref, "try-error")) stop("cannot read reference file")
			fld <- try(data.table::fread(surfile))
			if (inherits(fld, "try-error")) stop("cannot read reference file")

			refname = input$reference_file$name
			surname = input$survey_file$name		
			output$rf_read <- renderText({
				paste0("reference: ", refname, ", ", nrow(ref), " records\n", 
					   "survey   : ", surname, ", ", nrow(fld), " records\n\n")
			})		
			crf <- varfinder::combine_rf(ref, fld)
			output$rf_combine <- renderText({
				paste0("combined : ", nrow(crf), " records\n\n")
			})		
			recoded <<- varfinder::recode_rf(crf, biallelic=TRUE, missflags="-")
			output$rf_recode <- renderText({
				paste0("recoded successfully")
			})		
		})
############### REFERENCE IDENTIFICATION ##############################

		observeEvent(input$run_id, {

			if (!exists("recoded")) {
				output$rf_id <- renderText({
					"input has not been generated (go back one tab)"
				})
			
			} else {
				out <- try(varfinder::match_rf(recoded, MAF_cutoff=0.05, SNP_mr=0.2, sample_mr=0.2, IBS_cutoff=.8))

				output$rf_id <- renderText({
					paste0(nrow(out$IBS_cutoff_0.8_best_match))
				})		

				DT::renderDataTable({datatable(out$IBS_cutoff_0.8_best_match)})

				output$download <- downloadHandler(
					filename = function() {"match_results.csv"},
					content = function(fname) {
						write.csv(out$IBS_cutoff_0.8_best_match, fname)
					}
				)
			}
		})


    ############### VISUALIZATION #########################################
		observeEvent(input$run_pca, {
			print("run_pca")		
		})
	} # server end
	
	shiny::shinyApp(ui, server, ...)
}

doshiny()

