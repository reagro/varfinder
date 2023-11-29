
check_files <- function(reffile, surfile, crdfile) {
	if (is.null(reffile)) return("empty reference file")
	if (is.null(surfile)) return("empty survey file")
	if (!file.exists(reffile)) return("reference file does not exist")
	if (!file.exists(surfile)) return("survey file does not exist")
	if ((!is.null(crdfile)) && (!file.exists(crdfile))) return("coordinates file does not exist")

	return("")
}

shiny_rf <- function(...) {

	# globals
	recoded <- crd <- js <- NULL

  #### dataset list ###
	datasetListUI <- function(id) {
		ns <- shiny::NS(id)
		shiny::uiOutput(ns('dataList'))
	}
	datasetListServer <- function(id, md_list) {
		shiny::moduleServer(id,
			function(input, output, session) {
				output$dataList = shiny::renderUI({
					shiny::selectizeInput(
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
							inputId = "coords_file",
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
						title = "Varietal Identification",
						width = 16,
						color = box_color,
						collapsible = FALSE,
						title_side = "top left",
						shiny::fluidRow(
							shiny::column(3,
								shiny::numericInput(
									inputId = "ibs",
									label = "IBS cut-off",
									value = .8,
									min = 0.1,
									max = 0.9
								),
								shiny::br(),
								shiny.semantic::button(
									input_id = "run_id",
									label = shiny::span(shiny::icon("play"), "RUN"),
									class = "ui green button"
								),
								shiny::br(),
								shiny::downloadButton('download',"Save to .csv"),
								shiny::br()
							)
						),
						DT::dataTableOutput("res.ID"),
						style = "height:800px; overflow-y: scroll;overflow-x: scroll;"
					)
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
						shiny.semantic::flow_layout(
							shiny.semantic::button(
								input_id = "make_map",
								label = shiny::span(shiny::icon("play"), "RUN"),
								class = "ui green button"
							),
							shiny::selectInput("variety", "variety:", c("none" = "none"))
						),
						shiny::p(),
						leaflet::leafletOutput("mymap"),
						shiny::p()
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
		ID_res <- shiny::reactiveVal(NULL)

		# Close button
		shiny::observeEvent(input$close, {
		  lapply(names(shiny::resourcePaths()), shiny::removeResourcePath)
		  js$closeWindow()
		  shiny::stopApp()
		})

############### REFERENCE CHECK #######################################

		shiny::observeEvent(input$run_ref_check, {

			reffile = input$reference_file$datapath
			surfile = input$survey_file$datapath
			crdfile = input$coords_file$datapath
			check = check_files(reffile, surfile, crdfile)
			if (check != "") {
				output$rf_read <- shiny::renderText({check})
				return(NULL)
			}

			ref <- try(data.table::fread(reffile))
			if (inherits(ref, "try-error")) stop("cannot read reference file")
			fld <- try(data.table::fread(surfile))
			if (inherits(fld, "try-error")) stop("cannot read reference file")
			if (!is.null(crdfile)) {
				crd <<- try(stats::na.omit(data.frame(data.table::fread(crdfile))))
			} else {
				crd <<- NULL
			}

			refname = input$reference_file$name
			surname = input$survey_file$name		
			crdname = input$coords_file$name
			pcrd = ifelse(is.null(crdname), "not provided", crdname)
			output$rf_read <- shiny::renderText({
				paste0("reference  : ", refname, ", ", nrow(ref), " records\n", 
					   "survey     : ", surname, ", ", nrow(fld), " records\n",
					   "coordinates: ", pcrd, ", ", max(0, nrow(crd)), " records\n\n")
			})		
			crf <- varfinder::combine_rf(ref, fld)
			output$rf_combine <- shiny::renderText({
				paste0("combined : ", nrow(crf), " records\n\n")
			})		
			recoded <<- varfinder::recode_rf(crf, biallelic=TRUE, missflags="-")
			output$rf_recode <- shiny::renderText({
				paste0("recoded successfully")
			})		

		})
############### REFERENCE IDENTIFICATION ##############################

		shiny::observeEvent(input$run_id, {

			if (!exists("recoded")) {
				output$rf_id <- shiny::renderText({
					"input has not been generated (go back one tab)"
				})
			
			} else {
				
				IBS_cutoff = input$ibs
				ibsvar <- paste0("IBS_cutoff_", IBS_cutoff, "_best_match")
				
				res_ID <<- try(varfinder::match_rf(recoded, 
					MAF_cutoff=0.05, SNP_mr=0.2, sample_mr=0.2, 
					IBS_cutoff=IBS_cutoff))$IBS_cutoff_0.8_best_match
					
				ID_res(res_ID)

				res_ID$IBS <- round(res_ID$IBS, 3)
				res_ID$Sample_SNP_mr <- round(res_ID$Sample_SNP_mr, 3)
				
				output$res.ID <- DT::renderDataTable({DT::datatable(res_ID)})

				output$download <- shiny::downloadHandler(
					filename = function() {"match_results.csv"},
					content = function(fname) {
						utils::write.csv(res_ID$IBS_cutoff_0.8_best_match, fname)
					}
				)
				
			}
		})


    ############### VISUALIZATION #########################################
		
		output$mymap <- leaflet::renderLeaflet({
			leaflet::leaflet() |> leaflet::setView(0, 0, zoom = 3) |>
			  leaflet::addProviderTiles("OpenStreetMap",
				options = leaflet::providerTileOptions(noWrap = TRUE)
			  ) # |> addMarkers(data = points())
		})

		drawPoints <- function(variety) {
			if (!exists("crd")) return(NULL)
			points <- crd[ , c("lon", "lat")]
			if (variety == "all varieties") {
				leaflet::leafletProxy("mymap", data = points) |>
					leaflet::clearShapes() |>
					leaflet::addCircles(radius = 15, weight = 10, color = "red")				
			} else {
				leaflet::leafletProxy("mymap", data = points) |>
					leaflet::clearShapes() |>
					leaflet::addCircles(radius = 10, weight = 6, color = "gray")				
				points <- crd[crd[,1] == variety, c("lon", "lat")]
				leaflet::leafletProxy("mymap", data = points) |>
					leaflet::addCircles(radius = 15, weight = 10, color = "red")	
			}
		}
		
		shiny::observeEvent(input$make_map, {
			uvars <- c("all varieties", unique(crd[,1]))
			shiny::updateSelectInput(session, "variety",
				label = "variety",
				choices = uvars,
				selected = uvars[1]
			)
			drawPoints(input$variety)
		})

		shiny::observeEvent(input$variety, {
			drawPoints(input$variety)
		})

	} # server end
	
	shiny::shinyApp(ui, server, ...)
}


