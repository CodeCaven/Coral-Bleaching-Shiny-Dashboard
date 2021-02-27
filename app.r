# ------------------------------------------------------
# This shiny app displays coral bleaching data at 8 different sites
# for 5 different coral types.
# -------------------------------------------------------------------
library(shiny)
library(leaflet)
library(ggplot2)
library(shinythemes)

# get/set data
coral_data <- read.csv("./data/assignment-02-data-formated.csv")
colours <- c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d","#666666")

# convert value to numeric and remove %
coral_data$value <- as.numeric(gsub("%", "", coral_data$value))

# get site names and lats
sites <- levels(coral_data$location)
lats <- unique(coral_data$latitude)

# sort and reassign
names(lats) <- sites
lats <- sort(lats, decreasing=TRUE)
coral_data$location <- factor(coral_data$location, levels=names(lats))

# add colours column for display
coral_data$colour <- ""
for(i in 1:length(colours)){
    site <- paste("site0",i, sep="")
    coral_data$colour[coral_data$location == site] <- colours[i]
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = shinytheme("cosmo"),

  # for circles in legend
  tags$style(type = "text/css", 
      ".leaflet .legend i{
      border-radius: 50%;
      width: 10px;
      height: 10px;
      margin-top: 4px;
      }
    "),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(position="right",
  
    # Sidebar panel for inputs ----
    sidebarPanel(width=3,

      # Radio Type: Buttons to select coral type ----
      radioButtons("radioType", h3("Coral Type"),
                        choices = list("Blue Corals" = 1, "Hard Corals" = 2,
                                       "Sea Fans" = 3, "Sea Pens" = 4, "Soft Corals" = 5),selected = 1),
									   
	# Radio Smoothing: Buttons to select smoothing method ----
      radioButtons("radioSmooth", h3("Smoothing Method"),
                        choices = list("lm" = 1, "glm" = 2, "gam" = 3, "loess" = 4),selected = 1),
						
	helpText("The smoothing method is for the regression line shown on the plots")	

    ),

    # Main panel for displaying outputs ----
    mainPanel(width=9,
	
	  # App title ----
	  div(titlePanel("Coral Bleaching on the Great Barrier Reef"),align="center"),
	
	  # Header
	  h3(textOutput("header"), align="center"),
	  
	  # Output: Map
	  leafletOutput("mymap", height="360px"),
	  
	  # Output: Static ----
      plotOutput("plotArea", height="180px")

    )
  )
)
 
server <- function(input, output, session) {
	
	# REACTIVES
	getCoral <- reactive({
		c_type <- switch(input$radioType,
			   "1" = "blue corals",
			   "2" = "hard corals",
			   "3" = "sea fans",
			   "4" = "sea pens",
			   "5" = "soft corals")
	})
	
	getSmoother <- reactive({
		s_type <- switch(input$radioSmooth,
                   "1" = "lm",
                   "2" = "glm",
				   "3" = "gam",
				   "4" = "loess")
	
	})

	# OUTPUTS
	  output$header <- renderText({
			toupper(getCoral())
	  })
 
	 output$mymap <- renderLeaflet({ 
				 
			# filter data first to most recent and type
			filter_data <- coral_data[coral_data$coralType == getCoral() & coral_data$year == 2017,]
  
			# create df for map
			radii <- filter_data$value/5
			lats <- unique(filter_data$latitude)
			longs <- unique(filter_data$longitude)
			sites <- unique(filter_data$location)
			f_colours <- unique(filter_data$colour)
			map_df <- data.frame(cbind(sites,lats,longs,radii,f_colours))
			
			# undo Rs auto factoring
			map_df$lats <- as.numeric(as.character(lats))
			map_df$longs <- as.numeric(as.character(longs))
			map_df$radii <- as.numeric(as.character(radii))
			
			# create map
			leaflet(data = map_df) %>%
			addTiles() %>%
			addLegend(colors=f_colours, labels=sites, opacity = 1) %>%
			addCircleMarkers(~longs, ~lats, radius=radii, label=sites, color=f_colours, opacity=1,
							popup=paste("Most recent bleaching at <strong>", sites, "</strong></br> for <strong>", 
							getCoral(), "</strong> is <strong>", radii*5, "%</strong>")) 
								
	  })
	  
	  output$plotArea <- renderPlot({
					  
			# filter
			plot_data <- data.frame(coral_data[coral_data$coralType == getCoral(),])
			
			# aes and theme
			g <- ggplot(plot_data, aes(x=year, y=value)) + theme_bw()

			# plot details
			gg <- g + geom_line(alpha=0.7, linetype=2) + geom_point(size=0.3) + 
			geom_smooth(method=getSmoother(), size=0.5, col="black") + facet_grid(~plot_data$location) +
			labs(x="Year", y="Bleaching") +
			scale_y_continuous(labels=function(x){paste(x, "%")})

			# plot theme
			ggg <- gg + theme(plot.title=element_text(size=20, 									
												hjust=0.5,
												lineheight=1.2),  # title
						axis.title.x=element_blank(),  # X axis title
						axis.title.y=element_text(
												  size=12,
												 vjust=2),  # Y axis title
						axis.text.x=element_text(size=8,face="bold", 
												 angle = 30),  # X axis text
						axis.text.y=element_text(size=8,face="bold"))

			ggg
	  })
}
 
shinyApp(ui, server)