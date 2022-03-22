#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(hrbrthemes)
library(plotly)


# Define UI for application
ui <- fluidPage(
  
  # Application title
  h2("Heatmap showing the number of evidence gap related to antimicrobial resistance, 2010-2021"),
  br(),
  br(),
  
  # Sidebar
  sidebarPanel(width = 3,
               selectInput(inputId = "geoscope", label = h6("Geoscope"), 
                           choices = list("All", "Global", "Regional", "--Asia", "--Europe", "--Latin America", "--North America", "--Sub-saharan Africa", "National", "Subnational/local"), 
                           selected = "All"),
               
               selectInput(inputId = "income", label = h6("Country income level"), 
                           choices = list("All", "General", "LIC", "MIC", "LMIC", "HIC"), 
                           selected = "All"),
               
               submitButton("Submit"))
  ,
  
  # Show a plot of the generated distribution
  mainPanel(
    plotlyOutput("hmplot", height = "200%")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Cleaning dataset
  dat <- read_csv("https://raw.githubusercontent.com/gilbertlzrus/AMR-ScR/main/data_EGM.csv")
  
  dat$Domain <- factor(dat$Domain, levels = rev(c("Measuring the burden", "Understanding risk factors", "Measuring prevalence of exposure to risk factors", "Evaluating efficacy and effectiveness of interventions in place", "Measuring prevalence of coverage of interventions in place", "Health policy analysis", "Health system structure analysis", "Financing/costs analysis", "Human resources", "Provision/infrastructure", "Operations research", "Responsiveness/recipients", "More affordable", "More feasible/deliverable", "More acceptable", "More effective", "More sustainable", "Clinical decision support system", "Diagnostics", "Digital health technology", "Medicines", "Vaccines", "Other preventive tools", "Others")))
  dat$Theme <- factor(dat$Theme, levels = c("Prevention - CAI", "Prevention - HAI", "Immunization", "IPC", "WASH", "Other", "Prevention - General", "Diagnosis", "Care & Treatment - CAI", "Care & Treatment - HAI", "Care & Treatment - General"))
  dat$Geoscope <- factor(dat$Geoscope, levels = c("All", "Global", "Regional", "--Asia", "--Europe", "--Latin America", "--North America", "--Sub-saharan Africa", "National", "Subnational/local"))	
  dat$Income <- factor(dat$Income, levels = c("All", "General", "LIC", "MIC", "LMIC", "HIC"))
  dat$Group.Domain <- factor(dat$Group.Domain, levels = c("Descriptive", "Delivery", "Development", "Discovery"))
  dat$Group.Theme <- factor(dat$Group.Theme, levels = c("Prevention", "Diagnosis", "Care & Treatment"))  
  
  output$hmplot <- renderPlotly({
    geoscope <- dat[, 5]
    income <- dat[, 6]
    
    sub_dat <- filter(dat, Geoscope == input$geoscope, Income == input$income)
    
    # draw the histogram with the specified number of bins
    
    hm.plotly <- ggplot(sub_dat, 
                        aes(x = Theme, y = Domain, fill= n)) + 
      geom_tile() +
      scale_fill_gradient(low="white", high="darkblue", name = "") +
      scale_y_discrete(position = "right") +
      scale_x_discrete(labels = c(
        "Prevention - CAI" = "CAI", "Prevention - HAI" = "HAI", "Immunization" = "Immunization", "IPC" = "IPC", "WASH" = "IPC", "Other" = "Other", "Prevention - General" = "General", "Diagnosis" = "Diagnosis", "Care & Treatment - CAI" = "CAI", "Care & Treatment - HAI" = "HAI", "Care & Treatment - General" = "General")) +
      facet_grid(Group.Domain ~ Group.Theme,
                 scales = "free", space = "free_x", switch = "x") +
      theme_bw() +
      theme(axis.ticks = element_blank(),
            legend.position = "left",
            strip.placement = "outside", 
            strip.background = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            legend.key.height = unit(6, "lines"),
            legend.title = element_blank(),
            panel.spacing = unit(0, "lines"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    ggplotly(hm.plotly) %>%
      layout(height = 500, width = 950)
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
