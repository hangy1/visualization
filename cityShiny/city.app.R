

library(shiny)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(sf)
library(dplyr)
library(tidyverse)
library(reshape2)


bos <- st_read("BOSnbhds/Bos_neighborhoods_new.shp")

df <- read_csv("neighborhood-childcare-affordabilty-analysis.csv")
str(bos)
df <- df[-c(2,3,4,19)] %>%
    na.omit() 

#combining neighborhood 

bos_new <- bos %>% 
    select(Name, geometry) %>%
        add_row(Name ="Allston/Brighton", geometry = st_combine(bos[bos$Name==c("Allston","Brighton"),]) ) %>% 
add_row(Name ="Back Bay/Beacon Hill", geometry = st_combine(bos[bos$Name==c("Back Bay","Beacon Hill"),]) ) %>%
add_row(Name ="Fenway/Kenmore", geometry = st_combine(bos[bos$Name==c("Fenway", "Longwood Medical Area"),]) )  %>%
add_row(Name ="Central Boston", geometry = st_combine(bos[c(5,6,7,8,14,16),])) %>%
add_row(Name ="Roxbury", geometry = st_combine(bos[bos$Name ==c("Roxbury","Mission Hill"),])) %>%
add_row(Name ="South Boston", geometry = st_combine(bos[c(23,24),]))   
bos_new <- bos_new[-c(9,23,24),]
colnames(bos_new) <- c("Neighborhoods", "geometry")
#join two table
merge <- merge(df,bos_new)[-16,]

str(merge)

merge$MedianIncome <- as.numeric(gsub('[$,]','',merge$MedianIncome))

merge[,3:10] <- lapply(merge[,3:10],FUN = function(x)as.numeric(sub("%", "", x))/100)
merge[12:15] <- lapply(merge[,12:15],FUN = function(x)as.numeric(sub("%", "", x))/100)

#merge$geometry <- st_geometry(merge$geometry)

merge$Neighborhoods[2] <- "Back Bay"
merge$Neighborhoods[7] <- "Fenway/K"
merge$Neighborhoods[3] <- "C. Bos"

merge$careInc <- (merge$InfCostCareMedInc+merge$PreCostCareMedInc)/2
merge$Unaffordable <- (merge$InfCarAfford+ merge$PreCareAfford)/2

#normalize income and affordability 
normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
}

merge.n <- merge
merge.n[,3:15] <- lapply(X = merge.n[,3:15],normalize)
merge.n[,17:18] <- lapply(X = merge.n[,17:18],normalize)
col <- brewer.pal(8, "Oranges")


Median.plot <- ggplotly( 
    ggplot(merge,aes(geometry = geometry, fill= MedianIncome)) + 
        geom_sf() + 
        scale_fill_gradientn(colors = col) +
        geom_sf_text(aes(label = Neighborhoods, geometry = geometry), 
                     fun.geometry= st_centroid, size = 1.8) + 
        ggtitle("Median of Household Income for each neighborhood") +
        theme_void()
)

cost_med.plot <- ggplotly( 
    ggplot(merge,aes(geometry = geometry, fill = careInc)) + 
        geom_sf() + 
        scale_fill_gradientn(colors = col) +
        geom_sf_text(aes(label = Neighborhoods, geometry = geometry), 
                     fun.geometry= st_centroid, size = 1.8) +
        ggtitle("Average cost of care as a percentage of median family income")+
        theme_void() +
    labs(fill = "cost/Income")
)



unAfford.plot <- ggplotly( 
    ggplot(merge,aes(geometry = geometry, fill =Unaffordable)) + 
        geom_sf() + 
        scale_fill_gradientn(colors = col) +
        geom_sf_text(aes(label = Neighborhoods, geometry = geometry), 
                     fun.geometry= st_centroid, size = 1.8) + 
        ggtitle("Share of families that cannot afford child care") +
        theme_void()
)

dfm <- melt(merge.n[,c("Neighborhoods","careInc","Unaffordable")],id.vars = 1)
summary <- ggplot(dfm,aes(x=Neighborhoods, y = value))+
    geom_bar( aes(fill = variable),stat = "identity")+ theme_classic() +
    ggtitle(" Normalized Child care cost over median household income and Child Unafforable rate among Boston Neighborhoods")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel( "Child Affordability Among Boston Neighborhoods"),
    sidebarLayout(
        sidebarPanel(width = 10,
            h2("Introduction"),
            p("The cost of raising child in a city like Boston become very high and caused 
            many social problems: low brith rate, education inequality, and child neglect. 
            Here we visualize the family median income, average cost of care as a percentage of median family income,
share of families that cannot afford care from Analyze Boston website and investigate which 
              neighborhood suffers the most from high cost child care")),  
    mainPanel(width = 10,
          h2("Data Types"),   
                tabsetPanel(
                    tabPanel("Child Cost /Income", plotlyOutput("plotCost")),
                    tabPanel("Unaffordable rate", plotlyOutput("plotUnaf")),
                    tabPanel("Household Income Median", plotlyOutput("Median")),
                tabPanel("Summary",verbatimTextOutput("summaryText"),plotOutput("summary")))
    ),
    ),
    uiOutput("tab"))
    
server <- function(input,output) {
    output$plotCost <- renderPlotly({cost_med.plot})
    output$plotUnaf <- renderPlotly({unAfford.plot})
    output$Median <- renderPlotly({Median.plot})
    output$summary <- renderPlot({summary})
    output$summaryText <- renderText("According this graph, Back bay has the lowest care cost and income ratio and unaffordable rate, and roxbury residents have the highest burden of child care")
    url <- a("Boston Analyze", href="https://data.boston.gov/dataset/boston-opportunity-agenda-state-of-early-early-education-and-care")
    output$tab <- renderUI({
        tagList("Data Source:", url)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
