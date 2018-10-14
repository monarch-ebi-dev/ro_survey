#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(stringr)
library(plyr)
library(ggplot2)
library(DT)

ws = "rosurvey.RData"

if(file.exists(ws)){
  load(ws)
} else {
  stop("Data is not available!")
}

ui <- fluidPage(
   
   # Application title
   titlePanel("Relation Ontology Analysis"),
   h6("This App was produced by Nico Matentzoglu and David Osumi-Sutherland (EMBL-EBI) for the Monarch Initiative to help understanding the use of RO across ontologies on the web."),
   
   sidebarLayout(
      sidebarPanel(
        selectInput("corpus", label = "Corpus?",
                    choices = unique(ct_p$corpus), selected = "bp"),
        selectInput("import", label = "Imports?",
                    choices = unique(ct_p$ftype), selected = "merged")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h4("How many ontologies use RO?"),
        DT::dataTableOutput("ct_ro"),
        
        hr(),
        h4("How many different RO relations does this ontology use?"),
        DT::dataTableOutput("ontology_usage_table"),
        sliderInput("rel_ct_offset", "Cut-off (Relation count):",min = 0, max = max(ct_p$freq)+1,step=1,value=median(unique(ct_p$freq))),
        plotOutput("gg_toprel"),
       
        hr(),
        h5("What kind of axiomtype is the RO relation used in? In how many axioms of that type is it used?"),
        DT::dataTableOutput("df_axiomdata"),
        
        hr(),
        h4("What kind of axiomtype is the RO relation used in? In how many ontologies?"),
        DT::dataTableOutput("df_axiomdata_o"),
        
        hr(),
        h5("What kind of class expressions is the RO relation used in? In how many axioms of that type is it used?"),
        DT::dataTableOutput("df_expressiondata"),
        
        hr(),
        h4("What kind of class expressions is the RO relation used in? In how many ontologies?"),
        DT::dataTableOutput("df_expressiondata_o"),
        
        hr(),
        h4("Asserted domains and ranges of OBO relations"),
        DT::dataTableOutput("df_domain_range_asserted"),
        
        hr(),
        h4("Inferred domains of OBO relations"),
        DT::dataTableOutput("df_domain"),
        
        hr(),
        h4("Inferred ranges of OBO relations"),
        DT::dataTableOutput("df_range"),
        
        hr(),
        h4("Full breakdown of relations used across ontologies"),
        DT::dataTableOutput("ro_ontology_table")
        
      )
   ) , 
   hr(),
   h4("Experiment"),
   DT::dataTableOutput("ct_download_sucess"),
   h6("https://monarchinitiative.org/")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$gg_toprel<-renderPlot({
    ggplot(ct_p[ct_p$ftype==input$import&ct_p$corpus==input$corpus&ct_p$freq>input$rel_ct_offset,c("label","freq")],aes(x=reorder(label,freq),y=freq))+geom_bar(stat="identity")+xlab("RO Relation") + ylab("Number of ontologies used in") + coord_flip()
    },height = 400,width = 600)   

   output$ct_download_sucess <- DT::renderDataTable({
     ct_download_sucess[ct_download_sucess$corpus==input$corpus,]
   },options = list(pageLength = 10, autoWidth = TRUE),
   rownames= FALSE)
   
   output$ct_ro <- DT::renderDataTable({
     ct_ro[ct_ro$ftype==input$import&ct_ro$corpus==input$corpus,]
   },options = list(pageLength = 10, autoWidth = TRUE),
   rownames= FALSE)
   
   output$ro_coverage_table <- DT::renderDataTable({
     ct_p[ct_p$ftype==input$import&ct_p$corpus==input$corpus,c("p","label","freq")]
   },options = list(pageLength = 10, autoWidth = TRUE),
   rownames= FALSE)
   
   output$ro_ontology_table <- DT::renderDataTable({
     df_p[df_p$ftype==input$import&df_p$corpus==input$corpus,c("p","label","o")]
   },options = list(pageLength = 10, autoWidth = TRUE),
   rownames= FALSE)
   
   output$ontology_usage_table <- DT::renderDataTable({
     ct_o[ct_o$ftype==input$import&ct_o$corpus==input$corpus,c("o","freq")]
   },options = list(pageLength = 10, autoWidth = TRUE),
   rownames= FALSE)
   
   output$df_axiomdata <- DT::renderDataTable({
     ct_axt[ct_axt$ftype==input$import&ct_axt$corpus==input$corpus,c("p","axiomtype","freq")]
   },options = list(pageLength = 10, autoWidth = TRUE),
   rownames= FALSE)
   
   output$df_axiomdata_o <- DT::renderDataTable({
     ct_axto[ct_axto$ftype==input$import&ct_axto$corpus==input$corpus,c("p","axiomtype","freq")]
   },options = list(pageLength = 10, autoWidth = TRUE),
   rownames= FALSE)
   
   output$df_expressiondata_o <- DT::renderDataTable({
     ct_expo[ct_expo$ftype==input$import&ct_expo$corpus==input$corpus,c("p","expressiontype","freq")]
   },options = list(pageLength = 10, autoWidth = TRUE),
   rownames= FALSE)
   
   output$df_expressiondata <- DT::renderDataTable({
     ct_exp[ct_exp$ftype==input$import&ct_exp$corpus==input$corpus,c("p","expressiontype","freq")]
   },options = list(pageLength = 10, autoWidth = TRUE),
   rownames= FALSE)
   
   output$df_domain <- DT::renderDataTable({
     ct_dom[ct_dom$ftype==input$import&ct_dom$corpus==input$corpus,c("p","subject","expressiontype","freq")]
   },options = list(pageLength = 10, autoWidth = TRUE),
   rownames= FALSE)
   
   output$df_range <- DT::renderDataTable({
     ct_ran[ct_ran$ftype==input$import&ct_ran$corpus==input$corpus,c("p","object","expressiontype","freq")]
   },options = list(pageLength = 10, autoWidth = TRUE),
   rownames= FALSE)
   
   output$df_domain_range_asserted <- DT::renderDataTable({
     df_domain_range_asserted[df_domain_range_asserted$ftype==input$import&df_domain_range_asserted$corpus==input$corpus,c("p","domain","range")]
   },options = list(pageLength = 10, autoWidth = TRUE),
   rownames= FALSE)
   
}

# Run the application 
shinyApp(ui = ui, server = server)

