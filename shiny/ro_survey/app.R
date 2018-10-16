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

options(scipen = 1000)

pc<-function(x,y) {
  return(round(100*(x/y),2))
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
        h3("How many ontologies use RO?"),
        p("The corpus size is the number of ontologies that 1) downloaded successfully and 2) could be successfully preprocessed by Robot. 
          Reasons for failing the pre-processing are mostly parse errors (OWL API) and unloadable imports (for full break-down of processing success see bottom of page)."),
        DT::dataTableOutput("ct_ro"),
        
        p(),hr(),p(),
        
        h3("How many different RO relations does this ontology use?"),
        p("Use the axiomtype restrictor to filter what kinds of axioms are considered for determining usage. For example, if you select SubClassOf and EquivalentClasses, only those ontologies will be counted that, for a relation P, used at least one SubClassOf or EquivalentClasses axiom using P."),
        selectInput("in_axiomtype", label = "Which axiom type?",
                    choices = unique(ct_axto$axiomtype), selected = "SubClassOf",multiple = TRUE),
        uiOutput("rel_ct_offset"),
        plotOutput("gg_toprel"),
        p("The following table shows how many ontologies use a given relation across all the axiom types selected above."),
        DT::dataTableOutput("ontology_usage_table"),
        
        p(),hr(),p(),
        
        h3("What kind of axiomtype is the RO relation used in? In how many axioms of that type is it used?"),
        p("Select an axiom type to see how many axioms of that type where used together with a particular relation. "),
        selectInput("rel_ax_type", label = "Which axiom type?",
                    choices = unique(ct_axto$axiomtype), selected = "SubClassOf",multiple = FALSE),
        uiOutput("rel_ct_ax_offset"),
        plotOutput("gg_toprelax"),
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
    dgg<-df_ax[df_ax$ftype==input$import&df_ax$corpus==input$corpus&(df_ax$axiomtype %in% input$in_axiomtype),]
    dgg<-merge(dgg,unique(df_p[,c("ftype","corpus","corpussize")]),by=c("ftype","corpus"),all.x = TRUE)
    dgg<-unique(dgg[,c("label","o","corpussize")])
    head(dgg)
    cdgg<-count(dgg[,c("label","corpussize")])
    cdgg$pc<-pc(cdgg$freq,cdgg$corpussize)
    head(cdgg)
    cdgg<-cdgg[cdgg$freq>=input$rel_ct_offset[1]&cdgg$freq<=input$rel_ct_offset[2],]
    ggplot(cdgg,aes(x=reorder(label,freq),y=freq))+geom_bar(stat="identity")+ geom_text(aes(label=paste(freq," (",pc," %)",sep="")), hjust=1.1, color="white") +xlab("RO Relation") + ylab("Number of ontologies used in") + coord_flip()
    },height = 400,width = 600)   
  
  output$gg_toprelax<-renderPlot({
    ggplot(ct_axt[ct_axt$ftype==input$import&ct_axt$corpus==input$corpus&ct_axt$freq>=input$rel_ct_ax_offset[1]&ct_axt$freq<=input$rel_ct_ax_offset[2]&ct_axt$axiomtype==input$rel_ax_type,c("label","axiomtype","freq")],aes(x=reorder(label,freq),y=freq))+geom_bar(stat="identity")+xlab("RO Relation") + ylab("Number of axioms used in") + coord_flip()
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
   
   output$rel_ct_ax_offset <- renderUI({
     df<-ct_axt[ct_axt$ftype==input$import&ct_axt$corpus==input$corpus,c("p","freq")]
     sliderInput("rel_ct_ax_offset", "Cut-off (Relation count):",min = 0, max = max(df$freq)+1,step=1,value=c(median(unique(df$freq)),max(unique(df$freq))))
   })
   
   
   output$rel_ct_offset <- renderUI({
     d2<-unique(ct_p[ct_p$ftype==input$import&ct_p$corpus==input$corpus,c("p","freq")])
     sliderInput("rel_ct_offset", "Cut-off (Relation count):",min = 0, max = max(d2$freq)+1,step=1,value=c(median(unique(d2$freq)),max(unique(d2$freq))+1))
   })
   
   output$ontology_usage_table <- DT::renderDataTable({
     dgg<-unique(df_ax[df_ax$ftype==input$import&df_ax$corpus==input$corpus&(df_ax$axiomtype %in% input$in_axiomtype),c("label","o")])
     cdgg<-count(dgg[,c("o")])
     cdgg<-cdgg[cdgg$freq>=input$rel_ct_offset[1]&cdgg$freq<=input$rel_ct_offset[2],]
     cdgg[order(-cdgg$freq),]
   },options = list(pageLength = 10, autoWidth = TRUE),
   rownames= FALSE)
   
   output$df_axiomdata <- DT::renderDataTable({
     ct_axt[ct_axt$ftype==input$import&ct_axt$corpus==input$corpus&ct_axt$axiomtype==input$rel_ax_type,c("p","label","axiomtype","freq")]
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

