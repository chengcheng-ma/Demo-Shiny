library(dplyr)
library(ggplot2)
library(stringr)
library(haven)
library(shiny)
library(openxlsx)
library(shinydashboard)
library(gridExtra)
library(tinytex)


### 解决ShinyApps上没有中文字体的问题 ###

options(shiny.usecairo = FALSE)
font_home <- function(path = '') file.path('~', '.fonts', path)
if (Sys.info()[['sysname']] == 'Linux') {
  dir.create(font_home())
  file.copy('Chinese_font.ttc', font_home())
  system2('fc-cache', paste('-f', font_home()))
}
rm(font_home)
if (.Platform$OS.type == "windows") {
  if (!grepl("Chinese", Sys.getlocale())) {
    Sys.setlocale(, 'Chinese')
  }
}

subchunkify <- function(g, fig_height=7, fig_width=15) {
  g_deparsed <- paste0(deparse(
    function() {g}
  ), collapse = '')
  
  sub_chunk <- paste0("`","``{r sub_chunk_", floor(runif(1) * 10000), ", fig.height=", fig_height, ", fig.width=", fig_width, ", echo=FALSE,results='hide'}",
                      "\n(", 
                      g_deparsed
                      , ")()",
                      "\n`","``")
  
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}

### define chooserInput custom input ###

chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE) {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
    singleton(tags$head(
      tags$script(src="chooser-binding.js"),
      tags$style(type="text/css",
                 HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",
        div(class="chooser-container chooser-left-container",
            tags$select(class="left", size=size, multiple=multiple, leftChoices)
        ),
        div(class="chooser-container chooser-center-container",
            icon("arrow-right", "right-arrow fa-2x"),
            tags$br(),
            icon("arrow-left", "left-arrow fa-2x")
        ),
        div(class="chooser-container chooser-right-container",
            tags$select(class="right", size=size, multiple=multiple, rightChoices)
        )
    )
  )
}

registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  if (is.null(data))
    NULL
  else
    list(left=as.character(data$left), right=as.character(data$right))
}, force = TRUE)

### 坐标轴变量排序 ###

reorder_size <- function(x,y) {
  factor(x, levels = names(sort(table(x),decreasing = y)))
}

### UI ###

ui<-function(require){dashboardPage(
  
  dashboardHeader(title = "Basic data processing",
                  titleWidth = "calc(100% - 44px)"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Data", tabName = "data", icon = icon("list-alt")),
      menuItem("Analysis", tabName = "analysis", icon = icon("table")),
      menuItem("Charts", tabName ="charts", icon = icon("chart-bar"),
               menuSubItem("Basic", tabName = "basic", icon = icon("chart-line")),
               menuSubItem("Professional", tabName = "pro",icon = icon("chart-area"))),
      bookmarkButton(
        label = "Bookmark",
        width='83%',
        icon = shiny::icon("link", lib = "glyphicon")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      ### data UI part ###
      tabItem("data",
              fluidRow(
                column(width = 3,
                       box(width = NULL, collapsible=T,
                           fileInput("import", "Choose sas Dataset",
                                     multiple = TRUE,
                                     accept = c(".sas7bdat")),
                           
                           selectInput(inputId = "dataset",
                                       label = "Choose a dataset:",
                                       choices = NULL),
                           
                           checkboxInput(inputId = "filter",
                                         label = "Filter",
                                         value  = FALSE),
                           
                           uiOutput("add_filter"),
                           
                           checkboxInput(inputId = "select",
                                         label = "Select",
                                         value  = FALSE),
                           
                           uiOutput("add_select"),
                           
                           checkboxInput(inputId = "order",
                                         label = "Order",
                                         value  = FALSE),
                           
                           uiOutput("add_order"),
                           
                           actionButton("button", "Show"),
                           
                           downloadButton("downloadData", "Download")
                       )
                ),
                column(width = 9, 
                       box(width = NULL, collapsible=T,
                           dataTableOutput("table")
                       )
                )
              )
      ),
      
      tabItem("analysis",
              fluidRow(
                column(width = 3,
                       box(width = NULL, collapsible=T,
                           
                           selectInput("model", "Choose Analysis model:",
                                       choices = c("Histogram","Scatter chart"))
                       )
                ),
                column(width = 9, 
                       box(width = NULL, collapsible=T,
                           dataTableOutput("table2")
                       )
                )
              )
      ),
      ### chart basic UI part ###
      tabItem("basic",
              fluidRow(
                column(width = 3,
                       box(width = NULL, collapsible=T,
                           
                           selectInput("type", "Choose plot type:",
                                       choices = c("Histogram","Scatter chart")),
                           
                           selectInput(inputId = "xaxis",
                                       label = "X:",
                                       choices = NULL),
                           conditionalPanel("input.type == 'Histogram'",
                                            uiOutput("ui_hist")
                           ),
                           
                           conditionalPanel("input.type != 'Histogram'",
                                            uiOutput("ui_y")
                           ),
                           
                           actionButton("button2", "Show"),
                           
                           downloadButton("downloadplot", "Download")
                       )
                ),
                column(width = 9, 
                       box(width = NULL, collapsible=T,
                           uiOutput("plotui")
                       )
                )
              )
      )
    )
  )
)
  
}

server <- function(input, output,session) {
  ### Data part ###
  df<-reactive({
    fileNames_sas<-str_subset(input$import$name, ".sas7bdat")
  })
  
  observeEvent(df(), {
    freezeReactiveValue(input, "dataset")
    updateSelectInput(inputId = "dataset", choices = df())
  })
  
  #读取data
  data<-reactive({
    req(input$dataset)
    file<-input$import%>%filter(name==input$dataset)
    data <- read_sas(file$datapath)
    return(data)
  })
  
  #filter=TRUE 时显示的格外UI
  output$add_filter <- renderUI({
    if (input$filter == TRUE) {
      tagList(
        textAreaInput("filter_text", "",row=3),
        uiOutput("ui_filter_error")
      )
    }
  })
  
  #select=TRUE 时显示的格外UI
  output$add_select <- renderUI({
    if (input$select == TRUE) {
      chooserInput("select_vars", "Available vars", "Selected vars",
                   names(data()), c(), size = 10, multiple = TRUE)
    }
  })
  
  #order=TRUE 时显示的格外UI
  output$add_order <- renderUI({
    if (input$order == TRUE) {
      if(input$select == TRUE){
        selectizeInput("order_vars", "", choices =input$select_vars$right,
                       multiple = TRUE,  options = list(plugins = list("remove_button", "drag_drop")))
      }else{
        selectizeInput("order_vars", "", choices =names(data()),
                       multiple = TRUE,  options = list(plugins = list("remove_button", "drag_drop")))
      }
    }
  })
  
  #点击button 生成数据集
  df_1<-eventReactive(input$button,{
    req(input$dataset)
    if (input$filter == TRUE) {
      req(input$filter_text)
      filter_cond <- isolate(input$filter_text)
      tryCatch({
        data1<-data()%>%filter(eval(parse(text=filter_cond)))
        output$ui_filter_error <- renderUI({
          return()
        })},
        error=function(err) {
          url <- a("common filter expressions",href="https://docs.google.com/document/d/1mtmMBw94VYu3DqgUp-u63_11FEQ7X_iCCtmNgk6UL7A/edit?usp=sharing",target="_blank")
          output$ui_filter_error <- renderUI({
            helpText("Condition Erorr: please reference to ",url)
          })
          data1 <- data.frame()
          return(NULL)
        }
      )
    }else{
      data1<-data()
    }
    
    if(exists("data1")){
      if(input$select == TRUE) {
        req(input$select_vars$right)
        data2<-data1%>%select(all_of(input$select_vars$right))
      }else{
        data2<-data1
      }
    }else{
      data2 <- data.frame()
      return(data2)
    }
    
    if(exists("data2")){
      if(input$order == TRUE) {
        req(input$order_vars)
        data3<-data2%>%arrange(across(all_of(input$order_vars)))
      }else{
        data3<-data2
      }
      return(data3)
    }else{
      return(NULL)
    }
  }
  )
  
  #display data
  output$table<-renderDataTable(df_1(), options = list(pageLength = 8,scrollX = TRUE))
  
  #export data as excel file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(str_remove(input$dataset, ".sas7bdat"), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(df_1(), file)
    }
  )
  
  ### charts basic part ###
  
  # add y axis UI panel
  output$ui_y <- renderUI({
    selectInput(inputId = "yaxis",
                label = "Y:",
                choices = names(df_1()))
  })
  
  # add Histogram UI panel
  output$ui_hist <- renderUI({
    tagList(
      checkboxInput(inputId = "Horizontal",
                    label = "Horizontal",
                    value  = FALSE),
      checkboxInput(inputId = "Subject",
                    label = "Count subject",
                    value  = FALSE))
  })
  
  # updata xaxis and yaxis choices
  observeEvent(df_1(), {
    freezeReactiveValue(input, "xaxis")
    updateSelectInput(inputId = "xaxis", choices = names(df_1()))
  })
  
  # updata plotOutput height
  plot_ui<-eventReactive(input$button2,{
    if(input$Horizontal==F){
      plotOutput("Plot", height=(ceiling(length(unique(df_1()[[input$xaxis]]))/8)*250))
    }
    else{
      plotOutput("Plot", height=(150+length(unique(df_1()[[input$xaxis]]))*20))
    }
    
  })
  
  output$plotui <- renderUI({
    plot_ui()
  })
  
  #draw plot
  plot<-function(){
    if(input$Subject==T){
      data3<-df_1()%>%distinct(USUBJID,.data[[input$xaxis]],.keep_all = TRUE)
    }else{
      data3<-df_1()
    }
    
    df0 <-data3 %>% count(.data[[input$xaxis]]) %>% arrange(desc(n))
    var<-df0[[input$xaxis]]
    length_var <- length(var)
    nplot<- ceiling(length_var/8)
    if(input$type=="Histogram"&input$Horizontal==F){
      p<-list()
      for (i in 1:nplot) {
        if((8*i)>length_var){
          list_var<-var[((i-1)*8+1):length_var]
        }else{
          list_var<-var[((i-1)*8+1):(8*i)]
        }
        data4<-data3%>%filter(.data[[input$xaxis]] %in% list_var)
        
        p[[i]]<-ggplot(data4,mapping= aes(x = reorder_size(.data[[input$xaxis]],T), y = ..count..)) + 
          geom_bar(stat = 'count',fill="steelblue")+ 
          geom_text(aes(label=..count..),stat = 'count',vjust=-0.2,size = 3) +
          ylim(0, 1.1*max(table(data3[[input$xaxis]])))+
          theme(axis.text.x = element_text(size=8))+
          scale_x_discrete(labels=function(x) str_replace(x, "(^.{8})","\\1\n"))+
          labs(x=isolate(input$xaxis),y='计数')
        }
        return(p)
      }else if(input$type=="Histogram"&input$Horizontal==T){
      plot1 <- ggplot(data3,mapping= aes(x = reorder_size(.data[[input$xaxis]],F), y = ..count..)) + 
        geom_bar(stat = 'count',fill="steelblue") + 
        geom_text(aes(label=..count..),stat = 'count',hjust = -0.2,size = 3) +
        theme(axis.text.x = element_text(size=8))+
        scale_x_discrete(labels=function(x) str_replace(x, "(^.{8})","\\1\n"))+
        coord_flip()+
        labs(x=isolate(input$xaxis),y='计数')
      return(plot1)
    }
    else{
      plot2 <- ggplot(data=df_1(), aes_string(x=input$xaxis, y=input$yaxis)) +
        geom_point()+
        theme(axis.text.x = element_text(size=6))+
        scale_x_discrete(labels=function(x) str_replace(x, "(^.{8})","\\1\n"))
      return(plot2)
    }
  }
  
  plot_cut<-function(){
    if(input$Subject==T){
      data3<-df_1()%>%distinct(USUBJID,.data[[input$xaxis]],.keep_all = TRUE)
    }else{
      data3<-df_1()
    }
    
    df0 <-data3 %>% count(.data[[input$xaxis]]) %>% arrange(desc(n))
    var<-df0[[input$xaxis]]
    list_var<-var[1:30]
    if(input$type=="Histogram"&input$Horizontal==T){
      data4<-data3%>%filter(.data[[input$xaxis]] %in% list_var)
      plot1 <- ggplot(data4,mapping= aes(x = reorder_size(.data[[input$xaxis]],F), y = ..count..)) + 
        geom_bar(stat = 'count',fill="steelblue") + 
        geom_text(aes(label=..count..),stat = 'count',hjust = -0.2,size = 3) +
        theme(axis.text.x = element_text(size=8))+
        scale_x_discrete(labels=function(x) str_replace(x, "(^.{8})","\\1\n"))+
        coord_flip()+
        labs(x=isolate(input$xaxis),y='计数')
      return(plot1)
    }

  }
  
  plot_server<-eventReactive(input$button2,{
    if(input$type=="Histogram"&input$Horizontal==F){
      do.call(grid.arrange,c(plot(),ncol=1))
    }else{plot()}
  })
  
  output$Plot <- renderPlot({
    plot_server()
  })
  
  output$downloadplot = downloadHandler(
    filename = function() {"plots.docx"},
    content = function(file) {
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }
                     
                     ## End of progression
                     src <- normalizePath('summary_report.Rmd')
                     
                     # temporarily switch to the temp dir, in case you do not have write
                     # permission to the current working directory
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))
                     file.copy(src, 'summary_report.Rmd', overwrite = TRUE)
                     
                     library(rmarkdown)
                     out <- render('summary_report.Rmd', params = list(type = input$Horizontal,n=length(unique(df_1()[[input$xaxis]]))),word_document(),)
                     file.rename(out, file)
                   })
    }
  )
}

shinyApp(ui, server,enableBookmarking = "server")
