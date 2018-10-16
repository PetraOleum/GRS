library(shiny)
library(ggplot2)
library(scales)
library(tidyverse)
library(shinycssloaders)

# UI ----
ui <- navbarPage("Gender Reassignment Surgery", collapsible = TRUE, 
                 selected = "About",
  # Vaginoplasty UI ----
  tabPanel("Vaginoplasty — \"MtF\"",
       sidebarPanel(width=3, 
            checkboxInput("vagprojc", label="Project waiting list out to:", 
                          value=FALSE),
            sliderInput("vagprojyears", label=NULL, min=2030, max=2080, 
                        step=1, value=2030, sep=""),
            numericInput("vagin", "Additions to list per year", min=0, 
                         max=NA, step=0.1, value=0),
            numericInput("vagout", "Funded vaginoplasties per year", 
                         min=0.1, max=NA, step=0.1, value=1.5),
            h4("Options"),
            checkboxInput("vagwly", "Show waiting list length in years"),
            checkboxInput("vagminc", "Set graph minimum", value=TRUE),
            conditionalPanel("input.vagminc",
             numericInput("vagmin", label=NULL, value=0)),
            checkboxInput("vagmaxc", "Set graph maximum", value=FALSE),
            conditionalPanel("input.vagmaxc",
             numericInput("vagmax", label=NULL, value=NA)),
            checkboxInput("vagdatesc", "Set graph date range", value=FALSE),
            conditionalPanel("input.vagdatesc",
             dateRangeInput("vagdates", label=NULL,
                            start="2015-04-02", end=NULL,
                            format="dd/mm/yyyy"))
       ),
       mainPanel(
          tabsetPanel(
            tabPanel("Graph",
              h3("New Zealand vaginoplasty waiting list"),
              plotOutput("vagPlot") %>% withSpinner(type=5)
            ),
            tabPanel("Table",
              h3("Table of projected waiting list values"),
              tableOutput("vagpTable") %>% withSpinner(type=1)
            )
          )
       )
  ),
  # Phalloplasty UI ----
  tabPanel("Phalloplasty — \"FtM\"",
       sidebarPanel(width=3,
            checkboxInput("phalprojc", label="Project waiting list out to:", 
                          value=FALSE),
            sliderInput("phalprojyears", label=NULL, min=2030, max=2080, 
                        step=1, value=2030, sep=""),
            numericInput("phalin", "Additions to list per year", min=0, 
                         max=NA, step=0.1, value=0),
            numericInput("phalout", "Funded phalloplasties per year", 
                         min=0.1, max=NA, step=0.1, value=0.5),
            h4("Options"),
            checkboxInput("phalwly", "Show waiting list length in years"),
            checkboxInput("phalminc", "Set graph minimum", value=TRUE),
            conditionalPanel("input.phalminc",
             numericInput("phalmin", label=NULL, value=0)),
            checkboxInput("phalmaxc", "Set graph maximum", value=FALSE),
            conditionalPanel("input.phalmaxc",
             numericInput("phalmax", label=NULL, value=NA)),
            checkboxInput("phaldatesc", "Set graph date range", value=FALSE),
            conditionalPanel("input.phaldatesc",
             dateRangeInput("phaldates", label=NULL,
                            start="2015-04-02", end=NULL,
                            format="dd/mm/yyyy"))
       ),
       mainPanel(
          tabsetPanel(
            tabPanel("Graph",
              h3("New Zealand phalloplasty waiting list"),
              plotOutput("phalPlot") %>% withSpinner(type=5)
            ),
            tabPanel("Table",
              h3("Table of projected waiting list values"),
              tableOutput("phalpTable") %>% withSpinner(type=1)
            )
          )
       )
  ),
  # About Page ----
  tabPanel("About",
           h1("Important update"),
           p("The following information and app was created prior to a change",
             "in the funding rules for GRS made by the government in mid-2018.",
             "This app can be used as a resource to project out what would have",
             "happened had the change not taken place, but cannot in its present",
             "form take into account the changes. For more information read",
             a(href="https://www.newsroom.co.nz/@pro/2018/10/15/277680/gender-reassignment-surgery-cap-lifted?amp=1", "this article"), "at newsroom. The rest of this page has",
             "been left as-is."),
           hr(),
           p("Since a review in 2003 the New Zealand Ministry of Health",
             "has nominally funded \"MtF\" GRS (i.e. trans women's vaginoplasty) at",
             "a rate of three every two years, and \"FtM\" GRS (trans men's phalloplasty)",
             "at one every two years. The retirement of Peter Walker, who was performing",
             "vaginoplasty for the MoH, in 2014—coupled with the apparent increase",
             "in number of publically out transgender people—has led to astronomical rises to",
             "the waiting lists for both types of surgery."),
           p("Several people have obatained details about the waiting lists from",
             "the Ministry via Official Information Act requests since 2015.",
             "The data obtained is summarised at the bottom of this page;",
             "if you have newer datapoints or others",
             "that have been missed please send them to me on", 
             a(href="https://twitter.com/OleumPetra", "Twitter"), "or",
             a(href="mailto:oleumpetra+blog@gmail.com", "via email"), "."),
           p("This app shows that data visually, but it also projects it",
             "into the future, allowing the rate of people being added",
             "to the list to be changed by the user, along with the",
             "level of government funding. The inadequacy of the present",
             "funding levels can be easily seen, and the size of the increase",
             "that is required estimated."),
           p("Note however that, as this crisis has been ongoing for some",
             "time, many people interested in surgery will not have taken",
             "the time and expense to formally add themselves to the",
             "waiting list. Therefore the true size of the problem",
             "cannot be easily estimated. In addition the Ministry",
             "has contacted a number of people on the list and found that,",
             "given the sheer length of time that has elapsed already,",
             "some do not wish to continue with surgery or have found",
             "some way to self-fund, shortening",
             "the list without actually funding a surgery."),
           hr(),
           h4("Known vaginoplasty waiting list lengths"),
           tableOutput("mtfData") %>% withSpinner(type=1, proxy.height = "100px"),
           h4("Known phalloplasty waiting list lengths"),
           tableOutput("ftmData") %>% withSpinner(type=1, proxy.height = "100px")
  )
   
)

# Server ----
server <- function(input, output) {
  # Data logic ----
  pData <- reactive({
    read.csv("P.csv", header=T, stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
  })
  
  vData <- reactive({
    read.csv("V.csv", header=T, stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
  })
  
  output$mtfData <- renderTable({
    vData() %>% mutate(Date = format(Date, "%d/%m/%Y"),
                       `Length (years)` = Length / 1.5) %>%
      select("Date", "Length", "Length (years)", "Source")
  })
  
  output$ftmData <- renderTable({
    pData() %>% mutate(Date = format(Date, "%d/%m/%Y"),
                       `Length (years)` = Length / 0.5) %>%
      select("Date", "Length", "Length (years)", "Source")
  })
  
  # Projection logic ----
  vagProj <- reactive({
    vd <- vData()
    ld <- vd$Date[nrow(vd)]
    ld.y <- as.numeric(format(ld, "%Y")) # year of last data value
    ld.d <- format(ld, "-%m-%d") # date part of last data value
    lv <- vd$Length[nrow(vd)] # last length value
    yrs <- (ld.y + 1):(input$vagprojyears) # Vector of projection years
    ny <- length(yrs) # Number of projection years
    pc <- rep(input$vagin - input$vagout, ny)
    pv <- cumsum(pc) + lv
    pv <- ifelse(pv < 0, 0, pv)
    dates <- as.Date(paste0(yrs, ld.d))
    data.frame(Date=c(ld, dates), Length = c(lv, pv))
  })
  
  phalProj <- reactive({
    pd <- pData()
    ld <- pd$Date[nrow(pd)]
    ld.y <- as.numeric(format(ld, "%Y")) # year of last data value
    ld.d <- format(ld, "-%m-%d") # date part of last data value
    lv <- pd$Length[nrow(pd)] # last length value
    yrs <- (ld.y + 1):(input$phalprojyears) # Vector of projection years
    ny <- length(yrs) # Number of projection years
    pc <- rep(input$phalin - input$phalout, ny)
    pv <- cumsum(pc) + lv
    pv <- ifelse(pv < 0, 0, pv)
    dates <- as.Date(paste0(yrs, ld.d))
    data.frame(Date=c(ld, dates), Length = c(lv, pv))
  })
  
  # V plot logic ----
  output$vagPlot <- renderPlot({
    vd <- vData()
    showyearsleft <- input$vagwly
    if (showyearsleft) {
      vd <- mutate(vd, Length = Length / 1.5)
    }
    ggplot(vd, aes(Date, Length)) -> v.p
    v.p + geom_line(size = 1.5, na.rm = TRUE) -> v.p
    if (input$vagprojc) {
      vpr <- vagProj()
      if (showyearsleft) {
        vpr <- mutate(vpr, Length = Length / input$vagout)
      }
      v.p + geom_line(data=vpr, aes(Date, Length), 
                      size=1.5, linetype = "dashed",
                      na.rm=TRUE) -> v.p
    }
    if (showyearsleft) {
      v.p + ylab("Waiting list length (years)") -> v.p
    } else {
      v.p + ylab("Waiting list length (people)") -> v.p
    }
    
    v.p + theme_linedraw() -> v.p
    glim <- c(
      ifelse(input$vagminc, input$vagmin, NA),
      ifelse(input$vagmaxc, input$vagmax, NA)
    )
    v.p + scale_y_continuous(limits=glim) -> v.p
    if (input$vagdatesc) {
      v.p + coord_cartesian(xlim = input$vagdates) -> v.p
    }
    v.p
  })
  
  # P plot logic ----
  output$phalPlot <- renderPlot({
    pd <- pData()
    showyearsleft <- input$phalwly
    if (showyearsleft) {
      pd <- mutate(pd, Length = Length / 0.5)
    }
    ggplot(pd, aes(Date, Length)) -> p.p
    p.p + geom_line(size = 1.5, na.rm=TRUE) -> p.p
    if (input$phalprojc) {
      ppr <- phalProj()
      if (showyearsleft) {
        ppr <- mutate(ppr, Length = Length / input$phalout)
      }
      p.p + geom_line(data=ppr, aes(Date, Length), 
                      size=1.5, linetype = "dashed",
                      na.rm=TRUE) -> p.p
    }
    if (showyearsleft) {
      p.p + ylab("Waiting list length (years)") -> p.p
    } else {
      p.p + ylab("Waiting list length (people)") -> p.p
    }
    
    p.p + theme_linedraw() -> p.p
    glim <- c(
      ifelse(input$phalminc, input$phalmin, NA),
      ifelse(input$phalmaxc, input$phalmax, NA)
    )
    p.p + scale_y_continuous(limits=glim) -> p.p
    if (input$phaldatesc) {
      p.p + coord_cartesian(xlim = input$phaldates) -> p.p
    }
    p.p
  })
  
  # Projection table logic ----
  output$phalpTable <- renderTable({
    pp <- phalProj() %>% mutate(Date = format(Date, "%Y"))
    names(pp) <- c("Year", "Length")
    if (input$phalwly) {
      pp <- mutate(pp, Length = Length / input$phalout)
      names(pp) <- c("Year", "Length (years)")
    }
    pp
  })
  
  output$vagpTable <- renderTable({
    vp <- vagProj() %>% mutate(Date = format(Date, "%Y"))
    names(vp) <- c("Year", "Length")
    if (input$vagwly) {
      vp <- mutate(vp, Length = Length / input$vagout)
      names(vp) <- c("Year", "Length (years)")
    }
    vp
  })
}

shinyApp(ui = ui, server = server)

