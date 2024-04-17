library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggthemes)
library(shinythemes)
library(plotly)
library(extrafont)
library(grid)
library(gridExtra)
library(readr)
library(mapDK)
library(mapproj)
library(plyr)
#library(pdftools)

rt <- read.csv("Rt_cases_2021_03_23.csv", sep = ";", dec = ",")

#rt <- reactiveFileReader(1000, session, "Rt_cases_2021_02_09.csv", read.csv(sep = ";", dec = ","))


pos <- read_delim("Test_pos_over_time.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                        grouping_mark = "."), trim_ws = TRUE)
ind <- read_delim("Newly_admitted_over_time.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                 grouping_mark = "."), trim_ws = TRUE)

reg <- read_delim("Region_summary.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                           grouping_mark = "."), trim_ws = TRUE)
dead <- read.csv("Deaths_over_time.csv", sep = ";")

mun <- read_delim("Municipality_test_pos.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                           grouping_mark = "."), trim_ws = TRUE)
age <- read_delim("Cases_by_age.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                           grouping_mark = "."), trim_ws = TRUE)
sex <- read_delim("Cases_by_sex.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                           grouping_mark = "."), trim_ws = TRUE)
#vacpdf <- pdf_subset("Vaccinationstilslutning-06012021-gr01.pdf", pages = 2)

#vac <- pdf_data(vacpdf)

#vac <- data.frame(matrix(unlist(vac), nrow=length(vac), byrow=T))

ui <- dashboardPage(title = "COVID-19 Dashboard", skin = "blue",
                    dashboardHeader(title = "COVID-19 Dashboard"),
                    
                    dashboardSidebar(collapsed = TRUE,
                        sidebarMenu(
                            menuItem("Danmarks nøgletal", tabName = "dashboard", icon = icon("virus")),
                                     menuItem(tagList(shiny::icon("linkedin"),"LinkedIn"), href = "https://www.linkedin.com/in/simon-eilersen/"),
                            menuItem(tagList(shiny::icon("address-card"),"Hjemmeside"), href = "https:visualiseringskonsulent.dk/")
                    )),

        dashboardBody(
           fluidRow(
               valueBoxOutput(width = 3, "rt"), 
               valueBoxOutput(width = 3, "pos"),
               #valueBoxOutput(width = 2, "vac"),
               #valueBoxOutput(width = 2, "vac1"),
               #valueBoxOutput(width = 3, "conf"),
               valueBoxOutput(width = 3, "ind"),
               #valueBoxOutput(width = 3, "new_add"),
               valueBoxOutput(width = 3, "dead")), 
               fluidRow(tabBox(title = tagList(shiny::icon("chart-line"), "Tendens & kortlægning"), id = "tabset1", height = "200px", width = 12, 
                               tabPanel(tagList(shiny::icon("people-arrows"),"Kontakttal"), "", box(title = "", width = 12, plotOutput("rt_tendens", height = 400))),
                               tabPanel(tagList(shiny::icon("percent"),"Positive prøvesvar"), "", box(title = "", width = 12, plotOutput("pos_tendens", height = 400))),
                               tabPanel(tagList(shiny::icon("chart-bar"),"Aldersfordeling"), "", box(title = "", width = 12, plotOutput("age_tendens", height = 400))),
                               tabPanel(tagList(shiny::icon("ambulance"),"Nye indlæggelser"), box(title = "", width = 12, plotOutput("ind_tendens", height = 400))),
                               tabPanel(tagList(shiny::icon("procedures"),"Nye indlæggelser fordelt på Region"), box(title = "", width = 12, plotOutput("ind_reg_tendens", height = 500))),
                               #tabPanel(tagList(shiny::icon("procedures"),"Totale indlæggelser"), box(title = "", width = 12, plotOutput("new_add_tendens", height = 1000))),
                               tabPanel(tagList(shiny::icon("cross"),"Antal døde"), box(title = "", width = 12, plotOutput("dead_tendens", height = 500))),
                               tabPanel(tagList(shiny::icon("map-marked"),"Incidenstal per 100.000"), box(title = "", width = 12, plotOutput("inc_tendens", height = 1000))))),
           fluidRow(tabBox(title = tagList(shiny::icon("hand-point-up"), "Interaktivt"), id = "tabset2", height = "200px", width = 12, 
                           tabPanel(tagList(shiny::icon("people-arrows"),"Kontakttal"), "", box(title = "", width = 12, plotlyOutput("rt_plotly", height = 500))),
                    tabPanel(tagList(shiny::icon("percent"),"Positive prøvesvar"), "", box(title = "", width = 12, plotlyOutput("pos_plotly", height = 700))),
                    tabPanel(tagList(shiny::icon("chart-bar"),"Aldersfordeling"), box(title = "", width = 12, plotlyOutput("age_plotly", height = 500))),
                    tabPanel(tagList(shiny::icon("ambulance"),"Nye indlæggelser"), box(title = "", width = 12, plotlyOutput("ind_plotly", height = 500))),
                    tabPanel(tagList(shiny::icon("cross"),"Antal døde"), box(title = "", width = 12, plotlyOutput("dead_plotly", height = 500)))))) 
    )



server <- function(input, output, session) {

    output$rt <- renderValueBox({
        
        last_row <- tail(rt, n = 1)
        
        valueBox(value = last_row$estimate, "Kontakttal", subtitle = paste0("Kontakttal pr. ", last_row$SampleDate), icon = icon("people-arrows"), 
                 color = "aqua")
    
    })
    
    output$pos <- renderValueBox({
      
      pos1 <- slice(pos, 1:(n()-3))
      
      pos2 <- tail(pos1, n = 7)
      
      sum <- round(sum(pos2$PosPct)/7, digits = 2)
      
    
      valueBox(value = paste0(sum, " %"), "Positiv %", subtitle = paste0("Positive prøvesvar - rullende 7 dage"), icon = icon("percent"),
               color = "blue")
      
    })
    
    output$vac <- renderValueBox({
      
      valueBox(value = vac$X1012, "Total antal vaccinerede", subtitle = "Total antal vaccinerede", icon = icon("syringe"),
               color = "teal")
      
    })
    
    output$vac1 <- renderValueBox({
      
      valueBox(value = paste0(vac$X1013, " %"), "Vaccinationstilslutning", subtitle = "Vaccinationstilslutning", icon = icon("percent"),
               color = "green")
    
      })
    
    output$conf <- renderValueBox({
      
      sex1 <- tail(sex, n = 1)
      
      valueBox(value = sex1$I_alt, "Bekræftede COVID-19 tilfælde", subtitle = "Bekræftede COVID-19 tilfælde", icon = icon("virus"),
               color = "purple")
      
    })
    
    output$ind <- renderValueBox({
      
      ind1 <- tail(ind, n = 1)
      
      valueBox(value = ind1$Total, "Nye indlæggelser", subtitle = paste0("Nye indlæggelser "), icon = icon("ambulance"),
               color = "yellow")
      
    })
    
    #output$new_add <- renderValueBox({
      
    #reg2 <- tail(reg, n = 1)
    
    #valueBox(value = reg2$Indlagt_total, "Totale antal indlæggelser", subtitle = paste0("Totale antal indlæggelser"), icon = icon("procedures"),
    #         color = "orange")
      
    #})
    
    output$dead <- renderValueBox({
      
      dead2 <- slice(dead, 1:(n()-2))
      
      dead1 <- tail(dead2, n = 1)
      
      valueBox(value = dead1$Antal_døde, "Nye dødstilfælde", subtitle = paste0("Nye dødstilfælde"), icon = icon("cross"),
               color = "red")
    
    })
    
    output$rt_tendens <- renderPlot({
        
        theme_set(theme_minimal())
   
        
        ggplot(rt, aes(as.Date(SampleDate), estimate, group = 1)) +
            geom_smooth() +
            geom_hline(yintercept = 1) +
          scale_x_date(date_breaks = "1 month", date_labels = "%b %y", expand = c(0,22)) +
          scale_y_continuous(breaks = seq(0, 2, by = 0.1)) +
          
            labs(x = "",
                 y = "Kontakttal") +
          theme(panel.grid.minor = element_blank())
            
        
      })
    
    output$pos_tendens <- renderPlot({
      
      theme_set(theme_minimal())
      
      pos1 <- slice(pos, 1:(n()-3))
      
      ggplot(pos1, aes(as.Date(Date), PosPct, group = 1)) +
        geom_smooth(se = FALSE) +
        coord_cartesian(ylim = c(0,7.5)) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %y" , expand = c(0,22)) +
        labs(x = "",
             y = "Positiv %") +
        theme(panel.grid.minor = element_blank())
      
    })
    
    output$age_tendens <- renderPlot({
      
      theme_set(theme_minimal())
      
      sex1 <- slice(sex, 1:(n()-1))
      
      ggplot(sex1, aes(x = Aldersgruppe, y = I_alt)) +
        geom_bar(stat = "identity", color = "blue", fill = "blue", width = 0.9, alpha = 0.8) +
        labs(y = "Bekræftede COVID-19 tilfælde")
      
    })
      
    output$ind_tendens <- renderPlot({
    
    ggplot(ind, aes(as.Date(Dato), Total, group = 1)) +
      geom_smooth(se = FALSE) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %y" , expand = c(0,22)) +
      labs(title = "Totalt for Danmark",
           x = "",
           y = "Nye indlæggelser") +
      theme(panel.grid.minor = element_blank())
    
    })
    
    
    output$ind_reg_tendens <- renderPlot({
      
      phoved <- ggplot(ind, aes(as.Date(Dato), Hovedstaden)) +
        geom_smooth(se = FALSE, color = "grey", alpha = 0.2) +
        coord_cartesian(ylim = c(0,80)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "",
             x = "",
             y = "Nye indlæggelser") +
        theme(panel.grid.minor = element_blank())
      
      p1 <- phoved + geom_smooth(data = ind, aes(as.Date(Dato), Midtjylland), se = FALSE, color = "lightgrey", alpha = 0.2)
      
      p2 <- p1 + geom_smooth(data = ind, aes(as.Date(Dato), Nordjylland), se = FALSE, color = "lightgrey", alpha = 0.2)
      
      p3 <- p2 + geom_smooth(data = ind, aes(as.Date(Dato), Sjælland), se = FALSE, color = "lightgrey", alpha = 0.2)
      
      p4 <- p3 + geom_smooth(data = ind, aes(as.Date(Dato), Syddanmark), se = FALSE, color = "lightgrey", alpha = 0.2)
      
      
      ph <- p4 + geom_smooth(data = ind, aes(as.Date(Dato), Hovedstaden), se = FALSE, color = "blue") +
        coord_cartesian(ylim = c(0,100)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Region Hovedstaden",
             x = "",
             y = "Nye indlæggelser") +
        theme(panel.grid.minor = element_blank())
      
      pn <- p4 + geom_smooth(data = ind, aes(as.Date(Dato), Nordjylland), se = FALSE, color = "blue") +
        coord_cartesian(ylim = c(0,100)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Region Nordjylland",
             x = "",
             y = "") +
        theme(panel.grid.minor = element_blank())  
      
      pm <- p4 + geom_smooth(data = ind, aes(as.Date(Dato), Midtjylland), se = FALSE, color = "blue") +
        coord_cartesian(ylim = c(0,100)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Region Midtjylland",
             x = "",
             y = "") +
        theme(panel.grid.minor = element_blank())
      
      psj <- p4 + geom_smooth(data = ind, aes(as.Date(Dato), Sjælland), se = FALSE, color = "blue") +
        coord_cartesian(ylim = c(0,100)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Region Sjælland",
             x = "",
             y = "") +
        theme(panel.grid.minor = element_blank())
      
      psy <- p4 + geom_smooth(data = ind, aes(as.Date(Dato), Syddanmark), se = FALSE, color = "blue") +
        coord_cartesian(ylim = c(0,100)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Region Syddanmark",
             x = "",
             y = "") +
        theme(panel.grid.minor = element_blank())
      
      
      phoved <- ggplot(ind, aes(as.Date(Dato), Hovedstaden)) +
        geom_smooth(se = FALSE) +
        coord_cartesian(ylim = c(0,100)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Region Hovedstaden",
             x = "",
             y = "Nye indlæggelser") +
        theme(panel.grid.minor = element_blank())
      
      pnord <- ggplot(ind, aes(as.Date(Dato), Nordjylland)) +
        geom_smooth(se = FALSE) +
        coord_cartesian(ylim = c(0,100)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Region Nordjylland",
             x = "",
             y = "") +
        theme(panel.grid.minor = element_blank())
      
      psjæl <- ggplot(ind, aes(as.Date(Dato), Sjælland)) +
        geom_smooth(se = FALSE) +
        coord_cartesian(ylim = c(0,100)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Region Sjælland",
             x = "",
             y = "") +
        theme(panel.grid.minor = element_blank())
      
      psyd <- ggplot(ind, aes(as.Date(Dato), Syddanmark)) +
        geom_smooth(se = FALSE) +
        coord_cartesian(ylim = c(0,100)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Region Syddanmark",
             x = "",
             y = "") +
        theme(panel.grid.minor = element_blank())
      
      pmidt <- ggplot(ind, aes(as.Date(Dato), Midtjylland)) +
        geom_smooth(se = FALSE) +
        coord_cartesian(ylim = c(0,100)) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Region Midtjylland",
             x = "",
             y = "") +
        theme(panel.grid.minor = element_blank())
      
      
      grid.arrange(ph, pm, pn, psj, psy, ncol = 5)
      
      
    })
    
    output$new_add_tendens <- renderPlot({
      
      reg$Region <- revalue(reg$Region, c("Hovedstaden" = "region hovedstaden", "Nordjylland" = "region nordjylland", 
                                          "Sjælland" = "region sjælland", "Midtjylland" = "region midtjylland", "Syddanmark" = "region syddanmark"))
      
      reg1 <- slice(reg, 1:(n()-2))
      
      mapDK(values = "Indlagt_total", detail = "region", id = "Region", data = reg1, 
            guide.label = "Totale indlagte")
      
      
    })
    
    output$dead_tendens <- renderPlot({
      
      dead1 <- slice(dead, 1:(n()-1))
      
      p1 <- ggplot(dead1, aes(as.Date(Dato), Antal_døde, group = 1)) +
        geom_smooth(se = FALSE) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %y" , expand = c(0,22)) +
        labs(title = "Totalt for Danmark",
             x = "",
            y = "Nye dødstilfælde") +
      theme(panel.grid.minor = element_blank())
      
      reg$Region <- revalue(reg$Region, c("Hovedstaden" = "region hovedstaden", "Nordjylland" = "region nordjylland", 
                                          "Sjælland" = "region sjælland", "Midtjylland" = "region midtjylland", "Syddanmark" = "region syddanmark"))
      
      #reg2 <- slice(reg, 1:(n()-1))
      
      p2 <- mapDK(values = "Døde", detail = "region", id = "Region", data = reg, 
            guide.label = "Totale døde")
      
      
      grid.arrange(p1, p2, ncol = 2)
      
    })
    
    output$inc_tendens <- renderPlot({
      
      mapDK(values = "Kumulativ_incidens_.per_100000.", detail = "municipal", id = "Kommune_.navn.", data = mun, 
            guide.label = "Bekræftede smittede \nper 100.000 indbyggere")
  

    })
    
    output$rt_plotly <- renderPlotly({
        
        theme_set(theme_minimal())
        
        rt_plot <- ggplot(rt, aes(as.Date(SampleDate), estimate, group = 1, text = paste("Kontakttal: ", estimate, "<br>Dato: ", as.Date(SampleDate)))) +
            geom_line() +
          scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
            labs(x = "",
                 y = "Kontakttal")
        
        ggplotly(rt_plot, tooltip = c("text"))
        
    })
    
    output$pos_plotly <- renderPlotly({
      
      theme_set(theme_minimal())
      
      pos1 <- slice(pos, 1:(n()-3))
      
      pos_plot <- ggplot(pos1, aes(as.Date(Date), PosPct, group = 1, text = paste("Positiv %: ", PosPct, "<br>Dato: ", as.Date(Date)))) +
        geom_line() +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
        labs(x = "",
             y = "Procentuel andel positive prøver")
      
      ggplotly(pos_plot, tooltip = c("text"))
      
    })
    
    output$age_plotly <- renderPlotly({
      
      sex1 <- slice(sex, 1:(n()-1))
      
      
      p1 <- ggplot(sex1, aes(x = Aldersgruppe, y = I_alt, group = 1, text = paste0("Bekræftede tilfælde: ", I_alt, "<br>Kvinder (%): ", `Kvinder_(procent)`, "<br>Mænd (%): ", `Mænd_(procent)`))) +
        geom_bar(stat = "identity", color = "black", fill = "black", width = 0.9) +
        labs(y = "Bekræftede COVID-19 tilfælde")
      
      
      ggplotly(p1, tooltip = c("text"))
      
    })
    
    output$ind_plotly <- renderPlotly({
      
      ind_plot <- ggplot(ind, aes(as.Date(Dato), Total, group = 1, text = paste("Dato: ", as.Date(Dato),
                                                                                "<br>Nye indlæggelser: ", Total,
                                                                                "<br>Hovedstaden: ", Hovedstaden,
                                                                                "<br>Midtjylland: ", Midtjylland,
                                                                                "<br>Nordjylland: ", Nordjylland,
                                                                                "<br>Sjælland: ", Sjælland,
                                                                                "<br>Syddanmark:", Syddanmark))) +
        geom_line() +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %y" , expand = c(0,22)) +
        labs(x = "",
             y = "Nye indlæggelser") +
        theme(panel.grid.minor = element_blank())
      
      ggplotly(ind_plot, tooltip = c("text"))
      
      
    })
    
    output$dead_plotly <- renderPlotly({
      
    dead1 <- slice(dead, 1:(n()-2))
        
    dead_plot <- ggplot(dead1, aes(as.Date(Dato), Antal_døde, group = 1, text = paste("Antal døde: ", Antal_døde, "<br>Dato: ", as.Date(Dato)))) +
      geom_line() +
      ylim(c(0,35)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %y" , expand = c(0,22)) +
      labs(title = "Totalt for Danmark",
           x = "",
           y = "Nye dødstilfælde") +
      theme(panel.grid.minor = element_blank())
    
    ggplotly(dead_plot, tooltip = c("text"))
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
