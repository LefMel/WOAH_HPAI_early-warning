# UI_EVI_Avian_Influenza
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.


library(shiny)    
library(shinydashboard)
library(plotly)
library(DT)
# library(EVI)
library(ggplot2)
library(shinyWidgets)
library(shinythemes)

ui <- dashboardPage(
  # HEADER
  skin = "green", 
  dashboardHeader(
    title = "Early warning of epidemics: An application of early warning tools to HPAI data from the WOAH",
    titleWidth = 1000),
  
  # SIDEBAR
  dashboardSidebar(
    width = 470,
    div(class = "inlay", style = "height:15px;width:100%;background-color: #2c3e50;"),
    tabsetPanel(
      tabPanel(
        "Configuration",
        icon = icon("cog"),
        sidebarMenu(
          id = "tabs",
          # Time Period
          menuItem("Time Period (Range of dates for Avian influenza outbreaks)", tabName = "Time_Period", 
                   icon = icon("clock")),
          dateRangeInput(
            "rdates_Global",
            "",
            start = as.Date(EVI_Global$Days, origin = "1970-01-01")[nrow(EVI_Global) - 365],
            end = max(as.Date(EVI_Global$Days, origin = "1970-01-01"))
          ),
          # Location
          menuItem("Location", tabName="Location", icon=icon("globe")),
          selectInput("region", "Choose a Region:", choices = c("Global", "America", "Africa", "Asia - Pacific", "Europe"), selected = "Global"),
          
          menuItem(
            "Model Features",
            tabName = "dashboard",
            icon = icon("bar-chart"),
            # Model parameters (ratio, moving average, history, rolling window size)
            # Case-definition threshold (ratio for increase)
            selectInput("case_ratio", 
                        "Case-definition increase threshold (%)", 
                        choices = c("5%" = 0.05,
                                    "20%" = 0.2,
                                    "30%" = 0.3),
                        selected = 0.2),
  
            # Moving average
            selectInput("moving_avg", 
                        "Moving Average (days)", 
                        choices = c("7 days" = 7,
                                   "30 days" = 30),
                        selected = 30),
            
            # Maximum rolling window
            selectInput("max_rolling", 
                        "Maximum Rolling Window Size (days)", 
                        choices = c("15 days" = 15,
                                    "30 days" = 30),
                        selected = 15),
            
            # Historical memory
            selectInput("history_days",
                        "Memory of Historical Data",
                        choices = c("90 days" = 90, 
                                    "½ year" = 182,
                                    "1 year" = 365,
                                    "2 years" = 730),
                        selected = 90)
          ),
          menuItem(
            "Plot Features",
            tabName = "dashboard",
            icon = icon("line-chart"),
            checkboxInput("rlog", "Logarithmic scale", value = FALSE),
            sliderInput(inputId = "sizeindex",label = "Point size",min = 0.5,max = 5,value = 1,step = 0.5)
           
          ),
          # Alert
          menuItem("Alert Features", tabName = "Alert", icon = icon("exclamation-triangle"),
          numericInput("ppv_thresh", "Positive Predictive Value Threshold (0 to 1):", value = 0.7, min = 0, max = 1, step = 0.05),
          sliderInput("consec_days", "Consecutive Days for Final Alert:", min = 1, max = 21, value = 3, step = 1),
          checkboxInput("show_final_alerts", "Show Alerts (Red Circles)", value = TRUE)),
          
          hr(),
          menuItem(
            "Table Features",
            tabName = "dashboard2",
            icon = icon("table"),
            #switchInput(
            # inputId = "evi_cevi", label = "- Mode -",
            #  onLabel = "EVI", offLabel = "cEVI", value = TRUE,
            #  width = "100%"
            #)
            checkboxInput("show_only_alerts", "Show only rows with Final Alerts", value = TRUE)
          )
        )
      ),
      tabPanel(
        "Ackws",
        icon = icon("user"),
        h4("References"),
        helpText(tags$b(""), "Kostoulas P, Meletis E, Pateras K et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021)",
                 tags$a(
                   href = "https://www.nature.com/articles/s41598-021-02622-3",
                   "Read it here"
                 ),
        ),
        helpText(tags$b(""), "Meletis E, Pateras K, Eusebi P et al. EVI R-package: Epidemic Volatility Index as an Early-Warning Tool",
                 tags$a(
                   href = "https://cran.r-project.org/web/packages/EVI/EVI.pdf",
                   "R-package Info"
                 ),
        ),
        helpText(tags$b(""), "Pateras K, Meletis E, Denwood M, et al. The convergence epidemic index (cEVI) an early warning tool for identifying waves in an epidemic.",
                 tags$a(
                   href = "https://www.sciencedirect.com/science/article/pii/S2468042723000349",
                   "Read it here"
                 ),
        ),
      ),
      tabPanel(
        "Info",
        icon = icon("info"),
        h4("Version 1.0", style = "margin-left: 45mm;"),
        h4("Figures Explained [Fix fonts and make readable]", style = "margin-left: 1mm;"),
        h6("Observations (updated daily), Logarithmic scale(unselected), presented on the original scale, with red dots corresponding to dates that, according to EVI, an early warning was issued.", style = "color: gray; margin-left: 1mm;"),
        h6("Observations (updated daily), Logarithmic scale(selected), presented on the logarithmic scale, which facilitates the comparison of the steepness of the epidemic curve between the different waves.", style = "color: gray; margin-left: 1mm;"),
        hr(),
        h4("Table variables explained", style = "margin-left: 1mm;"),
        h6("Days: The serial number for each time point.", style = "color: gray; margin-left: 1mm;"),
        h6("EVI/cEVI: The estimated EVI/cEVI for each time point, cEVI takes values 0/1.", style = "color: gray; margin-left: 1mm;"),
        h6("Outbreaks: The rolling average of the newly observed Outbreaks for each time point. A 7-day rolling average is calculated by default (i.e., r_a=7). The user will be given the option to change this [~Next update~]", style = "color: gray; margin-left: 1mm;"),
        h6("Index: Takes values 1 or 0 for the issuance of an early warning or not, respectively", style = "color: gray; margin-left: 1mm;"),
        hr(),
        h5("[~Next update~]", style = "margin-left: 1mm;"),
        h6("Model Predictive value : Positive predictive value (PPV) for the days that an early warning was issued. Higher color intensity corresponds to PPV closer to the value of 1.", style = "color: gray; margin-left: 1mm;"),
        h6("Model Predictive value : Negative predictive values (NPV) for the days that an early warning was not issued. Higher color intensity corresponds to NPV closer to the value of 1.", style = "color: gray; margin-left: 1mm;"),
        h6("lag_max:  Restriction of the maximum window size for the rolling window size. The default is set to one month (lag_max=30) to prevent excess volatility of past epidemic waves from affecting the most recent volatility estimates.  The user could be given the option to change this [~Next update~]", style = "color: gray; margin-left: 1mm;"),
        h6("past: Restriction on the historical data that EVI/cEVI will use. This is set to 365 (default) to account for a year and aid running times. The user could be given the option to change this [~Next update~]", style = "color: gray; margin-left: 1mm;")
      )
    #,
    #selectInput("region", "Choose a Region:", choices = c("Global", "America", "Africa", "Asia - Pacific", "Europe"), selected = "Global")
    
    )
  ),
  
  
  dashboardBody(
    tabPanel(h5("Avian Influenza"),
             fluidRow(
               #plotlyOutput("boxGlobal1", width = "100%"),
               plotlyOutput("box1", width = "100%"),
               #dataTableOutput("EVI_cEVI_Global")
               #dataTableOutput("EVI_cEVI")
               dataTableOutput("Alert", width = "100%")
               
             )
      )
    )
)



#server.R


server <- function(input, output) {
  
  evirlap <- function(Index1,Index2, ln=T, type="p",size.index=1,
                      Index1.lab="EVI1",Index2.lab="EVI2",Index3.lab="EVI-",Index.country=NULL, origin="1970-01-01") {
    
    Index1$Days = as.Date(Index1$Days, origin="1970-01-01")
    Index1$Index=Index1$Index
    Index2$Days = as.Date(Index2$Days, origin="1970-01-01")
    Index2$Index=Index2$Index*2
    Index=Index1
    Index$Index=Index$Index+Index2$Index
    if(length(table(Index$Index))<3)
      Index$Index[1:3]<-1:3
    Index$cases_1=Index$Cases*Index$Index
    Index$cases_1[Index$cases_1 == 0] <- NA
    Index$cases_0=Index$Cases*(1-Index$Index)
    Index$cases_0[Index$cases_0 == 0] <- NA
    
    Index$npv=Index$npv*(1-Index$Index)
    Index$npv[Index$npv == 0] <- NA
    Index$ppv=Index$ppv*Index$Index
    Index$ppv[Index$ppv == 0] <- NA
    Index$variable<-"x"
    Index$Index[is.na(Index$Index)]<-0
    Index$Index<-factor(Index$Index,labels = c("No warning",paste(Index1.lab,"alone"),paste(Index2.lab,"alone"), Index3.lab))
    Index$Outbreaks<-Index$Cases
    if (ln==F) {
      sp3<-ggplot(Index, aes_string(x="Days",group="variable"))+
        list(
          geom_point(aes_string(y=("Outbreaks"), color="Index"), size=size.index),
          #scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
          scale_colour_grey(start = 1,end = 0),
          scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
          labs(title = paste0("HPAI- (c)EVI - ",input$region), y = "Outbreaks", x="Days"),
          #          labs(title = paste0("Graph combining outputs ",Index1.lab,", ", Index2.lab," and ", Index3.lab," - ",Index.country), y = "Outbreaks", x="Days"),
          theme(legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size=8),
                legend.key.height = unit(0, 'cm')),
          if (type=="l")  geom_path(aes_string(y="Outbreaks",colour="Index"),size=size.index)
        )
    }
    
    if (ln==T) {
      sp3<-ggplot(Index, aes_string(x="Days",group="variable"))+
        list(
          geom_point(aes_string(y="log(Outbreaks)", color="Index"), size=size.index),
          #scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
          scale_colour_grey(start = 1,end = 0),
          scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
          labs(title = paste0("HPAI- (c)EVI - ",input$region), y = "Outbreaks", x="Days"),
          #          labs(title = paste0("Graph combining outputs ",Index1.lab,", ", Index2.lab," and ", Index3.lab," - ",Index.country), y = "log(Outbreaks)", x="Days"),
          theme(legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size=8),
                legend.key.height = unit(0, 'cm'))
        )
      if (type=="l"){ geom_path(aes_string(y="log(Outbreaks)",colour="Index"), size=size.index) }
    }
    print(sp3)
    
  }
  
  
  detect_final_alerts <- function(evi_df, cevi_df, ppv_threshold, consecutive_days ) 
  {
    library(dplyr)
    library(zoo)
    
    # Ensure date formatting
    evi_df$Days <- as.Date(evi_df$Days)
    cevi_df$Days <- as.Date(cevi_df$Days)
    
    # Merge EVI and cEVI data
    combined_df <- evi_df %>%
      select(Days, Cases, EVI_Index = Index, EVI_ppv = ppv, EVI_npv = npv) %>%
      left_join(
        cevi_df %>% select(Days, cEVI_Index = Index, cEVI_ppv = ppv, cEVI_npv=npv),
        by = "Days"
      ) %>%
      mutate(
        #combined_signal = EVI_Index + 2 * cEVI_Index,  # 1 = EVI only, 2 = cEVI only, 3 = both
        ppv = case_when(
          EVI_Index == 1 & cEVI_Index == 1 ~ pmin(EVI_ppv, cEVI_ppv, na.rm = TRUE),
          EVI_Index == 1 & (cEVI_Index == 0 | is.na(cEVI_Index)) ~ EVI_ppv,
          (EVI_Index == 0 | is.na(EVI_Index)) & cEVI_Index == 1 ~ cEVI_ppv,
          EVI_Index == 0 & cEVI_Index == 0 ~ 0,
          TRUE ~ NA_real_
        ),
        npv = case_when(
          EVI_Index == 1 & cEVI_Index == 1 ~ 0,
          EVI_Index == 1 & (cEVI_Index == 0 | is.na(cEVI_Index)) ~ EVI_npv,
          (EVI_Index == 0 | is.na(EVI_Index)) & cEVI_Index == 1 ~ cEVI_npv,
          EVI_Index == 0 & cEVI_Index == 0 ~ pmin(EVI_npv, cEVI_npv, na.rm = TRUE),
          TRUE ~ NA_real_
        ),
        valid_signal = ifelse((EVI_Index == 1 | cEVI_Index == 1) & !is.na(ppv) & ppv >= ppv_threshold, 1, 0)
        
      ) %>% 
      
      # Rolling window check
      mutate(
        signal_count = ifelse(
          row_number() >= consecutive_days,
          rollapply(valid_signal, consecutive_days, sum, fill = NA, align = "right"),
          NA
        ),
        mean_ppv_window = ifelse(
          row_number() >= consecutive_days,
          rollapply(ppv, consecutive_days, mean, fill = NA, align = "right", na.rm = TRUE),
          NA
        ),
        final_alert = ifelse(signal_count >= consecutive_days & mean_ppv_window >= ppv_threshold, 1, 0)
      )
    
    return(combined_df)
  }
  
  evirlap_alert <- function(Index, ln = TRUE, type = "p", size.index = 1,
                            Index1.lab = "EVI", Index2.lab = "cEVI", Index3.lab = "EVI-",
                            Index.country = NULL, origin = "1970-01-01", region = NULL) {
    
    Index$Days <- as.Date(Index$Days)
    
    # Set signal label (0 = no warning, 1 = EVI, 2 = cEVI, 3 = both)
    Index$Index <- as.numeric(Index$EVI_Index) + 2 * as.numeric(Index$cEVI_Index)
    
    # Prevent factor issue when not all levels are present
    if (length(table(Index$Index)) < 3) Index$Index[1:3] <- 1:3
    
    Index$cases_1 <- Index$Cases * Index$Index
    Index$cases_1[Index$cases_1 == 0] <- NA
    Index$cases_0 <- Index$Cases * (1 - Index$Index)
    Index$cases_0[Index$cases_0 == 0] <- NA
    
    Index$npv <- Index$npv * (1 - Index$Index)
    Index$npv[Index$npv == 0] <- NA
    Index$ppv <- Index$ppv * Index$Index
    Index$ppv[Index$ppv == 0] <- NA
    
    Index$variable <- "x"
    Index$Index[is.na(Index$Index)] <- 0
    Index$Index <- factor(Index$Index,
                          labels = c("No warning",
                                     paste(Index1.lab, "alone"),
                                     paste(Index2.lab, "alone"),
                                     Index3.lab))
    
    Index$Outbreaks <- Index$Cases
    
    # Basic ggplot object
    if (ln == FALSE) {
      sp3 <- ggplot(Index, aes(x = Days, group = variable)) +
        geom_point(aes(y = Outbreaks, color = Index), size = size.index) +
        scale_color_manual(values = c("grey69", "yellow3", "orange3", "red4")) +
        labs(title = paste0("HPAI- (c)EVI - ", region),
             y = "Outbreaks", x = "Days") +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = 8),
              legend.key.height = unit(0, 'cm'))
    }
    
    if (ln == TRUE) {
      sp3 <- ggplot(Index, aes(x = Days, group = variable)) +
        geom_point(aes(y = log(Outbreaks), color = Index), size = size.index) +
        scale_color_manual(values = c("grey69", "yellow3", "orange3", "red4")) +
        labs(title = paste0("HPAI- (c)EVI - ", region),
             y = "log(Outbreaks)", x = "Days") +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = 8),
              legend.key.height = unit(0, 'cm'))
    }
    
    # ➕ Optional: Overlay Final Alerts as RED CIRCLES
    if ("final_alert" %in% names(Index)) {
      sp3 <- sp3 +
        geom_point(data = Index[Index$final_alert == 1, ],
                   aes(x = Days, y = if (ln) log(Outbreaks) else Outbreaks),
                   shape = 21, fill = "red", color = "black", size = size.index + 1.5, stroke = 0.5)
    }
    
    print(sp3)
  }
  
  
  
  
  pdf(NULL)
  data <- reactive({
    switch(input$region,
           "Global" = list(dataset1 = EVI_Global, dataset2 = cEVI_Global),
           "Africa" = list(dataset1 = EVI_Africa, dataset2 = cEVI_Africa),
           "Asia - Pacific" = list(dataset1 = EVI_Asia_Pacific, dataset2 = cEVI_Asia_Pacific),
           "Europe" = list(dataset1 = EVI_Europe, dataset2 = cEVI_Europe),
           "America" = list(dataset1 = EVI_America, dataset2 = cEVI_America))
  })  
  
  final_alert_data <- reactive({
    req(input$ppv_thresh, input$consec_days)
    detect_final_alerts(
      evi_df = data()$dataset1,
      cevi_df = data()$dataset2,
      ppv_threshold = input$ppv_thresh,
      consecutive_days = input$consec_days
    )
  })
  

  #    output$boxGlobal1 <- renderPlotly({
  #    options()
  #    par(mfrow=c(1,1))
  #    LL=ifelse(identical(which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[1]),integer(0)), yes = which(as.Date(EVI_Global$Days, origin="1970-01-01")==(as.Date(input$rdates_Global[1]+1))), no = which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[1])) 
  #    UL=ifelse(identical(which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[2]),integer(0)), yes = which(as.Date(EVI_Global$Days, origin="1970-01-01")==(as.Date(input$rdates_Global[2]-1))), no = which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[2])) 
  #    evirlap(Index1 = EVI_Global[LL:UL,], Index2 = cEVI_Global[LL:UL,],size.index = input$sizeindex,
  #            Index1.lab = "EVI", Index2.lab = "cEVI", Index3.lab = "cEVI-", ln = ifelse(test=input$rlog, T , F), Index.country = "HPAI", type = ifelse(test = input$rlines,"l","p"))
  
  #  })
  
  #  output$boxGlobal2 <- renderPlot({
  #    LL=ifelse(identical(which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[1]),integer(0)), yes = which(as.Date(EVI_Global$Days, origin="1970-01-01")==(as.Date(input$rdates_Global[1]+1))), no = which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[1])) 
  #    UL=ifelse(identical(which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[1]),integer(0)), yes = which(as.Date(EVI_Global$Days, origin="1970-01-01")==(as.Date(input$rdates_Global[2]-1))), no = which(as.Date(EVI_Global$Days, origin="1970-01-01")==input$rdates_Global[2])) 
  #    evirlap(Index1 = EVI_Global, Index2 = cEVI_Global,
  #            Index1.lab = "EVI", Index2.lab = "cEVI", Index3.lab = "EVI-", ln = T, Index.country = "HPAI", type = ifelse(test = input$rlines,"l","p"))
  #  })
  
  

    output$box1 <- renderPlotly({
         LL <- which.min(abs(as.Date(data()$dataset1$Days, origin = "1970-01-01") - input$rdates_Global[1]))
         UL <- which.min(abs(as.Date(data()$dataset1$Days, origin = "1970-01-01") - input$rdates_Global[2]))
  
        df_final <- final_alert_data()
        df_plot <- df_final[LL:UL, ]
        
        if (input$show_final_alerts) {
          p <- evirlap_alert(
            Index = df_plot,
            ln = input$rlog,
            size.index = input$sizeindex,
            Index1.lab = "EVI", Index2.lab = "cEVI", Index3.lab = "cEVI-",
            region = input$region
          )
        } else {
          # fallback to regular evirlap if alerts not shown
          p <- evirlap(
            Index1 = data()$dataset1[LL:UL, ],
            Index2 = data()$dataset2[LL:UL, ],
            ln = input$rlog,
            size.index = input$sizeindex,
            Index1.lab = "EVI", Index2.lab = "cEVI", Index3.lab = "cEVI-",
            Index.country = "HPAI"
          )
        }
        
        ggplotly(p)
      })
  
  #EVI_cEVI<-renderUI({
  #  
  #}) 
  
  
  
#  output$EVI_cEVI <- DT::renderDataTable({
#    if(input$evi_cevi==TRUE) {
     # out<-round(data()$dataset1[,1:4],3)
    #  out$Date<-as.Date(out$Days,"1970-01-01")
    #  out<-out[c(5,2,3,4)]
    #  names(out)[2:3]<-c("cEVI","Outbreaks")
    #}
#    if(input$evi_cevi==FALSE) {
     # out<-round(data()$dataset2[,1:4],3)
    #  out$Date<-as.Date(out$Days,"1970-01-01")
    #  out<-out[c(5,2,3,4)]
    #  names(out)[2:3]<-c("cEVI","Outbreaks")
    #}
    #out<-EVI_Global
#    datatable(
#      out,
#      extensions = 'Buttons',
#      options = list(
#       paging = TRUE,
#       searching = TRUE,
#       fixedColumns = TRUE,
#       autoWidth = TRUE,
#       ordering = TRUE,
#       dom = 'Blfrtip',
#       buttons = c('copy', 'csv', 'excel'),
#       lengthMenu = list(c(10, 30, 50, -1),
#                         c('10', '30', '50', 'All')),
#       paging = T)
#   )
  #})
  # as.Date(EVI_Global$Days, origin="2024-12-31")
  
    output$Alert <- DT::renderDataTable({
      req(final_alert_data())  # Ensure data is available
      
      df <- final_alert_data() %>%
        select(
          Date = Days,
          Cases,
          EVI_Signal = EVI_Index,
          cEVI_Signal = cEVI_Index,
          PPV = ppv,
          Signal_Streak = signal_count,
          Mean_PPV_Window = mean_ppv_window,
          Final_Alert = final_alert
        ) %>%
        mutate(Date = as.Date(Date, origin = "1970-01-01"))
      
      if (input$show_only_alerts) {
        df <- df %>% filter(Final_Alert == 1)
      }
      
      datatable(
        df,
        extensions = 'Buttons',
        options = list(
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel'),
          lengthMenu = list(c(10, 30, 50, -1), c('10', '30', '50', 'All'))
        )
      )
    })
    
    

  
}

shinyApp(ui=ui, server = server)

