server <- function(input, output, session) {
  
  # Cargar datos                            ####
  # df <- readRDS('N:/UDMTD/UDTMDCOM/AdvITI_github/04-01.VisualizeOutput/data/E30052/AdvITI_ts_annualRates.rds')
  df <- readRDS('AdvITI_ts_annualRates_publi.rds')
  # 
  # cat(dim(df))
  # cat(names(df))
  # 
  # df_uploaded <- reactive({
  #   if (is.null(input$uploaded_file))
  #     return(NULL)
  #   df <- readRDS('AdvITI_ts_annualRates0.rds')
  #   df 
  #   #You can also do your analysis right here in the reactive expression if you want
  # }) 
    
    # You can access the value of the widget with input$slider1, e.g.
    tabla <- reactive({df %>% filter(agrupacion == input$tabselected) %>%
      filter((date >= input$slider2[1]) & (date <= input$slider2[2])) %>%
      filter(sub %in% eval(parse(text= paste0("input$",input$tabselected)))) 
    })  
  
    output$table <- renderTable(tabla())
    value_height<-reactive(length(unique(tabla()$sub)))
    # You can access the values of the second widget with input$slider2, e.g.
    output$plot <- renderPlot({
      batch_values <- c('batch 01', 'batch 02', 'batch 03');
      batch_labels <- c('batch 1: t+20', 'batch 2: t+27', 'batch 3: t+38');
      names(batch_labels) <- batch_values
      ggplot(data = tabla(), aes(date, annual_rate)) + 
        geom_line(data = subset(tabla(), !indexVersion %in% c('final', 'initial')), aes(colour = indexVersion), width = 0.5, linetype = "longdash") +
        geom_point(aes(shape = indexVersion, color = indexVersion, alpha = indexVersion), size = 3) +
        geom_errorbar(aes(ymin= annual_rate - rmse, ymax= annual_rate + rmse), width= 14, colour = 'grey17', position= position_dodge(.9)) +
        geom_hline(yintercept = 0, aes(linetype = 'dotted', color = 'brown1', size = 2)) +
        facet_grid(sub ~ batch, scales = "free", labeller = labeller(batch = batch_labels)) +
        scale_x_date(date_breaks = "3 month", date_minor_breaks = "1 month", date_labels = "%m-%Y") +
        scale_color_manual(name = "Index Version", values = c('black', 'red', 'darkblue'), labels = c('advanced \u00B1 rmse', 'final', 'initial')) +
        scale_shape_manual(name = "Index Version", values = c(19, 15, 15), labels = c('advanced \u00B1 rmse', 'final', 'initial')) +
        scale_alpha_discrete(name = "Index Version", range = c(1, 0.3, 0.3), labels = c('advanced \u00B1 rmse', 'final', 'initial')) + 
        labs(title = 'Industrial Turnover Index Annual Rate', x = '', y= '', colour = 'Index Version') +
        theme_bw() +
        theme(plot.title    = element_text(hjust = 0.5, size = 15, face="bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 14),
              axis.text.y   = element_text(size = 10),
              axis.text.x   = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1),
              strip.text  = element_text(size = 12), legend.position = 'top',
              legend.title  = element_text(size= 12), legend.text = element_text(size= 12))
    },height=function(){350+(value_height()-1)*200},width=700)
    
    ccaa.input <- reactive({
        switch(input$ccaa, list.ccaa)
    })
    division.input <- reactive({
        switch(input$division, list.division)
    })
    div_subdiv.input <- reactive({
        switch(input$div_subdiv, list.div_subdiv)
    })
    mig.input <- reactive({
        switch(input$MIG, list.MIG)
    })
    mig2.input <- reactive({
        switch(input$MIG2, list.MIG2)
    })
      
    observe({
        
      if (input$select.ccaa == 0) {
        
        return(NULL) 
        
      } else if (input$select.ccaa%%2 == 0) {
          
          updateCheckboxGroupInput(session,"ccaa","NUTS2", choices=list.ccaa)
        
      } else {
          
          updateCheckboxGroupInput(session,"ccaa","NUTS2", choices=list.ccaa, selected=list.ccaa)
      
      }
    })
    
    observe({
        
      if(input$select.division == 0) {
        
        return(NULL) 
        
      } else if (input$select.division%%2 == 0) {
    
          updateCheckboxGroupInput(session,"division","Division",choices=list.division)
      
      } else {
        
          updateCheckboxGroupInput(session,"division","Division",choices=list.division,selected=list.division)
        
      }
    })
    
  }

