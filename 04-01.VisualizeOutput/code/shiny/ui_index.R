

## Definir desagregaciones                ####
list.total <-  list(
  "Total Index" = 'general'
)
list.ccaa <-  list(
  "Andaluc\u00EDa" = '01',
  "Arag\u00F3n" = '02',
  "Asturias" = '03',
  'Baleares' = '04',
  'Canarias' = '05',
  'Cantabria' = '06',
  'Castilla y Le\u00F3n' = '07',
  'Castilla la Mancha' = '08',
  'Catalu\u00F1a' = '09',
  'Comunidad Valenciana' = '10',
  'Extremadura' = '11',
  'Galicia' = '12',
  'Madrid' = '13',
  'Murcia' = '14',
  'Navarra' = '15',
  'Pa\u00EDs Vasco' = '16',
  'La Rioja' = '17',
  'Ceuta' = '18',
  'Melilla' = '19'
)
list.div_subdiv <-list(
  'Extraction of anthracite, coal and lignite' = '05',
  'Mining of metal ores' = '07',
  'Other mining and quarrying' = '08',
  'Food industries (except grain mill products and food for animals)' = '10A',
  'Manufacture of grain mill products, starches, and food for animals' = '10B',
  'Manufacture of beverages' = '11',
  'Manufacture of tobacco' = '12',
  'Preparation and spinning of textile fibres. Manufacture of woven textiles. Textile finishings' = '13A',
  'Manufacture of knitted fabrics, carpets, rope, non-w oven fabrics, textile products for technical use and industrial and other textile products' = '13B',
  'Manufacture of garments' = '14',
  'Leather and footwear industry' = '15',
  'Manufacture of wood and of products of wood and cork, except furniture; basketmaking and wickerwork' = '16',
  'Paper industry' = '17',
  'Graphic arts and reproduction of recorded media' = '18',
  'Manufacture of coke and refined petroleum products' = '19',
  'Manufacture of cleaning articles, perfumes and cosmetics' = '20A',
  'Chemical industry except cleaning articles, perfumes and cosmetics' = '20B',
  'Manufacture of pharmaceutical products' = '21',
  'Rubber and plastic material transformation industry' = '22',
  'Manufacture of other non-metallic ore products' = '23',
  'Metallurgy; manufacture of iron, steel and ferro-alloy products' = '24',
  'Manufacture of metal products for construction, containers made of metal, steam generators, weapons and ammunition' = '25A',
  'Forging, stamping, embossing and rolling of metals. - Manufacture of tools, hardware goods, containers and other metal products' = '25B',
  'Manufacture of electronic components, assembled printed circuits, and magnetic and optical media' = '26A',
  'Manufacture of computers, peripherals and telecommunications equipment; appliances for measuring and navigation; radiation and medical and therapeutic equipment' = '26B',
  'Manufacture of consumer electronics, optical instruments and photographic equipment' = '26C',
  'Manufacture of household appliances' = '27A',
  'Manufacture of electrical material and equipment except household appliances' = '27B',
  'Manufacture of machinery and equipment n.e.c.' = '28',
  'Manufacture of motor vehicles, trailers and semi-trailers' = '29',
  'Naval, railway, aircraft and spacecraft construction. Combat vehicles' = '30A',
  'Manufacture of motorcycles, bicycles, vehicles for the disabled and others' = '30B',
  'Manufacture of furniture' = '31',
  'Manufacture of jewellery, costume jewellery and musical instruments' = '32A',
  'Manufacture of sporting goods; games and toys and Other manufacturing industries' = '32B',
  'Manufacture of medical and dental instruments and supplies' = '32C',
  'Repair and installation of machinery and equipment' = '33'
)

list.division <-
  list(
    'Extraction of anthracite, coal and lignite' = '05',
    'Mining of metal ores' = '07',
    'Other mining and quarrying' = '08',
    'Food industry' = '10',
    'Manufacture of beverages' = '11',
    'Manufacture of tobacco' = '12',
    'Textile industry' = "13",
    'Manufacture of garments' = '14',
    'Leather and footwear industry' = '15',
    'Manufacture of wood and of products of wood and cork, except furniture; basketmaking and wickerwork' = '16',
    'Paper industry' = '17',
    'Graphic arts and reproduction of recorded media' = '18',
    'Manufacture of coke and refined petroleum products' = '19',
    'Chemical industry' = '20',
    'Manufacture of pharmaceutical products' = '21',
    'Rubber and plastic material transformation industry' = '22',
    'Manufacture of other non-metallic ore products' = '23',
    'Metallurgy; manufacture of iron, steel and ferro-alloy products' = '24',
    'Manufacture of metal products, except machinery and equipment' = '25',
    'Manufacture of computer, electronic and optical products' = '26',
    'Manufacture of electrical material and equipment' = '27',
    'Manufacture of machinery and equipment n.e.c.' = '28',
    'Manufacture of motor vehicles, trailers and semi-trailers' = '29',
    'Manufacture of other transport material' = '30',
    'Manufacture of furniture' = '31',
    'Other manufacturing industries' = '32',
    'Repair and installation of machinery and equipment' = '33'
  )

list.MIG <-
  list(
    "Capital Goods" = "BC",
    "Intermediate Goods" = "BI",
    "Durable Consumer Goods" = "CD",
    "Non-Durable Consumer Goods" = "CN",
    "Energy" = "EN"
  )

list.seccion <- list("Mining and quarrying industries" = "B", "Manufacturing industry" = "C")
list.MIG2 <- list(
  "Capital Goods" = "BC",
  "Intermediate Goods" = "BI",
  "Energy" = "EN",
  "Consumer Goods" = "XC"
)



sidebar <- dashboardSidebar(
  sidebarMenu(style = "position: fixed; overflow: visible;"),
  conditionalPanel(condition="input.tabselected=='general'",
                   checkboxGroupInput("general", label = "Total Index", 
                                      choices = list.total ,
                                      selected = list('general'))
  ),
  conditionalPanel(condition="input.tabselected=='ccaa'",
                   actionLink("select.ccaa","Select All"),
                   checkboxGroupInput("ccaa", label = "NUTS2", 
                                      choices = list.ccaa ,
                                      selected = list('01'))
  ),
  
  conditionalPanel(condition="input.tabselected=='div_subdiv'",
                   actionLink("select.div_subdiv","Select All"),
                   checkboxGroupInput("div_subdiv", label = "Division-Group", #"Subdivisi\u00F3n", 
                                      choices = list.div_subdiv,
                                      selected = list('05'))
                   
                   
  ),
  conditionalPanel(condition="input.tabselected=='division'",
                   actionLink("select.division","Select All"),
                   checkboxGroupInput("division", label = "Division", #"Divisi\u00F3n", 
                                      choices = list.division,
                                      selected = list('05'))),
  
  conditionalPanel(condition="input.tabselected=='seccion'",
                   checkboxGroupInput("seccion", label = "Section",  #"Secci\u00F3n", 
                                      choices = list.seccion,
                                      selected = list.seccion)),
  conditionalPanel(condition="input.tabselected=='MIG'",
                   checkboxGroupInput("MIG", label = "MIG", 
                                      choices = list.MIG,
                                      selected = list.MIG)),
  
  conditionalPanel(condition="input.tabselected=='MIG2'",
                   checkboxGroupInput("MIG2", label = "MIG2", 
                                      choices = list.MIG2,
                                      selected = list.MIG2))
  
)

# Header ----
header <- dashboardHeader(title="Advanced ITI")

# Body ----
body <- dashboardBody(fluidPage(
  sliderInput("slider2", label = NULL, 
              min = as.Date("01-10-2017","%d-%m-%Y"), max = as.Date("01-04-2021","%d-%m-%Y"),
              value=c(as.Date("01-01-2020","%d-%m-%Y"), as.Date("01-04-2021","%d-%m-%Y")),
              timeFormat="%b %Y"),
  mainPanel(
    tabsetPanel(
      tabPanel("Total Index", value="general",
               h4("Total Index")),
      tabPanel("NUTS2", value="ccaa",
               h4("NUTS2")),
      tabPanel("Division-Group", value="div_subdiv",
               h4("Division-Group")),
      tabPanel("Division", value="division",
               h4("Division")),
      tabPanel("Section", value="seccion",
               h4("Section")),
      tabPanel("MIG", value="MIG",
               h4("MIG")),
      tabPanel("MIG2", value="MIG2",
               h4("MIG2")),
      id = "tabselected"
    ),
    #tableOutput('table'),
    plotOutput(outputId='plot'))
  #imageOutput("myImage", height = "100%", width = "100%")
  
)
)

ui <- dashboardPage(header, sidebar, body)