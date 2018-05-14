# load libraries

library(shinydashboard)
library(data.table)
library(dplyr)
library(geojsonio)
library(sp)
library(leaflet)
library(stringr)
library(htmltools)
library(plotly)
library(ggplot2)

data <- fread("./WranglingCompleteNew.csv") %>%
      mutate(EMPLOYMENT_START_DATE = as.Date(EMPLOYMENT_START_DATE))

stateBoundary <- geojsonio::geojson_read("./gz_2010_us_040_00_500k.json", what = "sp")

origins <- fread("./H1BCountryOfOrigin.csv")

ui <- dashboardPage(skin = "purple",
      dashboardHeader(title = "H-1B Visa Applicants 2010 - 2018"),
      dashboardSidebar(
            sidebarMenu(
                  menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
                  menuItem("Booming Economy?", tabName = "boomingEcon", icon = icon("computer", class  = "fas fa-laptop")),
                  menuItem("Coastal Preference?", tabName = "coastalPref", icon = icon("bicycle", class = "fas fa-bicycle")),
                  menuItem("Crowding Out?", tabName = "crowdingOut", icon = icon("th")),
                  menuItem("DataTable", tabName = "nittyGritty", icon = icon("database", class = "fas fa-database")),
                  menuItem("About", tabName = "about", icon = icon("info", class = "fas fa-info"))
                  )
            ),
      
      dashboardBody(
            tabItems(
                  tabItem(tabName = "introduction",
                          titlePanel("Welcome to Our Page!"),
                          fluidPage(
                                box(title = strong("Background information on this project"), 
                                    HTML(paste("Imagine attending a career fair as an international college student, learning about companies and meeting people who are doing amazing work in the fields that you are interested in. Finally, when you decide to send in your job application, you find out that the company doesn’t sponsor work visas. This is a common scenario for international students in the US. Having been through this process ourselves, we decided to make things slightly easier for our friends from across the world who are hoping to work full-time in the US. We created an interactive dashboard analyzing over 3 million records of H1-B work visa petitions from 2010-2018 using data from the Office of Foreign Labor Certification (OFLC). It is important to note that applying for an H-1B does not necessarily mean employment, however we only have data on the former. With this web application, we made it effortless to find out information about past visa applicants. ",
                                               "<br>", "<br>",
                                               "“What is a work visa and how does a foreigner get the visa?” you might ask. Work visas are employment-based, non-immigrant visa categories that allow foreign nationals to work in the US for a temporary period of time. The most popular work visa category is the H-1B visa that employers use to hire foreign workers in ‘speciality occupations’ for which there are not enough skilled American workers to fill available jobs. In order to qualify for the H-1B visa, the workers must have a bachelor’s degree. After the companies identify a worker they wish to hire, they submit a petition (a collection of forms and documents describing the job and the unique qualifications of the person chosen to fill it) on the candidate’s behalf, and pay fees that are around $10,000, everything considered. Because the United States Citizenship and Immigration Services (USCIS) receives more petitions than there are visas (capped at 65,000 for those with bachelor’s degree with 20,000 more available for those with master’s degrees or higher), the USCIS holds a lottery to randomly select which petitions it will process. The success rate for bachelor's degree holders varies amongst years but is consistently low, around 30-40%. For more information we recommend you check out this", a("article.", href = "https://www.nytimes.com/2018/04/06/us/what-are-h1b-visas.html"), 
                                               "<br>", "<br>", 
                                               "To classify the jobs into distinct occupations, we rely on the Bureau of Labor Statistics’ 2010 Standard Occupational Classification System (SOC). This system sorts jobs into 23 major occupational categories, which are then sorted into 840 occupational subcategories. The first two numbers in an SOC-code reflect the (major) categories, while the latter four represent the subcategories, all organized in a hierarchical nature (digit-by-digit). More details on the system can be found", a("here.", href = "https://www.bls.gov/soc/soc_structure_2010.pdf"),
                                               "<br>", "<br>",
                                               "Through this application we not only hope to provide information to foreigners who are hoping to gain employment in the US, but also to dispel myths about the H-1B visa in general. On the graphs below, we recreated the most common criticisms about the H-1B program: it is mostly used by Indian tech outsourcing firms to bring low-wage IT professionals to the US (applicants’ countries of origin are not published on a granular level by the OFLC, so for that graph we relied on summary data from the USCIS). However, we believe that reality is more nuanced, and we hope that through the graphs, map, and data table we created in this application, we will provide greater insight. The site can be navigated by clicking on the tabs on the left."
                                               )),
                                    
                                    width = 12),
                                box(title = strong("Top 10 Sponsoring Companies"),
                                    plotlyOutput("topten", height = "400px"), width = 6),
                                box(title = strong("Top 10 Countries of Origin"),
                                    plotlyOutput("origins", height = "400px"), width = 6)
                                )
                          ),
                  
                  tabItem(tabName = "boomingEcon",
                          titlePanel("Is the job market booming for H-1B applicants?"),
                          fluidPage(
                              box(title = strong("Changes in Mean Wage by Job Category"), 
                                  selectInput(inputId = "occupationG",
                                              label = h4("Occupation(s) to compare: "), 
                                              choices = sort(unique(data$OTitle)), 
                                              selected = "Management", multiple = TRUE), 
                                  plotlyOutput("salaryChange"),
                                  HTML(paste("<br>", "<br>",
                                             "In this graph you can compare how occupations' salaries have changed over the years. The wages are not adjusted for inflation, and the large jump in 2018 might be misleading, as we only have data for the First Quarter of the 2018 Financial Year (e.g. 2017 October - December). It is possible that the relatively large increase is due to selection bias (as October 1st is the first day those H-1Bs that went through the lottery can start), so perhaps there is something special in the salaries of that seasons cohort versus the  rest.")),
                                  width = 12)
                              )
                        ),
                
                  tabItem(tabName = "coastalPref",
                          titlePanel("Where are the H-1B applicants located?"),
                          fluidPage(
                                box(title = strong("State Level Comparison of H-1B Applicants, by Job Category"), 
                                    selectInput(inputId = "occupationM",
                                                label = h4("Select an occupation to visualize: "),
                                                choices = sort(unique(data$OTitle)), 
                                                selected = "Management"), 
                                    HTML(paste("Don't forget to click on the states for more information, and drag the map to see Alaska and Hawaii!",
                                               "<br>", "<br>")),
                                    leafletOutput("mapdens", height = "600px"), 
                                    width = 12)
                                )
                          ),

                  tabItem(tabName = "crowdingOut",
                          fluidPage(
                                titlePanel("Are H-1B applicants crowding out American workers?"),
                                box(title = strong("Comparison of H-1B Mean and Prevailing Wages, by Job Sub-categories"),
                                    plotlyOutput("salaryComp", height = "600px"), 
                                    HTML(paste("<br>", "<br>",
                                               "The prevailing wage for a position is defined as the average wage paid to workers in the requested occupation in the same area of intended employment. It is included in the OFLC dataset, and is based on Government Statistics.",
                                               "The mean wage is calculated by us as the average of an applicants' minimum and maximum wage. Since in most of the entries the applicant’s maximum proposed wage rate was empty, so the mean wage is the same as the employer’s proposed wage rate in most cases.",
                                               "<br>", "<br>",
                                               "If the mean prevailing wage and the mean wage were equal for each job sub-category, all the bubbles would lie on the y = x line (shown on the graph). However, it is obvious from the plot that all the bubbles (different colors represent different job sub-categories, bubble size represents the number of applicants) are on the left side of y = x line. This demonstrates that all the subcategories' mean wages were higher than the prevailing wages. There is a myth that employers often hire H-1B workers for lower salaries than they would pay American workers to do the same job. Our plot does not seem to support this belief.",
                                               "<br>", "<br>",
                                               "That said, for transparency it is also important to note that during our data wrangling process, we found values in the mean wage rate column which clearly seemed like outliers. For example, there were hourly salaries of millions of dollars for some  positions and yearly salaries of less than a thousand dollars for others. While there were only a few of these observations, they heavily skewed our means. To solve the problem, we dropped observations where the mean wage rate was not within 3 standard deviations of the prevailing wage rate. In total, this eliminated only about 600 out of over 3 million observations, so it should not have a significant effect on the above graph.")),
                                    width = 12),
                                box(title = strong("Effects of Congress Increasing Minimum Salary Requirement for H-1B Applicants"),
                                    sliderInput(inputId = "minwage", 
                                                label = h4("Select minimum salary requirement:"),
                                                min = 60000,
                                                max = 130000,
                                                value = 90000,
                                                round = TRUE),
                                    plotlyOutput("quota", height = "600px"), 
                                    HTML(paste("<br>", "<br>",
                                               "In order to prevent companies from using the H-1B program as a means to hire workers for lower rates than Americans would be willing and able to do the work, Congress has set a minimum wage rate for most occupations at $60000 (there are some exceptions that we will not go into). As this figure has not been updated for more than 20 years, bills to increase it periodically emerge. Most recently, this was a topic of discussion last fall, when a bill that would increase the rate to $90000 was passed in the House Judiciary Committee (", a("link for more info", href = "http://fortune.com/2017/11/16/darrell-issa-bill-h1b-rules-passes-house-committee/"), "). With this widget, we visualize how an increased minimum base salary would impact H-1B recipients in various job subcategories.",
                                               "<br>", "<br>",
                                               "As there are some exceptions to this law, in this graph we only look at job subcategories where the mean earnings are already above $60000, as it is unclear whether there would be any affect on them should the minimum wage rate be raised. We are also only showing data-points after 2017, as using previous years’ statistics would not have taken wage growth into consideration, and could have therefore skewed our data.",
                                               "<br>", "<br>",
                                               "The y-axis represents the gross number of people who would be affected by an increase in the minimum rate (remember, it shows only applicants, and not people who got the visa) in 2017 and 2018 Financial Q1. On the x-axis we calculate the proportion of a given occupation subcategory who would be affected by the increase.")),
                                    width = 12)
                                )
                          ),
                  
                  tabItem(tabName = "nittyGritty",
                          fluidPage(
                                titlePanel("Dive into the nitty-gritty, search the full dataset"),
                                box(textInput(inputId = "employerF", label = h4("Select an Employer:")),
                                    textInput(inputId = "jobF", label = h4("Select a Job Title:")),
                                    HTML(paste("You can further filter the dataset using the boxes at the bottom of the table.")),
                                    dataTableOutput("dtFull"), 
                                    width = 12)
                                )
                          ),
                
                  tabItem(tabName = "about",
                          titlePanel("Find out more about us and the data we used"),
                          fluidPage(
                                box(HTML(paste("We got all of our data from the Office of Foreign Labor Certification, U.S. Department of Labor and it can be publicly accessed at the following ", a("link.", href = "https://www.foreignlaborcert.doleta.gov/performancedata.cfm"), 
                                               "<br>", "<br>",
                                               "For information on applicants’ birth country, we used data from US. Citizenship and Information Services (USCIS), which can be found ", a("here.", href = "https://www.uscis.gov/sites/default/files/USCIS/Resources/Reports%20and%20Studies/Immigration%20Forms%20Data/BAHA/h-1b-2007-2017-trend-tables.pdf"),
                                               "<br>", "<br>",
                                               "Before using the data in our app, we wrangled the data extensively, all of which is made public at the following", a("link.", href = "https://github.com/icsel/H1B-Project/blob/master/DataWrangling.Rmd"), "The process consisted of merging the separate years’ data into one file, selecting relevant columns for our analyses, deleting certain parts of strings to ensure consistency amongst filings, correcting mistyped SOC codes, prorating all wages to yearly levels, changing incorrect prevailing wages, removing observations that were obviously errors, and adding extra columns as string explanations to the job category previously expressed as two digits.",
                                               "<br>", "<br>",
                                               "Throughout the data manipulation process we removed a few thousand observations from the dataset, but our final dataset still consists of more than 3.2 million unique observations, spanning 2011 - 2018.",
                                               "<br>", "<br>",
                                               "We thoroughly enjoyed creating this collection of widgets, and hope you found it informative. Feel free to connect with us with any questions, yadav.rajnish13@gmail.com and icselot1@swarthmore.edu.")),
                                    width = 12)
                                )
                          )
                  )
            )
      )

server <- function(input, output) {
 
      output$mapdens <- renderLeaflet({ 
            
            ### mean and median similar -- tested
            mapH1B_edit <- reactive({
                  data %>%
                        filter(OTitle == input$occupationM) %>%
                        group_by(StateLong, OTitle) %>%
                        summarize(StateTotal = n(), 
                                  MeanWage = as.integer(mean(AVG_WAGE_RATE)), 
                                  MedianWage = as.integer(median(AVG_WAGE_RATE)))
            })
            
            # use second grouping to avoid ties
            mapH1B_edit2 <- reactive({
                  data %>%
                        filter(OTitle == input$occupationM) %>%
                        group_by(StateLong, OTitle, EMPLOYER_NAME) %>%
                        summarize(EmployerMax = n()) %>%
                        top_n(1) %>%
                        group_by(StateLong, OTitle, EmployerMax) %>%
                        slice(1L) %>%
                        mutate(EMPLOYER_NAME = str_to_title(EMPLOYER_NAME))
            })
            
            mapH1B_edit3 <- reactive({
                  data %>%
                        filter(OTitle == input$occupationM) %>%
                        group_by(StateLong, OTitle, SOC_TITLE_O) %>%
                        summarize(SOC_Max = n()) %>%
                        top_n(1) %>%
                        group_by(StateLong, OTitle, SOC_Max) %>%
                        slice(1L)
            })
            
            mapH1B_edit4 <- reactive({
                  mapH1B_edit() %>%
                        left_join(mapH1B_edit2(), by = c("StateLong", "OTitle")) %>%
                        left_join(mapH1B_edit3(), by = c("StateLong", "OTitle"))
            }) 
            
            merged_map <- reactive({merge(x = stateBoundary, y = mapH1B_edit4(), 
                                          by.x = c("NAME"), by.y = c("StateLong"), 
                                          duplicateGeoms = TRUE)}) 
            
            pal <- colorNumeric("YlOrRd", domain = NULL)
            
            leaflet(merged_map()) %>%
                  setView(-96, 37.8, 4) %>%
                  addProviderTiles("CartoDB.Positron") %>%
                  addPolygons(fillColor = ~pal(merged_map()$StateTotal),
                              weight = 2,
                              opacity = 1,
                              color = "white",
                              dashArray = "1",
                              smoothFactor = 0.3,
                              fillOpacity = 2, 
                              highlight = highlightOptions(
                                    weight = 5,
                                    color = "#666",
                                    dashArray = "",
                                    fillOpacity = 0.7,
                                    bringToFront = TRUE),
                              popup = ~paste(NAME, ":", StateTotal, "applicants", br(), 
                                             "Mean Wage: $", MeanWage, br(),
                                             "Most Popular Employer:", EMPLOYER_NAME, br(),
                                             "Most Popular Subcategory:", SOC_TITLE_O)) %>%
                  addLegend(pal = pal, values = ~merged_map()$StateTotal, opacity = 0.7, 
                            title = "Number of H1B Applicants", position = "bottomright")
        
            }) 
      
      output$salaryChange <- renderPlotly({
            
            req(input$occupationG)
            
            graphH1B <- data %>%
                  group_by(Year, OTitle) %>%
                  filter(OTitle %in% input$occupationG) %>%
                  summarize(meanWage = mean(AVG_WAGE_RATE), 
                            medianWage = median(AVG_WAGE_RATE), 
                            n = n())
  
            salaryChange <- ggplot(graphH1B, 
                                   aes(x = Year, y = meanWage, col = OTitle, group = 1,
                                       text = paste('Job Category: ', OTitle, '<br>Mean Salary ($):', 
                                                    as.integer(meanWage), '<br>No. of Applicants: ', n))) +
                  geom_line() + 
                  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) +
                  scale_y_continuous(breaks = c(25000, 50000, 75000, 100000, 125000, 150000)) +
                  labs(y = "Mean Wage") +
                  theme_minimal() +
                  scale_color_brewer(name = "Legend", palette = "Dark2")
  
            ggplotly(salaryChange, tooltip = c("text"))
  
            })
 
      output$dtFull <- renderDataTable({
            
            ifelse(nchar(input$employerF) >= 1, req(input$employerF), req(input$jobF))
            
            ifelse(nchar(input$employerF) >= 1 & nchar(input$jobF) >= 1,
                   dataFull <- data %>%
                         filter(str_detect(EMPLOYER_NAME, paste0(str_to_upper(input$employerF)))) %>%
                         filter(str_detect(JOB_TITLE, paste0(str_to_upper(input$jobF)))) %>%
                         select(EMPLOYER_NAME, JOB_TITLE, AVG_WAGE_RATE, WORKSITE_CITY, StateLong, SOC_TITLE_O, Year),
                   ifelse(nchar(input$employerF) >= 1,
                          dataFull <- data %>%
                                filter(str_detect(EMPLOYER_NAME, paste0(str_to_upper(input$employerF)))) %>%
                                select(EMPLOYER_NAME, JOB_TITLE, AVG_WAGE_RATE, WORKSITE_CITY, StateLong, SOC_TITLE_O, Year),
                          dataFull <- data %>%
                                filter(str_detect(JOB_TITLE, paste0(str_to_upper(input$jobF)))) %>%
                                select(EMPLOYER_NAME, JOB_TITLE, AVG_WAGE_RATE, WORKSITE_CITY, StateLong, SOC_TITLE_O, Year)
                          )
                   )
            
            dataFull %>%
                  mutate(AVG_WAGE_RATE = as.integer(AVG_WAGE_RATE)) %>%
                  mutate(WORKSITE_CITY = str_to_title(WORKSITE_CITY)) %>%
                  dplyr::rename("Employer Name" = EMPLOYER_NAME, 
                                "Job Title" = JOB_TITLE, 
                                "Wage Rate" = AVG_WAGE_RATE, 
                                "City" = WORKSITE_CITY,
                                "State" = StateLong,
                                "Job Sub-Category" = SOC_TITLE_O)
            
            })
  
      output$salaryComp <- renderPlotly({
            
            mapH1B_summary <- data %>%
                  group_by(OTitle, SOC_TITLE_O) %>%
                  summarize(MPW = mean(PREVAILING_WAGE), MAW = mean(AVG_WAGE_RATE), n = n()) %>%
                  filter(n >= 100)
            
            ggplotly(ggplot(data = mapH1B_summary, 
                            aes(x = MPW, y = MAW, size = n, col = OTitle, alpha = 0.8,
                                text = paste('Job Sub-category: ', SOC_TITLE_O, '<br>Mean Wage ($):', 
                                             as.integer(MAW), '<br>Prevailing Wage ($):', as.integer(MPW), '<br>No. of Applicants: ', n))) +
                           geom_point() +
                           geom_abline(slope = 1, intercept = 0) +
                           theme_minimal() +
                           labs(x = "Mean Prevailing Wage ($)", y = "Mean Wage ($)", col = "Job Categories"),
                     tooltip = c("text"))
            
            })
  
      output$quota <- renderPlotly({
        
            eligibleQ <- data %>%
                  filter(Year >= 2017) %>%
                  filter(VISA_CLASS == "H-1B") %>%
                  filter(AVG_WAGE_RATE >= 60000) 
      
            totalQ <- eligibleQ %>%
                  group_by(OTitle, SOC_TITLE_O) %>%
                  summarize(Totaln = n())
        
            affectedQ <- eligibleQ %>%      
                  filter(AVG_WAGE_RATE <= input$minwage) %>%
                  group_by(OTitle, SOC_TITLE_O) %>%
                  summarize(Affectedn = n(), wage = mean(AVG_WAGE_RATE)) %>%
                  filter(Affectedn >= 100)
        
            mergedQ <- inner_join(totalQ, affectedQ, by = c("OTitle", "SOC_TITLE_O")) %>%
                  mutate(PropAffected = 100*(Affectedn / Totaln))
        
            ggplotly(ggplot(data = mergedQ, 
                            aes(x = PropAffected, y = Affectedn,
                                text = paste(SOC_TITLE_O, "<br>Number affected:", Affectedn, "<br>Proportion affected (%):", as.integer(PropAffected)), 
                                col = OTitle, alpha = 0.8)) +
                           geom_point() +
                           theme_minimal() +
                           labs(x = "Proportion Affected (%)", y = "Number Affected") +
                           scale_color_discrete(name = "Occupation Group"),

                     tooltip = c("text")
                     )
            
            })
  
      output$topten <- renderPlotly({
        
            top_10Employer <- data %>%
                  filter(VISA_CLASS == "H-1B") %>%
                  group_by(EMPLOYER_NAME) %>%
                  summarise(n = n()) %>%
                  top_n(10) %>%
                  arrange(desc(n)) %>%
                  mutate(EMPLOYER_NAME = str_to_title(EMPLOYER_NAME))
        
            employerplot <- ggplot(data = top_10Employer, 
                                   aes(x = reorder(EMPLOYER_NAME, n), y = n/1000,
                                       text = paste("Number (k):", as.integer(n/1000)))) +  
                  geom_bar(stat = "identity", fill = "blue", colour = "black") +
                  theme_minimal(base_size = 10)  +
                  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
                  labs(x ="Top 10 Employers", y = "Number of Applicants (thousands)")
        
            ggplotly(employerplot, tooltip = c("text"))
        
            })
  
      output$origins <-renderPlotly({

            ggplotly(ggplot(data = origins, 
                            aes(x = reorder(COUNTRIES, APPLICANTS), y = APPLICANTS/1000,
                                text = paste("Gross Number (k):", as.integer(APPLICANTS/1000),
                                             "<br>Proportion of Total (%):", as.integer(100*(APPLICANTS/sum(APPLICANTS)))))) +
                           geom_bar(stat = "identity", fill = "orange", colour = "black") +
                           theme_minimal(base_size = 10) +
                           theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
                           labs(x = "Top 10 Country of Births", y = "Number of Applicants (thousands)"),
                  tooltip = c("text")
                  )
            
      })
      
      }

shinyApp(ui, server)
