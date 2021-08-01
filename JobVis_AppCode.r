library(shiny)
library(shiny)
library(plotly)
library(xml2)
library(gapminder)
library(readxl)
library(tidyverse)
library(tidytext)
library(scales)
library(stopwords)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(highcharter)
library(countrycode)
library(dashboardthemes)
library(usmap)
library(openintro)
library(highcharter)
library(treemap)
library(fmsb)
library(networkD3)
library(reshape2)
library(wordcloud)
library(textstem)
library(igraph)
library(ggraph)
library(widyr)
library(shinyswipr)
library(shinysense)
library(shinydashboardPlus)
library(magrittr)
library(shinyhelper)
#devtools::install_github("nstrayer/shinyswipr")
#devtools::install_github("nstrayer/shinysense")

#------------------------------------------------------------------------------------------------------------------------------------

#Read in the data
industry <- read_xlsx('Indeed.xlsx',sheet="Industry")
google <- read_xlsx('Indeed.xlsx',sheet='Google Skills (Kaggle)')
review <- read_xlsx('Indeed.xlsx',sheet='Google Review')
world_long_lat <- read_csv('world_long_lat.csv')

#Cleaning the data 
#Industry Dataset
industry$Description <- industry$Description %>% gsub('\r\n',' ',.)
industry$Date <- industry$Date %>% gsub(' days ago| day ago','',.) %>% gsub('Today|Just posted','0',.)
industry$Location <- industry$Location %>% gsub('[0-9]+.','',.) %>% gsub('\\(.\\)','',.) %>% gsub('United States','Remote',.) %>% gsub('New York State','New York, NY',.) 
industry$Title <- industry$Title %>% gsub('Sr.|Sr','Senior ',.) %>% gsub('Jr.|Jr','Junior ',.) %>% gsub('\\*$',' ',.) %>% gsub("DATA SCIENTIST","Data Scientist",.) %>% gsub("Data Scientist 1","Data Scientist I",.) %>% gsub("Data Scientist 2","Data Scientist II",.)
industry$Industry <- industry$Industry %>% gsub('&','and',.) %>% gsub("Computer and Electronics$","Computers and Electronics",.) %>% gsub("Agriculture$","Agriculture and Extraction",.) %>% gsub('Restaurants and Food Service$','Restaurants and Food Services',.)
industry <- transform(industry, Overall_Review = as.numeric(Overall_Review), Happiness_Review = as.numeric(Happiness_Review), Work_Life_Balance = as.numeric(Work_Life_Balance), Pay_Benefits = as.numeric(Pay_Benefits), Job_Security_and_Advancement = as.numeric(Job_Security_and_Advancement), Management= as.numeric(Management),Culture = as.numeric(Culture), Woman_Rating = as.numeric(Woman_Rating))
# Clean the location col
industry$State_ID <- str_split_fixed(industry$Location, ",", 2)[,2]
industry$State_ID <- trimws(industry$State_ID)
industry$State_ID <- substr(industry$State_ID, start = 1, stop = 2)
industry$State_Name <- abbr2state(industry$State_ID) 
# Add Average salary col
industry$Salary_Avg <- (industry$Salary_Max+industry$Salary_Min)/2

industries <- sort(unique(industry$Industry))

industries <- sort(unique(industry$Industry))

#Google Dataset
google$Country <- gsub('.*,\\s*', '', google$Location)
google <- google %>% mutate(Country=case_when(Country=='Dubai - United Arab Emirates'~'United Arab Emirates',Country=='Czechia'~'Czech Republic',Country=='USA'~'United States',TRUE~Country))
job_vacancies <- google%>% group_by(Country) %>%summarise(Num_Vacancy=n())
colnames(world_long_lat)[4] <- "Country"
job_vacancies_complete <- left_join(job_vacancies,world_long_lat, by="Country")
job_vacancies_complete <- job_vacancies_complete[-48,]
job_vacancies_complete$VacanciesCount <- job_vacancies_complete$Num_Vacancy

#Google Review Dataset
review$Review_Location <- review$Review_Location %>% gsub('.*\\d+$','',.) %>% gsub('Work from home|At Home','Remote',.) %>% gsub('2563 S 88th ','',.)
review$Review[26] <- 'I worked there through an agency. The company is very good. Google is one of the best companies in its work environment.'
review$Review[29] <- 'The team is incredible, the leaders and the daily motivation is excellent.'
review$Review[150] <- 'It is a place of constant learning and development.'
review$Review[154] <- 'Considered the best place in the world to start working.'

# salary data
salary_data <- industry %>% drop_na(Salary_Min)

# Ratings data
happiness_complete <-  industry[complete.cases(industry$Happiness_Review),]
happiness_count1 <- happiness_complete %>% select(Industry, Happiness_Review)
happiness_count1$scaled  <- rescale(happiness_count1$Happiness_Review, to=c(0,100))
happiness_count1$happiness_labels <- cut(happiness_count1$scaled, breaks = c(0,20,40,60,80,100),labels = c('Very Low','Low','Normal','High', 'Very High'),include.lowest=TRUE)
happiness_count2 <- happiness_count1 %>% group_by(Industry, happiness_labels) %>% tally() %>% na.omit(cols = "Industry")

#Stopwords
stopwords <- stopwords::stopwords(language='english',source='stopwords-iso') %>% c(.,"it's","i've","they're","you'll",'u.s.','u.s','job','company','companies','cloud','lot','feel','sap') %>% as.data.frame() %>% rename(.,'word'='.') 

# Create Industry Lists
Industry_list <- list(unique(industry$Industry))[[1]]
Industry_list <- Industry_list[!is.na(Industry_list)]
Industry_list <- sort(Industry_list)
Industry_list_with_All <-append(Industry_list,"All", after=0)
Industry_list_with_NA <- append(Industry_list,"NA", after=0)
Industry_list_with_All
Industry_list_with_NA
# Create Company Lists
Company_list <- list(unique(industry$Company))[[1]]
Company_list <- Company_list[!is.na(Company_list)]
Company_list <- sort(Company_list)
Company_list_with_NA <- append(Company_list,"NA", after=0)


#------------------------------------------------------------------------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- 
    dashboardPage(
    dashboardHeader(title='JobVis'),
    dashboardSidebar(sidebarMenu(
        
        menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
        
        menuItem("User Profile", tabName = "User", icon = icon("address-card")),
        
        menuItem("Jobs", tabName = "Jobs", icon = icon("briefcase"),
                 menuSubItem('Jobs by Industry', tabName = "Jobs_by_Industry", icon = icon('industry')),
                 menuSubItem('Jobs by Company', tabName = "Jobs_by_Company", icon = icon('building'))),
        
        menuItem("Salary", tabName = "Salary", icon = icon("briefcase"),
                 menuSubItem('Salary by Industry', tabName = "Salary_by_Industry", icon = icon('industry')),
                 menuSubItem('Salary by Company', tabName = "Salary_by_Company", icon = icon('building'))),
        
        menuItem("Ratings", tabName = "Ratings", icon = icon("briefcase"),
                 menuSubItem('Ratings by Industry', tabName = "Ratings_by_Industry", icon = icon('industry')),
                 menuSubItem('Ratings by Company', tabName = "Ratings_by_Company", icon = icon('building'))),
        
        menuItem("Skills", tabName = "Skills", icon = icon("code-branch")),
        
        menuItem("Reviews", tabName = "Reviews", icon = icon("comments")),
        menuItem("More information", tabName = "More_info", icon = icon("info-circle"))
        #menuItem("Other Plans", tabName = "Other Plans", icon = icon("address-card")),
        #menuItem("Other Plans", tabName = "Other Plans", icon = icon("address-card")),
           )),
    dashboardBody(shinyDashboardThemes(
        theme = "flat_red"),
        chooseSliderSkin("Flat", color = "#112446"),
        tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
        ")),
        tabItems(
            #Dashboard Content
            tabItem(tabName = "Dashboard",
                    tags$strong(tags$h1("JobVis: For a Smarter Job Search",style = "color:black;")),
                    tags$h4("This dashboard will give you an overview of job vacancies across a wide range of industries and companies around the world."),
                    valueBoxOutput("valuebox_vacancies",width=3),
                    valueBoxOutput("valuebox_companies",width=3),
                    valueBoxOutput("valuebox_countries",width=3),
                    valueBoxOutput("valuebox_industries",width=3),
                    
                    fluidRow(column(width=8,
                                    plotlyOutput('job_posting'),height="400px"),
                             box(title="My Favorite Shortcuts",status='primary',width=4,color='red'),
                             infoBoxOutput("out_salary"),
                             infoBoxOutput("out_requirements"),
                             infoBoxOutput("out_jobfilter")),
                    fluidRow(gradientBox(title="Filter Options",icon = "fa fa-th",
                                         boxToolSize = "sm", width=12,solidHeader=T,status='info',
                                         fluidRow(
                                             column(4,selectInput('filter_location','Location: ',choices=c("All Locations",unique(industry$State_Name,na.rm=TRUE)),selected ="All Locations",multiple=T)),
                                             column(4,selectInput('filter_company','Company: ',choices=c("All Companies",unique(industry$Company, na.rm=TRUE)),selected="All Companies",multiple=T)),
                                             column(4,selectInput('filter_industry','Industry: ',choices=c("All Industries",unique(industry$Industry,na.rm=TRUE)),selected="All Industries",multiple=T))),
                                         fluidRow(
                                             column(6,sliderInput('filter_salary','Salary Range: ',min=0,max=ceiling(max(industry$Salary_Max, na.rm = TRUE)),value=c(700,8000))),
                                             column(6,sliderInput('filter_rating','Overall Ratings more than: ',min=0,max=5,value=1)),
                                             tableOutput("job_summary_table"))))),
            
        #User Profile Content
        tabItem(tabName = "User",
                h2("User Profile"),
                tags$h4("View your job application progress."),
                widgetUserBox(width=12,
                    title = "Jane Doe",
                    subtitle = "Business Analytics Student at NUS Business School",
                    type = 2,
                    src = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
                    color = "yellow",
                    "About Me",
                    footer = "I am in my sophomore year of university and I aspire to be a business analyst. I did a summer stint as a data scientist intern at Google and I am currently looking out for job opportunities within the tech industry."
                ),
                fluidRow(
                    box(width = 12,
                        title = "User Progress",
                        color = "green", ribbon = TRUE, title_side = "top right",
                        column(width = 5),
                        sankeyNetworkOutput('UserProgress') %>%
                            helper(type = "inline",
                                   title = "Sankey Network",
                                   content = "Each node represents the actions of either the user of the company and the number on the link represents the
                                   value of the interaction",
                                   size = "s"),
                    ))
        ),
       
        #Jobs Content
        
        #Jobs by Industry 
        tabItem(tabName = "Jobs_by_Industry",
                h2("Jobs by Industry"),
                tags$h4("Compare Job vacancies in the US by Industry"),
                fluidRow(
                box(title='Industry of Interest',width=12,status='warning',
                    selectInput('usmap_ind','Choose an industry:',Industry_list_with_All)),
                box(title='Map of Job Vacancies', width=12,status='warning',solidHeader = TRUE, 
                    uiOutput('heatmapUI')%>%
                        helper(type = "inline",
                               title = "HeatMap of the Job vacancies by state",
                               content = "The color density represents the number of job vacancies across the different states by Industry",
                               size = "s"))),
                 
        ),
        
        # JObs by company
        tabItem(tabName = "Jobs_by_Company",
                h2("Jobs by Company"),
                tags$h4("Compare job vacancies across different countries in the world for a company."),
                fluidRow(
                box(width=12,status='danger',
                    selectInput('pickCompany','Choose a company: ',choices=c("Google"),selected="Google")),
                
                box(title='Map of Job Vacancies',width=6,status='warning',solidHeader=TRUE,
                    selectInput('country','Select Country: ', choices=c("All Countries",unique(job_vacancies_complete$Country))),
                    sliderInput('vacancies','Vacancies more than: ',min=1,max=88,value=1),
                    hr(style = 'border-color:black;'),
                    highchartOutput('plotChoroplethMap',height='400px')%>%
                        helper(type = "inline",
                               title = "Choropleth Map of Job Vacancies around the World",
                               content = "The color density represents the nunber of vacancies across the different countries in the world for a company. Hover over the map to see more details.",
                               size = "s")),
                box(title='Filtering Jobs by Location or Category',width=6,status='warning',solidHeader=TRUE,
                    selectInput('continent', 'Choose a continent: ', 
                                choices = c("Asia","Africa",
                                            "Europe","Oceania")),
                    selectInput('filter_type','Choose a filter type: ',choices=c("Country","Category")),
                    plotlyOutput('plotSunburst',height='450px')%>% helper(type = "inline",
                                                                          title = "Sunburst Chart of Job Vacancies by Country or Category",
                                                                          content = "Hover over each sunburst chart to view more details on each vacancy. Click over each slice of the chart to expand and enlarge the view of the vacancies for that particular country/category. ",
                                                                          size = "s")))),
        
        
    
        
        #Salary Content
        ## Salary by Industry
        tabItem(tabName = "Salary_by_Industry",
                h2("Salary and Vacancies by Industry"),
                tags$h4("Compare the average salaries and Job vacancies of different companies based on the industry."),
                fluidRow(
                box(title='Industry of Interest',width=12,status='warning',
                    selectInput('salary_tree_ind','Industry:',Industry_list_with_All)),
                box(title='Salary and Vacancies', width=12,status='warning',solidHeader = TRUE, 
                    highchartOutput("SalaryVacancyTree", height = "600px")%>%
                        helper(type = "inline",
                               title = "Salary and Vacancies",
                               content = "The Size of the box represents the number of vacancies of the company while the color intensity represents the average salary",
                               size = "s"))),
                
        ),

                
        ## Salary by Company
        tabItem(tabName = "Salary_by_Company",
                h2("Compare Salaries across Companies"),
                tags$h4("Compare the salary distribution of the different companies."),
                fluidRow(
                    box(width = 12,
                        title = "Salary",
                        color = "green", ribbon = TRUE, title_side = "top right",
                        column(width = 5),
                        plotlyOutput("salaryfig"),
                        selectizeInput('SalaryCompany','Select Companies:',c(sort(unique(salary_data$Company))),multiple = T, options = list(maxItems = 5, placeholder = "Select up to 5 companies"))
                    ))),
        
        #Ratings Content
        
        ## Ratings by Industry
        tabItem(tabName = "Ratings_by_Industry",
                h2("Compare Ratings across Industries"),
                tags$h4("Compare the ratings of the different industries."),
                box(title ='Type of Comparison', width=6,status='warning',
                    radioButtons('radar_ind_type','Type:',c("Industry_Average","Personal_Score"))%>%
                        helper(type = "inline",
                               title = "Select Type of Comparison",
                               content = "Choose to compare the ratings of the industries against the user score or the industry average",
                               size = "s")
                ),
                box(title = "Industries of Interest",width=6, status = 'warning',
                    selectizeInput('radarind','Select Industries:', Industry_list ,multiple = T, options = list(maxItems = 4, placeholder = "Select up to 4 Industries"))),
                box(title='Ratings', width=12,status='warning',solidHeader = TRUE, 
                    plotOutput("Radar_Ind", height = "550px")%>%
                        helper(type = "inline",
                               title = "Ratings",
                               content = "The red line represents the scores of the Industries while the shaded area represents the comparion type (either user score or industry average)",
                               size = "s")),
                fluidRow(
                    box(title = "Happiness ratings",width = 12,
                        plotlyOutput("happiness_fig"),
                        selectizeInput('HappinessIndustry','Select Industries:',c(sort(unique(happiness_count2$Industry))),multiple = T, options = list(maxItems = 5, placeholder = "Select up to 5 industries"))
                    )),
                    
                    
                ),
        ## Ratings by Company
        
        tabItem(tabName = "Ratings_by_Company",
                h2("Compare Ratings across Companies"),
                tags$h4("Compare the ratings of the different companies."),
                fluidRow(
                box(title ='Type of Comparison',width=6, status='warning', 
                    radioButtons('radar_comp_type','Type:',c("Companies_Average","Personal_Score"))%>%
                        helper(type = "inline",
                               title = "Select Type of Comparison",
                               content = "Choose to compare the ratings of the Companies against the user score or the Company average",
                               size = "s")
                ),
                box(title = "Companies of Interest", width=6,status = 'warning',
                    selectizeInput('radarcomp','Select Companies:', Company_list ,multiple = T, options = list(maxItems = 4, placeholder = "Select up to 4 Companies"))),
                box(title='Ratings', width=12, status='warning',solidHeader = TRUE, 
                    plotOutput("Radar_Comp", height = "450px")%>%
                        helper(type = "inline",
                               title = "Ratings",
                               content = "The red line represents the scores of the Companies while the shaded area represents the comparion type (either user score or industry average)",
                               size = "s")),
                    
                )),
        
        #Skills Content
        tabItem(tabName = "Skills",
                h2("Check Skills Required"),
                tags$h4("Check what are the technical and people skills required at the different companies."),
                fluidRow(
                    
                    box(title='Check Degree Requirements by Industry', status='warning',solidHeader = TRUE, plotlyOutput('degree_ind'),
                        radioButtons('industry_type','Select Type',c('Degrees','Subjects')),
                        selectInput('industry_degree','Select Industry',unique(industry$Industry))),
                    
                    
                    box(title='Check Degree Requirements by Company', status='danger',solidHeader = TRUE, plotlyOutput('degree_comp'),
                        radioButtons('company_type','Select Type',c('Degrees','Subjects')),
                        selectInput('company_degree','Select Company',unique(industry$Company))),
                    
                    
                    box(title='Check Industry Level Skills', status='primary',solidHeader = TRUE, plotlyOutput('Skills_ind'),
                        selectInput('industry_skill','Select Industry',unique(industry$Industry)),
                        sliderInput('industry_skill_freq','Select Number of Words',1,10,5)),
                    
                    box(title='Check Company Level Skills', status='danger',solidHeader = TRUE, plotlyOutput('Skills_comp'),
                        selectInput('company_skill','Select Company',unique(industry$Company)),
                        sliderInput('company_skill_freq','Select Number of Words',1,10,5))
                    
                )),
        
       
        #Reviews Content
        tabItem(tabName = "Reviews",
                h2("Check Company Reviews"),
                tags$h4("Sentiment Analysis of the reviews of Google"),
                
                fluidRow(
                    box(title="Most common positive and negative by Reviewer's Location",plotOutput('location_sentiment'),
                        selectInput('reviews_location','Company:',unique(review$Review_Location))),
                    
                    box(title="Most common positive and negative by Reviewer's Position",plotOutput('position_sentiment'),
                        selectInput('reviews_position','Company:',unique(review$Review_Author))),
                    
                    box(title='See most frequent words',width=6, status='primary',solidHeader = TRUE, plotOutput('wordcloud'),
                        selectInput('reviews','Company:','Google'),
                        radioButtons('rating','Rating:',c('1.0','2.0','3.0','4.0','5.0'),inline = TRUE),
                        sliderInput('word_freq','Number of Words:',5,100,60)),
                    
                    box(title='Company Emotional Sentiment Analysis',width=6, status='primary',solidHeader = TRUE, plotlyOutput('emotion_sentiment')))
        ),
        
        # More info content
        tabItem(
            tabName = "More_info", 
            
            navbarPage("More information",
                       
                       
                       tabPanel("About the application",
                                mainPanel(
                                    
                                    div(class="panel panel-default",
                                        div(class="panel-body",  
                                            fluidRow(
                                                shiny::HTML("<br><br><center> 
                                            <h1>More Information</h1> 
                                            </center>
                                            <br>
                                            <br>"),
                                                style = "height:100px;"),
                                            
                                            fluidRow(
                                                
                                                column(12,
                                                       tags$ul(align = "Center",
                                                               tags$span(h4("JobVis is designed to allow job seekers to build interactive visualizations and make useful comparisons between companies and industries based on job characteristics they value the most. Through these features, our app aims to provide deeper insights on an industry and company level so that users can assess whether they are aligned with the company or industry’s requirements and if they fit in with the company/industry culture.")), 
                                                               tags$span(h4("Motivation for this project stemmed from our observations that career portals very rarely provided a holistic view of culture, ratings, skills required (etc), at either an industry level or company level. Instead users would have to manually check each job posting to form an understanding of each characteristic. Additionally, we found that many lacked the nuances of allowing users to make tailored comparisons, and if they did it would be in a tabular format (example: Glassdoors Salary). Another major flaw was that portals did not display visualizations that allow users to keep track of their job applications and success rates. As such, JobVis addresses these pain points through graphical representations, filter functions and an improved UI layout that simplifies and adds value to the job searching experience.")), 
                                                               tags$span(h4("To note, we believe this app is particularly important today since Covid-19 has severely impacted the job market around the world, and has left job seekers questioning which industries and companies are still hiring, what skills they should upgrade to improve chances of landing jobs, and what industry/company best suits their requirements. Moreover, we see that companies are switching to utilizing AI to select candidates, thus it has become imperative for job seekers to determine the keywords that companies are searching for in each job posting and address them in either their resumes or in their job interviews."))
                                                       )
                                                )))))
                                
                       ),
                       tabPanel("About the Creators",
                                mainPanel(
                                    
                                    div(class="panel panel-default",
                                        div(class="panel-body",  
                                            fluidRow(
                                                shiny::HTML("<br><br><center> 
                                            <h1>About the Creators</h1> 
                                            </center>
                                            <br>
                                            <br>"),
                                                style = "height:100px;"),
                                            
                                            fluidRow(
                                                
                                                column(12,
                                                       tags$ul(align = "Center",
                                                               tags$span(h4("This app is created by Group 10 of DBA3702 Class SA1. The members include Chloe, Jovita, Saloni and Ying Jie."))
                                                       )))))
                                )),
                       tabPanel("What makes us unique",
                                mainPanel(
                                    fluidRow(
                                        column(6),
                                        column(12,
                                               # Panel for Background on Data
                                               div(class="panel panel-default",
                                                   div(class="panel-body",  
                                                       tags$div( align = "center",
                                                                 div( align = "center", 
                                                                      h4("What makes us unique?")
                                                                 )
                                                       ),
                                                       tags$p(h6("JobVis has three main categories of features that sets us apart from conventional job portals such as Symplicity, Indeed and Glassdoors. These features directly target pain points that we found job seekers find most frustrating. Thus, our unique selling points are as listed below:")),
                                                   ))
                                        )),
                                    
                                    
                                    fluidRow(
                                        box(title = "1. Fitting into the culture", width = 12, collapsible = T,collapsed = TRUE,
                                            box(tags$span(h6("Understanding the company/industry culture in terms of its overall review, happiness review, work-life balance, pay benefits, job security and advancement, etc, is pivotal in determining the user’s fit in the company/industry. Most job portals place very little importance on this or simply provide the statistics without any option to compare.  ", align = "left"))), 
                                            
                                            fluidRow(
                                                box(title = "Radar charts with comparing capabilities",collapsible = T, collapsed = T, tags$ul(
                                                    tags$span(h6("This feature creates radar charts that allow users to see how an industry perform against other industries in terms of several rating metrics. There are seven ratings altogether and they are (1) Overall Review, (2) Work Life Balance, (3) Pay Benefits, (4) Job Security and Advancement/Security , (5) Management, (6) Culture and (7) Woman Ratings.", align = "left")), 
                                                    
                                                )),
                                                box(title = "Reviews Analysis",collapsible = T, collapsed = T, tags$ul(
                                                    tags$span(h6("
Develop a comprehensive understanding of what employees are saying about working at the company! The word cloud represents the top positive and negative words users have mentioned, while the sentiment analysis graphs show the frequency of the different emotional words that employees have shown in their reviews", align = "left")), 
                                                    
                                                )))
                                        )),
                                    fluidRow(
                                        box(title = "2. Discovering skills requirement", width = 12, collapsible = T,collapsed = TRUE,
                                            
                                            fluidRow(
                                                box(title = "Degree requirements",collapsible = T, collapsed = T, tags$ul(
                                                    tags$span(h6("[Currently only available for Google] This feature allows users to determine the level of education the company is looking for for each of the role categories", align = "left")), 
                                                    
                                                )),
                                                box(title = "People skills requirements",collapsible = T, collapsed = T, tags$ul(
                                                    tags$span(h6("Determine what People skills (such as Decision Making, Communication, etc) companies are frequently mentioning in their job listings. For Google, the feature will allow users to further determine the skills required for each role category", align = "left")), 
                                                    
                                                )),
                                                box(title = "Technical skills requirements",collapsible = T, collapsed = T, tags$ul(
                                                    tags$span(h6("Determine what Technical skills (such as Machine Learning, Analytics, etc) companies are frequently mentioning in their job listings. For Google, the feature will allow users to further determine the skills required for each role category", align = "left")), 
                                                    
                                                )))
                                            
                                        )),
                                    fluidRow(
                                        box(title = "3. Discover trends in Demand and Compare Salaries", width = 12, collapsible = T,collapsed = TRUE,
                                            box(title = "Heat Maps to check the hottest hiring spots",collapsible = T, collapsed = T, tags$ul(
                                                tags$span(h6("This feature provides a visualization that shows the number of job vacancies across the different US states across different industries", align = "left")), 
                                                
                                            )),
                                            box(title = "Compare Salaries ",collapsible = T, collapsed = T, tags$ul(
                                                tags$span(h6("The dumbbell chart allows users to select five preferred companies to compare maximum and minimum salaries of each job role within the chosen companies and get a gauge of the range of prospective jobs they may be interested in. ", align = "left")), 
                                                
                                            ))
                                            
                                        )),
                                    fluidRow(
                                        box(title = "4. Track your Application Progress", width = 12, collapsible = T,collapsed = TRUE,
                                            tags$ul(
                                                tags$span(h4("Sankey Graph", align = "left")), 
                                                tags$span(h6("The Sankey Network helps the user track his jobs application progress. The nodes represent the actions of both the user and the company while the size of the link represents the number of actions linking each node (For example: Jobs applied to -> No replies 50 means that 50 of the companies the user applied to did not give a reply to the user)"))
                                                
                                            )
                                        )),
                                    fluidRow(
                                        box(title = "5. Other Considerations", width = 12, collapsible = T,collapsed = TRUE,
                                            tags$ul(
                                                tags$span(h4("CV Scanner", align = "left")), 
                                                tags$span(h6("The application will extract information on the skill sets and experiences listed in the uploaded CV and make comparisons against those required by the companies. We can use shinysense feature (that includes using camera) to add this feature into our app."))
                                                
                                            )
                                        ))
                                    
                                ),
                               )
            )
        )
        )))
#--------------------------------------------------------------------------------------------------------------------------------   
                
server <- function(input, output, session) {
    observe_helpers(withMathJax = TRUE)
    # Popup
    
    Intro_modal <- modalDialog(
        title = "Welcome to JobVis!", 
        HTML("JobVis is a job application that will allow you to make a well rounded and educated decision on which jobs you
        want to pursue by curating and visualizing important information about the various job listings on the market. <br> <br> 
        "),
        easyClose = T,
        size =  "m",
        footer = list(modalButton("Close")),
        
    )
    #Modal_2 <- modalDialog(
    #    title = "Title 2",
    #    "Message 2",
    #    easyClose = T,
    #    footer = list(modalButton("Close")),
    #)
    
    # Show the modal on start up
    showModal(Intro_modal)
    
    #Dashboard Content 
    output$valuebox_vacancies <- renderValueBox({
        valueBox(
            formatC(nrow(industry), format="d", big.mark=',')
            ,"job vacancies available"
            ,icon = icon("search")
            ,color = "red")  
    })
    
    output$valuebox_companies <- renderValueBox({
        valueBox(
            formatC(length(Company_list), format="d", big.mark=',')
            ,"companies to apply for"
            ,icon = icon("briefcase")
            ,color = "purple")  
    })
    
    output$valuebox_countries <- renderValueBox({
        valueBox(
            formatC(nrow(job_vacancies), format="d", big.mark=',')
            ,"states to choose from"
            ,icon = icon("globe")
            ,color = "yellow")  
    })
    
    output$valuebox_industries <- renderValueBox({
        valueBox(
            formatC(length(industries), format="d", big.mark=',')
            ,"industries to work at"
            ,icon = icon("stats",lib='glyphicon')
            ,color = "aqua")  
    })
    
    
    card_swipe <- callModule(shinyswipr, "my_swiper")
    sample_data <- industry[sample(nrow(industry), 1), ]
    output$job_title <- renderText({ sample_data$Title })
    output$company_name <- renderText({ sample_data$Company })
    output$job_industry <- renderText({ sample_data$Industry })
    output$job_location <- renderText({ sample_data$Location})
    
    # Render the table of swipes
    output$resultsTable <- renderDataTable({ appVals$swipes })
    
    # Set up reactive values object
    appVals <- reactiveValues(
        swipes = data.frame(
            job_title = character(), company_name = character(), job_industry=character(), job_location=character(),swipe = character()
        )
    )
    
    observeEvent(card_swipe(), {
        
        # Record the last swipe result
        latest_result <- data.frame(
            job_title = appVals$sample_data$Title,
            company_name = appVals$sample_data$Company,
            job_industry = appVals$sample_data$Industry,
            job_location = appVals$sample_data$Location,
            swipe = card_swipe())
        
        # Add to table of all swipe results
        appVals$swipes <- rbind(latest_result, appVals$swipes)
        
        # Send results to the output
        output$resultsTable <- renderTable({ appVals$swipes })
        
        # Update the villager
        appVals$sample_data <- sample_n(industry, 1)
        
        # Send update to ui
        output$job_title <- renderText({ appVals$sample_data$Title })
        output$company_name <- renderText({ appVals$sample_data$Company })
        output$job_industry <- renderText({ appVals$sample_data$Industry})
        output$job_location <- renderText({ appVals$sample_data$Location})
        
        the_data <- eventReactive(
            { card_swipe() }
        )
        
        output$table <- renderTable(the_data())
        
    }) 
    
    #For the filtered table
    subset_summary <- industry%>%select(Title,State_Name,Company,Industry,Salary_Min,Salary_Max,Overall_Review)
    
    filtered_location <- reactive({
        if(input$filter_location=="All Locations"){
            subset_summary
        }
        else{subset_summary%>%filter(State_Name == input$filter_location)}
    })
    
    filtered_company <- reactive({
        if(input$filter_company=="All Companies"){
            filtered_location()
        }
        else{filtered_location()%>%filter(Company == input$filter_company)}
    })
    
    filtered_industry <- reactive({
        if(input$filter_industry=="All Industries"){
            filtered_company()
        }
        else{filtered_company()%>%filter(Industry == input$filter_industry)}
    })
    
    filtered_salary_review <- reactive({
        filtered_industry()%>%filter(Overall_Review>=input$filter_rating & Salary_Min >= input$filter_salary[1])
    })
    
    
    output$job_summary_table <- renderTable({
        filtered_salary_review()
    })
    
    #Info Boxes at the top of dashboard
    output$out_salary <- renderInfoBox({
        infoBox("Click to open tab",  
                a("Salary Comparison Tool", onclick = "openTab('Salary_by_Company')", href="#"),
                icon = icon('usd', lib = 'glyphicon'), color = "green"
        )
    })
    
    output$out_requirements <- renderInfoBox({
        infoBox("Click to open tab",  
                a("Skills Requirement Checker", onclick = "openTab('Skills')", href="#"),
                icon = icon('check', lib = 'glyphicon'), color = "green"
        )
    })
    
    output$out_jobfilter <- renderInfoBox({
        infoBox("Click to open tab",  
                a("Job Filter Toolkit", onclick = "openTab('Jobs_by_Company')", href="#"),
                icon = icon('filter', lib = 'glyphicon'), color = "green"
        )
    })
    
    
    date_posted <- industry %>% group_by(Date)%>%summarise(Days_Ago=n())
    date_posted$Date <- as.numeric(date_posted$Date)
    date_posted <- date_posted[complete.cases(date_posted$Date),]
    date_posted <- date_posted %>% arrange(desc(Date))
    output$job_posting <-renderPlotly({plot_ly(
        x = date_posted$Date,
        y = date_posted$Days_Ago, text=date_posted$Days_Ago,textposition='auto',
        type = "scatter", mode='lines+markers', marker = list(color = 'rgb(158,202,225)',
                                                              line = list(color = 'rgb(8,48,107)')))%>% layout(title= "Recent Job Posting Activity Rate",
                                                                                                               yaxis=list(title="No. of Job Postings"),
                                                                                                               xaxis=list(title="Date Posted (Days Ago)",autorange="reversed"))
    })
        
    
    #User's Profile Content
    # Sankey graph
    
    links <- data.frame(
        source=c("Jobs_applied_to","Jobs_applied_to", "Job_applied_to", "Replies", "Replies", "Initial Interview",
                 "Initial Interview","Initial Interview", "Initial Interview", "Offer Received", "Offer Received", "Final Interview",
                 "Final Interview","Final Interview", "Offer Received ", "Offer Received "), 
        target=c("Rejected by Company","No replies", "Replies", "Rejected by Company ", "Initial Interview", "Rejected by Company  ",
                 "No Followup", "Offer Received", "Final Interview", "Applicant Accepts", "Applicant Rejects", "Rejected by Company   ",
                 "No Followup " , "Offer received ", "Applicant Accepts ", "Applicant Rejects "),
        value=c(25 ,50 ,25 ,6 ,19 ,10 ,2 ,2 ,5 ,0 ,2, 2, 1, 2, 1, 1 )
    )
    
    
    # From these flows we need to create a node data frame: it lists every entities involved in the flow
    nodes <- data.frame(
        name=c(as.character(links$source), 
               as.character(links$target)) %>% unique()
    )
    
    # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
    links$IDsource <- match(links$source, nodes$name)-1 
    links$IDtarget <- match(links$target, nodes$name)-1
    
    # Make the Network
    sankey_plot <- sankeyNetwork(Links = links, Nodes = nodes,
                                 Source = "IDsource", Target = "IDtarget",
                                 Value = "value", NodeID = "name", 
                                 sinksRight=FALSE, fontSize = 12)
    output$UserProgress <- renderSankeyNetwork({sankey_plot})
   
    #Jobs Content
    #Jobs by Industry Content
    ## Create a summary grouped by State/State+Industry
    vac_state_data <- industry %>% group_by(State_Name) %>% summarize(number_of_vacancies = n())
    colnames(vac_state_data) <- c("state","number_of_vacancies" )
    vac_ind_state_data <- industry %>% group_by(State_Name, Industry) %>% summarize(number_of_vacancies = n())
    colnames(vac_ind_state_data) <- c("state", "Industry","number_of_vacancies" )
    output$heatmapUI <- renderUI({
       plotOutput('Job_Industry_State',height=620)
        }
    )
    output$Job_Industry_State <- renderPlot({
        if(input$usmap_ind =="All"){
            plot_usmap(data = vac_state_data, values = "number_of_vacancies", color = "red", labels= TRUE) + 
                scale_fill_continuous(low = "white", high = "red", name = "Number of Vacancies", label = scales::comma
                ) + theme(legend.position = "right")}
        else{
            vac_ind_state_data1 <- vac_ind_state_data  %>% filter(Industry==input$usmap_ind)
            plot_usmap(data = vac_ind_state_data1,values = "number_of_vacancies", color = "red", labels= TRUE) + 
                scale_fill_continuous(low = "white", high = "red", name = "Number of Vacancies", label = scales::comma
                ) + theme(legend.position = "right")
        }
        
        
    })
    # Jobs By Company
    thedata <- reactive({
        if (input$country=="All Countries"){
            job_vacancies_complete%>% 
                mutate(iso3 = countrycode(Country,"country.name","iso3c")) %>%
                filter(VacanciesCount>=input$vacancies)}
        else{
            job_vacancies_complete %>%
                filter(Country==input$country) %>%
                mutate(iso3 = countrycode(Country,"country.name","iso3c")) %>%
                filter(VacanciesCount>=input$vacancies)}
    })
    
    #Plotting the choropleth map 
    output$plotChoroplethMap <-renderHighchart(highchart(type = "map") %>% 
                                               hc_add_series_map(map = worldgeojson, df = thedata(), value = "Num_Vacancy",joinBy ="iso3" ) %>% 
                                               hc_colorAxis(stops = color_stops()) %>% 
                                               hc_tooltip(useHTML=TRUE,headerFormat='',pointFormat = paste0('  {point.Country} Vacancies : {point.VacanciesCount} ')) %>% 
                                               hc_title(text = 'Map of Job Vacancies around the World') %>% 
                                               hc_subtitle(text = paste0('Vacancies more than: ',input$vacancies)) %>% hc_exporting(enabled = TRUE,filename = 'custom'))
    
                                                   
    #Plotting the sunburst chart    
    getSunburst <- reactive({
        continent_countries <- gapminder[gapminder$continent == input$continent,]
        df3_continent <- google[google$Country %in% continent_countries$country,]
        df3_sunburst <- data.frame(labels=character(),values=numeric(),parents=character(),ids=character())
        df3_sunburst[1,]<- c(input$continent,nrow(df3_continent),"",input$continent)
        if (input$filter_type=="Country"){
            df3_x <- df3_continent%>%group_by(Country)%>%summarise(Count=n())
            df3_x$parents <- input$continent
            df3_x$ids <- paste0(df3_x$parents," - ",df3_x$Country)
            df3_x <- df3_x %>%rename(labels=Country,values=Count)
            df3_y <- df3_continent%>%group_by(Category,Country)%>%summarise(values=n())
            df3_y$parents <- paste0(input$continent," - ",df3_y$Country)
            df3_y$ids <- paste0(df3_y$parents," - ",df3_y$Category)
            df3_y  <- df3_y %>%rename(labels=Category)
            df3_y <-df3_y %>%select(-Country)}
        else if(input$filter_type=="Category"){
            df3_x <- df3_continent%>%group_by(Category)%>%summarise(Count=n())
            df3_x$parents <- input$continent
            df3_x$ids <- paste0(df3_x$parents," - ",df3_x$Category)
            df3_x <- df3_x %>%rename(labels=Category,values=Count)
            df3_y <- df3_continent%>%group_by(Country,Category)%>%summarise(values=n())
            df3_y$parents <- paste0(input$continent," - ",df3_y$Category)
            df3_y$ids <- paste0(df3_y$parents," - ",df3_y$Country)
            df3_y <- df3_y %>%rename(labels=Country)
            df3_y <-df3_y %>%select(-Category)}
        
        df3_sunburst<- rbind(df3_sunburst,df3_x,df3_y)
        df3_sunburst
        
    })
    
    output$plotSunburst <- renderPlotly({
        plot_ly(
            data = getSunburst(), ids = ~ids, 
            labels= ~labels, parents = ~parents, 
            values= ~values, type='sunburst', branchvalues = 'total') %>%
            layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                   paper_bgcolor = "rgba(0, 0, 0, 0)",
                   fig_bgcolor   = "rgba(0, 0, 0, 0)")
    })
    #Salary Content 
    
    ## Salary by Industry 
    # Treemap for salary/vacancy
    
    ## Create salary/vacancy summary by company/ company + industry
    vac_sal_comp <- industry %>% group_by(Company) %>% summarize(number_of_vacancies = n(), mean_salary = mean(Salary_Avg))
    colnames(vac_sal_comp) <- c("Company","number_of_vacancies","Average_Salary" )
    vac_sal_comp_ranked <- head(arrange(vac_sal_comp, desc(number_of_vacancies)), n=50)
    vac_sal_comp_ind <- industry %>% group_by(Company, Industry) %>% summarize(number_of_vacancies = n(), mean_salary = mean(Salary_Avg))
    colnames(vac_sal_comp_ind) <- c("Company","Industry","number_of_vacancies" ,"Average_Salary" )
    ## Create a summary grouped by State/State+Industry
    vac_state_data <- industry %>% group_by(State_Name) %>% summarize(number_of_vacancies = n())
    colnames(vac_state_data) <- c("state","number_of_vacancies" )
    vac_ind_state_data <- industry %>% group_by(State_Name, Industry) %>% summarize(number_of_vacancies = n())
    colnames(vac_ind_state_data) <- c("state", "Industry","number_of_vacancies" )
    output$SalaryVacancyTree <- renderHighchart({
        if(input$salary_tree_ind =="All"){
            hctreemap2(vac_sal_comp_ranked, group_vars = c("Company"),size_var = "number_of_vacancies", color_var = "Average_Salary",
                       layoutAlgorithm = "squarified")}
        else{hctreemap2(vac_sal_comp_ind %>% filter(Industry == input$salary_tree_ind), 
                        group_vars = c("Company"),
                        size_var = "number_of_vacancies", color_var = "Average_Salary",
                        layoutAlgorithm = "squarified")}
        
    })
    
    ## Salary by Company
    output$salaryfig <- renderPlotly({plot_ly(salary_data[salary_data$Company %in% input$SalaryCompany ,],color = I("gray80")) %>% 
            add_segments(x = ~Salary_Min, xend = ~Salary_Max, y = ~Company, yend = ~Company, showlegend = FALSE) %>% 
            add_markers(x = ~Salary_Min, y = ~Company, name = "Min", color = I("pink"),text = ~Title, hovertemplate = '<b>Title</b>: %{text}<br><b>Salary</b>: %{x}') %>% 
            add_markers(x = ~Salary_Max, y = ~Company,  name = "Max", color = I("blue"),text = ~Title, hovertemplate = '<b>Title</b>: %{text}<br><b>Salary</b>: %{x}') %>% 
            layout(title = "Earnings range per company", xaxis = list(title = "Monthly Salary (in thousands)"), yaxis = list(title = paste0(c(rep("&nbsp;",25),"Company", rep("&nbsp;", 50), rep("\n&nbsp;", 10)), collapse = ""))
            ) 
    })
    
    
    #Ratings Content
    # Ratings by Industry Content
    
    ## Create data for radar plot for ratings by industry
    df_radar_ind <- industry %>% filter(Industry != "") %>% group_by(Industry) %>% summarize(Overall = mean(Overall_Review, na.rm = T), 
                                                                                             Balance = mean(Work_Life_Balance, na.rm = T), Pay = mean(Pay_Benefits, na.rm = T), Stability = mean(Job_Security_and_Advancement,na.rm = T),
                                                                                             Management = mean(Management, na.rm = T), Culture = mean(Culture, na.rm = T), Woman = mean(Woman_Rating, na.rm = T))
    df_radar_ind1 <-df_radar_ind[,-1]
    rownames(df_radar_ind1)<-df_radar_ind$Industry
    
    ## Fetch minima and maxima of every column of the data-set
    colMax <- function (x) { apply(x, MARGIN=c(2), max, na.rm=T) }
    colMin <- function (x) { apply(x, MARGIN=c(2), min, na.rm=T) }
    maxmin_ind <- data.frame(max=colMax(df_radar_ind1),min=colMin(df_radar_ind1))
    ratings_max <- c(rep(5,7))
    ratings_min <- c(rep(0,7))
    maxmin_ratings <- as.data.frame(matrix(NA,nrow=7,ncol=2))
    maxmin_ratings[,1] <- ratings_min
    maxmin_ratings[,2] <- ratings_max
    rownames(maxmin_ratings) <-c("Overall" , "Balance" , "Pay", 'Stability', "Management", "Culture", "Woman")
    colnames(maxmin_ratings) <-c("min", "max")
    ## Calculate the average profile
    average_ind <- data.frame(rbind(maxmin_ind$max,maxmin_ind$min,t(colMeans(df_radar_ind1, na.rm=T))))
    colnames(average_ind) <- colnames(df_radar_ind1)
    
    output$Radar_Ind <- renderPlot({
        if (input$radar_ind_type == "Industry_Average") {
            data <- df_radar_ind1 %>% filter(row.names(df_radar_ind1) %in% input$radarind)
            par(mar=rep(1.6,4))
            par(mfrow=c(1,4))
            for (i in 1:nrow(data)) {
                toplot <- rbind(
                    maxmin_ratings$max,
                    maxmin_ratings$min,
                    average_ind[3,],
                    data[i,]
                )
                plot <- radarchart(
                    toplot,
                    axistype = 2,
                    pfcol = c(rgb(0.2,0.5,0.5,0.4),NA),
                    pcol= c(NA,2),
                    pty = 32, 
                    plty = 1,
                    plwd = 2,
                    title = row.names(data)[i]
                )
            }
            plot
            par(mfrow = c(1,1))
        }
        else {
            User_Results <- c(3.2, 3.0, 4.4, 3.6, 4.8, 3.4, 3)
            user_data <- data.frame(matrix(nrow=2,ncol=7))
            user_data[1,] <- User_Results
            colnames(user_data) <- c("Overall" , "Balance" , "Pay", 'Stability', "Management", "Culture", "Woman")
            
            
            
            data <- df_radar_ind1 %>% filter(row.names(df_radar_ind1) %in% input$radarind)
            par(mar=rep(1.6,4))
            par(mfrow=c(1,4))
            for (i in 1:nrow(data)) {
                toplot <- rbind(
                    maxmin_ratings$max,
                    maxmin_ratings$min,
                    user_data[1,],
                    data[i,]
                )
                plot <- radarchart(
                    toplot,
                    axistype = 2,
                    pfcol = c(rgb(0.8,0.2,0.5,0.4),NA),
                    pcol= c(NA,2),
                    pty = 32, 
                    plty = 1,
                    plwd = 2,
                    title = row.names(data)[i]
                )
            }
            plot
            par(mfrow = c(1,1))            
            
            
        }
        
        
    })
    
    output$happiness_fig <- renderPlotly({plot_ly(happiness_count2[happiness_count2$Industry %in% input$HappinessIndustry ,],color = I("gray80")) %>% 
            add_bars(x=~n, y = ~Industry, color = ~happiness_labels, orientation = 'h' ) %>% 
            layout(barmode='stack', xaxis=list( showgrid=T, showlegend = F, zeroline=T,showline=FALSE, ticks='',showticklabels=T))
    })
    
    
    #Ratings by Company Content
    
    # Radar Chart for Companies
    
    ## Create data for radar plot for ratings by Company
    df_radar_comp <- industry %>% filter(Company != "") %>% group_by(Company) %>% summarize(Overall = mean(Overall_Review, na.rm = T), 
                                                                                            Balance = mean(Work_Life_Balance, na.rm = T), Pay = mean(Pay_Benefits, na.rm = T), Stability = mean(Job_Security_and_Advancement,na.rm = T),
                                                                                            Management = mean(Management, na.rm = T), Culture = mean(Culture, na.rm = T), Woman = mean(Woman_Rating, na.rm = T))
    df_radar_comp1 <-df_radar_comp[,-1]
    rownames(df_radar_comp1)<-df_radar_comp$Company
    # Fetch minima and maxima of every column of the data-set
    maxmin_comp <- data.frame(max=colMax(df_radar_comp1),min=colMin(df_radar_comp1))
    
    # Calculate the average profile
    average_comp <- data.frame(rbind(maxmin_comp$max,maxmin_comp$min,t(colMeans(df_radar_comp1, na.rm=T))))
    colnames(average_comp) <- colnames(df_radar_comp1)
    
    output$Radar_Comp <- renderPlot({
        if (input$radar_comp_type == "Companies_Average") {
            
            data <- df_radar_comp1 %>% filter(row.names(df_radar_comp1) %in% input$radarcomp)
            par(mar=rep(1.6,4))
            par(mfrow=c(1,4))
            for (i in 1:nrow(data)) {
                toplot <- rbind(
                    maxmin_ratings$max,
                    maxmin_ratings$min,
                    average_comp[3,],
                    data[i,]
                )
                plot <- radarchart(
                    toplot,
                    axistype = 2,
                    pfcol = c(rgb(0.2,0.5,0.5,0.4),NA),
                    pcol= c(NA,2),
                    pty = 32, 
                    plty = 1,
                    plwd = 2,
                    title = row.names(data)[i]
                )
            }
            plot
            par(mfrow = c(1,1))
        }
        else {
            User_Results <- c(3.2, 3.0, 4.4, 3.6, 4.8, 3.4, 3)
            user_data <- data.frame(matrix(nrow=2,ncol=7))
            user_data[1,] <- User_Results
            colnames(user_data) <- c("Overall" , "Balance" , "Pay", 'Stability', "Management", "Culture", "Woman")
            
            
            data <- df_radar_comp1 %>% filter(row.names(df_radar_comp1) %in% input$radarcomp)
            par(mar=rep(1.6,4))
            par(mfrow=c(1,4))
            for (i in 1:nrow(data)) {
                toplot <- rbind(
                    maxmin_ratings$max,
                    maxmin_ratings$min,
                    user_data[1,],
                    data[i,]
                )
                plot <- radarchart(
                    toplot,
                    axistype = 2,
                    pfcol = c(rgb(0.8,0.2,0.5,0.4),NA),
                    pcol= c(NA,2),
                    pty = 32, 
                    plty = 1,
                    plwd = 2,
                    title = row.names(data)[i]
                )
            }
            plot
            par(mfrow = c(1,1))            
            
            
        }
        
        
    })
    
    
    #Skills Content
    softskills <- c('integrity','dependability','effective communication','communication','teamwork','creativity','problem solving','critical','adaptability','organization','willingness to learn','empathy','humble','leadership','lead','resourceful','analytical','motivated','interpersonal','curious','driven','research','work ethic','flexibility','attention-to-detail','confidence','presentation','training','management','independent','dedicated','social','enthusiastic','collaboration','passionate','communicate','decision making','quick','friendly','networking','relationship','team building','analytical thinker','storytelling','critical observation','will learn','innovation','logical thinking','troubleshooting','conflict resolution','deal making','inspiring people','mentoring','resolving issues','managing difficult conversations','giving clear feedback','dealing with politics','hard working','business ethics','meeting deadlines','multitasking','persistence','proper business etiquette','planning','reliability','self directed','experience')
    hardskills <- c("analytic solutions","machine learning","predictive modeling","database systems","clinical decision engines", "algorithms", "NLP/ML", "SQL",  "MongoDB","DynamoDB", "R, ","Python","dplyr","GGPlot", "Pandas","OLS","MLE","Machine Learning",  "Decision Tree","Random Forest","AI" , "Visualization","A/B tests set-up","Reporting",  "data visualizations","numpy", "scipy","scikit-learn", "tensorflow","pytorch" , "keras","genism", "vowpal wabbit","Heap.io","Google Analytics","Big Data","Business Analytics","Oracle","Relational Database Management System (RDMS)","Statistical Programming Language","Regression","Decision Trees","K-Means","Tableau","looker","R Programming" ,"Microsoft Office" , "SPSS","No-SQL", "Cassandra","Hadoop", "Pig","Hive", "HPCC Systems","Javascript" , "Java programming","PowerBI","Linux","TensorFlow", "Keras","Shiny","Artificial Intelligence","NLP", "Tesseract","Jenkins CI/CD", "Azure","logistic regression","k-means clustering","decision forests", "JavaScript","Cloud data", "MATLAB","Excel", "Jupyter","Gurobi","agile", "Git","Github" , "Qlikview","Business Intelligence", "supply chain","D3", "big data",'business sense','C',' API', 'Get Requests', 'Push Requests', 'Update Requests','AWS', 'Sagemaker','Power BI','PowerBI','Cognos','suite', 'Business Objects','Amplitude','Mixpanel','Salesforce', 'Qlik','Microstrategy', 'java','business analytics','reporting','C+','C++','perl','php','hadoop','web scraping','text mining','ruby','Word','NLP','unstructured data') %>% tolower()
    industry_unnest <- industry %>% select(Industry,Company, Description) %>% unnest_tokens(word,Description) %>% anti_join(.,stopwords,by='word') %>% filter(!str_detect(word,'[0-9]+'))
    google_unnest <- google %>% select(Category,`Minimum Qualifications`,`Preferred Qualifications`) %>% gather(.,Type,Qualifications,`Minimum Qualifications`:`Preferred Qualifications`) %>% select(-Type) %>% unnest_tokens(word,Qualifications) %>% anti_join(.,stopwords,by='word') %>% filter(!str_detect(word,'[0-9]+'))
    
    degrees <- c('ba','bs',"bachelor's",'bachelors','phd','masters','ms','mba',"master's","undergraduate","graduate")
    subjects <- c('engineering','marketing','finance','Arts','Accounting','Law','economics','science','humanities') %>% tolower()
    
    output$degree_ind <- renderPlotly({
        if(input$industry_type=='Degrees') {
            industry_unnest %>% filter(Industry==input$industry_degree) %>% filter(word %in% degrees) %>% 
                mutate(word=case_when(word=='bs'~'Bachelors',word=='ba'~'Bachelors',word=="bachelor's"~'Bachelors',word=="bachelors"~"Bachelors", word=='mba'~'MBA',word=='ms'~'Masters',word=="master's"~'Masters',word=="masters"~'Masters',word=='phd'~'PhD',word=='undergraduate'~'Bachelors',word=='graduate'~'Graduates',TRUE~word)) %>% count(word,sort=TRUE) %>% 
                mutate(percentage=paste(formatC((n/sum(n)*100),2),'%')) %>% plot_ly(x=~case_when(word=='PhD'~'PhD', word=='MBA'~'MBA',TRUE~str_to_title(word)),y=~n,hovertext=~percentage) %>% layout(xaxis=list(title='Degrees'), yaxis=list(visible=FALSE))
        } else {
            industry_unnest %>% filter(Industry==input$industry_degree) %>% filter(word %in% subjects) %>% count(word,sort=TRUE) %>% mutate(percentage=paste(formatC((n/sum(n)*100),2),'%')) %>%
                plot_ly(x=~str_to_title(word),y=~n,hovertext=~percentage) %>% layout(xaxis=list(title='Subject'), yaxis=list(visible=FALSE))}
    })
    
    output$degree_comp <- renderPlotly({
        if(input$company_type=="Degrees"){
            if(input$company_degree=='Google') {
                google_unnest %>% filter(word %in% degrees) %>% mutate(word=case_when(word=='bs'~'Bachelors',word=='ba'~'Bachelors',word=="bachelor's"~'Bachelors',word=="bachelors"~"Bachelors", word=='mba'~'MBA',word=='ms'~'Masters',word=="master's"~'Masters',word=="masters"~'Masters',word=='phd'~'PhD',word=='undergraduate'~'Bachelors',word=='graduate'~'Graduates',TRUE~word)) %>% count(word,sort=TRUE) %>% mutate(percentage=paste(formatC((n/sum(n)*100),2),'%')) %>% 
                    plot_ly(x=~case_when(word=='PhD'~'PhD',word=='MBA'~'MBA',TRUE~str_to_title(word)),y=~n,hovertext=~percentage) %>% layout(xaxis=list(title='Degrees'), yaxis=list(visible=FALSE))
            }else{
                industry_unnest %>% filter(Company==input$company_degree) %>% filter(word %in% degrees) %>% mutate(word=case_when(word=='bs'~'Bachelors',word=='ba'~'Bachelors',word=="bachelor's"~'Bachelors',word=="bachelors"~"Bachelors", word=='mba'~'MBA',word=='ms'~'Masters',word=="master's"~'Masters',word=="masters"~'Masters',word=='phd'~'PhD',word=='undergraduate'~'Bachelors',word=='graduate'~'Graduates',TRUE~word)) %>% count(word,sort=TRUE) %>% mutate(percentage=paste(formatC((n/sum(n)*100),2),'%')) %>% 
                    plot_ly(x=~case_when(word=='PhD'~'PhD',word=='MBA'~'MBA',TRUE~str_to_title(word)),y=~n,hovertext=~percentage) %>% layout(xaxis=list(title='Degrees'), yaxis=list(visible=FALSE))}
        } else {
            if(input$company_degree=='Google') {
                google_unnest%>% filter(word %in% subjects) %>% count(word,sort=TRUE) %>% mutate(percentage=paste(formatC((n/sum(n)*100),2),'%')) %>% plot_ly(x=~str_to_title(word),y=~n,hovertext=~percentage) %>% layout(xaxis=list(title='Subject'), yaxis=list(visible=FALSE))
            } else {
                industry_unnest %>% filter(Company==input$company_degree) %>% filter(word %in% subjects) %>% count(word,sort=TRUE) %>% mutate(percentage=paste(formatC((n/sum(n)*100),2),'%')) %>% 
                    plot_ly(x=~str_to_title(word),y=~n,hovertext=~percentage) %>% layout(xaxis=list(title='Subject'), yaxis=list(visible=FALSE))
            }
        }
        
    })
    
    output$Skills_ind <- renderPlotly({
        
        industry_soft <-industry_unnest %>% filter(Industry==input$industry_skill) %>% filter(word %in% softskills) %>% count(word,sort=TRUE) %>% mutate(percentage=paste0((formatC(((n/sum(n))*100),4)),'%')) %>% 
            top_n(input$industry_skill_freq,n) %>% plot_ly(x=~str_to_title(word),y=~n,type='bar', hovertext=~percentage, marker=list(color='purple'),showlegend=FALSE) %>% layout(yaxis=list(visible=FALSE),xaxis=list(title='People Skills'))
        
        industry_hard <-industry_unnest %>% filter(Industry==input$industry_skill) %>% filter(word %in% hardskills) %>% count(word,sort=TRUE) %>% mutate(percentage=paste0((formatC(((n/sum(n))*100),4)),'%')) %>% top_n(input$industry_skill_freq,n) %>%
            plot_ly(type='bar',x=~case_when(word=='nlp'~'NLP',word=='sql'~'SQL',word=='aws'~'AWS',TRUE~str_to_title(word)),y=~n, hovertext=~percentage,marker = list(color="rgba(255, 0, 0, 0.6)")) %>% layout(yaxis=list(visible=FALSE),xaxis=list(title='Technical Skills'),showlegend=FALSE) 
        
        subplot(industry_soft,industry_hard,nrows=1,titleX = TRUE, titleY = TRUE)
        
    })
    
    output$Skills_comp <- renderPlotly({
        
        if(input$company_skill=='Google') {
            company_soft <- google_unnest %>% filter(word %in% softskills) %>% count(word,sort=TRUE) %>% mutate(percentage=paste0((formatC(((n/sum(n))*100),4)),'%')) %>% top_n(input$company_skill_freq,n) %>% 
                plot_ly(type='bar',x=~str_to_title(word),y=~n, hovertext=~percentage,marker = list(color="purple"),showlegend=FALSE) %>% layout(yaxis=list(visible=FALSE),xaxis=list(title='People Skills'))
            
            company_hard <- google_unnest %>% filter(word %in% hardskills) %>% count(word,sort=TRUE) %>% mutate(percentage=paste0((formatC(((n/sum(n))*100),4)),'%')) %>% top_n(input$company_skill_freq,n) %>% 
                plot_ly(type='bar',x=~case_when(word=='nlp'~'NLP',word=='sql'~'SQL',word=='aws'~'AWS',TRUE~str_to_title(word)),y=~n, hovertext=~percentage,marker = list(color="rgba(255, 0, 0, 0.6)"),showlegend=FALSE) %>% layout(yaxis=list(visible=FALSE),xaxis=list(title='Technical Skills'))
            
            subplot(company_soft,company_hard,nrows=1,titleX = TRUE, titleY = TRUE)
            
        } else {
            
            company_soft <-industry_unnest %>% filter(Company==input$company_skill) %>% filter(word %in% softskills) %>% count(word,sort=TRUE) %>% mutate(percentage=paste0((formatC(((n/sum(n))*100),4)),'%')) %>% top_n(input$company_skill_freq,n) %>% 
                plot_ly(type='bar',x=~str_to_title(word),y=~n, hovertext=~percentage, marker=list(color='purple'),showlegend=FALSE) %>% layout(yaxis=list(visible=FALSE),xaxis=list(title='People Skills'))
            
            company_hard <-industry_unnest %>% filter(Company==input$company_skill) %>% filter(word %in% hardskills) %>% count(word,sort=TRUE) %>% mutate(percentage=paste0((formatC(((n/sum(n))*100),4)),'%')) %>% top_n(input$company_skill_freq,n) %>% 
                plot_ly(type='bar',x=~case_when(word=='nlp'~'NLP',word=='sql'~'SQL',word=='aws'~'AWS',TRUE~str_to_title(word)),y=~n, hovertext=~percentage,marker = list(color="rgba(255, 0, 0, 0.6)"),showlegend=FALSE) %>% layout(yaxis=list(visible=FALSE),xaxis=list(title='Technical Skills'))
            
            subplot(company_soft,company_hard,nrows=1,titleX = TRUE, titleY = TRUE)
        }})
    
    
    
    #Reviews Content 
    review_unnest <- review %>% select(-Company) %>% unnest_tokens(word,Review) %>% anti_join(.,stopwords, by='word') 
    
    output$wordcloud <- renderPlot({
        review_unnest %>% filter(Rating==input$rating) %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>%acast(word ~ sentiment, value.var = "n", fill = 0) %>% comparison.cloud(colors = c("red", "darkGreen"),max.words = input$word_freq, scale=c(4,0.5))
    })
    
    output$emotion_sentiment <- renderPlotly({
        review_unnest %>% inner_join(get_sentiments('nrc') )%>% count(sentiment)%>% mutate(percentage=paste(formatC((n/sum(n))*100,3),'%')) %>% 
            group_by(sentiment) %>% arrange(desc(n)) %>% top_n(10,n) %>% mutate(Type=ifelse(sentiment %in% c('fear','disgust','anger','negative','sadness'),'Negative Emotion','Positive Emotion')) %>% plot_ly(x=~str_to_title(sentiment),y=~n,hovertext=~percentage,color=~Type,colors=c('red','darkGreen')) %>% layout(xaxis=list(title='Sentiment'),yaxis=list(visible=FALSE))
    })
    
    output$location_sentiment <- renderPlot({
        review_unnest %>% inner_join(get_sentiments("bing")) %>% anti_join(stopwords) %>% mutate(word=lemmatize_words(word),word=case_when(word=='ambiguity'~'ambiguous',word=='effectively'~'effective',TRUE~word)) %>% filter(Review_Location==input$reviews_location) %>% count(word, sentiment, sort = TRUE) %>% group_by(sentiment) %>% 
            top_n(10,n) %>% ungroup() %>% mutate(word = reorder(word, n)) %>% ggplot(aes(str_to_title(word), n, fill = sentiment)) +geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") +labs(y = "Contribution to sentiment",x = NULL) +coord_flip()
    })
    
    output$position_sentiment <- renderPlot({
        review_unnest %>% inner_join(get_sentiments("bing")) %>% anti_join(stopwords) %>% mutate(word=lemmatize_words(word),word=case_when(word=='ambiguity'~'ambiguous',word=='effectively'~'effective',TRUE~word)) %>% filter(Review_Author==input$reviews_position) %>% count(word, sentiment, sort = TRUE) %>% group_by(sentiment) %>%
            top_n(10,n) %>% ungroup() %>% mutate(word = reorder(word, n)) %>% ggplot(aes(str_to_title(word), n, fill = sentiment)) +geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") +labs(y = "Contribution to sentiment",x = NULL) +coord_flip()
    })
    
    
}    

shinyApp(ui, server)


