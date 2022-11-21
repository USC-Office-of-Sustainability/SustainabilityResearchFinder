# USC SHINY WEB APP
# BRIAN TINSLEY
# EXTENSION FROM PETER WU AT CMU

# require(devtools)
# install_github("lchiffon/wordcloud2")
# library(wordcloud2)

# Run App with the Run App button at the top of the screen

library(tidyverse)
library(shiny)
library(shinydashboard)
# install.packages("plotly")
library(plotly)
# install.packages("wordcloud")
library(wordcloud)
# install.packages("DT")
library(DT)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("ggrepel")
library(ggrepel)

# read in the filtered or unfiltered data
classes = read.csv("master_course_sdg_data.csv")

sdg_colors <- c('#e5243b', '#DDA63A', '#4C9F38', '#C5192D', '#FF3A21', '#26BDE2', 
                '#FCC30B', '#A21942', '#FD6925', '#DD1367', '#FD9D24', '#BF8B2E',
                '#3F7E44', '#0A97D9', '#56C02B', '#00689D', '#19486A')
exclude_words <- c() #this has already been accounted for in earlier files

# data for pie chart
sustainability_related = read.csv("usc_courses_full.csv")


### Begin Shiny App Code ###

# Define UI for application that draws a histogram
ui <- dashboardPage( skin="black",
                     
                     # Application title
                     dashboardHeader(title = "USC SDG Mapping"),
                     
                     
                     dashboardSidebar(
                       sidebarMenu( #will eventually add Schools to sdgs and sdgs to schools
                         menuItem("Home (About)", tabName = "6"),
                         menuItem("Mapping the 17 SDGs", tabName = "5"),
                         menuItem("Find SDGs by Classes", tabName = "1"),
                         menuItem("Find Classes by SDGs", tabName = "3"),
                         menuItem("All Sustainability-Related Classes", tabName = "2"),
                         menuItem("Map Your Classes", tabName = "7"),
                         menuItem("FAQ", tabName = "8")
                       )
                     ),
                     
                     
                     dashboardBody( tags$head(tags$link(rel="stylesheet", type="text/css", href="custom.css")), #link up css stylesheet
                                    tabItems(
                                      tabItem(tabName = "1",
                                              tabPanel("All", fluidPage(
                                                h1("Find SDGs by Classes"),
                                                h4(""),
                                                h3("Select a USC semester and course ID below to view the SDG mapping for 
                          that particular class. If the mapping is
                          blank, the course description may have been too short to draw
                          any concrete conclusions. To check out the USC course catalogue, click ", a("here.", href="https://catalogue.usc.edu/")),
                                                h5("*This app is a work in progress, and we are continually improving accuracy. 
                                                   If you have feedback, please email: oosdata@usc.edu"),
                                                div(style="font-size:24px;", selectInput(inputId = "usc_semester1",
                                                                                         label = "Choose USC Semester",
                                                                                         # selected = "SP18",
                                                                                         choices = unique(classes$semester))),
                                                div(style="font-size:24px;",selectizeInput(inputId = "usc_classes", 
                                                                                           label = "Choose USC Class", 
                                                                                           # selected = "ACCT-525",
                                                                                           # choices = unique(classes$courseID),
                                                                                           choices = NULL,
                                                                                           options = list(maxOptions = 10000))),
                                                br(),
                                                h3(strong("Course description:")),
                                                h3(textOutput("course_desc")),
                                                fluidRow(bootstrapPage(
                                                  column(6, plotOutput(outputId = "classes_to_wordcloud"), br()),
                                                  column(6, plotOutput(outputId = "classes_to_keywords"), br())
                                                  # column(6, plotOutput(outputId = "test_run"), br())
                                                )),
                                                fluidRow(bootstrapPage(
                                                  column(6, plotOutput(outputId = "classes_to_goals"), br()),
                                                  column(6, img(src = "un_17sdgs.png", width = "100%"))
                                                )),
                                                h1("Keyword Table"),
                                                fluidRow(bootstrapPage(
                                                  column(12, DT::dataTableOutput("classes_table"))
                                                ))
                                              ))
                                      ),#end tabitem
                                      tabItem(tabName = "3",
                                              fluidPage(
                                                h1("Find Classes by SDGs"),
                                                h3("Select a USC semester and one of the SDGs to display the 10 most relevant USC classes that map to
                           that goal. To check out the USC course catalogue, click ", a("here.", href="https://catalogue.usc.edu/")),
                                                h5("*This app is a work in progress, and we are continually improving accuracy. 
                                                   If you have feedback, please email: oosdata@usc.edu"),
                                                div(style="font-size:24px;", selectInput(inputId = "usc_semester3",
                                                                                         label = "Choose USC Semester",
                                                                                         # selected = "SU18",
                                                                                         choices = unique(classes$semester))),
                                                div(style="font-size:24px;", selectizeInput(inputId = "sdg_goal1", 
                                                                                            label = "Choose SDG", 
                                                                                            choices = c(1:17)
                                                )),
                                                fluidRow(bootstrapPage(
                                                  column(6, plotOutput(outputId = "goals_to_classes"), br()),
                                                  column(6, img(src = "un_17sdgs.png", width = "100%"))
                                                )),
                                                h1(textOutput("sdg_name")),
                                                fluidRow(bootstrapPage(
                                                  column(12, DT::dataTableOutput("top_classes_sdg_table"))
                                                ))
                                              )
                                      ), #end tabitem2
                                      tabItem(tabName = "5",
                                              fluidPage(
                                                h1("Mapping the 17 SDGs"),
                                                h3("Below displays a wordcloud for the top keywords for each SDG.
                           The keywords come from the ", a("CMU250 keywords list,", 
                                                           href="https://github.com/CMUSustainability/SDGmapR/tree/main/data"), "and the weights of the words relative to each SDG 
                           were calculated using ",a("Google's word2vec.", href="https://code.google.com/archive/p/word2vec/"), 
                                                   "These word and weight combinations are the criteria for the course mappings in the other pages of this website."),
                                                h3("In the near future, this list will be updated to a USC and CMU combined list with the input of
                           the PWG. To see the words in a CSV file format, please see the ",a("USC-SDGmap package.", href="https://code.google.com/archive/p/word2vec/")),
                                                h2("Select an SDG below to see its most relevant keywords by weight."),
                                                h5("*This app is a work in progress, and we are continually improving accuracy. 
                                                   If you have feedback, please email: oosdata@usc.edu"),
                                                div(style="font-size:24px;",
                                                    selectizeInput(inputId = "sdg_goal3", label = "Choose SDG", choices = c(1:17)
                                                    )),
                                                tags$head(tags$style(HTML(".selectize-input {height: 45px; width: 450px; font-size: 24px;}"))),
                                                fluidRow(bootstrapPage(
                                                  column(6, plotOutput(outputId = "visualize_sdg"),br()),
                                                  column(6, img(src = "un_17sdgs.png", width = "100%"))
                                                )),
                                                h1("SDG Keywords Table"),
                                                fluidRow(bootstrapPage(
                                                  column(12, DT::dataTableOutput("keywords_table"))
                                                ))
                                              )
                                      ), # end tab item 5
                                      tabItem(tabName = "6",
                                              fluidPage(
                                                h1("Home (About)"),
                                                # fluidRow(
                                                         h3(strong("Are you interested in sustainability and the ",
                                                                   a("UN Sustainability Development Goals (SDGs)?", href="https://sdgs.un.org")), 
                                                                   "If so, you have come to the right place!", strong("This 
                           dashboard is a tool that enables you to see which classes at USC 
                           relate to the 17 UN SDGs. If you are a student, you can use this 
                           tool to shape your education toward these sustainability goals."),
                           "Sustainability incorporates protection for the environment, 
                           balancing a growing economy, and social responsibility to lead to an 
                           improved quality of life for current and future generations. Here, 
                           we have created a program in collaboration with Carnegie Mellon University 
                           to elevate awareness of sustainability in higher education."),
                                                         br(),
                                                fluidRow(img(src="Education.png", height="550", style="display: block; margin-left: auto; margin-right: auto;"))
                                              )
                                      ), # end tab item 6
                                      tabItem(tabName = "8",
                                              fluidPage(
                                              h2(strong("FAQ")),
                                              h3(strong("How Do I Use this Dashboard?"), "You can choose your search function in 
                                 the main menu in the upper-left corner of this dashboard. 
                                 Here you can either find classes by the 17 different SDGs 
                                 (Find Classes by SDG) or see which SDGs map to a selected class 
                                 (Find SDGs by Class). To see how many classes at USC are sustainability-focused 
                                 or sustainability-inclusive, please click on the bottom menu bottom 
                                 ‘All Sustainability-Related Classes”."),
                                              
                                              h3(strong("How was this dashboard created?"), "This dashboard was created with Rshiny, 
                                 based on source code in R through a collaboration of USC’s Office of Sustainability 
                                 (Source Code Developers: PSIP Intern- Brian Tinsley and Data Analyst- Dr. Julie Hopper) 
                                 with Carnegie Mellon University (Source Code Developers: Director of Sustainability 
                                 Initiatives - Alex Hinicker and Undergraduate Alumni - Peter Wu). CMU’s original 
                                 version of this program can be found at CMU SDG Mapping. All of the datasets and 
                                 source code used in this dashboard are open-source and can be found through our 
                                 Github page."),
                                              
                                              h3(strong("What are the UN’s 17 Sustainability Development Goals (SDGs)?"),
                                                 "The 2030 Agenda for Sustainable Development was adopted in 2015 by all UN member 
                                    states and provides a ‘blueprint for peace and prosperity for people and the planet, 
                                    now and into the future’. At the center of this are the 17 Sustainable Development
                                    Goals (SDGs). These goals acknowledge that ending poverty and other deprivations must
                                    accompany strategies that reduce inequality,  improve education and health, and 
                                    spur economic growth – all while working to preserve our natural ecosystems and 
                                    tackling climate change. To explore the 17 SDGs, 
                                    please visit ", a("their website.", href= "https://sdgs.un.org/goals#icons")),
                                              
                                              h3(strong("How are USC's classes mapped to the 17 SDGs?"), "Please visit our page 
                                    “Mapping the 17 SDGs” to learn more."),
                                              
                                              h3(strong("What if I have more Questions/Comments or Suggestions?"), "Please contact: oosdata@usc.edu"),
                                      )),
                                     
                                      tabItem(tabName = "2",
                                              fluidPage(
                                                h1("All Sustainably-Related Classes"),
                                                h3("The below charts show the percent and number of USC courses and 
                                                   departments that are ‘sustainability-focused’, ‘sustainability-inclusive’ or ‘not-related’ to 
                                                   sustainability. To count as sustainability-inclusive, a course has to map to at least one of the 17 UN SDGs. 
                                                   To count as sustainability-focused, a course has to map to a combination of SDGs that includes at least one environmental 
                                                   SDG and at least one economic or social SDG. ‘Sustainability-focused’ departments have at least 
                                                   one program (major, minor or graduate degree program) that requires at least 1 sustainability focused course. 
                                                   ‘Sustainability-inclusive’ departments have at least one program that requires at least one sustainability-inclusive course."),
                                                h5("*This app is a work in progress, and we are continually improving accuracy. 
                                                   If you have feedback, please email: oosdata@usc.edu"),
                                                div(style="font-size:24px;",selectInput(inputId = "usc_year",
                                                                                        label = "Choose USC Academic Year",
                                                                                        # selected = "AY19",
                                                                                        choices = unique(sustainability_related$year))),
                                                h4("Academic year determined by the year of the Spring semester and includes Summer and Fall terms of the previous calendar year. (AY19 = SU18, F18, SP19)"),
                                                # h3("Not including multiple sections"),
                                                # plotOutput("pie1"),
                                                # textOutput("pie1_numbers")
                                              
                                              # fluidRow(
                                              #   h3("Including multiple sections"),
                                              #   plotOutput("pie2"),
                                              #   textOutput("pie2_numbers")
                                              # ),
                                              # fluidRow(
                                              #   h3("By department"),
                                              #   plotOutput("pie3"),
                                              #   textOutput("pie3_numbers")
                                              # ),
                                                h3("Sustainability Related Courses Offered"),
                                                fluidRow(column(6, plotOutput("pie4"))),
                                                # textOutput("pie4_numbers")
                                                h3("Sustainability Related Departments"),
                                                fluidRow(column(6, plotOutput("pie3"))),
                                              h3("Department Sustainability Classification Table"),
                                                column(12, DT::dataTableOutput("sustainability_table")),
                                              ) # end fluid page
                                      ), # end tab item 6
                                      tabItem(tabName = "7",
                                              fluidPage(
                                                h1("Map Your Classes"),
                                                h3("Select your classes below and see how your curriculum relates to the 17 SDGs."), 
                                                h5("*This app is a work in progress, and we are continually improving accuracy. 
                                                   If you have feedback, please email: oosdata@usc.edu"),
                                                # h3("Enter Your USC Courses"),
                                                div(style="font-size:24px;", selectizeInput(
                                                  inputId = "user_classes",
                                                  label = "Enter Your USC Courses",
                                                  choices = unique(classes$courseID),
                                                  selected = NULL,
                                                  multiple = TRUE,
                                                  width = "100%",
                                                  options = list(
                                                    'plugins' = list('remove_button'),
                                                    'create' = TRUE,
                                                    'persist' = TRUE
                                                  )
                                                )),
                                                h3("SDG Mapping Data for:"),
                                                h4(textOutput("personal_classes")),
                                                br(),
                                                fluidRow(
                                                  column(6, plotOutput(outputId = "user_to_goals"), br(),
                                                  plotOutput(outputId = "user_classes_barplot"), br()),
                                                  column(6, img(src = "un_17sdgs.png", width="100%"))
                                                ),
                                                h3("Your Classes"),
                                                fluidRow(bootstrapPage(
                                                  column(12, DT::dataTableOutput("user_table"))
                                                ))
                                              
                                              )#end fluid page
                                      )# end tab item 7
                                    )#end tabitems
                     )#end dashboard body
                     
) #end UI


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #this part filters the classes by the semester chosen in input field
  observeEvent(input$usc_semester1,
               {
                 updateSelectizeInput(session, "usc_classes",
                                      server = TRUE,
                                      choices = sort(classes %>% filter(semester == input$usc_semester1) %>% select(courseID) %>% pull()), #change this section to course_
                                      selected = unique(classes %>% filter(semester == input$usc_semester1) %>% select(courseID) %>% pull())[1])
                 
               })
  
  #this retunrs a dataframe with the descriptions of all the classes the user selected
  output$course_desc <- renderText({
    usc_course_desc <- classes %>%
      filter(semester == input$usc_semester1) %>%
      filter(courseID == input$usc_classes) %>% #changed from section
      distinct(section, .keep_all = TRUE) %>%
      select(course_desc) %>%
      pull()
    
    return(paste(usc_course_desc))
  })
  
  # get the courses
  output$personal_classes <- renderText({
    if (length(input$user_classes) > 0){ #first make sure they typed something in
      
    class_list <- classes %>%
      filter(courseID == input$user_classes) %>% #changed from section
      distinct(section, .keep_all = TRUE) %>%
      select(courseID) %>%
      unique() %>%
      pull()
    
    # return(paste(dQuote(class_list, q = FALSE), collapse = ", "))
    return(paste(class_list, collapse = ", "))
    }
  })
  
  # trying to get Classes by SDGs table name
  output$sdg_name = renderText({
    paste("All Classes Mapped to SDG", input$sdg_goal1, sep="")
  })
  
  
  ### Classes Forward
  output$classes_to_goals <- renderPlot({
    sdg_class_keyword_colors <- classes %>%
      filter(semester == input$usc_semester1) %>%
      filter(courseID %in% input$usc_classes) %>%
      group_by(goal) %>%
      mutate(sum_weight = sum(weight)) %>%
      arrange(desc(sum_weight)) %>%
      ungroup() %>%
      distinct(goal, .keep_all = TRUE) %>%
      arrange(goal) %>%
      select(color) %>%
      unique() %>%
      pull()
    # print(sdg_class_keyword_colors)
    if (length(sdg_class_keyword_colors) == 0) {return(ggplot())}
    
    sdg_class_name <-  classes %>%
      filter(semester == input$usc_semester1) %>%
      filter(courseID %in% input$usc_classes) %>%
      select(courseID) %>%
      unique() %>%
      pull()
    # print(sdg_class_name)
    
    sdg_class_goal_barplot <- classes %>%
      filter(semester == input$usc_semester1) %>%
      filter(courseID %in% input$usc_classes) %>%
      group_by(goal) %>%
      mutate(sum_weight = sum(weight)) %>%
      arrange(desc(sum_weight)) %>%
      ungroup() %>%
      distinct(goal, .keep_all = TRUE) %>%
      ggplot(aes(x = reorder(goal, sum_weight), y = sum_weight, fill = factor(as.numeric(goal)))) +
      geom_col() +
      coord_flip() +
      # geom_hline(yintercept = c(10, 15), color = c("#ffc33c", "#00bc9e")) +
      labs(title = paste0(sdg_class_name, " (", input$usc_classes, ") ", "SDGs"),
           fill = "SDG",
           x = "SDG",
           y = "Total SDG Weight") +
      guides(alpha = FALSE) +
      theme(text = element_text(size = 18)) +
      scale_fill_manual(values = sdg_class_keyword_colors)
    
    return(sdg_class_goal_barplot)
  })
  
  
  
  output$classes_to_keywords <- renderPlot({
    sdg_class_keyword_colors <-  classes %>%
      filter(semester == input$usc_semester1) %>%
      filter(courseID %in% input$usc_classes) %>%
      select(color) %>%
      unique() %>%
      pull()
    
    sdg_class_name <-  classes %>%
      filter(semester == input$usc_semester1) %>%
      filter(courseID %in% input$usc_classes) %>%
      select(course_title) %>% #changes here
      unique() %>%
      pull()
    
    sdg_class_keyword_barplot <- classes %>%
      filter(semester == input$usc_semester1) %>%
      filter(courseID %in% input$usc_classes) %>%
      arrange(desc(weight)) %>%
      ggplot(aes(x = reorder(keyword, weight), y = weight, fill = factor(as.numeric(goal)))) +
      geom_col() +
      coord_flip() +
      labs(title = paste0(sdg_class_name, " (", input$usc_classes, ") ", "\nSDG Keywords"),
           fill = "SDG",
           x = "SDG Keyword",
           y = "Total SDG Weight") +
      scale_fill_manual(values = sdg_class_keyword_colors) +
      theme(text = element_text(size = 18))
    
    # ggsave(plot = sdg_class_keyword_barplot, filename = paste0(input$cmu_classes, "_top_goals.pdf"),
    #    device = "pdf")
    
    return(sdg_class_keyword_barplot)
  })
  
  
  output$classes_to_wordcloud <- renderPlot({
    sdg_class_keyword_colors <-  classes %>%
      filter(semester == input$usc_semester1) %>%
      filter(courseID %in% input$usc_classes) %>%
      distinct(keyword, .keep_all = TRUE) %>%
      select(color) %>%
      pull()
    
    sdg_class_keyword_weights <-  classes %>%
      filter(semester == input$usc_semester1) %>%
      filter(courseID %in% input$usc_classes) %>%
      distinct(keyword, .keep_all = TRUE) %>%
      mutate(weight = 100 * weight) %>%
      select(weight) %>%
      pull()
    
    sdg_class_keywords <-  classes %>%
      filter(semester == input$usc_semester1) %>%
      filter(courseID %in% input$usc_classes) %>%
      distinct(keyword, .keep_all = TRUE) %>%
      select(keyword) %>%
      pull()
    print(length(sdg_class_keywords))
    
    if (length(sdg_class_keywords) == 0) {
      return(ggplot())
    }
    
    # data = data.frame(sdg_class_keywords, sdg_class_keyword_weights)
    # wordcloud2(data, color = sdg_class_keyword_colors,
    #            ordered.colors = TRUE)
    
    sdg_class_keyword_wordcloud <- wordcloud(sdg_class_keywords,
                                             sdg_class_keyword_weights,
                                             colors = sdg_class_keyword_colors,
                                             ordered.colors = TRUE)

    return(sdg_class_keyword_wordcloud)
  })
  
  
  
  output$classes_table = DT::renderDataTable({
    classes %>%
      filter(semester == input$usc_semester1) %>%
      filter(courseID %in% input$usc_classes) %>%
      # rename(Keyword = keyword,
      #        `Keyword Weight` = weight,
      #        Semester = semester,
      #        `Course Number` = section,
      #        SDG = goal,
      #        `Course Department` = course_dept) %>%
      rename(SDG = goal ,Weight = weight, Keyword = keyword, 'Course Description' = course_desc) %>%
      select(SDG, Keyword, Weight, 'Course Description')
  }, rownames=FALSE)
  
  num_top_classes <- 10
  
  # this is for SDGs to classes, table item 3
  output$goals_to_classes <- renderPlot({
    goals_to_classes_barplot <- classes %>%
      filter(semester == input$usc_semester3) %>%
      filter(goal %in% input$sdg_goal1) %>%
      # left_join(classes %>% select(section, courseID), by = "section") %>% 
      # mutate(full_courseID = paste0(courseID, " (", section, ")")) %>%
      group_by(courseID) %>%
      mutate(total_weight = sum(weight)) %>%
      ungroup() %>%
      mutate(courseID1 = fct_reorder(courseID, total_weight)) %>%
      arrange(desc(total_weight)) %>%
      distinct(courseID, .keep_all = TRUE) %>%
      head(num_top_classes) %>%
      ggplot(aes(x = courseID1, y = total_weight)) +
      geom_col(fill = sdg_colors[as.numeric(input$sdg_goal1)], alpha = 1) +
      coord_flip() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
      labs(title = paste0("Top 10 Classes that Map to SDG", input$sdg_goal1),
           x = "Course",
           y = "Total SDG Weight") +
      theme(text = element_text(size = 20))
    
    # ggsave(plot = goals_to_classes_barplot, filename = paste0("sdg_", input$sdg_goal1, "_top_classes.pdf"),
    #        device = "pdf")
    return(goals_to_classes_barplot)
  })
  
  # table for sdgs to classes
  output$top_classes_sdg_table <- DT::renderDataTable({
    classes %>%
      filter(goal %in% input$sdg_goal1) %>%
      filter(semester == input$usc_semester3) %>%
      # left_join(classes %>% select(section, courseID), by = "section") %>% 
      # mutate(full_courseID = paste0(courseID, " (", section, ")")) %>%
      group_by(courseID) %>%
      mutate(total_weight = sum(weight)) %>%
      mutate(courseID1 = fct_reorder(courseID, total_weight)) %>%
      arrange(desc(total_weight)) %>%
      distinct(courseID, .keep_all = TRUE) %>%
      rename('Course ID' = courseID, Semester = semester, "Course Title" = course_title, 'Total SDG Weight'= total_weight, "Course Description" = course_desc) %>%
      select('Course ID', "Course Title", "Course Description", "Total SDG Weight")
  }, rownames=FALSE)
  
  # options = list(
  #     autoWidth = TRUE)
  #     # columnDefs = list(list(width = '200px', targets = "_all"))
  # )
  
  
  ### Visualize SDG Goals
  output$visualize_sdg <- renderPlot({
    sdg_goal_keyword_df <- classes %>%
      filter(goal %in% input$sdg_goal3) %>%
      mutate(weight = 100 * weight) %>%
      distinct(keyword, .keep_all = TRUE) %>%
      arrange(desc(weight)) %>%
      filter(!(keyword %in% exclude_words))
    
    sdg_goal_keyword_wordcloud <- wordcloud(sdg_goal_keyword_df$keyword, 
                                            sdg_goal_keyword_df$weight,
                                            colors = sdg_colors[as.numeric(input$sdg_goal3)])
    
    return(sdg_goal_keyword_wordcloud)
  })
  
  
  #sdg keywords table
  output$keywords_table <- DT::renderDataTable({
    read_csv("filtered_keywords.csv") %>%
      filter(!(keyword %in% exclude_words)) %>%
      filter(goal == input$sdg_goal3) %>%
      rename(Keyword = keyword,
             `Keyword Weight` = weight,
             SDG = goal) %>%
      select(SDG, Keyword, `Keyword Weight`)
  }, rownames=FALSE)
  
  
  # output$pie1 <- renderPlot({
  #   pie_data <- sustainability_related # have not yet filtered by semester, need to add columns to dataframe
  #   # filter(semester == input$usc_semester_pie)
  #   nr = sum(pie_data$sustainability_classification == "Not Related")
  #   foc = sum(pie_data$sustainability_classification == "Focused")
  #   inc = sum(pie_data$sustainability_classification == "Inclusive")
  #   vals=c(nr, foc, inc)
  #   pie_labels <- paste0(round(100 * vals/sum(vals), 2), "%", labels=c(" Not Related", " Focused", " Inclusive"))
  #   result = pie(vals, labels = pie_labels, main="Proportion of Sustainability Related Courses (n=6461)")
  #   return(result)
  # })
  
  # output$pie1_numbers <- renderText({
  #   pie_data <- sustainability_related # have not yet filtered by semester, need to add columns to dataframe
  #   # filter(semester == input$usc_semester_pie)
  #   nr = sum(pie_data$sustainability_classification == "Not Related")
  #   foc = sum(pie_data$sustainability_classification == "Focused")
  #   inc = sum(pie_data$sustainability_classification == "Inclusive")
  #   vals=c(nr, foc, inc)
  #   return(paste("Not Related:", vals[1], "Focused:", vals[2], "Inclusive:", vals[3]))
  # })
  
  
  # output$pie2 <- renderPlot({
  #   pie_data <- sustainability_related # have not yet filtered by semester, need to add columns to dataframe
  #   sum_notrelated = 0
  #   sum_inclusive = 0
  #   sum_focused = 0
  #   for (i in 1:nrow(pie_data)){
  #     if (pie_data$sustainability_classification[i] == "Not Related"){
  #       sum_notrelated = sum_notrelated + pie_data$N.Sections[i]
  #     }
  #     if (pie_data$sustainability_classification[i] == "Inclusive"){
  #       sum_inclusive = sum_inclusive + pie_data$N.Sections[i]
  #     }
  #     if (pie_data$sustainability_classification[i] == "Focused"){
  #       sum_focused = sum_focused + pie_data$N.Sections[i]
  #     }
  #   }
  #   vals=c(sum_notrelated, sum_focused, sum_inclusive)
  #   pie_labels <- paste0(round(100 * vals/sum(vals), 2), "%", labels=c(" Not Related", " Focused", " Inclusive"))
  #   result = pie(vals, labels = pie_labels, main="Sustainability Related Classes (n=19539)")
  #   return(result)
  # })
  
  # output$pie2_numbers <- renderText({
  #   pie_data <- sustainability_related # have not yet filtered by semester, need to add columns to dataframe
  #   sum_notrelated = 0
  #   sum_inclusive = 0
  #   sum_focused = 0
  #   for (i in 1:nrow(pie_data)){
  #     if (pie_data$sustainability_classification[i] == "Not Related"){
  #       sum_notrelated = sum_notrelated + pie_data$N.Sections[i]
  #     }
  #     if (pie_data$sustainability_classification[i] == "Inclusive"){
  #       sum_inclusive = sum_inclusive + pie_data$N.Sections[i]
  #     }
  #     if (pie_data$sustainability_classification[i] == "Focused"){
  #       sum_focused = sum_focused + pie_data$N.Sections[i]
  #     }
  #   }
  #   vals=c(sum_notrelated, sum_focused, sum_inclusive)
  #   return(paste("Not Related:", vals[1], "Focused:", vals[2], "Inclusive:", vals[3]))
  # })
  
  # sustainability related departments
  output$pie3 <- renderPlot({
    pie_data <- sustainability_related %>% filter(year %in% input$usc_year)
    department = unique(pie_data$department) # theres 179 departments
    departments = data.frame(department)
    # split courses into df by department and see if theres focused course or not
    total = length(departments$department)
    notrelated = 0
    inclusive = 0
    focused = 0
    for (i in 1:nrow(departments)){
      mini_df = pie_data[pie_data$department == departments$department[i], ]
      department_classifications = unique(mini_df$sustainability_classification)
      if ("Focused" %in% department_classifications){
        focused = focused + 1
        next
      }
      else if ("Inclusive" %in% department_classifications){
        inclusive = inclusive + 1
        next
      }
      else{
        notrelated = notrelated + 1
      }
    }
    vals=c(notrelated, focused, inclusive)
    labels=c("Not Related", "Focused", "Inclusive")
    pie = data.frame(labels, vals)
    pie2 <- pie %>% 
      mutate(csum = rev(cumsum(rev(vals))), 
             pos = vals/2 + lead(csum, 1),
             pos = if_else(is.na(pos), vals/2, pos))
    # ggplot(pie, aes(x = "", y = vals, fill = labels)) +
    #   geom_col() +
    #   coord_polar(theta = "y")
    ggplot(pie, aes(x = "", y = vals, fill = labels)) +
      geom_col(color = "black") +
      # ggtitle("Title") +
      geom_label_repel(data = pie2,
                       aes(y = pos, label = paste0(vals)),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      geom_text(aes(label = vals),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("#990000", 
                                   "#FFC72C", "#767676")) +
      theme_void()
    # pie_labels <- paste0(round(100 * vals/sum(vals), 2), "%", labels=c("Not Related", " Focused", " Inclusive"))
    # result = pie(vals, labels = pie_labels, main="Sustainability Related Departments (n=179)")
    # return(result)
  })
  
  # output$pie3_numbers <- renderText({
  #   pie_data <- sustainability_related
  #   department = unique(pie_data$department) # theres 179 departments
  #   departments = data.frame(department)
  #   # split courses into df by department and see if theres focused course or not
  #   total = length(departments$department)
  #   notrelated = 0
  #   inclusive = 0
  #   focused = 0
  #   for (i in 1:nrow(departments)){
  #     mini_df = pie_data[pie_data$department == departments$department[i], ]
  #     department_classifications = unique(mini_df$sustainability_classification)
  #     if ("Focused" %in% department_classifications){
  #       focused = focused + 1
  #       next
  #     }
  #     else if ("Inclusive" %in% department_classifications){
  #       inclusive = inclusive + 1
  #       next
  #     }
  #     else{
  #       notrelated = notrelated + 1
  #     }
  #   }
  #   vals=c(notrelated, focused, inclusive)
  #   return(paste("Not Related:", vals[1], "Focused:", vals[2], "Inclusive:", vals[3]))
  #   
  # })
  
  # sustainability courses offered
  output$pie4 <- renderPlot({
    pie_data <- sustainability_related %>% filter(year %in% input$usc_year) 
    sum_notrelated = 0
    sum_inclusive = 0
    sum_focused = 0
    for (i in 1:nrow(pie_data)){
      if (pie_data$sustainability_classification[i] == "Not Related"){
        sum_notrelated = sum_notrelated + pie_data$N.Sections[i]
      }
      if (pie_data$sustainability_classification[i] == "Inclusive"){
        sum_inclusive = sum_inclusive + pie_data$N.Sections[i]
      }
      if (pie_data$sustainability_classification[i] == "Focused"){
        sum_focused = sum_focused + pie_data$N.Sections[i]
      }
    }
    vals=c(sum_notrelated, sum_focused, sum_inclusive)
    labels=c("Not Related", "Focused", "Inclusive")
    pie = data.frame(labels, vals)
    pie = data.frame(labels, vals)
    pie2 <- pie %>% 
      mutate(csum = rev(cumsum(rev(vals))), 
             pos = vals/2 + lead(csum, 1),
             pos = if_else(is.na(pos), vals/2, pos))
    # ggplot(pie, aes(x = "", y = vals, fill = labels)) +
    #   geom_col() +
    #   coord_polar(theta = "y")
    
    # pie(pie$vals, labels=paste(round(prop.table(vals)*100), "%", sep=""), col= c("#767676", "#990000", "#FFC72C"), radius=1)
    
    ggplot(pie, aes(x = "", y = vals, fill = labels)) +
      geom_col(color = "black") +
      # ggtitle("Title") +
      geom_label_repel(data = pie2,
                       aes(y = pos, label = paste0(vals)),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      geom_text(aes(label = vals),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("#990000",
                                   "#FFC72C", "#767676")) +
      theme_void()
  })
  
  # sustainability departments table
  output$sustainability_table <- DT::renderDataTable({
    pie_data <- sustainability_related %>% filter(year %in% input$usc_year)
    department = unique(pie_data$department) # theres 44 departments
    departments = data.frame(department)
    departments["sustainability"] = NA
    departments["focused_classes"] = NA
    for (i in 1:nrow(departments)){
      # grab the course data for this department
      mini_df = pie_data[pie_data$department == departments$department[i], ] # just noticed that some departments are NA
      # remove the classes with no department listed
      mini_df = mini_df[!is.na(mini_df$department),]
      department_classifications = unique(mini_df$sustainability_classification)
      if ("Focused" %in% department_classifications){
        departments[i, "sustainability"] = "Focused"
        # now grab the classes
        classes_df = mini_df[mini_df$sustainability_classification == "Focused", ]
        courses = unique(classes_df$courseID)
        departments[i, "focused_classes"] = paste(courses, collapse = ", ")
        next
      }
      else if ("Inclusive" %in% department_classifications){
        departments[i, "sustainability"] = "Inclusive"
        next
      }
      else{
        departments[i, "sustainability"] = "Not Related"
        next
      }
    }
    
    departments %>% 
      arrange(department) %>%
      rename (Department = department, "Sustainability Classification" = sustainability, "Sustainability-Focused Classes" = focused_classes) %>%
      select(Department, "Sustainability Classification", "Sustainability-Focused Classes")
  }, rownames=FALSE)
  
  
  
  
  
  # output for users classes
  output$user_classes_barplot <- renderPlot({
    
    sdg_class_keyword_colors <-  classes %>%
      filter(courseID %in% input$user_classes) %>%
      arrange(goal) %>% #arranged in order again because multiple classes screwed it up
      select(color) %>%
      unique() %>%
      pull()
    
    sdg_class_name <-  classes %>%
      filter(courseID %in% input$user_classes) %>%
      select(courseID) %>% 
      unique() %>%
      pull()
    
    if (length(sdg_class_keyword_colors) == 0) {return(ggplot())}
    
    sdg_class_keyword_barplot <- classes %>%
      filter(courseID %in% input$user_classes) %>%
      distinct(goal, keyword, .keep_all = TRUE) %>%
      arrange(desc(weight)) %>%
      ggplot(aes(x = reorder(keyword, weight), y = weight, fill = factor(as.numeric(goal)))) +
      geom_col() +
      coord_flip() +
      labs(title = paste0("All Keywords"),
           fill = "SDG",
           x = "SDG Keyword",
           y = "Total SDG Weight") +
      theme(text = element_text(size = 20)) +
      scale_fill_manual(values = sdg_class_keyword_colors)
    
    
    # ggsave(plot = sdg_class_keyword_barplot, filename = paste0(input$cmu_classes, "_top_goals.pdf"),
    #    device = "pdf")
    
    return(sdg_class_keyword_barplot)
  })
  
  # user to goals barplot
  output$user_to_goals <- renderPlot({
    sdg_class_keyword_colors <- classes %>%
      filter(courseID %in% input$user_classes) %>%
      group_by(goal) %>%
      # mutate(sum_weight = sum(weight)) %>%
      # arrange(desc(sum_weight)) %>%
      ungroup() %>%
      distinct(goal, .keep_all = TRUE) %>%
      arrange(goal) %>%
      select(color) %>%
      unique() %>%
      pull()
    # print(sdg_class_keyword_colors)
    if (length(sdg_class_keyword_colors) == 0) {return(ggplot())}
    
    sdg_class_name <-  classes %>%
      filter(courseID %in% input$user_classes) %>%
      select(courseID) %>%
      unique() %>%
      pull()
    # print(sdg_class_name)
    
    sdg_class_goal_barplot <- classes %>%
      filter(courseID %in% input$user_classes) %>%
      distinct(goal, keyword, .keep_all = TRUE) %>%
      group_by(goal) %>%
      mutate(sum_weight = sum(weight)) %>%
      arrange(desc(sum_weight)) %>%
      ungroup() %>%
      distinct(goal, .keep_all = TRUE) %>% 
      ggplot(aes(x = reorder(goal, sum_weight), y = sum_weight, fill = factor(as.numeric(goal)))) +
      geom_col() +
      coord_flip() +
      # geom_hline(yintercept = c(10, 15), color = c("#ffc33c", "#00bc9e")) +
      labs(title = paste0("All Classes"),
           fill = "SDG",
           x = "SDG",
           y = "Total SDG Weight") +
      theme(text = element_text(size = 20)) +
      guides(alpha = FALSE) +
      scale_fill_manual(values = sdg_class_keyword_colors)
    
    return(sdg_class_goal_barplot)
  })
  
  output$user_table <- DT::renderDataTable({
    sustainability_related %>% filter(courseID %in% input$user_classes) %>%
      distinct(courseID, .keep_all = TRUE) %>%
      rename("Course ID" = courseID, "Course Description" = course_desc, "All Goals" = all_goals) %>%
      select("Course ID", "Course Description", "All Goals") %>%
      unique(by=c("courseID"))
  }, rownames=FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)