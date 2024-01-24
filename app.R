library(dplyr)
library(shiny)
library(ggplot2)
library(MASS)
library(ggtext)
library(emojifont)
library(tidyr)
library(reshape2)

ui <- fluidPage(
  
  tags$style("h1 { color: #0570b0;}"),
  tags$style("h2 { color: #0570b0;}"),
  tags$style("h3 { color: #0570b0;}"),
  tags$style("h4 { color: black;}"),
  
  tabsetPanel(
    id = "tabset",
    tabPanel(h2("Introduction"),
             fluidRow(
               column(6,
               h3("Make a New Asthma Medicine"),
               h4("Have you ever wondered how statistics could be useful in the real world?"),
               h4("Imagine that scientists have produced a new type of asthma inhaler. 
                  Any new medicine first needs to be tested in a clinical trial to be sure it works and hasn't any serious side effects."),
               h4("In this app we're going to learn how stats can be used to analyse the results of a clinical trial, and make a decision on whether or not the medicine should be given a licence."),
               h4("Are you ready to be a pharmaceutical statistician? Click on 'Lets Go' to continue..."),
               ),
               column(3,
                      imageOutput("photo"))
             )),
    tabPanel(h2("Let's Go!"),
             fluidRow(
               column(3,
                      sliderInput(inputId = "TE",
                                  label = h4("How Well Does the Medicine Work?"),
                                  min = 0,
                                  max = 25,
                                  value = 10,
                                  step=5)),
               column(3,
                      sliderInput(inputId = "N",
                                  label = h4("How Many Patients?"),
                                  min = 50,
                                  max = 500,
                                  value = 100,
                                  step=10)),
               column(3,
                      sliderInput(inputId = "SD",
                                  label = h4("Variability"),
                                  min = 5,
                                  max = 45,
                                  value = 10,
                                  step=5)),
               column(3,
                      actionButton("simulate", h4("Refresh"), class = "btn-block"))
               
             ),
             fluidRow(
               navlistPanel(well = TRUE,
                 id = "panel",
                 tabPanel(h4("Step 1: Get Some Patients"),
                          fluidRow(
                              h4("The first step is to recruit some asthma patients into the study,
                              and randomly assign each patient to one of two groups: the new medicine (orange) or a control group (green).
                                 Patients further to the right on the below figure had more of an improvement in their asthma."),
                              h4("Q: Do you think there's any difference between patients given the new treatment compared to the control group?
                                 Within each group, did all patients improve by the same amount?"),
    
                              h4("Try experimenting with the sliders, to see the effect on the results."), 
                              h4(""), 
                              h4("Now let's go on to Step 2 to take a closer look..."),                             
                              
                          plotOutput(outputId = "myPlot1"
                                     , width = "800px", height = "450px"
                                     ))
                          ),
                 tabPanel(h4("Step 2: Look at the Data"),
                          fluidRow(
                            h4("This figure provides allows us to compare the results for the two treatment groups. If, for example, the orange curve is shifted
                               to the right compared to the green curve, it shows that the medicine might be effective compared to the control group."),
                            h4(""),
                            h4("Try moving the slider labelled 'How Well Does The Medicine Work' to the right, simulating a more effective medicine. Do you notice any difference in the curves?"),
                            h4("Now try changing the slider labelled 'Variability'. What effect does it have on the curves? "),
                            
                           plotOutput(outputId = "myPlot2", width = "800px",
                                     ))
                          ),
                 tabPanel(h4("Step 3: Making a Decision?"), 
                          fluidRow(
                           h4("To decide whether or not the medicine works, we can use a Confidence Interval (shown below as a coloured bar). If this bar is entirely to the right
                               of the black line, we have strong enough evidence to conclude that the medicine works."),
                            h4("Try changing the sliders to the following settings:"),
                            h4("--How Well Does The Medicine Work? = 10"),   
                            h4("--How Many Patients? = 50"),
                            h4("--Variability=45"),
                            h4("What happens to the colour of the bar? In this app, if the bar goes red it indicates a 'False Negative'. This means that even though in reality the drug works, 
                               the study has drawn a wrong conclusion. The evidence wasn't convincing enough to conclude that the medicine works. (Unlike this app, in a real clinical trial
                               we wouldn't know trhe truth about whether the medicine works.)"), 
                            h4("A false negative is definitely a bad outcome! See if you can fix the problem by increasing the number of patients. One of the jobs of the statistician is to make sure we have enough patients in the study to avoid this happening. 
                               Let's go onto Step 4 to investigate further."),

                          plotOutput(outputId = "myPlot3", width = "800px", height = "150px"
                                     )),
                 ),
                 tabPanel(h4("Step 4:Can We Trust the Results?"),
                          fluidRow(
                   h4("Now imagine we re-run the study several times, with the same settings. The confidence intervals are shown below."),
                   h4("Do we get the same result every time? Another job of a statistian
                      is to understand how much the results are likely to vary from study to study, and how often we see a false negative result."),
                   h4("Now try changing the sliders to the following settings:"),
                   h4("--How Well Does The Medicine Work? = 0"),   
                   h4("--How Many Patients? = 100"),
                   h4("--Variability=20"),
                   h4("Here we are simulating a situation where the new medicine dosen't work. Did you notice any red confidence intervals? (If not, try hitting the Refresh button a few times,)
                      This is an example of a False Positive, i.e. the confidence interval is entirely to the left or right of the black reference line, even though the drug doesn't work. This is a serious
                      situation, because we don't want a medicine that doesn't work to be given to patients (due to possible side effects). Another job of the statistician is to make sure that
                      a false positive result is highly unlikely."),
                   h4("So we've seen that statistics plays an important role in developing new medicines, and in helping pharmaceutical companies and health authorities make the right
                      decisions for patients."),
                   
                   
                          plotOutput(outputId = "myPlot4", width = "800px", height = "800px"))
                 ))
               )
             )
          )
        )
  
server <- function(input, output, session) {
  
  my_data1 <- reactive({
    input$simulate
    N <- as.numeric(input$N)
    sd <- as.numeric(input$SD)
    m_p <- 10
    m_a <- 10 + as.numeric(input$TE)
    sub <- 1:N
    trt <- as.factor(rbinom(N, 1, 0.5))

    data.frame(sub, trt) %>%
      mutate(m = if_else(trt==0, m_p, m_a)) %>%
      mutate(chg = rnorm(N, m, sd))
    })

  my_data2 <- reactive({
    input$simulate
    nsim <- 20
    N <- as.numeric(input$N)
    sd <- rep(as.numeric(input$SD), N*nsim)
    m_p <- 10
    m_a <- 10 + as.numeric(input$TE)
    trt <- as.factor(rbinom(N*nsim, 1, 0.5))
    sim <- crossing(sim=1:nsim, sub=1:N) 
    m = if_else(trt==0, m_p, m_a)
    chg = rnorm(n=sim$sim, mean=m, sd=sd)
    df <- cbind(sim, trt, chg, m)
    summary_stats <- df %>%
      group_by(sim, trt) %>%
      summarise(mean_value = mean(chg),
                sd_value = sd(chg),
                n = n())
    
    ss1 <- dcast(summary_stats, sim ~ trt, value.var="mean_value") %>%
      rename(pla_mn="0", act_mn="1") %>%
      data.frame()
    
    ss2 <- dcast(summary_stats, sim ~ trt, value.var="sd_value") %>%
      rename(pla_sd="0", act_sd="1") %>%
      data.frame()
    
    ss3 <- dcast(summary_stats, sim ~ trt, value.var="n") %>%
      rename(pla_n="0", act_n="1") %>%
      data.frame()
    
    ss4 <- full_join(ss1, ss2, by="sim")
    
    full_join(ss4, ss3, by="sim") %>%
      mutate(diff_mean = act_mn - pla_mn) %>%
      mutate(se_diff = sqrt((act_sd^2/act_n) + (pla_sd^2/pla_n))) %>%
      mutate(ci_lower = diff_mean - qt(0.975, df = act_n + pla_n - 2) * se_diff) %>%
      mutate(ci_upper = diff_mean + qt(0.975, df = act_n + pla_n - 2) * se_diff) %>%
      mutate(error_flag = (input$TE > 0 & ci_lower < 0 & ci_upper > 0 | (input$TE == 0 & (ci_lower > 0 | ci_upper < 0)))) %>%
      mutate(c = if_else(error_flag, "0", "1"))
      
  })

  output$photo <- renderImage({
    list(
      src = "photos/asthma.jpg",
      contentType = "image/jpeg",
      width = 250,
      height = 200
    )
  }, deleteFile = FALSE)
  
  output$photo2 <- renderImage({
    list(
      src = "photos/jury.jpg",
      contentType = "image/jpeg",
      width = 125,
      height = 100
    )
  }, deleteFile = FALSE)
  
     output$myPlot1 <- renderPlot({

    df <- my_data1()
    
     ggplot() +
      scale_x_continuous("Health Score",
                         limits=c(0,60)) +
      scale_y_continuous(" ") +
      scale_color_manual(values=c("#1b9e77", "#d95f02")) +
      theme(plot.title = element_markdown(colour = "#636363",
                                          size = 20),
            panel.background=element_rect(fill="white"),
            panel.grid.major.x=element_line(colour = "#bdbdbd",
                                          linewidth = 0.5,
                                          linetype = 1),
            panel.grid.major.y=element_blank(),
            axis.line.x=element_line(colour = "#bdbdbd",
                                   linewidth = 1,
                                   linetype = 1),
            axis.line.y=element_blank(),
            axis.text.x=element_text(
              colour = "#636363",
              size = 20),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(
              colour = "#636363",
              size = 20),
            axis.title.y=element_blank()
            ) +
       labs(title=("Patients Treated With <span style='color:#1b9e77'>Placebo</span> or <span style='color:#d95f02'>New Medicine</span>")) +
       geom_fontawesome(
         alias = "fa-child",
         x = df$chg,
         y = df$sub,
         size = 10, 
         alpha = 0.6,
         color = ifelse(df$trt == 0, "#1b9e77", "#d95f02")
       ) 
       
     }) 
    
    output$myPlot2 <- renderPlot({
      
      ggplot(my_data1(), aes(x = chg, fill = trt, group = trt)) +
        geom_density(show.legend = FALSE,
                   size=0.25, shape=1, alpha=0.7) +
        scale_x_continuous("Health Score",
                           limits=c(0,60)) +
        # scale_y_continuous("",
        #                    limits=c(0, 0.1)) +
        scale_fill_manual(values=c("#1b9e77", "#d95f02")) +
        theme(plot.title = element_markdown(colour = "#636363",
                                            size = 20),
              panel.background=element_rect(fill="white"),
              panel.grid.major.x=element_line(colour = "#636363",
                                            linewidth = 0.5,
                                            linetype = 1),
              panel.grid.major.y=element_blank(),
              axis.line.x=element_line(colour = "#636363",
                                       linewidth = 1,
                                       linetype = 1),
              axis.line.y=element_blank(),
              axis.text.x=element_text(
                colour = "#636363",
                size = 20),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title=element_text(
                colour = "#636363",
                size = 20)) +
        labs(title=("Patients Treated With <span style='color:#1b9e77'>Placebo</span> or <span style='color:#d95f02'>New Medicine</span>"))
    })

    output$myPlot3 <- renderPlot({
      
      df <- my_data2() %>%
        filter(sim == 1) 

      ggplot(data=df) +
        geom_rect(show.legend = FALSE, aes(xmin = ci_lower, xmax = ci_upper, ymin = (sim - 0.2), ymax = (sim + 0.2), fill = c, color = c)) +
        # geom_point(show.legend = FALSE, aes(x = diff_mean, y = 1, color = c, fill = c), shape = 21, size = 12, alpha = 0.7) +
        # geom_point(aes(x = as.numeric(input$TE), y = 20), shape=5, size = 8, color = "red") +
        # geom_vline(show.legend = FALSE, aes(xintercept = as.numeric(input$TE)), color = "red", linetype = "dotted", size = 0.5) +
        # geom_vline(aes(xintercept = as.numeric(input$TE)), color = "red", linetype = "dotted", size = 1) +
        geom_vline(aes(xintercept = 0), color = "#636363", size = 1) +       scale_x_continuous("Effect of Treatment", limits=c(-50,70)) +
        scale_y_continuous("",
                           limits=c(0,2)) +
        scale_color_manual(values = c("0" = "red", "1" = "blue")) +
        scale_fill_manual(values = c("0" = "red", "1" = "blue")) +
        theme(plot.title = element_markdown(colour = "#636363",
                                            size = 20),
              panel.background=element_rect(fill="white"),
              # panel.border=element_rect(colour = "#636363",
              #                           fill = NULL),
              panel.grid.major.x=element_line(colour = "#f0f0f0",
                                              linewidth = 0.5,
                                              linetype = 1),
              panel.grid.major.y=element_blank(),
              axis.line.x=element_line(colour = "#f0f0f0",
                                       linewidth = 1,
                                       linetype = 1),
              axis.line.y=element_blank(),
              axis.text.x=element_text(
                colour = "#636363",
                size = 20),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title=element_text(
                colour = "#636363",
                size = 20)) 
      
    })
    
    output$myPlot4 <- renderPlot({
  
        ggplot(data=my_data2()) +
        # geom_errorbarh(show.legend = FALSE, aes(xmin = ci_lower, xmax = ci_upper, y = sim, color = c)) +
        geom_rect(show.legend = FALSE, aes(xmin = ci_lower, xmax = ci_upper, ymin = (sim - 0.02), ymax = (sim + 0.02), color = c)) +
        # geom_point(show.legend = FALSE, aes(x = diff_mean, y = sim, color = c, fill = c), shape = 21, size = 2, alpha = 0.7) +
        geom_vline(show.legend = FALSE, aes(xintercept = as.numeric(input$TE)), color = "red", linetype = "dotted", size = 0.5) +
        geom_vline(show.legend = FALSE, aes(xintercept = 0), color = "#636363", size = 1) +
        scale_x_continuous("Effect of Treatment", limits=c(-50,70)
        ) +
        scale_y_continuous(""
                           # ,limits=c(0,21)
                           ) +
        scale_color_manual(values = c("0" = "red", "1" = "blue")) +
        scale_fill_manual(values = c("0" = "red", "1" = "blue")) +
        theme(plot.title = element_markdown(colour = "#636363",
                                            size = 20),
              panel.background=element_rect(fill="white"),
              panel.grid.major.x=element_line(colour = "#f0f0f0",
                                              linewidth = 0.5,
                                              linetype = 1),
              panel.grid.major.y=element_blank(),
              axis.line.x=element_line(colour = "#f0f0f0",
                                       linewidth = 1,
                                       linetype = 1),
              axis.line.y=element_blank(),
              axis.text.x=element_text(
                colour = "#636363",
                size = 20),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title=element_text(
                colour = "#636363",
                size = 20)) 
      
    })
}

shinyApp(ui, server)