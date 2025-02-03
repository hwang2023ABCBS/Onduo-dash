# LIBRARY -----------------------------------------------------------------

# library(shinycssloaders)
# library(shinybusy)
library(rjson)
# library(gt)
library(VennDiagram)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(htmltools)
library(jsonlite)
library(htmlwidgets) #needed for JS() in the reactable()
library(reactable)
library(reactablefmtr) #needed for the reactable color styling
library(leaflet)
library(sf)
library(RColorBrewer) #needed for Joe's part
# library(ggridges) #Joe's
# library(ggokabeito) #Joe's
library(memoise)
library(lubridate)
# library(data.table)
library(tidyverse)
# library(tidytable)
library(plotly)
library(bsicons)
library(bslib)
library(arrow)
library(rio)
library(nanoparquet)
library(pins)
# library(duckplyr)

#source("~/DASHBOARDS/MATERNITY DASHBOARD/functions_inputs.R", echo=TRUE)
source("functions_inputs.R", echo=TRUE)


# options(shiny.error = recover)
# options(shiny.fullstacktrace = TRUE)

# UI  --------------------------------------------------------------

ui <- 
  
  ## HTML -----------------------------------------------------------------
page_navbar(
  theme = bs_theme(bootswatch = 'lumen'),
  tags$style(HTML("
                    .card-body {
                      padding: 0 !important;
                    }
                    ")),
  # Custom CSS to style accordion panels
  tags$style(HTML("
    .cohort1-panel .accordion-body {
      background-color: lightblue; /* Light blue for Cohort 1 */
      color: #000;
    }
    .cohort1-panel .accordion-button {
      background-color: #3B8EA5; /* Darker blue for Cohort 1 button */
      color: white;
    }
    
    .cohort2-panel .accordion-body {
      background-color: #9ee1c6; /* Light purple for Cohort 2 */
      color: #000;
    }
    .cohort2-panel .accordion-button {
      background-color: #66A597; /* Darker purple for Cohort 2 button */
      color: white;
    }
  ")),
  tags$style(HTML("
  .btn-cohort1 {
    background-color: #3B8EA5;
    color: white;
    border-color: #3B8EA5;
  }
  .btn-cohort2 {
    background-color: #66A597;
    color: white;
    border-color: #66A597;
  }
")),
  
  
  
  ## Busy Indicators ----------------------------
  header = tagList(
    useBusyIndicators(),
    busyIndicatorOptions(
      spinner_type = "bars",
      spinner_color = "#4aa3bf"
      #spinner_selector = ".card-color-group-1"
    ),
    busyIndicatorOptions(
      spinner_type = "bars2",
      spinner_size = "100px",
      spinner_color = "#EC0606",
      spinner_selector = ".card-color-group-1"
    ),
    busyIndicatorOptions(
      spinner_type = "dots2",
      spinner_color = "#9ECAE1",
      spinner_size = "100px",
      spinner_selector = ".card-color-group-2"
    ),
    busyIndicatorOptions(
      spinner_type = "pulse",
      spinner_size = "100px",
      spinner_color = "#2D728F",
      spinner_selector = ".card-color-group-3"
    ),
    busyIndicatorOptions(
      spinner_type = "pulse2",
      spinner_size = "110px",
      spinner_color = "#3B8EA5",
      spinner_selector = ".card-color-group-4"
    ),
    busyIndicatorOptions(
      spinner_type = "pulse2",
      spinner_size = "120px",
      spinner_color = "#204652",
      spinner_selector = ".card-color-group-5"
    ),
    busyIndicatorOptions(
      spinner_type = "pulse3",
      spinner_size = "160px",
      spinner_color = "#00befa",
      spinner_selector = ".card-color-group-6"
    ),
    busyIndicatorOptions(
      spinner_type = "pulse3",
      spinner_size = "160px",
      spinner_color = "#0edb88",
      spinner_selector = ".card-color-group-7"
    ),
    busyIndicatorOptions(
      spinner_type = "pulse2",
      spinner_size = "200px",
      spinner_color = "#ff8709",
      spinner_selector = ".card-color-group-8"
    ),
    busyIndicatorOptions(
      spinner_type = "pulse3",
      spinner_size = "160px",
      spinner_color = "#d02d88",
      spinner_selector = ".card-color-group-9"
    )
  ),
  
  title = "ONDUO - Population Health Dashboard",
  # div(class = "main-content-wrapper",
  
  
  
  ## MAIN SIDEBAR ------------------------------------------------------- 
  sidebar = sidebar(
    bg = "#C9CECE",
    open = FALSE,
    actionButton("apply_filters", "Apply Filters", class = "btn btn-primary", #style = "margin-top: 10px;"
    ),
    accordion(
      open = FALSE,
      accordion_panel("General Filters:", icon = bsicons::bs_icon("people-fill"),
                      company_name_input, termed_input, self_funded_input),
      
      accordion_panel("Intervention Filters:", icon = bsicons::bs_icon("headset"),
                      switchInput(inputId = "celeste_filters_toggle", label = "Celeste Filters", labelWidth = 200, value = FALSE, onLabel = "ON", offLabel = "OFF"),
                      celeste_programs_input,  
                      
                      switchInput(inputId = "cm_filters_toggle", label = "CM Filters", labelWidth = 200, value = FALSE, onLabel = "ON", offLabel = "OFF"),
                      cm_programs_input,
                      radioButtons("cm_contact_aggregation", "Choose Engagement Aggregation Type:", 
                                   choices = c("Total Success" = "total", "Percent Success" = "percent")),
                      # radioButtons("cm_contact_level", "Choose Engagement Aggregation Level:", 
                      #              choices = c("Overall" = "overall", "Program" = "program")),
                      selectInput("cm_options_selection", "Engagement Level (Total Success):", 
                                  label = "Total Successful CM Contacts:",
                                  choices = c("0 - No Engagement",
                                              "1 - Low Engagement",
                                              "2 - Moderate Engagement",
                                              "3+ - High Engagement"
                                  ) #end choices
                      ) #end selectInput
      ),
      
      accordion_panel("Maternity Filters:", icon = bsicons::bs_icon("heart-pulse"),
                      switchInput(inputId = "maternity_filters_toggle", label = "Maternity Filters", labelWidth = 300, value = FALSE, onLabel = "ON", offLabel = "OFF"),
                      outcome_input, delivery_type_input, delivery_comp_input, delivery_preterm_input, 
                      last_gestation_input, num_pregnancy_input),
                      
      accordion_panel("Demographic Filters:", icon = bsicons::bs_icon("person-vcard"),
                      age_cat_input, age_group_input, generation_input, gender_input,
                      race_input, minority_input),
      accordion_panel("Location Filters:", icon = bsicons::bs_icon("globe-americas"),
                      county_name_input, state_input, rural_cat_input),
      accordion_panel("Access/Barrier Filters:", icon = bsicons::bs_icon("geo-fill"),
                      socioeconomic_svi_input, housingtransp_svi_input, internet_access_input, maternal_care_desert_input)
    )#end accordion for side bars,
  )#end sidebar
  ,
  nav_spacer(),
  
  
  
  ## OVERVIEW PAGE ------------------------------------------------------------
  nav_panel("Overview",
            #------------Option 2------------# 
            layout_columns(
              fill = FALSE,
              as_fill_carrier( #this is to give the non-fill element div() the correct properties so contents can lay correctly
                # we wrap div() within as_fill_carrier() for it to take effect
                # learn more about the function here:
                # https://rstudio.github.io/bslib/reference/as_fill_carrier.html
                # https://rstudio.github.io/bslib/articles/filling/index.html
                div(
                  # class = "p-0", #this adds padding around the WHOLE div() NOT between its content (goes up to 5)
                  # class = "m-3", #this controls spaces BETWEEN all rows/columns
                  class = "p-0 gap-1 d-flex flex-column",
                  # layout_columns(
                  #   fill = FALSE,
                  #   value_box(title = "Total Members", value = textOutput("member_count"), 
                  #             theme = "bg-gradient-cyan-red", 
                  #             showcase = bs_icon("people-fill"), showcase_layout = "top right",
                  #             # showcase = plotlyOutput("members_seen_by_prov_type"), showcase_layout = "left center",
                  #             # class = "p-1",
                  #             # class = "m-1",
                  #             class = "p-0 m-1",
                  #             # class = "g-3",
                  #             full_screen = FALSE, fill = FALSE, height = 150L
                  #   ),
                  #   value_box(title = "PMPM", value = textOutput("pmpm"), 
                  #             theme = "bg-gradient-teal-red", 
                  #             showcase = bsicons::bs_icon("cash-coin"), showcase_layout = "top right",
                  #             class = "p-0 m-1",
                  #             # class = "p-3",
                  #             # class = "m-3",
                  #             # class = "g-3",
                  #             full_screen = FALSE, fill = FALSE, height = 150L
                  #   ),
                  #   value_box(title = "Program Participation", value = textOutput("program_participation"), 
                  #             theme = "bg-gradient-blue-green", 
                  #             showcase = bsicons::bs_icon("headset"), showcase_layout = "top right",
                  #             class = "p-0 m-1",
                  #             # class = "p-3",
                  #             # class = "m-3",
                  #             # class = "g-3",
                  #             full_screen = FALSE, fill = FALSE, height = 150L
                  #   ),
                  #   
                  #   value_box(title = "Total Members", value = textOutput("member_count"), 
                  #             theme = "bg-gradient-cyan-red", 
                  #             showcase = bs_icon("people-fill"), showcase_layout = "top right",
                  #             # showcase = plotlyOutput("members_seen_by_prov_type"), showcase_layout = "left center",
                  #             # class = "p-1",
                  #             # class = "m-1",
                  #             class = "p-0 m-1",
                  #             # class = "g-3",
                  #             full_screen = FALSE, fill = FALSE, height = 150L
                  #   ),
                  #   value_box(title = "PMPM", value = textOutput("pmpm"), 
                  #             theme = "bg-gradient-teal-red", 
                  #             showcase = bsicons::bs_icon("cash-coin"), showcase_layout = "top right",
                  #             class = "p-0 m-1",
                  #             # class = "p-3",
                  #             # class = "m-3",
                  #             # class = "g-3",
                  #             full_screen = FALSE, fill = FALSE, height = 150L
                  #   ),
                  #   value_box(title = "Program Participation", value = textOutput("program_participation"), 
                  #             theme = "bg-gradient-blue-green", 
                  #             showcase = bsicons::bs_icon("headset"), showcase_layout = "top right",
                  #             class = "p-0 m-1",
                  #             # class = "p-3",
                  #             # class = "m-3",
                  #             # class = "g-3",
                  #             full_screen = FALSE, fill = FALSE, height = 150L
                  #   )
                  # ), #end layout_columns 1 - valueboxes 
                  
                  ### Value Boxes --------------------------------
                  # 1st row
                  fluidRow(
                    column(4,  value_box(title = "Total Members", value = textOutput("member_count"), 
                                                     theme = "bg-gradient-cyan-red",
                                                     showcase = bs_icon("people-fill"), showcase_layout = "top right",
                                                     # showcase = plotlyOutput("members_seen_by_prov_type"), showcase_layout = "left center",
                                                     # class = "p-1",
                                                     # class = "m-1",
                                                     class = "p-0 m-1",
                                                     # class = "g-3",
                                                     full_screen = FALSE, fill = FALSE, height = 150L
                                           )),
                    column(4, value_box(title = "PMPM", value = textOutput("pmpm"), 
                                                    theme = "bg-gradient-teal-red",
                                                    showcase = bsicons::bs_icon("cash-coin"), showcase_layout = "top right",
                                                    class = "p-0 m-1",
                                                    # class = "p-3",
                                                    # class = "m-3",
                                                    # class = "g-3",
                                                    full_screen = FALSE, fill = FALSE, height = 150L
                                          )),
                    column(4, value_box(title = "Program Participation", value = textOutput("program_participation"), 
                                                    theme = "bg-gradient-blue-green",
                                                    showcase = bsicons::bs_icon("headset"), showcase_layout = "top right",
                                                    class = "p-0 m-1",
                                                    # class = "p-3",
                                                    # class = "m-3",
                                                    # class = "g-3",
                                                    full_screen = FALSE, fill = FALSE, height = 150L
                                          ))
                  ),
                  # 2nd row
                  fluidRow(
                    column(4,  value_box(title = "Median Engagement Length(days)", value = textOutput("engagement_length"), 
                                         theme = "bg-gradient-cyan-red",
                                         showcase = bs_icon("calendar"), showcase_layout = "top right",
                                         # showcase = plotlyOutput("members_seen_by_prov_type"), showcase_layout = "left center",
                                         # class = "p-1",
                                         # class = "m-1",
                                         class = "p-0 m-1",
                                         # class = "g-3",
                                         full_screen = FALSE, fill = FALSE, height = 150L
                    )),
                    column(4, value_box(title = "ER Per 1K", value = textOutput("er_pmpm"), 
                                        theme = "bg-gradient-teal-red",
                                        showcase = bsicons::bs_icon("hospital-fill"), showcase_layout = "top right",
                                        class = "p-0 m-1",
                                        # class = "p-3",
                                        # class = "m-3",
                                        # class = "g-3",
                                        full_screen = FALSE, fill = FALSE, height = 150L
                    )),
                    column(4, value_box(title = "Poor Internet Access Rate", value = textOutput("internet_access"), 
                                        theme = "bg-gradient-blue-green",
                                        showcase = bsicons::bs_icon("wifi"), showcase_layout = "top right",
                                        class = "p-0 m-1",
                                        # class = "p-3",
                                        # class = "m-3",
                                        # class = "g-3",
                                        full_screen = FALSE, fill = FALSE, height = 150L
                    ))
                  ),
                  # 3rd row 
                  fluidRow(
                    column(4,  value_box(title = "Avg CM Contact Success Count", value = textOutput("cm_contact_sucess_rate"),
                                         theme = "bg-gradient-cyan-red",
                                         showcase = bs_icon("telephone-outbound-fill"), showcase_layout = "top right",
                                         # showcase = plotlyOutput("members_seen_by_prov_type"), showcase_layout = "left center",
                                         # class = "p-1",
                                         # class = "m-1",
                                         class = "p-0 m-1",
                                         # class = "g-3",
                                         full_screen = FALSE, fill = FALSE, height = 150L
                    )),
                    column(4, value_box(title = "Med Adherence Rate", value = textOutput("med_adherence_rate"),
                                        theme = "bg-gradient-teal-red",
                                        showcase = bsicons::bs_icon("prescription"), showcase_layout = "top right",
                                        class = "p-0 m-1",
                                        # class = "p-3",
                                        # class = "m-3",
                                        # class = "g-3",
                                        full_screen = FALSE, fill = FALSE, height = 150L
                    )),
                    column(4, value_box(title = "GLP-1 Usage(PMPM)", value = textOutput("glp_1_usage"),
                                        theme = "bg-gradient-blue-green",
                                        showcase = bsicons::bs_icon("prescription2"), showcase_layout = "top right",
                                        class = "p-0 m-1",
                                        # class = "p-3",
                                        # class = "m-3",
                                        # class = "g-3",
                                        full_screen = FALSE, fill = FALSE, height = 150L
                    ))
                  ),
                  
                  
                  
                  ### E.Summary -----------------
                  # card(
                  #   card_header(tags$b("Executive Summary")),
                  #   card_body(
                  #     class = "m-1", # give a little space between the edges by increasing the margins
                  #     tags$h4("Overview:"),
                  #     tags$p("This population health dashboard provides insights into the health and well-being of members. 
                  #           Maternal outcomes, access barriers, and healthcare utilization are a few of the metrics used to explore and 
                  #           identify disparities, target resources, and improve healthcare delivery.
                  #           Participation in Case Management and various Celeste vendors are also included as a glimpse on the usage of additonal resources aiming to 
                  #           support members and prevent declines in health."),
                  #     
                  #     # Key Findings:
                  #     tags$h4("Aim & Scope:"),
                  #     # Numbered list using ul()-for unordered bulleted list and li()
                  #     tags$ol(
                  #       tags$li(strong("Supporting Data-Driven Decision-Making: "),
                  #               "Health administrators, policymakers, and clinicians can use the dashboard to track progress over time, measure the impact of interventions, and adjust strategies based on data."),
                  #       
                  #       tags$li(strong("Promoting Preventive Care:"), 
                  #               "Barriers to care increase the risk of complications and more costly forms of utilization. 
                  #               Tracking these metrics, such as maternal care deserts and internet access, allows healthcare providers and policymakers to advocate for improved access in areas, 
                  #               potentially reducing healthcare costs and improving outcomes."),
                  #       
                  #       tags$li(strong("Empowering Community and Patient Engagement: "), 
                  #               "Use data to identify areas where community awareness and engagement is underdeveloped. 
                  #               Help communities advocate for necessary resources and foster partnerships between public health organizations and healthcare providers to tackle various health challenges."),
                  #       
                  #       tags$li(strong("Improving Access to Care: "), 
                  #               "Disparities in health outcomes often reflect broader inequities in healthcare access. 
                  #               This dashboard can highlight geographic or demographic differences in access, guiding policy and resource allocation to underserved areas."),
                  #       
                  #       tags$li(strong("Reducing Complications and Mortality: "), 
                  #               "By tracking health metrics, we can identify areas with high rates of disease and work to prevent avoidable outcomes. 
                  #               This data-driven approach helps improve healthcare policies and interventions aimed at managing chronic conditions, birth outcomes, and other adverse events.")
                  #     ), 
                  #     
                  #     # Next Steps:
                  #     tags$h4("Next Steps:"),
                  #     tags$ul(
                  #       tags$li("Readmission Analysis"),
                  #       tags$li("Provider Analysis w/ Purple Lab"),
                  #     ) # end ul()  
                  #   ), #end card_body() 
                  #   min_height = 400,
                  #   max_height = 400
                  # )#, #end card
                  
                ) #end left side
              ),# end as_fill_carrier()
              
              
              #div(
              #card(
              #title = "Trends",
              navset_card_tab(
                full_screen = TRUE, #full screen option goes in the navset_card_tab() and will affect all nav_panel() within; doesn't seem to work when placed in the nested nav_panel()
                
                ### Member Trends nav_panel ------
                nav_panel(
                  title = "Member Trends",
                  full_screen = TRUE,
                  layout_sidebar(
                    sidebar = sidebar(
                      open = FALSE,
                      position = "right",
                      accordion(
                        icon = bsicons::bs_icon("list"),
                        # checkboxInput("add_trendline", "Show Trendline", value = TRUE),
                        accordion_panel("Plot Options:", icon = bsicons::bs_icon("list"),
                                        radioButtons("member_trends_plot", "",
                                                     choices = c(
                                                       "Total Members" = "total_members",
                                                       "Newly Enrolled" = "new_members",
                                                       "Members Lost" = "lost_members"
                                                     ) #end choices
                                        ) #end radiobuttons)
                        ) #end accordion_panel_1()
                      ) # end accordion()
                    ), #end sidebar()
                    #card_body(
                    plotlyOutput("member_trends")#,
                    #class = "p-0", #removes extra padding (whitespace) around table
                    #)
                  ) #end layout_sidebar()
                ), #end nav_panel() - Member Trends
                
                
                ### Maternity Trends nav_panel ------
                nav_panel(
                  title = "Maternity Trends",
                  full_screen = TRUE,
                  layout_sidebar(
                    sidebar = sidebar(
                      open = FALSE,
                      position = "right",
                      accordion(
                        icon = bsicons::bs_icon("list"),
                        # checkboxInput("add_trendline", "Show Trendline", value = TRUE),
                        accordion_panel("Plot Options:", icon = bsicons::bs_icon("list"),
                                        radioButtons("maternity_trends_plot", "",
                                                     choices = c(
                                                       "Pregnancies Over Time" = "preg_over_time",
                                                       "Delivery Outcome Rates" = "delivery_outcome_rates"
                                                     ) #end choices
                                        ) #end radiobuttons)
                        ) #end accordion_panel_1()
                      ) # end accordion()
                    ), #end sidebar()
                    #card_body(
                    plotlyOutput("maternity_trends")#,
                    #class = "p-0", #removes extra padding (whitespace) around table
                    #)
                  ) #end layout_sidebar()
                ), #end nav_panel() - Member Trends
                
                
                ### Interventions nav_panel ------
                nav_panel(
                  title = "Intervention Trends",
                  layout_sidebar(
                    sidebar = sidebar(
                      open = FALSE,
                      position = "right",
                      radioButtons("intervention_trends_options", "Options:",
                                   choices = c(
                                     "Celeste Vendor Enrollment" = "celeste_vendor_enrollment",
                                     "CM Engagement" = "cm_engagement"
                                   ) #end choices
                      ) #end radiobuttons
                    ), #end sidebar()
                  plotlyOutput("intervention_trends")
                  )
                ), #end nav_panel() - Intervention
                
                ### Cost/Utilization Trends nav_panel ------
                nav_panel(
                  title = "Cost/Utilization Trends",
                  layout_sidebar(
                    sidebar = sidebar(
                      open = FALSE,
                      position = "right",
                      radioButtons("cost_trends_grouping", "Options:",
                                   choices = c(
                                     "ER Rate" = "er",
                                     "IP Rate" = "ip",
                                     "ER Costs/Visits" = "er_normalized",
                                     "IP Costs/Visits" = "ip_normalized",
                                     "Pharmacy Spend" = "pharm",
                                     "PMPM" = "pmpm",
                                     "GLP-1 Usage PMPM" = "glp1_usage_pmpm"
                                   ) #end choices
                      ) #end radiobuttons
                    ), #end sidebar()
                    plotlyOutput("cost_utilization_trends")
                  ) #end layout_sidebar()
                ), #end nav_panel() - Cost/Utilization Trends
                
                ### Cost/Utilization per Visit Trends nav_panel ------
                # nav_panel(
                #   title = "Cost/Utilization per Visit Trends",
                #   layout_sidebar(
                #     sidebar = sidebar(
                #       open = FALSE,
                #       position = "right",
                #       radioButtons("cost_normalized_trends", "Options:",
                #                    choices = c(
                #                      "ER" = "er",
                #                      "IP" = "ip"
                #                    ) #end choices
                #       ) #end radiobuttons
                #     ), #end sidebar
                #     plotlyOutput("cost_normalized_trends")
                #   ), #end layout_sidebar
                # ) #end nav_panel() - Cost/Utilization by Visits Trends
                
                ### BMI change over time nav_panel ------
                nav_panel(
                  title = "BMI Change Over Time",
                  plotOutput("TrendLines_BMI")
                ),
                
                ### Who are they? (Logit model results)-------------
                nav_panel(
                  title = 'Characteristics-Logit Model Results',
                  plotlyOutput("Onduo_Logit_Charateristics")
                )
                
                
                
              ) #end navset_card_tab
              #), # end trends card
              
              
              
              #) #end div - right side
              
            ), #end split - layout_columns() 
            
            ### Bottom Tables ----------
            navset_card_tab(
              full_screen = TRUE,
              height = 200, #need to use height() for navset_card_tab; not min_height() or max_height()
             
              nav_panel(
                title = "YOY Comparison",
                card_body(
                  reactableOutput("yoy_comparison_table"),
                  class = "p-0" #removes extra padding (whitespace) around table; now taken care of in global code at beginning of UI
                ), #end card_body()
              ), #end nav_panel() - YOY Comparison
              nav_panel(
                title = "Maternity Summary",
                card_body(
                  class = "p-0",
                  reactableOutput("maternity_summary_table"))
              ), #end nav_panel() - Pregnancy Breakout
              # nav_panel(
              #   title = "Intervention Summary",
              #   card_body(
              #     class = "p-0",
              #     reactableOutput("intervention_summary_table"))
              # ), #end nav_panel() - Intervention Summary
              nav_panel(
                title = "Chronic Condition Summary",
                card_body(
                  class = "p-0",
                  reactableOutput("cc_summary_table"))
              ), #end nav_panel() - CC Breakout
              nav_panel(
                title = "Characteristics Summary",
                card_body(
                  class = "p-0",
                  reactableOutput("characteristics_summary_table"))
              ), #end nav_panel() - Characteristics Summary
              nav_panel(
                title = "Location Summary",
                card_body(
                  class = "p-0",
                  reactableOutput("location_summary_table"))
              ), #end nav_panel() - Location Summary
              nav_panel(
                title = "Utilization Summary",
                card_body(
                  class = "p-0",
                  reactableOutput("utilization_summary_table"))
              ) #end nav_panel() - Utilization Summary
              # nav_spacer(),
              # nav_panel(
              #   title = "Count/Percent Switch",
              #   card_body(
              #     class = "m-5",
              #     radioButtons("count_pct_switch", "",
              #                  choices = c(
              #                    "Counts" = "count",
              #                    "Percentages" = "pct"
              #                  ) #end choices
              #     ) #end radiobuttons
              #   ) #end card_body()
              # )#, #end nav_panel() - Count/Pct Summary
              # nav_panel(
              #   tooltip(
              #     bs_icon("info-circle"),
              #     "Characteristics & Utilization Summary values are based on unique members.
              #   Other summary tables are based on unique pregnancies."
              #   )
              # )
            ) #end navset_card_tab
            
            
  ), #end nav_panel (Main)

  ## OVERVIEW2- EVENT STUDY -----------------------------------------------------------------------
  nav_panel("Event study",
            #------------Option 2------------# 
            layout_columns(
              fill = FALSE,
              as_fill_carrier( #this is to give the non-fill element div() the correct properties so contents can lay correctly
                # we wrap div() within as_fill_carrier() for it to take effect
                # learn more about the function here:
                # https://rstudio.github.io/bslib/reference/as_fill_carrier.html
                # https://rstudio.github.io/bslib/articles/filling/index.html
                div(
                  # class = "p-0", #this adds padding around the WHOLE div() NOT between its content (goes up to 5)
                  # class = "m-3", #this controls spaces BETWEEN all rows/columns
                  class = "p-0 gap-1 d-flex flex-column",
                  
                  ### Value Boxes --------------------------------
                  # 1st row
                  fluidRow(
                    column(4,  value_box(title = "Total Members", value = textOutput("member_count2"), 
                                         theme = "bg-gradient-cyan-red",
                                         showcase = bs_icon("people-fill"), showcase_layout = "top right",
                                         # showcase = plotlyOutput("members_seen_by_prov_type"), showcase_layout = "left center",
                                         # class = "p-1",
                                         # class = "m-1",
                                         class = "p-0 m-1",
                                         # class = "g-3",
                                         full_screen = FALSE, fill = FALSE, height = 150L
                    )),
                    column(4, value_box(title = "PMPM", value = textOutput("pmpm2"), 
                                        theme = "bg-gradient-teal-red",
                                        showcase = bsicons::bs_icon("cash-coin"), showcase_layout = "top right",
                                        class = "p-0 m-1",
                                        # class = "p-3",
                                        # class = "m-3",
                                        # class = "g-3",
                                        full_screen = FALSE, fill = FALSE, height = 150L
                    )),
                    column(4, value_box(title = "Program Participation", value = textOutput("program_participation2"), 
                                        theme = "bg-gradient-blue-green",
                                        showcase = bsicons::bs_icon("headset"), showcase_layout = "top right",
                                        class = "p-0 m-1",
                                        # class = "p-3",
                                        # class = "m-3",
                                        # class = "g-3",
                                        full_screen = FALSE, fill = FALSE, height = 150L
                    ))
                  ),
                  # 2nd row
                  fluidRow(
                    column(4,  value_box(title = "Median Engagement Length(days)", value = textOutput("engagement_length2"), 
                                         theme = "bg-gradient-cyan-red",
                                         showcase = bs_icon("calendar"), showcase_layout = "top right",
                                         # showcase = plotlyOutput("members_seen_by_prov_type"), showcase_layout = "left center",
                                         # class = "p-1",
                                         # class = "m-1",
                                         class = "p-0 m-1",
                                         # class = "g-3",
                                         full_screen = FALSE, fill = FALSE, height = 150L
                    )),
                    column(4, value_box(title = "ER Per 1K", value = textOutput("er_pmpm2"), 
                                        theme = "bg-gradient-teal-red",
                                        showcase = bsicons::bs_icon("hospital-fill"), showcase_layout = "top right",
                                        class = "p-0 m-1",
                                        # class = "p-3",
                                        # class = "m-3",
                                        # class = "g-3",
                                        full_screen = FALSE, fill = FALSE, height = 150L
                    )),
                    column(4, value_box(title = "Poor Internet Access Rate", value = textOutput("internet_access2"), 
                                        theme = "bg-gradient-blue-green",
                                        showcase = bsicons::bs_icon("wifi"), showcase_layout = "top right",
                                        class = "p-0 m-1",
                                        # class = "p-3",
                                        # class = "m-3",
                                        # class = "g-3",
                                        full_screen = FALSE, fill = FALSE, height = 150L
                    ))
                  ),
                  # 3rd row 
                  fluidRow(
                    column(4,  value_box(title = "Avg CM Contact Success Count", value = textOutput("cm_contact_sucess_rate2"),
                                         theme = "bg-gradient-cyan-red",
                                         showcase = bs_icon("telephone-outbound-fill"), showcase_layout = "top right",
                                         # showcase = plotlyOutput("members_seen_by_prov_type"), showcase_layout = "left center",
                                         # class = "p-1",
                                         # class = "m-1",
                                         class = "p-0 m-1",
                                         # class = "g-3",
                                         full_screen = FALSE, fill = FALSE, height = 150L
                    )),
                    column(4, value_box(title = "Med Adherence Rate", value = textOutput("med_adherence_rate2"),
                                        theme = "bg-gradient-teal-red",
                                        showcase = bsicons::bs_icon("prescription"), showcase_layout = "top right",
                                        class = "p-0 m-1",
                                        # class = "p-3",
                                        # class = "m-3",
                                        # class = "g-3",
                                        full_screen = FALSE, fill = FALSE, height = 150L
                    )),
                    column(4, value_box(title = "GLP-1 Usage", value = textOutput("glp_1_usage2"),
                                        theme = "bg-gradient-blue-green",
                                        showcase = bsicons::bs_icon("prescription2"), showcase_layout = "top right",
                                        class = "p-0 m-1",
                                        # class = "p-3",
                                        # class = "m-3",
                                        # class = "g-3",
                                        full_screen = FALSE, fill = FALSE, height = 150L
                    ))
                  ),
                  
                  #div(
                  #card(
                  #title = "Trends",
                  navset_card_tab(
                    full_screen = TRUE, #full screen option goes in the navset_card_tab() and will affect all nav_panel() within; doesn't seem to work when placed in the nested nav_panel()
                    
                    ### Member Trends nav_panel ------
                    # nav_panel(
                    #   title = "Member Trends",
                    #   full_screen = TRUE,
                    #   layout_sidebar(
                    #     sidebar = sidebar(
                    #       open = FALSE,
                    #       position = "right",
                    #       accordion(
                    #         icon = bsicons::bs_icon("list"),
                    #         # checkboxInput("add_trendline", "Show Trendline", value = TRUE),
                    #         accordion_panel("Plot Options:", icon = bsicons::bs_icon("list"),
                    #                         radioButtons("member_trends_plot", "",
                    #                                      choices = c(
                    #                                        "Total Members" = "total_members",
                    #                                        "Newly Enrolled" = "new_members",
                    #                                        "Members Lost" = "lost_members"
                    #                                      ) #end choices
                    #                         ) #end radiobuttons)
                    #         ) #end accordion_panel_1()
                    #       ) # end accordion()
                    #     ), #end sidebar()
                    #     #card_body(
                    #     plotlyOutput("member_trends2")#,
                    #     #class = "p-0", #removes extra padding (whitespace) around table
                    #     #)
                    #   ) #end layout_sidebar()
                    # ), #end nav_panel() - Member Trends
                    
                    
                    # ### Maternity Trends nav_panel ------
                    # nav_panel(
                    #   title = "Maternity Trends",
                    #   full_screen = TRUE,
                    #   layout_sidebar(
                    #     sidebar = sidebar(
                    #       open = FALSE,
                    #       position = "right",
                    #       accordion(
                    #         icon = bsicons::bs_icon("list"),
                    #         # checkboxInput("add_trendline", "Show Trendline", value = TRUE),
                    #         accordion_panel("Plot Options:", icon = bsicons::bs_icon("list"),
                    #                         radioButtons("maternity_trends_plot", "",
                    #                                      choices = c(
                    #                                        "Pregnancies Over Time" = "preg_over_time",
                    #                                        "Delivery Outcome Rates" = "delivery_outcome_rates"
                    #                                      ) #end choices
                    #                         ) #end radiobuttons)
                    #         ) #end accordion_panel_1()
                    #       ) # end accordion()
                    #     ), #end sidebar()
                    #     #card_body(
                    #     plotlyOutput("maternity_trends")#,
                    #     #class = "p-0", #removes extra padding (whitespace) around table
                    #     #)
                    #   ) #end layout_sidebar()
                    # ), #end nav_panel() - Member Trends
                    
                    
                    ### Interventions nav_panel ------
                    # nav_panel(
                    #   title = "Intervention Trends",
                    #   layout_sidebar(
                    #     sidebar = sidebar(
                    #       open = FALSE,
                    #       position = "right",
                    #       radioButtons("intervention_trends_options", "Options:",
                    #                    choices = c(
                    #                      "Celeste Vendor Enrollment" = "celeste_vendor_enrollment",
                    #                      "CM Engagement" = "cm_engagement"
                    #                    ) #end choices
                    #       ) #end radiobuttons
                    #     ), #end sidebar()
                    #     plotlyOutput("intervention_trends2")
                    #   )
                    # ), #end nav_panel() - Intervention
                    
                    ### Cost/Utilization Trends nav_panel ------
                    nav_panel(
                      title = "Cost/Utilization Trends- Event Study",
                      layout_sidebar(
                        sidebar = sidebar(
                          open = FALSE,
                          position = "right",
                          radioButtons("cost_utilization_trends_grouping2", "Options:",
                                       choices = c(
                                         "ER Rate" = "er",
                                         "IP Rate" = "ip",
                                         "OP Rate" = "op",
                                         "BH Rate" = "bh",
                                         "Telehealth Rate" = "telehealth",
                                         "Allowed" = "allowed",
                                         "Allowed Pharmacy"= "allowed_pharmacy",
                                         "Paid" = "paid",
                                         "Paid Pharmacy" = "paid_pharmacy"
                                       ) #end choices
                          ) #end radiobuttons
                        ), #end sidebar()
                        plotlyOutput("cost_utilization_trend_eventStudy")
                      ) #end layout_sidebar()
                    ), #end nav_panel() - Cost/Utilization Trends
                    
                    
                    
                    ### BMI change over time nav_panel ------
                    # nav_panel(
                    #   title = "BMI Change Over Time",
                    #   plotOutput("TrendLines_BMI2")
                    # ),
                    
                    ### Who are they? (Logit model results)-------------
                    # nav_panel(
                    #   title = 'Characteristics-Logit Model Results',
                    #   plotlyOutput("Onduo_Logit_Charateristics2")
                    # )
                    
                    
                    ### Group by conditions Cost/Utilization Trends nav_panel ------
                    nav_panel(
                      
                      title = "Cost/Utilization Trends- Group By Conditions",
                      layout_sidebar(
                        sidebar = sidebar(
                          open = FALSE,
                          position = "right",
                          radioButtons("cost_utilization_trends_grouping3", "Options:",
                                       choices = c(
                                         "ER Rate" = "er",
                                         "IP Rate" = "ip",
                                         "OP Rate" = "op",
                                         "BH Rate" = "bh",
                                         "Telehealth Rate" = "telehealth",
                                         "Allowed" = "allowed",
                                         "Allowed Pharmacy"= "allowed_pharmacy",
                                         "Paid" = "paid",
                                         "Paid Pharmacy" = "paid_pharmacy"
                                       ) #end choices
                          ) #end radiobuttons
                        ), #end sidebar()
                        plotlyOutput("cost_utilization_trend_eventStudy_groupbyConditions")
                      ) #end layout_sidebar()
                      
                    )
                    
                    
                    
                    
                  ) #end navset_card_tab
                  #), # end trends card
                  
                  
                  
                  #) #end div - right side
                  
                ) #end split - layout_columns() 
                
                
                
                
              )
            )
  ),
  
  
  
  
  
  
  ## MAPPING -----------------------------------------------------------------
  
  nav_panel("Geography",
            
            layout_column_wrap(
              # width = 1/2,
              
              # #LEFT SIDE:
              # card(
              #   full_screen = TRUE,
              #   class = "card-color-group-3",
              #   card_header("Member Map"),
              #   card_body(leafletOutput("map"))
              # ), #end Map card()
              
              #RIGHT SIDE:
              card(
                full_screen = TRUE,
                class = "card-color-group-8",
                card_header("Map"),
                layout_sidebar(
                  sidebar = sidebar(
                    open = TRUE,
                    position = "right",
                    
                    # actionButton("update_map", "Update Map", class = "btn btn-primary"),
                    
                    accordion(
                      icon = bsicons::bs_icon("globe-americas"),
                      
                      accordion_panel(
                        open = TRUE,
                        "Map Options:", 
                        icon = bsicons::bs_icon("list"),
                        radioButtons("map_options", "",
                                     choices = c(
                                       "State - County Level" = "state_county",
                                       "National - State Level" = "national_state"
                                     ), #end choices
                                     selected = "state_county"
                        ), #end radiobuttons
                        conditionalPanel(
                          condition = ("input.map_options == 'state_county'"),
                          selectInput("state_selection", "State Options:",
                                      choices = state.abb, selected = "AR" 
                                      )
                        ), # end conditonalPanel()
                        radioButtons(
                          "label_option",
                          "Choose Labels:",
                          choices = c("County Name" = "name", "County Value" = "value", "None" = "none"),
                          selected = "name"
                        )
                        
                      ), #end accordion_panel_1
                      accordion_panel(
                        open = TRUE,
                        "Value Options:", 
                        icon = bsicons::bs_icon("list"),
                        radioButtons("map_value_options", "",
                                     choices = c(
                                       "Member Count" = "member_count",
                                       "Single Variable" = "single_var",
                                       "Multi Variable - Ranking (Coming Soon!)" = "ranking"
                                     ), #end choices
                                     selected = "member_count"
                        ) #end radiobuttons,
                        
                        
                      ), #end accordion_panel_1()
                      accordion_panel(
                        open = FALSE,
                        "Bin Breaks:", 
                        icon = bsicons::bs_icon("list"),
                        # bin break options:
                        radioButtons("map_bin_options", "",
                                     choices = c(
                                       "Default" = "default",
                                       "Custom" = "custom"
                                     ), #end choices
                        ), #end radiobuttons
                        conditionalPanel(
                          condition = "input.map_bin_options == 'custom'",
                          textInput(
                            inputId = "custom_breaks",
                            label = "Custom Bin Breaks (comma-separated):",
                            placeholder = "e.g., 1, 50, 100, 500, 1000, 5000, Inf",
                            value = "1, 50, 100, 500, 1000, 5000, Inf"
                          ) #end textInput()
                        ), #end conditionalPanel()
                      ), #end accordion_panel_2()  
                      
                    ), #end accordion()
                    
                    
                    
                    ### numeric single variable -----------------
                    # user chooses aggregation type:
                    conditionalPanel(
                      condition = "input.map_value_options == 'single_var'",
                      radioButtons("map_single_aggregation", "Aggregation Options:", 
                                   choices = c("Sum" = "Sum", "Average" = "Average", "Median" = "Median", "Rate per 100" = "Rate_per_100"),
                                   selected = "Average"),
                    ),
                    
                    # option appears for type:
                    conditionalPanel(
                      condition = "input.map_value_options == 'single_var'",
                      radioButtons("map_single_var_type", "Variable Type:", 
                                   choices = c("Demographic" = "Demographic", "Conditions" = "Conditions", 
                                               "Utilization" = "Utilization", "Environmental" = "Environmental"),
                                   selected = "Conditions"),
                    ),
                    
                    # the 4 options appear based on the selection:
                    conditionalPanel(
                      condition = "input.map_value_options == 'single_var'",
                      varSelectInput("map_single_selection", "Condition Options:", 
                                     Static_Mbr_Level %>% select(any_of(selection_6)))
                    )
                    
                    
                    
                    # conditionalPanel(
                    #   condition = "input.map_single_var_type == 'demographic' & input.map_value_options == 'single_var'",
                    #   varSelectInput("map_single_selection", "Demographic Options:", 
                    #                  Static_Mbr_Level %>% select(any_of(selection_7)), selected = "Female")
                    # ),
                    # conditionalPanel(
                    #   condition = "input.map_single_var_type == 'conditions' & input.map_value_options == 'single_var'",
                    #   varSelectInput("map_single_selection", "Condition Options:", 
                    #                  Static_Mbr_Level %>% select(any_of(selection_6)), selected = "HYPERTENSION")
                    # ),
                    # conditionalPanel(
                    #   condition = "input.map_single_var_type == 'utilization' & input.map_value_options == 'single_var'",
                    #   varSelectInput("map_single_selection", "Utilization Options:", 
                    #                  Static_Mbr_Level %>% select(any_of(selection_9)), selected = "ER")
                    # ),
                    # conditionalPanel(
                    #   condition = "input.map_single_var_type == 'environmental' & input.map_value_options == 'single_var'",
                    #   varSelectInput("map_single_selection", "Environmental Options:", 
                    #                  Static_Mbr_Level %>% select(any_of(selection_8)), selected = "Internet_Access_County")
                    # )
                    
                  ), #end sidebar()
                card_body(
                  class = "m-0 p-0",
                  leafletOutput("map")
                  ) #end card_body()
                ) #end layout_sidebar()
              ) #end Location card()
              
            ) #end layout_column_wrap()
            
  ), # END GEOGRAPHY nav_panel
  

  ## UTILIZATION  -----------------------------------------------------
  nav_menu("Utilization",
           
           ### Event Study ----------------------------------
           nav_panel("IP/ER Patterns",
                     
                     #layout_columns(
                       
                       #LEFT SIDE:
                       as_fill_carrier(
                         div(
                           # class = "p-0 gap-1 d-flex flex-column",
                           layout_columns(
                             # this sliderInput is the only thing in layout_columns(), 
                             # but I'm using layoutcolumns so I can control the height the sliderInput() gets
                             height = 100,
                             sliderInput("event_study_period_range", "Period Window (Months):", min = -24, max = 24, value = c(-12, 12), step = 1, round = TRUE), 
                           ), #end layout columns
                           card(
                             class = "card-color-group-6",
                             full_screen = TRUE,
                             # min_height = 700,
                             card_header("Event Study"), #end card_header
                             layout_sidebar(
                               sidebar = sidebar(
                                 radioButtons("event_study_event_selection", "Select Event:", choices = c("Enrollment" = "start", "Celeste Program Enroll" = "celeste_enroll", "Recent (minus last 3 months)" = "three_months_ago"), selected = "start"),
                                 # radioButtons("event_study_time_grouping", "Select Aggregation:", choices = c("Weeks" = "week", "Months" = "month"), selected = "month"),
                                 conditionalPanel(
                                   condition = "input.event_study_event_selection == 'celeste_enroll'",
                                   pickerInput("celeste_vendor", "Celeste Vendor:", choices = sort(unique(Celeste_Programs$VENDOR), na.last = T), 
                                               selected = unique(Celeste_Programs$VENDOR),
                                               multiple= TRUE, pickerOptions(actionsBox = TRUE))
                                 ),
                                 
                                 radioButtons("admission_type_1", "Choose Admission Type:", choices = c("ER" = "er", "IP" = "ip")),
                                 radioButtons("event_study_codes", "Diag Code Options:", choices = c("All" = "all", "Specific Codes" = "specific"), selected = "all"),
                                 # selectInput("code_prefix", "Diag Code Prefix:", choices = LETTERS),
                                 conditionalPanel(
                                   condition = "input.event_study_codes == 'specific'",
                                   selectInput("code_prefix", "Diag Code Prefix:", choices = LETTERS)
                                 )#,
                                 # radioButtons("event_study_options", "Choose Breakout:", choices = c("General" = "general", "By Intervention" = "intervention"))
                               ), #end sidebar
                               card_body(
                                 class = "m-1",
                                 min_height = 450,
                                 plotlyOutput("utilization_patterns_event_study"),
                               ),
                               card_body(
                                 # class = "m-1 lead container",
                                 tags$p("The event study looks at member activity before and after an event.
                                 All members have their timeline oriented around the event.
                                 Period represents the number of months or weeks relative to the event."
                                 ), #end tags$p
                                 tags$ul(
                                   tags$li("Pre-window (-n): Months or weeks before the user selected pregnancy event (-1 ~ 1 month/week before event)"),
                                   tags$li("Event (0): The day of the last menstrual period (LMP) or pregnancy outcome (user directed)"),
                                   tags$li("Post-window (n): Months or weeks after the user selected pregnancy event (1 ~ 1 month/week after event)")
                                 ) #end tags$ul
                               ) #end card_body
                             ) # end layout_sidebar
                           ), #end card - Event Study
                         ) #end div()
                       ), #end as_fill_carrier()
                       
                       #RIGHT SIDE:
                     #   as_fill_carrier(
                     #     div( #right side
                     #       class = "p-0 gap-1 d-flex flex-column", # we need extra padding between elements on right side
                     #       card(
                     #         min_height = 200,
                     #         max_height = 300,
                     #         card_header("Event Study Description"),
                     #         card_body(
                     #           class = "m-3",
                     #           tags$p(
                     #             "The event study looks at member activity before and after an event. 
                     #                   All members have their timeline oriented around the event. 
                     #                   Period represents the number of months or weeks relative to the event."
                     #           ), #end tags$p
                     #           tags$ul(
                     #             tags$li("Pre-window (-n): Months or weeks before the user selected pregnancy event (-1 ~ 1 month/week before event)"),
                     #             tags$li("Event (0): The day of the last menstrual period (LMP) or pregnancy outcome (user directed)"),
                     #             tags$li("Post-window (n): Months or weeks after the user selected pregnancy event (1 ~ 1 month/week after event)")
                     #           ) #end tags$ul
                     #         ) #end card_body
                     #       ), #end card - description
                     #       card(
                     #         full_screen = TRUE,
                     #         min_height = 400,
                     #         class = "card-color-group-6",
                     #         card_header("Overall Counts"),
                     #         card_body(reactableOutput("utilization_patterns_table"))
                     #       ) #end card - Overall Counts (utilization patterns table)
                     #       
                     #     ) #end div
                     #   ) #end as_fill_carrier
                     # )# end layout_columns
           ), #end nav_panel IP/ER patterns
           
           
           ### Diagnosis Codes---------------------------
           nav_panel("Admitting Diagnosis",
                     layout_columns(
                       as_fill_carrier(
                         div(
                           class = "p-0 gap-3 d-flex flex-column",
                           layout_columns(
                             radioButtons("admission_type_2", "Choose Admission Type:", choices = list("ER" = "er", "IP" = "ip"), selected = "er"),
                             #this is dynamic and is handled in the server based on radioButton input
                             selectInput("diagnosis_col_type", "Choose Diagnosis Column:", choices = c("MEDICAL_DIAG_DESC_CODE",
                                                                                                       "DIAG1_SUPERGROUPER_DESC", "DIAG1_GROUPER_DESC",
                                                                                                       "DIAG2_SUPERGROUPER_DESC", "DIAG2_GROUPER_DESC",
                                                                                                       "DIAG3_SUPERGROUPER_DESC", "DIAG3_GROUPER_DESC") 
                             ) #end selectInput
                           ), #end layout_columns
                           card(
                             class = "card-color-group-9",
                             full_screen = TRUE,
                             card_header("Diagnosis Code Related to Admission"),
                             reactableOutput("admission_diags_table"),
                             min_height = 650,
                             # max_height = 750
                           ) #end card()
                         ), #end div()
                       ), #end as_fill_carrier
                       as_fill_carrier(
                         div(
                           layout_columns(
                             height = 100,
                             sliderInput("top_n_utilization", "Top Diagnosis to Show:", min = 1, max = 15, value = 10),
                           ),
                           card(
                             class = "card-color-group-9",
                             full_screen = TRUE,
                             card_header("Visualize Top Conditions"),
                             plotlyOutput("admission_diags_bar_chart"),
                             min_height = 650
                             #max_height = 600
                           )#end card()
                         ) #end div
                       ) #end as_fill_carrier()
                     ) #end layout_columns
           )#, #end nav_panel Admitting Diagnosis
           # nav_panel("Length of Stay"),
           # nav_panel("Place of Service"),
           # nav_panel("Cost")
           
  ), #end nav_menu Utilization
  
  ## BH/SEVERITY ---------------------------------------------------------
  nav_panel("Mental Health",
            layout_columns(
              
              #LEFT SIDE:
              as_fill_carrier(
                div(
                  class = "p-0 gap-1 d-flex flex-column",
                  layout_columns(
                    # this sliderInput is the only thing in layout_columns(), 
                    # but I'm using layoutcolumns so I can control the height the sliderInput() gets
                    height = 100,
                    sliderInput("event_study_period_range_bh", "Period Window (Months):", min = -24, max = 24, value = c(-12, 12), step = 1, round = TRUE), 
                  ), #end layout columns
                  card(
                    class = "card-color-group-7",
                    full_screen = TRUE,
                    min_height = 650,
                    card_header("Event Study - Behavioral Health"), 
                    layout_sidebar(
                      sidebar = sidebar(
                        radioButtons("event_study_event_selection_bh", "Select Event:", choices = c("Enrollment" = "start", "Celeste Program Enroll" = "celeste_enroll", "Recent (minus last 3 months)" = "three_months_ago"), selected = "start"),
                        # radioButtons("event_study_time_grouping_bh", "Select Aggregation:", choices = c("Weeks" = "week", "Months" = "month"), selected = "month"),
                        conditionalPanel(
                          condition = "input.event_study_event_selection_bh == 'celeste_enroll'",
                          pickerInput("celeste_vendor_bh", "Celeste Vendor:", choices = sort(unique(Celeste_Programs$VENDOR), na.last = T), 
                                      selected = unique(Celeste_Programs$VENDOR),
                                      multiple= TRUE, pickerOptions(actionsBox = TRUE))
                        ), #end conditionalPanel()
                        
                        radioButtons("admission_type_3", "Choose Admission Type:", choices = c("BH ER" = "er", "BH IP" = "ip")),
                        # radioButtons("event_study_remove_o_codes_bh", "Visits Related to Pregnancy (O-Codes):", choices = c("Keep" = "keep", "Remove" = "remove", "Specific Codes" = "specific"), selected = "remove"),
                        # selectInput("code_prefix", "Diag Code Prefix:", choices = LETTERS),
                        # conditionalPanel(
                        #   condition = "input.event_study_remove_o_codes_bh == 'specific'",
                        #   selectInput("code_prefix_bh", "Diag Code Prefix:", choices = LETTERS)
                        # )#,
                        # radioButtons("event_study_options_bh", "Choose Breakout:", choices = c("General" = "general", "By Intervention" = "intervention"))
                      ),
                      plotlyOutput("utilization_patterns_event_study_bh")
                    )
                  ) #end card - BH Event Study
                ) #end div() for LEFT SIDE
                
              ), #end as_fill_carrier
              
              card(
                full_screen = TRUE,
                class = "card-color-group-7",
                card_header("Mental Health Conditions"),
                card_body(reactableOutput("bh_conditions_general_table")) #end card_body
              )
              
              #RIGHT SIDE:
              # navset_card_tab(
              #   full_screen = TRUE,
              #   nav_panel(
              #     title = "BH Condition Breakout",
              #     class = "card-color-group-7",
              #     card_body(reactableOutput("bh_conditions_general_table")) #end card_body
              #   ), #end nav_panel BH Condition Breakout
              #   nav_panel(
              #     title = "MH Significance",
              #     class = "card-color-group-7",
              #     card_body(lorem::ipsum(paragraphs = 2, sentences = 5)) #add filler text)
              #   ) #end nav_panel MH Description
              # ) #navset_card_tab() 
              
            ), #end layout columns
  ), #END nav_panel Mental Health
  
  
  
  ## CHARACTERISTICS ---------------------------------------------------------
  nav_panel("Member Characteristics",
            layout_columns(
              #fill = FALSE,
              value_box(title = "Avg Age", value = textOutput("avg_age"), 
                        theme = value_box_theme(bg = "#4f7a90", fg = "white"),
                        showcase = bsicons::bs_icon("clock-fill")),
              value_box(title = "Female", value = textOutput("avg_female"), 
                        theme = value_box_theme(bg = "#4f7a90", fg = "white"),
                        # theme = "bg-gradient-teal-purple", 
                        showcase = bsicons::bs_icon("gender-female")),
              value_box(title = "Total Members", value = textOutput("member_count2"), 
                        theme = value_box_theme(bg = "#4f7a90", fg = "white"),
                        # theme = "bg-gradient-blue-green", 
                        showcase = bsicons::bs_icon("people-fill")),
              value_box(title = "Living Rural", value = textOutput("total_rural"), 
                        theme = value_box_theme(bg = "#4f7a90", fg = "white"),
                        # theme = "bg-gradient-teal-purple", 
                        showcase = bsicons::bs_icon("house-door-fill")),
              value_box(title = "Facing Socioeconomic Barriers", value = textOutput("avg_socio_barriers"), 
                        theme = value_box_theme(bg = "#4f7a90", fg = "white"),
                        # theme = "bg-gradient-blue-yellow", 
                        showcase = bsicons::bs_icon("cash-stack"))
            ), #end value box layout_columns(),
            layout_column_wrap(
              fill = TRUE,
              fillable = TRUE, 
              width = 1/2, # will put 2 cards on each row; without all 4 cards will be on the same row with layout_column_wrap()
              card(
                class = "card-color-group-5",
                full_screen = TRUE,
                card_header("Categorical Variables",
                            div(
                              varSelectInput("barchart1_input", "", 
                                             Static_Mbr_Level %>% select(any_of(selection_2)), 
                                             selected = "Rural_County_Cat"
                              ), # end varSelectInput
                              style = "margin-bottom: 0; padding-bottom:0;", #Remove margin/padding the input in hopes of getting rid of empty space from its title
                              tags$style(HTML("#barchart1_input-label { display: none; }")) #don't forget that # or it won't work
                            ), #end div()
                            class = "d-flex justify-content-between"),
                card_body(
                  plotlyOutput("barchart1_output"),
                  class = "p-2"
                )
              ),
              card(
                class = "card-color-group-5",
                full_screen = TRUE,
                card_header("Numeric Variables",
                            div(
                              varSelectInput("histogram_input", "", 
                                             Static_Mbr_Level %>% select(any_of(selection_1)), 
                                             selected = "Age"
                              ), # end varSelectInput
                              style = "margin-bottom: 0; padding-bottom:0;", #Remove margin/padding the input in hopes of getting rid of empty space from its title
                              tags$style(HTML("#histogram_input-label { display: none; }"))
                            ), #end div()
                            class = "d-flex justify-content-between"), #the class part helps create the space btw the 2 so the content doesn't jump to next line
                card_body(
                  plotlyOutput("histogram_output"),
                  class = "m-2" 
                ) #end card_body()
              ), #end card
              card(
                class = "card-color-group-5",
                full_screen = TRUE,
                card_header("Social Vulnerability Index", 
                            div(
                              varSelectInput("svi_input", "", 
                                             Static_Mbr_Level %>% select(any_of(selection_3)), 
                                             selected = "Socioeconomic_SVI_Cat"
                              ), # end varSelectInput 
                              style = "margin-bottom: 0; padding-bottom:0;", #Remove margin/padding the input in hopes of getting rid of empty space from its title
                              tags$style(HTML("#svi_input-label { display: none; }"))
                            ), #end div()
                            class = "d-flex justify-content-between"),
                card_body(
                  plotlyOutput("svi_output"),
                  class = "m-2"
                ) # end card body
              ), #end card()
              card(
                class = "card-color-group-5",
                full_screen = TRUE,
                card_header("Top Conditions"),
                card_body(
                  plotlyOutput("top_cc_output"),
                  class = "m-2"  
                ), # end card body
              ) # end card()
            ) # end layout_column_wrap() 
  ), #END Characteristics nav_panel()
  
  ## VENN DIAGRAM ---------------------------------------------------------
  nav_panel("Condition Intersection",
            navset_card_tab(
              nav_panel(
                title = "Conditions Intersection",
                layout_sidebar(
                  sidebar = sidebar(
                    open = TRUE,
                    position = "right",
                    venn_1_input, venn_2_input, venn_3_input,
                  ), #end sidebar()
                  card_body(
                    plotOutput("conditions_venn_diagram")
                  )
                ) #end layout_sidebar()
              ), #end nav_panel() - Conditions Intersection
              nav_panel(
                title = "Condition Category Intersection",
                layout_sidebar(
                  sidebar = sidebar(
                    open = TRUE,
                    position = "right",
                    venn_4_input, venn_5_input, venn_6_input,
                  ), #end sidebar()
                  plotOutput("conditions_venn_diagram_cc_category")
                ) #end layout_sidebar()
              ),
              nav_panel(
                title = "Vendors Interaction",
                layout_sidebar(
                  sidebar = sidebar(
                    open = TRUE, 
                    position = "right",
                    venn_7_input,venn_8_input,venn_9_input,
                  ), # end sidebar()
                  plotOutput("vendors_venn_diagram")
                ) # end of layout_sidebar
              ),#end nav_panel() - Vendors Intersection
              nav_panel(
                title = "Programs Interaction",
                layout_sidebar(
                  sidebar = sidebar(
                    open = TRUE, 
                    position = "right",
                    venn_10_input,venn_11_input,venn_12_input,
                  ), # end sidebar()
                  plotOutput("programs_venn_diagram")
                ) # end of layout_sidebar
              ),#end nav_panel() - Programs Intersection
              
              
            ) #end navset_card_tab
            
  ), #END Venn Diagram nav_panel (page)
  
  
  
  ## LINKS/RESOURCES ---------------------------------------------------------         
  nav_menu("Links/Resources",
           
           nav_item(tags$a("Maternity Dashboard", href = "https://rconssweb01.abcbs.net/HE_Maternity_Dashboard/")),
           nav_item(tags$a("March of Dimes - AR Report", href = "https://www.marchofdimes.org/peristats/reports/arkansas/report-card")),
           nav_item(tags$a("Maternal Scorecard - Primrose Project", href = "https://ar.maternalhealth.us/")),
           
           nav_item(tags$a("Health Economics Website", href = "https://rconssweb01.abcbs.net/Health-Econ-Website/")),
           nav_item(tags$a("Pharmacy Drug Comporator", href = "https://rconssweb01.abcbs.net/Pharmacy_Drug_Comparator/")),
           nav_item(tags$a("Procedure Codes Event Study", href = "https://rconssweb01.abcbs.net/PrePost_by_CPT/")),
           
           nav_item(tags$a("Social Vulnerability Index (SVI)", href = "https://www.atsdr.cdc.gov/place-health/php/svi/index.html"))
           
  ), #END nav_menu Links/Resources
  nav_spacer(),
           # nav_panel("Additional Details",
           #           card(
           #             full_screen = TRUE,
           #             
           #             card_header(tags$b("Additional Details")),
           #             card_body(
           #               class = "m-1", # give a little space between the edges by increasing the margins
           #               tags$h4("Methodology Distinctions:"),
           #               tags$p(""),
           #               
           #               # Definitions:
           #               tags$h4("Definitions:"),
           #               # Numbered list using ul()-for unordered bulleted list and li()
           #               tags$ol(
           #                 tags$li(strong("CM Engagement"), 
           #                         "- member engagement with Case Management can be defined in several ways. 
           #                This dashboard defines engagement as having 1 or 2 successful contacts between CM and member within a month.")
           #                 
           #               ),
           #               
           #               # Other Decisions:
           #               tags$h4("Other Decisions:"),
           #               tags$ul(
           #                 tags$li("Removed blood transfusions from severe maternal morbity flags")
           #                 
           #               ) # end ul()  
           #             ) #end card_body() 
           #           ) #end card
           # ) # END Additional Details nav_panel
  # ), #END Appendix nav_menu 
  # Testing -----------------------------------------------------------------

  # nav_panel("Testing",
  #          card(
  #            class = "card-color-group-8",
  #            tableOutput("test")
  #            )
  #          )
  
  
  
) #end page_navbar




