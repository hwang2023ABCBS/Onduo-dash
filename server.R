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
library(htmlwidgets)
library(jsonlite)
library(reactable)
library(reactablefmtr)
library(RColorBrewer) #needed for Joe's part
# library(ggridges) #Joe's
# library(ggokabeito) #Joe's
library(leaflet)
library(sf)
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

#duckplyr::methods_overwrite()

server <- function(input, output, session) {
  
  
  
  # OPTION UPDATES ----------------------------------------------------
  
  ## CM Engagement Observes ---------------- 
  # Observe radio button input and update the selectInput choices:
  observe({
    if (input$cm_contact_aggregation == "total"){
      updateSelectInput(session, "cm_options_selection",
                        label = "Total Successful CM Contacts:",
                        choices = c("0 - No Engagement",
                                    "1 - Low Engagement",
                                    "2 - Moderate Engagement",
                                    "3+ - High Engagement"
                        ) #end choices
      ) #end updateSelectInput
    }
    else if (input$cm_contact_aggregation == "total"){
      updateSelectInput(session, "cm_options_selection",
                        label = "Percent Successful CM Contacts - (Overall):",
                        choices = c("0% - No Engagement",
                                    "1-25% - Low Engagement",
                                    "25%-50% - Moderate Engagement",
                                    "50%-75% - High Engagement",
                                    "75%-100% -Very High Engagement"
                        ) #end choices
      ) #end updateSelectInput
    }
  })
  
  ## Utilization Observes ----------------------
  observe({
    if (input$admission_type_2 == "er"){
      updateSelectInput(session, "diagnosis_col_type",
                        label = "Choose Diagnosis Column (ER Table):",
                        choices = c("MEDICAL_DIAG_DESC_CODE",
                                    "DIAG1_SUPERGROUPER_DESC", "DIAG1_GROUPER_DESC",
                                    "DIAG2_SUPERGROUPER_DESC", "DIAG2_GROUPER_DESC",
                                    "DIAG3_SUPERGROUPER_DESC", "DIAG3_GROUPER_DESC"
                        ) #end choices
      ) #end updateSelectInput
    }
    else if (input$admission_type_2 == "ip"){
      updateSelectInput(session, "diagnosis_col_type",
                        label = "Choose Diagnosis Column (IP Table):",
                        choices = c("MEDICAL_DIAG_DESC_CODE",
                                    "SVC_ADMITTING_DIAG_DESC",
                                    "ADMITTING_DIAG_SUPERGROUPER_DESC",
                                    "ADMITTING_DIAG_GROUPER_DESC"
                        ) #end choices
      ) #end updateSelectInput
    } #end else if
  })
  
  
  # DATAINPUT ---------------------------------------------------------------
  
  # Notes on memoise():
  # Reactives like input$age_cat_input or input$gender should NOT be accessed directly inside memoise(),
  # as memoised functions should not contain reactive dependencies directly.
  # Instead, you should pass these values to the memoised function explicitly as arguments
  # 
  # eventReactive() is properly used to trigger when the apply_filters button is pressed, but
  # the memoised function should ideally take the filter valeus as arguements.
  # 
  # When filtering Table2 using dataInput_Table1(), you need to access the reactive  table with parentheses
  # (dataInput_Table1()), because reactive values in Shiny are functions aand must be called to access their value.
  
  
  ## Static/Mbr Monthly ----------------------------------------------
  #memoise instead of reactive here: memoise is caching the filtered results to improve the performance
  
  # Memoized function for filtering Table1
  Static_Mbr_Level_filtering <- memoise(function(company_name_input,
                                                 age_cat_input, age_group_input, generation_input,
                                                 gender_input, race_input, minority_input,
                                                 county_name_input, state_input,
                                                 socioeconomic_svi_input, housingtransp_svi_input, 
                                                 rural_cat_input, maternal_care_desert_input, internet_access_input
                                                 #conditons_input
  ){
    Static_Mbr_Level %>%
      ungroup() %>% #just in case
      #call the memoized function with explicit arguments
      filter(Company_Name %in% company_name_input) %>%
      #filter(between(Age, input$age_input[1], input$age_input[2])) %>%
      filter(Age_Cat %in% age_cat_input) %>%
      filter(Age_Group %in% age_group_input) %>%
      filter(Generation %in% generation_input) %>%
      filter(Gender %in% gender_input) %>%
      filter(Race %in% race_input) %>%
      filter(Race_Minority_Cat %in% minority_input) %>%
      filter(Mbr_County_Name %in% county_name_input) %>% #losing 4K people
      filter(Mbr_State %in% state_input) %>%
      filter(Socioeconomic_SVI_Cat %in% socioeconomic_svi_input) %>%
      filter(HousingTransp_SVI_Cat %in% housingtransp_svi_input) %>% 
      filter(Rural_County_Cat %in% rural_cat_input) %>%
      filter(Maternity_Desert_Cat %in% maternal_care_desert_input) %>%
      filter(Internet_Access_County_Cat %in% internet_access_input) 
  })
  
  # Use eventReactive to trigger filtering when "apply_filters" is pressed
  dataInput_Static_Mbr_Level <- eventReactive(input$apply_filters, {
    Static_Mbr_Level_filtering(input$company_name_input,
                               input$age_cat_input, input$age_group_input, input$generation_input, 
                               input$gender_input, input$race_input, input$minority_input,
                               input$county_name_input, input$state_input, 
                               input$socioeconomic_svi_input, input$housingtransp_svi_input,
                               input$rural_cat_input, input$maternal_care_desert_input, input$internet_access_input
                               #input$conditions_input
    )
  }, ignoreNULL = FALSE)
  
  ## Maternity  ----------------------------------------------
  Maternity_filtering <- memoise(function(#member_list_input, 
    outcome_input, delivery_type_input, delivery_comp_input,
    delivery_preterm_input, last_gestation_input, num_pregnancy_input){
    Maternity %>%
      #filter(MEMBERID %in% member_list_input) %>% 
      filter(OUTCOME %in% outcome_input) %>%
      filter(DELIVERY_TYPE %in% delivery_type_input) %>% 
      filter(DELIVERY_COMPLICATIONS %in% delivery_comp_input) %>% 
      filter(PRETERM_ICD %in% delivery_preterm_input) %>% 
      filter(GEST_FINAL %in% last_gestation_input) %>%
      filter(GESTATION_EPISODE_NUMBER %in% num_pregnancy_input) 
    
  })
  
  dataInput_Maternity <- eventReactive(input$apply_filters, {
    Maternity_filtering(
      # Call the memoized function with filtered results from Table1 and inputs
      #dataInput_Static_Mbr_Level()$Mbr_ID,
      input$outcome_input,
      input$delivery_type_input,
      input$delivery_comp_input,
      input$delivery_preterm_input,
      input$last_gestation_input,
      input$num_pregnancy_input
    )
  }, ignoreNULL = FALSE)
  
  
  ## CM  -------------------------------
  
  CM_Monthly_filtering <- memoise(function(cm_programs_input){
    CM_Monthly %>% 
      filter(PROGRAM %in% cm_programs_input) 
  })
  
  dataInput_CM_Monthly <- eventReactive(input$apply_filters, {
    CM_Monthly_filtering(
      input$cm_programs_input
    )
  }, ignoreNULL = FALSE)
  
  ## Celeste  -------------------------------
  
  Celeste_Programs_filtering <- memoise(function(celeste_programs_input){
    Celeste_Programs %>% 
      filter(VENDOR %in% celeste_programs_input) 
  })
  
  dataInput_Celeste_Programs <- eventReactive(input$apply_filters, {
    Celeste_Programs_filtering(
      input$celeste_programs_input
    )
  }, ignoreNULL = FALSE)
  
  
  ## ER (MH)----------------------------------------------
  #no join because that's expensive. we will just filter for the member IDs present in all tables
  BH_ER_filtering <- memoise(function(member_list_input){
    BH_ER_Claims#%>% 
    # filter(MBR_ID %in% member_list_input) 
  })
  
  dataInput_BH_ER <- eventReactive(input$apply_filters, {
    BH_ER_filtering(dataInput_Static_Mbr_Level()$Mbr_ID)
  }, ignoreNULL = FALSE)
  
  
  ## ER (general)----------------------------------------------
  #no join because that's expensive. we will just filter for the member IDs present in all tables
  General_ER_filtering <- memoise(function(member_list_input){
    General_ER_Claims #%>% 
    # filter(MBR_ID %in% member_list_input) 
  })
  
  dataInput_General_ER <- eventReactive(input$apply_filters, {
    General_ER_filtering(dataInput_Static_Mbr_Level()$Mbr_ID)
  }, ignoreNULL = FALSE)
  
  
  ## IP (MH)----------------------------------------------
  BH_IP_filtering <- memoise(function(member_list_input){
    BH_IP_Claims #%>% 
    # filter(MBR_ID %in% member_list_input) 
  })
  
  dataInput_BH_IP <- eventReactive(input$apply_filters, {
    BH_IP_filtering(dataInput_Static_Mbr_Level()$Mbr_ID)
  }, ignoreNULL = FALSE)
  
  
  ## IP (general)----------------------------------------------
  General_IP_filtering <- memoise(function(member_list_input){
    General_IP_Claims #%>% 
    # filter(MBR_ID %in% member_list_input) 
  })
  
  dataInput_General_IP <- eventReactive(input$apply_filters, {
    General_IP_filtering(dataInput_Static_Mbr_Level()$Mbr_ID)
  }, ignoreNULL = FALSE)
  
  
  ## Severity ----------------------------------------------
  Severity_filtering <- memoise(function(member_list_input){
    Severity #%>% 
    # filter(MEMBERID %in% member_list_input) 
  })
  
  dataInput_Severity <- eventReactive(input$apply_filters, {
    Severity_filtering(dataInput_Static_Mbr_Level()$Mbr_ID)
  }, ignoreNULL = FALSE)
  
  ## Mbr Monthly ----------------------------------------------
  Mbr_Monthly_filtering <- memoise(function(member_list_input){
    Mbr_Monthly #%>%
    #   filter(MEMBERID %in% member_list_input) 
  })
  
  dataInput_Mbr_Monthly <- eventReactive(input$apply_filters, {
    Mbr_Monthly_filtering(dataInput_Static_Mbr_Level()$Mbr_ID)
  }, ignoreNULL = FALSE)
  
  ## FINAL POPULATION ---------------------------------------
  Population_filtering <- memoise(function(maternity_mbr_list_input,
                                           cm_mbr_list_input,
                                           celeste_mbr_list_input,
                                           static_mbr_list_input
  ){
    Population %>%
      # Apply Maternity filters if toggled on; otherwise, no filter
      { if (input$maternity_filters_toggle)
        filter(., MEMBERID %in% maternity_mbr_list_input) else . } %>%
      
      # Apply CM filters filters if toggled on; otherwise, no filter
      { if (input$cm_filters_toggle)
        filter(., MEMBERID %in% cm_mbr_list_input) else . } %>%
      
      # Apply Celeste filters if toggled on; otherwise, no filter
      { if (input$celeste_filters_toggle)
        filter(., MEMBERID %in% celeste_mbr_list_input) else . } %>%
      
      # Always apply the static filter
      filter(MEMBERID %in% static_mbr_list_input)
    
    
  })
  
  dataInput_Population <- eventReactive(input$apply_filters, {
    Population_filtering(
      dataInput_Maternity()$MEMBERID,
      dataInput_CM_Monthly()$MEMBERID,
      dataInput_Celeste_Programs()$MEMBER_ID,
      dataInput_Static_Mbr_Level()$Mbr_ID
    )
  }, ignoreNULL = FALSE)
  
  
  
  
  
  # VALUE BOXES -------------------------------------------------------------
  
  
  output$member_count <- renderText({
    as.character(
      dataInput_Population() %>% summarise(member_count = scales::label_comma()(n_distinct(MASTER_ID))) %>% pull()
    )
  })
  
  
  
  output$member_count2 <- renderText({
    as.character(
      dataInput_Population() %>% summarise(member_count = scales::label_comma()(n_distinct(MASTER_ID))) %>% pull()
    )
  })
  # data <- dataInput_Mbr_Monthly() %>% 
  #   inner_join(dataInput_Population(), by = c("MEMBERID" = "MEMBERID")) %>% 
  #   # mutate(Month = format(SVC_SERVICE_TO_DATE, "%Y-%m-01")) %>% 
  
  
  output$pmpm <- renderText({
    as.character(
      dataInput_Population() %>%
        inner_join(dataInput_Mbr_Monthly() %>%
                     select(MEMBERID, MONTH, PAID),
                   by = "MEMBERID") %>%
        
        mutate(TALLY = 1) %>% 
        summarise(PAID = sum(PAID, na.rm = TRUE), member_months = sum(TALLY)) %>% 
        mutate(PMPM = scales::label_dollar()(PAID/member_months)) %>% 
        pull()
    )
  })
  
  
  output$program_participation <- renderText({
    as.character(
      dataInput_Population() %>% 
        mutate(Program = if_else(MEMBERID %in% dataInput_Celeste_Programs()$MEMBER_ID | MEMBERID %in% dataInput_CM_Monthly()$MEMBERID, 1, 0)) %>% 
        summarise(participation_pct = scales::label_percent()(mean(Program, na.rm=TRUE))) %>% pull()
    )
  })
  
  
  # engagement_length
  
  output$engagement_length <- renderText({
    as.character(
      dataInput_Population() %>% 
        inner_join(dataInput_Celeste_Programs(),
                   join_by('MEMBERID'=='MEMBER_ID')) %>% 
        mutate(ENROLLED_LENGTH= as.numeric(difftime(ENROLLMENT_END_DATE,ENROLLMENT_START_DATE, units = 'days'))) %>%
        # mutate(ENROLLED_LENGTH= sql("DATEDIFF(day, ENROLLMENT_START_DATE,ENROLLMENT_END_DATE)")) %>% 
        summarise(ENROLLED_LENGTH_median= median(ENROLLED_LENGTH, na.rm= TRUE)) %>% pull()
    )
  })
  
  # er_pmpm
  output$er_pmpm <- renderText({
    as.character(
      dataInput_Population() %>% 
        inner_join(dataInput_General_ER(), join_by('MEMBERID'=='MBR_ID')) %>% 
        distinct(MEMBERID,SVC_SERVICE_FRM_DATE) %>% 
        summarise(MbrCnts= n_distinct(MEMBERID), ClaimsCnts= n()) %>% 
        summarise(er_per1K= round(ClaimsCnts/MbrCnts * 1000, 0))
    )
  })
  
  # internet_access
  output$internet_access <- renderText({
    as.character(
      dataInput_Population() %>% 
        inner_join(dataInput_Static_Mbr_Level() %>% select(Mbr_ID,Internet_Access_County_Cat), join_by('MEMBERID'=='Mbr_ID')) %>% 
        mutate(Internet_Poor_Access_Flag= ifelse(Internet_Access_County_Cat=='Poor Broadband Access' | Internet_Access_County_Cat=='Very Poor Broadband Access', 1, 0)) %>% 
        summarise(Internet_Poor_Access_Rate= scales::label_percent()(mean(Internet_Poor_Access_Flag, na.rm= TRUE)))
    )
  })
  
  # cm_contact_sucess_rate
  output$cm_contact_sucess_rate <- renderText({
    as.character(
      dataInput_Population() %>% 
        inner_join(dataInput_CM_Monthly() %>% select(MEMBERID,ContactMonth,PROGRAM,overall_avg_success), join_by('MEMBERID'=='MEMBERID')) %>% 
        group_by(MEMBERID) %>% 
        summarise(overall_avg_success= mean(overall_avg_success,na.rm=TRUE)) %>% 
        ungroup() %>% 
        summarise(overall_avg_success= round(mean(overall_avg_success,na.rm=TRUE),2))
    )
  })
  
  
  # med adherence rate 
  output$med_adherence_rate <- renderText({
    adherence_rate <- as.character(
       dataInput_Population() %>% 
        inner_join(ONDUO_MedAdherence_Rate_ByMbrDrugClass, join_by('MEMBERID'=='MEMBERID') ) %>% 
        group_by(MEMBERID) %>% 
        summarise(mean_adhernece_rate= mean(MED_ADHERENCE_RATE, na.rm= TRUE)) %>% 
        ungroup() %>% 
        summarise(cohort_mean_adherence_rate= round(mean(mean_adhernece_rate, na.rm= TRUE),2))
    )
      
    diab_adherence_rate <- as.character(
      dataInput_Population() %>%
        inner_join(
          ONDUO_MedAdherence_Rate_ByMbrDrugClass,
          join_by('MEMBERID' == 'MEMBERID')
        ) %>%
        filter(Diab_Drug == 1) %>%
        group_by(MEMBERID) %>%
        summarise(mean_adhernece_rate = mean(MED_ADHERENCE_RATE, na.rm = TRUE)) %>%
        ungroup() %>%
        summarise(cohort_mean_adherence_rate = round(mean(
          mean_adhernece_rate, na.rm = TRUE
        ), 2))
    )
    # concatenate both adherence rates into a long string
    result <- paste(adherence_rate, "(diab",diab_adherence_rate,")")
    return(result)

  
  })
  
  
  # GLP-1 usage
  output$glp_1_usage <- renderText({
    as.character(
      dataInput_Population() %>% 
        inner_join(df_Pharma_VirtaOnduo %>% 
                     select(MBR_ID,SVC_RX_CLASS_DESC,SERVICEMONTH,MEMBER_PAID,PLAN_PAID,GLP_1_flag) %>% 
                     filter(GLP_1_flag==1), join_by('MEMBERID'=='MBR_ID')) %>% 
        group_by(MEMBERID,SVC_RX_CLASS_DESC,SERVICEMONTH) %>% 
        summarise(TOTAL_PAID= MEMBER_PAID+PLAN_PAID) %>% 
        ungroup() %>% 
        group_by(MEMBERID,SERVICEMONTH) %>% 
        summarise(TOTAL_PAID= sum(TOTAL_PAID)) %>% 
        ungroup() %>% 
        mutate(TALLY = 1) %>% 
        summarise(TOTAL_PAID = sum(TOTAL_PAID), member_months = sum(TALLY)) %>% 
        mutate(TOTAL_PAID_PMPM = scales::label_dollar()(round(TOTAL_PAID/member_months,0))) %>% 
        select(TOTAL_PAID_PMPM)
    )
  })
  
  
  
  
  output$delivery_count <- renderText({
    as.character(
      dataInput_Maternity() %>% select() %>% filter(OUTCOME == "Still Birth" | OUTCOME == "Live Birth") %>% summarise(delivery_count = scales::label_comma()(sum(OUTCOME, na.rm = TRUE))) %>% pull()
    )
  })
  
  output$avg_age <- renderText({
    as.character(
      dataInput_Static_Mbr_Level() %>% distinct(Mbr_ID, .keep_all = TRUE) %>% summarise(avg_age = scales::label_comma()(mean(Age, na.rm=T))) %>% pull()
    )
  })
  
  output$avg_female <- renderText({
    as.character(
      dataInput_Static_Mbr_Level() %>% distinct(Mbr_ID, .keep_all = TRUE) %>% summarise(avg_female = scales::label_percent()(mean(Female, na.rm=T))) %>% pull()
    )
  })
  
  output$total_rural <- renderText({
    as.character(
      dataInput_Static_Mbr_Level() %>% distinct(Mbr_ID, .keep_all = TRUE) %>% 
        mutate(total_rural = if_else(Rural_County_Cat == "Rural" | Rural_County_Cat == "Very Rural", 1, 0)) %>% 
        summarise(avg_rural = scales::label_percent()(mean(total_rural, na.rm = TRUE))) %>% pull()
    )
  })
  
  output$avg_socio_barriers <- renderText({
    as.character(
      dataInput_Static_Mbr_Level() %>% distinct(Mbr_ID, .keep_all = TRUE) %>% summarise(avg_socio_barriers = scales::label_percent()(mean(Socioeconomic_SVI_Tract, na.rm=T))) %>% pull()
    )
  })
  
  output$mbrs_location_missing <- renderText({
    as.character(
      dataInput_Static_Mbr_Level() %>%  distinct(Mbr_ID, .keep_all = TRUE) %>% summarise(total_missing_location = scales::label_comma()(sum(is.na(GEOID)))) %>% pull()
    )
  })
  
  
  
  
  
  
  # LANDING PAGE --------------------------------------------------------------
  
  ## Member Trends -----------------------------------------------------------
  
  ### total members ----------------------------
  output$member_trends <- renderPlotly({
    if (input$member_trends_plot == "total_members"){
      
      data <- Mbr_Monthly %>%
        select(MEMBERID, MONTH) %>%
        inner_join(Population, by = c("MEMBERID" = "MEMBERID")) %>%
        mutate(Month = floor_date(MONTH, "month")) %>%
        group_by(Month) %>%
        summarise(
          Total_Members = n_distinct(MASTER_ID)
        ) %>%
        mutate(
          Month_Label = format(as.Date(Month), "%b %Y") #format date as Jan 2022
        )
      # filter(Month < "2024-01-01")
      
      # href_line <- data %>%
      #   filter(Month == as.Date("2023-01-01")) %>%
      #   pull(ER_Rate)
      
      data %>%
        plot_ly(x = ~Month, y = ~Total_Members,
                type = 'scatter', mode = 'lines+markers',
                text = ~paste("Month:", Month_Label, "<br>Total Members:", scales::label_comma()(round(Total_Members))),
                hoverinfo = "text"
                # line = list(color = "#9ECAE1", width = 2),
                # marker = list(color = '#4F7A90', size = 8)
        ) %>%
        layout(xaxis = list(title = 'Month'),
               yaxis = list(title = ''),
               title = "Total Members"
        ) #end layout
      
    } #END total members over time
    
    
    ### new enrolled members ----------------------------
    else if (input$member_trends_plot == "new_members"){
      
      data <- Static_Mbr_Level %>%
        select(Mbr_ID, FIRSTDATE) %>%
        inner_join(Population, by = c("Mbr_ID" = "MEMBERID")) %>%
        mutate(Month = floor_date(FIRSTDATE, "month")) %>%
        group_by(Month) %>%
        summarise(
          New_Members = n_distinct(MASTER_ID)
        ) %>%
        mutate(
          Month_Label = format(as.Date(Month), "%b %Y") #format date as Jan 2022
        )
      # filter(Month < "2024-01-01")
      
      # href_line <- data %>%
      #   filter(Month == as.Date("2023-01-01")) %>%
      #   pull(ER_Rate)
      
      data %>%
        plot_ly(x = ~Month, y = ~New_Members,
                type = 'scatter', mode = 'lines+markers',
                text = ~paste("Month:", Month_Label, "<br>New Members Enrolled:", scales::label_comma()(round(New_Members, 1))),
                hoverinfo = "text"
                # line = list(color = "#9ECAE1", width = 2),
                # marker = list(color = '#4F7A90', size = 8)
        ) %>%
        layout(xaxis = list(title = 'Month'),
               yaxis = list(title = ''),
               title = "New Members Enrolled"
        ) #end layout
      
    } #END new members over time
    
    ### lost members ----------------------------
    else if (input$member_trends_plot == "lost_members"){
      
      data <- Static_Mbr_Level %>%
        filter(TERMED == 1) %>% 
        select(Mbr_ID, LASTDATE) %>%
        inner_join(Population, by = c("Mbr_ID" = "MEMBERID")) %>%
        mutate(Month = floor_date(LASTDATE, "month")) %>%
        group_by(Month) %>%
        summarise(
          Lost_Members = n_distinct(MASTER_ID)
        ) %>%
        mutate(
          Month_Label = format(as.Date(Month), "%b %Y") #format date as Jan 2022
        )
      # filter(Month < "2024-01-01")
      
      # href_line <- data %>%
      #   filter(Month == as.Date("2023-01-01")) %>%
      #   pull(ER_Rate)
      
      data %>%
        plot_ly(x = ~Month, y = ~Lost_Members,
                type = 'scatter', mode = 'lines+markers',
                text = ~paste("Month:", Month_Label, "<br>Termed Members:", scales::label_comma()(round(Lost_Members))),
                hoverinfo = "text"
                # line = list(color = "#9ECAE1", width = 2),
                # marker = list(color = '#4F7A90', size = 8)
        ) %>%
        layout(xaxis = list(title = 'Month'),
               yaxis = list(title = ''),
               title = "Members Leaving Plan"
        ) #end layout
      
    } #END lost members over time
    
  }) #END renderPlotly on MEMBER TRENDS
  
  output$maternity_trends <- renderPlotly({
    
    ## Maternity Trends ----------------------------
    if (input$maternity_trends_plot == "preg_over_time"){
      
      data <- dataInput_Maternity() %>%
        mutate(Month = floor_date(LMP, "month")) %>%
        group_by(Month) %>%
        summarise(
          Pregnancies = n_distinct(PREGNANCY_ID),
          Deliveries = sum(DELIVERY, na.rm = TRUE)
        ) %>%
        mutate(
          Month_Label = format(as.Date(Month), "%b %Y") #format date as Jan 2022
        ) %>%
        pivot_longer(cols = c(Pregnancies, Deliveries), names_to = "Value_Type", values_to = "Value")
      # filter(Month < "2024-01-01")
      
      # href_line <- data %>%
      #   filter(Month == as.Date("2023-01-01")) %>%
      #   pull(ER_Rate)
      
      data %>%
        plot_ly(x = ~Month, y = ~Value, color = ~Value_Type,
                type = 'scatter', mode = 'lines+markers',
                text = ~paste("Month:", Month_Label, "<br>", Value_Type, scales::label_comma()(round(Value, 1))),
                hoverinfo = "text"
                # line = list(color = "#9ECAE1", width = 2),
                # marker = list(color = '#4F7A90', size = 8)
        ) %>%
        layout(xaxis = list(title = 'Month'),
               yaxis = list(title = ''),
               title = "Pregnancies & Deliveries Over Time"
        ) #end layout
      
    } #END preg_over_time
    
    ### delivery outcome rates ----------------------------
    else if (input$maternity_trends_plot == "delivery_outcome_rates"){
      
      data <- dataInput_Maternity() %>%
        inner_join(dataInput_Population(), by = c("MEMBERID" = "MEMBERID")) %>%
        filter(DELIVERY == 1) %>%
        mutate(Month = floor_date(OUTCOME_FINAL_DATE, "month")) %>%
        group_by(Month) %>%
        summarise(
          Deliveries = sum(DELIVERY, na.rm = TRUE),
          PreTerm_Births = sum(PRETERM_WEEKS, na.rm = TRUE),
          Cestions = sum(DELIVERY_CESAREAN, na.rm = TRUE),
          Still_Births = sum(if_else(OUTCOME == "Still Birth", 1, 0), na.rm = TRUE)
        ) %>%
        mutate(PreTerm_Birth_Rate = round((PreTerm_Births/Deliveries) * 100,1), #rate per 100 deliveries
               Csection_Rate = round((Cestions/Deliveries) * 100,1),
               Still_Birth_Rate = round((Still_Births/Deliveries) * 100,1),
               Month_Label = format(as.Date(Month), "%b %Y")) %>% #format date as Jan 2022
        pivot_longer(cols = contains("Rate"), names_to = "Rate_Type", values_to = "Rate")
      # href_line <- data %>%
      #   filter(Month == as.Date("2023-01-01")) %>%
      #   pull(ER_Rate)
      
      data %>%
        plot_ly(x = ~Month, y = ~Rate, color = ~Rate_Type,
                type = 'scatter', mode = 'lines+markers',
                text = ~paste(Rate_Type, scales::label_comma()(round(Rate, 2)), "per 100 births", "<br>Month:", Month_Label),
                hoverinfo = "text"
                # line = list(color = "#9ECAE1", width = 2),
                # marker = list(color = '#4F7A90', size = 8)
        ) %>%
        # add_trace(y = ~Csection_Rate, name = "C-Section Rate", mode = "lines+markers") %>%
        # add_trace(y = ~Still_Birth_Rate, name = "Still Birth Rate", mode = "lines+markers") %>%
        layout(xaxis = list(title = 'Month'),
               yaxis = list(title = ''),
               title = "Delivery Outcome Rates (per 100 births)"
        ) #end layout
      
    } #END Outcome Trends
    
    
  }) #end renderPlotly on MATERITY TRENDS
  
  
  
  
  ## Cost/Utilization Trends ---------------------------------------------
  
  
  ### er --------------------------------------
  
  output$cost_utilization_trends <- renderPlotly({
    
    
    if (input$cost_trends_grouping == "er"){
      
      data <- dataInput_General_ER() %>% 
        inner_join(dataInput_Population(), by = c("MBR_ID" = "MEMBERID")) %>% 
        # mutate(Month = format(SVC_SERVICE_TO_DATE, "%Y-%m-01")) %>% 
        mutate(Month = floor_date(SVC_SERVICE_TO_DATE, "month")) %>% 
        group_by(Month) %>% 
        summarise(ER_Visits = n_distinct(INTEGER_CLAIM_NUMBER),
                  Member_Count = n_distinct(MASTER_ID)) %>% 
        mutate(ER_Rate = round((ER_Visits/Member_Count) * 100,1), #rate per 100 patients
               Month_Label = format(as.Date(Month), "%b %Y")) #format date as Jan 2022 
      
      # href_line <- data %>% 
      #   filter(Month == as.Date("2023-01-01")) %>% 
      #   pull(ER_Rate)
      
      data %>% 
        plot_ly(x = ~Month, y = ~ER_Rate, 
                type = 'scatter', mode = 'lines+markers',
                text = ~paste("ER_Rate: ", round(ER_Rate, 2), "<br>Month: ", Month_Label), 
                hoverinfo = "text",
                line = list(color = "#9ECAE1", width = 2),
                marker = list(color = '#4F7A90', size = 8)) %>% 
        layout(xaxis = list(title = 'Month'),
               yaxis = list(title = ''),
               title = "ER Rate per 100 Mbrs"
        ) #end layout
    }
    
    ### ip --------------------------------------
    else if (input$cost_trends_grouping == "ip"){
      
      data <- dataInput_General_IP() %>% 
        inner_join(dataInput_Population(), by = c("MBR_ID" = "MEMBERID")) %>% 
        # mutate(Month = format(SVC_SERVICE_TO_DATE, "%Y-%m-01")) %>%
        # mutate(Month = format(as.Date(floor_date(ADMISSION_END_DATE, "month")))) %>% 
        mutate(Month = floor_date(ADMISSION_END_DATE, "month")) %>%
        group_by(Month) %>% 
        summarise(IP_Visits = n_distinct(ADMISSIONID),
                  Member_Count = n_distinct(MASTER_ID)) %>% 
        mutate(IP_Rate = round((IP_Visits/Member_Count) * 100,1), #rate per 100 patients
               Month_Label = format(as.Date(Month), "%b %Y")) #format date as Jan 2022 
      
      # href_line <- data %>% 
      #   filter(Month == as.Date("2023-01-01")) %>% 
      #   pull(IP_Rate)
      
      data %>% 
        plot_ly(x = ~Month, y = ~IP_Rate, 
                type = 'scatter', mode = 'lines+markers',
                text = ~paste("IP_Rate: ", round(IP_Rate, 2), "<br>Month: ", Month_Label), 
                hoverinfo = "text",
                line = list(color = "#9ECAE1", width = 2),
                marker = list(color = '#4F7A90', size = 8)) %>% 
        layout(xaxis = list(title = 'Month'),
               yaxis = list(title = ''),
               title = "IP Rate per 100 Mbrs"
        ) #end layout
    } #end IP IF
    
    ### er costs/visits --------------------------------------
    else if (input$cost_trends_grouping == "er_normalized"){
      
      data <- dataInput_General_ER() %>% 
        inner_join(dataInput_Population(), by = c("MBR_ID" = "MEMBERID")) %>% 
        # mutate(Month = format(SVC_SERVICE_TO_DATE, "%Y-%m-01")) %>% 
        mutate(Month = floor_date(SVC_SERVICE_TO_DATE, unit = "month")) %>% 
        group_by(Month) %>% 
        summarise(ER_Visits = n_distinct(INTEGER_CLAIM_NUMBER),
                  Member_Count = n_distinct(MASTER_ID),
                  ER_Paid = sum(REV_PAID_AMT)) %>% 
        mutate(ER_Paid_Visit_Rate = round(ER_Paid/ER_Visits), #rate per 100 patients
               Month_Label = format(as.Date(Month), "%b %Y")) #format date as Jan 2022 
      
      # href_line <- data %>% 
      #   filter(Month == as.Date("2023-01-01")) %>% 
      #   pull(ER_Paid_Visit_Rate)
      
      data %>% 
        plot_ly(x = ~Month, y = ~ER_Paid_Visit_Rate, 
                type = 'scatter', mode = 'lines+markers',
                text = ~paste("Paid/ER Visits: ", round(ER_Paid_Visit_Rate), "<br>Month: ", Month_Label), 
                hoverinfo = "text",
                line = list(color = "#9ECAE1", width = 2),
                marker = list(color = '#4F7A90', size = 8)) %>% 
        layout(xaxis = list(title = 'Month'),
               yaxis = list(title = '', tickformat = "$,.0f"),
               title = "Total ER Cost/Total ER Visits"
        ) #end layout
    } #END ER
    
    ### ip costs/visits --------------------------------------
    else if (input$cost_trends_grouping == "ip_normalized"){
      
      data <- dataInput_General_IP() %>% 
        inner_join(dataInput_Population(), by = c("MBR_ID" = "MEMBERID")) %>% 
        # mutate(Month = format(SVC_SERVICE_TO_DATE, "%Y-%m-01")) %>% 
        mutate(Month = floor_date(SVC_SERVICE_TO_DATE, unit = "month")) %>% 
        group_by(Month) %>% 
        summarise(IP_Visits = n_distinct(ADMISSIONID),
                  Member_Count = n_distinct(MASTER_ID),
                  IP_Paid = sum(REV_PAID_AMT)) %>% 
        mutate(IP_Paid_Visit_Rate = round(IP_Paid/IP_Visits), #rate per 100 patients
               Month_Label = format(as.Date(Month), "%b %Y")) #format date as Jan 2022 
      
      # href_line <- data %>% 
      #   filter(Month == as.Date("2023-01-01")) %>% 
      #   pull(IP_Paid_Visit_Rate)
      
      data %>% 
        plot_ly(x = ~Month, y = ~IP_Paid_Visit_Rate, 
                type = 'scatter', mode = 'lines+markers',
                text = ~paste("IP_Rate: ", round(IP_Paid_Visit_Rate), "<br>Month: ", Month_Label), 
                hoverinfo = "text",
                line = list(color = "#9ECAE1", width = 2),
                marker = list(color = '#4F7A90', size = 8)) %>% 
        layout(xaxis = list(title = 'Month'),
               yaxis = list(title = '', tickformat = "$,.0f"),
               title = "Total IP Cost/Total IP Visits"
        ) #end layout
    } #end IP IF
    
    ### pharmacy --------------------------------------
    else if (input$cost_trends_grouping == "pharm"){
      
      data <- dataInput_Mbr_Monthly() %>%
        inner_join(dataInput_Population(), by = c("MEMBERID" = "MEMBERID")) %>% 
        group_by(MONTH) %>% 
        filter(MONTH >= "2021-01-01") %>% 
        summarise(Total_Pharmacy_Spend = sum(PAID_PHARM)) %>% 
        mutate(Month_Label = format(as.Date(MONTH), "%b %Y")) #format date as Jan 2022 
      
      
      data %>% 
        plot_ly(x = ~MONTH, y = ~Total_Pharmacy_Spend, 
                type = 'scatter', mode = 'lines+markers',
                text = ~paste("Pharmacy Spend: ", round(Total_Pharmacy_Spend, 2), "<br>Month: ", Month_Label), 
                hoverinfo = "text",
                line = list(color = "#9ECAE1", width = 2),
                marker = list(color = '#4F7A90', size = 8)) %>% 
        layout(xaxis = list(title = 'Month'),
               yaxis = list(title = '', tickformat = "$,.0f"),
               title = "Total Pharmacy Spend"
        ) #end layout
    } #end Pharmacy IF
    
    ### pmpm --------------------------------------
    else if (input$cost_trends_grouping == "pmpm"){
      
      data <- dataInput_Mbr_Monthly() %>% 
        inner_join(dataInput_Population(), by = c("MEMBERID" = "MEMBERID")) %>% 
        # mutate(Month = format(SVC_SERVICE_TO_DATE, "%Y-%m-01")) %>% 
        
        group_by(MONTH) %>% 
        #filter(MONTH >= "2021-01-01") %>% 
        summarise(PMPM = sum(PAID)/n_distinct(MASTER_ID)) %>% 
        mutate(Month_Label = format(as.Date(MONTH), "%b %Y")) #format date as Jan 2022 
      
      
      data %>% 
        plot_ly(x = ~MONTH, y = ~PMPM, 
                type = 'scatter', mode = 'lines+markers',
                text = ~paste("Pharmacy Spend: ", round(PMPM, 2), "<br>Month: ", Month_Label), 
                hoverinfo = "text",
                line = list(color = "#9ECAE1", width = 2),
                marker = list(color = '#4F7A90', size = 8)) %>% 
        layout(xaxis = list(title = 'Month'),
               yaxis = list(title = '', tickformat = "$,.0f"),
               title = "PMPM"
        ) #end layout
    } #end PMPM IF
    
    ### glp1_usage_pmpm --------------------------------------
    else if (input$cost_trends_grouping == "glp1_usage_pmpm"){
      
      data <- dataInput_Population() %>% 
        inner_join(df_Pharma_VirtaOnduo %>% 
                     select(MBR_ID,SVC_RX_CLASS_DESC,SERVICEMONTH,MEMBER_PAID,PLAN_PAID,GLP_1_flag) %>% 
                     filter(GLP_1_flag==1,SERVICEMONTH >=start_date & SERVICEMONTH <=three_months_ago  ), join_by('MEMBERID'=='MBR_ID')) %>% 
        group_by(MEMBERID,SVC_RX_CLASS_DESC,SERVICEMONTH) %>% 
        summarise(TOTAL_PAID= MEMBER_PAID+PLAN_PAID,
                  PLAN_PAID= PLAN_PAID) %>% 
        ungroup() %>% 
        group_by(MEMBERID,SERVICEMONTH) %>% 
        mutate(TALLY = 1) %>% 
        summarise(TOTAL_PAID= sum(TOTAL_PAID),
                  PLAN_PAID= sum(PLAN_PAID),
                  member_months = sum(TALLY)) %>% 
        ungroup() %>% 
        group_by(SERVICEMONTH) %>% 
        summarise(TOTAL_PAID= sum(TOTAL_PAID),
                  PLAN_PAID= sum(PLAN_PAID),
                  member_months = sum(member_months)
                  ) %>% 
        mutate(TOTAL_PAID_PMPM= TOTAL_PAID/member_months,
                  PLAN_PAID_PMPM= PLAN_PAID/member_months) %>% 
        select(SERVICEMONTH ,TOTAL_PAID_PMPM,PLAN_PAID_PMPM) %>% 
        ungroup()
      
      # plot 
      # p <- plot_ly(data, x= ~SERVICEMONTH) %>% 
      #   add_lines(y= ~ TOTAL_PAID_PMPM , name= "Total Paid pmpm", line= list(color="blue")) %>% 
      #   add_lines(y = ~ PLAN_PAID_PMPM, name = "Planed Paid pmpm", line= list(color="red")) %>% 
      #   layout(title= "GLP-1 usage pmpm over time",
      #          xaxis= list(title= "Month"),
      #          yaxis= list(title= "Cost PMPM"),
      #          hovermode= "compare")
      # p
      
      
      plot <- plot_ly() %>%
        
        add_trace(
          
          data= data, 
          
          x= ~SERVICEMONTH, 
          
          y= ~TOTAL_PAID_PMPM,
          
          type= "scatter", 
          
          mode = "lines+markers",
          
          name= "TOTAL PAID PMPM",
          
          line = list(color="red")) %>% 
        
        add_trace(
          
          data=data, 
          
          x= ~SERVICEMONTH, 
          
          y= ~PLAN_PAID_PMPM,
          
          type= "scatter", 
          
          mode = "lines+markers",
          
          name="PLAN PAID PMPM",
          
          line = list(color="green")) %>% 
        
        layout(
          
          title= "GLP-1 Usage cost pmpm", 
          
          xaxis = list(title="Month",
                       
                       zeroline=FALSE#,
                       
                       # zerolinewidth=2,
                       
                       # zerolinecolor="red"
          ),
          
          yaxis = list(title="Cost pmpm")#, 
          
          # shapes= list(
          #   
          #   list( type= "line", 
          #         
          #         x0=0, x1=0,
          #         
          #         y0=min(Virta_look[, 3:6]), 
          #         
          #         y1=max(Virta_look[, 3:6]),
          #         
          #         line=list(dash ="dot", color="red")
          #         
          #   )
          #   
          # )
          
        )
      
      plot
      
      
      
    } #end PMPM IF
    
    
    
  }) # END cost_utilization_trends
  
  
  
  
  ## Intervention Trends --------------------------------
  output$intervention_trends <- renderPlotly({
    
    if (input$intervention_trends_options == "celeste_vendor_enrollment"){ 
      
      dataInput_Celeste_Programs() %>%
        inner_join(dataInput_Population(), by = c("MEMBER_ID" = "MEMBERID")) %>% 
        
        mutate(Month = floor_date(as.Date(ENROLLMENT_START_DATE, format = "%Y-%m-%d"), unit = "month")) %>% 
        group_by(Month, VENDOR) %>%
        summarise(Total_Enrolled = n_distinct(MASTER_ID)) %>%
        
        mutate(Month_Label = format(as.Date(Month), "%b %Y")) %>% #format date as Jan 2022
        
        ungroup() %>% 
        
        plot_ly(x = ~Month, y = ~Total_Enrolled, color = ~VENDOR,
                type = 'scatter', mode = 'lines+markers',
                text = ~paste("Month:", Month_Label, "<br>Members Enrolled:", scales::label_comma()(Total_Enrolled)),
                hoverinfo = "text"
                # line = list(color = "#9ECAE1", width = 2),
                # marker = list(color = '#4F7A90', size = 8)
        ) %>%
        layout(xaxis = list(title = 'Month'),
               yaxis = list(title = ''),
               title = "Celeste Vendor Enrollment"
        ) #end layout
    }
    
    else if (input$intervention_trends_options == "cm_engagement"){ 
      dataInput_CM_Monthly() %>%
        inner_join(dataInput_Population(), by = c("MEMBERID")) %>% 
        mutate(Engaged_1 = if_else(CM_Success > 0, 1, 0),
               Engaged_2 = if_else(CM_Success > 1, 1, 0)) %>%
        group_by(ContactMonth) %>%
        mutate(Value = Engaged_2) %>%
        # mutate(Value = if_else(input$cm_engaged_definition == "2 Successful Contact/Month", "Engaged_2", "Engaged_1")) %>%
        # mutate(Value = if (input$cm_engaged_defintion == "2 Successful Contact/Month") {
        #   Engaged_1
        # } else {
        #   Engaged_2
        # }) %>% 
        summarise(Value = sum(Value),
                  Total_Members = n_distinct(MASTER_ID)) %>%
        mutate(Engaged_Rate = Value/Total_Members) %>%
        mutate(
          Month_Label = format(as.Date(ContactMonth), "%b %Y") #format date as Jan 2022
        ) %>%
        plot_ly(x = ~ContactMonth, y = ~Engaged_Rate,
                type = 'scatter', mode = 'lines+markers',
                text = ~paste("CM Engagement Rate:", scales::label_percent(accuracy = 0.1)(Engaged_Rate), "<br>Month:", Month_Label),
                hoverinfo = "text"
                # line = list(color = "#9ECAE1", width = 2),
                # marker = list(color = '#4F7A90', size = 8)
        ) %>%
        layout(xaxis = list(title = 'Month'),
               yaxis = list(title = ''),
               title = "CM Engagement Rate"
        ) #end layout
    }
    
    
  }) #END intervention trends 
  
  ## BMI trend over time ---------------------------------------
  BMI_trendLine_df_all <- arrow::read_parquet(here::here("data/BMI_trendLine_df_all.parquet"))
  ONDUO_median_start_date <- as.Date("2020-11-27")
  VIRTA_median_start_date <- as.Date("2024-04-26")
  output$TrendLines_BMI <- renderPlot({
    ggplot(BMI_trendLine_df_all,aes(MONTH, BMI, color = VENDOR)) +
      geom_line(size= 1.5) +
      geom_smooth(method = "lm", linewidth=1) +
      geom_vline(xintercept = as.numeric(ONDUO_median_start_date), color = "black", linetype = "dashed", size=1) +
      annotate("text", x = ONDUO_median_start_date, y = max(BMI_trendLine_df_all$BMI) + 1, label = "ONDUO Enrollment Start", color = "black", angle = 0, vjust = -0.5, hjust = 1.1, size=5) +
      geom_vline(xintercept = as.numeric(VIRTA_median_start_date), color = "black", linetype = "dashed",size=1) +
      annotate("text", x = VIRTA_median_start_date, y = max(BMI_trendLine_df_all$BMI) + 1, label = "VIRTA Enrollment Start", color = "black", angle = 0, vjust = -0.5, hjust = 1.1,size=5) +
      theme_minimal(base_size = 15)+
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "grey"),
        axis.text = element_text(color = "black", size=12),
        axis.title = element_text(color = "black", size=14),
        plot.title = element_text(color = "black", size=16),
        plot.subtitle = element_text(color = "black", size=14),
        plot.caption = element_text(color = "black",size=12)
      ) +
      labs(
        title = "BMI Trend Lines",
        x = "Month",
        y = "BMI"
      )
    
    
  })
  
  
  ## Charateristics Logit model result ---------------------------------------
  tidy_model_O <- readRDS(file = "RDS Objects/tidy_model_O.rds")
  output$Onduo_Logit_Charateristics <- renderPlotly({
    
    # ggplot(tidy_model_O, aes(x = estimate, y = term, group = effects))+ geom_col(aes(fill = effects))+
    #   geom_text(aes(x = max(estimate)*1.1, label = round(estimate,1)))+
    #   ylab("")+
    #   ggtitle("Who are more likely to engage with ONDUO?")+
    #   theme_classic()+
    #   theme(legend.position = "none") # remove legend
    
    # redo in plotly
    plot_ly(tidy_model_O, x = ~estimate, y = ~term, type = 'bar', color = ~effects, orientation = 'h') %>%
      layout(
        title = "Who are more likely to engage with ONDUO?",
        yaxis = list(title = ""),
        xaxis = list(title = "Estimate"),
        barmode = 'group',
        showlegend = FALSE
      ) %>%
      add_annotations(
        x = ~estimate * 1.1,
        y = ~term,
        text = ~round(estimate, 1),
        showarrow = FALSE,
        xanchor = 'left'
      )
    
    
    
  })
  
  
  # LANDING PAGE 2 EVENT STUDY --------------------------------------------------------------
  ## Cost/Utilization trend event study ---------------------------------------------------------
  ### er -------------------------------------------------------------
  output$ cost_utilization_trend_eventStudy <- renderPlotly({
    if ( input$cost_utilization_trends_grouping2 =='er'){
      
      plot <- plot_ly() %>%
        
        add_trace(
          data = Onduo_look,
          
          x = ~ journey_month,
          
          y = ~ mean_ER*100,
          
          type = "scatter",
          
          mode = "lines+markers",
          
          name = "Average ER",
          
          line = list(color = "#3B8EA5")
        ) %>% 
        layout(
          title = "ONDUO - Before and After Event",
          
          xaxis = list(
            title = "Journey Month"#,
            
            # zeroline = TRUE,
            # 
            # zerolinewidth = 2,
            # 
            # zerolinecolor = "black"
          ),
          
          yaxis = list(title = "Average Value Per 100"),
          
          shapes = list(list(
            type = "line",
            
            x0 = 0,
            x1 = 0,
            
            y0 = min(Onduo_look[, 3:6]),
            
            y1 = max(Onduo_look[, 3:6]),
            
            line = list(dash = "dot", color = "black")
            
          ))
          
        )
      
      plot
      
      
    }
    ### ip -------------------------------------------------------------
    else if (input$cost_utilization_trends_grouping2 == "ip"){
      plot <- plot_ly() %>%
        
        add_trace(
          data = Onduo_look,
          
          x = ~ journey_month,
          
          y = ~ mean_IP * 100,
          
          type = "scatter",
          
          mode = "lines+markers",
          
          name = "Average IP",
          
          line = list(color = "#3B8EA5")
        ) %>% 
        layout(
          title = "ONDUO - Before and After Event",
          
          xaxis = list(
            title = "Journey Month"#,
            
            # zeroline = TRUE,
            # 
            # zerolinewidth = 2,
            # 
            # zerolinecolor = "black"
          ),
          
          yaxis = list(title = "Average Value Per 100"),
          
          shapes = list(list(
            type = "line",
            
            x0 = 0,
            x1 = 0,
            
            y0 = min(Onduo_look[, 3:6]),
            
            y1 = max(Onduo_look[, 3:6]),
            
            line = list(dash = "dot", color = "black")
            
          ))
          
        )
      
      plot
    }
    
    ### op---------------------------------------------------------------
    else if (input$cost_utilization_trends_grouping2 == "op"){
      plot <- plot_ly() %>%
        
        add_trace(
          data = Onduo_look,
          
          x = ~ journey_month,
          
          y = ~ mean_OP * 100,
          
          type = "scatter",
          
          mode = "lines+markers",
          
          name = "Average OP",
          
          line = list(color = "#3B8EA5")
        ) %>% 
        layout(
          title = "ONDUO - Before and After Event",
          
          xaxis = list(
            title = "Journey Month"#,
            
            # zeroline = TRUE,
            # 
            # zerolinewidth = 2,
            # 
            # zerolinecolor = "black"
          ),
          
          yaxis = list(title = "Average Value Per 100"),
          
          shapes = list(list(
            type = "line",
            
            x0 = 0,
            x1 = 0,
            
            y0 = min(Onduo_look[, 3:6]),
            
            y1 = max(Onduo_look[, 3:6]),
            
            line = list(dash = "dot", color = "black")
            
          ))
          
        )
      
      plot
    }
    
    ### bh---------------------------------------------------------------
    else if (input$cost_utilization_trends_grouping2 == "bh"){
      plot <- plot_ly() %>%
        
        add_trace(
          data = Onduo_look,
          
          x = ~ journey_month,
          
          y = ~ mean_BH * 100,
          
          type = "scatter",
          
          mode = "lines+markers",
          
          name = "Average BH",
          
          line = list(color = "#3B8EA5")
        ) %>% 
        layout(
          title = "ONDUO - Before and After Event",
          
          xaxis = list(
            title = "Journey Month"#,
            
            # zeroline = TRUE,
            # 
            # zerolinewidth = 2,
            # 
            # zerolinecolor = "black"
          ),
          
          yaxis = list(title = "Average Value Per 100"),
          
          shapes = list(list(
            type = "line",
            
            x0 = 0,
            x1 = 0,
            
            y0 = min(Onduo_look[, 3:6]),
            
            y1 = max(Onduo_look[, 3:6]),
            
            line = list(dash = "dot", color = "black")
            
          ))
          
        )
      
      plot
    }
    
    ### telehealth---------------------------------------------------------------
    else if (input$cost_utilization_trends_grouping2 == "telehealth"){
      plot <- plot_ly() %>%
        
        add_trace(
          data = Onduo_look,
          
          x = ~ journey_month,
          
          y = ~ mean_TH * 100,
          
          type = "scatter",
          
          mode = "lines+markers",
          
          name = "Average Telehealth",
          
          line = list(color = "#3B8EA5")
        ) %>% 
        layout(
          title = "ONDUO - Before and After Event",
          
          xaxis = list(
            title = "Journey Month"#,
            
            # zeroline = TRUE,
            # 
            # zerolinewidth = 2,
            # 
            # zerolinecolor = "black"
          ),
          
          yaxis = list(title = "Average Value Per 100"),
          
          shapes = list(list(
            type = "line",
            
            x0 = 0,
            x1 = 0,
            
            y0 = min(Onduo_look[, 3:6]),
            
            y1 = max(Onduo_look[, 3:6]),
            
            line = list(dash = "dot", color = "black")
            
          ))
          
        )
      
      plot
    }
    
    ### allowed ---------------------------------------------------------------
    else if (input$cost_utilization_trends_grouping2 == "allowed"){
      plot <- plot_ly() %>%
        
        add_trace(
          data = Onduo_look,
          
          x = ~ journey_month,
          
          y = ~ mean_ALLOWED,
          
          type = "scatter",
          
          mode = "lines+markers",
          
          name = "Average Allowed",
          
          line = list(color = "#3B8EA5")
        ) %>% 
        layout(
          title = "ONDUO - Before and After Event",
          
          xaxis = list(
            title = "Journey Month"#,
            
            
            # zeroline = TRUE,
            # 
            # zerolinewidth = 2,
            # 
            # zerolinecolor = "black"
          ),
          
          yaxis = list(title = "Average Value", tickformat = "$,.0f"),
          
          shapes = list(list(
            type = "line",
            
            x0 = 0,
            x1 = 0,
            
            y0 = min(Onduo_look[, 3:6]),
            
            y1 = max(Onduo_look[, 3:6]),
            
            line = list(dash = "dot", color = "black")
            
          ))
          
        )
      
      plot
    }
    
    ### allowed_pharmacy  ---------------------------------------------------------------
    else if (input$cost_utilization_trends_grouping2 == "allowed_pharmacy"){
      plot <- plot_ly() %>%
        
        add_trace(
          data = Onduo_look,
          
          x = ~ journey_month,
          
          y = ~ mean_ALLOWED_PHARM ,
          
          type = "scatter",
          
          mode = "lines+markers",
          
          name = "Average Allowed Pharmacy",
          
          line = list(color = "#3B8EA5")
        ) %>% 
        layout(
          title = "ONDUO - Before and After Event",
          
          xaxis = list(
            title = "Journey Month"#,
            
            # zeroline = TRUE,
            # 
            # zerolinewidth = 2,
            # 
            # zerolinecolor = "black"
          ),
          
          yaxis = list(title = "Average Value",tickformat = "$,.0f"),
          
          shapes = list(list(
            type = "line",
            
            x0 = 0,
            x1 = 0,
            
            y0 = min(Onduo_look[, 3:6]),
            
            y1 = max(Onduo_look[, 3:6]),
            
            line = list(dash = "dot", color = "black")
            
          ))
          
        )
      
      plot
    }
    
    
    ### paid  ---------------------------------------------------------------
    else if (input$cost_utilization_trends_grouping2 == "paid"){
      plot <- plot_ly() %>%
        
        add_trace(
          data = Onduo_look,
          
          x = ~ journey_month,
          
          y = ~ mean_PAID ,
          
          type = "scatter",
          
          mode = "lines+markers",
          
          name = "Average Paid",
          
          line = list(color = "#3B8EA5")
        ) %>% 
        layout(
          title = "ONDUO - Before and After Event",
          
          xaxis = list(
            title = "Journey Month"#,
            
            # zeroline = TRUE,
            # 
            # zerolinewidth = 2,
            # 
            # zerolinecolor = "black"
          ),
          
          yaxis = list(title = "Average Value",tickformat = "$,.0f"),
          
          shapes = list(list(
            type = "line",
            
            x0 = 0,
            x1 = 0,
            
            y0 = min(Onduo_look[, 3:6]),
            
            y1 = max(Onduo_look[, 3:6]),
            
            line = list(dash = "dot", color = "black")
            
          ))
          
        )
      
      plot
    }
    
    
    ### paid_pharmacy  ---------------------------------------------------------------
    else if (input$cost_utilization_trends_grouping2 == "paid_pharmacy"){
      plot <- plot_ly() %>%
        
        add_trace(
          data = Onduo_look,
          
          x = ~ journey_month,
          
          y = ~ mean_PAID_PHARM ,
          
          type = "scatter",
          
          mode = "lines+markers",
          
          name = "Average Paid Pharmacy",
          
          line = list(color = "#3B8EA5")
        ) %>% 
        layout(
          title = "ONDUO - Before and After Event",
          
          xaxis = list(
            title = "Journey Month"#,
            
            # zeroline = TRUE,
            # 
            # zerolinewidth = 2,
            # 
            # zerolinecolor = "black"
          ),
          
          yaxis = list(title = "Average Value",tickformat = "$,.0f"),
          
          shapes = list(list(
            type = "line",
            
            x0 = 0,
            x1 = 0,
            
            y0 = min(Onduo_look[, 3:6]),
            
            y1 = max(Onduo_look[, 3:6]),
            
            line = list(dash = "dot", color = "black")
            
          ))
          
        )
      
      plot
    }
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  ## YOY Comparison Table ---------------------------
  
  output$yoy_comparison_table <- renderReactable({
    
    table <- dataInput_Population() %>%
      
      #merge in all tables with variables we need:
      inner_join(dataInput_Mbr_Monthly(), by = "MEMBERID") %>% 
      
      
      # filter(WEEK_ROUND >= LMP & WEEK_ROUND <= OUTCOME_DATE) %>% 
      mutate(Year = floor_date(MONTH, unit = "year")) %>%
      
      group_by(Year) %>%
      summarise(Total_Members = n_distinct(MASTER_ID),
                Total_Claims = sum(CLAIMCOUNT),
                Total_Paid = sum(PAID)) %>% 
      mutate(Year = substr(Year, 1, 4))
    
    reactable::reactable(table,
                         striped = TRUE,
                         highlight = TRUE,
                         # bordered = TRUE,
                         # filterable = TRUE,
                         sortable = TRUE,
                         defaultPageSize = 10,
                         #defaultSorted = list(Percent = "desc"),
                         defaultColDef = colDef(align = "center", format = colFormat(separators = TRUE)),
                         columns = list(
                           Year = colDef(style = list(fontWeight = "bold", format = colFormat(separators = FALSE))),
                           
                           Total_Members = colDef(name = "Total Members", align = "center", format = colFormat(separators = TRUE),
                                                  style = color_scales(table, colors = c("lightblue", "#3B8EA5"), bias = 2)),
                           Total_Claims = colDef(name = "Total Claims", align = "center", format = colFormat(separators = TRUE),
                                                 style = color_scales(table, colors = c("#9ee1c6", "#66A597"), bias = 2)),
                           Total_Paid = colDef(name = "Total Paid", align = "center",
                                               format = colFormat(currency = "USD", separators = TRUE, locales = "en-US"),
                                               style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2))
                         )
    )
    
  }) #END yoy_comparison_table
  
  ## Preg Outcomes Breakout Table -------------------------
  
  output$maternity_summary_table <- renderReactable({
    
    table <- dataInput_Maternity() %>% 
      
      inner_join(dataInput_Population(), by = c("MEMBERID" = "MEMBERID")) %>% 
      
      # filter(WEEK_ROUND >= LMP & WEEK_ROUND <= OUTCOME_DATE) %>%
      mutate(Estimated_Preg_Start = if_else(is.na(LMP), FIRST_PREGNANCY_CODE, LMP)) %>% 
      mutate(Year = floor_date(Estimated_Preg_Start, unit = "year")) %>%
      
      group_by(Year, PREGNANCY_ID) %>% 
      summarise(across(c(DELIVERY_UNCOMP, DELIVERY_LB, DELIVERY_VAGINAL, DELIVERY_COMPLICATIONS,
                         PRETERM_WEEKS, DELIVERY_CESAREAN, DELIVERY_ABORTION), ~max(.x))) %>%  # cost/utilization
      ungroup() %>% 
      group_by(Year) %>% 
      summarise(
        Pregnancies = n_distinct(PREGNANCY_ID),
        # Uncomplicated = sum(DELIVERY_UNCOMP, na.rm = TRUE),
        LiveBirth = sum(DELIVERY_LB, na.rm = TRUE),
        # Vbirth = sum(DELIVERY_VAGINAL, na.rm = TRUE),
        Cbirth = sum(DELIVERY_CESAREAN, na.rm = TRUE),
        Abortions = sum(DELIVERY_ABORTION, na.rm = TRUE),
        Complicated = sum(DELIVERY_COMPLICATIONS, na.rm = TRUE),
        Preterm = sum(PRETERM_WEEKS, na.rm = TRUE)
      ) %>% 
      mutate(Year = substr(Year, 1, 4)) %>% 
      mutate(#Uncomplicated_Pct = (Uncomplicated/Pregnancies),
        LiveBirth_Pct = (LiveBirth/Pregnancies),
        # Vbirth_Pct = (Vbirth/Pregnancies),
        Cbirth_Pct = (Cbirth/Pregnancies),
        Abortions_Pct = (Abortions/Pregnancies),
        Complicated_Pct = (Complicated/Pregnancies),
        Preterm_Pct = (Preterm/Pregnancies)
      ) 
    
    reactable::reactable(table,
                         striped = TRUE, 
                         highlight = TRUE,
                         compact = TRUE,
                         resizable = TRUE,
                         # bordered = TRUE,
                         # filterable = TRUE,
                         sortable = TRUE,
                         defaultPageSize = 20,
                         #defaultSorted = list(Percent = "desc"),  
                         defaultColDef = colDef(align = "center", format = colFormat(separators = TRUE)),
                         columns = list(
                           Year = colDef(name = "Year", style = list(fontWeight = "bold"), format = colFormat(separators = FALSE)),
                           
                           Pregnancies = colDef(name = "Pregnancies", align = "center", format = colFormat(separators = TRUE),
                                                style = color_scales(table, colors = c("lightblue", "#3B8EA5"), bias = 2)),
                           
                           # Uncomplicated = colDef(name = "(n)", style = color_scales(table, colors = c("#9ee1c6", "#66A597"), bias = 2)),
                           LiveBirth = colDef(name = "(n)", style = color_scales(table, colors = c("#9ee1c6", "#66A597"), bias = 2)),
                           # Vbirth = colDef(name = "(n)", style = color_scales(table, colors = c("#9ee1c6", "#66A597"), bias = 2)),
                           Cbirth = colDef(name = "(n)", style = color_scales(table, colors = c("#e6d6df", "#995a7d"), bias = 2)),
                           Abortions = colDef(name = "(n)", style = color_scales(table, colors = c("#e6d6df", "#995a7d"), bias = 2)),
                           Complicated = colDef(name = "(n)", style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           Preterm = colDef(name = "(n)", style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           # MM = colDef(name = "Maternal Morbidity", style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           # SMM = colDef(name = "(n)", style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           
                           # Uncomplicated_Pct = colDef(name = "(%)", format = colFormat(percent = TRUE, digits = 1), style = color_scales(table, colors = c("#9ee1c6", "#66A597"), bias = 2)),
                           LiveBirth_Pct = colDef(name = "(%)", format = colFormat(percent = TRUE, digits = 1), style = color_scales(table, colors = c("#9ee1c6", "#66A597"), bias = 2)),
                           # Vbirth_Pct = colDef(name = "(%)", format = colFormat(percent = TRUE, digits = 1), style = color_scales(table, colors = c("#9ee1c6", "#66A597"), bias = 2)),
                           Cbirth_Pct = colDef(name = "(%)", format = colFormat(percent = TRUE, digits = 1), style = color_scales(table, colors = c("#e6d6df", "#995a7d"), bias = 2)),
                           Abortions_Pct = colDef(name = "(%)", format = colFormat(percent = TRUE, digits = 1), style = color_scales(table, colors = c("#e6d6df", "#995a7d"), bias = 2)),
                           Complicated_Pct = colDef(name = "(%)", format = colFormat(percent = TRUE, digits = 1), style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           Preterm_Pct = colDef(name = "(%)", format = colFormat(percent = TRUE, digits = 1), style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2))
                           # MM = colDef(name = "(%)", style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           # SMM_Pct = colDef(name = "(%)", format = colFormat(percent = TRUE, digits = 1), style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2))
                         ), 
                         columnGroups = list(
                           # colGroup(name = "Uncomplicated", columns = c("Uncomplicated", "Uncomplicated_Pct")),
                           colGroup(name = "Live Births", columns = c("LiveBirth", "LiveBirth_Pct")),
                           # colGroup(name = "Vaginal Births", columns = c("Vbirth", "Vbirth_Pct")),
                           colGroup(name = "C-sections", columns = c("Cbirth", "Cbirth_Pct")),
                           colGroup(name = "Abortions", columns = c("Abortions", "Abortions_Pct")),
                           colGroup(name = "Complications", columns = c("Complicated", "Complicated_Pct")),
                           colGroup(name = "Preterm", columns = c("Preterm", "Preterm_Pct"))#,
                           # colGroup(name = "Severe Maternal Morbidity", columns = c("SMM", "SMM_Pct"))
                         )
    )
  }) # END preg_outcomes_breakout_table
  
  
  ## Condition Breakout Table -------------------------
  
  output$cc_summary_table <- renderReactable({
    
    table <- dataInput_Mbr_Monthly() %>% 
      
      inner_join(dataInput_Population(), by = c("MEMBERID" = "MEMBERID")) %>% 
      
      mutate(Year = floor_date(MONTH, unit =  "year")) %>%
      group_by(Year, MASTER_ID) %>% 
      summarise(across(c(DIABETES, HYPERTENSION, OBESITY, STI, SUD), ~max(.x))) %>%  # cost/utilization
      ungroup() %>% 
      group_by(Year) %>% 
      summarise(
        Members = n_distinct(MASTER_ID),
        Diabetes = sum(DIABETES, na.rm = TRUE),
        Hypertension = sum(HYPERTENSION, na.rm = TRUE),
        Obesity = sum(OBESITY, na.rm = TRUE),
        STI = sum(STI, na.rm = TRUE),
        SUD = sum(SUD, na.rm = TRUE)
      ) %>% 
      mutate(Year = substr(Year, 1, 4)) %>% 
      mutate(Diabetes_Pct = Diabetes/Members,
             Hypertension_Pct = Hypertension/Members,
             Obesity_Pct = Obesity/Members,
             STI_Pct = STI/Members,
             SUD_Pct = SUD/Members
      ) %>% 
      select(-c(Members))
    
    reactable::reactable(table,
                         striped = TRUE, 
                         highlight = TRUE,
                         compact = TRUE,
                         # bordered = TRUE,
                         # filterable = TRUE,
                         sortable = TRUE,
                         defaultPageSize = 20,
                         #defaultSorted = list(Percent = "desc"),  
                         defaultColDef = colDef(align = "center", format = colFormat(separators = TRUE)),
                         columns = list(
                           Year = colDef(style = list(fontWeight = "bold"), format = colFormat(separators = FALSE)),
                           
                           Diabetes = colDef(name = "(n)", style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           Hypertension = colDef(name = "(n)", style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           Obesity = colDef(name = "(n)", style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           STI = colDef(name = "(n)", style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           SUD = colDef(name = "(n)", style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           
                           Diabetes_Pct = colDef(name = "(%)", format = colFormat(percent = TRUE, digits = 1), style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           Hypertension_Pct = colDef(name = "(%)", format = colFormat(percent = TRUE, digits = 1), style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           Obesity_Pct = colDef(name = "(%)", format = colFormat(percent = TRUE, digits = 1), style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           STI_Pct = colDef(name = "(%)", format = colFormat(percent = TRUE, digits = 1), style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           SUD_Pct = colDef(name = "(%)", format = colFormat(percent = TRUE, digits = 1), style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2))
                         ),
                         columnGroups = list(
                           colGroup(name = "Diabetes",  columns = c("Diabetes", "Diabetes_Pct")),
                           colGroup(name = "Hypertension",  columns = c("Hypertension", "Hypertension_Pct")),
                           colGroup(name = "Obesity",  columns = c("Obesity", "Obesity_Pct")),
                           colGroup(name = "STI",  columns = c("STI", "STI_Pct")),
                           colGroup(name = "SUD",  columns = c("SUD", "SUD_Pct"))
                         )
    )
  }) #END cc_breakout_table
  
  ## Characteristics Summary Table -------------------------
  
  output$characteristics_summary_table <- renderReactable({
    
    # Step 1: Summarise the counts by specialty and type
    base_table <- dataInput_Mbr_Monthly() %>% 
      
      inner_join(dataInput_Population(), by = c("MEMBERID" = "MEMBERID")) %>% 
      
      mutate(Year = floor_date(MONTH, unit =  "year")) %>%
      select(Year, MEMBERID, MASTER_ID, AGE_CAT) %>% #cut the extra columns to save processing power
      left_join(
        dataInput_Static_Mbr_Level() %>% 
          select(Mbr_ID, Race, Female), #cut the extra columns to save processing power
        by = c("MEMBERID"="Mbr_ID")
      ) %>% 
      mutate(AGE_CAT = recode(AGE_CAT, "Missing" = "Missing Age Cat")) %>% #recoding this will stop us from getting a Missing.x and Missing.y from combining our Cat columns into one table later
      mutate(Race = recode(Race, "Missing" = "Missing Race")) %>% #recoding this will stop us from getting a Missing.x and Missing.y from combining our Cat columns into one table later
      
      group_by(Year, MASTER_ID) %>%  
      summarise(AGE_CAT = unique(AGE_CAT), 
                Race = unique(Race),
                Female = max(Female)) %>% 
      ungroup()
    
    
    # Calculate Counts by Year, Age Category, and Race
    age_counts <- base_table %>%
      # filter(AGE_CAT != "Young Child" & AGE_CAT != "Baby") %>%
      group_by(Year, AGE_CAT) %>%
      summarise(Age_Count = n()) %>%
      pivot_wider(names_from = AGE_CAT, values_from = Age_Count, values_fill = 0) %>% 
      ungroup()
    
    race_counts <- base_table %>%
      group_by(Year, Race) %>%
      summarise(Race_Count = n()) %>%
      pivot_wider(names_from = Race, values_from = Race_Count, values_fill = 0) %>% 
      ungroup()
    
    # Merge Counts into One Table
    table <- age_counts %>%
      full_join(race_counts, by = "Year") %>%
      mutate(Year = substr(Year, 1, 4))
    
    age_columns <- colnames(table)[colnames(table) %in% unique(base_table$AGE_CAT)]
    race_columns <- colnames(table)[colnames(table) %in% unique(base_table$Race)]
    
    table <- table %>%
      mutate(Age_Sums = rowSums(select(., all_of(age_columns))),
             Race_Sums = rowSums(select(., all_of(race_columns)))
      ) %>% 
      mutate(
        across(all_of(age_columns), ~(.x/Age_Sums), .names = "{.col}_Pct"),
        across(all_of(race_columns), ~(.x/Race_Sums), .names = "{.col}_Pct")
      ) %>% 
      select(-contains("Sums")) %>% 
      
      #doing this because we're having issues using columns with spaces instead of underscores
      #this must be done AFTER the the code generating the percents above
      #because the age age_columns and race_columns don't have spaces in them;
      #we won't be able to the mutates with the all_of() a few lines above because it won't be able to match the column names;
      #Just make sure the rename() is last...
      rename_with(~ gsub(" ", "_", .x)) 
    
    
    # Switch to Percentages if "Percentages" view is selected
    # req(input$count_pct_switch) #Ensure input$view_mode is available before preceding
    # 
    # if (input$count_pct_switch == "pct") { # switchInput returns FALSE when toggled to "Percentages"
    #   # Identify columns for Age and Race dynamically
    #   age_columns <- colnames(table)[colnames(table) %in% unique(base_table$Age_Cat)]
    #   race_columns <- colnames(table)[colnames(table) %in% unique(base_table$Race)]
    #   
    #   # Calculate totals for each category
    #   table <- table %>%
    #     mutate(Age_Sums = rowSums(select(., all_of(age_columns))),
    #            Race_Sums = rowSums(select(., all_of(race_columns)))
    #     ) %>% 
    #     mutate(
    #       across(all_of(age_columns), ~(.x/Age_Sums), .names = "{.col} (%)"),
    #       across(all_of(race_columns), ~(.x/Race_Sums), .names = "{.col} (%)")
    #     ) %>% 
    #     select(Year, contains("%"))
    # } else if (input$count_pct_switch == "count") {
    #   table
    # }
    
    numeric_columns <- table %>% select(where(is.numeric))
    
    
    column_groups <- list()
    
    if (all(c("Baby", "Baby_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Baby", columns = c("Baby", "Baby_Pct"))))
    }
    if (all(c("Child", "Child_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Child", columns = c("Child", "Child_Pct"))))
    }
    if (all(c("Young_Child", "Young_Child_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Young_Child", columns = c("Young_Child", "Young_Child_Pct"))))
    }
    if (all(c("Teen", "Teen_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Teen", columns = c("Teen", "Teen_Pct"))))
    }
    if (all(c("Young_Adult", "Young_Adult_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Young Adult", columns = c("Young_Adult", "Young_Adult_Pct"))))
    }
    if (all(c("Adult", "Adult_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Adult", columns = c("Adult", "Adult_Pct"))))
    }
    if (all(c("Senior", "Senior_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Senior", columns = c("Senior", "Senior_Pct"))))
    }
    if (all(c("Missing_Age_Cat", "Missing_Age_Cat_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Missing_Age_Cat", columns = c("Missing_Age_Cat", "Missing_Age_Cat_Pct"))))
    }
    if (all(c("Asian", "Asian_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Asian", columns = c("Asian", "Asian_Pct"))))
    }
    if (all(c("Black", "Black_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Black", columns = c("Black", "Black_Pct"))))
    }
    if (all(c("Hispanic", "Hispanic_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Hispanic", columns = c("Hispanic", "Hispanic_Pct"))))
    }
    if (all(c("White", "White_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "White", columns = c("White", "White_Pct"))))
    }
    if (all(c("Other", "Other_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Other", columns = c("Other", "Other_Pct"))))
    }
    if (all(c("Missing_Race", "Missing_Race_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Missing_Race", columns = c("Missing_Race", "Missing_Race_Pct"))))
    }
    
    
    reactable::reactable(table,
                         striped = TRUE, 
                         highlight = TRUE,
                         compact = TRUE,
                         resizable = TRUE,
                         # bordered = TRUE,
                         # filterable = TRUE,
                         sortable = TRUE,
                         defaultPageSize = 10,
                         #defaultSorted = list(Percent = "desc"),  
                         defaultColDef = colDef(
                           align = "center",
                           format = colFormat(separators = TRUE),
                           style = color_scales(numeric_columns, span = 1:ncol(numeric_columns), colors = c("lightblue", "#3B8EA5"), bias = 2)),
                         
                         # Here I'm renaming the count columns to be (n) and the percent columns to be (%)
                         # It won't matter that they have the same display names because we'll group them with columnGroups() further down
                         # columnGroups() will still refer to their actual column names in the table
                         # !!! Another note
                         
                         columns = list(
                           Year = colDef(style = list(fontWeight = "bold")),
                           Pct_Female = colDef(name = "Female (%)", align = "center", format = colFormat(digits = 1)),
                           
                           #Age Cat count renames:
                           Baby = colDef(name = "(n)"),
                           Young_Child = colDef(name = "(n)"),
                           Child = colDef(name = "(n)"),
                           Teen = colDef(name = "(n)"),
                           Young_Adult = colDef(name = "(n)"),
                           Adult = colDef(name = "(n)"),
                           Senior = colDef(name = "(n)"),
                           Missing_Age_Cat = colDef(name = "(n)"),
                           
                           Asian = colDef(name = "(n)"),
                           Black = colDef(name = "(n)"),
                           Hispanic = colDef(name = "(n)"),
                           White = colDef(name = "(n)"),
                           Other = colDef(name = "(n)"),
                           Missing_Race = colDef(name = "(n)"),
                           
                           
                           Baby_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Child_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Young_Child_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Teen_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Young_Adult_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Adult_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Senior_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Missing_Age_Cat_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           
                           
                           Asian_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Black_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Hispanic_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           White_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Other_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Missing_Race_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1))
                           
                           
                           # Missing.x = colDef(name = "(Missing)n) Age", align = "center"), #if this column is present then it's for Age_Cat because that came first in the final table
                           # Missing.y = colDef(name = "Missing Race", align = "center")
                           #Grand_Total = colDef(name = "Total", align = "right", aggregate = "sum")
                         ),
                         columnGroups = column_groups
                         
    ) # end reactable
  }) # END characteristics_summary_table
  
  
  ## Location Summary Table -------------------------
  
  output$location_summary_table <- renderReactable({
    
    # Step 1: Summarise the counts by specialty and type
    table <- dataInput_Mbr_Monthly() %>%
      
      inner_join(dataInput_Population(), by = c("MEMBERID" = "MEMBERID")) %>% 
      
      mutate(Year = floor_date(MONTH, unit =  "year")) %>%
      select(Year, MEMBERID, MASTER_ID) %>% #cut the extra columns to save processing power
      left_join(
        dataInput_Static_Mbr_Level() %>% 
          select(Mbr_ID, Rural_County_Cat), #cut the extra columns to save processing power
        by = c("MEMBERID"="Mbr_ID")
      ) %>% 
      
      group_by(Year, Rural_County_Cat) %>% 
      summarise(Rural_County_Count = n()) %>% 
      ungroup() %>% 
      pivot_wider(names_from = Rural_County_Cat, values_from = Rural_County_Count, values_fill = 0) %>%
      
      mutate(Year = substr(Year, 1, 4))
    
    rural_columns <- colnames(table)[colnames(table) %in% unique(dataInput_Static_Mbr_Level()$Rural_County_Cat)]
    
    table <- table %>% 
      mutate(Rural_Sums = rowSums(select(., all_of(rural_columns)))) %>% 
      mutate(across(all_of(rural_columns), ~(.x/Rural_Sums), .names = "{.col}_Pct")) %>% 
      select(-contains("Sums")) %>% 
      
      #doing this because we're having issues using columns with spaces instead of underscores
      #this must be done AFTER the the code generating the percents above
      #because the age age_columns and race_columns don't have spaces in them;
      #we won't be able to the mutates with the all_of() a few lines above because it won't be able to match the column names;
      #Just make sure the rename() is last...
      rename_with(~ gsub(" ", "_", .x)) 
    
    
    column_groups <- list()
    
    if (all(c("Very_Rural", "Very_Rural_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Very Rural", columns = c("Very_Rural", "Very_Rural_Pct"))))
    }
    if (all(c("Rural", "Rural_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Rural", columns = c("Rural", "Rural_Pct"))))
    }
    if (all(c("Metro", "Metro_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Metro", columns = c("Metro", "Metro_Pct"))))
    }
    if (all(c("Urban", "Urban_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Urban", columns = c("Urban", "Urban_Pct"))))
    }
    if (all(c("Major_City", "Major_City_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Major City", columns = c("Major_City", "Major_City_Pct"))))
    }
    if (all(c("Missing", "Missing_Pct") %in% colnames(table))) {
      column_groups <- append(column_groups, list(colGroup(name = "Missing", columns = c("Missing", "Missing_Pct"))))
    }
    
    
    numeric_columns <- table %>% select(where(is.numeric))
    
    reactable::reactable(table,
                         striped = TRUE, 
                         highlight = TRUE,
                         compact = TRUE,
                         resizable = TRUE,
                         sortable = TRUE,
                         defaultPageSize = 10,
                         #defaultSorted = list(Percent = "desc"),  
                         # defaultColDef = colDef(align = "center", format = colFormat(separators = TRUE), style = color_scales(numeric_columns, span = 2:ncol(numeric_columns), colors = c("lightblue", "#3B8EA5"), bias = 2)),
                         defaultColDef = colDef(align = "center", format = colFormat(separators = TRUE), style = color_scales(numeric_columns, colors = c("lightblue", "#3B8EA5"), bias = 2)),
                         columns = list(
                           Year = colDef(style = list(fontWeight = "bold")),
                           
                           Very_Rural = colDef(name = "(n)"),
                           Rural = colDef(name = "(n)"),
                           Metro = colDef(name = "(n)"),
                           Urban = colDef(name = "(n)"),
                           Major_City = colDef(name = "(n)"),
                           Missing = colDef(name = "(n)"),
                           
                           Very_Rural_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Rural_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Metro_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Urban_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Major_City_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1)),
                           Missing_Pct = colDef(name = "(%)", align = "center", format = colFormat(percent = TRUE, digits = 1))
                         ),
                         columnGroups = column_groups
    ) #end reactable
  }) # END location_summary_table
  
  
  ## Utilization Summary Table ------------------------------------------------
  
  output$utilization_summary_table <- renderReactable({
    
    # Step 1: Bring in general ER, IP, PMPM, and Pharm
    table <- dataInput_General_ER() %>%
      
      # Match with our population:
      inner_join(dataInput_Population(), by = c("MBR_ID" = "MEMBERID")) %>% 
      
      select(MBR_ID, INTEGER_CLAIM_NUMBER, SVC_SERVICE_FRM_DATE, REV_PAID_AMT) %>% 
      mutate(Year = year(SVC_SERVICE_FRM_DATE)) %>% 
      
      group_by(Year) %>% 
      summarise(ER_Visits = n_distinct(INTEGER_CLAIM_NUMBER),
                Member_Count = n_distinct(MBR_ID),
                ER_Paid = sum(REV_PAID_AMT)) %>% 
      mutate(ER_Rate_per_100 = round((ER_Visits/Member_Count) * 100,1)) %>%  #rate per 100 patients
      select(Year, ER_Visits, ER_Rate_per_100, ER_Paid) %>% 
      left_join(
        dataInput_BH_ER() %>%
          select(MBR_ID, INTEGER_CLAIM_NUMBER, SVC_SERVICE_FRM_DATE, REV_PAID_AMT) %>% 
          mutate(Year = year(SVC_SERVICE_FRM_DATE)) %>% 
          group_by(Year) %>% 
          summarise(BH_ER_Visits = n_distinct(INTEGER_CLAIM_NUMBER),
                    Member_Count = n_distinct(MBR_ID),
                    BH_ER_Paid = sum(REV_PAID_AMT)) %>% 
          mutate(BH_ER_Rate_per_100 = round((BH_ER_Visits/Member_Count) * 100,1)) %>%  #rate per 100 patients
          select(Year, BH_ER_Visits, BH_ER_Rate_per_100, BH_ER_Paid)  
        ,by = "Year"
      ) %>% 
      left_join(
        dataInput_General_IP() %>%
          select(MBR_ID, ADMISSIONID, SVC_SERVICE_FRM_DATE, REV_PAID_AMT) %>% 
          mutate(Year = year(SVC_SERVICE_FRM_DATE)) %>% 
          group_by(Year) %>% 
          summarise(IP_Visits = n_distinct(ADMISSIONID),
                    Member_Count = n_distinct(MBR_ID),
                    IP_Paid = sum(REV_PAID_AMT)) %>% 
          mutate(IP_Rate_per_100 = round((IP_Visits/Member_Count) * 100,1)) %>%  #rate per 100 patients
          select(Year, IP_Visits, IP_Rate_per_100, IP_Paid)
        ,by = "Year"
      ) %>% 
      left_join(
        dataInput_BH_IP() %>%
          select(MBR_ID, ADMISSIONID, SVC_SERVICE_FRM_DATE, REV_PAID_AMT) %>% 
          mutate(Year = year(SVC_SERVICE_FRM_DATE)) %>% 
          group_by(Year) %>% 
          summarise(BH_IP_Visits = n_distinct(ADMISSIONID),
                    Member_Count = n_distinct(MBR_ID),
                    BH_IP_Paid = sum(REV_PAID_AMT)) %>% 
          mutate(BH_IP_Rate_per_100 = round((BH_IP_Visits/Member_Count) * 100,1)) %>%  #rate per 100 patients
          select(Year, BH_IP_Visits, BH_IP_Rate_per_100, BH_IP_Paid)  
        ,by = "Year"
      )  %>% 
      
      mutate(Year = substr(Year, 1, 4))
    
    reactable::reactable(table,
                         striped = TRUE, 
                         highlight = TRUE,
                         # bordered = TRUE,
                         # filterable = TRUE,
                         sortable = TRUE,
                         defaultPageSize = 10,
                         #defaultSorted = list(Percent = "desc"),  
                         defaultColDef = colDef(align = "center"),
                         columns = list(
                           Year = colDef(style = list(fontWeight = "bold")),
                           ER_Visits = colDef(name = "ER Visits", align = "center", format = colFormat(separators = TRUE),
                                              style = color_scales(table, colors = c("#9ee1c6", "#66A597"), bias = 2)),
                           ER_Rate_per_100 = colDef(name = "ER per 100", align = "center", format = colFormat(separators = TRUE),
                                                    style = color_scales(table, colors = c("#9ee1c6", "#66A597"), bias = 2)),
                           ER_Paid = colDef(name = "ER Paid", align = "center", format = colFormat(currency = "USD", separators = TRUE),
                                            style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           BH_ER_Visits = colDef(name = "ER Visits (BH)", align = "center", format = colFormat(separators = TRUE),
                                                 style = color_scales(table, colors = c("#9ee1c6", "#66A597"), bias = 2)),
                           BH_ER_Rate_per_100 = colDef(name = "ER per 100 (BH)", align = "center", format = colFormat(separators = TRUE),
                                                       style = color_scales(table, colors = c("#9ee1c6", "#66A597"), bias = 2)),
                           BH_ER_Paid = colDef(name = "ER Paid (BH)", align = "center", format = colFormat(currency = "USD", separators = TRUE),
                                               style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           IP_Visits = colDef(name = "IP Visits", align = "center", format = colFormat(separators = TRUE),
                                              style = color_scales(table, colors = c("lightblue", "#2D728F"), bias = 2)),
                           IP_Rate_per_100 = colDef(name = "IP per 100", align = "center", format = colFormat(separators = TRUE),
                                                    style = color_scales(table, colors = c("lightblue", "#2D728F"), bias = 2)),
                           IP_Paid = colDef(name = "IP Paid", align = "center", format = colFormat(currency = "USD", separators = TRUE),
                                            style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2)),
                           BH_IP_Visits = colDef(name = "IP Visits (BH)", align = "center", format = colFormat(separators = TRUE),
                                                 style = color_scales(table, colors = c("lightblue", "#2D728F"), bias = 2)),
                           BH_IP_Rate_per_100 = colDef(name = "IP per 100 (BH)", align = "center", format = colFormat(separators = TRUE),
                                                       style = color_scales(table, colors = c("lightblue", "#2D728F"), bias = 2)),
                           BH_IP_Paid = colDef(name = "IP Paid (BH)", align = "center", format = colFormat(currency = "USD", separators = TRUE),
                                               style = color_scales(table, colors = c("#f6c5be", "#c64633"), bias = 2))
                         )
    )
  }) # END utilization_summary_table
  
  
  
  # EXPECTATIONS ------------------------------------------------------------
  
  # output$pregnancy_milestones <- renderPlotly({
  #   
  #   # Create a data frame with all relevant information for the pregnancy timeline
  #   pregnancy_timeline <- data.frame(
  #     weeks = c(1, 8, 6, 12, 13, 16, 20, 20, 24, 28, 32, 36, 39, 40),
  #     events = c("Conception", "First Prenatal Visit", "Heartbeat Detected", "First Ultrasound", 
  #                "Nuchal Translucency Test", "Quickening (Fetal Movements)", "Anatomy Ultrasound",
  #                "Gender Reveal", "Glucose Screening", "Frequent Check-ups", "Braxton Hicks Contractions", 
  #                "Group B Strep Test", "Full Term", "Labor & Delivery"),
  #     details = c("Fertilization and implantation", "Confirm pregnancy, estimate due date",
  #                 "Detect baby's heartbeat via ultrasound", "Confirm normal development",
  #                 "Screen for chromosomal abnormalities", "First noticeable baby movements",
  #                 "Detailed scan for growth and abnormalities", "Optional: Find out baby's sex",
  #                 "Screening for gestational diabetes", "More frequent prenatal check-ups",
  #                 "Mild contractions as body prepares for labor", "Screen for Group B Strep",
  #                 "Baby considered full term", "Prepare for labor and delivery")
  #   )
  #   
  #   # Alternate the position of the labels (above/below the timeline)
  #   pregnancy_timeline$y_positions <- ifelse(seq_along(pregnancy_timeline$weeks) %% 2 == 0, 0.2, -0.2)
  #   
  #   # Create timeline plot without text, we'll add annotations for labels
  #   fig <- plot_ly(
  #     data = pregnancy_timeline, 
  #     x = ~weeks, y = ~y_positions,
  #     hovertext = ~paste("Event: ", events, "<br>Details: ", details),  # Detailed tooltips
  #     mode = "markers", 
  #     marker = list(size = 12, color = "rgb(255, 127, 14)"),
  #     hoverinfo = "text"  # Display hovertext on hover
  #   )
  #   
  #   # Add milestone lines as red dashed lines
  #   fig <- fig %>%
  #     add_segments(x = ~weeks, xend = ~weeks, y = 0, yend = ~y_positions, 
  #                  line = list(color = "red", dash = "dash"))
  #   
  #   # Add annotations for event labels
  #   for(i in 1:nrow(pregnancy_timeline)) {
  #     fig <- fig %>% add_annotations(
  #       x = pregnancy_timeline$weeks[i],
  #       y = pregnancy_timeline$y_positions[i],
  #       text = pregnancy_timeline$events[i],
  #       showarrow = TRUE,
  #       arrowhead = 2,
  #       ax = 0,  # x offset for the annotation text
  #       ay = ifelse(pregnancy_timeline$y_positions[i] > 0, -40, 40),  # Adjust position to avoid overlap
  #       font = list(size = 12)
  #     )
  #   }
  #   
  #   # Add trimester labels
  #   fig <- fig %>%
  #     add_annotations(x = 6, y = -0.35, text = "First Trimester", showarrow = FALSE, font = list(size = 12)) %>%
  #     add_annotations(x = 20, y = -0.35, text = "Second Trimester", showarrow = FALSE, font = list(size = 12)) %>%
  #     add_annotations(x = 33, y = -0.35, text = "Third Trimester", showarrow = FALSE, font = list(size = 12))
  #   
  #   # Customize layout with larger hover text size
  #   fig <- fig %>%
  #     layout(
  #       title = "Pregnancy Timeline by Gestational Weeks",
  #       xaxis = list(title = "Weeks of Pregnancy", range = c(0, 41), tickvals = seq(0, 40, by = 4)),
  #       yaxis = list(visible = FALSE),
  #       hoverlabel = list(bgcolor = "white", font = list(size = 14)),  # Increase hover text font size
  #       showlegend = FALSE
  #     )
  #   
  # }) #END pregnancy milestones 
  
  
  # UTILIZATION --------------------------------------------------------------
  
  # Reactive expression to select the dataset based on input:
  selected_data_utilization_patterns <- reactive({
    if (input$admission_type_1 == "er"){
      dataInput_General_ER()
    }
    else{
      dataInput_General_IP()
    }
  }) #END selected_data_utilization_patterns
  
  
  ## Event Study ------------------
  
  ### ER -----------------
  output$utilization_patterns_event_study <- renderPlotly({
    
    # if (input$event_study_options == "general" & input$admission_type_1 == "er"){
    if (input$admission_type_1 == "er"){
      
      selected_data_utilization_patterns() %>% 
        filter(!is.na(SVC_SERVICE_TO_DATE)) %>% # get any rid of empty ER visits (members without ER visits) before merge
        
        # apply any user selection:
        filter(
          if(!is.null(input$code_prefix) && input$event_study_codes == "specific"){
            #KEEP codes prefixed with desired user input:
            str_detect(MEDICAL_DIAG_DESC_CODE, paste0("\\(", input$code_prefix))
          }
          else {
            #no filtering, keep all rows
            TRUE
          }
        ) %>% 
        
        # Match with our population:
        inner_join(dataInput_Population(), by = c("MBR_ID" = "MEMBERID")) %>%  
        
        # Bring in start and stop dates
        inner_join(dataInput_Static_Mbr_Level() %>% select(Mbr_ID, FIRSTDATE, LASTDATE), by = c("MBR_ID" = "Mbr_ID")) %>% 
        
        #Filter the my Utilization Claims table to be within the +/-1 year from pregnancy/delivery that Joe created: 
        #filter(SVC_SERVICE_TO_DATE >= LEFT_TRUNC & SVC_SERVICE_TO_DATE <= RIGHT_TRUNC) %>% 
        
        # Bring in Celeste Enrollment dates:
        inner_join(dataInput_Celeste_Programs(), by = c("MBR_ID" = "MEMBER_ID")) %>% 
        
        # Apply any vendor filters:
        filter(
          if (input$event_study_event_selection == "celeste_enroll") {
            VENDOR %in% input$celeste_vendor
          }
          else {
            TRUE
          }
        ) %>% 
        
        #Create timing based around user inputs on weeks/months, pregnancy start/end:
        mutate(ER_Timing = floor_date(as.Date(SVC_SERVICE_TO_DATE), unit = "month"),
               Event_Timing = floor_date(
                 as.Date(
                   if (input$event_study_event_selection == "start") {
                     FIRSTDATE
                   } 
                   else if (input$event_study_event_selection == "celeste_enroll") {
                     ENROLLMENT_START_DATE
                   } 
                   else if(input$event_study_event_selection == "three_months_ago"){
                     three_months_ago
                   }
                 ),
                 unit = "month"
               )) %>% 
        
        # we'll do the interval based on whether week or month is selected:
        mutate(Period = interval(Event_Timing, ER_Timing) %/% months(1)) %>% 
        # if_else(input$event_study_time_grouping == "month", months(1), weeks(1))) %>% 
        
        #filter down on the specified range:
        filter(Period >= input$event_study_period_range[1] & Period <= input$event_study_period_range[2]) %>% 
        
        group_by(Period) %>% 
        summarise(ER_Visits = n_distinct(INTEGER_CLAIM_NUMBER)) %>% 
        plot_ly(x = ~Period, y = ~ER_Visits, 
                type = "bar",
                text = ~paste("Period:", Period, "<br>Total Visits:", scales::label_comma()(ER_Visits)), 
                hoverinfo = "text", textposition = 'auto',
                marker = list(color = '#f78b92',
                              line = list(color = '#cf4951', width = 1.5))) %>% 
        layout(title = "", 
               xaxis = list(title = "Period (Pre/Event/Post)"),
               # xaxis = list(title = paste(input$histogram_input)), 
               # yaxis = list(title = ""),
               autosize = TRUE)
    } #end if
    
    
    ### IP -----------------
    # else if (input$event_study_options == "general" & input$admission_type_1 == "ip"){
    else if (input$admission_type_1 == "ip"){
      
      selected_data_utilization_patterns() %>% 
        filter(!is.na(SVC_SERVICE_TO_DATE)) %>% # get any rid of empty IP visits (members without IP visits) before merge
        
        # apply any user selection:
        filter(
          if(!is.null(input$code_prefix) && input$event_study_codes == "specific"){
            #KEEP codes prefixed with desired user input:
            str_detect(MEDICAL_DIAG_DESC_CODE, paste0("\\(", input$code_prefix))
          }
          else {
            #no filtering, keep all rows
            TRUE
          }
        ) %>% 
        
        # Match with our population:
        inner_join(dataInput_Population(), by = c("MBR_ID" = "MEMBERID")) %>%  
        
        # Bring in start and stop dates:
        inner_join(dataInput_Static_Mbr_Level() %>% select(Mbr_ID, FIRSTDATE, LASTDATE), by = c("MBR_ID" = "Mbr_ID")) %>% 
        
        # Bring in Celeste Enrollment dates:
        inner_join(dataInput_Celeste_Programs(), by = c("MBR_ID" = "MEMBER_ID")) %>% 
        
        # Apply any vendor filters:
        filter(
          if (input$event_study_event_selection == "celeste_enroll") {
            VENDOR %in% input$celeste_vendor
          }
          else {
            TRUE
          }
        ) %>%  
        
        #Create timing based around user inputs on weeks/months, pregnancy start/end:
        mutate(IP_Timing = floor_date(as.Date(SVC_SERVICE_TO_DATE), unit = "month"),
               Event_Timing = floor_date(
                 as.Date(
                   if (input$event_study_event_selection == "start") {
                     FIRSTDATE
                   } 
                   else if (input$event_study_event_selection == "celeste_enroll") {
                     ENROLLMENT_START_DATE
                   } 
                   else if(input$event_study_event_selection == "three_months_ago"){
                     three_months_ago
                   }
                 ),
                 unit = "month"
               )) %>% 
        
        
        # we'll do the interval based on whether week or month is selected
        mutate(Period = interval(Event_Timing, IP_Timing) %/%  months(1)) %>% 
        # if_else(input$event_study_time_grouping == "month", months(1), weeks(1))) %>% 
        
        #filter down on the specified range:
        filter(Period >= input$event_study_period_range[1] & Period <= input$event_study_period_range[2]) %>% 
        
        group_by(Period) %>% 
        summarise(IP_Visits = n_distinct(INTEGER_CLAIM_NUMBER)) %>% 
        plot_ly(x = ~Period, y = ~IP_Visits, 
                type = "bar", 
                text = ~paste("Period:", Period, "<br>Total Visits:", scales::label_comma()(IP_Visits)), 
                hoverinfo = "text", textposition = 'auto',
                marker = list(color = '#cf4951',
                              line = list(color = '#b13e46', width = 1.5)))
    } #end if
    
    
    # else if (input$event_study_options == "intervention" & input$admission_type_1 == "ip"){
    #   
    #   selected_data_utilization_patterns() %>% 
    #     filter(!is.na(SVC_SERVICE_TO_DATE)) %>% # get any rid of empty IP visits (members without IP visits) before merge
    #     
    #     #filter out O-codes based on user selection:
    #     filter(
    #       if (input$event_study_remove_o_codes == "remove"){
    #         #REMOVE codes prefixed with O within the parentheses:
    #         !str_detect(MEDICAL_DIAG_DESC_CODE, "\\(O")
    #       } 
    #       else if(!is.null(input$event_study_remove_o_codes) && input$event_study_remove_o_codes == "specific"){
    #         #KEEP codes prefixed with desired user input:
    #         str_detect(MEDICAL_DIAG_DESC_CODE, paste0("\\(", input$code_prefix))
    #       }
    #       else {
    #         #no filtering, keep all rows
    #         TRUE
    #       }
    #     ) %>% 
    #     
    #     # Merge in pregnancy level and ONLY SELECT WHAT WE NEED to reduce weight:
    #     right_join(
    #       dataInput_Pregnancy_Level() %>% 
    #         select(MEMBERID, LEFT_TRUNC, RIGHT_TRUNC, OUTCOME_DATE, LMP), 
    #       by = c("MBR_ID" = "MEMBERID")
    #     ) %>% 
    #     
    #     #Filter the my Utilization Claims table to be within the +/-1 year from pregnancy/delivery that Joe created: 
    #     filter(SVC_SERVICE_TO_DATE >= LEFT_TRUNC & SVC_SERVICE_TO_DATE <= RIGHT_TRUNC) %>% 
    #     
    #     mutate(IP_Timing = floor_date(as.Date(SVC_SERVICE_TO_DATE), unit = input$event_study_time_grouping),
    #            Event_Timing = floor_date(as.Date(OUTCOME_DATE), unit = input$event_study_time_grouping)) %>% 
    #     
    #     # we'll do the interval based on whether week or month is selected
    #     mutate(Period = interval(Event_Timing, IP_Timing) %/% 
    #              if_else(input$event_study_time_grouping == "month", months(1), weeks(1))) %>% 
    #     
    #     #filter down on the specified range:
    #     filter(Period >= input$event_study_period_range[1] & Period <= input$event_study_period_range[2]) %>% 
    #     
    #     group_by(Intervention, Period) %>% 
    #     summarise(IP_Visits = n_distinct(INTEGER_CLAIM_NUMBER)) %>% 
    #     plot_ly(x = ~Period, y = ~IP_Visits, 
    #             type = "bar", 
    #             text = ~n, textposition = 'auto',
    #             marker = list(color = '#9ECAE1',
    #                           line = list(color = '#4F7A90', width = 1.5)))
    # } #end if
    
    
  }) # END utilization_patterns_event_study
  
  
  # ER/IP DIAG CODES -------------------------------------------------------
  
  # Reactive expression to select the dataset based on input:
  selected_data_admission_diags <- reactive({
    if (input$admission_type_2== "er"){
      dataInput_General_ER()  %>% 
        # Match with our population:
        inner_join(dataInput_Population(), by = c("MBR_ID" = "MEMBERID"))
    }
    else{
      dataInput_General_IP() %>% 
        # Match with our population:
        inner_join(dataInput_Population(), by = c("MBR_ID" = "MEMBERID"))
    }
  }) #END selected_data_admission_diags
  
  
  output$admission_diags_table <- renderReactable({
    
    # finding the total er/ip visits to get the frequency for the diag codes
    # using INTEGER_CLAIM_NUMBER even though we normally use ADMISSION_ID for IP visits b/c...
    # that INTEGER_CLAIM_NUMBER should be unique for both:
    total_visits <- selected_data_admission_diags() %>% summarise(total_visits = length(unique(INTEGER_CLAIM_NUMBER)))
    
    table <- selected_data_admission_diags() %>%
      filter(!is.na(!!sym(input$diagnosis_col_type))) %>% 
      count(!!sym(input$diagnosis_col_type), sort = TRUE) #%>% 
    #mutate(Percent = n/total_visits)
    
    
    reactable(
      table,
      striped = TRUE, 
      highlight = TRUE,
      bordered = TRUE,
      filterable = TRUE,
      defaultPageSize = 13,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5,10,15,20,25),
      defaultColDef = colDef(align = "center"),
      columns = list(
        n = colDef(name = "Total Count", format = colFormat(separators = TRUE),
                   filterable = FALSE,
                   style = color_scales(table, colors = c("#e6d6df", "#995a7d"), bias = 2))#,
        # Percent = colDef(name = "Percent", format = colFormat(percent = TRUE, digits = 1),
        #                  filterable = FALSE,
        #                  style = color_scales(table, colors = c("#e6d6df", "#995a7d"), bias = 2)) 
        
      ) #end list
    ) # end reactable
    
  }) #END admission_diags_table
  
  
  output$admission_diags_bar_chart <- renderPlotly({
    
    selected_data_admission_diags() %>%
      
      mutate(!!sym(input$diagnosis_col_type) := fct_lump(!!sym(input$diagnosis_col_type), n = input$top_n_utilization, other_level = "Other")) %>%
      count(!!sym(input$diagnosis_col_type), sort = TRUE) %>% 
      
      #remove Other:
      filter(!!sym(input$diagnosis_col_type) != "Other") %>% 
      
      #Plotting:
      plot_ly(x = ~n, y = ~reorder(get(input$diagnosis_col_type), n),
              type = "bar", orientation = "h",
              text = ~n, textposition = 'auto',
              marker = list(color = '#995a7d',
                            line = list(color = '#60384e', width = 1.5))) %>%
      layout(title = "", xaxis = list(title =  input$diagnosis_col_type), yaxis = list(title = ""))
    
  }) #END admission_diags_bar_chart
  
  
  
  
  
  
  # MENTAL HEALTH -------------------------------------------------------------
  
  ## Table ---------------------
  output$bh_conditions_general_table <- renderReactable({
    
    table <- dataInput_Severity() %>% 
      ungroup() %>%
      
      # Match with our population:
      inner_join(dataInput_Population(), by = c("MEMBERID" = "MEMBERID")) %>%  
      
      # drop unnecessary columns
      select(MASTER_ID,
             MBD001_SCHIZOPHRENIA_SPECTRUM_PSYCHOTIC_DISORDERS:MBD034_MENTAL_SUBSTANCE_DISORDERS_SEQUELA) %>%
      mutate(across(MBD001_SCHIZOPHRENIA_SPECTRUM_PSYCHOTIC_DISORDERS:MBD034_MENTAL_SUBSTANCE_DISORDERS_SEQUELA, ~if_else(is.na(.x),0,.x))) %>%
      group_by(MASTER_ID) %>%
      summarise_all(~max(.x, na.rm = T)) %>%
      ungroup() %>%
      select(-MASTER_ID)%>%
      summarise_all(~sum(.x, na.rm = T))%>%
      pivot_longer(cols = everything(),names_to = 'BH_Conditions',values_to = 'Members') %>%
      mutate(Percent = Members/length(unique(dataInput_Severity()[[sym("MEMBERID")]]))) %>% 
      mutate(BH_Conditions = str_remove(BH_Conditions, "^[^_]*_")) %>% #removes the BH Condition code before the first underscore (including the underscore)
      mutate(BH_Conditions = str_replace_all(BH_Conditions, "_", " ")) #gets rid of all underscores
    
    reactable(table,
              striped = TRUE, 
              highlight = TRUE,
              bordered = TRUE,
              #filterable = TRUE,
              sortable = TRUE,
              resizable = TRUE,
              defaultPageSize = 15,
              defaultSorted = list(Percent = "desc"),  
              defaultColDef = colDef(align = "center"),
              columns = list(
                BH_Conditions = colDef(name = "BH Conditions", minWidth = 300, filterable = TRUE),
                Percent = colDef(name = "Percent", minWidth = 50, format = colFormat(percent = TRUE, digits = 1),
                                 style = color_scales(table, colors = c("#9ee1c6", "#66A597"), bias = 2)),
                Members = colDef(name = "Members", minWidth = 50,  format = colFormat(separators = TRUE),
                                 style = color_scales(table, colors = c("lightblue", "#3B8EA5"), bias = 2))
              ))
  }) #END bh_conditions_general_table
  
  
  
  
  ## Event Study ------------------
  
  # Reactive expression to select the dataset based on input:
  selected_data_utilization_patterns_bh <- reactive({
    if (input$admission_type_3 == "er"){
      dataInput_BH_ER() 
    }
    else{
      dataInput_BH_IP() 
      
    }
  }) #END selected_data_utilization_patterns_bh
  
  ### ER -----------------
  output$utilization_patterns_event_study_bh <- renderPlotly({
    
    
    # if (input$admission_type_3 == "er" & input$event_study_options_bh == "general"){
    if (input$admission_type_3 == "er"){
      
      selected_data_utilization_patterns_bh() %>% 
        filter(!is.na(SVC_SERVICE_TO_DATE)) %>% # get any rid of empty ER visits (members without ER visits) before merge
        
        # Bring in start and stop dates:
        inner_join(dataInput_Static_Mbr_Level() %>% select(Mbr_ID, FIRSTDATE, LASTDATE), by = c("MBR_ID" = "Mbr_ID")) %>% 
        
        # Bring in Celeste Enrollment dates:
        inner_join(dataInput_Celeste_Programs(), by = c("MBR_ID" = "MEMBER_ID")) %>% 
        
        # Apply any vendor filters:
        filter(
          if (input$event_study_event_selection_bh == "celeste_enroll") {
            VENDOR %in% input$celeste_vendor_bh
          }
          else {
            TRUE
          }
        ) %>%  
        
        #Create timing based around user inputs on weeks/months, pregnancy start/end:
        mutate(ER_Timing = floor_date(as.Date(SVC_SERVICE_TO_DATE), unit = "month"),
               Event_Timing = floor_date(
                 as.Date(
                   if (input$event_study_event_selection_bh == "start"){
                     FIRSTDATE
                   } 
                   else if (input$event_study_event_selection_bh == "celeste_enroll"){
                     ENROLLMENT_START_DATE
                   } 
                   else if(input$event_study_event_selection_bh == "three_months_ago"){
                     three_months_ago
                   }
                 ),
                 unit = "month"
               )) %>%
        
        # we'll do the interval based on whether week or month is selected:
        mutate(Period = interval(Event_Timing, ER_Timing) %/% months(1)) %>% 
        #if_else(input$event_study_time_grouping_bh == "month", months(1), weeks(1))) %>% 
        
        #filter down on the specified range:
        filter(Period >= input$event_study_period_range_bh[1] & Period <= input$event_study_period_range_bh[2]) %>% 
        
        group_by(Period) %>% 
        summarise(ER_Visits = n_distinct(INTEGER_CLAIM_NUMBER)) %>% 
        plot_ly(x = ~Period, y = ~ER_Visits, 
                type = "bar", 
                text = ~paste("Period:", Period, "<br>Total Visits:", ER_Visits), 
                hoverinfo = "text",
                textposition = 'auto',
                marker = list(color = '#6fd3bc',
                              line = list(color = '#539e8d', width = 1.5)))
    } #end if
    
    ### IP -----------------
    # else if (input$admission_type_3 == "ip" & input$event_study_options_bh == "general"){
    else if (input$admission_type_3 == "ip"){
      
      selected_data_utilization_patterns_bh() %>% 
        filter(!is.na(SVC_SERVICE_TO_DATE)) %>% # get any rid of empty IP visits (members without IP visits) before merge
        
        # Bring in start and stop dates:
        inner_join(dataInput_Static_Mbr_Level() %>% select(Mbr_ID, FIRSTDATE, LASTDATE), by = c("MBR_ID" = "Mbr_ID")) %>% 
        
        # Bring in Celeste Enrollment dates:
        inner_join(dataInput_Celeste_Programs(), by = c("MBR_ID" = "MEMBER_ID")) %>% 
        
        # Apply any vendor filters:
        filter(
          if (input$event_study_event_selection_bh == "celeste_enroll") {
            VENDOR %in% input$celeste_vendor_bh
          }
          else {
            TRUE
          }
        ) %>%  
        
        #Create timing based around user inputs on weeks/months, pregnancy start/end:
        mutate(IP_Timing = floor_date(as.Date(SVC_SERVICE_TO_DATE), unit = "month"),
               Event_Timing = floor_date(
                 as.Date(
                   if (input$event_study_event_selection_bh == "start"){
                     FIRSTDATE
                   } 
                   else if (input$event_study_event_selection_bh == "celeste_enroll"){
                     ENROLLMENT_START_DATE
                   } 
                   else if(input$event_study_event_selection_bh == "three_months_ago"){
                     three_months_ago
                   }
                 ),
                 unit = "month"
               )) %>% 
        
        # we'll do the interval based on whether week or month is selected
        mutate(Period = interval(Event_Timing, IP_Timing) %/% months(1)) %>% 
        #if_else(input$event_study_time_grouping_bh == "month", months(1), weeks(1))) %>% 
        
        #filter down on the specified range:
        filter(Period >= input$event_study_period_range_bh[1] & Period <= input$event_study_period_range_bh[2]) %>% 
        
        group_by(Period) %>% 
        summarise(IP_Visits = n_distinct(ADMISSIONID)) %>% 
        plot_ly(x = ~Period, y = ~IP_Visits, 
                type = "bar", 
                text = ~paste("Period:", Period, "<br>Total Visits:", IP_Visits), 
                hoverinfo = "text", textposition = 'auto',
                marker = list(color = '#66A597',
                              line = list(color = '#40675e', width = 1.5)))
    } #end if
    
  }) #END utilization_patterns_event_study_bh
  
  
  
  
  
  
  # CHARACTERISTICS -----------------------------------------------------------
  
  ## HISTogram -------------------------------------------------
  output$histogram_output <- renderPlotly({
    dataInput_Static_Mbr_Level() %>%
      
      # Match with our population:
      inner_join(dataInput_Population(), by = c("Mbr_ID" = "MEMBERID")) %>% 
      
      select(MASTER_ID, Mbr_ID, Age, Nearest_ED_Dist_Tract:Nearest_Clinic_Dist_Tract) %>% 
      
      mutate(Grouper = !!input$histogram_input) %>% 
      filter(!is.na(Grouper)) %>%
      plot_ly(x = ~Grouper, type = 'histogram',
              text = "", textposition = 'auto',
              marker = list(color = '#9ECAE1',
                            line = list(color = '#4F7A90', width = 1.5))) %>% 
      layout(title = "", 
             xaxis = list(title = ""),
             # xaxis = list(title = paste(input$histogram_input)), 
             yaxis = list(title = ""),
             autosize = TRUE)
    
  })#end of histogram chart
  
  ## BAR chart --------------------------------------------------
  output$barchart1_output <- renderPlotly({
    dataInput_Static_Mbr_Level() %>%
      
      # Match with our population:
      inner_join(dataInput_Population(), by = c("Mbr_ID" = "MEMBERID")) %>% 
      
      select(MASTER_ID, Mbr_ID, Gender, Race, Race_Minority_Cat, 
             Age_Group, Age_Cat, Generation,
             Rural_County_Cat, Internet_Access_County_Cat) %>% 
      
      mutate(Grouper = !!input$barchart1_input) %>% 
      group_by(Grouper) %>%
      summarise(member_count = n_distinct(MASTER_ID)) %>% 
      filter(!is.na(Grouper)) %>%
      plot_ly(x = ~as.factor(Grouper), y = ~member_count, type = 'bar',
              text = ~scales::label_comma()(member_count), 
              hovertext = ~paste0(Grouper,": ", scales::label_comma()(round(member_count))), 
              hoverinfo = "hovertext+text", textposition = 'auto',
              marker = list(color = '#9ECAE1',
                            line = list(color = '#4F7A90', width = 1.5))) %>% 
      layout(title = "", 
             xaxis = list(title = ""),
             # xaxis = list(title = paste(input$barchart1_input)), 
             yaxis = list(title = ""))
    
  })#end of bar chart
  
  ## SVI chart -------------------------------------------------
  output$svi_output <- renderPlotly({
    dataInput_Static_Mbr_Level() %>%
      
      # Match with our population:
      inner_join(dataInput_Population(), by = c("Mbr_ID" = "MEMBERID")) %>% 
      
      select(MASTER_ID, Mbr_ID, Socioeconomic_SVI_Cat, Household_SVI_Cat, Minority_SVI_Cat, HousingTransp_SVI_Cat, All_SVI_Cat) %>% 
      
      mutate(Grouper = !!input$svi_input) %>% 
      group_by(Grouper) %>%
      summarise(member_count = n_distinct(MASTER_ID)) %>% 
      filter(!is.na(Grouper)) %>%
      plot_ly(x = ~as.factor(Grouper), y = ~member_count, type = 'bar',
              text = ~scales::label_comma()(member_count),  
              hovertext = ~paste0(Grouper,": ", scales::label_comma()(round(member_count))), 
              hoverinfo = "hovertext+text", textposition = 'auto',
              marker = list(color = '#9ECAE1',
                            line = list(color = '#4F7A90', width = 1.5))) %>% 
      layout(title = "", 
             xaxis = list(title = ""),
             # xaxis = list(title = paste(input$svi_input)), 
             yaxis = list(title = ""))
    
  })#end of svi chart
  
  
  ## CC Bar Chart -----------------------------------------------------
  
  output$top_cc_output <- renderPlotly({
    
    #table_for_denom <- dataInput_Static_Mbr_Level()
    
    dataInput_Static_Mbr_Level() %>%
      
      # Match with our population:
      inner_join(dataInput_Population(), by = c("Mbr_ID" = "MEMBERID")) %>% 
      
      select(Mbr_ID,
             AIDS:VALVULAR_DISEASE) %>%
      
      mutate(across(AIDS:VALVULAR_DISEASE, ~if_else(is.na(.x),0,.x))) %>% 
      group_by(Mbr_ID) %>%
      summarise_all(~max(.x, na.rm = T)) %>%
      ungroup() %>%
      select(-Mbr_ID)%>%
      summarise_all(~sum(.x, na.rm = T))%>%
      pivot_longer(cols = everything(),names_to = 'CC',values_to = 'Members') %>%
      mutate(Percent = round(Members/length(unique(dataInput_Static_Mbr_Level()[[sym("Mbr_ID")]]))*100,1)) %>%
      slice_max(Percent, n=10) %>% 
      arrange(Percent, desc(Percent)) %>%
      mutate(CC = factor(CC, levels = CC)) %>% 
      plot_ly(x = ~Percent, y = ~CC, type = 'bar',
              text = ~paste0(Percent, "%"), textposition = 'auto',
              marker = list(color = '#9ECAE1',
                            line = list(color = '#4F7A90', width = 1.5))) %>%
      layout(title = "", 
             xaxis = list(title = ""),
             # xaxis = list(title = paste(input$cc_input)), 
             yaxis = list(title = ""))
  })
  
  
  
  # VENN DIAGRAM ------------------------------------------------------------
  
  # Conditions Venn Diagram:
  output$conditions_venn_diagram <- renderPlot({
    df_Venn <- dataInput_Static_Mbr_Level() %>%
      
      # Match with our population:
      inner_join(dataInput_Population(), by = c("Mbr_ID" = "MEMBERID")) %>% 
      
      #df_Venn <- df_Venn %>%
      mutate(Venn_Selection_1 = !!input$venn_1_input,
             Venn_Selection_2 = !!input$venn_2_input,
             Venn_Selection_3 = !!input$venn_3_input) %>%
      select(Mbr_ID, Venn_Selection_1, Venn_Selection_2, Venn_Selection_3)
    # create a list of sets for each disease state
    sets_list <- list(Venn_Selection_1 = which(df_Venn$Venn_Selection_1==1),
                      Venn_Selection_2 = which(df_Venn$Venn_Selection_2==1),
                      Venn_Selection_3 = which(df_Venn$Venn_Selection_3==1))
    # create the Venn Diagram
    venn.plot <- venn.diagram(
      x=sets_list,
      category.names= c(input$venn_1_input, input$venn_2_input, input$venn_3_input),
      cat.cex = 1.5,
      cex = 2,
      fill=c("#FF0000","#0198f9","#F1C40F"),
      filename= NULL
    )
    #F4D03F
    #display
    grid.draw(venn.plot)
  })
  
  # Condition CATEGORY Venn Diagram:
  output$conditions_venn_diagram_cc_category <- renderPlot({
    df_Venn <- dataInput_Static_Mbr_Level() %>%
      
      # Match with our population:
      inner_join(dataInput_Population(), by = c("Mbr_ID" = "MEMBERID")) %>% 
      
      #df_Venn <- df_Venn %>%
      mutate(Venn_Selection_4 = !!input$venn_4_input,
             Venn_Selection_5 = !!input$venn_5_input,
             Venn_Selection_6 = !!input$venn_6_input) %>%
      select(Mbr_ID, Venn_Selection_4, Venn_Selection_5, Venn_Selection_6)
    # create a list of sets for each disease state
    sets_list <- list(Venn_Selection_4 = which(df_Venn$Venn_Selection_4==1),
                      Venn_Selection_5 = which(df_Venn$Venn_Selection_5==1),
                      Venn_Selection_6 = which(df_Venn$Venn_Selection_6==1))
    # create the Venn Diagram
    venn.plot <- venn.diagram(
      x=sets_list,
      category.names= c(input$venn_4_input, input$venn_5_input, input$venn_6_input),
      cat.cex = 1.5,
      cex = 2,
      fill=c("#FF0000","#0198f9","#F1C40F"),
      filename= NULL
    )
    #F4D03F
    #display
    grid.draw(venn.plot)
  })
  
  # PS-Vendor overlapping Venn Diagram:
  output$vendors_venn_diagram <- renderPlot({
    df_Venn <- df_wide %>% 
      inner_join(dataInput_Population(), by = c("MEMBER_ID" = "MEMBERID")) %>% 
      mutate(Venn_Selection_7 = !!input$venn_7_input,
             Venn_Selection_8 = !!input$venn_8_input,
             Venn_Selection_9 = !!input$venn_9_input) %>% 
      select(MEMBER_ID,Venn_Selection_7,Venn_Selection_8,Venn_Selection_9)
    # create a list of sets for each selected vendor
    sets_list <- list(Venn_Selection_7 = which(df_Venn$Venn_Selection_7==1),
                      Venn_Selection_8 = which(df_Venn$Venn_Selection_8==1),
                      Venn_Selection_9 = which(df_Venn$Venn_Selection_9==1))
    # create the Venn Diagram
    venn.plot <- venn.diagram(
      x=sets_list,
      category.names= c(input$venn_7_input, input$venn_8_input, input$venn_9_input),
      cat.cex = 1.5,
      cex = 2,
      fill=c("#FF0000","#0198f9","#F1C40F"),
      filename= NULL
    )
    #F4D03F
    #display
    grid.draw(venn.plot)
  })
  
  # PS-Program overlapping Venn Diagram:
  output$programs_venn_diagram <- renderPlot({
    df_Venn <- df_wide_2 %>% 
      inner_join(dataInput_Population(), by = c("MEMBER_ID" = "MEMBERID")) %>% 
      mutate(Venn_Selection_10 = !!input$venn_10_input,
             Venn_Selection_11 = !!input$venn_11_input,
             Venn_Selection_12 = !!input$venn_12_input) %>% 
      select(MEMBER_ID,Venn_Selection_10,Venn_Selection_11,Venn_Selection_12)
    # create a list of sets for each selected program
    sets_list <- list(Venn_Selection_10 = which(df_Venn$Venn_Selection_10==1),
                      Venn_Selection_11 = which(df_Venn$Venn_Selection_11==1),
                      Venn_Selection_12 = which(df_Venn$Venn_Selection_12==1))
    # create the Venn Diagram
    venn.plot <- venn.diagram(
      x=sets_list,
      category.names= c(input$venn_10_input, input$venn_11_input, input$venn_12_input),
      cat.cex = 1.5,
      cex = 2,
      fill=c("#FF0000","#0198f9","#F1C40F"),
      filename= NULL
    )
    #F4D03F
    #display
    grid.draw(venn.plot)
    
      
  })
  
  
  # MAP 2.0---------------------------------------------------------------------
  
  observeEvent(input$map_single_var_type, {
    if (input$map_single_var_type == "Demographic") {
      updateVarSelectInput(session, "map_single_selection", "Demographic Options:",  dataInput_Static_Mbr_Level() %>% select(any_of(selection_7)), selected = "Female")
    } else if (input$map_single_var_type == "Conditions") {
      updateVarSelectInput(session, "map_single_selection", "Condition Options:", dataInput_Static_Mbr_Level() %>% select(any_of(selection_6)), selected = "HYPERTENSION")
    } else if (input$map_single_var_type == "Utilization") {
      updateVarSelectInput(session, "map_single_selection", "Utilization Options:", dataInput_Static_Mbr_Level() %>% select(any_of(selection_9)), selected = "ER")
    } else if (input$map_single_var_type == "Environmental") {
      updateVarSelectInput(session, "map_single_selection", "Environmental Options:", dataInput_Static_Mbr_Level() %>% select(any_of(selection_8)), selected = "Internet_Access_County")
    }
  })
  
  
  observeEvent(input$state_selection, {
    selected_state <- US_Counties %>%
      filter(STUSPS == input$state_selection) %>%
      st_union()
    
    # VIEW READJUST OPTION 1:
    # bbox <- st_bbox(selected_state)
    # state_centroid <- st_centroid(st_geometry(selected_state))
    # 
    # Update the map view dynamically
    # leafletProxy("map") %>%
    #   fitBounds(
    #     lng1 = bbox$xmin,
    #     lat1 = bbox$ymin,
    #     lng2 = bbox$xmax,
    #     lat2 = bbox$ymax
    #   )
    
    
    # VIEW READJUST OPTION 2:
    state_centroid <- st_centroid(st_geometry(selected_state))
    lng_state <- st_coordinates(state_centroid)[1]
    lat_state <- st_coordinates(state_centroid)[2]
    
    # lng_county <- st_coordinates(state_centroid)[,1]
    # lat_county <- st_coordinates(state_centroid)[,2]
    
    # county_centroid <- selected_state %>% 
    #   mutate(
    #     lng_county <- st_coordinates(geometry)[,1],
    #     lat_county <- st_coordinates(geometry)[,2]
    #   ) 
    
    
    
    
    
    
    leafletProxy("map", session) %>%
      
      # This set the zoom onto a state when user changes:
      setView(lng = lng_state, lat = lat_state, zoom = 7) #%>% 
    
    # This sets the labels for the counties:
    # clearGroup("labels") %>%  # Clear existing labels
    # addLabelOnlyMarkers(
    #   data = state_centroid,
    #   lng = ~lng_county,
    #   lat = ~lat_county,
    #   label = ~county_labels(),  # Use reactive labels
    #   labelOptions = labelOptions(
    #     noHide = TRUE,
    #     offset = c(10,10),
    #     direction = "center",
    #     style = list(
    #       "color" = "black",
    #       "font-size" = "12px",
    #       "font-weight" = "bold",
    #       "background-color" = "white",
    #       "padding" = "2px"
    #     )
    #   ),
    #   group = "labels"  # Add to a group for easier management
    # )
    
    
  })
  
  ## Bins Reactive ------------------
  
  # Reactive value to store bin breaks
  # bin_breaks <- reactiveVal(c(1, 50, 100, 500, 1000, 5000, Inf))
  
  # Observe button click to update bin breaks
  # observeEvent(input$update_map, {
  #   # Check if input is empty
  #   if (trimws(input$custom_breaks) == "") {
  #     showNotification("No changes made. Using current breaks.", type = "message")
  #   } else {
  #     # Parse custom breaks
  #     new_breaks <- as.numeric(strsplit(input$custom_breaks, ",")[[1]])
  #     
  #     # Validate input: Ensure breaks are sorted and valid
  #     if (any(is.na(new_breaks)) || !is.numeric(new_breaks)) {
  #       showNotification("Invalid bin breaks. Please enter numeric values separated by commas.", type = "error")
  #     } else {
  #       bin_breaks(sort(new_breaks))
  #       showNotification("Bin breaks updated successfully!", type = "message")
  #     }
  #   }
  # })
  
  
  
  output$map <- renderLeaflet({
    
    ## Member Count -----------------------------
    if(input$map_value_options == "member_count"){
      
      table <- dataInput_Population() %>%
        
        # grab the GEOID needed for counties from the Static table: 
        inner_join(dataInput_Static_Mbr_Level() %>% 
                     select(Mbr_ID, GEOID),
                   by = c("MEMBERID" = "Mbr_ID")) %>% 
        
        # get the member count in each county: 
        group_by(GEOID) %>%
        summarise(total_members = as.numeric(n_distinct(MASTER_ID))) %>% 
        
        # right_join if  I want to show all AR counties;
        # inner_join if I wanted to show only counties where people are (even after filtering) 
        # left_join if I want to keep track of the members unable to be mapped and show a count
        right_join(US_Counties %>% filter(STUSPS == input$state_selection), by = "GEOID") %>% 
        
        # we need to make a precomputed label to get around the HTML issues leaflet is having:
        # mutate(label = HTML(paste("County:", NAME, "<br>Total Members:", total_members))) %>% 
        
        # ungroup() %>% 
        
        # transform:
        st_as_sf() #%>% 
      
      #st_union()  #combine geometries if needed
      
      # Get the bounding box of the selected state:
      # bbox <- st_bbox(table)
      # 
      # # Update the map view dynamically:
      # leafletProxy("map")
      # fitBounds(
      #   lng1 = bbox$xmin, 
      #   lat1 = bbox$ymin,
      #   lng2 = bbox$xmax, 
      #   lat2 = bbox$ymax
      # )
      # 
      
      # set bins and color:  
      bins <- c(1, 50, 100, 500, 1000, 5000, Inf)
      palette <- colorBin(palette = "YlOrRd", domain = table$total_members, bins = bins)
      
      # Reactive color palette based on bin breaks
      # palette <- colorBin("YlOrRd", domain = table$total_members, bins = bin_breaks())
      
      county_labels <- reactive({
        if (input$label_option == "name") {
          paste0(table$NAME)
        } 
        else if (input$label_option == "value"){
          paste0(table$total_members)
        }
        else{
          ""
        }
      })
      
      leaflet(table) %>%
        addTiles() %>%
        addPolygons(
          group = "counties",
          fillColor = ~palette(total_members),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          popup = ~paste0(
            "<b>County:</b> ", NAME, "<br>",
            "<b>Total Members:</b> ", scales::label_comma()(total_members), "<br>"
          )
          # labelOptions = labelOptions(
          #   style = list("font-weight" = "normal", padding = "3px 8px"),
          #   textsize = "15px",
          #   direction = "auto",
          #   html = TRUE #enable HTML rendering
          # )
        ) %>%
        addLegend(pal = palette, values = ~total_members, opacity = 0.7,
                  title = "Total Members",
                  position = "topright") 
    } #end member count if{}
    
    
    ## Single Variable Numeric -----------------------------
    else if (input$map_value_options == "single_var") {
      req(input$map_single_aggregation, input$map_single_var_type, input$map_single_selection)
      
      joined_data <- dataInput_Population() %>%
        inner_join(
          dataInput_Static_Mbr_Level() %>%
            select(Mbr_ID, GEOID, !!sym(input$map_single_selection)),
          by = c("MEMBERID" = "Mbr_ID")
        )
      
      # Ensure GEOID matches
      joined_data <- joined_data %>% mutate(GEOID = as.character(GEOID))
      
      # Filter missing GEOIDs
      missing_geoids <- setdiff(US_Counties$GEOID, joined_data$GEOID)
      if (length(missing_geoids) > 0) {
        print("Missing GEOIDs:")
        print(missing_geoids)
      }
      
      # Filter invalid data
      # joined_data <- joined_data %>% filter(!is.na(GEOID) & !is.na(!!sym(input$map_single_selection)))
      
      # Aggregate data
      table <- joined_data %>%
        group_by(GEOID) %>%
        summarise(
          aggregated_value = tryCatch(
            case_when(
              input$map_single_aggregation == "Sum" ~ sum(!!sym(input$map_single_selection), na.rm = TRUE),
              input$map_single_aggregation == "Average" ~ mean(!!sym(input$map_single_selection), na.rm = TRUE),
              input$map_single_aggregation == "Median" ~ median(!!sym(input$map_single_selection), na.rm = TRUE),
              input$map_single_aggregation == "Rate_per_100" ~ round((sum(!!sym(input$map_single_selection), na.rm = TRUE) / n_distinct(MASTER_ID)) * 100, 1)
            ),
            error = function(e) {
              print(paste("Error during aggregation for GEOID:", GEOID))
              print(e$message)
              NA
            }
          )
        ) %>%
        right_join(US_Counties %>% filter(STUSPS == input$state_selection), by = "GEOID") %>%
        filter(!is.na(aggregated_value)) %>%
        st_as_sf()
      
      
      # Debug: Check final table
      print("Final table:")
      print(head(table))
      
      # Check aggregated_value
      if (!"aggregated_value" %in% colnames(table) || any(is.na(table$aggregated_value))) {
        stop("aggregated_value column is missing or contains invalid data.")
      }
      
      # Compute breaks:
      reactive_breaks <- reactive({
        req(table$aggregated_value)
        
        if (input$map_single_aggregation == "Rate_per_100") {
          # breaks <- pretty(c(0, max(table$aggregated_value / 100, na.rm = TRUE)), n = 5)
          breaks <- pretty(range(table$aggregated_value, na.rm = TRUE), n = 5)
          units <- "Rate per 100"
        } else {
          breaks <- pretty(range(table$aggregated_value, na.rm = TRUE), n = 5)
          units <- input$map_single_aggregation
        }
        list(breaks = breaks, units = units)
      })
      
      
      palette <- reactive({
        req(table$aggregated_value, reactive_breaks())  # Ensure `aggregated_value` exists
        colorBin("YlOrRd", domain = table$aggregated_value, bins = reactive_breaks()$breaks)
      })
      
      
      # Render leaflet map
      leaflet(table) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~palette()(aggregated_value),
          weight = 1,
          opacity = 1,
          color = "white",
          fillOpacity = 0.7,
          popup = ~paste0("<b>County:</b> ", NAME, "<br>", "<b>", input$map_single_selection, ":</b> ", aggregated_value)
        ) %>%
        addLegend(
          pal = palette(),
          values = table$aggregated_value,
          title = paste0(input$map_single_selection, " (", reactive_breaks()$units, ")"),
          position = "topright",
          opacity = 0.7
        )
    }
    
  }) #END MAP
  
  # TESTING -----------------------------------------------------------------
  
  # Render the table  based on the selected dataset:
  # output$test <- renderTable({
  # 
  #   table <- dataInput_Population() %>%
  #     
  #     # grab the GEOID needed for counties from the Static table: 
  #     inner_join(dataInput_Static_Mbr_Level() %>%
  #                  # select(Mbr_ID, GEOID, !!sym(input$map_single_selection)),
  #                  select(Mbr_ID, GEOID, Female),
  #                by = c("MEMBERID" = "Mbr_ID")) %>% 
  #     
  #     # get the member count in each county: 
  #     group_by(GEOID) %>%
  #     
  #     #check aggregation:
  #     summarise(aggregated_value = case_when(
  #       input$map_single_aggregation == "sum" ~ sum(Female, na.rm = TRUE),
  #       input$map_single_aggregation == "average" ~ mean(Female, na.rm = TRUE),
  #       input$map_single_aggregation == "median" ~ median(Female, na.rm = TRUE),
  #       input$map_single_aggregation == "rate_per_100" ~ round((mean(Female, na.rm = TRUE)/n_distinct(MASTER_ID)) * 100,1) #rate per 100 patients
  #       # input$map_single_aggregation == "sum" ~ sum(!!sym(input$map_single_selection), na.rm = TRUE),
  #       # input$map_single_aggregation == "average" ~ mean(!!sym(input$map_single_selection), na.rm = TRUE),
  #       # input$map_single_aggregation == "median" ~ median(!!sym(input$map_single_selection), na.rm = TRUE),
  #       # input$map_single_aggregation == "rate_per_100" ~ round((mean(!!sym(input$map_single_selection), na.rm = TRUE)/n_distinct(MASTER_ID)) * 100,1) #rate per 100 patients
  #       # input$map_single_aggregation == "percent" ~ round((mean(!!sym(input$map_single_selection), na.rm = TRUE)/n_distinct(MASTER_ID)) * 100,1)
  #     )) %>% 
  #     
  #     
  #     # right_join if  I want to show all AR counties;
  #     # inner_join if I wanted to show only counties where people are (even after filtering) 
  #     # left_join if I want to keep track of the members unable to be mapped and show a count
  #     right_join(US_Counties %>% filter(STATEFP == "05"), by = "GEOID") %>% 
  #     
  #     # we need to make a precomputed label to get around the HTML issues leaflet is having:
  #     # mutate(label = HTML(paste("County:", NAME, "<br>Total Members:", aggregation))) %>% 
  #     
  #     ungroup() %>% 
  #     
  #     # transform:
  #     st_as_sf() #%>% 
  #   
  #   # st_transform(crs = 4326)
  #   
  #   # set bins and color:  
  #   # bins <- c(1, 10, 25, 100, 500, Inf)
  #   # palette <- colorBin(palette = "YlOrRd", domain = table$aggregation, bins = bins)
  #   
  #   
  #   # Reactive calculation of sensible breaks and legend units
  #   reactive_breaks <- reactiveVal({
  #     req(input$map_single_selection, input$map_single_aggregation)
  #     # selected_var <- table[[input$variable]]
  #     
  #     # Calculate sensible breaks based on aggregation
  #     if (input$map_single_aggregation == "sum") {
  #       breaks <- pretty(range(table$aggregated_value, na.rm = TRUE), n = 5)
  #       units <- "Total"
  #     } else if (input$map_single_aggregation == "average") {
  #       breaks <- pretty(range(table$aggregated_value, na.rm = TRUE), n = 5)
  #       units <- "Average"
  #     } else if (input$map_single_aggregation == "median") {
  #       breaks <- pretty(range(table$aggregated_value, na.rm = TRUE), n = 5)
  #       units <- "Median"
  #     } else if (input$map_single_aggregation == "rate_per_100") {
  #       breaks <- pretty(c(0, max(table$aggregated_value / 100, na.rm = TRUE)), n = 5)
  #       units <- "Rate per 100"
  #     } else if (input$map_single_aggregation == "percent") {
  #       breaks <- seq(0, 100, by = 20)
  #       units <- "%"
  #     }
  #     
  #     list(breaks = breaks, units = units)
  #   })
  #   
  #   
  #   # Custom breaks handling
  #   # bin_breaks <- reactiveVal()
  #   # observeEvent(input$update_map, {
  #   #   if (trimws(input$custom_breaks) == "") {
  #   #     bin_breaks(reactive_breaks()$breaks)
  #   #     showNotification("No custom breaks provided. Using sensible breaks.", type = "message")
  #   #   } else {
  #   #     custom_breaks <- as.numeric(strsplit(input$custom_breaks, ",")[[1]])
  #   #     if (any(is.na(custom_breaks))) {
  #   #       showNotification("Invalid custom breaks. Retaining sensible breaks.", type = "error")
  #   #     } else {
  #   #       bin_breaks(sort(custom_breaks))
  #   #       showNotification("Custom breaks applied successfully!", type = "message")
  #   #     }
  #   #   }
  #   # })
  #   
  #   palette <- reactive({
  #     req(input$map_single_aggregation)  # Ensure input$variable is not NULL
  #     
  #     # Check if the column exists
  #     if (!table$Female %in% colnames(table)) {
  #       stop("Selected variable does not exist in the dataset.")
  #     }
  #     
  #     variable_data <- table[[table$Female]]
  #     
  #     # Ensure variable_data is not NULL or empty
  #     if (is.null(variable_data) || length(variable_data) == 0) {
  #       stop("No data available for the selected variable.")
  #     }
  #     
  #     colorBin("YlOrRd", domain = table$aggregated_value, bins = reactive_breaks()$breaks)
  #   })
  
  
  
  # })
  
  
  
  
} #end server