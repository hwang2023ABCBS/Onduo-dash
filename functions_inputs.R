# FUNCTIONS & INPUTS ############################################################


# Main Import ------------------------------------------------------------------

board <- pins::board_connect()


Population <- pins::pin_read(board, "hwang/ONDUO_Dash_Population") 


Celeste_Programs <- pins::pin_read(board, "hwang/ONDUO_Dash_Celeste_Programs")
ENROLLED_MBRS_MASTER_VENDORS_mostUpdated <- pin_read(board, "hwang/ENROLLED_MBRS_MASTER_VENDORS_mostUpdated") # add this one
# wide table of member counts by vendors & programs 
df_wide <- readRDS(here::here("data/df_wide.rds"))
df_wide_2 <- readRDS(here::here("data/df_wide_2.rds"))


Maternity <- pins::pin_read(board, "hwang/ONDUO_Dash_Maternity") 
CM_Monthly <- pins::pin_read(board, "hwang/ONDUO_Dash_CM_Monthly")

General_ER_Claims <- pins::pin_read(board, "hwang/ONDUO_Dash_General_ER_Claims")
General_IP_Claims <- pins::pin_read(board, "hwang/ONDUO_Dash_General_IP_Claims")
BH_ER_Claims <- pins::pin_read(board, "hwang/ONDUO_Dash_BH_ER_Claims")
BH_IP_Claims <- pins::pin_read(board, "hwang/ONDUO_Dash_BH_IP_Claims")

Severity <- pins::pin_read(board, "hwang/ONDUO_Dash_Severity")

Mbr_Monthly <- pins::pin_read(board, "hwang/ONDUO_Dash_Mbr_Monthly")
Static_Mbr_Level <- pins::pin_read(board, "hwang/ONDUO_Dash_Static_Mbr_Level") 

# event study data passed by Dilan
Onduo_look <- pin_read(board, "dsalpergin/Onduo_look") # pre/post utilizaitons and costs
Virta_look <- pin_read(board, "dsalpergin/Virta_look")


# GLP-1 usage
df_Pharma_VirtaOnduo <- pin_read(board, "dsalpergin/df_Pharma_VirtaOnduo")

# Med adherence 
ONDUO_MedAdherence_Rate_ByMbrDrugClass <- pin_read(board, "hwang/ONDUO_MedAdherence_Rate_ByMbrDrugClass")


US_Counties <- pins::pin_read(board,"dmpettis/US_Counties")
US_States <- pins::pin_read(board,"dmpettis/US_States")


start_date <- "2021-01-01"
three_months_ago <- Sys.Date() %m-% months(3)

# load(file="Joe_Data.RData")
# load(file="Jonathan_Logit_Models.RData")

# Colors -----------------------------------------------

colors1 <- c("#9ECAE1","#C7C7A6", "#7CD1BE", "#66A597", "#EF8354",
             "#e1c29e", '#9ee1c6', 
             "#E0DDCF", "#F1F0EA", "#715B64", "#2D728F", "#3B8EA5")

colors2 <- c("#4F7A90", "#00befa", "#4aa3bf", "#204652")


# MAIN INPUTS ##################################################################

# Selection Options -------------------------------------------------------

# numeric options:
# Used in the histogram on the Member Characteristics page
selection_1 <- Static_Mbr_Level %>% 
  select(Age,Nearest_ED_Dist_Tract:Nearest_Clinic_Dist_Tract
         #Rural_County_Pct,Internet_Access_County:Smokers_County_Pct
  ) %>% 
  colnames()

# categorical options:
# Used in the bar chart on the Member Characteristics page
selection_2 <- Static_Mbr_Level %>% 
  select(Gender, Race, Race_Minority_Cat, 
         Age_Group, Age_Cat, Generation,
         Rural_County_Cat, Internet_Access_County_Cat
         #Maternity_Desert, 
         #EM_Rank, ERA_Rank
  ) %>% 
  colnames()

# svi options:
# Used in the bar chart for SVI on the Member Characteristics page
selection_3 <- Static_Mbr_Level %>% 
  select(Socioeconomic_SVI_Cat, Household_SVI_Cat, Minority_SVI_Cat, HousingTransp_SVI_Cat, All_SVI_Cat) %>% 
  colnames()

# condition options:
# Used in the Venn Diagram section
selection_4 <- Static_Mbr_Level %>%
  select(AIDS:WEIGHT_LOSS) %>%
  colnames()

# condition category options:
# Used in the Venn Diagram section
selection_5 <- Static_Mbr_Level %>%
  select(BLOOD_IMMUNE:SYMPTOMS_SIGNS_ABNORMALITES) %>%
  colnames()

# condition options (limited version):
# Used for the Condition filters section in Main Sidebar
selection_6 <- Static_Mbr_Level %>%
  select(HYPERTENSION, HYPERTENSION_CX, HYPERTENSION_UNCX, 
         HEART_FAILURE,
         OBESITY, 
         DIABETES, DIABETES_CX, DIABETES_UNCX, 
         RENAL, RENAL_MOD, RENAL_SEVERE,
         CANCER, 
         BH, ANXIETY_STRESS_PTSD, DEPRESSION,
         SUD, ALCOHOL_ABUSE, DRUG_ABUSE, NICOTINE,
         STI) %>%
  colnames()

# Map options: 
# Used for the Demographics options:
selection_7 <- Static_Mbr_Level %>%
  select(Family_Size, Age, Female,
         Race_Minority, White, Black, Hispanic, Asian, Native_American, Other,
         Self_Reported_Race) %>% 
  colnames()

# Map options:
# Used for the Environmental options:
selection_8 <- Static_Mbr_Level %>% 
  select(Socioeconomic_SVI_Tract, Household_SVI_Tract, Minority_SVI_Tract, HousingTransp_SVI_Tract, All_SVI_Tract,
         Nearest_Clinic_Dist_Tract, Nearest_Maternal_Care_Dist_Tract, Nearest_ED_Dist_Tract,
         Smokers_County_Pct, Internet_Access_County, PCP_per_100K_FCC, BH_Prov_per_100K_FCC) %>% 
  colnames()

# Map options:
# Used for the Utilization options
selection_9 <- Static_Mbr_Level %>% 
  select(ER, IP, OP, URGENT, OFFICE, TH) %>%
  colnames()

# vendor options: 
# used for venn diagram for vendor overlapping checking options
selection_10 <- df_wide %>% select(-MEMBER_ID) %>% colnames()

# program options:
# used for venn diagram for program/product overlapping checking options
selection_11 <- df_wide_2   %>% select(-MEMBER_ID) %>% colnames()


# Input Creation ----------------------------------------------------------

## General Details ----------------------------------------------------------- 
company_name_input <- pickerInput("company_name_input", "Company Name:", 
                                  choices = sort(unique(Static_Mbr_Level$Company_Name), na.last = T),
                                  selected = unique(Static_Mbr_Level$Company_Name), multiple= TRUE, 
                                  pickerOptions(actionsBox = TRUE, liveSearch = TRUE, liveSearchPlaceholder = "Search"))


termed_input <- pickerInput("termed_input", "Exited Plan:", 
                            choices = sort(unique(Static_Mbr_Level$TERMED), na.last = T),
                            selected = unique(Static_Mbr_Level$TERMED), multiple= TRUE, 
                            pickerOptions(actionsBox = TRUE, liveSearch = TRUE, liveSearchPlaceholder = "Search"))

self_funded_input <- pickerInput("self_funded_input", "Self-Funded Group:", 
                                 choices = sort(unique(Static_Mbr_Level$SELF_FUND_CD), na.last = T),
                                 selected = unique(Static_Mbr_Level$SELF_FUND_CD), multiple= TRUE, 
                                 pickerOptions(actionsBox = TRUE, liveSearch = TRUE, liveSearchPlaceholder = "Search"))

## Celeste Program Filters -----------------------------------------------------------------
celeste_programs_input <- pickerInput("celeste_programs_input", "Celeste Programs:", 
                             choices = sort(unique(Celeste_Programs$VENDOR), na.last = T), 
                             selected = unique(Celeste_Programs$VENDOR),
                             multiple= TRUE, pickerOptions(actionsBox = TRUE))

## CM Filters -----------------------------------------------------------------
cm_programs_input <- pickerInput("cm_programs_input", "CM Programs:",
                                 choices = sort(unique(CM_Monthly$PROGRAM), na.last = T), 
                                 selected = unique(CM_Monthly$PROGRAM),
                                 multiple= TRUE, pickerOptions(actionsBox = TRUE))


## Maternity Filters -----------------------------------------------------------

outcome_input <- pickerInput("outcome_input", "Pregnancy Outcome:", 
                                   choices = sort(unique(Maternity$OUTCOME), na.last = T), 
                                   selected = unique(Maternity$OUTCOME),
                                   multiple= TRUE, pickerOptions(actionsBox = TRUE))

delivery_type_input <- pickerInput("delivery_type_input", "Delivery Method:", 
                                   choices = sort(unique(Maternity$DELIVERY_TYPE), na.last = T), 
                                   selected = unique(Maternity$DELIVERY_TYPE),
                                   multiple= TRUE, pickerOptions(actionsBox = TRUE))

delivery_comp_input <- pickerInput("delivery_comp_input", "Delivery w/ Complications:", 
                                   choices = sort(unique(Maternity$DELIVERY_COMPLICATIONS), na.last = T), 
                                   selected = unique(Maternity$DELIVERY_COMPLICATIONS),
                                   multiple= TRUE, pickerOptions(actionsBox = TRUE))

delivery_preterm_input <- pickerInput("delivery_preterm_input", "Pre-Term Birth:", 
                                      choices = sort(unique(Maternity$PRETERM_WEEKS), na.last = T), 
                                      selected = unique(Maternity$PRETERM_WEEKS),
                                      multiple= TRUE, pickerOptions(actionsBox = TRUE))

num_pregnancy_input <- pickerInput("num_pregnancy_input", "Pregnancy Episode:", 
                                      choices = sort(unique(Maternity$GESTATION_EPISODE_NUMBER), na.last = T), 
                                      selected = unique(Maternity$GESTATION_EPISODE_NUMBER),
                                      multiple= TRUE, pickerOptions(actionsBox = TRUE))


last_gestation_input <- pickerInput("last_gestation_input", "Last Known Gestation Week:", 
                                    choices = sort(unique(Maternity$GEST_FINAL), na.last = T), 
                                    selected = unique(Maternity$GEST_FINAL),
                                    multiple= TRUE, pickerOptions(actionsBox = TRUE))

## Condition Filters -------------------------------------------------------

num_cc_input <- pickerInput("num_cc_input", "Num Conditions:", 
                            choices = sort(unique(Static_Mbr_Level$NUMCC), na.last = T),
                            selected = unique(Static_Mbr_Level$NUMCC),
                            multiple= TRUE, pickerOptions(actionsBox = TRUE))

# conditions_input <- pickerInput("conditions_input", "Select Condition:", 
#                                    choices = Static_Mbr_Level %>% select(any_of(selection_6)), 
#                                    selected = Static_Mbr_Level %>% select(any_of(selection_6)),
#                                    multiple= TRUE, pickerOptions(actionsBox = TRUE))

## Demographic Filters -------------------------------------------------------
# age_input <- sliderInput("age_input", "Age Range:", value = c(0,110), min = 0, max = 110)

age_cat_input <- pickerInput("age_cat_input", "Age Category:", 
                             choices = sort(unique(Static_Mbr_Level$Age_Cat), na.last = T),
                             selected = unique(Static_Mbr_Level$Age_Cat),
                             multiple= TRUE, pickerOptions(actionsBox = TRUE))

age_group_input <- pickerInput("age_group_input", "Age Group:", 
                               choices = sort(unique(Static_Mbr_Level$Age_Group), na.last = T),
                               selected = unique(Static_Mbr_Level$Age_Group),
                               multiple= TRUE, pickerOptions(actionsBox = TRUE))

generation_input <- pickerInput("generation_input", "Generation:", 
                                choices = sort(unique(Static_Mbr_Level$Generation), na.last = T),
                                selected = unique(Static_Mbr_Level$Generation),
                                multiple= TRUE, pickerOptions(actionsBox = TRUE))

gender_input <- pickerInput("gender_input", "Gender:", 
                            choices = sort(unique(Static_Mbr_Level$Gender), na.last = T),
                            selected = unique(Static_Mbr_Level$Gender),
                            multiple= TRUE, pickerOptions(actionsBox = TRUE))

race_input <- pickerInput("race_input", "Race Options:", 
                          choices = sort(unique(Static_Mbr_Level$Race), na.last = T), 
                          selected = unique(Static_Mbr_Level$Race),
                          multiple= TRUE, options = pickerOptions(actionsBox = TRUE))  
minority_input <- pickerInput("minority_input", "Minority Options:", 
                              choices = sort(unique(Static_Mbr_Level$Race_Minority_Cat), na.last = T), 
                              selected = unique(Static_Mbr_Level$Race_Minority_Cat),
                              multiple= TRUE,options = pickerOptions(actionsBox = TRUE)) 

## Location Filters ----------------------------------------------------------
county_name_input <- pickerInput("county_name_input", "County Name:", 
                                 choices = sort(unique(Static_Mbr_Level$Mbr_County_Name), na.last = T), 
                                 selected = unique(Static_Mbr_Level$Mbr_County_Name), 
                                 multiple= TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, liveSearchPlaceholder = "Search"))  
state_input <- pickerInput("state_input", "State:", 
                           choices = sort(unique(Static_Mbr_Level$Mbr_State), na.last = T), 
                           selected = unique(Static_Mbr_Level$Mbr_State), 
                           multiple= TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, liveSearchPlaceholder = "Search"))  
rural_cat_input <- pickerInput("rural_cat_input", "Area Classification:", 
                               choices = sort(unique(Static_Mbr_Level$Rural_County_Cat), na.last = T), 
                               selected = unique(Static_Mbr_Level$Rural_County_Cat), 
                               multiple= TRUE, options = pickerOptions(actionsBox = TRUE))



## Access/Barrier Filters ----------------------------------------------------

socioeconomic_svi_input <- pickerInput("socioeconomic_svi_input","Socioeconomic SVI Category:", 
                                       choices = sort(unique(Static_Mbr_Level$Socioeconomic_SVI_Cat),na.last = T), 
                                       selected = unique(Static_Mbr_Level$Socioeconomic_SVI_Cat),
                                       multiple= TRUE, pickerOptions(actionsBox = TRUE))
housingtransp_svi_input <- pickerInput("housingtransp_svi_input","Housing/Transp SVI Category:", 
                                       choices = sort(unique(Static_Mbr_Level$HousingTransp_SVI_Cat),na.last = T), 
                                       selected = unique(Static_Mbr_Level$HousingTransp_SVI_Cat),
                                       multiple= TRUE, pickerOptions(actionsBox = TRUE))

maternal_care_desert_input <- pickerInput("maternal_care_desert_input", "Maternal Care Category (County):", 
                                          choices = sort(unique(Static_Mbr_Level$Maternity_Desert_Cat), na.last = T), 
                                          selected = unique(Static_Mbr_Level$Maternity_Desert_Cat), 
                                          multiple= TRUE, options = pickerOptions(actionsBox = TRUE))
internet_access_input <- pickerInput("internet_access_input", "Internet Access (County):", 
                                          choices = sort(unique(Static_Mbr_Level$Internet_Access_County_Cat), na.last = T), 
                                          selected = unique(Static_Mbr_Level$Internet_Access_County_Cat), 
                                          multiple= TRUE, options = pickerOptions(actionsBox = TRUE))


## Venn Diagram --------------------------------------------------------------

venn_1_input <- varSelectInput("venn_1_input", "Select Condition:", Static_Mbr_Level %>% select(any_of(selection_4)), selected = "HYPERTENSION")
venn_2_input <- varSelectInput("venn_2_input", "Select Condition:", Static_Mbr_Level %>% select(any_of(selection_4)), selected = "OBESITY")
venn_3_input <- varSelectInput("venn_3_input", "Select Condition:", Static_Mbr_Level %>% select(any_of(selection_4)), selected = "SUD")

venn_4_input <- varSelectInput("venn_4_input", "Select CC Category:", Static_Mbr_Level %>% select(any_of(selection_5)), selected = "CIRCULATORY")
venn_5_input <- varSelectInput("venn_5_input", "Select CC Category:", Static_Mbr_Level %>% select(any_of(selection_5)), selected = "DIGESTIVE")
venn_6_input <- varSelectInput("venn_6_input", "Select CC Category:", Static_Mbr_Level %>% select(any_of(selection_5)), selected = "MENTAL_BEHAVORIAL_DISORDERS")

venn_7_input <- varSelectInput("venn_7_input","Select Vendor:", df_wide  %>% select(any_of(selection_10)), selected ="ONDUO")
venn_8_input <- varSelectInput("venn_8_input","Select Vendor:", df_wide  %>% select(any_of(selection_10)), selected ="VIRTA")
venn_9_input <- varSelectInput("venn_9_input","Select Vendor:", df_wide  %>% select(any_of(selection_10)), selected ="AFFINITE")

venn_10_input <- varSelectInput("venn_10_input","Select Program:", df_wide_2  %>% select(any_of(selection_11)), selected ="Type2Diabetes_DiabetesManagement")
venn_11_input <- varSelectInput("venn_11_input","Select Program:", df_wide_2  %>% select(any_of(selection_11)), selected ="Obesity_Reversal")
venn_12_input <- varSelectInput("venn_12_input","Select Program:", df_wide_2  %>% select(any_of(selection_11)), selected ="BehavioralHealth")



############### END MAIN INPUTS ###############################################x


