# Load Packages


pacman::p_load(
  ggplot2 ,    # plotting and graphing results
  rlang,      # read in excel files
  forcats,    # read in excel files
  broom,      # tidy code
  readxl,     # read in excel files
  pacman,     # loading and reading in packages
  rio,        # importing data
  here,       # relative file pathways
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,  # data management and visualization
  gtsummary,  # logistic regression and plotting results
  gt,         # for gtsummary
  flextable,  # table creation and manipulating
  car,        # data management and visualization
  readstata13,# read in Stata files
  finalfit,   # logistic regression and plotting results
  survminer,  # forest plots
  easystats,
  BiocManager,
  survival,
  forestplot,
  rticles,     # templates for scientific journal articles in RMarkdown
  jtools,
  corrplot,
  bstfun,
  ggforestplot,
  codebookr,
  codebook,
  sjlabelled,
  likert,
  kableExtra,
  haven,
  here,
  flexdashboard, # dashboard versions of R Markdown reports
  plotly, # interactive plots
  shiny,
  webshot,
  webshot2,
  grateful
)

load(
  here::here("analysis", "data", "processed_data", "ethos_working.rda")
)


subset <- wave_complete%>%
  dplyr::select(sex:prison6m)








    peerbus <- read.csv(
      here::here("analysis", "data", "raw_data", "peerbus_merge4.csv")
    )


    # data_peerbus <- read.csv("peerbus_merge4.csv", stringsAsFactors = TRUE)


    # Filter  all the valid entries

    # Not found on REDCAP

    datfilter <- peerbus %>%
      filter(recid != "Not found on RC")

    # No location data entered but site visit entered (looking at the lhd variable in the dataset)

    pow <- datfilter %>%
      filter(lhd!= "NA")

    # Recoding the variables. I have used the merged dataset from this point onwards

    # AGE
    # Calculating particpants age from date of birth and date survey completed

    data <- pow %>%
      mutate(

    # Participants age
        #
        # q_comd = ymd(q_comd),
        # q_pdob = ymd(q_pdob),
        # age = floor(decimal_date(q_comd) - decimal_date(q_pdob)),



    # Age groups
      # age_group = case_when(
      #   age < 36              ~ "18 - 35 years",
      #   age >= 36 & age < 46  ~ "36 - 45 years",
      #   age >= 46 & age < 56  ~ "46 - 55 years",
      #   age >= 56             ~ ">55 years")%>%
      #   ff_label("Age group")%>%
      #   fct_relevel("18 - 35 years",
      #               "36 - 45 years",
      #               "46 - 55 years",
      #               ">55 years"),

    # Gender
      gender.factor = as_factor(q_gen.factor)%>%
        fct_recode("Male" = "Male",
                   "Female" = "Female",
                   "Transgender" = "Transgender")%>%
        ff_label("Gender category")%>%
        fct_relevel("Female",
                    "Male",
                    "Transgender"),

    # Gender (condensed)
        gender.condensed = as_factor(q_gen.factor)%>%
          fct_recode("Male" = "Male",
                     "Female" = "Female") %>%
          ff_label("Gender")%>%
      fct_relevel("Female",
                  "Male"),

    # Ethnicity
        atsi = case_when(
        q_atsi == 0 ~ "Non-indigenous",
        q_atsi == 1 ~ "Aboriginal",
        q_atsi == 2 ~ "Torres Strait Islander",
        q_atsi == 3 ~ "Both Aboriginal and Torres Strait Islander")%>%
          ff_label("Ethnicity")%>%
          fct_relevel("Non-indigenous",
                     "Aboriginal",
                     "Torres Strait Islander",
                     "Both Aboriginal and Torres Strait Islander"),

    # Indigenous
        indigenous = case_when(
          q_atsi == 0 ~ "Non-indigenous",
          q_atsi == 1 ~ "Indigenous",
          q_atsi == 2 ~ "Indigenous",
          q_atsi == 3 ~ "Indigenous")%>%
        ff_label("Indigenous")%>%
        fct_relevel("Non-indigenous",
                   "Indigenous"),
    # Indigenous (yes/no)
      indigenous.yn = case_when(
        q_atsi == 0 ~ "No",
        q_atsi == 1 ~ "Yes",
        q_atsi == 2 ~ "Yes",
        q_atsi == 3 ~ "Yes")%>%
        ff_label("Indigenous")%>%
        fct_relevel("No",
                   "Yes"),

    # Homeless

    currenthomeless = case_when(
        q_hom_yn == 0              ~ "No",
        q_hom_yn == 1              ~ "Yes")%>%
        ff_label("Homesless")%>%
        fct_relevel("No",
                    "Yes"),

    # Incarcerated
    prison = case_when(
        q_pri == 1              ~ "No",
        q_pri == 2              ~ "Yes > 6 months",
        q_pri == 3              ~ "Yes < 6 months > 30 days",
        q_pri == 4              ~ "Yes < 6 months > currently")%>%
        ff_label("Incarceration history")%>%
        fct_relevel("No",
                    "Yes > 6 months",
                    "Yes < 6 months > 30 days",
                    "Yes < 6 months > currently"),

    # OAT use
    oat_status = case_when(
        q_oat == 1 ~ "No",
        q_oat == 2 ~ "Yes > 6 months",
        q_oat == 3 ~ "Yes < 6 months > 30 days",
        q_oat == 4 ~ "Yes < 30 days > currently",
        q_oat == 5 ~ "Currently")%>%
        ff_label("OAT status")%>%
         fct_relevel("No",
                     "Yes > 6 months",
                     "Yes < 6 months > 30 days",
                     "Yes < 30 days > currently",
                     "Currently"),


    # Currently receiving OAT
    # This variable is under the enrollment dataset - MJ to ask Gabriel for the data (7 June 2023)

    # data_pow <- data_pow %>%
    # mutate(oat_tx = case_when(
    # oat_tx == 1              ~ "Methadone",
    # oat_tx == 2              ~ "Buprenorphine (Subutex)",
    # oat_tx == 3              ~ "Buprenorphine (Unknown)",
    # oat_tx == 4              ~ "Buprenorphine-naloxone (Suboxone)",
    # oat_tx == 5              ~ "XR-buprenorphine",
    # oat_tx == 77              ~ "Other"))

    # Any injecting

    injany = case_when(
        q_injh == 1              ~ "Never injected",
        q_injh == 2              ~ "Yes > 6 months",
        q_injh == 3              ~ "Yes < 6 months > 30 days",
        q_injh == 4              ~ "Yes < 30 days")%>%
        ff_label("Drug injecting history")%>%
        fct_relevel("Never injected",
                    "Yes > 6 months",
                    "Yes < 6 months > 30 days",
                    "Yes < 30 days"),

    injfrequency = fct_relevel(q_inj_lm.factor,
                                        "2 to 3 times most days",                      "Less than weekly",
                                        "More than three times most days",             "More than weekly ,Not daily (uses between 1-6 days per week)",
                                        "Once a day"),

# Injecting frequency

injectingfreq = case_when(
    q_inj_lm == 1 ~ "More than 3 times a day",
    q_inj_lm == 5 ~ "Weekly or less",
    q_inj_lm == 4 ~ "More than weekly, not daily",
    q_inj_lm == 3 ~ "Once daily",
    q_inj_lm == 2 ~ "2 - 3 times a day")%>%
        ff_label("Drug injecting frequency")%>%
        fct_relevel("Once daily",
                   "2 - 3 times a day",
                   "More than 3 times a day",
                   "Weekly or less",
                   "More than weekly, not daily"),

#Injecting - drug specific
inj_drugtype = case_when(
    q_inj_mlm == 1      ~ "Heroin",
    q_inj_mlm == 2      ~ "Cocaine",
    q_inj_mlm == 3      ~ "Amphetamines",
    q_inj_mlm == 4      ~ "Other opioids",
    q_inj_mlm == 6      ~ "Benzodiazepines",
    q_inj_mlm == 77     ~ "Other")%>%
        ff_label("Drug type injected")%>%
        fct_relevel("Heroin",
                    "Other opioids",
                    "Amphetamines",
                    "Cocaine",
                    "Benzodiazepines",
                    "Other"),


# Ever HCV antibody test

everhcvab = case_when(
    q_hcab_test == 1  ~ "Yes < 12 months",
    q_hcab_test == 2  ~ "Yes > 12 months",
    q_hcab_test == 3  ~ "Never tested",
    q_hcab_test == 99 ~ "Don't know")%>%
        ff_label("Ever received an HCV antibody test")%>%
        fct_relevel("Never tested",
                    "Yes < 12 months",
                    "Yes > 12 months",
                    "Don't know"),

# HCV antobody result

hcv_ab_res1.factor =as_factor(hcv_ab_res1.factor)%>%
        fct_recode("Not tested" = "Negative",
                   "Positive" = "Positive")%>%
        ff_label("HCV antibody test result")%>%
        fct_relevel("Negative",
                    "Positive"),

# Ever HCV RNA test

everhcvrna = case_when(
    q_hcrna_test == 1  ~ "Yes < 12 months",
    q_hcrna_test == 2  ~ "Yes > 12 months",
    q_hcrna_test == 3  ~ "Never tested",
    q_hcrna_test == 99 ~ "Don't know")%>%
        ff_label("Ever received an HCV RNA test")%>%
        fct_relevel("Never tested",
                   "Yes < 12 months",
                   "Yes > 12 months",
                   "Don't know"),


# Been told that you have HCV infection
toldhcv_infection= as_factor(q_hc_inf.factor)%>%
        fct_recode("Yes"="Yes",
                   "No"="No")%>%
        ff_label("Told have HCV infection")%>%
        fct_relevel("No",
                    "Yes"),

# HCV current infection
currenthcv = case_when(
    q_hcs_cur == 1  ~ "Have HCV",
    q_hcs_cur == 2  ~ "Cleared without treatment (spontaneously)",
    q_hcs_cur == 3  ~ "Cleared with treatment",
    q_hcs_cur == 99 ~ "Don't know")%>%
        ff_label("Current HCV infection")%>%
        fct_relevel("Have HCV",
                    "Cleared without treatment (spontaneously)",
                    "Cleared with treatment",
                    "Don't know"
                    ),


# # HCV treatment ever
everhcvtreat = case_when(
    q_hc_txe == 1  ~ "Yes < 12 months",
    q_hc_txe == 2  ~ "Yes > 12 months",
    q_hc_txe == 3  ~ "No",
    q_hc_txe == 99 ~ "Don't know")%>%
        ff_label("Ever treated for HCV infection")%>%
        fct_relevel("No",
                    "Yes < 12 months",
                    "Yes > 12 months",
                    "Don't know"),


# HCV antibody testing - did the participant receive an HCV antibody test
hvc_ab_yn = as_factor(hvc_ab_yn1.factor)%>%
        fct_recode("No" = "0",
                   "Yes" = "1")%>%
        ff_label("HCV antibody test")%>%
        fct_relevel("No",
                    "Yes"))

































# Save
save(data, file =
       here::here("analysis", "data", "processed_data", "ethos_all_factors.rda")
)


crss <- data %>%
  tbl_cross()




# Create a theme for figures, tables and all processed text

apa_theme <- function (ft)  {
  ft %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::fontsize(size = 10, part = "all") %>%
    flextable::align(align = "left", part = "all") %>%
    flextable::align(align = "left", part = "header") %>%
    #flextable::rotate(rotation = "lrtb", align = "top", part = "body") %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = officer::fp_border(width = 1.5), part = "all") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 1.5), part = "all") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 1.5), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 1.5), part = "footer") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 2), part = "all") %>%
    flextable::autofit()
}

# Set the theme to compact [reduce spacing around text and numbers]

## Set the theme as a compact [spacing]
set_gtsummary_theme(theme_gtsummary_compact())



data_table <- ethos %>%
  # dplyr::select(waves:site_name)%>%
  tbl_summary(by=meth_roa,
              include=c(waves, surveyage, eatsi, genderall.as_factor, homeless.fct,
                        prison.yn, oat_current.yn, oatdur, oat.month.yn, oat.type, inj_days, overdoseopi_12m,
                        naloxone_train,naloxone_access,dose.satisfied, dose.location, dose.takeaways.yn, collection.week,
                        health.pain, stigma.inject, stigma.inject.yn, stigma.hcv, stigma.hcv.yn, discrim.yn,
                        meth_roa),
              type = all_categorical() ~ "categorical",
              # label =
              #   list(age ~ "Age",
              #        agegroup ~ "Age group",
              #        # gender.mf ~ "Gender",
              #        methadoneever.yn ~ "Ever received methadone",
              #        homeless.factor ~ "Currently homeless"),
              missing = "no",
              percent = "row",
              statistic =(all_continuous()  ~ "{median} ({p25} - {p75})"))%>%
  add_stat_label()%>%
  add_overall(statistic = list(all_continuous()  ~ "{median} ({p25} - {p75})",
                               all_categorical() ~ "{n}")) %>%
  modify_footnote(update = everything() ~ NA, abbreviation = FALSE) %>%
  modify_header(
    stat_0 = '**Overall** \n **(n = {n})**',
    stat_1 = '**Inject methamphetamine** \n **(n = {n})**',
    stat_2 = '**Smoke methamphetamine** \n **(n = {n})**',
    stat_3 = '**Do not use methamphetamine** \n **(n = {n})**'
  ) %>%
  modify_caption("Table 1. Participant Characteristics")%>%
  bold_labels()%>%
  as_flex_table()%>%
  apa_theme()

data_table


amph_summary <- subset.opi6m %>%
  tbl_summary(by=meth_roa)%>%
  as_flex_table()%>%
  apa_theme()




























df_long <-
  data %>%
  pivot_longer(
    cols = c("heroin_inj", "amphet_inj", "coke_inj","methadone_inj", "bupe_inj",
             "morph_inj", "fent_inj", "otheropi_inj", "benzo_inj"),
    names_to = "drug_type_injected",
    values_to = "drug_injected"
  ) %>%
  mutate(drug_injected = replace_na(drug_injected, "unknown")) %>% # convert NA to "unknown"

  ggplot(                                                        # begin ggplot!
    mapping = aes(x = oatdur, fill = meth_roa))+
  geom_bar(position = "fill", col = "black") +
  scale_fill_brewer(palette = "Pastel1")+
  labs(
    x = "Symptom",
    y = "Symptom status (proportion)"
  )

df_long

symptoms_data_long <- symptoms_data %>%    # begin with "mini" linelist called symptoms_data

  pivot_longer(
    cols = -case_id,                       # pivot all columns except case_id (all the symptoms columns)
    names_to = "symptom_name",             # assign name for new column that holds the symptoms
    values_to = "symptom_is_present") %>%  # assign name for new column that holds the values (yes/no)

  mutate(symptom_is_present = replace_na(symptom_is_present, "unknown"))



ggplot(df_long, aes(x = drug_type_injected, fill = drug_injected)) +
  geom_col(position = "dodge", colour = "black") +
  scale_fill_brewer(palette = "Pastel1")


create_pivot(data)


create_pivot <- function(pt, attrib) {
  pt$addData(df)
  pt$addColumnDataGroups("isIntUsagePersonal")
  pt$addRowDataGroups(attrib, addTotal=TRUE)
  pt$defineCalculation(calculationName="Total", summariseExpression="n()")
  pt$evaluatePivot()
  cells <- pt$findCells(highN=1, totals = "exclude")
  pt$setStyling(cells=cells, declarations=list("background-color"="#00FF00"))
  cells <- pt$findCells(lowN=1, totals = "exclude")
  pt$setStyling(cells=cells, declarations=list("background-color"="red"))
  pt$renderPivot()
}

create_ggplot <- function(attrib, titleName, xlabName) {
  ggplot(data=data, aes(x=attrib, fill=isIntUsagePersonal)) +  ggtitle(titleName) +
    xlab(xlabName) + ylab("InternetUsage") +
    geom_bar(stat="count", position=position_dodge(), colour="black")+
    geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1, colour = "white", position = position_dodge(.9))
}

create_barplot <- function(attrib_value,attrib) {
  title <- paste0("Number of Respondents by ", str_to_title(attrib_value))
  xaxis_title <- str_to_title(attrib_value)
  Barplot <- barplot(sort(table(attrib)), main = title,
                     ylab = "Number of Respondents",
                     xlab = xaxis_title ,
                     las = 1, cex.names=0.9,col="#00BBDB")
}



cre





count_data <- df_long %>%
  count(drugs_injected)

count_data

ggplot(data = df_long) +
  geom_bar(
    mapping = aes(x = drugs_injected))


amph_summary <- subset.opi6m %>%
  tbl_summary(by=meth_roa)%>%
  as_flex_table()%>%
  apa_theme()

amph_summary

t <- tbl_summary(ds_opi6m)

tbl_character <- data  %>%
  tbl_summary(by=meth_roa, include = c(waves, surveyage, eatsi, genderall.as_factor, homeless.fct,
                                       prison.yn, oat_current.yn, oatdur, oat.month.yn, oat.type, inj_days, overdoseopi_12m,
                                       naloxone_train,naloxone_access,dose.satisfied, dose.location, dose.takeaways.yn, collection.week,
                                       health.pain, stigma.inject, stigma.inject.yn, stigma.hcv, stigma.hcv.yn, discrim.yn,
                                       meth_roa),


              type = all_categorical() ~ "categorical",
              # label =
              #   list(age ~ "Age",
              #        agegroup ~ "Age group",
              #        # gender.mf ~ "Gender",
              #        methadoneever.yn ~ "Ever received methadone",
              #        homeless.factor ~ "Currently homeless"),
              missing = "no",
              statistic = all_continuous() ~ ("{median} ({p25} - {p75})"),
              digits = list(all_categorical() ~ c(0),
                            injectingbenzo.month.f ~ c(1)) %>%
                add_stat_label()%>%
                modify_footnote(update = everything() ~ NA, abbreviation = FALSE) %>%
                # modify_header(
                #   stat_0 = "**Enrolled** \n **(n = {N})**",
                # )%>%
                modify_caption("Table 1. Participant Characteristics"))


# Convert to flextable [the suffix "ft" = "flextable"]

tbl_character.ft <- tbl_character %>%
  as_flex_table()%>%
  apa_theme()

# Display the table

tbl_character.ft

# Save the table as an a) .docx file; and b) .html file

# a)
tbl_character.ft %>%
  flextable::save_as_html(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics.html")

# b)
tbl_character.ft %>%
  flextable::save_as_docx(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics.docx")



