# title: Clean freeCodeCamp's 2018 New Coder Survey
# description: This script cleans specifically freeCodeCamp's 2018 New Coder
#   Survey.
# author: Eric Leung (@erictleung)
# date: 2018-09-03
# last_updated: 2019-01-28


# Overview -------------------------------------------------------------
# - Load packages               Load necessary packages for cleaning
# - Useful Functions            Sub-components for processing data
# - Main Processing Functions   Big, main components of cleaning
# - Main Function               Run entire script to clean data


# Load Packages --------------------------------------------------------

# Used packages in tidyverse
# - dplyr
# - tidyr
# - stringr
library(tidyverse)
library(here)
library(tools)


# Sub-Process Functions -----------------------------------
# Description:
#   These functions perform larger grouped data transformations

# Title:
#   Change Characters to One
# Description:
#   Lots of columns need to be changed from characters to just 1, indicated that
#   the respondent checked this option. Check if input is NA and changes it to a
#   character 1.
#
#   Note: vchar_to_one is meant to be used on vectors.
# Input:
#   Vector of characters, can have NA
# Output:
#   Vector of characters, with just "1"'s and NAs
# Usage:
#   > test_vec <- c(sample(letters, 10), rep(NA, 3))
#   > vchar_to_one(tes)
#   #   x    f    r    w    i    v    q    a    l    h <NA> <NA> <NA>
#   # "1"  "1"  "1"  "1"  "1"  "1"  "1"  "1"  "1"  "1"   NA   NA   NA
char_to_one <- function(x) {
  if (!is.na(x)) {
    "1"
  } else {
    x
  }
}
vchar_to_one <- Vectorize(char_to_one)


# Main Process Functions ----------------------------------
# Description:
#   These functions encompass the bulk work of the cleaning and transformation

#   Rename Survey Variables
# Usage:
#   > renamed_data <- rename_data_vars(dat)
rename_data_vars <- function(dat) {
  renamed_data <- dat %>%
    rename(
      is_software_dev = "Are you already working as a software developer?",
      is_first_dev_job = "Is this your first software development job?",
      months_job_search = "Before you got this job, how many months did you spend looking for a job?",
      job_pref = "Would you prefer to...",

      # Job interests
      job_intr_fllstck = "Full-Stack Web Developer",
      job_intr_backend = "Back-End Web Developer",
      job_intr_frntend = "Front-End Web Developer",
      job_intr_mobile = "Mobile Developer",
      job_intr_devops = "DevOps / SysAdmin",
      job_intr_datasci = "Data Scientist",
      job_intr_teacher = "Teacher / Trainer / Developer Evangelist",
      job_intr_qa_engn = "Quality Assurance Engineer",
      job_intr_ux_engn = "User Experience Designer",
      job_intr_projm = "Product Manager",
      job_intr_gamedev = "Game Developer",
      job_intr_infosec = "Information Security",
      job_intr_dataengn = "Data Engineer",
      job_intr_other = "Other",

      when_appl_job = "When do you plan to start applying for developer jobs?",
      expected_earn = "About how much money do you expect to earn per year at your first developer job (in US Dollars)?",
      job_lctn_pref = "Would you prefer to work...",
      job_relocate = "Are you willing to relocate for a job?",

      # Reasons to code
      reasons_to_code = "What is your biggest reason for learning to code?",
      reasons_to_code_other = "Other_1", # Very inspiring column to read

      # Learning resources
      rsrc_fcc = "freeCodeCamp",
      rsrc_mdn = "Mozilla Developer Network (MDN)",
      rsrc_so = "Stack Overflow",
      rsrc_edx = "EdX",
      rsrc_coursera = "Coursera",
      rsrc_khan_acdm = "Khan Academy",
      rsrc_pluralsght = "Pluralsight",
      rsrc_codeacdm = "Codecademy",
      rsrc_udacity = "Udacity",
      rsrc_udemy = "Udemy",
      rsrc_code_wars = "Code Wars",
      rsrc_treehouse = "Treehouse",
      rsrc_hackerrank = "HackerRank",
      rsrc_frntendmstr = "Front End Masters",
      rsrc_lynda = "Lynda.com",
      rsrc_egghead = "Egghead.io",
      rsrc_css_tricks = "CSS Tricks",
      rsrc_other = "Other_2",

      # Coding events attended
      codeevnt_fcc = "freeCodeCamp study groups",
      codeevnt_hackthn = "hackathons",
      codeevnt_confs = "conferences",
      codeevnt_workshps = "workshops",
      codeevnt_startupwknd = "Startup Weekend",
      codeevnt_nodeschl = "NodeSchool",
      codeevnt_womenwc = "Women Who Code",
      codeevnt_girldevit = "Girl Develop It",
      codeevnt_coderdojo = "CoderDojo",
      codeevnt_meetup = "Meetup.com events",
      codeevnt_railsbrdg = "RailsBridge",
      codeevnt_gamejam = "Game Jam",
      codeevnt_railsgrls = "Rails Girls",
      codeevnt_djangogrls = "Django Girls",
      codeevnt_wkndbtcmp = "weekend bootcamps",
      codeevnt_other = "Other_3",

      # Podcasts listened to
      podcast_fcc = "The freeCodeCamp Podcast",
      podcast_codenewbie = "Code Newbie",
      podcast_changelog = "The Changelog",
      podcast_sedaily = "Software Engineering Daily",
      podcast_js_jabber = "JavaScript Jabber",
      podcast_syntaxfm = "Syntax.fm",
      podcast_ltcwm = "Learn To Code With Me",
      podcast_fullstckrd = "Full Stack Radio",
      podcast_frnthppyhr = "Front End Happy Hour",
      podcast_codingblcks = "Coding Blocks",
      podcast_shoptalk = "Shop Talk Show",
      podcast_devtea = "Developer Tea",
      podcast_progthrwdwn = "Programming Throwdown",
      podcast_geekspeak = "Geek Speak",
      podcast_hanselmnts = "Hanselminutes",
      podcast_talkpythonme = "Talk Python To Me",
      podcast_rubyrogues = "Ruby Rogues",
      podcast_codepenrd = "CodePen Radio",
      podcast_seradio = "Software Engineering Radio",
      podcast_other = "Other_4",

      # YouTube channels
      yt_mit_ocw = "MIT Open Courseware",
      yt_fcc = "freeCodeCamp's YouTube channel",
      yt_computerphile = "Computerphile",
      yt_devtips = "DevTips",
      yt_cs_dojo = "CS Dojo",
      yt_engn_truth = "Engineered Truth",
      yt_learncodeacdm = "LearnCode.Academy",
      yt_lvluptuts = "LevelUpTuts",
      yt_funfunfunct = "Fun Fun Function",
      yt_codingtuts360 = "Coding Tutorials 360",
      yt_codingtrain = "Coding Train",
      yt_derekbanas = "Derek Banas",
      yt_simplilearn = "Simplilearn",
      yt_simpleprog = "Simple Programmer (Bulldog Mindset)",
      yt_mozillahacks = "Mozilla Hacks",
      yt_googledevs = "Google Developers",
      yt_other = "Other_5",

      # Learning information
      hours_learning = "About how many hours do you spend learning each week?",
      months_programming = "About how many months have you been programming for?",

      # Bootcamps
      bootcamp_attend = "Have you attended a full-time coding bootcamp?",
      bootcamp_name = "Which one?",
      bootcamp_finished = "Have you finished yet?",
      bootcamp_have_loan = "Did you take out a loan to pay for the bootcamp?",
      bootcamp_recommend = "Based on your experience, would you recommend this bootcamp to your friends?",

      money_for_learning = "Aside from university tuition, about how much money have you spent on learning to code so far (in US dollars)?",
      age = "How old are you?",

      # Individual's gender
      gender = "What's your gender?",
      gender_other = "Other_6",

      # Demographics
      country_citizen = "Which country are you a citizen of?",
      country_live = "Which country do you currently live in?",
      live_city_population = "About how many people live in your city?",
      is_ethnic_minority = "Are you an ethnic minority in your country?",
      lang_at_home = "Which language do you you speak at home with your family?",

      # Education
      school_degree = "What's the highest degree or level of school you have completed?",
      school_major = "What was the main subject you studied in university?",

      # Personal and family information
      marital_status = "What's your marital status?",
      has_finance_depends = "Do you financially support any dependents?",
      has_children = "Do you have children?",
      num_children = "How many children do you have?",
      do_finance_support = "Do you financially support any elderly relatives or relatives with disabilities?",
      debt_amt = "Do you have any debt?",
      home_mrtg_has = "Do you have a home mortgage?",
      home_mrtg_owe = "About how much do you owe on your home mortgage (in US Dollars)?",
      student_debt_has = "Do you have student loan debt?",
      student_debt_amt = "About how much do you owe in student loans (in US Dollars)?",

      # Employment status information
      curr_emplymnt = "Regarding employment status, are you currently...",
      curr_emplymnt_other = "Other_7",
      curr_field = "Which field do you work in?",
      last_yr_income = "About how much money did you make last year (in US dollars)?",
      communite_time = "About how many minutes does it take you to get to work each day?",
      is_self_employed = "Do you consider yourself under-employed?",
      has_served_military = "Have you served in your country's military before?",
      is_recv_disab_bnft = "Do you receive disability benefits from your government?",
      has_high_spd_ntnet = "Do you have high speed internet at your home?",

      # Miscellaneous
      time_start = "Start Date (UTC)",
      time_end = "Submit Date (UTC)",
      network_id = "Network ID"
    )
  renamed_data
}


# Main Function -------------------------------------------

# Title:
#   Main Cleaning Function
# Description:
#   This is the main cleaning and transformation function. It will write a new
#   file in the `clean-data/` directory.
# Usage:
#   > main()
main <- function() {
  # Read in data
  data_path <- here("raw-data", "2018-New-Coders-Survey.csv")
  dat <- data_path %>%
    read_csv() %>%
    rename(ID = "#")

  # Rename variables with easier names
  renamed_data = rename_data_vars(dat)

  # Change variables (jobs,rsrc,codeevnt,podcast,yt) to boolean
  bool_changed_data <- renamed_data %>%
    # Change job interest
    mutate_at(vars(starts_with("job_intr_"), -job_intr_other),
              vchar_to_one) %>%

    # Change resources
    mutate_at(vars(starts_with("rsrc_"), -rsrc_other),
              vchar_to_one) %>%

    # Change coding events
    mutate_at(vars(starts_with("codeevnt_"), -codeevnt_other),
              vchar_to_one) %>%

    # Change podcasts
    mutate_at(vars(starts_with("podcast_"), -podcast_other),
              vchar_to_one) %>%

    # Change YouTube channels
    mutate_at(vars(starts_with("yt_"), -yt_other),
              vchar_to_one)

  # Remove outliers for age, but keep NA values
  # Oldest living is 116, so filtering on that age
  # https://en.wikipedia.org/wiki/List_of_the_oldest_living_people
  age_outlier_removed <- bool_changed_data %>%
    filter(age < 116 | is.na(age))

  # Remove questionable months learning by cross-checking age
  # Here, convert age to months and take the difference between age
  # and months programming. Here, remove entries that claim you've
  # programmed more than you've been alive.
  # Counts:
  # - 5 years =  60
  # - 10 years = 120
  # - 50 years = 600
  age_checked_learning <- age_outlier_removed %>%
    mutate(months_age = age * 12) %>%
    mutate(prog_age_diff = months_age - months_programming) %>%
    filter(prog_age_diff > 0) %>%
    select(-c(prog_age_diff, months_age))

  # Remove excess money for learning
  # Keep NA values as well
  remove_excess_money_spent <- age_checked_learning %>%
    filter(money_for_learning < 250000 |
             is.na(money_for_learning))

  # Remove high number of children
  rm_high_num_kids <- remove_excess_money_spent %>%
    filter(num_children < 20 | is.na(num_children))

  # Clean other columns by title casing terms
  cleaned_other_cols <- rm_high_num_kids %>%
    # Clean other job interests
    mutate(job_intr_other = if_else(
      !is.na(job_intr_other),
      toTitleCase(job_intr_other),
      job_intr_other)) %>%

    # Clean other resources used
    mutate(rsrc_other = if_else(
      !is.na(rsrc_other),
      toTitleCase(rsrc_other),
      rsrc_other)) %>%

    # Clean other coding events
    mutate(codeevnt_other = if_else(
      !is.na(codeevnt_other),
      toTitleCase(codeevnt_other),
      codeevnt_other)) %>%

    # Clean other podcasts
    mutate(podcast_other = if_else(
      !is.na(podcast_other),
      toTitleCase(podcast_other),
      podcast_other)) %>%

    # Clean other YouTube channels
    mutate(yt_other = if_else(
      !is.na(yt_other),
      toTitleCase(yt_other),
      yt_other))

  # Remove inconsistency in children
  # Keep all rational combinations of children
  #   - Doesn't have children and has 0 or didn't answer
  #   - Has children and has 1 or more children or didn't answer
  #   - Didn't answer either question
  # Variables:
  #   - num_children
  #   - has_children
  rm_inconsistent_kids <- cleaned_other_cols %>%
    filter(
      (has_children == 0 & num_children %in% c(0, NA)) |
      (has_children == 1 & (num_children > 0 | is.na(num_children))) |
      (is.na(has_children) & is.na(num_children))
    )

  # Remove high last year's income if greater than 1 million
  rm_high_income <- rm_inconsistent_kids %>%
    filter(last_yr_income < 1000000 | is.na(last_yr_income))

  # Remove too quick of responses
  rm_quick_responses <- rm_high_income %>%
    mutate(total_time_sec = time_end - time_start) %>%
    mutate(total_time_min = total_time_sec / 60) %>%
    mutate(total_time_min = as.numeric(total_time_min)) %>%
    rename(time_total_sec = total_time_sec) %>%
    filter(time_total_sec > 100) %>%
    select(-c(total_time_min))

  # Rename to final
  final <- rm_quick_responses

  # Combine data and create cleaned data
  out_path <- here("clean-data", "2018-fCC-New-Coders-Survey-Data.csv")
  write_csv(x = final, path = out_path)
}
