library(tidyverse)
library(janitor)
library(lubridate)

## C.1 -----------------------
senic <- read_table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC01.txt", col_names = FALSE)
senic %>%
  rename(id = X1,
         length = X2,
         age = X3,
         risk = X4,
         culturing_ratio = X5,
         xray_ratio = X6,
         beds = X7,
         med_school = X8,
         region = X9,
         patients = X10,
         nurses = X11,
         facilities = X12) %>%
  mutate(med_school = recode(med_school, "1" = "yes", "2" = "no"),
         region = recode(region, "1" = "NE", "2" = "NC", "3" = "S", "4" = "W")) %>%
  write_csv(file = "./senic.csv")

## C.2
cdi <- read_table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC02.txt", col_names = FALSE)
cdi %>%
  rename(id = X1,
         county = X2,
         state = X3,
         area = X4,
         pop = X5,
         percent_18_34 = X6,
         percent_65 = X7,
         physicians = X8,
         beds = X9,
         crimes = X10,
         high_school = X11,
         bachelors = X12,
         poverty = X13,
         unemployment = X14,
         capita_income = X15,
         total_income = X16,
         region = X17) %>%
  mutate(region = recode(region, "1" = "NE", "2" = "NC", "3" = "S", "4" = "W")) %>%
  write_csv(file = "./cdi.csv")


## C.3
market <- read_table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC03.txt", col_names = FALSE)
market %>%
  rename(num = X1,
         share = X2,
         price = X3,
         nielsen = X4,
         discount = X5,
         promotion = X6,
         month = X7,
         year = X8) %>%
  mutate(discount = recode(discount, "1" = "yes", "0" = "no"),
         promotion = recode(promotion, "1" = "yes", "0" = "no")) %>%
  write_csv("./market.csv")

## C.4
university <- read_table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC04.txt", col_names = FALSE)
university %>%
  rename(id = X1,
         gpa = X2,
         rank = X3,
         act = X4,
         year = X5) %>%
  write_csv("./university.csv")

## C.5
prostate <- read_table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC05.txt", col_names = FALSE)
prostate %>%
  rename(id = X1,
         psa = X2,
         volume = X3,
         weight = X4,
         age = X5,
         benign = X6,
         seminal = X7,
         capsular = X8,
         gleason = X9) %>%
  mutate(seminal = recode(seminal, "1" = "yes", "0" = "no")) %>%
  write_csv("./prostate.csv")

## C.6
website <- read_table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC06.txt", col_names = FALSE)
website %>%
  rename(id = X1,
         number = X2,
         backlog = X3,
         team = X4,
         experience = X5,
         change = X6,
         year = X7,
         quarter = X8) %>%
  write_csv("./website.csv")


## C.7
estate <- read_table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC07.txt", col_names = FALSE)
estate %>%
  rename(id = X1,
         price = X2,
         area = X3,
         bed = X4,
         bath = X5,
         ac = X6,
         garage = X7,
         pool = X8,
         year = X9,
         quality = X10,
         style = X11,
         lot = X12,
         highway = X13) %>%
  mutate(ac = recode(ac, "1" = "yes", "0" = "no"),
         pool = recode(pool, "1" = "yes", "0" = "no"),
         quality = recode(quality, "1" = "high", "2" = "medium", "3" = "low"),
         highway = recode(highway, "1" = "yes", "0" = "no")) %>%
  write_csv("./estate.csv")



## Mile
mile <- read_csv("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Mile/data/mile.csv")
mile %>%
  select(year, seconds) %>%
  write_csv(file = "./mile.csv")


## Kleiber
kleiber <- read_csv("../../raw_data/kleiber_raw.csv")
kleiber <- janitor::clean_names(kleiber)
kleiber %>%
  select(order, family, species, mass = species_avg_mass_g, bmr = species_avg_bmr_w) %>%
  filter(!is.na(mass)) %>%
  mutate(mass = str_remove_all(mass, "\\s+"),
         bmr = str_remove_all(bmr, "\\s+"),
         mass = str_replace_all(mass, "·", "\\."),
         bmr = str_replace_all(bmr, "·", "\\."),
         mass = parse_double(mass),
         bmr = parse_double(bmr)) ->
  kleiber

write_csv(x = kleiber, file = "kleiber.csv")


dccovid <- read_csv("../../raw_data/dccovid.csv")
tibble(date = names(dccovid)) %>%
  mutate(date = str_remove(date, "\\.\\.\\.\\d+")) %>%
  mutate(is2021 = cumsum(date == "1-Jan") == 1) %>%
  mutate(date = case_when(is2021 ~ str_c(date, "-2021"),
                          !is2021 ~ str_c(date, "-2020"))) %>%
  .$date ->
  names(dccovid)
names(dccovid)[1:2] <- c("type", "var")

dccovid %>%
  filter(type == "Testing") %>%
  select(-type) %>%
  gather(-var, key = "day", value = "count", na.rm = FALSE) %>%
  spread(key = var, value = count, drop = FALSE) %>%
  janitor::clean_names() %>%
  mutate(day = dmy(day)) %>%
  arrange(day) %>%
  rename(cleared = cleared_from_isolation,
         lost = total_lives_lost,
         dctested = total_number_of_dc_residents_tested,
         tested = total_overall_number_of_tests,
         positives = total_positives) %>%
  mutate(cleared = parse_number(cleared),
         lost = parse_number(lost),
         dctested = parse_number(dctested),
         tested = parse_number(tested),
         positives = parse_number(positives)) %>%
  write_csv("./dccovid.csv")

# C.8
heating <- read_table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC08.txt", col_names = FALSE)
heating %>%
  rename(id = X1,
         orders = X2,
         interest = X3,
         homes = X4,
         discount = X5,
         inventories = X6,
         sold = X7,
         tempdev = X8,
         year = X9,
         month = X10) %>%
  mutate(date = make_date(year = year, month = month)) %>%
  select(-year, -month) %>%
  write_csv("./heating.csv")

# C.9
ischemic <- read_table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC09.txt", col_names = FALSE)
ischemic %>%
  rename(id = X1,
         cost = X2,
         age = X3,
         gender = X4,
         interventions = X5,
         drugs = X6,
         er = X7,
         complications = X8,
         comorbidities = X9,
         duration = X10) %>%
  mutate(gender = if_else(gender == 1, "male", "other")) %>%
  write_csv("./ischemic.csv")

# C.10
disease <- read_table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC10.txt", col_names = FALSE)
disease %>%
  rename(id = X1,
         age = X2,
         socioeconomic = X3,
         sector = X4,
         disease = X5,
         savings = X6) %>%
  mutate(sector = if_else(sector == 1, "s1", "s2")) %>%
  write_csv("./disease.csv")

# C.11
ipo <- read_table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC11.txt", col_names = FALSE)
ipo %>%
  rename(id = X1,
         vc = X2,
         value = X3,
         shares = X4,
         buyout = X5) %>%
  mutate(vc = if_else(vc == 0, "no", "yes"),
         buyout = if_else(buyout == 0, "no", "yes")) %>%
  write_csv("./ipo.csv")

# Copier Maintenance
copier <- read_table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR20.txt", col_names = FALSE)
small <- read_table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%208%20Data%20Sets/CH08PR15.txt", col_names = FALSE)
colnames(small) <- "model"
copier %>%
  bind_cols(small) %>%
  rename(copiers = X2,
         minutes = X1) %>%
  mutate(model = recode(model,
                        "1" = "S",
                        "0" = "L")) %>%
  write_csv("./copiers.csv")

# Earnings
earnings <- read_csv("https://github.com/avehtari/ROS-Examples/raw/master/Earnings/data/earnings.csv")
earnings %>%
  mutate(mother_education = recode(mother_education, `99` = NA_real_),
         father_education = recode(father_education, `99` = NA_real_),
         sex = recode(male, `1` = "male", `0` = "female"),
         smokenow = recode(smokenow, `1` = "yes", `2` = "no")) %>%
  select(-male) %>%
  write_csv(file = "./earnings.csv")

# Ballot Data
ballot <- read_csv("../../raw_data/ballot_extract.csv", col_names = FALSE)
ballot %>%
  rename(poll = X1, vote = X2) %>%
  mutate(poll = round(poll, digits = 1),
         vote = round(vote, digits = 1)) %>%
  arrange(vote) ->
  ballot
write_csv("./ballot.csv", x = ballot)

# Muscle Mass
library(tidyverse)
muscle <- read_table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR27.txt", col_names = FALSE)
muscle %>%
  rename(mass = X1,
         age = X2) %>%
  write_csv("./muscle.csv", x = .)
