library(tidyverse)

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










