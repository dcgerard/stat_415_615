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

