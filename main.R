library("gt")
library("readxl")
library("writexl")
library("lubridate")
library("tidyverse")


data = readxl::read_xlsx("sales.xlsx")
str(data)