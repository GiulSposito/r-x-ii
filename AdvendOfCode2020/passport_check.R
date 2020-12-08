library(tidyverse)

# read data as text (vector of string by lines)
input <- read_lines("AdvendOfCode2020/day04_input.txt")

# change a empty line to "|" (trick to split later)
input[input==""] <- "|"

# colapse the data and split the data 
# into a list of fields x value
passports <- input %>% 
  # colapse into one string
  str_c(collapse = " ") %>% 
  # split each passport data
  str_split("\\|") %>% 
  unlist() %>% 
  # for each passport split the fields
  str_split(" ") %>% 
  # pre-process the fields
  map(function(line){
    # remove empty strings and split into "name":"value
    pd <- str_split(line[line!=""], ":", simplify = T)
    # return a list of fields
    pd[,2] %>% 
      split(1:length(.)) %>% 
      set_names(pd[,1]) %>% 
      return()
  })


is.valid.passdata <- function(pdata){
  
  # # it hasn't enough fields
  # if (length(pdata)<7) return(F)
  # 
  # # it hasn't enough field
  # if ( length(pdata)==7 & is.null(pdata[["cid"]]) ) return(F)
  
  # byr (Birth Year) - four digits; at least 1920 and at most 2002.
  byr <- (
    !is.null(pdata[["byr"]]) &&
      nchar(pdata[["byr"]])==4 &&
      as.integer(pdata[["byr"]]) >= 1920 &&
      as.integer(pdata[["byr"]]) <= 2002 
  )
  
  # iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  iyr <- (
    !is.null(pdata[["iyr"]]) &&
      nchar(pdata[["iyr"]])==4 &&
      as.integer(pdata[["iyr"]]) >= 2010 &&
      as.integer(pdata[["iyr"]]) <= 2020 
  )
  
  # eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  eyr <- (
    !is.null(pdata[["eyr"]]) &&
      nchar(pdata[["eyr"]])==4 &&
      as.integer(pdata[["eyr"]]) >= 2020 &&
      as.integer(pdata[["eyr"]]) <= 2030 
  )
  
  # hgt (Height) - a number followed by either cm or in:
  hgt <- !is.null(pdata[["hgt"]]) && (
    str_detect(pdata[["hgt"]], "[0-9]+[cm|in]") &&
      #  - If cm, the number must be at least 150 and at most 193.
      ( (str_detect(pdata[["hgt"]],"cm") &&
           as.integer(str_extract(pdata[["hgt"]],"[0-9]+"))>=150 &&
           as.integer(str_extract(pdata[["hgt"]],"[0-9]+"))<=193 )  ||
          #  - If in, the number must be at least 59 and at most 76.
          (str_detect(pdata[["hgt"]],"in") &
             as.integer(str_extract(pdata[["hgt"]],"[0-9]+"))>=59 &&
             as.integer(str_extract(pdata[["hgt"]],"[0-9]+"))<=76 ) )
  )
  
  # hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  hcl <- !is.null(pdata[["hcl"]]) &&
    str_detect(pdata[["hcl"]],"#[0-9a-f]{6}") &&
    nchar(pdata[["hcl"]])==7
  
  # ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  ecl <- !is.null(pdata[["ecl"]]) &&
    (pdata[["ecl"]] %in% c("amb","blu","brn","gry","grn","hzl","oth"))
  
  # pid (Passport ID) - a nine-digit number, including leading zeroes.
  pid <- !is.null(pdata[["pid"]]) &&
    str_detect(pdata[["pid"]], "[0-9]{9}") &&
    nchar(pdata[["pid"]])==9
  
  # cid (Country ID) - ignored, missing or not.
  # ...

  tibble(
    pdata = list(as.tibble(pdata)),
    valid = (byr & iyr & eyr & hgt & hcl & ecl & pid),
    l.byr = byr,
    l.iyr = iyr,
    l.eyr = eyr,
    l.hgt = hgt,
    l.hcl = hcl,
    l.ecl = ecl,
    l.pid = pid
  ) %>% 
    return()
}

pdata <- passports[[1]]

pass.check <- passports %>% 
  map_df(is.valid.passdata)

sum(pass.check$valid * 1)

pc <- pass.check %>% 
  unnest(pdata)



pc %>% 
  # byr (Birth Year) - four digits; at least 1920 and at most 2002.
  mutate( byr = as.integer(byr) ) %>% 
  mutate( check.byr = (!is.na(byr) & byr>=1920 & byr<=2002) ) %>% 
  # filter( check.iyr == l.iyr)
  # iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  mutate( iyr = as.integer(iyr) ) %>% 
  mutate( check.iyr = (!is.na(iyr) & iyr>=2010 & iyr<=2020) ) %>% 
  # filter( check.iyr == l.iyr)
  # eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  mutate( eyr = as.integer(eyr) ) %>% 
  mutate( check.eyr = (!is.na(eyr) & eyr>=2020 & eyr<=2030)) %>% 
  # filter( check.eyr == l.eyr)
  # hgt (Height) - a number followed by either cm or in:
  # - If cm, the number must be at least 150 and at most 193.
  # - If in, the number must be at least 59 and at most 76.
  mutate( hgt.fmt.chk = str_detect(hgt,"[0-9]+(in|cm)") ) %>% 
  mutate( hgt.val = as.integer(str_extract(hgt,"[0-9]+")),
          hgt.unit = str_extract(hgt,"(in|cm)")) %>% 
  mutate( check.hgt = hgt.fmt.chk & case_when(
    (!hgt.fmt.chk) ~ F,
    hgt.unit == "cm" ~ (hgt.val>=150 & hgt.val<=193),
    hgt.unit == "in" ~ (hgt.val>=59 & hgt.val<=76),
    T ~ F
  )) %>% 
  #filter( l.hgt == check.hgt ) %>% 
  # hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  mutate( check.hcl = (!is.na(hcl) & str_detect(hcl, "#[0-9a-f]{6}")) ) %>% 
  filter( l.hcl == check.hcl ) %>% 
  # ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  mutate( check.ecl = (!is.na(ecl) & ecl %in% c("amb","blu","brn","gry","grn","hzl","oth"))) %>% 
  filter( check.ecl == l.ecl) %>% 
  # pid (Passport ID) - a nine-digit number, including leading zeroes.
  mutate( check.pid = nchar(pid)==9 ) %>% 
  filter( check.pid != l.pid ) %>% 
  select(pid, check.pid, l.pid) %>% 
  mutate( pid.chk = str_detect(pid,"[0-9]{9}"))
# cid (Country ID) - ignored, missing or not.
