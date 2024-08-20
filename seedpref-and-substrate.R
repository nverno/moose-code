#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)

## This is the final code analyzing seedling preference and
## substrate availability
## Note(Lixi Kong 9/30/2023): We decided to focus only on "seedlings" that were
## less or equal to 20cm tall for

# Load data from the specified directories
seg11 <- read.csv('path_to_file/Segment2011/seesapmas11.csv')
tr11 <- read.csv('path_to_file/Transect2011/trseedmas11.csv')
soil <- read.csv('path_to_file/SegTrPPSoil/squadsub.csv')
gps <- read.csv('path_to_file/GPS_ELEV/eleva.csv')
subp <- read.csv('path_to_file/SubstratePaper/quadsub.csv')

## Subset live seedling which we collected substrate it grow on and plots where
## we collected substrate coverage Substrate seeding data grow on collected in
## 1988, 1989, 1998, 1999
seesapmas11 <- seg11
seesapmas11 <- seesapmas11 %>%
  mutate(sub98 = ifelse(yrtag == 1998 & !is.na(ht98) & sub98 == '' & sub99 != '', sub99, sub98),
    ht99 = ifelse(yrtag == 1998 & !is.na(ht98) & sub99 != '' & is.na(ht99), ht98, ht99))

# Macro function converted to R function
seedsub <- function(a) {
  segseed <- seesapmas11 %>%
    filter(get(paste0("stat", a)) == "ALIVE" & get(paste0("sub", a)) != '' & spec != '') %>%
    filter(get(paste0("ht", a)) > 0 & get(paste0("ht", a)) <= 20) %>%
    mutate(pht = get(paste0("ht", a)),
      sub = case_when(
        get(paste0("sub", a)) %in% c("LITC", "LITM", "LITD") ~ "LITT",
        TRUE ~ get(paste0("sub", a))
      ),
      sub2 = get(paste0("sub", a)),
      year = 1900 + a,
      year_col = 1) %>%
    select(contnam, stpace, tag, yrtag, spec, year, year_col, pht, sub, sub2, elevcl, aspcl)

  sum_df <- segseed %>%
    group_by(spec, elevcl, aspcl) %>%
    summarize(cnt = n_distinct(tag))

  sum_ml_df <- segseed %>%
    filter(sub %in% c("MSS", "LITT")) %>%
    group_by(spec, elevcl, aspcl) %>%
    summarize(cnt_ml = n_distinct(tag))

  return(list(segseed = segseed, sum_df = sum_df, sum_ml_df = sum_ml_df))
}

result88 <- seedsub(88)
result89 <- seedsub(89)
result98 <- seedsub(98)
result99 <- seedsub(99)

samplesize <- reduce(list(result88$sum_df, result89$sum_df, result98$sum_df, result99$sum_df), full_join, by = c("spec", "elevcl", "aspcl"))
samplesize_ml <- reduce(list(result88$sum_ml_df, result89$sum_ml_df, result98$sum_ml_df, result99$sum_ml_df), full_join, by = c("spec", "elevcl", "aspcl"))

## Rename 1989 tag number becasue it has duplicated tag numbers with 1999, which
## will cause problem
segseed89 <- result89$segseed %>%
  mutate(tag = as.numeric(paste0("999999", tag)))

# Combine 98 and 99 data, take substrate from 99 if available
## If the same seedling had data in both 98 and 99, we should take the substrate
## collected in 99
segseed9899 <- full_join(result98$segseed, result99$segseed, by = c("contnam", "stpace", "tag"))
segseed9899 <- segseed9899 %>%
  mutate(segseedup = ifelse(!is.na(year98) & !is.na(year99), "98_99", NA_character_)) %>%
  filter(!(segseedup == "98_99" & !is.na(sub98)))

# Merging all years together
segseed <- bind_rows(result88$segseed, segseed89, segseed9899)

## substrat where HH transect seedings grew on collected in 1999 and 2011
# Substrate data for 1999 and 2011
seed99 <- tr11 %>%
  filter(stat99 == "ALIVE" & sub99 != '' & aspcl != 'S') %>%
  mutate(sub = case_when(sub99 %in% c('LITC', 'LITM') ~ "LITT", TRUE ~ sub99),
    year = 1999, year99 = 1, pht = ht99) %>%
  filter(ht99 > 0 & ht99 <= 20)

seed11 <- tr11 %>%
  filter(stat11 == "ALIVE" & sub11 != '') %>%
  mutate(sub = case_when(sub11 %in% c('LITC', 'LITM', 'LITD') ~ "LITT", TRUE ~ sub11),
    year = 2011, year11 = 1, pht = ht11) %>%
  filter(ht11 > 0 & ht11 <= 20)

# Combine 1999 and 2011 data, take substrate from 2011 if available
trseed <- bind_rows(seed99, seed11) %>%
  mutate(plotid = paste0(tran, 'p', tplot),
    elevcl = "4_HH") %>%
  group_by(tran, tplot, tag) %>%
  mutate(trdup = ifelse(!is.na(year99) & !is.na(year11), "99_11", NA_character_)) %>%
  filter(!(trdup == "99_11" & !is.na(sub11))) %>%
  ungroup()

# Combining all seedling data
segtrseed1 <- bind_rows(segseed, trseed) %>%
  filter(aspcl != 'S') %>%
  mutate(elevcl = case_when(elevcl == 'H' ~ '3_H',
    elevcl == 'M' ~ '2_M',
    elevcl == 'L' ~ '1_L',
    TRUE ~ elevcl),
  sub = case_when(sub %in% c('WDG5', 'WDG1', 'WDG2') ~ 'WDG', TRUE ~ sub),
  # add in 2023: any BESPP at H/HH should be BECO
  spec = case_when(spec == 'BESPP' & elevcl %in% c('4_HH', '3_H') ~ 'BECO', TRUE ~ spec))

## Add elevation data from GPS master file to seeding data and substrate data
# Elevation data merging
elev <- gps %>%
  mutate(plotid = case_when(TRAN != '' ~ paste0(TRAN, 'p', TPLOT),
    CONTNAM != '' ~ paste0(CONTNAM, 'S', STPACE))) %>%
  group_by(plotid) %>%
  summarize(eleva = mean(ELEVA, na.rm = TRUE))

segtrseed <- left_join(segtrseed1, elev, by = "plotid") %>%
  mutate(eleva = case_when(grepl('L', plotid) ~ 840,
    grepl('M', plotid) ~ 990,
    grepl('H', plotid) ~ 1140,
    TRUE ~ eleva))

# Merge the quadsub_0 and elev datasets by plotid and apply various transformations
quadsub <- merge(quadsub_0, elev, by = "plotid") %>%
  filter(!is.na(cens)) %>%
  mutate(
    eleva = case_when(
      grepl('L', plotid) ~ 840,
      grepl('M', plotid) ~ 990,
      grepl('H', plotid) ~ 1140,
      TRUE ~ eleva
    ),
    LITM_C = ifelse(!is.na(LITM) & LITM != 0 & !is.na(LITC) & LITC != 0, 1, 0),
    LITC_D = ifelse(!is.na(LITC) & LITC != 0 & !is.na(LITD) & LITD != 0, 1, 0),
    LITM_D = ifelse(!is.na(LITD) & LITD != 0 & !is.na(LITM) & LITM != 0, 1, 0),
    LITC_ONLY = ifelse(!is.na(LITC) & LITC != 0 & LITD == 0 & LITM == 0, 1, 0),
    LITD_ONLY = ifelse(!is.na(LITD) & LITD != 0 & LITC == 0 & LITM == 0, 1, 0),
    LITM_ONLY = ifelse(!is.na(LITM) & LITM != 0 & LITC == 0 & LITD == 0, 1, 0)
  ) %>%
  arrange(cens, plotid, elevcl, aspcl)

# Create QUADSUB_DIST dataset by grouping and summarizing the quadsub data
quadsub_dist <- quadsub %>%
  group_by(CENS, ELEVCL) %>%
  summarise(
    LITM_C_CNT = sum(LITM_C, na.rm = TRUE),
    LITC_D_CNT = sum(LITC_D, na.rm = TRUE),
    LITM_D_CNT = sum(LITM_D, na.rm = TRUE),
    LITC_O_CNT = sum(LITC_ONLY, na.rm = TRUE),
    LITD_O_CNT = sum(LITD_ONLY, na.rm = TRUE),
    LITM_O_CNT = sum(LITM_ONLY, na.rm = TRUE)
  )

# Create QUADCNT dataset by counting the number of entries for each CENS and ELEVCL group
quadcns <- quadsub %>%
  group_by(CENS, ELEVCL) %>%
  summarise(CNT = n())

## Chi-square test and Chesson's index at ELEVCL level
## For each year/elevation/spec, % of seedings that grow on different
## substrates

# Calculate the number of seedlings by year, elevation, species, and substrate
seedelev1 <- segtrseed %>%
  filter(sub %in% c('MSS', 'LITT')) %>%
  group_by(year, elevcl, spec, sub) %>%
  summarise(count = n_distinct(tag)) %>%
  ungroup()

# Calculate the subtotal by year, elevation, and species
seedelev <- seedelev1 %>%
  group_by(year, elevcl, spec) %>%
  mutate(subtotal = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(seedpct = count / subtotal)

# Sort by substrate and year
seedelev <- seedelev %>%
  arrange(sub, year)

# Create a list of substrates with initial seed percentage set to 0 for the
# first occurrence
sublist <- seedelev %>%
  group_by(sub) %>%
  filter(row_number() == 1) %>%
  mutate(seedpct = 0) %>%
  select(sub, seedpct) %>%
  ungroup()

# Sort data by year, elevation, and species
seedelev <- seedelev %>%
  arrange(year, elevcl, spec)

# Create a unique list of year, elevation, and species
spec <- seedelev %>%
  distinct(year, elevcl, spec)

# Combine the spec and sublist to create the full list
list_df <- tidyr::expand_grid(spec, sublist) %>%
  arrange(year, elevcl, spec, sub)

# Sort seedelev data by year, elevation, species, and substrate
seedelev <- seedelev %>%
  arrange(year, elevcl, spec, sub)

# Update the list with seedelev data and fill in missing counts with 0
seedall1 <- full_join(list_df, seedelev, by = c("year", "elevcl", "spec", "sub")) %>%
  mutate(count = replace_na(count, 0))

# New analysis in 2023 distinguishing LITD and LITC
seedelev2 <- segtrseed %>%
  filter(sub2 %in% c("LITC", "LITD")) %>%
  group_by(year, elevcl, spec, sub2) %>%
  summarise(count = n_distinct(tag)) %>%
  ungroup()

seedelev_n <- seedelev2 %>%
  group_by(year, elevcl, spec) %>%
  mutate(subtotal = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(seedpct = count / subtotal)

# Sort by sub2 and year
seedelev_n <- seedelev_n %>%
  arrange(sub2, year)

# Create a list of sub2 with initial seed percentage set to 0 for the first occurrence
sublist2 <- seedelev_n %>%
  group_by(sub2) %>%
  filter(row_number() == 1) %>%
  mutate(seedpct = 0) %>%
  select(sub2, seedpct) %>%
  ungroup()

# Sort data by year, elevation, and species
seedelev_n <- seedelev_n %>%
  arrange(year, elevcl, spec)

# Create a unique list of year, elevation, and species for sub2
spec_n <- seedelev_n %>%
  distinct(year, elevcl, spec)

# Combine the spec_n and sublist2 to create the full list for sub2
list_n <- tidyr::expand_grid(spec_n, sublist2) %>%
  arrange(year, elevcl, spec, sub2)

# Sort seedelev_n data by year, elevation, species, and sub2
seedelev_n <- seedelev_n %>%
  arrange(year, elevcl, spec, sub2)

# Update the list with seedelev_n data and fill in missing counts with 0
seedall_2 <- full_join(list_n, seedelev_n, by = c("year", "elevcl", "spec", "sub2")) %>%
  mutate(count = replace_na(count, 0))

## For each elevation/spec, average of % and count of seeding growing on
## different substrate over different census years Only looking at MOSS and
## LITTER shall we recalculate the previous % only looking at moss and
## litter? -- Yes, updated this
# Calculate mean seed percentage and count, grouped by elevation, species, and substrate
seedall2 <- seedall1 %>%
  filter(sub %in% c('MSS', 'LITT')) %>%
  group_by(elevcl, sub, spec) %>%
  summarise(
    mseedpct = mean(seedpct, na.rm = TRUE),
    mcount = mean(count, na.rm = TRUE)
  ) %>%
  arrange(elevcl, spec) %>%
  ungroup()

# Calculate the sum of mcount grouped by elevation and species
seedall <- seedall2 %>%
  group_by(elevcl, spec) %>%
  mutate(sumcount = sum(mcount, na.rm = TRUE)) %>%
  ungroup()

# Calculate mpctcnt as an integer value of mseedpct multiplied by sumcount
seedall <- seedall %>%
  mutate(mpctcnt = as.integer(mseedpct * sumcount))

# New in 2023: Distinguishing LITD and LITC
seedall_n2 <- seedall_2 %>%
  filter(sub2 %in% c('LITD', 'LITC')) %>%
  group_by(elevcl, sub2, spec) %>%
  summarise(
    mseedpct = mean(seedpct, na.rm = TRUE),
    mcount = mean(count, na.rm = TRUE)
  ) %>%
  arrange(elevcl, spec) %>%
  ungroup()

# Calculate the sum of mcount for the new analysis grouped by elevation and species
seedall_N <- seedall_n2 %>%
  group_by(elevcl, spec) %>%
  mutate(sumcount = sum(mcount, na.rm = TRUE)) %>%
  ungroup()

# Calculate mpctcnt for the new analysis
seedall_N <- seedall_N %>%
  mutate(mpctcnt = as.integer(mseedpct * sumcount)) %>%
  arrange(elevcl, spec)

## Average MSS or LITT coverage at each ELEVCL
# Calculate average values for various substrate types grouped by census and elevation class
subelev <- quadsub %>%
  group_by(cens, elevcl) %>%
  summarise(
    avemss = mean(mssg, na.rm = TRUE) / 100,
    avelitt = mean(litt, na.rm = TRUE) / 100,
    avelitd = mean(LITD, na.rm = TRUE) / 100,
    avelitc = mean(LITC, na.rm = TRUE) / 100,
    avebla5 = mean(bla5, na.rm = TRUE) / 100,
    avebld5 = mean(bld5, na.rm = TRUE) / 100,
    avewdg = mean(wdg, na.rm = TRUE) / 100,
    avebsoil = mean(bsoil, na.rm = TRUE) / 100,
    averck = mean(rck, na.rm = TRUE) / 100,
    avewater = mean(water, na.rm = TRUE) / 100,
    avetip = mean(tipa, na.rm = TRUE) / 100,
    avestp = mean(stpa, na.rm = TRUE) / 100
  ) %>%
  ungroup()

# Pool the average values across all censuses, grouped by elevation class
subelevpool <- subelev %>%
  group_by(elevcl) %>%
  summarise(
    MSS = mean(avemss, na.rm = TRUE),
    LITT = mean(avelitt, na.rm = TRUE),
    LITD = mean(avelitd, na.rm = TRUE),
    LITC = mean(avelitc, na.rm = TRUE),
    BLA5 = mean(avebla5, na.rm = TRUE),
    BLD5 = mean(avebld5, na.rm = TRUE),
    WDG = mean(avewdg, na.rm = TRUE),
    BSOIL = mean(avebsoil, na.rm = TRUE),
    RCK = mean(averck, na.rm = TRUE),
    WATER = mean(avewater, na.rm = TRUE),
    TIP = mean(avetip, na.rm = TRUE),
    STP = mean(avestp, na.rm = TRUE)
  ) %>%
  ungroup()

# Calculate the normalized values for MSS and LITT
subelevpool <- subelevpool %>%
  mutate(
    nmss = MSS / (MSS + LITT),
    nlitt = LITT / (MSS + LITT)
  )

# New in 2023: Calculate normalized values for LITD and LITC
subelevpool_N <- subelevpool %>%
  mutate(
    nlitd = LITD / (LITD + LITC),
    nlitc = LITC / (LITD + LITC)
  )

## All substrate type covarege combining ELEVCL
## /*data quadsub; set quadsub;
## if wdg5 ne . and wdg1=. then wdg=wdg5;
## if wdg5=. and wdg1 ne . then wdg=wdg1;
## if wdg5=. and wdg1=. then wdg=0;
## proc sql; create table subelev_a as select
## cens,mean(mssg)/100 as avemss,mean(litt)/100 as avelitt, mean(bla5)/100 as
## avebla5, mean(bld5)/100 as avebld5,mean(wdg)/100 as avewdg,
## mean(bsoil)/100 as avebsoil, mean(rck)/100 as averck, mean(water)/100 as
## avewater, mean(tipa)/100 as avetip, mean(stpa)/100 as avestp
## from quadsub group by cens;quit;
## proc sql; create table subelevpool_a as select
## mean(avemss) as MSS, mean(avelitt) as LITT, mean(avebla5) as BLA5,
## MEAN(AVEBLD5) AS BLD5, MEAN(AVEWDG) AS WDG,
## MEAN(AVEBSOIL) AS BSOIL, MEAN(AVERCK) AS RCK, MEAN(AVEWATER) AS WATER,
## MEAN(AVETIP) AS TIP, MEAN(AVESTP) AS STP
## from subelev_a;quit;*/
## /********A Pre-test we did: we tested whether BEAL and BECO has different
## distribution on LITT and MSS,
## basing on average % (not average count). The result is not significant, so
## we’ll keep combing BEAL and BECO.***********************************/
## /*data becobeal; set seedall_a;
## if (spec='BECO' OR SPEC='BEAL') AND (SUB='LITT' OR SUB='MSS');
## ADJCNT=MSEEDPCT*SUMCOUNT;PROC SORT; BY SUB;RUN;
## PROC FREQ DATA=BECOBEAL;
## TABLE SPEC*SUB/CHISQ;
## WEIGHT ADJCNT;
## RUN;*/

## Chi-square tests
# Filter the seedall data by species
abba <- seedall %>% filter(spec == 'ABBA')
acsa <- seedall %>% filter(spec == 'ACSA')
bespp <- seedall %>% filter(spec == 'BESPP')
piru <- seedall %>% filter(spec == 'PIRU')
soam <- seedall %>% filter(spec == 'SOAM')
beal <- seedall %>% filter(spec == 'BEAL')
beco <- seedall %>% filter(spec == 'BECO')

# Function to perform chi-square tests
chisqyre <- function(data, elevcl_val, d, e) {
  data_filtered <- data %>% filter(elevcl == elevcl_val) %>% arrange(sub)
  chisq_result <- chisq.test(table(data_filtered$sub), p = c(d, e), rescale.p = TRUE)

  # Apply weights if necessary
  observed <- as.vector(table(data_filtered$sub))
  expected <- chisq_result$expected

  # Print results
  print(chisq_result)

  # Return the chisq_result if further analysis is needed
  return(chisq_result)
}

# Perform chi-square tests
chisqyre(abba, '1_L', 0.909, 0.091)
chisqyre(abba, '2_M', 0.807, 0.193)
chisqyre(abba, '3_H', 0.751, 0.249)
chisqyre(abba, '4_HH', 0.464, 0.536)

chisqyre(acsa, '1_L', 0.909, 0.091)

chisqyre(piru, '1_L', 0.909, 0.091)
chisqyre(piru, '2_M', 0.807, 0.193)

chisqyre(soam, '1_L', 0.909, 0.091)
chisqyre(soam, '2_M', 0.807, 0.193)
chisqyre(soam, '3_H', 0.751, 0.249)

chisqyre(bespp, '1_L', 0.909, 0.091)
chisqyre(bespp, '2_M', 0.807, 0.193)

chisqyre(beal, '1_L', 0.909, 0.091)
chisqyre(beal, '2_M', 0.807, 0.193)

chisqyre(beco, '1_L', 0.909, 0.091)
chisqyre(beco, '2_M', 0.807, 0.193)
chisqyre(beco, '3_H', 0.751, 0.249)
chisqyre(beco, '4_HH', 0.464, 0.536)

## -------------------------------------------------------------------
### Chesson's Index

# Transpose subelevpool data to get quadpct for nmss and nlitt
subelevpoolt <- subelevpool %>%
  pivot_longer(cols = c(nmss, nlitt), names_to = "sub", values_to = "quadpct") %>%
  mutate(sub = ifelse(sub == "nlitt", "LITT", "MSS")) %>%
  arrange(elevcl, sub)

# Sort seedall by elevcl and sub
seedall <- seedall %>%
  arrange(elevcl, sub)

# Merge seedall with subelevpoolt and calculate rn
chesson <- merge(seedall, subelevpoolt, by = c("elevcl", "sub")) %>%
  mutate(rn = mseedpct / quadpct) %>%
  filter(!(spec == "PIRU" & elevcl == "3_H")) %>%
  arrange(spec, elevcl, sub)

# Calculate srn and final Chesson index values
chessona <- chesson %>%
  group_by(spec, elevcl) %>%
  mutate(srn = sum(rn, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    sub = case_when(sub == "LITT" ~ "2_LITT", sub == "MSS" ~ "1_MSS", TRUE ~ sub),
    chesson = rn / srn,
    ABBA = ifelse(spec == "ABBA", chesson, NA),
    ACSA = ifelse(spec == "ACSA", chesson, NA),
    BECO = ifelse(spec == "BECO", chesson, NA),
    BEAL = ifelse(spec == "BEAL", chesson, NA),
    BESPP = ifelse(spec == "BESPP", chesson, NA),
    PIRU = ifelse(spec == "PIRU", chesson, NA),
    SOAM = ifelse(spec == "SOAM", chesson, NA),
    elev_r = case_when(
      elevcl == "4_HH" ~ "1224-1385",
      elevcl == "1_L" ~ "840",
      elevcl == "2_M" ~ "990",
      elevcl == "3_H" ~ "1140",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(sub, spec, elevcl)

# New in 2023: Transpose subelevpool_n to get quadpct for nlitd and nlitc
subelevpool_nt <- subelevpool_N %>%
  pivot_longer(cols = c(nlitd, nlitc), names_to = "sub2", values_to = "quadpct") %>%
  mutate(sub2 = ifelse(sub2 == "nlitc", "LITC", "LITD")) %>%
  arrange(elevcl, sub2)

# Sort seedall_n by elevcl and sub2
seedall_n <- seedall_N %>%
  arrange(elevcl, sub2)

# Merge seedall_n with subelevpool_nt and calculate rn
chesson_N <- merge(seedall_n, subelevpool_nt, by = c("elevcl", "sub2")) %>%
  mutate(rn = mseedpct / quadpct) %>%
  filter(!(spec == "PIRU" & elevcl == "3_H")) %>%
  arrange(spec, elevcl, sub2)

# Calculate srn and final Chesson index values for the new analysis
chesson_na <- chesson_N %>%
  group_by(spec, elevcl) %>%
  mutate(srn = sum(rn, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    chesson = rn / srn,
    ABBA = ifelse(spec == "ABBA", chesson, NA),
    ACSA = ifelse(spec == "ACSA", chesson, NA),
    BECO = ifelse(spec == "BECO", chesson, NA),
    BEAL = ifelse(spec == "BEAL", chesson, NA),
    BESPP = ifelse(spec == "BESPP", chesson, NA),
    PIRU = ifelse(spec == "PIRU", chesson, NA),
    SOAM = ifelse(spec == "SOAM", chesson, NA),
    elev_r = case_when(
      elevcl == "4_HH" ~ "1224-1385",
      elevcl == "1_L" ~ "840",
      elevcl == "2_M" ~ "990",
      elevcl == "3_H" ~ "1140",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(sub2, spec, elevcl)

## -------------------------------------------------------------------
### Fig. 5

# Assuming `chessona` is the data frame already prepared in your environment
# If `chessona` contains the necessary data filtered by '1_MSS', then:

# Create the base plot
plot <- ggplot(data = subset(chessona, sub == '1_MSS'), aes(x = elev_r)) +
  geom_point(aes(y = ABBA), shape = 1, color = "black", size = 5) +
  geom_line(aes(y = ABBA), color = "black") +

  geom_point(aes(y = ACSA), shape = 16, color = "grey", size = 5) +
  geom_line(aes(y = ACSA), color = "grey") +

  geom_point(aes(y = BECO), shape = 18, color = "grey", size = 5) +
  geom_line(aes(y = BECO), color = "grey") +

  geom_point(aes(y = BEAL), shape = 18, fill = "grey", color = "grey", size = 5) +
  geom_line(aes(y = BEAL), color = "grey") +

  geom_point(aes(y = PIRU), shape = 6, color = "black", size = 5) +
  geom_line(aes(y = PIRU), color = "black") +

  geom_point(aes(y = SOAM), shape = 15, color = "black", size = 5) +
  geom_line(aes(y = SOAM), color = "black") +

  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +

  labs(title = "Chesson's index by ELEVCL",
    x = "Elevation (m)",
    y = "Preference for Moss (Chesson index)") +

  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "italic")
  ) +

  scale_y_continuous(breaks = c(seq(0, 0.1, by = 0.1), seq(0.4, 1, by = 0.1)))

# Add custom annotations (replacing the annotation logic from SAS)
# You can add arrows, rectangles, or text using grid functions
annotation <- rectGrob(x = 0.1, y = 0.2, width = 0.2, height = 0.3, gp = gpar(fill = "white", col = NA))

# Draw the plot
grid.draw(annotation)
print(plot)

## -------------------------------------------------------------------
### Fig. 6

# Filter chessona for '1_MSS'
chesson_lm <- chessona %>% filter(sub == '1_MSS')

# Calculate average Chesson index grouped by species
avelm <- chesson_lm %>%
  group_by(spec) %>%
  summarise(ave_chesson = mean(chesson, na.rm = TRUE)) %>%
  arrange(spec)

# Create seedmass data
seedmass <- data.frame(
  spec = c("BECO", "BEAL", "SOAM", "PIRU", "ABBA", "ACSA"),
  seedmass = c(0.3289, 1.0101, 2.8571, 3.3333, 7.6923, 64.616)
)

# Merge avelm with seedmass
fig6 <- merge(avelm, seedmass, by = "spec") %>%
  arrange(seedmass)

# Perform linear regression
model <- lm(ave_chesson ~ seedmass, data = fig6)
fig6_fitted <- data.frame(fig6, pred = predict(model, fig6))

# Plot the results
ggplot(fig6_fitted, aes(x = seedmass, y = ave_chesson)) +
  geom_point(color = "black", size = 4) +
  geom_line(aes(y = pred), linetype = "dashed", color = "blue") +
  labs(
    x = "Seeding mass (mg)",
    y = "Seeding preference for moss (Chesson's index)",
    title = "Chesson's index vs Seeding Mass"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(0, 65), breaks = seq(0, 65, by = 5))

# Repeat the process for LITC and LITD (2023 analysis)
chesson_lm_n <- chesson_na %>% filter(sub2 == 'LITC')

# Calculate average Chesson index for LITC grouped by species
avelm_n <- chesson_lm_n %>%
  group_by(spec) %>%
  summarise(ave_chesson = mean(chesson, na.rm = TRUE)) %>%
  arrange(spec)

# Merge avelm_n with seedmass
fig6_n <- merge(avelm_n, seedmass, by = "spec") %>%
  arrange(seedmass)

# Perform linear regression for LITC analysis
model_n <- lm(ave_chesson ~ seedmass, data = fig6_n)
fig6_fitted_n <- data.frame(fig6_n, pred = predict(model_n, fig6_n))

# Plot the results for LITC
ggplot(fig6_fitted_n, aes(x = seedmass, y = ave_chesson)) +
  geom_point(color = "black", size = 4) +
  geom_line(aes(y = pred), linetype = "dashed", color = "blue") +
  labs(
    x = "Seeding mass (mg)",
    y = "Seeding preference for LITC (Chesson's index)",
    title = "Chesson's index vs Seeding Mass (LITC)"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(0, 65), breaks = seq(0, 65, by = 5))

## -------------------------------------------------------------------
### Logistic Regression at individual level

# Define a function similar to the SAS macro %sp
sp <- function(data, species) {
  # Calculate odds and predicted probabilities
  data <- data %>%
    mutate(
      ODDS = exp(XBETA),
      PPROB = PRED * 100,
      MSS = ifelse(SUB == 'MSS', PPROB, NA),
      LITT = ifelse(SUB == 'LITT', PPROB, NA)
    )

  # Calculate mean probabilities for each species and substrate
  tjur <- data %>%
    group_by(spec, SUB) %>%
    summarise(aveprob = mean(PRED, na.rm = TRUE)) %>%
    pivot_wider(names_from = SUB, values_from = aveprob) %>%
    mutate(tjur = MSS - LITT)

  # Plot predicted probabilities against NMSS for each elevcl
  ggplot(data %>% filter(spec == species), aes(x = NMSS)) +
    geom_point(aes(y = MSS), shape = 16, color = "grey", size = 4, alpha = 0.2) +
    geom_point(aes(y = LITT), shape = 1, color = "grey", size = 4) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
    facet_wrap(~elevcl) +
    labs(y = "Predicted probability", x = "NMSS") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))

  # Filter the data for a specific plotid and species
  plot_data <- data %>% filter(spec == species & plotid == 'LE1860S1840')

  # Plot predicted probabilities against PHT for each elevcl
  ggplot(plot_data, aes(x = PHT)) +
    geom_point(aes(y = MSS), shape = 16, color = "grey", size = 4) +
    geom_point(aes(y = LITT), shape = 1, color = "black", size = 4) +
    facet_wrap(~elevcl) +
    labs(y = "Predicted probability", x = "Initial height (cm)") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 20))
}

# Example usage:
# sp(logitbespp1, 'ABBA')
# sp(logitbespp1, 'BESPP')
# sp(logitbespp1, 'PIRU')
# sp(logitbespp1, 'SOAM')

# Turn off graphics
graphics.off()
