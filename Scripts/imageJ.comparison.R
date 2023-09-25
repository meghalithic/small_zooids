## Meghan A. Balk
## meghan.balk@nhm.uio.no

## This code:
# 1) compares the sizes of the small zooids extracted from Steginator
# to those by imageJ

#### LOAD PACKAGES ----
require(stringr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(lmodel2)
require(tidyverse)

#### SET UP ENV ----

col.form = c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", 
             "#00A9FF", "#C77CFF", "#FF61CC")


col.traits = c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", 
               "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC")

#### LOAD DATA ----

###### LOOK AT RESULTS FROM IMG J -----
table(sm.zoo$colony.id)
unique(sm.zoo$imageName[sm.zoo$colony.id == "077CV"]) #from images 1, 2, 3
## image 077_CV
imgj.res_077_1 <- read.csv("~/Desktop/007_CV_1_Results_13Sept2023.csv.csv", header = TRUE)
imgj.res_077_2 <- read.csv("~/Desktop/007_CV_2_Results_13Sept2023.csv", header = TRUE)
imgj.res_077_3 <- read.csv("~/Desktop/077_CV_3_Results_13Sept2023.csv", header = TRUE)
## scale
imgj.res_077_1[1,] #first row is the scale to .1 mm; scale is 28 pixels per .1 mm; or 280 per 1mm; or 280000 per 1 micrometer
# 28 px/.1 mm or .1 mm / 28 px
imgj.res_077_1$scale.zh <- (imgj.res_077_1$Length/28)*1000
imgj.res_077_2[1,] #29.017
imgj.res_077_2$scale.zh <- (imgj.res_077_2$Length/29.017)*1000
imgj.res_077_3[1,] #0.109
imgj.res_077_3$scale.zh <- (imgj.res_077_3$Length/0.109)*1000

imgj_077 <- rbind(imgj.res_077_1, imgj.res_077_2, imgj.res_077_3)

## log
imgj_077$ln.zh <- log(imgj_077$scale.zh)
range(imgj_077$ln.zh) # all small!!
#-1.715532  6.075998
range(df$ln.zh[df$imageName == "077_CV_1_15v_x30_BSE" |
                 df$imageName == "077_CV_2_15v_x30_BSE" |
                 df$imageName == "077_CV_3_15v_x30_BSE"])
#5.789308 6.086177

imgj.res_696_1 <- read.csv("~/Desktop/696_CC_1_Results_13Sept2023.csv", header = TRUE)
imgj.res_696_2 <- read.csv("~/Desktop/696_CC_2_Results_13Sept2023.csv", header = TRUE)
imgj.res_696_1[1,] #60
imgj.res_696_1$scale.zh <- (imgj.res_696_1$Length/60)*1000
imgj.res_696_2[1,] #60
imgj.res_696_2$scale.zh <- (imgj.res_696_2$Length/60)*1000
imgj.res_696 <- rbind(imgj.res_696_1, imgj.res_696_2)
imgj.res_696$ln.zh <- log(imgj.res_696$scale.zh)
range(imgj.res_696$ln.zh)
#6.907755 9.148988
range(df$ln.zh[df$imageName == "696_CC_1_10v_x30_BSE" |
                 df$imageName == "696_CC_2_10v_x30_BSE"])
#6.599759 7.061558

##273S
unique(sm.zoo$imageName[sm.zoo$colony.id == "273S"]) #from images 1, 2, 3
imgj.res_273_1 <- read.csv("~/Desktop/273_S_1_Results_13Sept2023.csv", header = TRUE)
imgj.res_273_2 <- read.csv("~/Desktop/273_S_2_Results_13Sept2023.csv", header = TRUE)
imgj.res_273_3 <- read.csv("~/Desktop/273_S_3_Results_13Sept2023.csv", header = TRUE)
imgj_273 <- rbind(imgj.res_273_1, imgj.res_273_2, imgj.res_273_3)
## scale
imgj_273$scale.zh <- imgj_273$Length/.606
## log
imgj_273$ln.zh <- log(imgj_273$scale.zh)
imgj_273$ln.zh #all small!
