## Meghan A. Balk
## meghan.balk@nhm.uio.no

## This code:
# 1) extracts colonies with small zooids
# These data are the output from creating traits from traits.R
# in the magnifica project

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

df <- read.csv("Data/traits_25Sept2023.csv",
         header = TRUE)


locality.df <- read.csv("Data/All.NZ.Samples_EDM_31.07.2023_sheet1.csv",
                        header = TRUE)

##### BIMODALITY -----
##explore bimodality, using zooid height as an example then see if it generalizes
p.ln.zh <- ggplot(df) +
  geom_density(aes(x = ln.zh)) +
  ggtitle(paste0("Zooid height, N zooids = ", nrow(df), ", N colony = ", length(keep))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(name = "Density") +
  scale_x_continuous(expression(ln~Zooid~Height~(mu*m)))

#ggsave(p.zh, 
#  file = "./Results/zooid.height_8Sept2023.png", width = 14, height = 10, units = "cm")

ggplot(df) +
  geom_histogram(aes(x = ln.zh)) +
  ggtitle(paste0("Zooid height, N zooids = ", nrow(df), ", N colony = ", length(keep))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(name = "Frequency") +
  scale_x_continuous(expression(ln~Zooid~Height~(mu*m)))

##driven by FORMATION?
#all but Punneki Limestone are bimodal
p.zh.form <- ggplot(df) +
  geom_density(aes(x = ln.zh,
                   group = formation,
                   col = formation)) + 
  ggtitle(paste0("Zooid height by formation, N zooids = ", nrow(df), ", N colony = ", length(keep))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(name = "Density") +
  scale_x_continuous(expression(ln~Zooid~Height~(mu*m)))

#ggsave(p.zh.form, 
# file = "./Results/zooid.height.by.formation_8Sept2023.png", width = 14, height = 10, units = "cm")

#only see bimodal in Waipurpu, NKBS, Upper Kai-Iwi

###### SAME INDIVIDUALS & COLONIES? ------
## really convince self that these are the same individuals
# make data more manageable by reducing it to the three formations
# make data even more manageable by reducing it to the small hump that is seen
nk.wa.uki <- df[df$formation == "NKBS" |
                  df$formation == "Waipuru" |
                  df$formation == "Upper Kai-Iwi",]
sm.zoo <- nk.wa.uki[nk.wa.uki$ln.zh <= 6.25,]
nrow(sm.zoo) #492 (was 943) zooids
length(unique(sm.zoo$colony.id)) #34 (was 64) colonies
table(sm.zoo$colony.id) #a lot of one offs, but some clusters
## look at a couple of these:
sm.zoo[sm.zoo$colony.id == "077CV",]

## are these individuals from the same colony or across colonies?

p.ow.zh <- ggplot(data = df) +
  geom_smooth(aes(x = ln.zh, y = ln.ow.m), method = "lm") +
  geom_point(aes(x = ln.zh, y = ln.ow.m)) + #two clusters
  ggtitle(paste0("Scaling of operculum mid-width with zooid height, N zooids = ", nrow(df), ", N colony = ", length(keep))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(name = "LN Operculum mid-width") +
  scale_x_continuous(expression(ln~Zooid~Height~(mu*m)))
summary(lm(df$ln.ow.m ~ df$ln.zh)) #slope = 0.772476

#ggsave(p.ow.zh, 
#file = "./Results/ow.zh.scaling.png", width = 14, height = 10, units = "cm")

#look at 4 images:
#2 with the largest operculum width
#2 with the largest zooid height

slice_max(df, n = 2, order_by = ln.ow.m)

slice_max(df, n = 2, order_by = ln.zh)

zh.owm.model <- lm(ln.ow.m ~ ln.zh,
                   data = df)

##### SMALL HUMP -----
# know that these are all related, use zh as test
## test where hump is...
# frequency by size bin (quarter ln bins)
df.bins <- df %>% 
  mutate(zh.bin = cut(ln.zh, breaks = seq(5.5, 7.5, .1))) %>%
  as.data.frame()

df.bin.f <- df.bins %>%
  dplyr::group_by(zh.bin) %>%
  dplyr::summarise(n = n()) %>%
  as.data.frame()
View(df.bin.f)
#(6.2,6.3]
#6.25 like I eyeballed

#write.csv(df.bin.f,
#          "./Results/zh.bin.frequency_8Sept2023.csv",
#          row.names = FALSE)

sm.traits <- df[df$ln.zh < 6.25,]
sm.colonies <- unique(sm.traits$colony.id) #41; was 95 images out of 891

bins <- c("(5.5,5.6]", "(5.6,5.7]", "(5.7,5.8]",
          "(5.8,5.9]", "(5.9,6]", "(6,6.1]", 
          "(6.1,6.2]", "(6.2,6.3]")

df.bins$zh.bin <- as.character(df.bins$zh.bin)

df.bins$sm <- FALSE
df.bins$sm[df.bins$zh.bin %in% bins] <- TRUE

#look at proportions
prop.sm <- df.bins %>% 
  dplyr::group_by(colony.id) %>%
  dplyr::summarise(n.zooid = length(zooid.id),
                   n.sm.zooid = sum(sm),
                   prop.sm = n.sm.zooid/n.zooid)
View(prop.sm)

#write.csv(prop.sm,
#          "./Results/proportion.small.colonies_8Sept2023.csv",
#          row.names = FALSE)

#goes from 100% to 20%; perhaps make 20% the cut off
rm.col <- prop.sm$colony.id[prop.sm$prop.sm == 1]
length(rm.col) #31 colonies

small.colonies <- df[df$colony.id %in% rm.col,]
reg.colonies <- df[!(df$colony.id %in% rm.col),]

#for those with 20%, see where the sizes are
sm.zooids <- prop.sm$colony.id[prop.sm$prop.sm < 1 &
                                 prop.sm$prop.sm > 0]
length(sm.zooids) #19

range(df$zh[df$colony.id %in% sm.zooids])
#330.5689 1701.3353
sort(df$ln.zh[df$colony.id %in% sm.zooids])
#only 11 below 6.25, so probably fine

##### WRITE OUT DATASETS ----
write.csv(small.colonies,
          "./Data/small.colonies.traits_8Sept2023.csv",
          row.names = FALSE)

#### EXPLORE DIFFERENCES IN SUMMARY STATISTICS ----

length(unique(df$colony.id)) #572
length(unique(reg.colonies$colony.id)) #541
length(unique(small.colonies$colony.id)) #31

range(reg.colonies$zh)
range(small.colonies$zh)
mean(reg.colonies$zh)
median(reg.colonies$zh)
sd(reg.colonies$zh)

range(reg.colonies$zh[reg.colonies$formation == "NKLS"]) #450.4467 1701.3353
mean(reg.colonies$zh[reg.colonies$formation == "NKLS"]) #803.359
sd(reg.colonies$zh[reg.colonies$formation == "NKLS"]) #101.0176

range(reg.colonies$zh[reg.colonies$formation == "NKBS"]) #494.0033 1495.0723
range(small.colonies$zh[small.colonies$formation == "NKBS"])
mean(reg.colonies$zh[reg.colonies$formation == "NKBS"]) #778.014
sd(reg.colonies$zh[reg.colonies$formation == "NKBS"]) #93.14028
mean(small.colonies$zh[small.colonies$formation == "NKBS"]) #392.4272; diff of 385.5868
median(reg.colonies$zh[reg.colonies$formation == "NKBS"])
median(small.colonies$zh[small.colonies$formation == "NKBS"])

range(reg.colonies$zh[reg.colonies$formation == "Tewkesbury"]) #330.5689 1668.3389
mean(reg.colonies$zh[reg.colonies$formation == "Tewkesbury"]) #780.6346
sd(reg.colonies$zh[reg.colonies$formation == "Tewkesbury"]) #99.51407

range(reg.colonies$zh[reg.colonies$formation == "Waipuru"]) #544.617 1361.006
range(small.colonies$zh[small.colonies$formation == "Waipuru"])
mean(reg.colonies$zh[reg.colonies$formation == "Waipuru"]) #801.5489
sd(reg.colonies$zh[reg.colonies$formation == "Waipuru"]) #95.42124
mean(small.colonies$zh[small.colonies$formation == "Waipuru"]) #379.6889; diff of 421.86
median(reg.colonies$zh[reg.colonies$formation == "Waipuru"])
median(small.colonies$zh[small.colonies$formation == "Waipuru"])

range(reg.colonies$zh[reg.colonies$formation == "Upper Kai-Iwi"]) #561.1774 1362.1250
range(small.colonies$zh[small.colonies$formation == "Upper Kai-Iwi"])
mean(reg.colonies$zh[reg.colonies$formation == "Upper Kai-Iwi"]) #885.2919
sd(reg.colonies$zh[reg.colonies$formation == "Upper Kai-Iwi"]) #118.5598
mean(small.colonies$zh[small.colonies$formation == "Upper Kai-Iwi"]) #392.8787; diff of 492.4132
median(reg.colonies$zh[reg.colonies$formation == "Upper Kai-Iwi"])
median(small.colonies$zh[small.colonies$formation == "Upper Kai-Iwi"])

range(reg.colonies$zh[reg.colonies$formation == "Tainui"]) #592.3886 1203.1921
mean(reg.colonies$zh[reg.colonies$formation == "Tainui"]) #947.1062
sd(reg.colonies$zh[reg.colonies$formation == "Tainui"]) #95.33073

range(reg.colonies$zh[reg.colonies$formation == "SHCSBSB"]) #418.482 1333.873
mean(reg.colonies$zh[reg.colonies$formation == "SHCSBSB"]) #932.8167
sd(reg.colonies$zh[reg.colonies$formation == "SHCSBSB"]) #95.79008

#### DISTRIBUTION OF OVERLAP ----

ggplot() +
  geom_histogram(aes(reg.colonies$ln.zh),
                 color = "#1F9E89FF", fill = "#1F9E89FF") +
  geom_histogram(aes(small.colonies$ln.zh),
                 color = "#482878FF", fill = "#482878FF") +
  ggtitle("Zooid height of Small and 'Regular' colonies") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(name = "Frequency") +
  scale_x_continuous(expression(ln~Zooid~Height~(mu*m)))

summary(lm(reg.colonies$ln.ow.m ~ reg.colonies$ln.zh)) #slope = 0.27
summary(lm(small.colonies$ln.ow.m ~ small.colonies$ln.zh)) #slope = 0.17

#### LOOK AT TRENDS RELATIVE TO LOCALITY ----
#use colony means
#mean_by_formation_colony
list.imageName <- str_split(df$imageName, fixed("_"))
df$specimenNum <- c()
for(i in 1:length(list.imageName)){
  df$specimenNum[i] <- list.imageName[[i]][1]
}
df$specimenNum
#remove all leading 0s
df$specimenNum <- str_remove(df$specimenNum, "^0+")

length(unique(df$specimenNum)) #566

locality.df$SAMPLE_ID 
length(unique(locality.df$SAMPLE_ID)) #531
nrow(locality.df) #538
# need to remove dupes
dupe.ids <- locality.df$SAMPLE_ID[duplicated(locality.df$SAMPLE_ID)]
#"115B"  "9.44"  "118"   "119"   "161"   "9.148" "9.95" 
# don't have 115B, 9.44, 9.148, 9.95 in df
# do have 118, 119, 161 in df
dupe.rid.ids <- c("115B", "9.148", "9.95", "9.44")
dupe.keep.ids <- c("118", "119", "161")
locality.df[locality.df$SAMPLE_ID %in% dupe.keep.ids,]
#118 from WABO I and II, different formations, can probably keep NKBS and not Landguard (esp since KV collected at NKBS)
#119 from WABO I and II, different formations, can probably keep NKBS and not Landguard (esp since KV collected at NKBS)
#161 both from WABO II, different formations, can probably keep Upper Castlecliff rather than Denby

locality.df.trim <- locality.df[-c(170, 171, 241),] #eliminate based on idnex
locality.df.trim[locality.df.trim$SAMPLE_ID %in% dupe.keep.ids,]
locality.df.trim <-  locality.df.trim[!(locality.df.trim$SAMPLE_ID %in% dupe.rid.ids),]
locality.df.trim[locality.df.trim$SAMPLE_ID %in% dupe.rid.ids,] #empty, few

loc.df.trim <- locality.df.trim %>%
  dplyr::select("Expedition", "SAMPLE_ID", "Formation_name",
                "GPS.lat", "GPS.long", "Physical.Description")

df.loc <- merge(df, loc.df.trim,
                by.x = "specimenNum",
                by.y = "SAMPLE_ID",
                all.x = TRUE, all.y = FALSE)
nrow(df.loc) #5971, same number as df

missing.loc <- c(unique(df.loc$specimenNum[is.na(df.loc$GPS.lat)]), unique(df.loc$specimenNum[df.loc$GPS.lat == ""]))
length(missing.loc) #357
#300: whiterock limestone, but we have it as NKBS (from WABO III)
length(df.loc$specimenNum[df.loc$specimenNum %in% missing.loc]) #3510

length(unique(df.loc$specimenNum[df.loc$specimenNum %in% missing.loc &
                                   df.loc$formation == "NKLS"])) #65
length(unique(df.loc$specimenNum[df.loc$specimenNum %in% missing.loc &
                                   df.loc$formation == "NKBS"])) #87
length(unique(df.loc$specimenNum[df.loc$specimenNum %in% missing.loc &
                                   df.loc$formation == "Tewkesbury"])) #106
length(unique(df.loc$specimenNum[df.loc$specimenNum %in% missing.loc &
                                   df.loc$formation == "Waipuru"])) #11
length(unique(df.loc$specimenNum[df.loc$specimenNum %in% missing.loc &
                                   df.loc$formation == "Upper Kai-Iwi"])) #18
length(unique(df.loc$specimenNum[df.loc$specimenNum %in% missing.loc &
                                   df.loc$formation == "Tainui"])) #19
length(unique(df.loc$specimenNum[df.loc$specimenNum %in% missing.loc &
                                   df.loc$formation == "SHCSBSB"])) #50

length(unique(df.loc$specimenNum[df.loc$GPS.lat != "" & 
                                   !is.na(df.loc$GPS.lat)])) #210 for which have info; less than half

## are the small ones in certain localities?
df.loc$size <- ""
df.loc$size[df.loc$ln.zh <= 6.25] <- "small"

nkbs.loc <- unique(df.loc$GPS.lat[df.loc$formation == "NKBS"])
nkbs.sm.loc <- unique(df.loc$GPS.lat[df.loc$formation == "NKBS" &
                                       df.loc$size == "small"]) #18 localities with small zooids
nkbs.reg.loc <- unique(df.loc$GPS.lat[df.loc$formation == "NKBS" &
                                        df.loc$size != "small"]) #18 localities with small zooids
setdiff(nkbs.sm.loc, nkbs.loc) #no diff, so all the small localities are also in the regular localities
length(nkbs.sm.loc) #18
length(nkbs.loc) #125
length(nkbs.reg.loc) #113
length(setdiff(nkbs.loc, nkbs.sm.loc)) #107 localities without small zooids
length(setdiff(nkbs.sm.loc, nkbs.reg.loc))

wai.loc <- unique(df.loc$GPS.lat[df.loc$formation == "Waipuru"])
wai.sm.loc <- unique(df.loc$GPS.lat[df.loc$formation == "Waipuru" &
                                      df.loc$size == "small"]) #1 localities with small zooids
wai.reg.loc <- unique(df.loc$GPS.lat[df.loc$formation == "Waipuru" &
                                       df.loc$size != "small"]) #1 localities with small zooids
setdiff(wai.sm.loc, wai.loc) #no diff, so all the small localities are also in the regular localities
length(wai.loc) #6
length(wai.reg.loc)
length(setdiff(wai.loc, wai.sm.loc)) #5 localities without small zooids


uki.loc <- unique(df.loc$GPS.lat[df.loc$formation == "Upper Kai-Iwi"])
uki.sm.loc <- unique(df.loc$GPS.lat[df.loc$formation == "Upper Kai-Iwi" &
                                      df.loc$size == "small"]) #4 localities with small zooids
uki.reg.loc <- unique(df.loc$GPS.lat[df.loc$formation == "Upper Kai-Iwi" &
                                       df.loc$size != "small"]) #4 localities with small zooids
setdiff(uki.sm.loc, uki.loc) #no diff, so all the small localities are also in the regular localities
length(uki.loc) #7
length(uki.reg.loc) #4
length(setdiff(uki.loc, uki.sm.loc)) #3 localities without small zooids


#### LOOK AT TRENDS RELATIVE TO SUBSTRATE ----


#### LOOK AT TRENDS RELATIVE TO TEMPERATURE ----
#use formation means
#mean_by_formation

df.form <- merge(reg.mean_by_formation, form.meta,
                 by.x = "formation",
                 by.y = "formationCode",
                 all.x = TRUE,
                 all.y = FALSE)

bottom = as.numeric(df.form$Isotope_Stage_Start)
top = as.numeric(df.form$Isotope_Stage_End)
df.form$med.O18 <- c()
df.form$sd.med.O18 <- c()
df.form$n.O18 <- c()
for (i in 1:nrow(df.form)){
  temp = oxy.18$d18O[which(oxy.18$Time <= bottom[i] & oxy.18$Time >= top[i])]
  df.form$med.O18[i] = median(temp)
  df.form$sd.med.O18[i] = sd(temp)
  df.form$n.O18[i] <- length(temp)
}

unique(df.form$med.O18)

p.temp.zh <- ggplot(df.form) +
  geom_point(aes(x = med.O18, y = avg.zh)) + 
  theme(text = element_text(size = 16)) +
  scale_x_continuous(expression(mean~delta^18~O)) +
  scale_y_continuous(expression(ln~Zooid~Height~(mu*m))) + 
  theme(text = element_text(size = 16),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

ggsave(p.temp.zh, 
       file = "./Results/temp.zh.png", 
       width = 14, height = 10, units = "cm")
#NO PATTERN
summary(lm(df.form$avg.zh ~ df.form$med.O18))
#slope = -0.04355; p = 0.7346, R2 = 0

ggplot(df.form) +
  geom_point(aes(x = sd.med.O18, y = avg.zh)) + 
  theme(text = element_text(size = 16)) +
  scale_x_continuous(expression(sd~delta~O^18)) +
  scale_y_continuous(expression(ln~Zooid~Height~(mu*m))) + 
  theme(text = element_text(size = 16),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
#NO PATTERN

## area
p.temp.area <- ggplot(df.form) +
  geom_point(aes(x = med.O18, y = avg.area/2)) + 
  theme(text = element_text(size = 16)) +
  scale_x_continuous(expression(mean~delta^18~O)) +
  scale_y_continuous(expression(ln~Zooid~Area~(mu*m^2))) + 
  theme(text = element_text(size = 16),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

ggsave(p.temp.area, 
       file = "./Results/temp.area.png", 
       width = 14, height = 10, units = "cm")
#NO PATTERN
summary(lm(df.form$avg.area ~ df.form$med.O18))
#slope = -0.09635; p = 0.7493, R2 = 0

# zooids scale to 0 to -6 with temp, getting smaller when warmer



