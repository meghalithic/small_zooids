#FOR EVERY 1˚C, A CHANGE IN 0 TO 6.5 um IN LENGTH

# can also read in "temperature_zooid" in Dropbox/ROCKS-PARADOX/Bryozoans

#from Menon 1972
#ep = Electra pilosa
#cr = Conopenum reticulum
#units are um
#degrees in C
ep.temps <- c(6, 12, 18, 22)
ep.len <- c(686.34, 596.37, 586.35, 577)
ep.wid <- c(303.61, 311.81, 314.37, 314.30)
summary(lm(ep.len ~ ep.temps)) #slope: -6.429 
summary(lm(ep.wid ~ ep.temps)) #slope: 0.6621 

plot(ep.len ~ ep.temps)
plot(ep.wid ~ ep.temps)


cr.temps <- c(12, 18, 22)
cr.len <- c(558.31, 518.65, 500)
cr.wid <- c(315.26, 285.18, 314.24)
summary(lm(cr.len ~ cr.temps)) #slope: -5.8925 
summary(lm(cr.wid ~ cr.temps)) #slope: -0.4897 

plot(cr.len ~ cr.temps)
plot(cr.wid ~ cr.temps)


#from Silén & Harmelin
#Haplopoma sciaphium
#units are um
hs.temps <- c()
hs.temps[1] <- mean(c(12, 23))
hs.temps[2] <- mean(c(9, 12))
hs.temps[3] <- mean(c(1, 17))
hs.len <- c(440.2, 501.7, 566.8)
summary(lm(hs.len ~ hs.temps)) #slope = -12.990

plot(hs.len ~ hs.temps)

#Lombardi et al.
#Pentapora fascialis
#mart = mean annual range of temperature; C
#mat = mean annual temperature; C
#units are um
pf.loc <- c("Plymouth", "Lizard", "Grmac", "Tino Island",
            "Palau", "Praiano", "Cala Gonone", "Palmi",
            "Scoglitti")
pf.mart <- c(7.5, 5.5, 3.2, 8.9, 9.2, 10, 9.4, 8, 6.3)
pf.mat <- c(12, 11, 10.5, 17.2, 15.5, 17.4, 17.5, 16.6, 17.5)
pf.len <- c(824.41, 849.69, 753.16, 782.94, 797.14, 757.66, 766,38, 863.33)
pf.wid <- c(458.22, 415.77, 420.89, 409.91, 401.16, 429.22, 420.81, 427.32, 380.57)
summary(lm(pf.mat ~ pf.len)) #slope = -0.002522
summary(lm(pf.mat ~ pf.wid)) #slope = -0.05435
summary(lm(pf.mart ~ pf.len)) #slope = -0.0009694
summary(lm(pf.mart ~ pf.wid)) #slope = 0.009702

plot(pf.len ~ pf.mart)
plot(pf.wid ~ pf.mart)

plot(pf.len ~ pf.mat)
plot(pf.wid ~ pf.mat)

#difference in small zooids to large zooids is ~400µm:
#y = mx + b
#y = zh
#figuring out x
#based on ep:
(400/704.738)+6.429 #6.996587 ˚C of temp change
#based on cr:
(400/627.7900)+5.8925 #6.529656 ˚C of temp change
#based on hs:
(400/663.109)+12.990 #13.59322 ˚C of temp change
#based on pf:
(400/16.824420)+0.002522 #23.77749 ˚C of temp change
