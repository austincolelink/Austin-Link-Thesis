########### Klebsiella sp. nov. has an effect on nematode life history response ##########

##### Austin Link #######

##### August 10th, 2024 ####

# This file is the code that I used for my manuscript + data analysis. This work was conducted in Gavin Woodruff's lab at the University of Oklahoma # 
# Each experiment is separated by a long break composed of "#" #

#R version 4.2.1 (2022-06-23)



######## Libraries #########

library(ggforce)
# citation("ggforce")
# Pedersen T (2022). ggforce: Accelerating 'ggplot2'. R package version 0.4.1,
# <https://CRAN.R-project.org/package=ggforce>.
#### USED FOR SINA PLOT!!!

library(cowplot)
# citation("cowplot")
# Wilke C (2020). cowplot: Streamlined Plot Theme and Plot Annotations for 'ggplot2'. R package
# version 1.1.1, <https://CRAN.R-project.org/package=cowplot>.
#### USED FOR THEME IN PLOTS!! 

library(reshape2)
# citation("reshape2")
# Hadley Wickham (2007). Reshaping Data with the reshape Package. Journal of Statistical Software,
# 21(12), 1-20. URL http://www.jstatsoft.org/v21/i12/.
#### USED TO MELT DATA FROM WIDE TO LONG

library(tidyverse)
# citation("tidyverse")
# Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L,
# Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu
# V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal
# of Open Source Software_, *4*(43), 1686. doi:10.21105/joss.01686
# <https://doi.org/10.21105/joss.01686>.

# GGPLOT2 REFERENCE 
## H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.

# TIDYR RFERENCE
## Wickham H, Vaughan D, Girlich M (2023). _tidyr: Tidy Messy Data. R package version 1.3.0,
## <https://CRAN.R-project.org/package=tidyr>.

# DPLYR REFERENCE
## Wickham H, François R, Henry L, Müller K, Vaughan D (2023). _dplyr: A Grammar of Data Manipulation. R package version 1.1.3, <https://CRAN.R-project.org/package=dplyr>.







library(effsize)
# citation("effsize")
# Torchiano M (2020). _effsize: Efficient Effect Size Computation_. doi:10.5281/zenodo.1480624
# <https://doi.org/10.5281/zenodo.1480624>, R package version 0.8.1,
# <https://CRAN.R-project.org/package=effsize>.
#### USED FOR COHENS D

library(lemon)
# citation("lemon")
# Edwards S (2023). _lemon: Freshing Up your 'ggplot2' Plots_. R package version 0.4.7,
# <https://CRAN.R-project.org/package=lemon>.

library(ggpubr)
# citation("ggpubr")
# Kassambara A (2023). _ggpubr: 'ggplot2' Based Publication Ready Plots_. R package version
# 0.6.0, <https://CRAN.R-project.org/package=ggpubr>.


library(lmtest)
# install.packages("lmtest")
# citation("lmtest")
# Achim Zeileis, Torsten Hothorn (2002). Diagnostic Checking in Regression Relationships. R
# News 2(3), 7-10. URL https://CRAN.R-project.org/doc/Rnews/


library(lsr)
# install.packages("lsr")
# citation("lsr")
# Navarro, D. J. (2015) Learning statistics with R: A tutorial for psychology students and
# other beginners. (Version 0.6) University of New South Wales. Sydney, Australia

########################

##########################################################################################################################






## STAT TEMPLATE

two.way <- aov(variable interest ~ variable1*variable2, data = dataframe)

summary(two.way)

tukey.two.way<-TukeyHSD(two.way)

tukey.two.way





##########################################################################################################################

############################## Crude fecundity (Without failed crosses) ##################################


# Set working directory 
setwd("/Users/austincolelink/Desktop/MANUSCRIPT/DATA/crosslevelfec")


# Import data into the R environment 
dat <- read.csv("crudefec.csv",header=T,stringsAsFactors = TRUE)


# Turning experiment and trial numbers into factors to make them "labels" as opposed to "numbers"
dat$experiment_number <- as.factor(dat$experiment_number)
dat$trial_number <- as.factor(dat$trial_number)


# Pulling out all of the clean data
clean_dat <- dat[dat$contamination == "clean",]


# Pasting trial and experiment numbers together to get unique ID's for grouping 
clean_dat$experiment_trial_number <- as.factor(paste(clean_dat$experiment_number, clean_dat$trial_number))


# Excluding failed crosses
## Contaminated crosses were already discared when pulling out the clean data, but this excludes plates that did not produce additional worms besides the original five in the cross
### This is why we are pulling out all of the crosses with more than 5 worms at the end of the seven day period 
clean_dat_no_failed_crosses <- clean_dat[clean_dat$number_worms > 5,]


# Assigning clean data to an object to alter names from WOUb# (lab strain nomenclature) to identified taxonomy 
named_bacteria_no_failed <- clean_dat_no_failed_crosses


# Creating Placeholders and then filling those placeholders with the Genus name corresponding to the WOUB#
named_bacteria_no_failed$bacteria_name_2 <- "placeholder"

named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "OP50",]$bacteria_name_2 <- "E. coli OP50"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "Klebsiella sp. nov.",]$bacteria_name_2 <- "Klebsiella sp. WOUb2"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB1",]$bacteria_name_2 <- "Stenotrophomonas sp. WOUb1"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB16",]$bacteria_name_2 <- "Comamonas sp. WOUb16"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB6",]$bacteria_name_2 <- "Stenotrophomonas sp. WOUb6"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB10",]$bacteria_name_2 <- "Pseudomonas sp. WOUb10"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB18",]$bacteria_name_2 <- "Leclercia sp. WOUb18"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB22",]$bacteria_name_2 <- "Acinetobacter sp. WOUb22"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB37",]$bacteria_name_2 <- "Agrobacterium sp. WOUb37"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB43",]$bacteria_name_2 <- "Microbacterium sp. WOUb43"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB9",]$bacteria_name_2 <- "Mammaliicoccus sp. WOUb9"


# Creating A "Fig or No" Prompt to be able to alter all fig/non-fig bacteria in ways corresponding to their isolation source, E. coli being the only non-fig
named_bacteria_no_failed$fig_or_no <- "placeholder"

named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "OP50",]$fig_or_no <- "No"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "Klebsiella sp. nov.",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB1",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB16",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB6",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB10",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB18",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB22",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB37",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB43",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB9",]$fig_or_no <- "Yes"


# Turning our bacterial names and fig/no prompt into factors, then arranging the levels of those into whatever order I want (descending order in the case of this figure) 
named_bacteria_no_failed$bacteria_name_2 <- as.factor(named_bacteria_no_failed$bacteria_name_2)
named_bacteria_no_failed$bacteria_name_2 <- factor(named_bacteria_no_failed$bacteria_name_2, levels=c("E. coli OP50", "Klebsiella sp. WOUb2", "Leclercia sp. WOUb18", "Agrobacterium sp. WOUb37", "Microbacterium sp. WOUb43", "Acinetobacter sp. WOUb22", "Comamonas sp. WOUb16", "Stenotrophomonas sp. WOUb6", "Stenotrophomonas sp. WOUb1", "Pseudomonas sp. WOUb10", "Mammaliicoccus sp. WOUb9"))
named_bacteria_no_failed$fig_or_no <- as.factor(named_bacteria_no_failed$fig_or_no)
named_bacteria_no_failed$fig_or_no <- factor(named_bacteria_no_failed$fig_or_no, levels=c("Yes","No"))


# Plotting!!
plot_crossfec <- ggplot(named_bacteria_no_failed, aes(x = bacteria_name_2, y = number_worms)) +
  geom_hline(yintercept=mean(named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "OP50",]$number_worms),linetype="dotted") +
  geom_sina(size = 1, scale="width", aes(colour=fig_or_no)) +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="red",position = position_dodge(width = 0.9))  +
  theme_cowplot() +
  xlab("Bacterial strain") +
  ylab("Number of nematodes after one week") +
  scale_y_continuous(limits=c(-5,2000), breaks=c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1,face="italic")) +
  scale_colour_manual(values=c("black","red"),name = "Fig bacteria?") +
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12))

# Calling plot object to view
plot_crossfec


### Stats!!
View(named_bacteria_no_failed)

aov <- aov(number_worms ~ strain_bacteria, data = named_bacteria_no_failed)

summary(aov)

# Df   Sum Sq Mean Sq F value   Pr(>F)    
# strain_bacteria  10  5285732  528573    7.98 2.71e-11 ***
#   Residuals       277 18348510   66240                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tukey.aov<-TukeyHSD(aov)

tukey.aov

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = number_worms ~ strain_bacteria, data = named_bacteria_no_failed)
# 
# $strain_bacteria
# diff          lwr        upr     p adj
# OP50-Klebsiella sp. nov.   -303.482609 -458.0623803 -148.90284 0.0000000
# WOUB1-Klebsiella sp. nov.  -391.818323 -592.0575806 -191.57907 0.0000000
# WOUB10-Klebsiella sp. nov. -405.076726 -642.1912720 -167.96218 0.0000037
# WOUB16-Klebsiella sp. nov. -346.664962 -583.7795073 -109.55042 0.0001739
# WOUB18-Klebsiella sp. nov. -142.622609 -350.1959386   64.95072 0.4854166
# WOUB22-Klebsiella sp. nov. -280.913043 -494.2533966  -67.57269 0.0013125
# WOUB37-Klebsiella sp. nov. -240.211180 -495.2016737   14.77931 0.0851929
# WOUB43-Klebsiella sp. nov. -275.090301 -537.4920419  -12.68856 0.0309711
# WOUB6-Klebsiella sp. nov.  -380.720109 -623.1846503 -138.25557 0.0000341
# WOUB9-Klebsiella sp. nov.  -485.560386 -790.0499925 -181.07078 0.0000233
# WOUB1-OP50                  -88.335714 -271.7694314   95.09800 0.8962330
# WOUB10-OP50                -101.594118 -324.6982997  121.51006 0.9259599
# WOUB16-OP50                 -43.182353 -266.2865350  179.92183 0.9999250
# WOUB18-OP50                 160.860000  -30.5527884  352.27279 0.1930267
# WOUB22-OP50                  22.569565 -175.0823373  220.22147 0.9999995
# WOUB37-OP50                  63.271429 -178.7457611  305.28862 0.9988981
# WOUB43-OP50                  28.392308 -221.4213066  278.20592 0.9999995
# WOUB6-OP50                  -77.237500 -306.0195409  151.54454 0.9910001
# WOUB9-OP50                 -182.077778 -475.7886704  111.63311 0.6406357
# WOUB10-WOUB1                -13.258403 -270.1169842  243.60018 1.0000000
# WOUB16-WOUB1                 45.153361 -211.7052195  302.01194 0.9999693
# WOUB18-WOUB1                249.195714   19.3268168  479.06461 0.0213298
# WOUB22-WOUB1                110.905280 -124.1843397  345.99490 0.9081317
# WOUB37-WOUB1                151.607143 -121.8396977  425.05398 0.7795662
# WOUB43-WOUB1                116.728022 -163.6426191  397.09866 0.9585984
# WOUB6-WOUB1                  11.098214 -250.7072034  272.90363 1.0000000
# WOUB9-WOUB1                 -93.742063 -413.8466448  226.36252 0.9971264
# WOUB16-WOUB10                58.411765 -228.1259780  344.94951 0.9998793
# WOUB18-WOUB10               262.454118   -0.1620614  525.07030 0.0502966
# WOUB22-WOUB10               124.163683 -143.0341399  391.36151 0.9161332
# WOUB37-WOUB10               164.865546 -136.6319199  466.36301 0.7939283
# WOUB43-WOUB10               129.986425 -177.8044839  437.77733 0.9544378
# WOUB6-WOUB10                 24.356618 -266.6238356  315.33707 1.0000000
# WOUB9-WOUB10                -80.483660 -424.8591680  263.89185 0.9995866
# WOUB18-WOUB16               204.042353  -58.5738261  466.65853 0.2964570
# WOUB22-WOUB16                65.751918 -201.4459047  332.94974 0.9993485
# WOUB37-WOUB16               106.453782 -195.0436846  407.95125 0.9873092
# WOUB43-WOUB16                71.574661 -236.2162486  379.36557 0.9996046
# WOUB6-WOUB16                -34.055147 -325.0356003  256.92531 0.9999994
# WOUB9-WOUB16               -138.895425 -483.2709327  205.48008 0.9666751
# WOUB22-WOUB18              -138.290435 -379.6574914  103.07662 0.7428124
# WOUB37-WOUB18               -97.588571 -376.4507266  181.27358 0.9881386
# WOUB43-WOUB18              -132.467692 -418.1224198  153.18704 0.9171772
# WOUB6-WOUB18               -238.097500 -505.5540454   29.35905 0.1317720
# WOUB9-WOUB18               -342.937778 -667.6805528  -18.19500 0.0286251
# WOUB37-WOUB22                40.701863 -242.4792105  323.88294 0.9999954
# WOUB43-WOUB22                 5.822742 -284.0497163  295.69520 1.0000000
# WOUB6-WOUB22                -99.807065 -371.7637211  172.14959 0.9830534
# WOUB9-WOUB22               -204.647343 -533.1063038  123.81162 0.6335034
# WOUB43-WOUB37               -34.879121 -356.6431461  286.88490 0.9999997
# WOUB6-WOUB37               -140.508929 -446.2317904  165.21393 0.9216028
# WOUB9-WOUB37               -245.349206 -602.2684332  111.57002 0.4847001
# WOUB6-WOUB43               -105.629808 -417.5608738  206.30126 0.9907853
# WOUB9-WOUB43               -210.470085 -572.7211811  151.78101 0.7260946
# WOUB9-WOUB6                -104.840278 -452.9210681  243.24051 0.9963786

################################################## END CRUDE FEC ########################################################################

##########################################################################################################################












##########################################################################################################################

############################## Individual Fecundity and end of reproduction ##################################


# Setting working directory 
setwd("/Users/austincolelink/Desktop/MANUSCRIPT/DATA/indfec_endrep")


######### INDIVIDUAL FECUNDITY #########

### Importing inopinata data ###
ino <- read.csv("woub2_ind_fec_ci.csv", stringsAsFactors = TRUE)

# Assigning inopinata data to an object to remove NA's in the "day bagged" column
dat_na_zero_ino <- ino

# Grabbing subset of data with Na's then putting a zero into those spots 
dat_na_zero_ino[is.na(dat_na_zero_ino)] <- 0

# Creating a "total progeny" column and adding up the total progeny for an individual reproductive female
dat_na_zero_ino$total_progeny <- dat_na_zero_ino$progeny_day_0 +
  dat_na_zero_ino$progeny_day_1 +
  dat_na_zero_ino$progeny_day_2 + 
  dat_na_zero_ino$progeny_day_3 + 
  dat_na_zero_ino$progeny_day_4 + 
  dat_na_zero_ino$progeny_day_5 + 
  dat_na_zero_ino$progeny_day_6 + 
  dat_na_zero_ino$progeny_day_7 + 
  dat_na_zero_ino$progeny_day_8

# selecting necessary variables to create a figure with
dat_na_zero_ino <- dat_na_zero_ino %>% 
  select(plate_id, bacterial_strain, species, sp_bac, total_progeny)



### Importing elegans data ###
ele <- read.csv("woub2_ind_fec_ce.csv", stringsAsFactors = TRUE)

# Assigning elegans data to an object to remove NA's in the "day bagged" column
dat_na_zero_ele <- ele

# Grabbing subset of data with Na's then putting a zero into those spots 
dat_na_zero_ele[is.na(dat_na_zero_ele)] <- 0

# Creating a "total progeny" column and adding up the total progeny for an individual reproductive female
dat_na_zero_ele$total_progeny <- dat_na_zero_ele$progeny_day_0 +
  dat_na_zero_ele$progeny_day_1 +
  dat_na_zero_ele$progeny_day_2 + 
  dat_na_zero_ele$progeny_day_3 + 
  dat_na_zero_ele$progeny_day_4 + 
  dat_na_zero_ele$progeny_day_5 + 
  dat_na_zero_ele$progeny_day_6 + 
  dat_na_zero_ele$progeny_day_7 + 
  dat_na_zero_ele$progeny_day_8

# selecting necessary variables to create a figure with
dat_na_zero_ele <- dat_na_zero_ele %>% 
  select(plate_id, bacterial_strain, species, sp_bac, total_progeny)

# binding datasets together 
wormsindfec <- rbind(dat_na_zero_ino, dat_na_zero_ele)

# Plot!!
individualfecundity <- ggplot(wormsindfec, aes(x=factor(species, level=c('C. elegans', 'C. inopinata')), y = total_progeny, group = sp_bac, color = bacterial_strain)) +
  geom_sina(size=1.25, alpha=1,scale="width") +
  stat_summary(aes(group=sp_bac),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  theme_cowplot() +
  labs(fill = "Bacterial Strains") +
  guides(color = guide_legend(title = "Bacterial Strains")) +
  xlab("Nematode Species") +
  ylab("Total progeny") +
  scale_y_continuous(limits=c(-5,850), breaks=c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900)) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  theme(axis.text.x=element_text(face="italic"), legend.text = element_text(face="italic"))
  
# Calling plot object to view it 
individualfecundity

Dimensions
5x5

### Stats!!
View(wormsindfec)

aov <- aov(total_progeny ~ bacterial_strain*species, data = wormsindfec)

summary(aov)

# Df  Sum Sq Mean Sq F value   Pr(>F)    
# bacterial_strain           1  242026  242026   19.63  2.3e-05 ***
#   species                    1 1549363 1549363  125.67  < 2e-16 ***
#   bacterial_strain:species   1  145662  145662   11.81 0.000841 ***
#   Residuals                106 1306818   12328                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tukey.aov<-TukeyHSD(aov)

tukey.aov

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = total_progeny ~ bacterial_strain * species, data = wormsindfec)
# 
# $bacterial_strain
# diff      lwr     upr   p adj
# Klebsiella sp. nov.-E. coli 94.06248 51.97292 136.152 2.3e-05
# 
# $species
# diff      lwr      upr p adj
# C. elegans-C. inopinata 237.5896 195.5489 279.6303     0
# 
# $`bacterial_strain:species`
# diff       lwr      upr     p adj
# Klebsiella sp. nov.:C. inopinata-E. coli:C. inopinata            32.35577 -44.16692 108.8785 0.6880625
# E. coli:C. elegans-E. coli:C. inopinata                         159.67077  78.48754 240.8540 0.0000077
# Klebsiella sp. nov.:C. elegans-E. coli:C. inopinata             338.23077 258.59516 417.8664 0.0000000
# E. coli:C. elegans-Klebsiella sp. nov.:C. inopinata             127.31500  49.95253 204.6775 0.0002242
# Klebsiella sp. nov.:C. elegans-Klebsiella sp. nov.:C. inopinata 305.87500 230.13818 381.6118 0.0000000
# Klebsiella sp. nov.:C. elegans-E. coli:C. elegans               178.56000  98.11710 259.0029 0.0000004


######### END OF REPRODUCTION #########

# Binding original, imported dataframes into one
worms <- rbind(ele, ino)

# Plot!!
endrep <- ggplot(worms, aes(x = species, y = end_rep, group = sp_bac, color = bacterial_strain)) + 
  geom_sina(size=1.25, alpha=1,scale="width") +
  stat_summary(aes(group=sp_bac),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0,10,1)) +
  labs(fill = "Bacterial Strains") +
  guides(color = guide_legend(title = "Bacterial Strains")) +
  xlab("Nematode species") +
  ylab("End of reproduction (day)") +
  theme_cowplot() +
  theme(axis.text.x=element_text(face="italic")) + 
  theme(axis.text.x=element_text(face="italic"), legend.text = element_text(face="italic")) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

# Calling plot object to view it 
endrep

Dimensions
5x5

### Stats!!
View(worms)

aov <- aov(end_rep ~ bacterial_strain*species, data = worms)

summary(aov)

# Df Sum Sq Mean Sq F value Pr(>F)  
# bacterial_strain           1  16.29  16.290   6.757 0.0107 *
#   species                    1   0.35   0.352   0.146 0.7031  
# bacterial_strain:species   1   1.25   1.248   0.518 0.4735  
# Residuals                106 255.53   2.411                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tukey.aov<-TukeyHSD(aov)

tukey.aov

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = end_rep ~ bacterial_strain * species, data = worms)
# 
# $bacterial_strain
# diff       lwr     upr     p adj
# Klebsiella sp. nov.-E. coli 0.7716849 0.1831303 1.36024 0.0106667
# 
# $species
# diff        lwr       upr     p adj
# C. inopinata-C. elegans -0.1132709 -0.7011422 0.4746003 0.7032213
# 
# $`bacterial_strain:species`
# diff        lwr       upr     p adj
# Klebsiella sp. nov.:C. elegans-E. coli:C. elegans                1.0000000 -0.1248641 2.1248641 0.0998307
# E. coli:C. inopinata-E. coli:C. elegans                          0.1153846 -1.0198319 1.2506011 0.9934254
# Klebsiella sp. nov.:C. inopinata-E. coli:C. elegans              0.6875000 -0.3942894 1.7692894 0.3506054
# E. coli:C. inopinata-Klebsiella sp. nov.:C. elegans             -0.8846154 -1.9981909 0.2289601 0.1684881
# Klebsiella sp. nov.:C. inopinata-Klebsiella sp. nov.:C. elegans -0.3125000 -1.3715572 0.7465572 0.8677106
# Klebsiella sp. nov.:C. inopinata-E. coli:C. inopinata            0.5721154 -0.4979310 1.6421618 0.5048909




################################################## END Individual Fecundity and end of reproduction ########################################################################


##########################################################################################################################













##########################################################################################################################

############################## Viability and Maturation/Dev. Timing ##################################

# setting working directory
setwd("/Users/austincolelink/Desktop/MANUSCRIPT/DATA/devtiming_viability")


### Viability ### 

# importing data
ci <- read.csv("ci.csv", stringsAsFactors = TRUE)
ce <- read.csv("ce.csv", stringsAsFactors = TRUE)




# Getting the fraction of surviving worms from the initial egg-lay and selecting variables necessary for plotting
via_ci <- ci %>%
  mutate(frac_via = adult_day_8/embryos_day_0) %>% 
  select(plate_number, bacterial_strain, sp_bac, frac_via, species)
via_ce <- ce %>%
  mutate(frac_via = adult_day_6/embryos_day_0) %>% 
  select(plate_number, bacterial_strain, sp_bac, frac_via, species)

# binding elegans and inopinata data together to plot together 
via_worms <- rbind(via_ce, via_ci)


# Plot!!!
viability <- ggplot(via_worms, aes(x = species, y = frac_via, group = sp_bac, color = bacterial_strain)) + 
  geom_sina() +
  stat_summary(aes(group=sp_bac),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 1.1), breaks = seq(0,1.1,.1)) +
  labs(fill = "Bacterial Strains") +
  guides(color = guide_legend(title = "Bacterial Strains")) +
  xlab("Nematode species") +
  ylab("Fraction of surviving adults") +
  theme_cowplot() +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12,face="italic"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  theme(legend.text = element_text(face="italic"))

# Calling plot object to view it 
viability

dimensions
5x5

### Stats!!
View(via_worms)

aov <- aov(frac_via ~ bacterial_strain*species, data = via_worms)

summary(aov)

# Df Sum Sq Mean Sq F value   Pr(>F)    
# bacterial_strain          1 0.3478  0.3478  15.691 0.000183 ***
#   species                   1 0.0980  0.0980   4.422 0.039244 *  
#   bacterial_strain:species  1 0.2712  0.2712  12.236 0.000837 ***
#   Residuals                67 1.4850  0.0222                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tukey.aov<-TukeyHSD(aov)

tukey.aov

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = frac_via ~ bacterial_strain * species, data = via_worms)
# 
# $bacterial_strain
# diff        lwr        upr     p adj
# Klebsiella sp. nov.-E. coli  -0.1399885 -0.2105279 -0.0694491 0.0001833
# 
# $species
# diff         lwr       upr     p adj
# C. inopinata-C. elegans 0.07371171 0.003172294 0.1442511 0.0408105
# 
# $`bacterial_strain:species`
# diff         lwr         upr     p adj
# Klebsiella sp. nov.:C. elegans-E. coli :C. elegans              -0.27188650 -0.40344930 -0.14032371 0.0000047
# E. coli :C. inopinata-E. coli :C. elegans                       -0.05199734 -0.18597433  0.08197966 0.7368792
# Klebsiella sp. nov.:C. inopinata-E. coli :C. elegans            -0.07458235 -0.19862094  0.04945625 0.3944687
# E. coli :C. inopinata-Klebsiella sp. nov.:C. elegans             0.21988917  0.07891742  0.36086091 0.0006256
# Klebsiella sp. nov.:C. inopinata-Klebsiella sp. nov.:C. elegans  0.19730416  0.06574136  0.32886695 0.0010657
# Klebsiella sp. nov.:C. inopinata-E. coli :C. inopinata          -0.02258501 -0.15656200  0.11139199 0.9705259


View(via_ci)
WOUb2via_ci <- via_ci[via_ci$bacterial_strain == "Klebsiella sp. WOUb2",]$frac_via
OP50via_ci <- via_ci[via_ci$bacterial_strain == "E. coli OP50",]$frac_via
cohen.d(WOUb2via_ci,OP50via_ci)
# Cohen's d
# 
# d estimate: -0.1539932 (negligible)
# 95 percent confidence interval:
#      lower      upper 
# -0.8499207  0.5419342 

View(via_ce)
WOUb2via_ce <- via_ce[via_ce$bacterial_strain == "Klebsiella sp. WOUb2",]$frac_via
OP50via_ce <- via_ce[via_ce$bacterial_strain == "E. coli OP50",]$frac_via
cohen.d(WOUb2via_ce,OP50via_ce)
# Cohen's d
# 
# d estimate: -1.800643 (large)
# 95 percent confidence interval:
#      lower      upper 
# -2.6072476 -0.9940389 











### Maturation ### 


##### Establishing custom function ##########
func_log_inf_conf <- function(dat,x,y){

	f <- function (x) 1/(1+exp(-x))
	the_glm <- glm(y ~ x, family="binomial", data=dat)
	conf <- confint(the_glm)
	#the inflection point

	p <- 0.5
	q <- (log(p/(1-p)) - coef(the_glm)[1]) / coef(the_glm)[2]

	# confidence interval at inflection point
	upper_q <- (log(p/(1-p)) - conf[1,1]) / conf[2,1]
	lower_q <- (log(p/(1-p)) - conf[1,2]) / conf[2,2]

	#return
	ret <- c(q, lower_q, upper_q)
	return(ret)
}


# importing data
ci <- read.csv("ci.csv", stringsAsFactors = TRUE)
ce <- read.csv("ce.csv", stringsAsFactors = TRUE)














############################### Cino #############################

# making plate number into a factor
ci$plate_number <- as.factor(ci$plate_number)

# convert wide data to long data
dat_melt_ci <- melt(ci,id.vars = c("plate_number","bacterial_strain","date_of_egglay", "species", "sp_bac"))

# making a day column
dat_melt_ci$day <- 0

# correcting the day column for the long data, "day 0" was the day of the egg lay 
dat_melt_ci[dat_melt_ci$variable == "embryos_day_0",]$day <- 0
dat_melt_ci[dat_melt_ci$variable == "embryos_day_1",]$day <- 1
dat_melt_ci[dat_melt_ci$variable == "embryos_day_2",]$day <- 2
dat_melt_ci[dat_melt_ci$variable == "embryos_day_3",]$day <- 3
dat_melt_ci[dat_melt_ci$variable == "embryos_day_4",]$day <- 4
dat_melt_ci[dat_melt_ci$variable == "embryos_day_5",]$day <- 5
dat_melt_ci[dat_melt_ci$variable == "embryos_day_6",]$day <- 6
dat_melt_ci[dat_melt_ci$variable == "embryos_day_7",]$day <- 7
dat_melt_ci[dat_melt_ci$variable == "embryos_day_8",]$day <- 8
dat_melt_ci[dat_melt_ci$variable == "larvae_day_0",]$day <- 0
dat_melt_ci[dat_melt_ci$variable == "larvae_day_1",]$day <- 1
dat_melt_ci[dat_melt_ci$variable == "larvae_day_2",]$day <- 2
dat_melt_ci[dat_melt_ci$variable == "larvae_day_3",]$day <- 3
dat_melt_ci[dat_melt_ci$variable == "larvae_day_4",]$day <- 4
dat_melt_ci[dat_melt_ci$variable == "larvae_day_5",]$day <- 5
dat_melt_ci[dat_melt_ci$variable == "larvae_day_6",]$day <- 6
dat_melt_ci[dat_melt_ci$variable == "larvae_day_7",]$day <- 7
dat_melt_ci[dat_melt_ci$variable == "larvae_day_8",]$day <- 8
dat_melt_ci[dat_melt_ci$variable == "L4_day_0",]$day <- 0
dat_melt_ci[dat_melt_ci$variable == "L4_day_1",]$day <- 1
dat_melt_ci[dat_melt_ci$variable == "L4_day_2",]$day <- 2
dat_melt_ci[dat_melt_ci$variable == "L4_day_3",]$day <- 3
dat_melt_ci[dat_melt_ci$variable == "L4_day_4",]$day <- 4
dat_melt_ci[dat_melt_ci$variable == "L4_day_5",]$day <- 5
dat_melt_ci[dat_melt_ci$variable == "L4_day_6",]$day <- 6
dat_melt_ci[dat_melt_ci$variable == "L4_day_7",]$day <- 7
dat_melt_ci[dat_melt_ci$variable == "L4_day_8",]$day <- 8
dat_melt_ci[dat_melt_ci$variable == "adult_day_0",]$day <- 0
dat_melt_ci[dat_melt_ci$variable == "adult_day_1",]$day <- 1
dat_melt_ci[dat_melt_ci$variable == "adult_day_2",]$day <- 2
dat_melt_ci[dat_melt_ci$variable == "adult_day_3",]$day <- 3
dat_melt_ci[dat_melt_ci$variable == "adult_day_4",]$day <- 4
dat_melt_ci[dat_melt_ci$variable == "adult_day_5",]$day <- 5
dat_melt_ci[dat_melt_ci$variable == "adult_day_6",]$day <- 6
dat_melt_ci[dat_melt_ci$variable == "adult_day_7",]$day <- 7
dat_melt_ci[dat_melt_ci$variable == "adult_day_8",]$day <- 8
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_0",]$day <- 0
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_1",]$day <- 1
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_2",]$day <- 2
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_3",]$day <- 3
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_4",]$day <- 4
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_5",]$day <- 5
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_6",]$day <- 6
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_7",]$day <- 7
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_8",]$day <- 8


# make stage column and then filling it with stages
dat_melt_ci$stage <- "embryo"
dat_melt_ci$stage <- factor(dat_melt_ci$stage, levels=c("embryo", "larvae", "L4","Adult","Gravid Female"))


# correcting stages for each line
dat_melt_ci[dat_melt_ci$variable == "embryos_day_0",]$stage <- "embryo"
dat_melt_ci[dat_melt_ci$variable == "embryos_day_1",]$stage <- "embryo"
dat_melt_ci[dat_melt_ci$variable == "embryos_day_2",]$stage <- "embryo"
dat_melt_ci[dat_melt_ci$variable == "embryos_day_3",]$stage <- "embryo"
dat_melt_ci[dat_melt_ci$variable == "embryos_day_4",]$stage <- "embryo"
dat_melt_ci[dat_melt_ci$variable == "embryos_day_5",]$stage <- "embryo"
dat_melt_ci[dat_melt_ci$variable == "embryos_day_6",]$stage <- "embryo"
dat_melt_ci[dat_melt_ci$variable == "embryos_day_7",]$stage <- "embryo"
dat_melt_ci[dat_melt_ci$variable == "embryos_day_8",]$stage <- "embryo"
dat_melt_ci[dat_melt_ci$variable == "larvae_day_0",]$stage <- "larvae"
dat_melt_ci[dat_melt_ci$variable == "larvae_day_1",]$stage <- "larvae"
dat_melt_ci[dat_melt_ci$variable == "larvae_day_2",]$stage <- "larvae"
dat_melt_ci[dat_melt_ci$variable == "larvae_day_3",]$stage <- "larvae"
dat_melt_ci[dat_melt_ci$variable == "larvae_day_4",]$stage <- "larvae"
dat_melt_ci[dat_melt_ci$variable == "larvae_day_5",]$stage <- "larvae"
dat_melt_ci[dat_melt_ci$variable == "larvae_day_6",]$stage <- "larvae"
dat_melt_ci[dat_melt_ci$variable == "larvae_day_7",]$stage <- "larvae"
dat_melt_ci[dat_melt_ci$variable == "larvae_day_8",]$stage <- "larvae"
dat_melt_ci[dat_melt_ci$variable == "L4_day_0",]$stage <- "L4"
dat_melt_ci[dat_melt_ci$variable == "L4_day_1",]$stage <- "L4"
dat_melt_ci[dat_melt_ci$variable == "L4_day_2",]$stage <- "L4"
dat_melt_ci[dat_melt_ci$variable == "L4_day_3",]$stage <- "L4"
dat_melt_ci[dat_melt_ci$variable == "L4_day_4",]$stage <- "L4"
dat_melt_ci[dat_melt_ci$variable == "L4_day_5",]$stage <- "L4"
dat_melt_ci[dat_melt_ci$variable == "L4_day_6",]$stage <- "L4"
dat_melt_ci[dat_melt_ci$variable == "L4_day_7",]$stage <- "L4"
dat_melt_ci[dat_melt_ci$variable == "L4_day_8",]$stage <- "L4"
dat_melt_ci[dat_melt_ci$variable == "adult_day_0",]$stage <- "Adult"
dat_melt_ci[dat_melt_ci$variable == "adult_day_1",]$stage <- "Adult"
dat_melt_ci[dat_melt_ci$variable == "adult_day_2",]$stage <- "Adult"
dat_melt_ci[dat_melt_ci$variable == "adult_day_3",]$stage <- "Adult"
dat_melt_ci[dat_melt_ci$variable == "adult_day_4",]$stage <- "Adult"
dat_melt_ci[dat_melt_ci$variable == "adult_day_5",]$stage <- "Adult"
dat_melt_ci[dat_melt_ci$variable == "adult_day_6",]$stage <- "Adult"
dat_melt_ci[dat_melt_ci$variable == "adult_day_7",]$stage <- "Adult"
dat_melt_ci[dat_melt_ci$variable == "adult_day_8",]$stage <- "Adult"
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_0",]$stage <- "Gravid Female"
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_1",]$stage <- "Gravid Female"
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_2",]$stage <- "Gravid Female"
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_3",]$stage <- "Gravid Female"
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_4",]$stage <- "Gravid Female"
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_5",]$stage <- "Gravid Female"
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_6",]$stage <- "Gravid Female"
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_7",]$stage <- "Gravid Female"
dat_melt_ci[dat_melt_ci$variable == "gravid_females_day_8",]$stage <- "Gravid Female"


# rename value to be "number of worms"
colnames(dat_melt_ci)[colnames(dat_melt_ci) == 'value'] <- 'number_of_worms'


# Assigning data to a new object to preserve previous object 
filt_dat_ci <- dat_melt_ci


#dcast(ID + seq ~ condition, data = DF, value.var = "Value")
filt_dat_ci <- subset(filt_dat_ci, select = -variable)


#put underscore between "Gravid Female"
levels(filt_dat_ci$stage)[levels(filt_dat_ci$stage)=="Gravid Female"] <- "Gravid_Female"


filt_dat_cast_ci <- dcast(filt_dat_ci, ...~stage,value.var="number_of_worms")


#copy to new variable
datb_no_NA_ci <- filt_dat_cast_ci


#fill NA's with zero
datb_no_NA_ci[is.na(datb_no_NA_ci)] <- 0


##add L4 or older column


datb_no_NA_ci$L4_older <- datb_no_NA_ci$L4 + datb_no_NA_ci$Adult


##add all worms per plate
datb_no_NA_ci$total_worms_observed <- datb_no_NA_ci$L4 + datb_no_NA_ci$Adult + datb_no_NA_ci$embryo + datb_no_NA_ci$larvae


##fraction at each stage
datb_no_NA_ci$fra_L1_L3 <- datb_no_NA_ci$larvae/datb_no_NA_ci$total_worms_observed
datb_no_NA_ci$fra_L4_older <- datb_no_NA_ci$L4_older/datb_no_NA_ci$total_worms_observed
datb_no_NA_ci$fra_adult <- datb_no_NA_ci$Adult/datb_no_NA_ci$total_worms_observed


##make variables to put the df's in
ad_coeff_df <- NULL
L4_coeff_df <- NULL


#for each plate, get the logistic model inflection point (median hour) for the timing of the developmental milestone (L4 or adult onset)
for (i in levels(datb_no_NA_ci$plate_number)){
	the_plate <- datb_no_NA_ci[datb_no_NA_ci$plate_number == i,]
	
	#get minimum number of worms observed
	min_worms_plate <- min(the_plate$total_worms_observed)
	
	#normalize to minimum number of worms observed
	
	the_plate$L1_L3_norm <- round(the_plate$fra_L1_L3*min_worms_plate)
	the_plate$L4_older_norm <- round(the_plate$fra_L4_older*min_worms_plate)
	the_plate$total_adult_norm <- round(the_plate$fra_adult*min_worms_plate)
	
	#get the worms not at milestone
	the_plate$not_L4 <- min_worms_plate-the_plate$L4_older_norm
	the_plate$not_adult <- min_worms_plate-the_plate$total_adult_norm
	#na's are 0
	the_plate[is.na(the_plate)] <- 0
	#expand the number of rows according to number of worms at milestone-- each worm gets a row per time observed
	dev_data_expand_at_L4 <- the_plate[rep(1:nrow(the_plate), the_plate$L4_older_norm),]
	dev_data_expand_at_adult <- the_plate[rep(1:nrow(the_plate), the_plate$total_adult_norm),]
	
	#expand the number of rows according to number of worms NOT at milestone-- each worm gets a row per time observed
	
	dev_data_expand_NOT_at_L4 <- the_plate[rep(1:nrow(the_plate), the_plate$not_L4),]
	dev_data_expand_NOT_at_adult <- the_plate[rep(1:nrow(the_plate), the_plate$not_adult),]
	
	
	#add column of ones to hatched worms
	dev_data_expand_at_L4$milestone_status <- rep(1,nrow(dev_data_expand_at_L4))
	dev_data_expand_at_adult$milestone_status <- rep(1,nrow(dev_data_expand_at_adult))
	
	#add column of zeros to not hatched worms
	dev_data_expand_NOT_at_L4$milestone_status <- rep(0,nrow(dev_data_expand_NOT_at_L4))
	dev_data_expand_NOT_at_adult$milestone_status <- rep(0,nrow(dev_data_expand_NOT_at_adult))
	
	
	L4_data_expand <- rbind(dev_data_expand_at_L4, dev_data_expand_NOT_at_L4)
	Adult_data_expand <- rbind(dev_data_expand_at_adult, dev_data_expand_NOT_at_adult)
		
	L4_coeff <- func_log_inf_conf(L4_data_expand,L4_data_expand$day,L4_data_expand$milestone_status)
	
	Adult_coeff <- func_log_inf_conf(Adult_data_expand,Adult_data_expand$day,Adult_data_expand$milestone_status)
	
	L4_df_to_add <- data.frame(bacterial_strain=unique(as.character(L4_data_expand$bacterial_strain)), plate_number=unique(as.character(L4_data_expand$plate_number)), Milestone= "L4", midpoint=L4_coeff[1],midpoint_95_CI_low=L4_coeff[2],midpoint_95_CI_high=L4_coeff[3])

	Adult_df_to_add <- data.frame(bacterial_strain=unique(as.character(Adult_data_expand$bacterial_strain)), plate_number=unique(as.character(Adult_data_expand$plate_number)), Milestone= "Adult", midpoint=Adult_coeff[1],midpoint_95_CI_low=Adult_coeff[2],midpoint_95_CI_high=Adult_coeff[3])

	L4_coeff_df <- rbind(L4_coeff_df,L4_df_to_add)

	ad_coeff_df <- rbind(ad_coeff_df,Adult_df_to_add)
}

inopinata_df <- rbind(L4_coeff_df,ad_coeff_df)

inopinata_df$species <- "C. inopinata"












################################ Cele ###############################

# making plate number into a factor
ce$plate_number <- as.factor(ce$plate_number)

# convert wide data to long data
dat_melt_ce <- melt(ce,id.vars = c("plate_number","bacterial_strain","date_of_egglay", "species", "sp_bac"))

# making a day column
dat_melt_ce$day <- 0

# correcting the day column for the long data, "day 0" was the day of the egg lay 
dat_melt_ce[dat_melt_ce$variable == "embryos_day_0",]$day <- 0
dat_melt_ce[dat_melt_ce$variable == "embryos_day_1",]$day <- 1
dat_melt_ce[dat_melt_ce$variable == "embryos_day_2",]$day <- 2
dat_melt_ce[dat_melt_ce$variable == "embryos_day_3",]$day <- 3
dat_melt_ce[dat_melt_ce$variable == "embryos_day_4",]$day <- 4
dat_melt_ce[dat_melt_ce$variable == "embryos_day_5",]$day <- 5
dat_melt_ce[dat_melt_ce$variable == "embryos_day_6",]$day <- 6
dat_melt_ce[dat_melt_ce$variable == "larvae_day_0",]$day <- 0
dat_melt_ce[dat_melt_ce$variable == "larvae_day_1",]$day <- 1
dat_melt_ce[dat_melt_ce$variable == "larvae_day_2",]$day <- 2
dat_melt_ce[dat_melt_ce$variable == "larvae_day_3",]$day <- 3
dat_melt_ce[dat_melt_ce$variable == "larvae_day_4",]$day <- 4
dat_melt_ce[dat_melt_ce$variable == "larvae_day_5",]$day <- 5
dat_melt_ce[dat_melt_ce$variable == "larvae_day_6",]$day <- 6
dat_melt_ce[dat_melt_ce$variable == "L4_day_0",]$day <- 0
dat_melt_ce[dat_melt_ce$variable == "L4_day_1",]$day <- 1
dat_melt_ce[dat_melt_ce$variable == "L4_day_2",]$day <- 2
dat_melt_ce[dat_melt_ce$variable == "L4_day_3",]$day <- 3
dat_melt_ce[dat_melt_ce$variable == "L4_day_4",]$day <- 4
dat_melt_ce[dat_melt_ce$variable == "L4_day_5",]$day <- 5
dat_melt_ce[dat_melt_ce$variable == "L4_day_6",]$day <- 6
dat_melt_ce[dat_melt_ce$variable == "adult_day_0",]$day <- 0
dat_melt_ce[dat_melt_ce$variable == "adult_day_1",]$day <- 1
dat_melt_ce[dat_melt_ce$variable == "adult_day_2",]$day <- 2
dat_melt_ce[dat_melt_ce$variable == "adult_day_3",]$day <- 3
dat_melt_ce[dat_melt_ce$variable == "adult_day_4",]$day <- 4
dat_melt_ce[dat_melt_ce$variable == "adult_day_5",]$day <- 5
dat_melt_ce[dat_melt_ce$variable == "adult_day_6",]$day <- 6
dat_melt_ce[dat_melt_ce$variable == "gravid_females_day_0",]$day <- 0
dat_melt_ce[dat_melt_ce$variable == "gravid_females_day_1",]$day <- 1
dat_melt_ce[dat_melt_ce$variable == "gravid_females_day_2",]$day <- 2
dat_melt_ce[dat_melt_ce$variable == "gravid_females_day_3",]$day <- 3
dat_melt_ce[dat_melt_ce$variable == "gravid_females_day_4",]$day <- 4
dat_melt_ce[dat_melt_ce$variable == "gravid_females_day_5",]$day <- 5
dat_melt_ce[dat_melt_ce$variable == "gravid_females_day_6",]$day <- 6






# make stage column and then filling it with stages
dat_melt_ce$stage <- "embryo"
dat_melt_ce$stage <- factor(dat_melt_ce$stage, levels=c("embryo", "larvae", "L4","Adult","Gravid Female"))


# correcting stages for each line
dat_melt_ce[dat_melt_ce$variable == "embryos_day_0",]$stage <- "embryo"
dat_melt_ce[dat_melt_ce$variable == "embryos_day_1",]$stage <- "embryo"
dat_melt_ce[dat_melt_ce$variable == "embryos_day_2",]$stage <- "embryo"
dat_melt_ce[dat_melt_ce$variable == "embryos_day_3",]$stage <- "embryo"
dat_melt_ce[dat_melt_ce$variable == "embryos_day_4",]$stage <- "embryo"
dat_melt_ce[dat_melt_ce$variable == "embryos_day_5",]$stage <- "embryo"
dat_melt_ce[dat_melt_ce$variable == "embryos_day_6",]$stage <- "embryo"
dat_melt_ce[dat_melt_ce$variable == "larvae_day_0",]$stage <- "larvae"
dat_melt_ce[dat_melt_ce$variable == "larvae_day_1",]$stage <- "larvae"
dat_melt_ce[dat_melt_ce$variable == "larvae_day_2",]$stage <- "larvae"
dat_melt_ce[dat_melt_ce$variable == "larvae_day_3",]$stage <- "larvae"
dat_melt_ce[dat_melt_ce$variable == "larvae_day_4",]$stage <- "larvae"
dat_melt_ce[dat_melt_ce$variable == "larvae_day_5",]$stage <- "larvae"
dat_melt_ce[dat_melt_ce$variable == "larvae_day_6",]$stage <- "larvae"
dat_melt_ce[dat_melt_ce$variable == "L4_day_0",]$stage <- "L4"
dat_melt_ce[dat_melt_ce$variable == "L4_day_1",]$stage <- "L4"
dat_melt_ce[dat_melt_ce$variable == "L4_day_2",]$stage <- "L4"
dat_melt_ce[dat_melt_ce$variable == "L4_day_3",]$stage <- "L4"
dat_melt_ce[dat_melt_ce$variable == "L4_day_4",]$stage <- "L4"
dat_melt_ce[dat_melt_ce$variable == "L4_day_5",]$stage <- "L4"
dat_melt_ce[dat_melt_ce$variable == "L4_day_6",]$stage <- "L4"
dat_melt_ce[dat_melt_ce$variable == "adult_day_0",]$stage <- "Adult"
dat_melt_ce[dat_melt_ce$variable == "adult_day_1",]$stage <- "Adult"
dat_melt_ce[dat_melt_ce$variable == "adult_day_2",]$stage <- "Adult"
dat_melt_ce[dat_melt_ce$variable == "adult_day_3",]$stage <- "Adult"
dat_melt_ce[dat_melt_ce$variable == "adult_day_4",]$stage <- "Adult"
dat_melt_ce[dat_melt_ce$variable == "adult_day_5",]$stage <- "Adult"
dat_melt_ce[dat_melt_ce$variable == "adult_day_6",]$stage <- "Adult"
dat_melt_ce[dat_melt_ce$variable == "gravid_females_day_0",]$stage <- "Gravid Female"
dat_melt_ce[dat_melt_ce$variable == "gravid_females_day_1",]$stage <- "Gravid Female"
dat_melt_ce[dat_melt_ce$variable == "gravid_females_day_2",]$stage <- "Gravid Female"
dat_melt_ce[dat_melt_ce$variable == "gravid_females_day_3",]$stage <- "Gravid Female"
dat_melt_ce[dat_melt_ce$variable == "gravid_females_day_4",]$stage <- "Gravid Female"
dat_melt_ce[dat_melt_ce$variable == "gravid_females_day_5",]$stage <- "Gravid Female"
dat_melt_ce[dat_melt_ce$variable == "gravid_females_day_6",]$stage <- "Gravid Female"


# rename value to be "number of worms"
colnames(dat_melt_ce)[colnames(dat_melt_ce) == 'value'] <- 'number_of_worms'

# Assigning data to a new object to preserve previous object 
filt_dat_ce <- dat_melt_ce


#dcast(ID + seq ~ condition, data = DF, value.var = "Value")
filt_dat_ce <- subset(filt_dat_ce, select = -variable)


filt_dat_cast_ce <- dcast(filt_dat_ce, ...~stage,value.var="number_of_worms")


#copy to new variable
datb_no_NA_ce <- filt_dat_cast_ce


#fill NA's with zero
datb_no_NA_ce[is.na(datb_no_NA_ce)] <- 0


##add L4 or older column
datb_no_NA_ce$L4_older <- datb_no_NA_ce$L4 + datb_no_NA_ce$Adult


##add all worms per plate
datb_no_NA_ce$total_worms_observed <- datb_no_NA_ce$L4 + datb_no_NA_ce$Adult + datb_no_NA_ce$embryo + datb_no_NA_ce$larvae

##fraction at each stage
datb_no_NA_ce$fra_L1_L3 <- datb_no_NA_ce$larvae/datb_no_NA_ce$total_worms_observed
datb_no_NA_ce$fra_L4_older <- datb_no_NA_ce$L4_older/datb_no_NA_ce$total_worms_observed
datb_no_NA_ce$fra_adult <- datb_no_NA_ce$Adult/datb_no_NA_ce$total_worms_observed


##make variables to put the df's in
ad_coeff_df <- NULL
L4_coeff_df <- NULL


for (i in levels(datb_no_NA_ce$plate_number)){
	the_plate <- datb_no_NA_ce[datb_no_NA_ce$plate_number == i,]
	
	#get minimum number of worms observed
	min_worms_plate <- min(the_plate$total_worms_observed)
	
	#normalize to minimum number of worms observed
	
	the_plate$L1_L3_norm <- round(the_plate$fra_L1_L3*min_worms_plate)
	the_plate$L4_older_norm <- round(the_plate$fra_L4_older*min_worms_plate)
	the_plate$total_adult_norm <- round(the_plate$fra_adult*min_worms_plate)
	
	#get the worms not at milestone
	the_plate$not_L4 <- min_worms_plate-the_plate$L4_older_norm
	the_plate$not_adult <- min_worms_plate-the_plate$total_adult_norm
	#na's are 0
	the_plate[is.na(the_plate)] <- 0
	#expand the number of rows according to number of worms at milestone-- each worm gets a row per time observed
	dev_data_expand_at_L4 <- the_plate[rep(1:nrow(the_plate), the_plate$L4_older_norm),]
	dev_data_expand_at_adult <- the_plate[rep(1:nrow(the_plate), the_plate$total_adult_norm),]
	
	#expand the number of rows according to number of worms NOT at milestone-- each worm gets a row per time observed
	
	dev_data_expand_NOT_at_L4 <- the_plate[rep(1:nrow(the_plate), the_plate$not_L4),]
	dev_data_expand_NOT_at_adult <- the_plate[rep(1:nrow(the_plate), the_plate$not_adult),]
	
	
	#add column of ones to hatched worms
	dev_data_expand_at_L4$milestone_status <- rep(1,nrow(dev_data_expand_at_L4))
	dev_data_expand_at_adult$milestone_status <- rep(1,nrow(dev_data_expand_at_adult))
	
	#add column of zeros to not hatched worms
	dev_data_expand_NOT_at_L4$milestone_status <- rep(0,nrow(dev_data_expand_NOT_at_L4))
	dev_data_expand_NOT_at_adult$milestone_status <- rep(0,nrow(dev_data_expand_NOT_at_adult))
	
	
	L4_data_expand <- rbind(dev_data_expand_at_L4, dev_data_expand_NOT_at_L4)
	Adult_data_expand <- rbind(dev_data_expand_at_adult, dev_data_expand_NOT_at_adult)
		
	L4_coeff <- func_log_inf_conf(L4_data_expand,L4_data_expand$day,L4_data_expand$milestone_status)
	
	Adult_coeff <- func_log_inf_conf(Adult_data_expand,Adult_data_expand$day,Adult_data_expand$milestone_status)
	
	L4_df_to_add <- data.frame(bacterial_strain=unique(as.character(L4_data_expand$bacterial_strain)), plate_number=unique(as.character(L4_data_expand$plate_number)), Milestone= "L4", midpoint=L4_coeff[1],midpoint_95_CI_low=L4_coeff[2],midpoint_95_CI_high=L4_coeff[3])

	Adult_df_to_add <- data.frame(bacterial_strain=unique(as.character(Adult_data_expand$bacterial_strain)), plate_number=unique(as.character(Adult_data_expand$plate_number)), Milestone= "Adult", midpoint=Adult_coeff[1],midpoint_95_CI_low=Adult_coeff[2],midpoint_95_CI_high=Adult_coeff[3])

	L4_coeff_df <- rbind(L4_coeff_df,L4_df_to_add)

	ad_coeff_df <- rbind(ad_coeff_df,Adult_df_to_add)
}


elegans_df <- rbind(L4_coeff_df,ad_coeff_df)

elegans_df$species <- "C. elegans"











# Combining both the elegans and inopinata dataframes into one to plot 
big_df <- rbind(inopinata_df,elegans_df)
big_df <- big_df %>% 
  mutate(day = midpoint)
big_df$Milestone <- factor(big_df$Milestone, levels=c("L4", "Adult"))

# Pull out the adult/maturation milestone only
adult_df <- big_df %>% 
  filter(Milestone == "Adult")


maturation <- ggplot(adult_df, aes(x=species,y=midpoint)) +
  geom_sina(aes(colour=bacterial_strain),size=1.5,scale="width") +
  stat_summary(aes(group=bacterial_strain),fun = mean, fun.min = mean, fun.max = mean, geom = "crossbar", width = 0.5, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits=c(0,6),breaks=seq(from=0,to=6,by=1)) +
  labs(fill = "Bacterial Strains") +
  guides(color = guide_legend(title = "Bacterial Strains")) +
  xlab("Nematode species") +
  ylab("Maturation Time (days)") +
  theme_cowplot() +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12,face="italic"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  theme(legend.text = element_text(face="italic"))

# Calling plot object to view it
maturation


### Stats!!
View(adult_df)

aov <- aov(midpoint ~ bacterial_strain*species, data = adult_df)

summary(aov)

# Df Sum Sq Mean Sq F value  Pr(>F)    
# bacterial_strain          1  0.019   0.019   0.488   0.487    
# species                   1 29.594  29.594 744.345 < 2e-16 ***
#   bacterial_strain:species  1  0.948   0.948  23.840 6.8e-06 ***
#   Residuals                67  2.664   0.040                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tukey.aov<-TukeyHSD(aov)

tukey.aov

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = midpoint ~ bacterial_strain * species, data = adult_df)
# 
# $bacterial_strain
# diff         lwr      upr     p adj
# Klebsiella sp. nov.-E. coli  0.03304939 -0.06142622 0.127525 0.4874422
# 
# $species
# diff      lwr      upr p adj
# C. inopinata-C. elegans 1.280898 1.186422 1.375373     0
# 
# $`bacterial_strain:species`
# diff         lwr        upr     p adj
# Klebsiella sp. nov.:C. elegans-E. coli :C. elegans               0.09652418 -0.07968192  0.2727303 0.4773017
# E. coli :C. inopinata-E. coli :C. elegans                        1.53915862  1.35971911  1.7185981 0.0000000
# Klebsiella sp. nov.:C. inopinata-E. coli :C. elegans             1.16962049  1.00349179  1.3357492 0.0000000
# E. coli :C. inopinata-Klebsiella sp. nov.:C. elegans             1.44263444  1.25382666  1.6314422 0.0000000
# Klebsiella sp. nov.:C. inopinata-Klebsiella sp. nov.:C. elegans  1.07309631  0.89689021  1.2493024 0.0000000
# Klebsiella sp. nov.:C. inopinata-E. coli :C. inopinata          -0.36953813 -0.54897764 -0.1900986 0.0000050

################################################## END Viability and Dev. timing ########################################################################

##########################################################################################################################








##########################################################################################################################

############################## Body size LENGTH ##################################

# set working directory
setwd("~/Desktop/MANUSCRIPT/DATA/bodysize")


# load in data
dat <- read.csv("bodysize.csv", header = TRUE)
View(dat)

names <- c("sp_bac_sex", "species","experiment")
dat <- dat %>%
  mutate(across(names, as.factor))

str(dat)
View(dat)

# Filtering data according to nematode species
dat_ino <- dat %>% 
  filter(species == "C. inopinata")

dat_ele <- dat %>% 
  filter(species == "C. elegans")




# Plotting!!

# Length Microns
A <- ggplot(dat_ino, aes(x = bacteria, y = length_microns, group = sp_bac_sex, color = sex)) + 
  geom_sina() +
  stat_summary(aes(group=sp_bac_sex),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 2300), breaks = seq(0,2400,200)) +
  labs(fill = "Nematode sex") +
  guides(color = guide_legend(title = "Nematode sex")) +
  xlab("Bacterial Strain") +
  ylab("Length of individual nematodes (microns)") +
  theme_cowplot() +
  theme(plot.title = element_text(hjust = 1)) +
  theme(axis.text.x=element_text(face="italic"))

A


B <- ggplot(dat_ele, aes(x = bacteria, y = length_microns, group = sp_bac_sex, color = sex)) + 
  geom_sina() +
  stat_summary(aes(group=sp_bac_sex),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 2300), breaks = seq(0,2400,200)) +
  labs(fill = "Nematode sex") +
  guides(color = guide_legend(title = "Nematode sex")) +
  xlab("Bacterial Strain") +
  ylab("Length of individual nematodes (microns)") +
  theme_cowplot() +
  theme(plot.title = element_text(hjust = 1)) +
  theme(axis.text.x=element_text(face="italic"))


B


legnthfigure <- ggarrange(A,B, labels = c("A", "B"), ncol = 1, nrow = 2)
legnthfigure



############ STATS #############


# Ino anova

### LENGTH
two.way <- aov(length_microns ~ sp_bac_sex, data = dat_ino)
summary(two.way)
# Df  Sum Sq Mean Sq F value Pr(>F)    
# sp_bac_sex   3 5587746 1862582      92 <2e-16 ***
#   Residuals   74 1498196   20246                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = length_microns ~ sp_bac_sex, data = dat_ino)
# 
# $sp_bac_sex
# diff        lwr       upr     p adj
# I_OP50_male-I_OP50_female -542.52645 -663.86406 -421.1888 0.0000000
# I_R_fem-I_OP50_female       51.63150  -66.78188  170.0449 0.6623970
# I_R_male-I_OP50_female    -466.36380 -587.70141 -345.0262 0.0000000
# I_R_fem-I_OP50_male        594.15795  475.74457  712.5713 0.0000000
# I_R_male-I_OP50_male        76.16265  -45.17496  197.5003 0.3575602
# I_R_male-I_R_fem          -517.99530 -636.40868 -399.5819 0.0000000



# Ele anova

### LENGTH
two.way <- aov(length_microns ~ sp_bac_sex, data = dat_ele)
summary(two.way)
# Df  Sum Sq Mean Sq F value Pr(>F)    
# sp_bac_sex   3 3672253 1224084   297.6 <2e-16 ***
#   Residuals   86  353793    4114                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = length_microns ~ sp_bac_sex, data = dat_ele)
# 
# $sp_bac_sex
# diff        lwr       upr    p adj
# E_OP50_male-E_OP50_female -317.9736 -375.95445 -259.9928 0.00e+00
# E_R_fem-E_OP50_female      206.6775  158.52707  254.8280 0.00e+00
# E_R_male-E_OP50_female    -214.7984 -264.10176 -165.4950 0.00e+00
# E_R_fem-E_OP50_male        524.6511  469.96275  579.3395 0.00e+00
# E_R_male-E_OP50_male       103.1752   47.46907  158.8814 3.16e-05
# E_R_male-E_R_fem          -421.4759 -466.86167 -376.0901 0.00e+00




################################################## END Body size ########################################################################

##########################################################################################################################








##########################################################################################################################

###########################################################################################

############################## Behavioral Choice  ##################################

setwd("~/Desktop/MANUSCRIPT/DATA/behavior")

# load in data
dat <- read.csv("dat.csv", header = TRUE)

names <- c("plate_ID", "bacteria", "observer", "orientation")
dat <- dat %>%
  mutate(across(names, as.factor))

str(dat)

# Filtering data according to nematode species
dat_ino <- dat %>% 
  filter(species == "C. inopinata")

dat_ele <- dat %>% 
  filter(species == "C. elegans")


# Plotting the preferences!!

# Ino
A <- ggplot(dat_ino, aes(x = bacterial_combo, y = fraction_worms, group = sp_bac_combo, color = bacteria)) + 
  geom_sina() +
  stat_summary(aes(group=sp_bac_combo),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.75)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,.1)) +
  labs(fill = "Where were the worms?") +
  guides(color = guide_legend(title = "Where were the worms?")) +
  xlab("Bacterial choice combination") +
  ylab("Fraction of worms in bacterial environments") +
  theme_cowplot() +
  theme(plot.title = element_text(hjust = 1)) +
  theme(axis.text.x=element_text(face="italic")) +
  theme(legend.text = element_text(face="italic")) +
  theme(axis.title.x = element_text(size=15),
      axis.title.y = element_text(size=15),
      axis.text.x = element_text(size=15,face="italic"),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15)) 

A

View(dat_ele)

# Ele
B <- ggplot(dat_ele, aes(x = bacterial_combo, y = fraction_worms, group = sp_bac_combo, color = bacteria)) + 
  geom_sina() +
  stat_summary(aes(group=sp_bac_combo),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,.1)) +
  labs(fill = "Where were the worms?") +
  guides(color = guide_legend(title = "Where were the worms?")) +
  xlab("Bacterial choice combination") +
  ylab("Fraction of worms in bacterial environments") +
  theme_cowplot() +
  theme(plot.title = element_text(hjust = 1)) +
  theme(axis.text.x=element_text(face="italic")) +
  theme(legend.text = element_text(face="italic")) +
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size=15,face="italic"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15)) 

B

figure <- ggarrange(A,B, labels = c("A", "B"), ncol = 1, nrow = 2)

figure






# STATS

two.way <- aov(fraction_worms ~ sp_bac_combo, data = dat)

summary(two.way)
# Df Sum Sq Mean Sq F value Pr(>F)    
# sp_bac_combo  17 11.182  0.6578   70.63 <2e-16 ***
#   Residuals    384  3.576  0.0093                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 3 observations deleted due to missingness



tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
## ELEGANS COMPARISON ##
# E_E_R_N-E_E_R_E -0.322788454 -0.432772364 -0.212804545 0.0000000
# E_E_R_R-E_E_R_E  0.307568109  0.197584199  0.417552018 0.0000000
# E_E_R_R-E_E_R_N  0.630356563  0.520372653  0.740340473 0.0000000

# E_E_S_S-E_E_S_N  0.518570941  0.411371887  0.625769996 0.0000000
# E_E_S_S-E_E_S_E  0.170769885  0.063570831  0.277968940 0.0000063
# E_E_S_N-E_E_S_E -0.347801056 -0.455000111 -0.240602001 0.0000000

# E_R_S_R-E_R_S_N  0.405679296  0.298480241  0.512878351 0.0000000
# E_R_S_S-E_R_S_N  0.515314837  0.408115782  0.622513892 0.0000000
# E_R_S_S-E_R_S_R  0.109635541  0.002436486  0.216834595 0.0387474


## INOPINATA COMPARISONS ##
# I_E_R_N-I_E_R_E -0.081943495 -0.177825245  0.013938254 0.2044137
# I_E_R_R-I_E_R_E  0.208167908  0.112286159  0.304049658 0.0000000
# I_E_R_R-I_E_R_N  0.290111404  0.194229654  0.385993153 0.0000000



# I_E_S_N-I_E_S_E -0.157254250 -0.253135999 -0.061372500 0.0000026
# I_E_S_S-I_E_S_E -0.037986159 -0.133867909  0.057895590 0.9957724
# I_E_S_S-I_E_S_N  0.119268090  0.023386341  0.215149840 0.0021370


# I_R_S_R-I_R_S_N  0.106036818  0.010155068  0.201918567 0.0141846
# I_R_S_S-I_R_S_N  0.114576518  0.018694768  0.210458267 0.0043090
# I_R_S_S-I_R_S_R  0.008539700 -0.087342049  0.104421450 1.0000000

## Comparing non-lawn elegans and inopinata for each combination ##
# I_R_S_N-E_R_S_N  0.237761341  0.129660064  0.345862619 0.0000000
# I_E_S_N-E_E_S_N  0.208720234  0.100618957  0.316821512 0.0000000
# I_E_R_N-E_E_R_N  0.189044479  0.079374160  0.298714797 0.0000005

# E_E_S_N-E_E_R_N  0.020722379 -0.094715962  0.136160720 0.9999999
# E_R_S_N-E_E_R_N  0.007565506 -0.107872835  0.123003847 1.0000000
# E_R_S_N-E_E_S_N -0.013156874 -0.127105625  0.100791878 1.0000000
# 
# I_E_S_N-I_E_R_N  0.040398135 -0.061520727  0.142316997 0.9957481
# I_R_S_N-I_E_R_N  0.056282368 -0.045636494  0.158201230 0.8945954
# I_R_S_N-I_E_S_N  0.015884233 -0.086034629  0.117803095 1.0000000


############################################################################




###########################################################################################

############################## Egg lay  ##################################

# Setting workflow
setwd("~/Desktop/MANUSCRIPT/DATA/behavior")


# importing data
dat <- read.csv("egg_lay.csv", header = TRUE)


########### FILTER ############

# Filtering out nematodes that laid less than 10 embryos 

dat <- dat %>%
  filter(life == "LIVING") %>%
  mutate(fraction = in_lawn/total_embryos) %>%
  filter(total_embryos >= 10) 

View(dat)

# NOTE: after filtering, there are less inopinata samples overall that meet the filtered criteria. These are the ones we will plot;
## however, this is because inopinata lays less embryos overall. 


# Plotting 
plot <- ggplot(dat, aes(x = species, y = fraction, group = sp_bac, color = bacteria)) + 
  geom_sina() +
  stat_summary(aes(group=sp_bac),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,.1)) +
  labs(fill = "Bacterial Strains") +
  guides(color = guide_legend(title = "Bacterial Strains")) +
  xlab("Nematode species") +
  ylab("Fraction of embryos laid in bacterial lawn") +
  theme_cowplot() +
  theme(axis.text.x=element_text(face="italic")) +
  theme(legend.text = element_text(face="italic")) +
  theme(axis.title.x = element_text(size=12),
      axis.title.y = element_text(size=12),
      axis.text.x = element_text(size=12,face="italic"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12)) 

# call the plot 
plot




## STATS FILTERED ##

two.way <- aov(fraction ~ species*bacteria, data = dat)

summary(two.way)

# Df Sum Sq Mean Sq F value  Pr(>F)    
# species           1 0.8922  0.8922  33.561 2.1e-07 ***
#   bacteria          1 0.1476  0.1476   5.552  0.0214 *  
#   species:bacteria  1 0.1159  0.1159   4.358  0.0407 *  
#   Residuals        66 1.7545  0.0266                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



tukey.two.way<-TukeyHSD(two.way)

tukey.two.way

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = fraction ~ species * bacteria, data = dat)
# 
# $species
# diff        lwr        upr p adj
# C. inopinata-C. elegans -0.2463578 -0.3312628 -0.1614528 2e-07
# 
# $bacteria
# diff        lwr       upr     p adj
# Klebsiella sp. WOUb2-E. coli OP50 0.09567358 0.01283588 0.1785113 0.0242641
# 
# $`species:bacteria`
# diff         lwr          upr     p adj
# C. inopinata:E. coli OP50-C. elegans:E. coli OP50                 -0.33605285 -0.51681149 -0.155294211 0.0000381
# C. elegans:Klebsiella sp. WOUb2-C. elegans:E. coli OP50            0.03435462 -0.10469889  0.173408120 0.9147714
# C. inopinata:Klebsiella sp. WOUb2-C. elegans:E. coli OP50         -0.11663116 -0.29268456  0.059422249 0.3085712
# C. elegans:Klebsiella sp. WOUb2-C. inopinata:E. coli OP50          0.37040747  0.21679216  0.524022774 0.0000001
# C. inopinata:Klebsiella sp. WOUb2-C. inopinata:E. coli OP50        0.21942170  0.03165432  0.407189070 0.0155835
# C. inopinata:Klebsiella sp. WOUb2-C. elegans:Klebsiella sp. WOUb2 -0.15098577 -0.29903569 -0.002935856 0.0439426

################################################


################################################## END Behavioral Choice and Egg lay ########################################################################

##########################################################################################################################







##########################################################################################################################

############################## SUPPLEMENTS ##################################




####################################################################
#### C elegans Population Growth ####

# Set working directory 
setwd("/Users/austincolelink/Desktop/MANUSCRIPT/DATA/crosslevelfec")

# Importing Data
dat <- read.csv("elegansWOUb2_crude_fec.csv",header=T,stringsAsFactors = TRUE)

# Getting Clean data 
clean_dat <- filter(dat, contamination_day_seven == "clean") %>%
  mutate(str_bac = paste(strain_worm,strain_bacteria))

clean_dat_no_failed_crosses <- clean_dat[clean_dat$number_worms_day_seven > 5,]
View(clean_dat_no_failed_crosses)

# Plot!
ceplot7day <-ggplot(clean_dat_no_failed_crosses, aes(x = strain_worm, y = number_worms_day_seven, group = str_bac, color = strain_bacteria)) + 
  geom_sina(size = 1.5) +
  stat_summary(aes(group=str_bac),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 2800), breaks = seq(0,2800,200)) +
  labs(fill = "Bacterial Strains") +
  guides(color = guide_legend(title = "Bacterial Strains")) +
  xlab("Nematode Strain") +
  ylab("Number of worms after 7 days") +
  theme_cowplot() +
  theme(axis.text.x=element_text(face="italic")) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  theme(legend.text = element_text(face="italic"))

ceplot7day




### Stats!!
View(clean_dat_no_failed_crosses)

aov <- aov(number_worms_day_seven ~ strain_bacteria*strain_worm, data = clean_dat_no_failed_crosses)

summary(aov)

# Df   Sum Sq Mean Sq F value   Pr(>F)    
# strain_bacteria              1  4721980 4721980  18.231 5.23e-05 ***
#   strain_worm                  1  1060045 1060045   4.093   0.0463 *  
#   strain_bacteria:strain_worm  1       57      57   0.000   0.9882    
# Residuals                   82 21238265  259003                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tukey.aov<-TukeyHSD(aov)

tukey.aov

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = number_worms_day_seven ~ strain_bacteria * strain_worm, data = clean_dat_no_failed_crosses)
# 
# $strain_bacteria
# diff      lwr      upr    p adj
# Klebsiella sp. nov.-E. coli 495.6497 264.7254 726.5741 5.23e-05
# 
# $strain_worm
# diff      lwr      upr     p adj
# PD1074-fog-2 221.4407 3.098471 439.7829 0.0469091
# 
# $`strain_bacteria:strain_worm`
# diff        lwr       upr     p adj
# Klebsiella sp. nov.:fog-2-E. coli:fog-2               511.2051   68.03447  954.3758 0.0171478
# E. coli:PD1074-E. coli:fog-2                          220.3510 -278.00127  718.7032 0.6538653
# Klebsiella sp. nov.:PD1074-E. coli:fog-2              735.0199  284.46733 1185.5726 0.0002915
# E. coli:PD1074-Klebsiella sp. nov.:fog-2             -290.8542 -704.02282  122.3145 0.2595339
# Klebsiella sp. nov.:PD1074-Klebsiella sp. nov.:fog-2  223.8148 -130.23469  577.8643 0.3525768
# Klebsiella sp. nov.:PD1074-E. coli:PD1074             514.6690   93.59207  935.7459 0.0101826


### Effect size ###

datPD1074 <- clean_dat_no_failed_crosses %>%
  filter(strain_worm == "PD1074") 
View(datPD1074)
PD1074fec_OP50 <- datPD1074[datPD1074$strain_bacteria == "E. coli OP50",]$number_worms_day_seven
PD1074fec_WOUB2 <- datPD1074[datPD1074$strain_bacteria == "Klebsiella sp. WOUb2",]$number_worms_day_seven
cohen.d(PD1074fec_WOUB2,PD1074fec_OP50)
# Cohen's d
# 
# d estimate: 1.011517 (large)
# 95 percent confidence interval:
#     lower     upper 
# 0.3373579 1.6856752 

datfog2 <- clean_dat_no_failed_crosses %>%
  filter(strain_worm == "fog-2") 
View(datfog2)
fog2fec_OP50 <- datfog2[datfog2$strain_bacteria == "E. coli OP50",]$number_worms_day_seven
fog2fec_WOUB2 <- datfog2[datfog2$strain_bacteria == "Klebsiella sp. WOUb2",]$number_worms_day_seven
cohen.d(fog2fec_WOUB2,fog2fec_OP50)
# Cohen's d
# 
# d estimate: 1.004257 (large)
# 95 percent confidence interval:
#     lower     upper 
# 0.2989097 1.7096052 

####################################################################



####################################################################
############ NON-FILTER EGG LAY ##############

# importing data
dat <- read.csv("egg_lay.csv", header = TRUE)


dat_no_filt <- dat %>%
  filter(life == "LIVING") %>%
  mutate(fraction = in_lawn/total_embryos) 

# REplacing nan due dividing 0 lawn embryos by 0 total embryos, aka, the worms that did not lay embryos but lived
dat_no_filt$fraction[is.na(dat_no_filt$fraction)] <- 0

# Plotting egg lay graph 
plot <- ggplot(dat_no_filt, aes(x = species, y = fraction, group = sp_bac, color = bacteria)) + 
  geom_sina() +
  stat_summary(aes(group=sp_bac),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,.1)) +
  labs(fill = "Bacterial Strains") +
  guides(color = guide_legend(title = "Bacterial Strains")) +
  xlab("Worm species") +
  ylab("Fraction of embryos laid in bacterial lawn") +
  theme_cowplot() +
  theme(axis.text.x=element_text(face="italic")) +
  theme(legend.text = element_text(face="italic")) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12,face="italic"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) 

plot



##### STATS #######

two.way <- aov(fraction ~ species*bacteria, data = dat_no_filt)

summary(two.way)

# Df Sum Sq Mean Sq F value   Pr(>F)    
# species            1  1.690   1.690  14.256  0.00024 ***
#   bacteria           1  3.843   3.843  32.426 7.58e-08 ***
#   species:bacteria   1  0.043   0.043   0.365  0.54656    
# Residuals        133 15.765   0.119                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



tukey.two.way<-TukeyHSD(two.way)
tukey.two.way


# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = fraction ~ species * bacteria, data = dat_no_filt)
# 
# $species
# diff        lwr        upr     p adj
# C. inopinata-C. elegans -0.223126 -0.3400133 -0.1062388 0.0002396
# 
# $bacteria
# diff       lwr      upr p adj
# Klebsiella sp. WOUb2-E. coli OP50 0.332374 0.2158619 0.448886 1e-07
# 
# $`species:bacteria`
# diff         lwr         upr     p adj
# C. inopinata:E. coli OP50-C. elegans:E. coli OP50                 -0.2144405 -0.43688803 0.008007076 0.0632181
# C. elegans:Klebsiella sp. WOUb2-C. elegans:E. coli OP50            0.3054912  0.09544506 0.515537346 0.0013154
# C. inopinata:Klebsiella sp. WOUb2-C. elegans:E. coli OP50          0.1631748 -0.07036310 0.396712736 0.2695131
# C. elegans:Klebsiella sp. WOUb2-C. inopinata:E. coli OP50          0.5199317  0.31539426 0.724469099 0.0000000
# C. inopinata:Klebsiella sp. WOUb2-C. inopinata:E. coli OP50        0.3776153  0.14901929 0.606211299 0.0001922
# C. inopinata:Klebsiella sp. WOUb2-C. elegans:Klebsiella sp. WOUb2 -0.1423164 -0.35886338 0.074230607 0.3226115

############################################################################


####################################################################
############ FAILED AND CONTAM POP GROWTH ##############


setwd("/Users/austincolelink/Desktop/MANUSCRIPT/DATA/crosslevelfec")


# Import data into the R environment 
dat <- read.csv("crudefec.csv",header=T,stringsAsFactors = TRUE)


# Turning experiment and trial numbers into factors to make them "labels" as opposed to "numbers"
dat$experiment_number <- as.factor(dat$experiment_number)
dat$trial_number <- as.factor(dat$trial_number)


# Pasting trial and experiment numbers together to get unique ID's for grouping 
dat$experiment_trial_number <- as.factor(paste(dat$experiment_number, dat$trial_number))


# Assigning clean data to an object to alter names from WOUb# (lab strain nomenclature) to identified taxonomy 
named_bacteria <- dat


# Creating Placeholders and then filling those placeholders with the Genus name corresponding to the WOUB#
named_bacteria$bacteria_name_2 <- "placeholder"

named_bacteria[named_bacteria$strain_bacteria == "OP50",]$bacteria_name_2 <- "E. coli OP50"
named_bacteria[named_bacteria$strain_bacteria == "Klebsiella sp. nov.",]$bacteria_name_2 <- "Klebsiella sp. WOUb2"
named_bacteria[named_bacteria$strain_bacteria == "WOUB1",]$bacteria_name_2 <- "Stenotrophomonas sp. WOUb1"
named_bacteria[named_bacteria$strain_bacteria == "WOUB16",]$bacteria_name_2 <- "Comamonas sp. WOUb16"
named_bacteria[named_bacteria$strain_bacteria == "WOUB6",]$bacteria_name_2 <- "Stenotrophomonas sp. WOUb6"
named_bacteria[named_bacteria$strain_bacteria == "WOUB10",]$bacteria_name_2 <- "Pseudomonas sp. WOUb10"
named_bacteria[named_bacteria$strain_bacteria == "WOUB18",]$bacteria_name_2 <- "Leclercia sp. WOUb18"
named_bacteria[named_bacteria$strain_bacteria == "WOUB22",]$bacteria_name_2 <- "Acinetobacter sp. WOUb22"
named_bacteria[named_bacteria$strain_bacteria == "WOUB37",]$bacteria_name_2 <- "Agrobacterium sp. WOUb37"
named_bacteria[named_bacteria$strain_bacteria == "WOUB43",]$bacteria_name_2 <- "Microbacterium sp. WOUb43"
named_bacteria[named_bacteria$strain_bacteria == "WOUB9",]$bacteria_name_2 <- "Mammaliicoccus sp. WOUb9"


# Creating A "Fig or No" Prompt to be able to alter all fig/non-fig bacteria in ways corresponding to their isolation source, E. coli being the only non-fig
named_bacteria$fig_or_no <- "placeholder"

named_bacteria[named_bacteria$strain_bacteria == "OP50",]$fig_or_no <- "No"
named_bacteria[named_bacteria$strain_bacteria == "Klebsiella sp. nov.",]$fig_or_no <- "Yes"
named_bacteria[named_bacteria$strain_bacteria == "WOUB1",]$fig_or_no <- "Yes"
named_bacteria[named_bacteria$strain_bacteria == "WOUB16",]$fig_or_no <- "Yes"
named_bacteria[named_bacteria$strain_bacteria == "WOUB6",]$fig_or_no <- "Yes"
named_bacteria[named_bacteria$strain_bacteria == "WOUB10",]$fig_or_no <- "Yes"
named_bacteria[named_bacteria$strain_bacteria == "WOUB18",]$fig_or_no <- "Yes"
named_bacteria[named_bacteria$strain_bacteria == "WOUB22",]$fig_or_no <- "Yes"
named_bacteria[named_bacteria$strain_bacteria == "WOUB37",]$fig_or_no <- "Yes"
named_bacteria[named_bacteria$strain_bacteria == "WOUB43",]$fig_or_no <- "Yes"
named_bacteria[named_bacteria$strain_bacteria == "WOUB9",]$fig_or_no <- "Yes"


# Turning our bacterial names and fig/no prompt into factors, then arranging the levels of those into whatever order I want (descending order in the case of this figure) 
named_bacteria$bacteria_name_2 <- as.factor(named_bacteria$bacteria_name_2)
named_bacteria$bacteria_name_2 <- factor(named_bacteria$bacteria_name_2, levels=c("E. coli OP50", "Klebsiella sp. WOUb2", "Leclercia sp. WOUb18", "Agrobacterium sp. WOUb37", "Microbacterium sp. WOUb43", "Acinetobacter sp. WOUb22", "Comamonas sp. WOUb16", "Stenotrophomonas sp. WOUb6", "Stenotrophomonas sp. WOUb1", "Pseudomonas sp. WOUb10", "Mammaliicoccus sp. WOUb9"))
named_bacteria$fig_or_no <- as.factor(named_bacteria$fig_or_no)
named_bacteria$fig_or_no <- factor(named_bacteria$fig_or_no, levels=c("Yes","No"))


# Plotting!!
plot_crossfec <- ggplot(named_bacteria, aes(x = bacteria_name_2, y = number_worms)) +
  geom_hline(yintercept=mean(named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "OP50",]$number_worms),linetype="dotted") +
  geom_sina(size = 1, scale="width", aes(colour=fig_or_no)) +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="orange",position = position_dodge(width = 0.9))  +
  theme_cowplot() +
  xlab("Bacterial strain") +
  ylab("Number of nematodes after one week") +
  scale_y_continuous(limits=c(-5,2000), breaks=c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1,face="italic")) +
  scale_colour_manual(values=c("black","red"),name = "Fig bacteria?") +
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size=15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12))

# Calling plot object to view
plot_crossfec



############################################################################


####################################################################
############ Paired Fig E. coli ##############

# Set working directory 
setwd("/Users/austincolelink/Desktop/MANUSCRIPT/DATA/crosslevelfec")


# Import data into the R environment 
dat <- read.csv("crudefecsupp.csv",header=T,stringsAsFactors = TRUE)


# Turning experiment and trial numbers into factors to make them "labels" as opposed to "numbers"
dat$experiment_number <- as.factor(dat$experiment_number)
dat$trial_number <- as.factor(dat$trial_number)


# Pulling out all of the clean data
clean_dat <- dat[dat$contamination == "clean",]


# Pasting trial and experiment numbers together to get unique ID's for grouping 
clean_dat$experiment_trial_number <- as.factor(paste(clean_dat$experiment_number, clean_dat$trial_number))
clean_dat$experiment_trial_number_bacteria <- as.factor(paste(clean_dat$experiment_trial_number, clean_dat$strain_bacteria))

# Excluding failed crosses
## Contaminated crosses were already discared when pulling out the clean data, but this excludes plates that did not produce additional worms besides the original five in the cross
### This is why we are pulling out all of the crosses with more than 5 worms at the end of the seven day period 
clean_dat_no_failed_crosses <- clean_dat[clean_dat$number_worms > 5,]


# Assigning clean data to an object to alter names from WOUb# (lab strain nomenclature) to identified taxonomy 
named_bacteria_no_failed <- clean_dat_no_failed_crosses


# Creating Placeholders and then filling those placeholders with the Genus name corresponding to the WOUB#
named_bacteria_no_failed$bacteria_name_2 <- "placeholder"

named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "OP50",]$bacteria_name_2 <- "E. coli OP50"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "Klebsiella sp. nov.",]$bacteria_name_2 <- "Klebsiella sp. WOUb2"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB1",]$bacteria_name_2 <- "Stenotrophomonas sp. WOUb1"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB16",]$bacteria_name_2 <- "Comamonas sp. WOUb16"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB6",]$bacteria_name_2 <- "Stenotrophomonas sp. WOUb6"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB10",]$bacteria_name_2 <- "Pseudomonas sp. WOUb10"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB18",]$bacteria_name_2 <- "Leclercia sp. WOUb18"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB22",]$bacteria_name_2 <- "Acinetobacter sp. WOUb22"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB37",]$bacteria_name_2 <- "Agrobacterium sp. WOUb37"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB43",]$bacteria_name_2 <- "Microbacterium sp. WOUb43"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB9",]$bacteria_name_2 <- "Mammaliicoccus sp. WOUb9"

# Creating A "Fig or No" Prompt to be able to alter all fig/non-fig bacteria in ways corresponding to their isolation source, E. coli being the only non-fig
named_bacteria_no_failed$fig_or_no <- "placeholder"

named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "OP50",]$fig_or_no <- "No"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "Klebsiella sp. nov.",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB1",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB16",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB6",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB10",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB18",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB22",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB37",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB43",]$fig_or_no <- "Yes"
named_bacteria_no_failed[named_bacteria_no_failed$strain_bacteria == "WOUB9",]$fig_or_no <- "Yes"


# Turning our bacterial names and fig/no prompt into factors, then arranging the levels of those into whatever order I want (descending order in the case of this figure) 
named_bacteria_no_failed$bacteria_name_2 <- as.factor(named_bacteria_no_failed$bacteria_name_2)
named_bacteria_no_failed$bacteria_name_2 <- factor(named_bacteria_no_failed$bacteria_name_2, levels=c("E. coli OP50", "Klebsiella sp. WOUb2", "Leclercia sp. WOUb18", "Agrobacterium sp. WOUb37", "Microbacterium sp. WOUb43", "Acinetobacter sp. WOUb22", "Comamonas sp. WOUb16", "Stenotrophomonas sp. WOUb6", "Stenotrophomonas sp. WOUb1", "Pseudomonas sp. WOUb10", "Mammaliicoccus sp. WOUb9"))
named_bacteria_no_failed$fig_or_no <- as.factor(named_bacteria_no_failed$fig_or_no)
named_bacteria_no_failed$fig_or_no <- factor(named_bacteria_no_failed$fig_or_no, levels=c("Yes","No"))

# Creating labels to relabel x axis since it is currently experiment trial numbers
xlabels <- c("Klebsiella sp. WOUb2 trial 1", "Klebsiella sp. WOUb2 trial 2", "Mammaliicoccus sp. WOUb9", "Stenotrophomonas sp. WOUb1", "Comamonas sp. WOUb16", "Stenotrophomonas sp. WOUb6", "Pseudomonas sp. WOUb10", "Leclercia sp. WOUb18", "Acinetobacter sp. WOUb22", "Agrobacterium sp. WOUb37", "Microbacterium sp. WOUb43")




# Plotting!!
plot_crossfec <- ggplot(named_bacteria_no_failed, aes(x = experiment_trial_number, y = number_worms, group = experiment_trial_number_bacteria, color = bacteria_name_2)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9))  +
  theme_cowplot() +
  xlab("Bacterial strain") +
  ylab("Number of nematodes after one week") +
  guides(color = guide_legend(title = "Bacterial Strains")) +
  scale_x_discrete(labels= xlabels) +
  scale_y_continuous(limits=c(-5,2000), breaks=c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1,face="italic")) +
  theme(legend.text = element_text(face="italic")) +
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size=30),
        axis.text.x = element_text(size=15),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 25))

# Calling plot object to view
plot_crossfec

############################################################################
############################################################################

############################## WIDTH ##################################

# set working directory
setwd("~/Desktop/MANUSCRIPT/DATA/bodysize")


# load in data
dat <- read.csv("bodysize.csv", header = TRUE)
View(dat)

names <- c("sp_bac_sex", "species","experiment")
dat <- dat %>%
  mutate(across(names, as.factor))

str(dat)
View(dat)

# Filtering data according to nematode species
dat_ino <- dat %>% 
  filter(species == "C. inopinata")

dat_ele <- dat %>% 
  filter(species == "C. elegans")




# Plotting!!


# Width Microns
C <- ggplot(dat_ino, aes(x = bacteria, y = width_microns, group = sp_bac_sex, color = sex)) + 
  geom_sina() +
  stat_summary(aes(group=sp_bac_sex),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,10)) +
  labs(fill = "Nematode sex") +
  guides(color = guide_legend(title = "Nematode sex")) +
  xlab("Bacterial Strain") +
  ylab("Width of individual nematodes (microns)") +
  theme_cowplot() +
  theme(plot.title = element_text(hjust = 1)) +
  theme(axis.text.x=element_text(face="italic"))

C

D <- ggplot(dat_ele, aes(x = bacteria, y = width_microns, group = sp_bac_sex, color = sex)) + 
  geom_sina() +
  stat_summary(aes(group=sp_bac_sex),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,10)) +
  labs(fill = "Nematode sex") +
  guides(color = guide_legend(title = "Nematode sex")) +
  xlab("Bacterial Strain") +
  ylab("Width of individual nematodes (microns)") +
  theme_cowplot() +
  theme(plot.title = element_text(hjust = 1)) +
  theme(axis.text.x=element_text(face="italic"))

D


widthfigure <- ggarrange(C,D, labels = c("A", "B"), ncol = 2, nrow = 1)
widthfigure





############ STATS #############

# Ino anova

##### WIDTH
two.way <- aov(width_microns ~ sp_bac_sex, data = dat_ino)
summary(two.way)
# Df Sum Sq Mean Sq F value Pr(>F)    
# sp_bac_sex   3   7039  2346.4   56.89 <2e-16 ***
#   Residuals   74   3052    41.2                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = width_microns ~ sp_bac_sex, data = dat_ino)
# 
# $sp_bac_sex
# diff       lwr        upr     p adj
# I_OP50_male-I_OP50_female -21.335585 -26.81194 -15.859233 0.0000000
# I_R_fem-I_OP50_female       1.017472  -4.32690   6.361844 0.9587250
# I_R_male-I_OP50_female    -14.279014 -19.75537  -8.802662 0.0000000
# I_R_fem-I_OP50_male        22.353057  17.00868  27.697429 0.0000000
# I_R_male-I_OP50_male        7.056571   1.58022  12.532923 0.0061171
# I_R_male-I_R_fem          -15.296485 -20.64086  -9.952114 0.0000000





# Ele anova

### WIDTH
two.way <- aov(width_microns ~ sp_bac_sex, data = dat_ele)
summary(two.way)
# Df Sum Sq Mean Sq F value Pr(>F)    
# sp_bac_sex   3  18404    6135   237.2 <2e-16 ***
#   Residuals   86   2224      26                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = width_microns ~ sp_bac_sex, data = dat_ele)
# 
# $sp_bac_sex
# diff        lwr       upr    p adj
# E_OP50_male-E_OP50_female -26.601913 -31.199209 -22.00462 0.00e+00
# E_R_fem-E_OP50_female      10.422353   6.604507  14.24020 0.00e+00
# E_R_male-E_OP50_female    -18.222986 -22.132248 -14.31372 0.00e+00
# E_R_fem-E_OP50_male        37.024266  32.688026  41.36051 0.00e+00
# E_R_male-E_OP50_male        8.378926   3.961987  12.79587 1.98e-05
# E_R_male-E_R_fem          -28.645340 -32.243975 -25.04670 0.00e+00




### Examining effects of sex ###


setwd("~/Desktop/MANUSCRIPT/DATA/bodysize")


# load in data
dat <- read.csv("bodysize.csv", header = TRUE)
View(dat)

names <- c("sp_bac_sex", "species", "sex","experiment")
dat <- dat %>%
  mutate(across(names, as.factor))

str(dat_OP50)
View(dat)

# Filtering data according to nematode species
dat_OP50 <- dat %>% 
  filter(bacteria == "E. coli (OP50)")
View(dat_OP50)

dat_W2 <- dat %>% 
  filter(bacteria == "Klebsiella sp. WOUb2")
View(dat_W2)


## Constructing figures ##
OP50 <- ggplot(dat_OP50, aes(x=sex, y=length_microns, color=species)) +
  geom_sina() +
  scale_color_discrete(name="Nematode Species",labels=c("C. elegans", "C. inopinata")) +
  scale_y_continuous(limits = c(0, 2300), breaks = seq(0,2400,500)) +
  ylab("Length of individual nematodes (microns)") +
  xlab("Nematode sex") +
  geom_smooth(method = "glm", se = FALSE, size = 1, 
              aes(linetype = species, group = species)) +
  theme_cowplot() +
  theme(legend.text = element_text(face="italic")) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

OP50

W2 <- ggplot(dat_W2, aes(x=sex, y=length_microns, color=species)) +
  geom_sina() +
  scale_color_discrete(name="Nematode Species",labels=c("C. elegans", "C. inopinata")) +
  scale_y_continuous(limits = c(0, 2300), breaks = seq(0,2400,500)) +
  ylab("Length of individual nematodes (microns)") +
  xlab("Nematode sex") +
  geom_smooth(method = "glm", se = FALSE, size = 1, 
              aes(linetype = species, group = species)) +
  theme_cowplot() +
  theme(legend.text = element_text(face="italic")) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

W2


lengthcomp <- ggarrange(OP50,W2, labels = c("A", "B"), ncol = 1, nrow = 2)
lengthcomp

# STATSOP50 #


fulllmOP50 <- lm(length_microns ~ species + sex + species*sex, data = dat_OP50)
anova(fulllmOP50)

# Analysis of Variance Table
# 
# Response: length_microns
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# species      1 5543374 5543374 437.096 < 2.2e-16 ***
#   sex          1 3420688 3420688 269.722 < 2.2e-16 ***
#   species:sex  1  224795  224795  17.725 7.564e-05 ***
#   Residuals   69  875077   12682                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


reducedlmOP50 <- lm(length_microns ~ species + sex, data = dat_OP50)
anova(reducedlmOP50)

# Analysis of Variance Table
# 
# Response: length_microns
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# species    1 5543374 5543374  352.80 < 2.2e-16 ***
#   sex        1 3420688 3420688  217.71 < 2.2e-16 ***
#   Residuals 70 1099872   15712                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


lrtest(reducedlmOP50, fulllmOP50)
# Likelihood ratio test
# 
# Model 1: length_microns ~ species + sex
# Model 2: length_microns ~ species + sex + species * sex
# #Df  LogLik Df  Chisq Pr(>Chisq)    
# 1   4 -454.72                         
# 2   5 -446.38  1 16.691    4.4e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1





# STATSW2 #

fulllmW2 <- lm(length_microns ~ species + sex + species*sex, data = dat_W2)
anova(fulllmW2)

# Analysis of Variance Table
# 
# Response: length_microns
# Df  Sum Sq Mean Sq  F value  Pr(>F)    
# species      1 5647270 5647270 526.0472 < 2e-16 ***
#   sex          1 5058004 5058004 471.1566 < 2e-16 ***
#   species:sex  1   53789   53789   5.0105 0.02763 *  
#   Residuals   91  976911   10735                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


reducedlmW2 <- lm(length_microns ~ species + sex, data = dat_W2)
anova(reducedlmW2)

# Analysis of Variance Table
# 
# Response: length_microns
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# species    1 5647270 5647270  504.07 < 2.2e-16 ***
#   sex        1 5058004 5058004  451.48 < 2.2e-16 ***
#   Residuals 92 1030701   11203                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



lrtest(reducedlmW2, fulllmW2)
# Likelihood ratio test
# 
# Model 1: length_microns ~ species + sex
# Model 2: length_microns ~ species + sex + species * sex
# #Df  LogLik Df  Chisq Pr(>Chisq)  
# 1   4 -576.16                       
# 2   5 -573.62  1 5.0918    0.02404 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1







### Effect sizes of sex dimorphism ###

# OP50 #
CEdat_OP50 <- dat %>% 
  filter(bacteria == "E. coli (OP50)") %>% 
  filter(species == "C. elegans")
CIdat_OP50 <- dat %>% 
  filter(bacteria == "E. coli (OP50)") %>% 
  filter(species == "C. inopinata")

CemaleOP50length <- CEdat_OP50[CEdat_OP50$sex == "male",]$length_microns
CefemaleOP50length <- CEdat_OP50[CEdat_OP50$sex == "female",]$length_microns
cohen.d(CemaleOP50length,CefemaleOP50length)
# Cohen's d
# d estimate: -4.148126 (large)
# 95 percent confidence interval:
#     lower     upper 
# -5.377050 -2.919203 

CimaleOP50length <- CIdat_OP50[CIdat_OP50$sex == "male",]$length_microns
CifemaleOP50length <- CIdat_OP50[CIdat_OP50$sex == "female",]$length_microns
cohen.d(CimaleOP50length,CifemaleOP50length)
# Cohen's d
# d estimate: -3.944066 (large)
# 95 percent confidence interval:
#     lower     upper 
# -5.073157 -2.814975 


# W2 #
CEdat_W2 <- dat %>% 
  filter(bacteria == "Klebsiella sp. WOUb2") %>% 
  filter(species == "C. elegans")
CIdat_W2 <- dat %>% 
  filter(bacteria == "Klebsiella sp. WOUb2") %>% 
  filter(species == "C. inopinata")

View(CemaleW2length)

CemaleW2length <- CEdat_W2[CEdat_W2$sex == "male",]$length_microns
CefemaleW2length <- CEdat_W2[CEdat_W2$sex == "female",]$length_microns
cohen.d(CemaleW2length,CefemaleW2length)
# Cohen's d
# d estimate: -7.673693 (large)
# 95 percent confidence interval:
#     lower     upper 
# -9.238005 -6.109381

CimaleW2length <- CIdat_W2[CIdat_W2$sex == "male",]$length_microns
CifemaleW2length <- CIdat_W2[CIdat_W2$sex == "female",]$length_microns
cohen.d(CimaleW2length,CifemaleW2length)
# Cohen's d
# d estimate: -3.532645 (large)
# 95 percent confidence interval:
#     lower     upper 
# -4.557407 -2.507883 











############################################################################
############################################################################
############################################################################
############################################################################
#########################################################################################################

