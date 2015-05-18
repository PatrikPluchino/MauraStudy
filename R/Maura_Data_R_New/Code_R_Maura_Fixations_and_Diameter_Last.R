#### Author: Patrik Pluchino

#### Maura Physiological (Fixations and Pupil diameter) ####

Pup_d <- read.table("/Users/patrikpluchino/Dropbox/Condivisa - Maura/Maura_Data_R_New/Maura_AOIfix_11p_no_p6.txt", header = T)
str(Pup_d)


# CONVERT VARIABLES IN VECTOR

Pup_d$Subject <- as.factor(Pup_d$Subject)

str(Pup_d)

Pup_d$Disposition <- as.factor(Pup_d$Disposition) 

str(Pup_d)

Pup_d$Detailed_Disposition <- as.factor(Pup_d$Detailed_Disposition)

str(Pup_d)

Pup_d$Numerosity <- as.factor(Pup_d$Numerosity)

str(Pup_d)


shapiro.test(Pup_d$AveragePupilDiameter_mm)
# 
# Shapiro-Wilk normality test                                   CHECK!!!
# Error in shapiro.test(Pup_d$AveragePupilDiameter_mm) : 
#   sample size must be between 3 and 5000

# Quick plot of Score to visually inspect the data
qplot(Pup_d$AveragePupilDiameter_mm)


# Anova: DV = Pupil Diameter & IV = Type of Stimulus 
aov1.out = aov(Pup_d$AveragePupilDiameter_mm ~ Pup_d$Detailed_Disposition + Pup_d$Numerosity + Pup_d$Detailed_Disposition:Pup_d$Numerosity + Error(Subject/(Pup_d$Detailed_Disposition*Pup_d$Numerosity)), data=Pup_d)

summary(aov1.out)
# Error: Subject
# Df Sum Sq Mean Sq F value Pr(>F)
# Pup_d$Detailed_Disposition                   4   2136   534.1   0.452  0.789
# Pup_d$Numerosity                             1   1462  1461.8   1.237  0.466
# Pup_d$Detailed_Disposition:Pup_d$Numerosity  4   1890   472.5   0.400  0.811
# Residuals                                    1   1182  1181.7               
# 
# Error: Subject:Pup_d$Detailed_Disposition
# Df Sum Sq Mean Sq F value  Pr(>F)   
# Pup_d$Detailed_Disposition                   4  20.13   5.032   1.683 0.17589   
# Pup_d$Numerosity                             1  19.29  19.293   6.454 0.01566 * 
#   Pup_d$Detailed_Disposition:Pup_d$Numerosity  4  63.71  15.928   5.328 0.00185 **
#   Residuals                                   35 104.63   2.990                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Error: Subject:Pup_d$Numerosity
# Df Sum Sq Mean Sq F value Pr(>F)
# Pup_d$Numerosity                             1  0.575  0.5749   0.696  0.436
# Pup_d$Detailed_Disposition:Pup_d$Numerosity  4  4.119  1.0297   1.246  0.385
# Residuals                                    6  4.959  0.8264               
# 
# Error: Subject:Pup_d$Detailed_Disposition:Pup_d$Numerosity
# Df Sum Sq Mean Sq F value Pr(>F)
# Pup_d$Detailed_Disposition:Pup_d$Numerosity  4   6.21   1.552   0.946  0.448
# Residuals                                   40  65.62   1.641               
# 
# Error: Within
# Df Sum Sq Mean Sq F value Pr(>F)
# Residuals 78899   2216 0.02808


# GRAPHS 

# MAIN EFFECT OF NUMEROSITY
Figure1<-ggplot(Pup_d,aes(x = Numerosity, y = AveragePupilDiameter_mm, colour = Numerosity, group = Numerosity, fill = Stimulus)) # aes (VarIndependent, VarDependent)
Figure1+stat_summary(fun.y=mean,geom="histogram", aes(colour = Stimulus), linetype=1) + stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=0.2, colour = "Black")+stat_summary(fun.y=mean,geom="point",size=2.7,colour="Black")+labs(x="Exploratory Screens",y="Mean pupil diameter (mm)")+theme_bw()+theme(panel.grid.minor=element_line(colour="White"))+theme(panel.grid.major=element_line(colour="White"))+theme(legend.position="bottom")+theme(legend.key=element_rect(colour="White"))+theme(panel.border=element_rect(colour="White"))+theme(axis.line=element_line()) +theme(axis.title.x = element_text(size = 20)) + theme(axis.title.y=element_text(size = 20)) + theme(axis.text.x = element_text(size = 15)) + theme(axis.text.y=element_text(size = 15)) #+ scale_fill_brewer(palette="Set1")


# Mean scores values as a function of Numerosity
tapply(Pup_d$AveragePupilDiameter_mm, Pup_d$Numerosity, mean)
#       1        2 
# 3.399619 3.419648 





## Pairwise comparisons Interaction Detailed Disposition and Numerosity 
TukeyHSD(aov(Pup_d$AveragePupilDiameter_mm ~ Pup_d$Numerosity:Pup_d$Detailed_Disposition, data=Pup_d))



pairwise.t.test(Pup_d$AveragePupilDiameter_mm, Pup_d$Numerosity:Pup_d$Detailed_Disposition, paired = T, p.adjust.method = "bonferroni")

# Pairwise comparisons using paired t tests 
# 
# data:  Behav_R3$Score and Behav_R3$Stimulus 
# 
#             mix14_IN mix14_OUT mix9_IN mix9_OUT ran14_1 ran14_2 ran9_1 ran9_2 tot14_IN tot14_OUT tot9_IN
#   mix14_OUT 0.278    -         -       -        -       -       -      -      -        -         -      
#   mix9_IN   1.000    0.476     -       -        -       -       -      -      -        -         -      
#   mix9_OUT  1.000    0.052     1.000   -        -       -       -      -      -        -         -      
#   ran14_1   1.000    1.000     1.000   0.239    -       -       -      -      -        -         -      
#   ran14_2   1.000    0.157     1.000   1.000    1.000   -       -      -      -        -         -      
#   ran9_1    1.000    0.334     1.000   1.000    1.000   1.000   -      -      -        -         -      
#   ran9_2    1.000    0.550     1.000   1.000    1.000   1.000   1.000  -      -        -         -      
#   tot14_IN  1.000    0.375     1.000   1.000    1.000   1.000   1.000  1.000  -        -         -      
#   tot14_OUT 1.000    1.000     1.000   1.000    1.000   1.000   1.000  1.000  1.000    -         -      
#   tot9_IN   1.000    0.155     1.000   1.000    1.000   1.000   1.000  1.000  1.000    1.000     -      
#   tot9_OUT  1.000    0.062     1.000   1.000    0.087   1.000   1.000  1.000  1.000    0.467     1.000  

#######################################################################################################################


RG_Pup_d <- subset(Pup_d$)

# Repeated Measures ANOVA - DV = Radial Graphs Fixations , IV = DISPOSITION & NUMEROSITY 
aov2.out = aov(Pup_d$Score ~ Behav_R3$Detailed_Disposition + Behav_R3$Numerosity + Behav_R3$Detailed_Disposition:Behav_R3$Numerosity + Error(Subject/(Detailed_Disposition*Numerosity)), data=Behav_R3)


summary(aov2.out)

# Error: Subject
# Df Sum Sq Mean Sq F value Pr(>F)
# Residuals  8  5.522  0.6902               
# 
# Error: Subject:Detailed_Disposition
#                               Df Sum Sq Mean Sq F value Pr(>F)  
# Behav_R3$Detailed_Disposition  4  2.633  0.6583   2.367 0.0735 .
# Residuals                     32  8.898  0.2781                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Error: Subject:Numerosity
#                     Df Sum Sq Mean Sq F value  Pr(>F)   
# Behav_R3$Numerosity  1  3.084  3.0839   20.88 0.00183 **
#   Residuals            8  1.182  0.1477                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Error: Subject:Detailed_Disposition:Numerosity
# Df Sum Sq Mean Sq F value   Pr(>F)    
# Behav_R3$Detailed_Disposition:Behav_R3$Numerosity  4  5.800  1.4499   6.957 0.000379 ***
#   Residuals                                         32  6.669  0.2084                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Error: Within
# Df Sum Sq Mean Sq F value Pr(>F)
# Residuals 18  4.969   0.276 


# MEAN VALUES AS A FUNCTION OF DISPOSITION & NUMEROSITY
tapply(Behav_R3$Score, list(Behav_R3$Detailed_Disposition, Behav_R3$Numerosity), mean)
#           1         2
# 3  1.555556 1.4305556
# 11 1.527778 1.2222222
# 12 1.750000 0.5277778
# 21 1.472222 1.7500000
# 22 1.833333 1.3055556



# GRAPHS

# PLOT NUMEROSITY MAIN EFFECT
Figure4<-ggplot(Behav_R3,aes(x = Numerosity, y = Score, colour = Numerosity, group = Numerosity, fill = Numerosity)) # aes (VarIndependent, VarDependent)
Figure4+stat_summary(fun.y=mean,geom="histogram", aes(colour = Numerosity), linetype= 1, position = "dodge") + stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=0.2, colour = "Black", position=position_dodge(.9))+stat_summary(fun.y=mean,geom="point",size=2.7,colour = "Black", aes(shape = Numerosity), position=position_dodge(.9))+labs(x="Disposition",y="Response Scores")+theme_bw()+theme(panel.grid.minor=element_line(colour="White"))+theme(panel.grid.major=element_line(colour="White"))+theme(legend.position="bottom")+theme(legend.key=element_rect(colour="White"))+theme(panel.border=element_rect(colour="White"))+theme(axis.line=element_line()) +theme(axis.title.x = element_text(size = 20)) + theme(axis.title.y=element_text(size = 20)) + theme(axis.text.x = element_text(size = 15)) + theme(axis.text.y=element_text(size = 15)) + coord_cartesian(ylim=c(0, 2.5))



# PLOT DETAILED DISPOSITION
Figure5<-ggplot(Behav_R3,aes(x = Detailed_Disposition, y = Score, colour = Detailed_Disposition, group = Detailed_Disposition, fill = Detailed_Disposition)) # aes (VarIndependent, VarDependent)
Figure5+stat_summary(fun.y=mean,geom="histogram", aes(colour = Numerosity), linetype= 1, position = "dodge") + stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=0.2, colour = "Black", position=position_dodge(.9))+stat_summary(fun.y=mean,geom="point",size=2.7,colour = "Black", aes(shape = Numerosity), position=position_dodge(.9))+labs(x="Disposition",y="Response Scores")+theme_bw()+theme(panel.grid.minor=element_line(colour="White"))+theme(panel.grid.major=element_line(colour="White"))+theme(legend.position="bottom")+theme(legend.key=element_rect(colour="White"))+theme(panel.border=element_rect(colour="White"))+theme(axis.line=element_line()) +theme(axis.title.x = element_text(size = 20)) + theme(axis.title.y=element_text(size = 20)) + theme(axis.text.x = element_text(size = 15)) + theme(axis.text.y=element_text(size = 15)) + coord_cartesian(ylim=c(0, 2.5))




# PLOT INTERACTION NUMEROSITY * DETAILED DISPOSITION - LINES
Figure2<-ggplot(Behav_R3,aes(x = Detailed_Disposition, y = Score, colour = Numerosity, group = Numerosity)) # aes (VarIndependent, VarDependent)
Figure2+stat_summary(fun.y=mean,geom="line", aes(colour = Numerosity), linetype= 1) + stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=0.2, aes(colour = Numerosity))+stat_summary(fun.y=mean,geom="point",size=2.7,aes(colour = Numerosity, shape = Numerosity))+labs(x="Disposition",y="Response Scores")+theme_bw()+theme(panel.grid.minor=element_line(colour="White"))+theme(panel.grid.major=element_line(colour="White"))+theme(legend.position="bottom")+theme(legend.key=element_rect(colour="White"))+theme(panel.border=element_rect(colour="White"))+theme(axis.line=element_line()) +theme(axis.title.x = element_text(size = 20)) + theme(axis.title.y=element_text(size = 20)) + theme(axis.text.x = element_text(size = 15)) + theme(axis.text.y=element_text(size = 15))


# PLOT INTERACTION NUMEROSITY * DETAILED DISPOSITION - HISTOGRAM
Figure3<-ggplot(Behav_R3,aes(x = Detailed_Disposition, y = Score, colour = Numerosity, group = Numerosity, fill = Numerosity)) # aes (VarIndependent, VarDependent)
Figure3+stat_summary(fun.y=mean,geom="histogram", aes(colour = Numerosity), linetype= 1, position = "dodge") + stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=0.2, colour = "Black", position=position_dodge(.9))+stat_summary(fun.y=mean,geom="point",size=2.7,colour = "Black", aes(shape = Numerosity), position=position_dodge(.9))+labs(x="Disposition",y="Response Scores")+theme_bw()+theme(panel.grid.minor=element_line(colour="White"))+theme(panel.grid.major=element_line(colour="White"))+theme(legend.position="bottom")+theme(legend.key=element_rect(colour="White"))+theme(panel.border=element_rect(colour="White"))+theme(axis.line=element_line()) +theme(axis.title.x = element_text(size = 20)) + theme(axis.title.y=element_text(size = 20)) + theme(axis.text.x = element_text(size = 15)) + theme(axis.text.y=element_text(size = 15)) + coord_cartesian(ylim=c(0, 2.5))


# Pairwise comparisons INTERACTION DISPOSITION * NUMEROSITY
TukeyHSD(aov(Behav_R3$Score ~ Behav_R3$Detailed_Disposition:Behav_R3$Numerosity, data = Behav_R3))

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = Behav_R3$Score ~ Behav_R3$Detailed_Disposition:Behav_R3$Numerosity, data = Behav_R3)
# 
# $`Behav_R3$Detailed_Disposition:Behav_R3$Numerosity`
#                    diff         lwr        upr     p adj
# 11:1-3:1  -2.777778e-02 -0.72465736  0.6691018 1.0000000
# 12:1-3:1   1.944444e-01 -0.50243514  0.8913240 0.9960385
# 21:1-3:1  -8.333333e-02 -0.78021292  0.6135463 0.9999964
# 22:1-3:1   2.777778e-01 -0.41910181  0.9746574 0.9535276
# 3:2-3:1   -1.250000e-01 -0.69399980  0.4439998 0.9993861
# 11:2-3:1  -3.333333e-01 -1.03021292  0.3635463 0.8686816
#                                                           12:2-3:1  -1.027778e+00 -1.72465736 -0.3308982 0.0002615**
# 21:2-3:1   1.944444e-01 -0.50243514  0.8913240 0.9960385
# 22:2-3:1  -2.500000e-01 -0.94687959  0.4468796 0.9763649
# 12:1-11:1  2.222222e-01 -0.58246501  1.0269095 0.9963341
# 21:1-11:1 -5.555556e-02 -0.86024279  0.7491317 1.0000000
# 22:1-11:1  3.055556e-01 -0.49913168  1.1102428 0.9657523
# 3:2-11:1  -9.722222e-02 -0.79410181  0.5996574 0.9999863
# 11:2-11:1 -3.055556e-01 -1.11024279  0.4991317 0.9657523
#                                                           12:2-11:1 -1.000000e+00 -1.80468723 -0.1953128 0.0042651**
# 21:2-11:1  2.222222e-01 -0.58246501  1.0269095 0.9963341
# 22:2-11:1 -2.222222e-01 -1.02690945  0.5824650 0.9963341
# 21:1-12:1 -2.777778e-01 -1.08246501  0.5269095 0.9817545
# 22:1-12:1  8.333333e-02 -0.72135390  0.8880206 0.9999990
# 3:2-12:1  -3.194444e-01 -1.01632403  0.3774351 0.8952416
# 11:2-12:1 -5.277778e-01 -1.33246501  0.2769095 0.5157965
#                                                           12:2-12:1 -1.222222e+00 -2.02690945 -0.4175350 0.0001482**
# 21:2-12:1  1.110223e-15 -0.80468723  0.8046872 1.0000000
# 22:2-12:1 -4.444444e-01 -1.24913168  0.3602428 0.7408488
# 22:1-21:1  3.611111e-01 -0.44357612  1.1657983 0.9067812
# 3:2-21:1  -4.166667e-02 -0.73854625  0.6552129 1.0000000
# 11:2-21:1 -2.500000e-01 -1.05468723  0.5546872 0.9912713
#                                                           12:2-21:1 -9.444444e-01 -1.74913168 -0.1397572 0.0090679*
# 21:2-21:1  2.777778e-01 -0.52690945  1.0824650 0.9817545
# 22:2-21:1 -1.666667e-01 -0.97135390  0.6380206 0.9996190
# 3:2-22:1  -4.027778e-01 -1.09965736  0.2941018 0.6879552
# 11:2-22:1 -6.111111e-01 -1.41579834  0.1935761 0.3038343
#                                                           12:2-22:1 -1.305556e+00 -2.11024279 -0.5008683 0.0000374**
# 21:2-22:1 -8.333333e-02 -0.88802057  0.7213539 0.9999990
# 22:2-22:1 -5.277778e-01 -1.33246501  0.2769095 0.5157965
# 11:2-3:2  -2.083333e-01 -0.90521292  0.4885463 0.9933950
#                                                           12:2-3:2  -9.027778e-01 -1.59965736 -0.2058982 0.0023372**
# 21:2-3:2   3.194444e-01 -0.37743514  1.0163240 0.8952416
# 22:2-3:2  -1.250000e-01 -0.82187959  0.5718796 0.9998839
# 12:2-11:2 -6.944444e-01 -1.49913168  0.1102428 0.1530413
# 21:2-11:2  5.277778e-01 -0.27690945  1.3324650 0.5157965
# 22:2-11:2  8.333333e-02 -0.72135390  0.8880206 0.9999990
#                                                           21:2-12:2  1.222222e+00  0.41753499  2.0269095 0.0001482**
# 22:2-12:2  7.777778e-01 -0.02690945  1.5824650 0.0670469
# 22:2-21:2 -4.444444e-01 -1.24913168  0.3602428 0.7408488

str(Behav_R3)

# Behav_N1 <- subset(Behav_R3, Behav_R3$Numerosity == 1)
# str(Behav_N1)
# 
# Behav_N2 <- subset(Behav_R3, Behav_R3$Numerosity == 2)
# str(Behav_N2)
# 
# 
# pairwise.t.test(Behav_N1$Score, Behav_N1$Detailed_Disposition, paired = T, p.adjust.method = "bonferroni")
