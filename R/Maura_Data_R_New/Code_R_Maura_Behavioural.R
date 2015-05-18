#### Maura_Behavioural ####

Behav_d <- read.table("/Users/hit/Desktop/Maura_Data_R_New/Maura_Behavioural_no_sj6.txt", header = T)
str(Behav_d)


Behav_d$Subject <- as.factor(Behav_d$Subject)

str(Behav_d)

Behav_d$Disposition <- as.factor(Behav_d$Disposition) 

str(Behav_d)

Behav_d$Detailed_Disposition <- as.factor(Behav_d$Detailed_Disposition)

str(Behav_d)

Behav_d$Numerosity <- as.factor(Behav_d$Numerosity)

str(Behav_d)





Behav_R3 <- subset(Behav_d, Behav_d$Resp_Type == "R3")
str(Behav_R3)




tapply(Behav_R3$Score_Prova, Behav_R3$Stimulus, mean)


Figure1<-ggplot(Behav_R3,aes(x = Stimulus, y = Score, colour = Stimulus, group = Stimulus, fill = Stimulus)) # aes (VarIndependent, VarDependent)
Figure1+stat_summary(fun.y=mean,geom="histogram", aes(colour = Stimulus), linetype=1) + stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=0.2, colour = "Black")+stat_summary(fun.y=mean,geom="point",size=2.7,colour="Black")+labs(x="Exploratory Screens",y="Response Scores")+theme_bw()+theme(panel.grid.minor=element_line(colour="White"))+theme(panel.grid.major=element_line(colour="White"))+theme(legend.position="bottom")+theme(legend.key=element_rect(colour="White"))+theme(panel.border=element_rect(colour="White"))+theme(axis.line=element_line()) +theme(axis.title.x = element_text(size = 20)) + theme(axis.title.y=element_text(size = 20)) + theme(axis.text.x = element_text(size = 15)) + theme(axis.text.y=element_text(size = 15)) #+ scale_fill_brewer(palette="Set1")


tapply(Behav_R3$Score, Behav_R3$Stimulus, mean)
# mix14_IN mix14_OUT   mix9_IN  mix9_OUT   ran14_1   ran14_2    ran9_1    ran9_2  tot14_IN tot14_OUT   tot9_IN  tot9_OUT 
# 1.2222222 0.5277778 1.5277778 1.7500000 1.1944444 1.6666667 1.6666667 1.4444444 1.7500000 1.3055556 1.4722222 1.8333333 


aov1.out = aov(Behav_R3$Score ~ Behav_R3$Stimulus + Error(Subject/Stimulus), data=Behav_R3)


summary(aov1.out)
# 
# Error: Subject
#           Df Sum Sq Mean Sq F value Pr(>F)
# Residuals  8  5.522  0.6902               
# 
# Error: Subject:Stimulus
#                   Df Sum Sq Mean Sq F value   Pr(>F)    
# Behav_R3$Stimulus 11  12.74  1.1584   4.975 5.67e-06 ***
#   Residuals         88  20.49  0.2329                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 

TukeyHSD(aov(Behav_R3$Score ~ Behav_R3$Stimulus, data=Behav_R3))

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = Behav_R3$Score ~ Behav_R3$Stimulus, data = Behav_R3)
# 
# $`Behav_R3$Stimulus`
#                             diff         lwr       upr     p adj
# mix14_OUT-mix14_IN  -6.944444e-01 -1.51653858 0.1276497 0.1844389
# mix9_IN-mix14_IN     3.055556e-01 -0.51653858 1.1276497 0.9836482
# mix9_OUT-mix14_IN    5.277778e-01 -0.29431635 1.3498719 0.5884566
# ran14_1-mix14_IN    -2.777778e-02 -0.84987191 0.7943164 1.0000000
# ran14_2-mix14_IN     4.444444e-01 -0.37764969 1.2665386 0.8078859
# ran9_1-mix14_IN      4.444444e-01 -0.37764969 1.2665386 0.8078859
# ran9_2-mix14_IN      2.222222e-01 -0.59987191 1.0443164 0.9989257
# tot14_IN-mix14_IN    5.277778e-01 -0.29431635 1.3498719 0.5884566
# tot14_OUT-mix14_IN   8.333333e-02 -0.73876080 0.9054275 1.0000000
# tot9_IN-mix14_IN     2.500000e-01 -0.57209413 1.0720941 0.9969072
# tot9_OUT-mix14_IN    6.111111e-01 -0.21098302 1.4332052 0.3586731
                                                                    # mix9_IN-mix14_OUT    1.000000e+00  0.17790587 1.8220941 0.0051115**
                                                                    # mix9_OUT-mix14_OUT   1.222222e+00  0.40012809 2.0443164 0.0001704***
# ran14_1-mix14_OUT    6.666667e-01 -0.15542747 1.4887608 0.2343565
                                                                    # ran14_2-mix14_OUT    1.138889e+00  0.31679476 1.9609830 0.0006474**
                                                                    # ran9_1-mix14_OUT     1.138889e+00  0.31679476 1.9609830 0.0006474**
                                                                    # ran9_2-mix14_OUT     9.166667e-01  0.09457253 1.7387608 0.0157670*
                                                                    # tot14_IN-mix14_OUT   1.222222e+00  0.40012809 2.0443164 0.0001704**
# tot14_OUT-mix14_OUT  7.777778e-01 -0.04431635 1.5998719 0.0815204
# tot9_IN-mix14_OUT    9.444444e-01  0.12235031 1.7665386 0.0109464
                                                                    # tot9_OUT-mix14_OUT   1.305556e+00  0.48346142 2.1276497 0.0000423***
# mix9_OUT-mix9_IN     2.222222e-01 -0.59987191 1.0443164 0.9989257
# ran14_1-mix9_IN     -3.333333e-01 -1.15542747 0.4887608 0.9684725
# ran14_2-mix9_IN      1.388889e-01 -0.68320524 0.9609830 0.9999893
# ran9_1-mix9_IN       1.388889e-01 -0.68320524 0.9609830 0.9999893
# ran9_2-mix9_IN      -8.333333e-02 -0.90542747 0.7387608 1.0000000
# tot14_IN-mix9_IN     2.222222e-01 -0.59987191 1.0443164 0.9989257
# tot14_OUT-mix9_IN   -2.222222e-01 -1.04431635 0.5998719 0.9989257
# tot9_IN-mix9_IN     -5.555556e-02 -0.87764969 0.7665386 1.0000000
# tot9_OUT-mix9_IN     3.055556e-01 -0.51653858 1.1276497 0.9836482
# ran14_1-mix9_OUT    -5.555556e-01 -1.37764969 0.2665386 0.5088880
# ran14_2-mix9_OUT    -8.333333e-02 -0.90542747 0.7387608 1.0000000
# ran9_1-mix9_OUT     -8.333333e-02 -0.90542747 0.7387608 1.0000000
# ran9_2-mix9_OUT     -3.055556e-01 -1.12764969 0.5165386 0.9836482
# tot14_IN-mix9_OUT    0.000000e+00 -0.82209413 0.8220941 1.0000000
# tot14_OUT-mix9_OUT  -4.444444e-01 -1.26653858 0.3776497 0.8078859
# tot9_IN-mix9_OUT    -2.777778e-01 -1.09987191 0.5443164 0.9924056
# tot9_OUT-mix9_OUT    8.333333e-02 -0.73876080 0.9054275 1.0000000
# ran14_2-ran14_1      4.722222e-01 -0.34987191 1.2943164 0.7411559
# ran9_1-ran14_1       4.722222e-01 -0.34987191 1.2943164 0.7411559
# ran9_2-ran14_1       2.500000e-01 -0.57209413 1.0720941 0.9969072
# tot14_IN-ran14_1     5.555556e-01 -0.26653858 1.3776497 0.5088880
# tot14_OUT-ran14_1    1.111111e-01 -0.71098302 0.9332052 0.9999989
# tot9_IN-ran14_1      2.777778e-01 -0.54431635 1.0998719 0.9924056
# tot9_OUT-ran14_1     6.388889e-01 -0.18320524 1.4609830 0.2925931
# ran9_1-ran14_2      -4.440892e-16 -0.82209413 0.8220941 1.0000000
# ran9_2-ran14_2      -2.222222e-01 -1.04431635 0.5998719 0.9989257
# tot14_IN-ran14_2     8.333333e-02 -0.73876080 0.9054275 1.0000000
# tot14_OUT-ran14_2   -3.611111e-01 -1.18320524 0.4609830 0.9446656
# tot9_IN-ran14_2     -1.944444e-01 -1.01653858 0.6276497 0.9996937
# tot9_OUT-ran14_2     1.666667e-01 -0.65542747 0.9887608 0.9999321
# ran9_2-ran9_1       -2.222222e-01 -1.04431635 0.5998719 0.9989257
# tot14_IN-ran9_1      8.333333e-02 -0.73876080 0.9054275 1.0000000
# tot14_OUT-ran9_1    -3.611111e-01 -1.18320524 0.4609830 0.9446656
# tot9_IN-ran9_1      -1.944444e-01 -1.01653858 0.6276497 0.9996937
# tot9_OUT-ran9_1      1.666667e-01 -0.65542747 0.9887608 0.9999321
# tot14_IN-ran9_2      3.055556e-01 -0.51653858 1.1276497 0.9836482
# tot14_OUT-ran9_2    -1.388889e-01 -0.96098302 0.6832052 0.9999893
# tot9_IN-ran9_2       2.777778e-02 -0.79431635 0.8498719 1.0000000
# tot9_OUT-ran9_2      3.888889e-01 -0.43320524 1.2109830 0.9104183
# tot14_OUT-tot14_IN  -4.444444e-01 -1.26653858 0.3776497 0.8078859
# tot9_IN-tot14_IN    -2.777778e-01 -1.09987191 0.5443164 0.9924056
# tot9_OUT-tot14_IN    8.333333e-02 -0.73876080 0.9054275 1.0000000
# tot9_IN-tot14_OUT    1.666667e-01 -0.65542747 0.9887608 0.9999321
# tot9_OUT-tot14_OUT   5.277778e-01 -0.29431635 1.3498719 0.5884566
# tot9_OUT-tot9_IN     3.611111e-01 -0.46098302 1.1832052 0.9446656






tapply(Behav_R3$Score, list(Behav_R3$Detailed_Disposition, Behav_R3$Numerosity), mean)
#           1         2
# 3  1.555556 1.4305556
# 11 1.527778 1.2222222
# 12 1.750000 0.5277778
# 21 1.472222 1.7500000
# 22 1.833333 1.3055556


Figure2<-ggplot(Behav_R3,aes(x = Detailed_Disposition, y = Score, colour = Numerosity, group = Numerosity)) # aes (VarIndependent, VarDependent)
Figure2+stat_summary(fun.y=mean,geom="line", aes(colour = Numerosity), linetype= 1) + stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=0.2, aes(colour = Numerosity))+stat_summary(fun.y=mean,geom="point",size=2.7,aes(colour = Numerosity, shape = Numerosity))+labs(x="Disposition",y="Response Scores")+theme_bw()+theme(panel.grid.minor=element_line(colour="White"))+theme(panel.grid.major=element_line(colour="White"))+theme(legend.position="bottom")+theme(legend.key=element_rect(colour="White"))+theme(panel.border=element_rect(colour="White"))+theme(axis.line=element_line()) +theme(axis.title.x = element_text(size = 20)) + theme(axis.title.y=element_text(size = 20)) + theme(axis.text.x = element_text(size = 15)) + theme(axis.text.y=element_text(size = 15))



Figure3<-ggplot(Behav_R3,aes(x = Detailed_Disposition, y = Score, colour = Numerosity, group = Numerosity, fill = Numerosity)) # aes (VarIndependent, VarDependent)
Figure3+stat_summary(fun.y=mean,geom="histogram", aes(colour = Numerosity), linetype= 1, position = "dodge") + stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=0.2, colour = "Black", position=position_dodge(.9))+stat_summary(fun.y=mean,geom="point",size=2.7,colour = "Black", aes(shape = Numerosity), position=position_dodge(.9))+labs(x="Disposition",y="Response Scores")+theme_bw()+theme(panel.grid.minor=element_line(colour="White"))+theme(panel.grid.major=element_line(colour="White"))+theme(legend.position="bottom")+theme(legend.key=element_rect(colour="White"))+theme(panel.border=element_rect(colour="White"))+theme(axis.line=element_line()) +theme(axis.title.x = element_text(size = 20)) + theme(axis.title.y=element_text(size = 20)) + theme(axis.text.x = element_text(size = 15)) + theme(axis.text.y=element_text(size = 15)) + coord_cartesian(ylim=c(0, 2.5))





# Repeated Measures ANOVA - Dependent Variable = Average_Pupil_Diam on all the Page  
aov2.out = aov(Behav_R3$Score ~ Behav_R3$Detailed_Disposition + Behav_R3$Numerosity + Behav_R3$Detailed_Disposition:Behav_R3$Numerosity + Error(Subject/(Detailed_Disposition*Numerosity)), data=Behav_R3)


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





Figure4<-ggplot(Behav_R3,aes(x = Numerosity, y = Score, colour = Numerosity, group = Numerosity, fill = Numerosity)) # aes (VarIndependent, VarDependent)
Figure4+stat_summary(fun.y=mean,geom="histogram", aes(colour = Numerosity), linetype= 1, position = "dodge") + stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=0.2, colour = "Black", position=position_dodge(.9))+stat_summary(fun.y=mean,geom="point",size=2.7,colour = "Black", aes(shape = Numerosity), position=position_dodge(.9))+labs(x="Disposition",y="Response Scores")+theme_bw()+theme(panel.grid.minor=element_line(colour="White"))+theme(panel.grid.major=element_line(colour="White"))+theme(legend.position="bottom")+theme(legend.key=element_rect(colour="White"))+theme(panel.border=element_rect(colour="White"))+theme(axis.line=element_line()) +theme(axis.title.x = element_text(size = 20)) + theme(axis.title.y=element_text(size = 20)) + theme(axis.text.x = element_text(size = 15)) + theme(axis.text.y=element_text(size = 15)) + coord_cartesian(ylim=c(0, 2.5))



Figure5<-ggplot(Behav_R3,aes(x = Detailed_Disposition, y = Score, colour = Detailed_Disposition, group = Detailed_Disposition, fill = Detailed_Disposition)) # aes (VarIndependent, VarDependent)
Figure5+stat_summary(fun.y=mean,geom="histogram", aes(colour = Numerosity), linetype= 1, position = "dodge") + stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=0.2, colour = "Black", position=position_dodge(.9))+stat_summary(fun.y=mean,geom="point",size=2.7,colour = "Black", aes(shape = Numerosity), position=position_dodge(.9))+labs(x="Disposition",y="Response Scores")+theme_bw()+theme(panel.grid.minor=element_line(colour="White"))+theme(panel.grid.major=element_line(colour="White"))+theme(legend.position="bottom")+theme(legend.key=element_rect(colour="White"))+theme(panel.border=element_rect(colour="White"))+theme(axis.line=element_line()) +theme(axis.title.x = element_text(size = 20)) + theme(axis.title.y=element_text(size = 20)) + theme(axis.text.x = element_text(size = 15)) + theme(axis.text.y=element_text(size = 15)) + coord_cartesian(ylim=c(0, 2.5))

