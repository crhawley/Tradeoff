
##################################
##      Baboon Tradeoff Code    ##
##################################

#libraries 
library(tidyverse)
library(brms)
library(rstanarm)
library(rstan)
library(ggmcmc)
library(ggthemes)
library(ggridges)
library(sjPlot)
library(grid)
library(gridExtra)

#set theme for ggplotting 
my_theme<-set_theme(theme_classic()+
                      theme(plot.title = element_text(hjust=.5, size=14, face= "bold", colour= "black" ),
                            axis.title.x = element_text(size=20, face="bold", colour = "black"),    
                            axis.title.y = element_text(size=16, face="bold", colour = "black"),    
                            axis.text.x = element_text(size=16, face="bold", colour = "black"), 
                            axis.text.y = element_text(size=12, colour = "black"),
                            strip.text.x = element_text(size = 10, face="bold", colour = "black" ),
                            strip.text.y = element_text(size = 10, face="bold", colour = "black"),
                            axis.line.x = element_line(color="black", size = 0.7),
                            axis.line.y = element_line(color="black", size = 0.7),
                      ))




#Model 1 (Male Approaches to Sexually Receptive Females by PAs)

#Data Cleaning~Model 1


Part3<-read.csv("MastersPart3ByMaleByDay.csv")
Part3$Z.Rank<-scale(Part3$rank, center=TRUE, scale=TRUE)
#remove males who were there shortly/didn't have ranks, remove NAs 
#create new vars for number of mins and hours observed daily
Part3<-Part3 %>%
  filter(male != "af") %>%
  filter(male != "hn") %>%
  filter(n_fs>0) %>%
  mutate(grp=as_factor(grp)) %>%
  mutate(obsmins = nobs*15) %>%
  mutate(obshours = obsmins/60) %>%
  filter(Z.Rank != "n/a") %>%
  filter(grp != "n/a") %>%
  filter(nprimassoc != "n/a")



#S1 Table For Approaches By Male


MaleDemoTable <- Part3 %>% 
  group_by(male) %>% 
  summarise(
    observations = sum(nobs, na.rm = TRUE),
    aver_rank = mean(elo,na.rm=TRUE),
    aver_primaryassociations=mean(nprimassoc, na.rm=TRUE)
  )
print(MaleDemoTable)



#Model and Diagnostics~~~~Model 1 


Part3_zinb1 <- brm(a1fs ~ 1 + nprimassoc +  (1 + nprimassoc|male) + Z.Rank + n_fs + grp + offset(log(obshours)),
                   data = Part3, family = zero_inflated_negbinomial(),
                   prior=c(set_prior("normal(0,2)", class = "b", coef="nprimassoc"),
                           set_prior("normal(0,2)", class = "b", coef="Z.Rank"),
                           set_prior("normal(0,2)", class = "b", coef="grpENK"),
                           set_prior("cauchy(0,2)", class = "b", coef="n_fs"),
                           set_prior("normal(0,2)", class ="Intercept"),
                           set_prior("cauchy(0,2)", class = "sd",group = "male"),
                           set_prior("gamma(0.01, 0.01)", class = "shape"),
                           set_prior("beta(2, 2)", class = "zi")),
                   chains=3, iter=6000, thin=2, warmup=1000, control = list(adapt_delta = 0.99))
summary(Part3_zinb1)
#Chains:
(stan_trace(Part3_zinb1$fit, ncol=4))
#Autocorrelation:
stan_ac(Part3_zinb1$fit)
#Step-size:
summary(do.call(rbind, args = get_sampler_params(Part3_zinb1$fit, inc_warmup = FALSE)), digits = 2)
#Step-size graphs:
stan_diag(Part3_zinb1$fit)
#Coefficients plot:
mcmc_plot(Part3_zinb1)
#Main effects:
conditional_effects(Part3_zinb1)



#Predictive Data Frames Across Ranks~~~~Model 1, APPROACHES

#PART 3 new.d and predicted df for LOW RANKERS
new.d.Part3.low<-data.frame(nprimassoc=c(0,1,2,3,4,5), Z.Rank=c(-1, -1, -1, -1, -1, -1), grp=NA, focal=NA, n_fs=rep(mean(Part3$n_fs),6), obshours=rep(1,6))
new.d.Part3.low$Z.Rank<-as.numeric(new.d.Part3.low$Z.Rank)
P.Part3.low<- fitted(Part3_zinb1, newdata = new.d.Part3.low,  re_formula = NA,   scale = c("response"),ndraws=1000,summary=FALSE,offset=TRUE)
P.Part3.low<-as.data.frame(P.Part3.low)
#PART 3 new.d and predicted df for MID RANKERS
new.d.Part3.mid<-data.frame(nprimassoc=c(0,1,2,3,4,5), Z.Rank=c(0,0,0,0,0,0), grp=NA, focal=NA, n_fs=rep(mean(Part3$n_fs),6), obshours=rep(1,6))
new.d.Part3.mid$Z.Rank<-as.numeric(new.d.Part3.mid$Z.Rank)
P.Part3.mid<- fitted(Part3_zinb1, newdata = new.d.Part3.mid,  re_formula = NA,   scale = c("response"),ndraws=1000,summary=FALSE,offset=TRUE)
P.Part3.mid<-as.data.frame(P.Part3.mid)
#PART 3 new.d and predicted df for HIGH RANKERS
new.d.Part3.high<-data.frame(nprimassoc=c(0,1,2,3,4,5), Z.Rank=c(1,1,1,1,1,1), grp=NA, focal=NA, n_fs=c(3,3,3,3,3,3), obshours=rep(1,6))
new.d.Part3.high$Z.Rank<-as.numeric(new.d.Part3.high$Z.Rank)
P.Part3.high<- fitted(Part3_zinb1, newdata = new.d.Part3.high,  re_formula = NA,   scale = c("response"),ndraws=1000,summary=FALSE,offset=TRUE)
P.Part3.high<-as.data.frame(P.Part3.high)



#Posterior Predictive Density Graph~~~~Model 1, APPROACHES, High Rank, PAs(2+, 2, 1, 0)

Part3.ManyPAs<-P.Part3.high %>%
  ggplot() +
  geom_density(aes(x = P.Part3.high[,5]), size = 0, fill = "red4", alpha = .75)+
  my_theme +
  coord_cartesian(xlim=c(.5,8), ylim=c(0,1.2))+
  annotate("point", x=12.8, y=.8, color="red4", size=6, alpha=.3) +
  annotate("text", x = 7, y = .8, label = "2+ Primary Associations", color = "black", size=8, fontface="bold") +
  geom_vline(xintercept = median(P.Part3.high[,5]), linetype="solid", color="red4", alpha=1, size=1.5) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=20, colour="black"))
Part3.2PAs<-P.Part3.high %>%
  ggplot() +
  geom_density(aes(x = P.Part3.high[,3]), size = 0, fill = "darkcyan", alpha = .75)+
  my_theme +
  coord_cartesian(xlim=c(.5,8), ylim=c(0,1))+
  annotate("point", x=12.8, y=.8, color="darkcyan", size=6, alpha=.3)+
  annotate("text", x = 7, y = .8, label = "2 Primary Associations", color = "black", size=8, fontface="bold") +
  geom_vline(xintercept = median(P.Part3.high[,3]), linetype="solid", color="darkcyan", alpha=1, size=1.5) + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=20, colour="black"))
Part3.1PAs<-P.Part3.high %>%
  ggplot() +
  geom_density(aes(x = P.Part3.high[,2]), size = 0, fill = "orange3", alpha = .75)+
  my_theme +
  coord_cartesian(xlim=c(.5,8), ylim=c(0,1))+
  annotate("point", x=12.8, y=.8, color="orange3", size=6, alpha=.3)+
  annotate("text", x = 7, y = .8, label = "1 Primary Association", color = "black", size=8, fontface="bold") +
  geom_vline(xintercept = median(P.Part3.high[,2]), linetype="solid", color="orange3", alpha=1, size=1.5) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=20, colour="black"))
Part3.0PAs<-P.Part3.high %>%
  ggplot() +
  geom_density(aes(x = P.Part3.high[,1]), size = 0, fill = "blue4", alpha = .75)+
  my_theme+
  coord_cartesian(xlim=c(.5,8), ylim=c(0,1))+
  annotate("point", x=12.8, y=.8, color="blue4", size=6, alpha=.3)+
  annotate("text", x = 7, y = .8, label = "No Primary Associations", color = "black", size=8, fontface="bold") +
  labs(x="Approaches to Receptive Females Per Hour", size=24)+
  geom_vline(xintercept = median(P.Part3.high[,1]), linetype="solid", color="blue4", alpha=1, size=1.5)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(size=24, colour = "black"),
        axis.text.x=element_text(size=20, colour="black"))

plot<-grid.arrange(Part3.ManyPAs, Part3.2PAs, Part3.1PAs, Part3.0PAs, ncol = 1,
                   top = textGrob("",
                                  gp=gpar(fontsize=15,fontface="bold")))
plot



#Posterior Predictive Density Graph~~~~Model 1, APPROACHES, Mean Rank, PAs(2+, 2, 1, 0)


Part3.ManyPAs<-P.Part3.mid %>%
  ggplot() +
  geom_density(aes(x = P.Part3.mid[,5]), size = 0, fill = "red4", alpha = .75)+
  my_theme +
  coord_cartesian(xlim=c(.5,8), ylim=c(0,3.2))+
  annotate("point", x=12.8, y=.8, color="red4", size=6, alpha=.3)+
  annotate("text", x = 7, y = .8, label = "2+ Primary Associations", color = "black", size=8, fontface="bold") +
  geom_vline(xintercept = median(P.Part3.mid[,5]), linetype="solid", color="red4", alpha=1, size=1.5) + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=20, colour="black"))
Part3.2PAs<-P.Part3.mid %>%
  ggplot() +
  geom_density(aes(x = P.Part3.mid[,3]), size = 0, fill = "darkcyan", alpha = .75)+
  my_theme+
  coord_cartesian(xlim=c(.5,8), ylim=c(0,2))+
  annotate("point", x=12.8, y=.8, color="darkcyan", size=6, alpha=.3)+
  annotate("text", x = 7, y = .8, label = "2 Primary Associations", color = "black", size=8, fontface="bold") +
  geom_vline(xintercept = median(P.Part3.mid[,3]), linetype="solid", color="darkcyan", alpha=1, size=1.5) + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=20, colour="black"))
Part3.1PAs<-P.Part3.mid %>%
  ggplot() +
  geom_density(aes(x = P.Part3.mid[,2]), size = 0, fill = "orange3", alpha = .75)+
  my_theme+
  coord_cartesian(xlim=c(.5,8), ylim=c(0,2))+
  annotate("point", x=12.8, y=.8, color="orange3", size=6, alpha=.3)+
  annotate("text", x = 7, y = .8, label = "1 Primary Association", color = "black", size=8, fontface="bold") +
  geom_vline(xintercept = median(P.Part3.mid[,2]), linetype="solid", color="orange3", alpha=1, size=1.5) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=20, colour="black"))
Part3.0PAs<-P.Part3.mid %>%
  ggplot() +
  geom_density(aes(x = P.Part3.mid[,1]), size = 0, fill = "blue4", alpha = .75)+
  my_theme+
  coord_cartesian(xlim=c(.5,8), ylim=c(0,1.5))+
  annotate("point", x=12.8, y=.8, color="blue4", size=6, alpha=.3)+
  annotate("text", x = 7, y = .8, label = "No Primary Associations", color = "black", size=8, fontface="bold") +
  labs(x="Approaches to Receptive Females Per Hour", size=24)+
  geom_vline(xintercept = median(P.Part3.mid[,1]), linetype="solid", color="blue4", alpha=1, size=1.5)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(size=24, colour = "black"),
        axis.text.x=element_text(size=20, colour="black"))

plot<-grid.arrange(Part3.ManyPAs, Part3.2PAs, Part3.1PAs, Part3.0PAs, ncol = 1,
                   top = textGrob("",
                                  gp=gpar(fontsize=15,fontface="bold")))
plot



#Posterior Predictive Density Graph~~~~Model 1, APPROACHES, Low Rank, PAs(2+, 2, 1, 0)

Part3.ManyPAs<-P.Part3.low %>%
  ggplot() +
  geom_density(aes(x = P.Part3.low[,5]), size = 0, fill = "red4", alpha = .75)+
  my_theme+
  coord_cartesian(xlim=c(.5,8), ylim=c(0,6))+
  annotate("point", x=12.8, y=.8, color="red4", size=6, alpha=.3)+
  annotate("text", x = 7, y = .8, label = "2+ Primary Associations", color = "black", size=8, fontface="bold") +
  geom_vline(xintercept = median(P.Part3.low[,5]), linetype="solid", color="red4", alpha=1, size=1.5) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=20, colour="black"))
Part3.2PAs<-P.Part3.low %>%
  ggplot() +
  geom_density(aes(x = P.Part3.low[,3]), size = 0, fill = "darkcyan", alpha = .75)+
  my_theme+
  coord_cartesian(xlim=c(.5,8), ylim=c(0,4))+
  annotate("point", x=12.8, y=.8, color="darkcyan", size=6, alpha=.3)+
  annotate("text", x = 7, y = .8, label = "2 Primary Associations", color = "black", size=8, fontface="bold") +
  geom_vline(xintercept = median(P.Part3.low[,3]), linetype="solid", color="darkcyan", alpha=1, size=1.5) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=20, colour="black"))
Part3.1PAs<-P.Part3.low %>%
  ggplot() +
  geom_density(aes(x = P.Part3.low[,2]), size = 0, fill = "orange3", alpha = .75)+
  my_theme+
  coord_cartesian(xlim=c(.5,8), ylim=c(0,3))+
  annotate("point", x=12.8, y=.8, color="orange3", size=6, alpha=.3)+
  annotate("text", x = 7, y = .8, label = "1 Primary Association", color = "black", size=8, fontface="bold") +
  geom_vline(xintercept = median(P.Part3.low[,2]), linetype="solid", color="orange3", alpha=1, size=1.5) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=20, colour="black"))
Part3.0PAs<-P.Part3.low %>%
  ggplot() +
  geom_density(aes(x = P.Part3.low[,1]), size = 0, fill = "blue4", alpha = .75)+
  my_theme+
  coord_cartesian(xlim=c(.5,8), ylim=c(0,2.5))+
  annotate("point", x=12.8, y=.8, color="blue4", size=6, alpha=.3)+
  annotate("text", x = 7, y = .8, label = "No Primary Associations", color = "black", size=8, fontface="bold") +
  labs(x="Approaches to Receptive Females Per Hour", size=24)+
  geom_vline(xintercept = median(P.Part3.low[,1]), linetype="solid", color="blue4", alpha=1, size=1.5)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(size=24, colour = "black"),
        axis.text.x=element_text(size=20, colour="black"))

plot<-grid.arrange(Part3.ManyPAs, Part3.2PAs, Part3.1PAs, Part3.0PAs, ncol = 1,
                   top = textGrob("",
                                  gp=gpar(fontsize=15,fontface="bold")))
plot




# Model 2 (Proportion of male approaches to PA by number of PAs)

#Data Cleaning~Model 2

Part4<-read.csv("MastersPart4ProportionalApproaches.csv")
Part4$dyad_ID<-paste0(Part4$male, "_", Part4$partner)
#DATA CLEANING:
Part4$Z.Rank<-scale(Part4$male_elo, center=TRUE, scale=TRUE)
Part4$appdummy<-as.numeric(Part4$appdummy)
Part4<-Part4 %>% 
  mutate(grp=as_factor(grp)) %>%
  mutate(obsmins = nobs*15) %>%
  mutate(obshours = obsmins/60) %>%
  filter(Z.Rank != "n/a") %>%
  filter(grp != "n/a") %>%
  filter(nprimassoc != "n/a") %>%
  filter(grp != "YNT") %>%
  filter(PACat > 0)
#problem with line 2464 so removed
Part4 <- Part4[-c(2464),]
Part4 <- Part4[-c(1921),]



#Model and Diagnostics~~~~~Model 2 

Part4_binom1 <- brm(appdummy | trials(3357) ~ PACat + (1 + PACat|male) + (1 + PACat|partner) + (1 + PACat|dyad_ID) +  Z.Rank + grp  + (1|date),
                    data = Part4, family = binomial(link="logit"),
                    prior=c(set_prior("normal(0,2)", class = "b", coef="PACat"),
                            set_prior("normal(0,2)", class = "b", coef="Z.Rank"),
                            set_prior("normal(0,2)", class = "b", coef="grpENK"),
                            set_prior("normal(0,2)", class ="Intercept"),
                            set_prior("cauchy(0,2)", class = "sd", group = "male"),
                            set_prior("cauchy(0,2)", class = "sd", group = "partner"),
                            set_prior("exponential(1)", class = "sd")),
                    chains=3, iter=6000, thin=2, init_r=.1, inits="0", warmup=1000, control = list(adapt_delta = 0.99))
#summary
summary(Part4_binom1)
#just the chains:
(stan_trace(Part4_binom1$fit, ncol=4))
#Autocorrelation
stan_ac(Part4_binom1$fit)
#step-size
summary(do.call(rbind, args = get_sampler_params(Part4_binom1$fit, inc_warmup = FALSE)), digits = 2)
#step-size graphs
stan_diag(Part4_binom1$fit)
###### BASIC GRAPHS
#effects coef plot:
mcmc_plot(Part4_binom1)
#basic graphs of the effects:
conditional_effects(Part4_binom1)


#Predictive Data Frame at Mid Rank~~~~Model 2

new.d.Part4<-data.frame(PACat=c(1,2,3), Z.Rank=c(0, 0, 0), grp=NA, male=NA, partner=NA, dyad_ID=NA, obshours=rep(1,3))
new.d.Part4$Z.Rank<-as.numeric(new.d.Part4$Z.Rank)
P.Part4<-fitted(Part4_binom1, newdata = new.d.Part4,  re_formula = NA,   scale = c("response"),ndraws=1000,summary=FALSE,offset=TRUE)
P.Part4<-as.data.frame(P.Part4)



#Posterior Predictive Density Graph~~~~Model 2, APPROACHES, Mid Rank, PAs(2+, 2, 1)

P.Part4 %>%
  ggplot() +
  geom_density(aes(x = P.Part4[,1]), size = 0, fill = "darkred", alpha = .75, ) +
  geom_vline(xintercept = median(P.Part4[,1]), linetype="solid", color="darkred", alpha=.75, size=1.5)+
  geom_density(aes(x = P.Part4[,2]), size = 0, fill = "darkcyan", alpha = .75) +
  geom_vline(xintercept = median(P.Part4[,2]), linetype="solid", color="darkcyan", alpha=.75, size=1.5)+
  geom_density(aes(x = P.Part4[,3]), size = 0, fill = "blue4", alpha = .75) +
  geom_vline(xintercept = median(P.Part4[,3]), linetype="solid", color="blue4", alpha=.75, size=1.5)+
  coord_cartesian(xlim=c(0,1), ylim=c(0,15))+
  labs(title = "",
       x="Proportion of Male Initiated Approaches to Primary Associates")+
  my_theme + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(size=24, colour = "black"),
        axis.text.x=element_text(size=20, colour="black"))+
  #annotate("point", x=1, y=2.5, color="darkred", size=10, alpha=.75)+
  annotate("text", x = .9, y = 3, label = "1 Primary Associate", color = "darkred", alpha=.75, size=8, fontface="bold") +
  #annotate("point", x=2.07, y=3.1, color="darkcyan", size=10, alpha=.75)+
  annotate("text", x = .9, y = 8, label = "2 Primary Associates", color = "darkcyan", size=8, fontface="bold")+
  #annotate("point", x=1.25, y=1.7, color="blue4", size=10, alpha=.75)+
  annotate("text", x = .9, y = 13, label = "2+ Primary Associates", color = "blue4", size=8, fontface="bold")
