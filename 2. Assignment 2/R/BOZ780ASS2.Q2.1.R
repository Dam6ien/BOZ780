library(tidyverse)
library(BBmisc)

ggplotReorderGroup <- function(df){
  col.names <- names(df)
  temp <- data.frame()
  
  for (i in col.names){
    temp <- rbind(temp,data.frame(label=i, value=unname(df[i])))
  }
  return(temp)
}

saveplotLatex <- function(ggplot,name, width = 8, height = 8){
  ggsave(plot = ggplot,
         device = "pdf", 
         filename = paste0("../Latex/",name,".pdf"),
         units = "cm", width = width, height = height)
}



radiation.data <- read.csv(file = "RadiationData.csv", sep = ";")

legendpos = "top"
fontsize = 10

num.bins = 10
alpha = 0.3
gg.zetall <- radiation.data %>% 
  ggplot()+
  geom_histogram(bins = num.bins,aes(x = zeta1,fill = "zeta1"),color="black",alpha = alpha)+
  geom_histogram(bins = num.bins,aes(x = zeta2,fill = "zeta2"),color="black",alpha = alpha)+
  geom_histogram(bins = num.bins,aes(x = zeta3,fill = "zeta3"),color="black",alpha = alpha)+
  geom_density(aes(y = ..count.., x = zeta1,color = "zeta1"),size = 1)+
  geom_density(aes(y = ..count.., x = zeta2,color = "zeta2"),size = 1)+
  geom_density(aes(y = ..count.., x = zeta3,color = "zeta3"),size = 1)+
  labs(y = 'Count', x = "Radition Amount")+
  theme_gray(base_size = fontsize, base_family = "serif")+
  theme(legend.title=element_blank(), legend.position=legendpos)+
  scale_fill_discrete(labels = c(paste("Zeta 1 ~N(",round(mean(radiation.data$zeta1),digits = 3),",",round(sd(radiation.data$zeta1),digits = 3),")",sep = ""),
                                 c(paste("Zeta 2 ~N(",round(mean(radiation.data$zeta2),digits = 3),",",round(sd(radiation.data$zeta2),digits = 3),")",sep = "")),
                                 c(paste("Zeta 3 ~N(",round(mean(radiation.data$zeta3),digits = 3),",",round(sd(radiation.data$zeta3),digits = 3),")",sep = ""))))+
  guides(color = FALSE, linetype =FALSE)

num.bins = 10
alpha = 0.3
gg.eta23 <- radiation.data %>% 
  ggplot()+
  geom_histogram(bins = num.bins,aes(x = eta2,fill = "zeta2"),color="black",alpha = alpha)+
  geom_histogram(bins = num.bins,aes(x = eta3,fill = "zeta3"),color="black",alpha = alpha)+
  geom_density(aes(y = ..count.., x = eta2,color = "zeta2"),size = 1)+
  geom_density(aes(y = ..count.., x = eta3,color = "zeta3"),size = 1)+
  labs(y = 'Count', x = "Radition Amount")+
  theme_gray(base_size = fontsize, base_family = "serif")+
  theme(legend.title=element_blank(), legend.position=legendpos)+
  scale_fill_discrete(labels = c(c(paste("Eta 2 ~N(",round(mean(radiation.data$eta2),digits = 3),",",round(sd(radiation.data$eta2),digits = 3),")",sep = "")),
                                 c(paste("Eta 3 ~N(",round(mean(radiation.data$eta3),digits = 3),",",round(sd(radiation.data$eta3),digits = 3),")",sep = ""))))+
  guides(color = FALSE, linetype =FALSE)


num.bins1 = 5
alpha = 0.3
gg.eta1 <- radiation.data %>% 
  ggplot()+
  geom_histogram(bins = num.bins1,aes(x = eta1,fill = "zeta1"),color="black",alpha = alpha)+
  geom_density(aes(y = ..count../5, x = eta1,color = "zeta1"),size = 1)+
  labs(y = 'Count', x = "Radition Amount")+
  theme_gray(base_size = fontsize, base_family = "serif")+
  theme(legend.title=element_blank(), legend.position=legendpos)+
  scale_fill_discrete(labels = c(paste("Eta 1 ~N(",round(mean(radiation.data$eta1),digits = 3),",",round(sd(radiation.data$eta1),digits = 3),")",sep = "")))+
  guides(color = FALSE, linetype =FALSE)


gg.eta1
gg.eta23
gg.zetall

saveplotLatex(gg.eta1, "gg.eta1")
saveplotLatex(gg.eta23, "gg.eta23")
saveplotLatex(gg.zetall,"gg.zetall", width = 18,height=9)

round(sd(radiation.data$zeta1)^2,digits = 3)
round(sd(radiation.data$zeta2)^2,digits = 3)
round(sd(radiation.data$zeta3)^2,digits = 3)
round(sd(radiation.data$eta1)^2,digits = 3)
round(sd(radiation.data$eta2)^2,digits = 3)
round(sd(radiation.data$eta3)^2,digits = 3)


# Question 2d approx approach calculations
m <- 2
alpha <- 0.01 
omega <- sqrt(2*log(m/alpha))
omega
# Sigma
sum((summarise_all(radiation.data,"sd")^2)[c(1,2,3)])^(0.5)*omega
sum((summarise_all(radiation.data,"sd")^2)[c(4,5,6)])^(0.5)*omega

#Fixed component
sum((summarise_all(radiation.data,"mean"))[c(1,2,3)])
sum((summarise_all(radiation.data,"mean"))[c(4,5,6)])


(summarise_all(radiation.data,"mean"))[c(1,2,3)]+(summarise_all(radiation.data,"sd")^2)[c(1,2,3)]^(0.5)*omega
(summarise_all(radiation.data,"mean"))[c(4,5,6)]+(summarise_all(radiation.data,"sd")^2)[c(4,5,6)]^(0.5)*omega
       