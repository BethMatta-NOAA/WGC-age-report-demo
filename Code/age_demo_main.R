#AFSC Age and Growth Program
##Summary data report

library(tidyverse)
library(FSA)
library(patchwork)



# READ IN DATA ------------------------------------------------------------

#example is based on 2018 walleye pollock Bering Sea bottom trawl survey data
data <- read.csv("Data/age_demo_data.csv")
#historical pollock age agreement data for comparison
hist.data.species <- read.csv("Data/age_demo_historical_agree.csv")



# COMPUTE PRECISION AND BIAS STATISTICS -----------------------------------

collection.name <- unique(as.character(paste(str_to_title(data$common_name), 
                                             data$collection_year,
                                             data$region,
                                             str_to_title(data$collection_type), sep = " ")))


#Filter data for read-test information, compute difference between reader and tester ages
trad.testing <- data %>% 
  filter(!is.na(test_age) & !is.na(read_age)) %>% 
  rowwise() %>% 
  mutate(diff_age = read_age-test_age,
         bias_dir = ifelse(read_age == test_age, "equal", 
                           ifelse(read_age > test_age, "plus", 
                                  ifelse(read_age < test_age, "minus", "other"))))

#https://fishr-core-team.github.io/fishR/blog/posts/2015-9-5_Age_Comparison_Results/
precision <- agePrecision(~read_age + test_age, data=trad.testing)
trad.testing <- cbind(trad.testing, precision$detail[3:10])


bias <- ageBias(read_age ~ test_age, data=trad.testing)
#bowker <- summary(bias, what = "Bowker")
#evanshoenig <- summary(bias, what = "EvansHoenig")
agree <- nrow(trad.testing[trad.testing$bias_dir == "equal",])
agree.per <- round(nrow(trad.testing[trad.testing$bias_dir == "equal",])/
                     precision$validn*100, 1)
minus.bias <- nrow(trad.testing[trad.testing$bias_dir == "minus",])
minus.bias.per <- round(nrow(trad.testing[trad.testing$bias_dir == "minus",])/
                          precision$validn*100, 1)
plus.bias <- nrow(trad.testing[trad.testing$bias_dir == "plus",])
plus.bias.per <- round(nrow(trad.testing[trad.testing$bias_dir == "plus",])/
                          precision$validn*100, 1)

#overall mean difference
overall.mean.diff = round(mean(trad.testing$diff_age, na.rm = TRUE),2)



# AGREE PLOTS -------------------------------------------------------------

#Bubble diagonal age bias plot
bubble.diag <- ggplot(trad.testing, aes(read_age, test_age)) +
  geom_abline(intercept=0, slope=1) +
  geom_smooth(method="gam", formula = y ~ s(x, bs='tp', k = 5),
              linetype="dashed", color="red", fill="pink") +
  geom_count(color="black", fill="gray", shape=21) +
  coord_fixed(xlim = c(min(trad.testing$read_age, trad.testing$test_age), 
                       max(trad.testing$read_age, trad.testing$test_age)),
              ylim = c(min(trad.testing$read_age, trad.testing$test_age), 
                       max(trad.testing$read_age, trad.testing$test_age))) +
  labs(x="\nRead Age (yr)", y="Test Age (yr)\n") +
  theme_bw()


#CV by age
#mean CV vs final age
cv.mean.age <- trad.testing[!is.na(trad.testing$read_age),] %>% 
  group_by(final_age) %>% 
  summarize(mean.cv = mean(CV),
            median.cv = median(CV),
            count = n(),
            se.cv = (mean.cv/sqrt(2*count))) %>%
  ggplot(aes(final_age, mean.cv, size=count)) +
  geom_line(size=0.5, color="darkgray") +
  geom_hline(yintercept = mean(trad.testing$CV, na.rm=TRUE), 
             linetype="dashed", color="red") +
  geom_point() +
  geom_errorbar(aes(ymin=(mean.cv-se.cv), ymax=(mean.cv+se.cv)), size=0.5) +
  labs(x="\nFinal age (yr)", y="Mean CV (%)\n") +
  theme_bw()


#Compare to historical data for same species
PA.vs.age <- ggplot(hist.data.species, aes(ave_test_age, percent_agree)) +
  geom_smooth(method="gam", formula = y ~ s(x, bs='tp', k = 5),
              color="lightblue", se=FALSE) +
  geom_point(color="gray50") +
  stat_ellipse(linetype="dashed", level=0.95) +
  stat_ellipse(level=0.99, geom="polygon", alpha=0.1) +
  geom_point(x=mean(trad.testing$test_age), y=precision$PercAgree, color="red", size=4) +
  labs(x="\nAverage test age (yr)", y="Percent agreement (%)\n") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

CV.vs.age <- ggplot(hist.data.species, aes(ave_test_age, cv)) +
  geom_smooth(method="gam", formula = y ~ s(x, bs='tp', k = 5),
              color="lightblue", se=FALSE) +
  geom_point(color="gray50") +
  stat_ellipse(linetype="dashed", level=0.95) +
  stat_ellipse(level=0.99, geom="polygon", alpha=0.1) +
  geom_point(x=mean(trad.testing$test_age), y=precision$ACV, color="red", size=4) +
  labs(x="\nAverage test age (yr)", y="Average CV (%)\n") +
  theme_bw()

hist.compare <- PA.vs.age/CV.vs.age


# RANGES PLOTS -----------------------------------------------
#von Bertalanffy (length/weight at age)
rename.sex <- c('1' = "Male",
                '2' = "Female", 
                '3' = "Unknown")


lw.plot <- ggplot(data, aes(length, weight, colour = factor(sex))) +
  geom_point(alpha=0.5) +
  #facet_grid(factor(sex)~.) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  #scale_color_discrete(labels = rename.sex) +
  scale_color_manual(values = c("#1b64d1", "#e01d65", "darkgray"),
                     labels = rename.sex) +
  labs(x="\nFish length (mm)", y="Fish weight (g)\n", color="Sex", shape= "Sex") +
  theme_bw()

vonB.bubble <- ggplot(data[data$sex != 3,], 
       aes(x=jitter(final_age, 0), y=length, colour = factor(sex)))+
  geom_count(alpha=0.5) +
  scale_color_manual(values = c("#1b64d1", "#e01d65"),
                     labels = rename.sex) +
  labs(x="\nAge (yr)", y="Length (mm)\n", color = "Sex") +
  geom_smooth(method = "nls",
              se = FALSE,
              method.args = list(formula = y ~Linf*(1-exp(-k*(x-t0))),
                                 start = list(Linf = 1100, k = 0.2, t0=-1))) +
  theme_bw()


#find b for von Bertalanffy weight at age
WL <- lm(log(weight) ~ log(length), data)
WLcoef <- coef(summary(WL))
b <- WLcoef[2]

vonB.bubble.wt <- ggplot(data[data$sex != 3,], 
                      aes(x=jitter(final_age, 0), y=weight, colour = factor(sex)))+
  geom_count(alpha=0.5) +
  scale_color_manual(values = c("#1b64d1", "#e01d65"),
                     labels = rename.sex) +
  labs(x="\nAge (yr)", y="Weight (g)\n", color = "Sex") +
  geom_smooth(method = "nls",
              se = FALSE,
              method.args = list(formula = y ~ (Winf*(1-exp(-k*(x-t0))))^b,
                                 start = list(Winf = 1100, k = 0.2, t0=-1))) +
  theme_bw()

#both von B plots in one panel
vonBs <- vonB.bubble/vonB.bubble.wt + plot_layout(guides = "collect")



#Distributions of length and age

age.dist <- ggplot(data, aes(final_age)) +
  geom_histogram(binwidth=1, color="black", fill="#4287f5") +
  geom_vline(xintercept = median(data$final_age, na.rm = TRUE), color="black") +
  geom_vline(xintercept = mean(data$final_age, na.rm = TRUE), linetype="dashed", color="red") +
  scale_y_continuous(labels = scales::comma) +
  labs(x="\nAge (yr)", y="Number of fish\n") +
  theme_bw()

  
length.dist <- ggplot(data, aes(length)) +
  geom_histogram(binwidth=10, color="black", fill="#04876f") +
  geom_vline(xintercept = median(data$length, na.rm = TRUE), color="black") +
  geom_vline(xintercept = mean(data$length, na.rm = TRUE), linetype="dashed", color="red") +
  scale_y_continuous(labels = scales::comma) +
  labs(x="\nLength (mm)", y="Number of fish\n") +
  theme_bw()

dist.plot <- age.dist/length.dist

