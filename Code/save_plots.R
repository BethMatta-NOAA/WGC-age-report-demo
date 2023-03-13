#AGE DEMO
#Save plots as separate files

source("Code/age_demo_main.R")


#Agreement plots
ggsave("Outputs/diagonal_bubble.png", plot=bubble.diag, dpi=300)
ggsave("Outputs/CV_mean_age.png", plot=cv.mean.age, dpi=300)
ggsave("Outputs/compare_historical.png", plot=hist.compare, dpi=300)


#Data properties plots
ggsave("Outputs/lw.png", plot=lw.plot, dpi=300)
ggsave("Outputs/vonB.png", plot=vonBs, dpi=300)
ggsave("Outputs/length_age_dist.png", plot=dist.plot, dpi=300)