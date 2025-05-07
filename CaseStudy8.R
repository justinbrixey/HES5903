library(BasketballAnalyzeR)
library(tidyverse)

rm(list=ls())
PbP <- PbPmanipulation(PbP.BDB)
subdata <- subset(PbP, team == "OKC")
subdata$xx <- subdata$original_x/10
subdata$yy <- subdata$original_y/10-41.75

shotchart(data=subdata, x="xx", y="yy", type = "density-polygons", scatter = F, pt.col="tomato", pt.alpha=0.1)

team <- shotchart(data=subdata, x="xx", y="yy", type="density-polygons", bg.col="#FFE4C4", courtline.col="black", palette = "bwr") +
  ggtitle("OKC Thunder Shot Heatmap")
PG <- shotchart(data=filter(subdata, player == "Paul George"), x="xx", y="yy", type="density-polygons", bg.col="#FFE4C4", courtline.col="black", palette = "bwr") +
  ggtitle("Paul George Shot Heatmap")
RW <- shotchart(data=filter(subdata, player == "Russell Westbrook"), x="xx", y="yy", type="density-polygons", bg.col="#FFE4C4", courtline.col="black", palette = "bwr") +
  ggtitle("Russell Westbrook Shot Heatmap")

combined_plot <- team/PG/RW
print(combined_plot)

PbP.OKC <- subset(PbP, team == "OKC")
mypal <- colorRampPalette(c("red", "green"))
expectedpts(data=PbP.OKC, col.team = "gray", palette = mypal,col.hline = "gray")





exp_df <- expectedpts(data=filter(subdata, player=="Paul George"), bw=2)
threshold <- 1.05
max_eff_dist <- max(exp_df$distance[exp_df$exp_points >= threshold])


library(ggplot2)
ggplot(exp_df, aes(x = distance, y = exp_points)) +
  geom_line() +
  geom_hline(yintercept = threshold, linetype="dashed", color="red") +
  labs(x="Shot Distance (ft)", y="Expected Points", 
       title="Paul George: Expected Points vs. Distance")
