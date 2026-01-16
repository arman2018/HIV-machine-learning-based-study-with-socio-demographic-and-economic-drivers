##Our working directory 
setwd("D:/Research/HIV_BD Research")

##Required libraries
library(readxl)
library(ggplot2)
library(gridExtra)

#Shap plot for HIV infected cases
data1<-read_excel("mean.shap.AHC.xlsx",sheet="Cases");data1
fig1 <- ggplot(data1, aes(x = SHAP, y = reorder(Features, SHAP), fill = Features)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") + # Adding black border to bars
  scale_fill_brewer(palette = "Set3") + # Changing color palette to a more vibrant one
  theme_minimal(base_size = 14) + # Using a minimal theme for a clean look
  theme(legend.position = "none", # Hiding the legend
        plot.title = element_text(face = "bold", hjust = 0.5), # Center-aligning the title
        axis.title.x = element_text(face = "bold", color = "firebrick"), # Customizing x-axis title
        axis.title.y = element_text(face = "bold", color = "firebrick"), # Customizing y-axis title
        axis.text = element_text(color = "black")) + # Ensuring axis text is black for readability
  ggtitle("A: HIV infected cases") + # Setting title
  xlab("Mean |SHAP|") + # Labeling x-axis
  ylab("Features") # Labeling y-axis

fig1


#Shap plot for HIV deaths
data2<-read_excel("mean.shap.AHC.xlsx",sheet="Deaths");data2
fig2 <- ggplot(data2, aes(x = SHAP, y = reorder(Features, SHAP), fill = Features)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") + # Adding black border to bars
  scale_fill_brewer(palette = "Set3") + # Changing color palette to a more vibrant one
  theme_minimal(base_size = 14) + # Using a minimal theme for a clean look
  theme(legend.position = "none", # Hiding the legend
        plot.title = element_text(face = "bold", hjust = 0.5), # Center-aligning the title
        axis.title.x = element_text(face = "bold", color = "firebrick"), # Customizing x-axis title
        axis.title.y = element_text(face = "bold", color = "firebrick"), # Customizing y-axis title
        axis.text = element_text(color = "black")) + # Ensuring axis text is black for readability
  ggtitle("B: HIV deaths") + # Setting title
  xlab("Mean |SHAP|") + # Labeling x-axis
  ylab("") # Labeling y-axis

fig2

grid.arrange(fig1,fig2,ncol=2)
