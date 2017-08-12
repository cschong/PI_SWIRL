#' Read CSV file
#' 
#' Simple wrapper for read.csv
#' 
#' @export
#' @param file a csv file.
#' @param ... arguments passed to read.csv
PI_SWIRL_Calc <- function(file, ...){
  if(!grepl(".csv$", file)){
    stop("Uploaded file must be a .csv file!")
  }
  data<-read.csv(file, ...);
  #boxplot(data)

groundLAI <- melt(data)
colnames(groundLAI) <- c("Location","Corrected_LAI")

groundLAI$Location <- factor(groundLAI$Location,levels=c("BG2","UW31","UW39","TAM"))
plotdata2 <- ggplot(groundLAI, aes(x=(Location), y=Corrected_LAI))+
  geom_boxplot(aes(fill=factor(Location)),outlier.shape=NA)+
  geom_point(color="black",shape=21,size=2,aes(fill = factor(Location)),position=position_jitterdodge(),alpha=0.4)+
  stat_summary(fun.y=mean, colour="green", geom="point", shape=18, size=4)+
  scale_fill_brewer(palette="YlOrRd")+
  labs(x="Locations",y="Leaf Area Index (LAI)") + #,colour="Friction/nVelocity/n(m/s)") 
  #theme(panel.grid.minor = element_line(color='black'))
  ggtitle("Leaf Area Index (LAI) - LI-COR Method") + 
  theme(plot.margin = unit(c(1.2,1.2,1.2,1.2), "cm")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", vjust=3))

plotdata2 <- plotdata2 #+ guides(fill=guide_legend(title="Friction/nVelocity/n(m/s)"))
plotdata2 <- plotdata2 + guides(fill=FALSE)

}
