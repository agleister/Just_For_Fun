input_table <- read.csv("Service_Request_Data_small.csv")
cats <- input_table$Category
complaints <- integer()

# determine the top three most prevalent issues
for (lev in levels(cats)){
        sub_list <- cats[cats == lev]
        complaints <- c(complaints, length(sub_list))
}
issue_frame <- data.frame(issue=levels(cats), instances = complaints)
issue_frame <- issue_frame[order(-issue_frame$instances),]
top_three_issues <- as.character(issue_frame[1:3,1])

#map the issues
map_table <- subset(input_table, Category %in% top_three_issues, select=c(Lat,Lng,Category))
pdf(file="Common_Indicent_Map.pdf", height=5.5, width=9.5)
plot(Lng,Lat, col=c("red", "blue", "green")[match(map_table$Category, top_three_issues)], ann=FALSE)
title(main="Map of Most Common Incidents in 2014", xlab="Longitude", ylab="Latittude")
legend(-72.984,41.265, top_three_issues, cex=0.6, col=c("red", "blue", "green"), pch=21)
dev.off()