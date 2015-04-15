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

create_mon <- strptime(input_table$Created.At.Local, "%m/%d/%Y - %I:%M%p")$mon

all_events <- data.frame(cats, create_mon)
problem_1 <- subset(all_events, cats == top_three_issues[1])
problem_2 <- subset(all_events, cats == top_three_issues[2])
problem_3 <- subset(all_events, cats == top_three_issues[3])

problem_1_vs_time <- unlist(lapply(0:11, function(num_mon) nrow(subset(problem_1, create_mon == num_mon))))
problem_2_vs_time <- unlist(lapply(0:11, function(num_mon) nrow(subset(problem_2, create_mon == num_mon))))
problem_3_vs_time <- unlist(lapply(0:11, function(num_mon) nrow(subset(problem_3, create_mon == num_mon))))

pdf(file="common_issues_vs_time.pdf", height=5, width=8)
plot(problem_1_vs_time, type="o", col="red", axes=FALSE, ann=FALSE, ylim=c(0,500))
lines(problem_2_vs_time, type="o", col="blue", pch=22)
lines(problem_3_vs_time, type="o", col="green",pch=23)

axis(1, at=1:12, lab=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                       "Jul","Aug", "Sep", "Oct", "Nov", "Dec"))
axis(2, las=1, at=seq(0,500, by=100))
box()
title(main="Common Incidents by Month in 2014")
title(xlab="Month")
title(ylab="Number of Indicents Reported")
legend(1,490, top_three_issues, cex=0.7, col=c("red", "blue", "green"), pch=21:23,lty=1)
dev.off()