make_time_plot <- function(){
        
        #input_table <- read.csv("SCF_2014_NHV_Service_Request_Data.csv")
        input_table <- read.csv("Service_Request_Data_small.csv")
        #names(input_table)
        
        #determine the month when each issue was created, acknowledged and closed
        create <- strptime(input_table$Created.At.Local, "%m/%d/%Y - %I:%M%p")$mon
        ack <- strptime(input_table$Acknowledged.At.Local, "%m/%d/%Y - %I:%M%p")
        closed <- strptime(input_table$Closed.At.Local, "%m/%d/%Y - %I:%M%p")
        
        ack_year <- ack$year
        closed_year <-closed$year
        
        ack <- ack$mon
        closed <- closed$mon
        
        # correct for entries that were closed but not acknowledged
        ack[which(is.na(ack))] <- closed[which(is.na(ack))]
        
        # don't take into account results of entries that were acknowledged or closed in 2015
        ack[which(ack_year == 115)] <- NA
        closed[which(closed_year == 115)] <- NA
        
        #clean each list for NA values
        create <- create[!is.na(create)]
        ack <- ack[!is.na(ack)]
        closed <- closed[!is.na(closed)]
        
        monthly_progress <- matrix(numeric(), 0, 3)
        
        #calculate the cumulative cases created, acknowledged, and closed each month
        for (i in 1:12){
                cr <- length(create[create < i])
                ac <- length(ack[ack < i])
                cl <- length(closed[closed < i])
                monthly_progress <- rbind(monthly_progress, c(cr, ac, cl))
        }
        
        monthly_progress <- as.data.frame(monthly_progress)
        colnames(monthly_progress) <- c("Reported", "Acknowledged", "Closed")
        monthly_progress
}

results_month <- make_time_plot()

#perform regression with each level of results:
b_reported <- cov(1:12, results_month$Reported)/var(1:12)
a_reported <- mean(results_month$Reported) - b_reported * mean(1:12)
b_acknowledged <- cov(1:12, results_month$Acknowledged)/var(1:12)
a_acknowledged <- mean(results_month$Acknowledged) - b_acknowledged * mean(1:12)
b_closed <- cov(1:12, results_month$Closed)/var(1:12)
a_closed <- mean(results_month$Closed) - b_closed * mean(1:12)

#make the plot
pdf(file="issues_vs_time.pdf", height=6, width=8)
plot(results_month$Reported, type="o", col="blue", axes=FALSE, ann=FALSE)
lines(results_month$Acknowledged, type="o", col="green", pch=22)
lines(results_month$Closed, type="o", col="red",pch=23)
abline(a = a_reported, b = b_reported, col="darkblue", lty=2)
abline(a = a_acknowledged, b = b_acknowledged, col="darkgreen", lty=2)
abline(a = a_closed, b = b_closed, col="darkred", lty=2)
grid(nx = 0, ny = NULL, col="darkgray")
axis(1, at=1:12, lab=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                       "Jul","Aug", "Sep", "Oct", "Nov", "Dec"))
axis(2, las=1, at=seq(0,10000, by=2000))
box()
title(main="Incidents by Month (Cumulative) in 2014")
title(xlab="Month")
title(ylab="Cumulative Number of Indicents")
slope_labels <- c(paste0("Report rate = ", round(b_reported,0), " / month"),
                  paste0("Acknlg rate = ", round(b_acknowledged), " / month"),
                  paste0("Close rate = ", round(b_closed), " / month"))
legend(1,9700, c(names(results_month), slope_labels), cex=0.7, 
       col=c("blue", "green", "red","darkblue", "darkgreen", "darkred"), 
       pch=c(21:23, rep(NA,3)), lty=c(rep(1,3),rep(2,3)), ncol=2)#
text(1,7500,adj=0, labels=paste0("Ratio of closing to reporting rate: ", round(b_closed/b_reported,2)))
dev.off()