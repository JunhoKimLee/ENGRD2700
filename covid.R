# clear environment
rm(list=ls())

# [read.tcsv] transposes a dataset. This function was taken from:
# https://stackoverflow.com/questions/17288197/reading-a-csv-file-organized-horizontally
read.tcsv = function(file, header=TRUE, sep=",", ...) {
  n = max(count.fields(file, sep=sep), na.rm=TRUE)
  x = readLines(file)
  .splitvar = function(x, sep, n) {
    var = unlist(strsplit(x, split=sep))
    length(var) = n
    return(var)
  }
  x = do.call(cbind, lapply(x, .splitvar, sep=sep, n=n))
  x = apply(x, 1, paste, collapse=sep) 
  out = read.csv(text=x, sep=sep, header=header, ...)
  return(out)
}

# [plot_covid] takes 3 vectors and plots them. Note that this plot is cumulative.
plot_covid = function(c,d,r) {
  
  c_confirmed = c
  c_deaths = d
  c_recovered = r
  
  plot(days_since_start, c_confirmed, type="o", col="blue", pch="o", lty=1)
  points(days_since_start, c_deaths, col="red", pch="*")
  lines(days_since_start, c_deaths, col="red",lty=2)
  points(days_since_start, c_recovered, col="green", pch="+")
  lines(days_since_start, c_recovered, col="green",lty=3)
}

# [plot_daily] takes 3 vectors and plots the # of new cases each day.
plot_daily = function(c,d,r) {
  
  c_confirmed = diff(c)
  c_deaths = diff(d)
  c_recovered = diff(r)
  days_since_start2 = days_since_start[c(2:109)]
  
  plot(days_since_start2, c_confirmed, type="o", col="blue", main = "COVID-19 Daily Infection Chart of Italy",
       xlab = "Days since 01/22/2020", ylab = "People (per 1 person)") #this title needs to change for every run
  points(days_since_start2, c_deaths, col="red")
  lines(days_since_start2, c_deaths, col="red")
  points(days_since_start2, c_recovered, col="green")
  lines(days_since_start2, c_recovered, col="green")
  legend(0, 6000, legend=c("Confirmed (daily new cases)", "Deaths (daily new cases)", "Recovered(daily new cases"),
         col=c("blue", "red", "green"), lty=1, cex=0.8) #this positioning needs to be adjusted for every run
}

# [to_histogram] takes a vector and converts it so that it may be read as a histogram.
# Note: this function is so horrendously inefficient but it's the best that works for now.
to_histogram = function(vec) {
  my_hist = c()
  for (i in (1:length(vec))) {
    cat("This is loop number:",i)
    print("")
    this_val = vec[i]
    for (j in (1:this_val)) {
      my_hist = append(my_hist, i)
    }
  }
  return(my_hist)
}

# [qq_analysis] takes a vector, converts it into a histogram, and creates a normal and gamma qq-plot with regression analysis.
qq_analysis = function(vec) {
  # convert our new-case data to histogram
  korea_data = to_histogram(diff(vec))
  
  #find the mean and std. dev.
  mu = mean(korea_data)
  std = sd(korea_data)
  print("Mean and std:")
  print(mu)
  print(std)
  
  # plot the histogram with normal distribution overlay
  hist(korea_data, main = "COVID-19 Daily Recovered Cases in Spain", #change this every run
       xlab = "Days since 01/22/2020", ylab = "People (normalized)", freq = FALSE)
  x = seq(0,109,1)
  curve(dnorm(x, mean = mu, sd = std), add=TRUE)
  
  # qq normal plot
  title = "Q-Q Plot of Spain Recovered Cases" #change this too
  n = length(korea_data)
  quantiles = (1:n-.5)/n
  x = qnorm(quantiles,mu,std)
  plot(x,sort(korea_data),main=paste("Normal",title, sep = " "),xlab = "Theoretical Quantiles", ylab= "Sample Quantiles")
  abline(0,1)
  # regression
  reg = lm(sort(korea_data) ~ x)
  abline(reg)
  print(summary(reg))
  
  # qq gamma plot
  my_beta = std^2/mu
  my_alpha = mu/my_beta
  x2 = qgamma(quantiles,shape=my_alpha,scale=my_beta)
  plot(x2,sort(korea_data),main=paste("Gamma",title, sep = " "), xlab = "Theoretical Quantiles", ylab= "Sample Quantiles")
  abline(0,1)
  reg2 = lm(sort(korea_data) ~ x2)
  abline(reg2)
  print(summary(reg2))
}

# load data
setwd("~/Documents/ENGRD2700/project/")
confirmed = read.tcsv(file = "time_series_covid19_confirmed_global.csv")
deaths = read.tcsv(file = "time_series_covid19_deaths_global.csv")
recovered = read.tcsv(file = "time_series_covid19_recovered_global.csv")

# initialize [days_since_start], which represents the number of days since 01/22/2020
dates = as.POSIXlt(confirmed$Country.Region, format="%m/%d/%y")
days_since_start = dates$yday-21

#data plots
#plot_daily(confirmed$Korea, deaths$Korea, recovered$Korea) #yes
#plot_daily(confirmed$US, deaths$US, recovered$US) #biggest dataset but not yet resolved
#plot_daily(confirmed$China.13, deaths$China.13, recovered$China.13) #yes
#plot_daily(confirmed$Italy, deaths$Italy, recovered$Italy) #yes
#plot_daily(confirmed$Spain, deaths$Spain, recovered$Spain) #yes
#plot_daily(confirmed$United.Kingdom.6, deaths$United.Kingdom.6, recovered$United.Kingdom.6) #no, not recovered yet
#plot_daily(confirmed$Russia, deaths$Russia, recovered$Russia) #no, not recovered yet
#plot_covid(confirmed$France.9, deaths$France.9, recovered$France.9) #no, not enough recovered yet

#qq_analysis(confirmed$Korea)
#qq_analysis(recovered$Korea)
#qq_analysis(confirmed$China.13)
#qq_analysis(recovered$China.13)
#qq_analysis(confirmed$Spain)
qq_analysis(recovered$Spain)
#qq_analysis(confirmed$Italy)
#qq_analysis(recovered$Italy)
