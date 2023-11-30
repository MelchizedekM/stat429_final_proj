if (!require('this.path')) install.packages('this.path')
if (!require('readxl')) install.packages("readxl")
if (!require('ggplot2')) install.packages("ggplot2")

library(this.path)
library(readxl)
library(ggplot2)

# Read in the data
curdir <- dirname(this.path())
infla_rate_CPI <- read_excel(paste0(curdir, "/data/inflation_rate.xlsx"))
unemp_rate <- read_excel(paste0(curdir, "/data/unemploy_rate.xls"))
grove_exp <- read_excel(paste0(curdir, "/data/Grov_exp.xls"))
invest <- read_excel(paste0(curdir, "/data/invest_real.xls"))
consump_no <- read_excel(paste0(curdir, "/data/consumption.xls"))
consump_real <- read_excel(paste0(curdir, "/data/PCE.xls"))


infla_rate <-  na.omit(ts(infla_rate_CPI[,3], start = c(1948, 1), frequency = 4))
CPI <-  na.omit(ts(infla_rate_CPI[,2], start = c(1948, 1), frequency = 4))
unemp_rate <-  na.omit(ts(unemp_rate[,2], start = c(1948, 1), frequency = 4))
grove_exp <-  na.omit(ts(grove_exp[,2], start = c(1948, 1), frequency = 4))
invest <-  na.omit(ts(invest[,2], start = c(1948, 1), frequency = 4))
consump_no <-  na.omit(ts(consump_no[,2], start = c(2007, 1), frequency = 4))
consump_real <-  na.omit(ts(consump_real[,2], start = c(1971, 1), frequency = 4))
diff_infla_rate <-  na.omit(diff(infla_rate))
diff_CPI <-  na.omit(diff(CPI))
diff_unemp_rate <-  na.omit(diff(unemp_rate))
t <- time(infla_rate)

data <- data.frame( na.omit( ts.intersect(infla_rate, diff_infla_rate,CPI, diff_CPI, t,unemp_rate, diff_unemp_rate, grove_exp, invest, consump_real)))

model_1 <- lm(CPI ~ I(1.01^(t-1971)) + I(exp(unemp_rate)) + grove_exp  + consump_real, data = data)
summary(model_1)

# using the fitted CPI to calculate the inflation rate
fitted_CPI <- fitted(model_1)
fitted_infla_rate <- diff(fitted_CPI)/fitted_CPI[-length(fitted_CPI)]

# plot the fitted inflation rate and the real inflation rate
if(nrow(data) > 1) {
  # Check the lengths of the vectors
  if(length(data[-1,'infla_rate']) == length(fitted_infla_rate)) {
    # If they match, plot the graph
    ggplot(data[-1,], aes(x = t, y = infla_rate)) + 
      geom_line(color = "blue") +
      geom_line(aes(y = fitted_infla_rate), color = "red") +
      labs(title = "Inflation rate and fitted values", x = "Year", y = "Inflation rate") +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    # If lengths don't match, print a message
    cat("Length of 'infla_rate' and 'fitted_infla_rate' do not match.")
  }
} else {
  # If data has only one row or none, print a message
  cat("Data frame 'data' has less than two rows.")
}

plot(data[-1,'t'], data[-1,'infla_rate'], type = "l", col = "blue", xlab = "Year", ylab = "Inflation rate")
plot(data[-1,'t'], fitted_infla_rate, type = "l", col = "red", xlab = "Year", ylab = "Inflation rate")

# plot these two line CPI shoud be the same length as the fitted values
ggplot(data, aes(x = t)) + 
  geom_line(aes(y = CPI), color = "blue") +
  geom_line(aes(y = fitted(model_1)), color = "red") +
  labs(title = "CPI and fitted values", x = "Year", y = "CPI") +
  theme(plot.title = element_text(hjust = 0.5))

# plot the residuals
ggplot(data, aes(x = t)) + 
  geom_line(aes(y = (resid(model_1))/data[,"CPI"]), color = "blue") +
  labs(title = "Residuals", x = "Year", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5))



# model 2
model_2 <- lm(infla_rate ~ t + unemp_rate + grove_exp  + consump_real + invest, data = data)
summary(model_2)

# plot the residuals
ggplot(data, aes(x = data[,'t'])) + 
  geom_line(aes(y = (resid(model_2))/data[,"infla_rate"]), color = "blue") +
  labs(title = "Residuals", x = "Year", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5))

# plot the fitted inflation rate and the real inflation rate
if(nrow(data) > 1) {
  # Check the lengths of the vectors
  if(length(data[,'infla_rate']) == length(fitted(model_2))) {
    # If they match, plot the graph
    ggplot(data, aes(x = t, y = data[,'infla_rate'])) + 
      geom_line(color = "blue") +
      geom_line(aes(y = fitted(model_2)), color = "red") +
      labs(title = "Inflation rate and fitted values", x = "Year", y = "Inflation rate") +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    # If lengths don't match, print a message
    cat("Length of 'infla_rate' and 'fitted_infla_rate' do not match.")
  }
} else {
  # If data has only one row or none, print a message
  cat("Data frame 'data' has less than two rows.")
}

length(data[,'infla_rate'])
length(fitted(model_2))

# model 3
model_3 <- lm(diff_infla_rate ~ t + unemp_rate + grove_exp  + consump_real + invest + I(t^2), data = data)
summary(model_3)

# plot the residuals
ggplot(data, aes(x = data[,'t'])) + 
  geom_line(aes(y = (resid(model_3))/data[,"diff_infla_rate"]), color = "blue") +
  labs(title = "Residuals", x = "Year", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5))

# plot the fitted inflation rate and the real inflation rate
if(nrow(data) > 1) {
  # Check the lengths of the vectors
 if(length(data[-1,'infla_rate']) == length(fitted_infla_rate)) {
    # If they match, plot the graph
    ggplot(data[-1,], aes(x = t, y = infla_rate)) + 
      geom_line(color = "blue") +
      geom_line(aes(y = fitted_infla_rate_3), color = "red") +
      labs(title = "Inflation rate and fitted values", x = "Year", y = "Inflation rate") +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    # If lengths don't match, print a message
    cat("Length of 'infla_rate' and 'fitted_infla_rate' do not match.")
  }
} else {
  # If data has only one row or none, print a message
  cat("Data frame 'data' has less than two rows.")
}

fitted_diff_infla_rate <- fitted(model_3)
fitted_infla_rate_3 <- fitted(model_3)[1:length(fitted(model_3))-1] + fitted(model_3)[2:length(fitted(model_3))] + data[,'infla_rate'][1]


# plot the fitted inflation rate and the real inflation rate
if(nrow(data) > 1) {
  # Check the lengths of the vectors
  if(length(data[-1,'infla_rate']) == length(fitted_infla_rate_3)) {
    # If they match, plot the graph
    ggplot(data, aes(x = t, y = data[-1,'infla_rate'])) + 
      geom_line(color = "blue") +
      geom_line(aes(y = fitted_infla_rate_3), color = "red") +
      labs(title = "Inflation rate and fitted values", x = "Year", y = "Inflation rate") +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    # If lengths don't match, print a message
    cat("Length of 'infla_rate' and 'fitted_infla_rate' do not match.")
  }
} else {
  # If data has only one row or none, print a message
  cat("Data frame 'data' has less than two rows.")
}

length(data[-1,'infla_rate'])
length(fitted_infla_rate_3)

# model 4 (ARIMA)
model_4 <- arima(data[,'infla_rate'], order = c(0,1,12), xreg = data[,c('t', 'unemp_rate', 'grove_exp', 'consump_real', 'invest')])

# Extracting fitted values
fitted_values <- predict(model_4, newxreg = data[,c('t', 'unemp_rate', 'grove_exp', 'consump_real', 'invest')])$pred

# Plotting
if(length(fitted_values) > 0) {
  ggplot(data, aes(x = t, y = infla_rate)) + 
    geom_line(color = "blue") +
    geom_line(aes(y = fitted_values), color = "red") +
    labs(title = "Inflation rate and fitted values", x = "Year", y = "Inflation rate") +
    theme(plot.title = element_text(hjust = 0.5))
} else {
  cat("No fitted values generated.")
}


