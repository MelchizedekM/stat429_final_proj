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
consump_real <-  na.omit(ts(consump_real[,2], start = c(1959, 1), frequency = 4))
t <- time(infla_rate)

sin_1 <- ts(sin(2*pi*(t-1959)/40),start = c(1948, 1), frequency = 4)
sin_2 <- ts(sin(2*pi*(t-1980)/24),start = c(1948, 1), frequency = 4)
sin_3 <- ts(sin(2*pi*(t-1980)/48),start = c(1948, 1), frequency = 4)

CPI_lag1 <- lag(CPI, -1)
CPI_lag2 <- lag(CPI, -2)
CPI_lag3 <- lag(CPI, -3)

infla_rate_lag1 <- lag(infla_rate, -1)
infla_rate_lag2 <- lag(infla_rate, -2)
infla_rate_lag3 <- lag(infla_rate, -3)

data <- data.frame( na.omit( ts.intersect(infla_rate, infla_rate_lag1 , infla_rate_lag2 , infla_rate_lag3,CPI, CPI_lag1,CPI_lag2, CPI_lag3, t, sin_1, sin_2,sin_3, unemp_rate, grove_exp, invest, consump_real)))

# model 0

model_0 <- lm(CPI ~ I(1.08^t), data = data)
summary(model_0)

fitted_CPI_0 <- fitted(model_0)

if(nrow(data) > 1) {
  # Check the lengths of the vectors
  if(length(data[,'CPI']) == length(fitted_CPI_0)) {
    # If they match, plot the graph
    ggplot(data, aes(x = t, y = CPI)) + 
      geom_line(color = "blue") +
      geom_line(aes(y = fitted_CPI_0), color = "red") +
      labs(title = "CPI and fitted values", x = "Year", y = "CPI") +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    # If lengths don't match, print a message
    cat("Length of 'infla_rate' and 'fitted_infla_rate' do not match.")
  }
} else {
  # If data has only one row or none, print a message
  cat("Data frame 'data' has less than two rows.")
}

acf(CPI)
pacf(CPI)

acf(model_0$residuals)
pacf(model_0$residuals)

model_0.1 <- lm(CPI ~  CPI_lag1 , data = data)
summary(model_0.1)

fitted_CPI_0.1 <- fitted(model_0.1)

if(nrow(data) > 1) {
  # Check the lengths of the vectors
  if(length(data[,'CPI']) == length(fitted_CPI_0.1)) {
    # If they match, plot the graph
    ggplot(data, aes(x = t, y = CPI)) + 
      geom_line(color = "blue") +
      geom_line(aes(y = fitted_CPI_0.1), color = "red") +
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


acf(model_0.1$residuals, lag = 200)

# model 1

model_1 <- lm(CPI ~ I(1.05^(t)) + I((t-1951)^2) + I(unemp_rate)  + consump_real  , data = data)
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
  geom_line(aes(y = (resid(model_1))), color = "blue") +
  labs(title = "Residuals", x = "Year", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5))

acf(model_1$residuals)

# model 1.1
model_1.1 <- lm(CPI ~ I(1.05^(t)) + I((t-1951)^2) + I((unemp_rate))  + consump_real + sin_1 + invest + grove_exp, data = data)
summary(model_1.1)

fitted_CPI_1.1 <- fitted(model_1.1)

if(nrow(data) > 1) {
  # Check the lengths of the vectors
  if(length(data[,'CPI']) == length(fitted_CPI_1.1)) {
    # If they match, plot the graph
    ggplot(data, aes(x = t, y = CPI)) + 
      geom_line(color = "blue") +
      geom_line(aes(y = fitted_CPI_1.1), color = "red") +
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

# plot the residuals
ggplot(data, aes(x = t)) + 
  geom_line(aes(y = (resid(model_1.1))), color = "blue") +
  labs(title = "Residuals", x = "Year", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5))

acf(model_1.1$residuals)


# model 1.2
model_1.2 <- lm(CPI ~ I(1.08^(t)) + I((t-1951)^2) + I((unemp_rate))  + consump_real + sin_1 + sin_2 + invest , data = data[ 1:(length(data[,'t'])-5),])
summary(model_1.2)

fitted_CPI_1.2 <- fitted(model_1.2)

if(nrow(data) > 1) {
  # Check the lengths of the vectors
  if(length(data[1:(length(data[,'t'])-5),'CPI']) == length(fitted_CPI_1.2)) {
    # If they match, plot the graph
    ggplot(data[1:(length(data[,'t'])-5),], aes(x = t, y = CPI)) + 
      geom_line(color = "blue") +
      geom_line(aes(y = fitted_CPI_1.2), color = "red") +
      labs(title = "CPI and fitted values", x = "Year", y = "CPI") +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    # If lengths don't match, print a message
    cat("Length of 'infla_rate' and 'fitted_infla_rate' do not match.")
  }
} else {
  # If data has only one row or none, print a message
  cat("Data frame 'data' has less than two rows.")
}

# plot the residuals
ggplot(data[1:(length(data[,'t'])-5),], aes(x = t)) + 
  geom_line(aes(y = (resid(model_1.2))), color = "blue") +
  labs(title = "Residuals", x = "Year", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5))

acf(model_1.2$residuals)

pre_1.2 <- predict(model_1.2, newdata = data[(length(data[,'t'])-4):length(data[,'t']),])
pre_1.2
data[(length(data[,'t'])-4):length(data[,'t']),'CPI']

test_error_1.2 <- mean(data[(length(data[,'t'])-4):length(data[,'t']),'CPI'] - pre_1.2)
test_error_1.2/mean(data[(length(data[,'t'])-4):length(data[,'t']),'CPI'])

# caculate the inflation rate
fitted_infla_rate_1.2 <- diff(fitted_CPI_1.2)/fitted_CPI_1.2[-length(fitted_CPI_1.2)]

# plot the fitted inflation rate and the real inflation rate
if(nrow(data) > 1) {
  # Check the lengths of the vectors
  if(length(data[ 1:(length(data[,'t'])-6),'infla_rate']) == length(fitted_infla_rate_1.2)) {
    # If they match, plot the graph
    ggplot(data[ 1:(length(data[,'t'])-6),], aes(x = t, y = infla_rate)) + 
      geom_line(color = "blue") +
      geom_line(aes(y = fitted_infla_rate_1.2), color = "red") +
      labs(title = "CPI and fitted values", x = "Year", y = "CPI") +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    # If lengths don't match, print a message
    cat("Length of 'infla_rate' and 'fitted_infla_rate' do not match.")
  }
} else {
  # If data has only one row or none, print a message
  cat("Data frame 'data' has less than two rows.")
}

# model 1.3
model_1.3 <- lm(CPI ~ I(1.05^t) + sin_1 + sin_2 + sin_3 + CPI_lag1 + unemp_rate + grove_exp + invest + consump_real, data = data[ 1:(length(data[,'t'])-5),])
summary(model_1.3)

fitted_CPI_1.3 <- fitted(model_1.3)

par(mfrow = c(2, 1))

if(nrow(data) > 1) {
  # Check the lengths of the vectors
  if(length(data[ 1:(length(data[,'t'])-5),'CPI']) == length(fitted_CPI_1.3)) {
    # If they match, plot the graph
    ggplot(data[ 1:(length(data[,'t'])-5),], aes(x = t, y = CPI)) + 
      geom_line(color = "blue") +
      geom_line(aes(y = fitted_CPI_1.3), color = "red") +
      labs(title = "CPI and fitted values", x = "Year", y = "CPI") +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    # If lengths don't match, print a message
    cat("Length of 'infla_rate' and 'fitted_infla_rate' do not match.")
  }
} else {
  # If data has only one row or none, print a message
  cat("Data frame 'data' has less than two rows.")
}

# plot the residuals
ggplot(data[ 1:(length(data[,'t'])-5),], aes(x = t)) + 
  geom_line(aes(y = (resid(model_1.3))), color = "blue") +
  labs(title = "Residuals", x = "Year", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5))


acf(model_1.3$residuals,lag = 200)

pre_1.3 <- predict(model_1.3, newdata = data[(length(data[,'t'])-4):length(data[,'t']),])
pre_1.3
data[(length(data[,'t'])-4):length(data[,'t']),'CPI']

test_error_1.3 <- sqrt(mean((data[(length(data[,'t'])-4):length(data[,'t']),'CPI'] - pre_1.3)^2))
test_error_1.3/mean(data[(length(data[,'t'])-4):length(data[,'t']),'CPI'])

# caculate the inflation rate
fitted_infla_rate_1.3 <- diff(fitted_CPI_1.3)/fitted_CPI_1.3[-length(fitted_CPI_1.3)]

# plot the fitted inflation rate and the real inflation rate
if(nrow(data) > 1) {
  # Check the lengths of the vectors
  if(length(data[ 1:(length(data[,'t'])-6),'infla_rate']) == length(fitted_infla_rate_1.3)) {
    # If they match, plot the graph
    ggplot(data[ 1:(length(data[,'t'])-6),], aes(x = t, y = infla_rate)) + 
      geom_line(color = "blue") +
      geom_line(aes(y = fitted_infla_rate_1.3), color = "red") +
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

# predict the inflation rate
pre_infla_rate_1.3 <- diff(pre_1.3)/pre_1.3[-length(pre_1.3)]

# plot the predicted inflation rate and the real inflation rate
if(nrow(data) > 1) {
  # Check the lengths of the vectors
  if(length(data[(length(data[,'t'])-4):(length(data[,'t'])-1),'infla_rate']) == length(pre_infla_rate_1.3)) {
    # If they match, plot the graph
    ggplot(data[(length(data[,'t'])-4):(length(data[,'t'])-1),], aes(x = t, y = infla_rate)) + 
      geom_line(color = "blue") +
      geom_line(aes(y = pre_infla_rate_1.3), color = "red") +
      labs(title = "Inflation rate and predicted values", x = "Year", y = "Inflation rate") +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    # If lengths don't match, print a message
    cat("Length of 'infla_rate' and 'pre_infla_rate_1.3' do not match.")
  }
} else {
  # If data has only one row or none, print a message
  cat("Data frame 'data' has less than two rows.")
}

# test error
pre_infla_rate_1.3
data[(length(data[,'t'])-4):(length(data[,'t'])-1),'infla_rate']
test_error_1.3 <- sqrt(mean((data[(length(data[,'t'])-4):(length(data[,'t'])-1),'infla_rate'] - pre_infla_rate_1.3)^2))
test_error_1.3/mean(data[(length(data[,'t'])-4):(length(data[,'t'])-1),'infla_rate'])


# model 1.4

model_1.4 <- lm(CPI ~ CPI_lag1 +I( 0.001* CPI_lag1*consump_real) + I( 0.001* CPI_lag1* invest) + I( 0.001* CPI_lag1* grove_exp) + I(CPI_lag1*unemp_rate) , data = data[ 1:(length(data[,'t'])-5),])

summary(model_1.4)

fitted_CPI_1.4 <- fitted(model_1.4)

if(nrow(data) > 1) {
  # Check the lengths of the vectors
  if(length(data[ 1:(length(data[,'t'])-5),'CPI']) == length(fitted_CPI_1.4)) {
    # If they match, plot the graph
    ggplot(data[ 1:(length(data[,'t'])-5),], aes(x = t, y = CPI)) + 
      geom_line(color = "blue") +
      geom_line(aes(y = fitted_CPI_1.4), color = "red") +
      labs(title = "Inflation rate and fitted values", x = "Year", y = "Inflation rate") +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    # If lengths don't match, print a message
    cat("Length of 'CPI' and 'fitted_CPI_1.4' do not match.")
  }
} else {
  # If data has only one row or none, print a message
  cat("Data frame 'CPI' has less than two rows.")
}


# plot the residuals
ggplot(data[ 1:(length(data[,'t'])-5),], aes(x = t)) + 
  geom_line(aes(y = (resid(model_1.4))), color = "blue") +
  labs(title = "Residuals", x = "Year", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5))

# predict the inflation rate
pre_1.4 <- predict(model_1.4, newdata = data[(length(data[,'t'])-4):length(data[,'t']),])
pre_1.4
data[(length(data[,'t'])-4):length(data[,'t']),'CPI']

test_error_1.4 <- sqrt(mean((data[(length(data[,'t'])-4):length(data[,'t']),'CPI'] - pre_1.4)^2))
test_error_1.4/mean(data[(length(data[,'t'])-4):length(data[,'t']),'CPI'])

# caculate the inflation rate
fitted_infla_rate_1.4 <- diff(fitted_CPI_1.4)/fitted_CPI_1.4[-length(fitted_CPI_1.4)]

# plot the fitted inflation rate and the real inflation rate
if(nrow(data) > 1) {
  # Check the lengths of the vectors
  if(length(data[ 1:(length(data[,'t'])-6),'infla_rate']) == length(fitted_infla_rate_1.4)) {
    # If they match, plot the graph
    ggplot(data[ 1:(length(data[,'t'])-6),], aes(x = t, y = infla_rate)) + 
      geom_line(color = "blue") +
      geom_line(aes(y = fitted_infla_rate_1.4), color = "red") +
      labs(title = "Inflation rate and fitted values", x = "Year", y = "Inflation rate") +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    # If lengths don't match, print a message
    cat("Length of 'infla_rate' and 'fitted_infla_rate_1.4' do not match.")
  }
} else {
  # If data has only one row or none, print a message
  cat("Data frame 'infla_rate' has less than two rows.")
}

# predict the inflation rate
pre_infla_rate_1.4 <- diff(pre_1.4)/pre_1.4[-length(pre_1.4)]

# plot the predicted inflation rate and the real inflation rate
if(nrow(data) > 1) {
  # Check the lengths of the vectors
  if(length(data[(length(data[,'t'])-4):(length(data[,'t'])-1),'infla_rate']) == length(pre_infla_rate_1.4)) {
    # If they match, plot the graph
    ggplot(data[(length(data[,'t'])-4):(length(data[,'t'])-1),], aes(x = t, y = infla_rate)) + 
      geom_line(color = "blue") +
      geom_line(aes(y = pre_infla_rate_1.4), color = "red") +
      labs(title = "Inflation rate and predicted values", x = "Year", y = "Inflation rate") +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    # If lengths don't match, print a message
    cat("Length of 'infla_rate' and 'pre_infla_rate_1.4' do not match.")
  }
} else {
  # If data has only one row or none, print a message
  cat("Data frame 'infla_rate' has less than two rows.")
}

# test error
pre_infla_rate_1.4
data[(length(data[,'t'])-4):(length(data[,'t'])-1),'infla_rate']
test_error_1.4 <- sqrt(mean((data[(length(data[,'t'])-4):(length(data[,'t'])-1),'infla_rate'] - pre_infla_rate_1.4)^2))
test_error_1.4/mean(data[(length(data[,'t'])-4):(length(data[,'t'])-1),'infla_rate'])

# AIC and BIC


AIC(model_1, model_1.1, model_1.2, model_1.3)
BIC(model_1, model_1.1, model_1.2, model_1.3)

# model 2
pacf(data[,'infla_rate'])

model_2 <- lm(infla_rate ~ t + unemp_rate + grove_exp  + consump_real + invest + infla_rate_lag1 + infla_rate_lag2 + infla_rate_lag3, data = data[ 1:(length(data[,'t'])-5),])
summary(model_2)

# plot the residuals
ggplot(data[ 1:(length(data[,'t'])-5),], aes(x = data[ 1:(length(data[,'t'])-5),'t'])) + 
  geom_line(aes(y = (resid(model_2))/data[ 1:(length(data[,'t'])-5),"infla_rate"]), color = "blue") +
  labs(title = "Residuals", x = "Year", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5))

# plot the fitted inflation rate and the real inflation rate
if(nrow(data) > 1) {
  # Check the lengths of the vectors
  if(length(data[ 1:(length(data[,'t'])-5),'infla_rate']) == length(fitted(model_2))) {
    # If they match, plot the graph
    ggplot(data[ 1:(length(data[,'t'])-5),], aes(x = t, y = infla_rate) )+ 
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

acf(model_2$residuals, lag = 200)


pre_2 <- predict(model_2, newdata = data[(length(data[,'t'])-4):length(data[,'t']),])
pre_2
data[(length(data[,'t'])-4):length(data[,'t']),'infla_rate']

test_error_2 <- sqrt(mean((data[(length(data[,'t'])-4):length(data[,'t']),'infla_rate'] - pre_2)^2))
test_error_2/mean(data[(length(data[,'t'])-4):length(data[,'t']),'infla_rate'])



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


# model 5

summary(model_5)

plot(data[,'t'], data[,'diff_CPI'], type = "l", col = "blue", xlab = "Year", ylab = "Inflation rate")
