library(tempdisagg)
library(readxl)
library(tsbox)
library(tsibble)
library(openxlsx)

yearly_data = read_excel(file.choose())

# Convert the yearly data to a time series object
yearly_ts <- ts(yearly_data$FDI_Total, start = 2016, end = 2022)
yearly_ts


# Perform temporal disaggregation using the Denton-Chollette method
monthly_ts <- td(yearly_ts ~ 0 + time_in() , to = "monthly", method = "denton-cholette")
monthly = predict(monthly_ts)

# Write the interpolated monthly data to a new Excel file
monthly_data = read_excel("C:/Users/snega/OneDrive/Desktop/PROJECT/Datatsets/FDI_interpolated_data.xlsx")
monthly_data$FDI_Total = monthly

write.xlsx(monthly_data, "C:/Users/snega/OneDrive/Desktop/PROJECT/Datatsets/FDI_interpolated_data.xlsx")




