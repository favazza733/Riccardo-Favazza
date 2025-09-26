dev.new(width = 12, height = 7)


pre_treatment_data <- subset(data_comp, TIME_PERIOD < "2022-S2")
View(pre_treatment_data)
pre_treatment_data <- pre_treatment_data[order(pre_treatment_data$TIME_PERIOD), ]
pre_treatment_data$TIME_PERIOD <- factor(pre_treatment_data$TIME_PERIOD, levels = unique(pre_treatment_data$TIME_PERIOD))
x_vals <- 1:length(levels(pre_treatment_data$TIME_PERIOD))
trattati <- subset(pre_treatment_data, geo %in% c("Spain", "Portugal"))
controllo <- subset(pre_treatment_data, geo %in% c("Germany", "Austria", "Italy", "Netherlands", "Belgium"))
plot(x_vals, trattati$energy_price[trattati$geo == "Spain"], type = "l", col = "red", lwd = 2,
     ylim = range(pre_treatment_data$energy_price) , xlab= "", ylab = "Electricity price (€/kWh)", 
     main = "Pre Treatment Trend for Household consumers",
     xaxt = "n")
lines(x_vals, trattati$energy_price[trattati$geo == "Portugal"], col = "red", lwd = 2)
lines(x_vals, controllo$energy_price[controllo$geo == "Germany"], col = "black", lwd = 2)
lines(x_vals, controllo$energy_price[controllo$geo == "Italy"], col = "black", lwd = 2)
lines(x_vals, controllo$energy_price[controllo$geo == "Netherlands"], col = "black", lwd = 2)
lines(x_vals, controllo$energy_price[controllo$geo == "Austria"], col = "black", lwd = 2)
lines(x_vals, controllo$energy_price[controllo$geo == "Belgium"], col = "black", lwd = 2)

axis(1, at = x_vals, labels = levels(pre_treatment_data$TIME_PERIOD), las = 2)

legend(x=0.9, y=0.28, 
       legend = c("Spagna", "Portogallo", "Germania", "Italia", "Paesi Bassi", "Austria", "Belgio"),
       col = c("red", "red", "black", "black", "black", "black", "black"),
       lty = 1, lwd = 2, bty = "n")

