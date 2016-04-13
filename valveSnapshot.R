rm(list = ls())

#this sequence builds from valveID or email inside of userInput

valveSnapshot <- function(userInput, readings = 7 * 24 * 6, offset = 0, limit = 1000) {
	#required libraries
	library(jsonlite)
	library(xts)

	#Pattern detection for userInput
	ifelse(grepl("@", userInput), c(userEmail <- userInput, valveID <- NA), c(valveID <- userInput, userEmail <- NA))

	# -------If using email, getting owner ID-------
		if (!is.na(userEmail)) {
		usingEmailLink <- paste("http://api.identity.prod.edyn.com/users?email=", userEmail, "&token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzY29wZXMiOlsiYmxpbmt1cDp3ZWJob29rcyIsImJsaW5rdXAiLCJzZW5zb3JzOmxpc3QiLCJyZWFkaW5nczpsaXN0IiwidmFsdmVzOmxpc3QiLCJ2YWx2ZTphZ2VudCIsImFkbWluOmFsbCJdLCJpYXQiOjE0NTk4MjI2MDYsInN1YiI6ImFwcDpkYXNoYm9hcmR0In0.OpZ07GDCLsB9pWFBeHPV86tHuFh71P9UiwZa-sn1ztI", 
			sep = "")
		emailID <- fromJSON(usingEmailLink)
		ownerID <- emailID$id
	}

	# -------If using valveID, get ownerID-------
	if (!is.na(valveID)) {
		getValveMacID <- paste("http://api.valve.prod.edyn.com/valves/", valveID, "?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzY29wZXMiOlsicmVhZGluZ3M6bGlzdCJdLCJpYXQiOjE0NTYzNTg5NzksInN1YiI6ImFwcDphcHA6aWRlbnRpdHktd29ya2VycyJ9.gGtqyXrPDFwT33hdWgEWhgXG6bNpLcHCDSUStCHT70A", 
			sep = "")
		jsonID <- fromJSON(getValveMacID)
		valveMacID <- jsonID$uuid
		ownerID <- jsonID$ownerId
	}

	ownerDirectory <- paste("./", ownerID, sep = "")
	if (!file.exists(ownerDirectory)) {
		dir.create(ownerDirectory)
	}

	closeAllConnections() #error handling if an incorrect email is given

	# -------Then we figure out the garden info-------	
	## -------THIS IS THE BIG LOOP-------
ownerInfoLink <- paste("https://edynpepper.firebaseio.com/users_gardens/", ownerID, ".json?auth=Uj9kHlU25azPe1jH9Xq75I9yDv78pKku4tHOdTEs", 
		sep = "")
	ownerInfo <- fromJSON(ownerInfoLink)
	gardenIDs <- names(ownerInfo)

	for (i in 1:length(gardenIDs)) {
		if (names(ownerInfo[[i]][2]) != "deleted_at") {
			device <- sapply(ownerInfo[[i]][[2]], function(x) x[["deviceType"]])
		} else {
			device <- "deleted"
		}
		if (any(device == "valve")) {
			if (length(device) < 2) {
				valveID <- names(device)
				sensorID <- NA
			} else {
				valveID <- names(device)[which(device == "valve")]
				sensorID <- names(device)[which(device == "sensor")]
			}
			gardenID <- gardenIDs[i]
			gardenDirectory <- paste(ownerDirectory, "/", gardenID, sep = "")
			if (!file.exists(gardenDirectory)) {
				dir.create(gardenDirectory)
			}

			# -------Get the valve Mac ID-------	
			getValveMacID <- paste("http://api.valve.prod.edyn.com/valves/", valveID, "?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzY29wZXMiOlsicmVhZGluZ3M6bGlzdCJdLCJpYXQiOjE0NTYzNTg5NzksInN1YiI6ImFwcDphcHA6aWRlbnRpdHktd29ya2VycyJ9.gGtqyXrPDFwT33hdWgEWhgXG6bNpLcHCDSUStCHT70A", 
				sep = "")
			jsonID <- fromJSON(getValveMacID)
			valveMacID <- jsonID$uuid

			# -------Now we need the sensor Mac ID-------
			if (!is.na(sensorID)) {
				sensorInfoLink <- paste("http://api.identity.prod.edyn.com/devices/", sensorID, "?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzY29wZXMiOlsicmVhZGluZ3M6bGlzdCJdLCJpYXQiOjE0NTYzNTg5NzksInN1YiI6ImFwcDphcHA6aWRlbnRpdHktd29ya2VycyJ9.gGtqyXrPDFwT33hdWgEWhgXG6bNpLcHCDSUStCHT70A", 
					sep = "")
				sensorInfo <- fromJSON(sensorInfoLink)
				sensorMacID <- sensorInfo$uuid
			}

			# -------And make sure we have the right timezone-------			
			tzLink <- paste("https://edynpepper.firebaseio.com/gardens-meta/", gardenID, ".json?auth=Uj9kHlU25azPe1jH9Xq75I9yDv78pKku4tHOdTEs", 
				sep = "")
			tzData <- fromJSON(tzLink)
			TZ <- tzData$valve_timezone
			# defining the "now"
			current.time <- as.POSIXct(Sys.time(), tz = TZ)
			# x-axis labels
			t1 = current.time - (24 * 60 * 60 * 7)
			t2 = current.time + (24 * 60 * 60 * 7)
			ts.axis <- as.POSIXct(seq(t1, t2, (24 * 60 * 60)), origin = "1970-01-01", tz = TZ)

			# -------Checking if smart watering or not-------
			smartLink <- paste("https://edynpepper.firebaseio.com/valves/v1/settings/", valveID, ".json?auth=Uj9kHlU25azPe1jH9Xq75I9yDv78pKku4tHOdTEs", 
				sep = "")
			ifSmart <- fromJSON(smartLink)
			#smart title
			tit <- paste("valve:", valveID, "smart:", ifelse(ifSmart$smartEnabled == "TRUE", "yes", "no"))

			# -------Now we're ready to pull and plot the sensor data-------
						#set up the plot output
			png(paste(gardenDirectory, "/", "valveSnapshot", "_", current.time, ".png", sep = ""), width = 800, height = 480)

			if (!is.na(sensorID)) {
				sensorLink = paste("http://readings.newton.edyn.com/devices/", sensorMacID, "/readings?limit=", readings, 
					"&offset=", offset, sep = "")
				sensorData <- fromJSON(sensorLink)
				data <- sensorData$response
				data$vwc <- 1.3 * ((log(data$capacitance)^2) * 1.478 + 16.7 * log(data$capacitance) + 45)
				data$vwc[data$vwc < 2] <- NA
				data$vwc[data$vwc > 60] <- NA

				yaxis <- "volumetric water content (%)"
				#now we actually plot the data
				plot(data[["vwc"]] ~ data[["timestamp"]], main = tit, ylab = yaxis, xlim = c(t1, t2), xaxt = "n", pch = 20, 
					type = "p", xlab = "", ylim = c(0, 40), col = "skyblue")
				axis.POSIXct(1, at = ts.axis, labels = format(ts.axis, "%m/%d"), las = 2)
				if (mean(data[["electrical_conductivity"]]) < 0.1) {
					text(current.time, 5, "check ec", col = "red")
				}
			} else {
				#empty plot if there is no linked sensor
				plot(0, main = tit, xlim = c(t1, t2), xaxt = "n", xlab = "", ylab = "", yaxt = "n")
				axis.POSIXct(1, at = ts.axis, labels = format(ts.axis, "%m/%d"), las = 2)
			}
			abline(v = current.time, lty = 2, lwd = 0.5)

			# -------Next we pull the valve History-------			
			valveHistLink <- paste("http://api.valve.prod.edyn.com/readings/", valveMacID, "?limit=", limit, "&token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzY29wZXMiOlsicmVhZGluZ3M6bGlzdCJdLCJpYXQiOjE0NTYzNTg5NzksInN1YiI6ImFwcDphcHA6aWRlbnRpdHktd29ya2VycyJ9.gGtqyXrPDFwT33hdWgEWhgXG6bNpLcHCDSUStCHT70A", 
				sep = "")
			valveHistJSON <- fromJSON(valveHistLink)

			# -------So that we can graph out the past watering events-------
			ValveWater <- valveHistJSON[valveHistJSON$valveOpen == "TRUE", ]
			ValveWater$time <- as.POSIXct(ValveWater$timestamp, origin = "1970-01-01", tz = TZ)
			abline(v = ValveWater$time, lwd = 0.5, lty = 1, col = rgb(39/255, 64/255, 139/255, 0.5))

			# labels for watering length	
			if (length(ValveWater[, 1]) > 0) {
				h2oLength <- xts(ValveWater$timestamp, order.by = ValveWater$time)
				start <- apply.daily(h2oLength, function(x) min(x))
				end <- apply.daily(h2oLength, function(x) max(x))
				diff.min <- round((end - start)/60, digits = 0)
				text(sort(ValveWater$time[endpoints(ValveWater$time, on = "days")]), 20, labels = diff.min, col = rgb(0/255, 
					0/255, 128/255))
			}

			# -------And now we want the upcoming scheduled watering-------			
			upLink <- paste("https://edynpepper.firebaseio.com/valves/v1/valves-upcoming/", valveID, ".json?auth=Uj9kHlU25azPe1jH9Xq75I9yDv78pKku4tHOdTEs", 
				sep = "")
			upData <- fromJSON(upLink)
			upData$time <- as.POSIXct(upData$datetime, origin = "1970-01-01", tz = TZ)
			upData$time2 <- upData$time + (upData$duration * 60)
			abline(v = c(upData$time, upData$time2), lwd = 0.5, lty = 1, col = "darkred")
			text(upData$time, ifelse(!is.na(sensorID), 20, 0), labels = upData$duration, col = "darkred")

			# -------Legend-------			
			legend("topright", c("soil moisture", "past irrigation (min)", "future irrigation (min)", "now"), pch = c(20, 
				35, 35, NA), lty = c(0, 1, 1, 3), col = c("skyblue", "royalblue4", "darkred", "black"), box.lwd = 0, box.col = "white", bg = "white")
			dev.off()
		}
	}
}
