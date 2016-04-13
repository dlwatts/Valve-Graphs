
#this sequence builds from valveID or userEmail

linkedEC <- function(userInput, readings = 7 * 24 * 6, offset = 0) {
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
			t1 = current.time - (24 * 60 * 60 * 14)
			t2 = current.time
			ts.axis <- as.POSIXct(seq(t1, t2, (24 * 60 * 60)), origin = "1970-01-01", tz = TZ)

			#smart title
			tit <- paste("electrical conductivity for", "sensor:", sensorID)

			# -------Now we're ready to pull and plot the sensor data-------			
			#set up the plot output
			png(paste(gardenDirectory, "/", "linkedEC", "_", current.time, ".png", sep = ""), width = 800, height = 480)

			if (!is.na(sensorID)) {
				sensorLink = paste("http://readings.newton.edyn.com/devices/", sensorMacID, "/readings?limit=", readings, 
					"&offset=", offset, sep = "")
				sensorData <- fromJSON(sensorLink)
				data <- sensorData$response

				yaxis <- "volts"
				#now we actually plot the data
				plot(data[["electrical_conductivity"]] ~ data[["timestamp"]], main = tit, ylab = yaxis, xlim = c(t1, t2), 
					xaxt = "n", pch = 20, type = "p", xlab = "", ylim = c(0, 2), col = "darkolivegreen3")
				axis.POSIXct(1, at = ts.axis, labels = format(ts.axis, "%m/%d"), las = 2)
				abline(h = 0.1, col = "red", lty = 3)
			}

			# -------Legend-------			
			legend("topright", "sensor EC", pch = 20, col = "darkolivegreen3", box.lwd = 0, box.col = "white", bg = "white")
			dev.off()
		}
	}
}

