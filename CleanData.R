# Author: Peter Byrd
# Clean the data

# clean and categorize 
chf[chf$Age == 1, "AgeGrp"] = "less than 25"
chf[chf$Age == 2, "AgeGrp"] = "25-44"
chf[chf$Age == 3, "AgeGrp"] = "45-64"
chf[chf$Age == 4, "AgeGrp"] = "65-69"
chf[chf$Age == 5, "AgeGrp"] = "70-74"
chf[chf$Age == 6, "AgeGrp"] = "75-79"
chf[chf$Age == 7, "AgeGrp"] = "80-84"
chf[chf$Age == 8, "AgeGrp"] = "85-89"
chf[chf$Age == 9, "AgeGrp"] = "90 and over"

chf[chf$Sex == 1, "Sex2"] = 'Male'
chf[chf$Sex == 2, "Sex2"] = "Female"

chf[chf$dischdest == 20, "Expired"] = 1
chf[chf$dischdest != 20, "Expired"] = 0

chf$Sex2 <- factor(chf$Sex2)
chf$AgeGrp <- factor(chf$AgeGrp)
chf$drgcode <- factor(chf$drgcode)