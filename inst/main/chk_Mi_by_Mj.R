# 07.07.2021
# We compute the ratio Mi/Mj in the food web data


bs_data <- read.csv("~/Google Drive/PhD (UZH)/tadnoll/AfonHirnantData_Gilljam_etal_AER_v45ch3.csv", comment.char="#")


Mi <- bs_data$preyMass
Mj <- bs_data$predMass

Mi_by_Mj <- Mi/Mj

ind <- which(Mi_by_Mj == min(Mi_by_Mj))


bs_data_extreme <- bs_data[ind,]

                           