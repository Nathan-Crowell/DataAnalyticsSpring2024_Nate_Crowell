# In this lab, we perform PCA on the USArrests data set, 
# which is part of the base R package. 
#The rows of the data set contain the 50 states, in alphabetical order. 
# We will use the USAArrest data that available on RStudio 
data("USArrests") 
help("USArrests")

states = row.names(USArrests)

names(USArrests)
