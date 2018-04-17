#main_script.R
library(plyr)
library(corrplot)

#read in codebook
codebook = read.csv2("./Project/data/codebook.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#read in county level data
county_data = read.csv2("./Project/data/religion_data.csv", header = TRUE, sep = ",", na.strings = "", dec = ".", 
                          fileEncoding="UTF-8-BOM")

#read in state level data
state_data = read.csv2("./Project/data/state_data.csv", header = TRUE, sep = ",", na.strings = "", dec = ".", 
                       fileEncoding="UTF-8-BOM")

#replace NAs with 0s
county_data[is.na(county_data)] = 0
state_data[is.na(state_data)] = 0

#Select subsets of data - CNG (number of congregations), ADH (number of adherents), RATE (rate of adherents)
CNG.var = subset(codebook, grepl("CNG$", variable_names))$variable_names
CNG.names = subset(codebook, grepl("CNG$", variable_names))$religion_name
ADH.var = subset(codebook, grepl("ADH$", variable_names))$variable_names
ADH.names = subset(codebook, grepl("ADH$", variable_names))$religion_name
RATE.var = subset(codebook, grepl("RATE$", variable_names))$variable_names
RATE.names = subset(codebook, grepl("RATE$", variable_names))$religion_name

CNG.data.county = county_data[,CNG.var]
ADH.data.county = county_data[,ADH.var]
RATE.data.county = county_data[,RATE.var]

CNG.data.state = state_data[,CNG.var]
ADH.data.state = state_data[,ADH.var]
RATE.data.state = state_data[,RATE.var]
  
  
#Define Cosine Similarity function
cosine.similarity = function(X, Y)
{
  return(t(as.matrix(X)) %*% as.matrix(Y)/
           (norm(as.matrix(X), type = "2")*norm(as.matrix(Y), type = "2")))
}

#CNG county level similarity matrix
CNG.sims.county = data.frame(matrix(data = NA, nrow = length(CNG.var), ncol = length(CNG.var)))
rownames(CNG.sims.county) = make.names(CNG.names)
colnames(CNG.sims.county) = make.names(CNG.names)
for(i in 1:nrow(CNG.sims.county))
{
  for(j in 1:ncol(CNG.sims.county))
  {
    CNG.sims.county[i,j] = cosine.similarity(CNG.data.county[,i], CNG.data.county[,j])
  }
}

#ADH county level similarity matrix
ADH.sims.county = data.frame(matrix(data = NA, nrow = length(ADH.var), ncol = length(ADH.var)))
rownames(ADH.sims.county) = make.names(ADH.names)
colnames(ADH.sims.county) = make.names(ADH.names)
for(i in 1:nrow(ADH.sims.county))
{
  for(j in 1:ncol(ADH.sims.county))
  {
    ADH.sims.county[i,j] = cosine.similarity(ADH.data.county[,i], ADH.data.county[,j])
  }
}

#RATE county level similarity matrix
RATE.sims.county = data.frame(matrix(data = NA, nrow = length(RATE.var), ncol = length(RATE.var)))
rownames(RATE.sims.county) = make.names(RATE.names)
colnames(RATE.sims.county) = make.names(RATE.names)
for(i in 1:nrow(RATE.sims.county))
{
  for(j in 1:ncol(RATE.sims.county))
  {
    RATE.sims.county[i,j] = cosine.similarity(RATE.data.county[,i], RATE.data.county[,j])
  }
}

#CNG state level similarity matrix
CNG.sims.state = data.frame(matrix(data = NA, nrow = length(CNG.var), ncol = length(CNG.var)))
rownames(CNG.sims.state) = make.names(CNG.names)
colnames(CNG.sims.state) = make.names(CNG.names)
for(i in 1:nrow(CNG.sims.state))
{
  for(j in 1:ncol(CNG.sims.state))
  {
    CNG.sims.state[i,j] = cosine.similarity(CNG.data.state[,i], CNG.data.state[,j])
  }
}

#ADH state level similarity matrix
ADH.sims.state = data.frame(matrix(data = NA, nrow = length(ADH.var), ncol = length(ADH.var)))
rownames(ADH.sims.state) = make.names(ADH.names)
colnames(ADH.sims.state) = make.names(ADH.names)
for(i in 1:nrow(ADH.sims.state))
{
  for(j in 1:ncol(ADH.sims.state))
  {
    ADH.sims.state[i,j] = cosine.similarity(ADH.data.state[,i], ADH.data.state[,j])
  }
}

#RATE state level similarity matrix
RATE.sims.state = data.frame(matrix(data = NA, nrow = length(RATE.var), ncol = length(RATE.var)))
rownames(RATE.sims.state) = make.names(RATE.names)
colnames(RATE.sims.state) = make.names(RATE.names)
for(i in 1:nrow(RATE.sims.state))
{
  for(j in 1:ncol(RATE.sims.state))
  {
    RATE.sims.state[i,j] = cosine.similarity(RATE.data.state[,i], RATE.data.state[,j])
  }
}

#Find most similar religions to a given religion based on a similarity matrix
most_sim = function(sim_df, rel_name, n)
{
  sim_df  = sim_df[order(sim_df[,rel_name]),]
  return(rownames(sim_df)[1:n])
}

#Return Cosine similarity between two religions
two_sim = function(sim_df, rel1_name, rel2_name)
{
  sim = sim_df[rel1_name, rel2_name]
  return(sim)
}



#Test most_sim function and compare across different similarity matrices
most_sim(CNG.sims.county, "Zoroastrian", 5)
most_sim(ADH.sims.county, "Zoroastrian", 5)
most_sim(RATE.sims.county, "Zoroastrian", 5)
most_sim(CNG.sims.state, "Zoroastrian", 5)
most_sim(ADH.sims.state, "Zoroastrian", 5)
most_sim(RATE.sims.state, "Zoroastrian", 5)

#Test two_sim function on two presbyterians across all similarity matrices
two_sim(CNG.sims.county, make.names("Presbyterian Church in America"), make.names("Presbyterian Church (U.S.A.)"))
two_sim(ADH.sims.county, make.names("Presbyterian Church in America"), make.names("Presbyterian Church (U.S.A.)"))
two_sim(RATE.sims.county, make.names("Presbyterian Church in America"), make.names("Presbyterian Church (U.S.A.)"))
two_sim(CNG.sims.state, make.names("Presbyterian Church in America"), make.names("Presbyterian Church (U.S.A.)"))
two_sim(ADH.sims.state, make.names("Presbyterian Church in America"), make.names("Presbyterian Church (U.S.A.)"))
two_sim(RATE.sims.state, make.names("Presbyterian Church in America"), make.names("Presbyterian Church (U.S.A.)"))

##Corrplot - Correlation plot of major groups
major.religions.county = RATE.sims.county[c("All.denominations.groups", "Evangelical.Protestant",
                                            "Mainline.Protestant", "Catholic", "Orthodox", "Other"), 
                                          c("All.denominations.groups", "Evangelical.Protestant",
                                            "Mainline.Protestant", "Catholic", "Orthodox", "Other")]
major.religions.state = RATE.sims.state[c("All.denominations.groups", "Evangelical.Protestant",
                                          "Mainline.Protestant", "Catholic", "Orthodox", "Other"), 
                                        c("All.denominations.groups", "Evangelical.Protestant",
                                          "Mainline.Protestant", "Catholic", "Orthodox", "Other")]

corrplot(as.matrix(major.religions.county), method = "circle", type = "upper")
corrplot(as.matrix(major.religions.state), method = "circle", type = "upper")






