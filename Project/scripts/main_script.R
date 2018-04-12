#main_script.R
library(plyr)

codebook = read.csv2("./Project/data/codebook.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
religion_data = read.csv2("./Project/data/religion_data.csv", header = TRUE, sep = ",", na.strings = "", dec = ".", 
                          fileEncoding="UTF-8-BOM")
religion_data[is.na(religion_data)] = 0

#Select subsets of data - CNG, ADH, RATE
CNG.var = subset(codebook, grepl("CNG$", variable_names))$variable_names
CNG.names = subset(codebook, grepl("CNG$", variable_names))$religion_name
ADH.var = subset(codebook, grepl("ADH$", variable_names))$variable_names
ADH.names = subset(codebook, grepl("ADH$", variable_names))$religion_name
RATE.var = subset(codebook, grepl("RATE$", variable_names))$variable_names
RATE.names = subset(codebook, grepl("RATE$", variable_names))$religion_name

CNG.data = religion_data[,CNG.var]
ADH.data = religion_data[,ADH.var]
RATE.data = religion_data[,RATE.var]

#Define Cosine Similarity
cosine.similarity = function(X, Y)
{
  return(t(as.matrix(X)) %*% as.matrix(Y)/
           (norm(as.matrix(X), type = "2")*norm(as.matrix(Y), type = "2")))
}

#CNG sims
CNG.sims = data.frame(matrix(data = NA, nrow = length(CNG.var), ncol = length(CNG.var)))
rownames(CNG.sims) = make.names(CNG.names)
colnames(CNG.sims) = make.names(CNG.names)
for(i in 1:nrow(CNG.sims))
{
  for(j in 1:ncol(CNG.sims))
  {
    CNG.sims[i,j] = cosine.similarity(CNG.data[,i], CNG.data[,j])
  }
}

#ADH sims
ADH.sims = data.frame(matrix(data = NA, nrow = length(ADH.var), ncol = length(ADH.var)))
rownames(ADH.sims) = make.names(ADH.names)
colnames(ADH.sims) = make.names(ADH.names)
for(i in 1:nrow(ADH.sims))
{
  for(j in 1:ncol(ADH.sims))
  {
    ADH.sims[i,j] = cosine.similarity(ADH.data[,i], ADH.data[,j])
  }
}

#RATE sims
RATE.sims = data.frame(matrix(data = NA, nrow = length(RATE.var), ncol = length(RATE.var)))
rownames(RATE.sims) = make.names(RATE.names)
colnames(RATE.sims) = make.names(RATE.names)
for(i in 1:nrow(RATE.sims))
{
  for(j in 1:ncol(RATE.sims))
  {
    RATE.sims[i,j] = cosine.similarity(RATE.data[,i], RATE.data[,j])
  }
}

#Find most similar religions to a given religion
most_sim = function(sim_df, rel_name, n)
{
  sim_df  = sim_df[order(sim_df[,rel_name]),]
  return(rownames(sim_df)[1:n])
}

most_sim(CNG.sims, "Zoroastrian", 5)

##corplot







