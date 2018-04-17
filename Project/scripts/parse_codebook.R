#parse_codebook.R

#Read in the codebook from file
text = readLines("./Project/data/codebook.txt")
text = text[-c(1:2)]

#parse variable names
variable_names = text[grep("^[0-9]{1,3})", text)]
variable_names = sapply(strsplit(variable_names, " "), `[`, 2)

#parse variable descriptions
variable_desc = text[grep("^[A-Za-z\\(]+", text)]
variable_desc = gsub(" \\(2010\\)$", "", variable_desc)

#extract religion names
religion_name = sapply(strsplit(variable_desc, "--"), `[`, 1)

#store parsed results in a codebook dataframe
codebook = data.frame(variable_names, variable_desc, religion_name)

#write to csv
write.table(codebook, "./Project/data/codebook.csv", row.names = FALSE, col.names = TRUE, sep = ",")
