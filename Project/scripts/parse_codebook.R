#parse_codebook.R

text = readLines("./Project/data/codebook.txt")
text = text[-c(1:2)]

variable_names = text[grep("^[0-9]{1,3})", text)]
variable_names = sapply(strsplit(variable_names, " "), `[`, 2)

variable_desc = text[grep("^[A-Za-z\\(]+", text)]
variable_desc = gsub(" \\(2010\\)$", "", variable_desc)

religion_name = sapply(strsplit(variable_desc, "--"), `[`, 1)

codebook = data.frame(variable_names, variable_desc, religion_name)

write.table(codebook, "./Project/data/codebook.csv", row.names = FALSE, col.names = TRUE, sep = ",")
