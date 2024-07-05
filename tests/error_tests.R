temp <- tempfile()
download.file(url = "https://zenodo.org/records/10698028/files/JGCRI/hector-v3.2.0.zip?download=1?raw=TRUE", destfile = temp, method = "auto", mode = "wb")
data <- read.csv(unz(temp, file.path("JGCRI-hector-461248a", "data-raw", "input_params.csv")))
unlink(temp)
