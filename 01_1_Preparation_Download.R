if (dir.exists("datacode")) {
  cat("The data from Kotz et al. 2024 seems to be ready (folder 'datacode'). Do nothing.\n")
} else {
  cat("Downloading Data and code for 'The economic commitment of climate change', Version 1.1\n")
  cat("DOI: 10.5281/zenodo.10568856\n")
  cat("...")
  temp <- tempfile()
  options(timeout = max(36000, getOption("timeout")))
  download.file("https://zenodo.org/records/10568856/files/datacode.zip", temp)
  cat("Unzipping...\n")
  unzip(temp)
  unlink(temp)
}
