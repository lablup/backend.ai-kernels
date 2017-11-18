# Demo for MRO and MKL

# Snippets taken from https://mran.revolutionanalytics.com/documents/rro/multithread/
cat("R version\n")
print(R.version)
cat("=========\n")

if(exists("Revo.version")) {
  cat("Revo version\n")
  print(Revo.version)
  cat("=========\n")
}

print(sessionInfo())
cat("The package RevoUtilsMath is already attached.\n")
cat("=========\n")
print(options("repos"))
cat("A MRAN snapshot is configured as CRAN repository.\n")
cat("=========\n")

cat("Run Simon Urbanek's benchmark v2.5:")
# https://mran.revolutionanalytics.com/documents/rro/multithread/#mt-setget
totalTestTime <- system.time({
  source(url("http://r.research.att.com/benchmarks/R-benchmark-25.R"))
})
cat("Total test time:", totalTestTime[[3]], "seconds\n")
cat("Try the same benchmark on your host machine!\n")
cat("============\n")
