Pkg.init()

# Let PyCall use system Python instead of Conda.
ENV["PYTHON"] = "/usr/local/bin/python3"

Pkg.add("DataFrames")
Pkg.add("JSON")
Pkg.add("PyCall")
Pkg.add("Plots")
Pkg.add("PyPlot")
