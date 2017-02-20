Pkg.init()

# Let PyCall use system Python instead of Conda.
ENV["PYTHON"] = "/usr/bin/python3"

Pkg.add("ZMQ")
Pkg.add("JSON")
Pkg.add("DataFrames")
Pkg.add("PyCall")
Pkg.add("Plots")
Pkg.add("PyPlot")
Pkg.add("Gadfly")
Pkg.add("Winston")
