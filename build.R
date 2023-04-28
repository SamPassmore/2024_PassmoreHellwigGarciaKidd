# this is the build script. Run the script from top to bottom to reproduce the results

# 1. Extract the language endangerment scores
system("RScript ./get_endangerment.R")

# 2. Cross reference the endangerment scores with Grambank
system("RScript ./cross_datasets.R")

# 3. Make figures
system("RScript ./plot_grambankegids.R")
