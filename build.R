# this is the build script. Run the script from top to bottom to reproduce the results

# 1. Extract the language endangerment scores
system("RScript ./get_endangerment.R")

# 2. Cross reference the endangerment scores with Grambank
system("RScript ./cross_datasets.R")

# 3. Cross reference Endangerment, Grambank, and Kidd & Garcia 2022


# 4. Make figures
dir.create("figures/", recursive = TRUE)
system("RScript ./kiddgarcia_egids.R") # Figure 1 & Figure 3
system("RScript ./functional_diversity.R") # Figure 2
system("RScript ./arima_model.R") # Figure 4

