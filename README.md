# Bayesian sample size determination: a simulation study for logistic regression
## Coen Willemsen, 23-6-2025
This repository contains the files associated with my bachelor thesis. The code for all the statistical algorithms, simulation scripts, analyses and raw data is included.

In the ```sequential``` folder, you'll find the code that runs a Bayesian sequential design called ```seq.R```. The ```seq_params.R``` file contains the code to loop over that function with different parameter sets, outputting the data to the ```data``` folder.

In the ```apriori``` folder, the script to run a Bayesian a priori sample size determination is included called ```apriori.R```. The ```apriori_params.R``` simulates that file across different parameter sets serially. 
This takes about 9 hours to complete, so the ```apriori_params_parallel``` is a parallelized version. The final data was generated using the parallelized function, which took about 3 hours.

The ```helpers``` file contains some helper functions, like functions to compute the Bayes factor. There is also a helper function that sources all helper functions, which reduces lines needed in the preamble of the main functions.

In the ```data``` folder, you'll find the data as well as analysis scripts. The data is in CSV format, with the ```seq_sim.csv``` file containing the data from the sequential design and the ```apriori_sim_parallel.csv``` containing the apriori data.
The analysis scripts take in that data and produce the results as can be found in the thesis.

Finally, the ```plots``` directory contains some scripts to create plots, and also the plots in PNG format themselves.
