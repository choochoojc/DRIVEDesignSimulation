# DRIVEDesignSimulation
This is the repository for the code used to decide the statistical design in DRIVE domain. Including code for the simulation, results manipulation and visualization.

# VFDS_pdf.rds
This file contains the probabilities of experiencing each outcome for the nine-level ordinal outcome used in the simulations for the DRIVE trial. As how it is presented in Table 1.

# Simulation_Code.R
This file contains the simulation code for the trial design. You can change the input for the `DB_simulation` function to alter the design setting. It also includes a chunk of code for parallel computing to run the function more efficiently on high performance computers. The results will be used to plot the figures and make the tables.

# Figure1_Code.R
This file contains the data extraction, manipulation and plotting code for Figure 1. The data (results) read in this file are the ones for the first stage simulation where we tried to choose the relatively well-performed designs. The way that the data are extracted and manipulated is rather manual, in the sense that, each trial design output are read and changed one-by-one. This R file aims to provide guidance on how we used the results from the simulation and how Table 3 and Figure 1 was constructed.

# Figure2_Code.R
This file contains the data extraction, manipulation and plotting code for Figure 2, and specifically for output for second stage simulation where we decided the one optimal design. Although only the code for one design option is presented, using the exactly same code with different `Simulation_Code.R` output can give the results manipulation of any design option. The way that the data are extracted and manipulated is rather manual, in the sense that, each trial design output are read and changed one-by-one. This R file aims to provide guidance on how we used the results from the simulation and how Figure 2  was constructed.

# Figure3_Code.R
This file contains the data extraction, manipulation and plotting code for Figure 3, and specifically for output for the finally chosen optimal design. 
