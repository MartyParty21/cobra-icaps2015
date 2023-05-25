#!/bin/bash

# list of environments
ENVS="empty-hall-r25 warehouse-r25 ubremen-r27"

# run the experiment for each environment

cd experiment/

for ENV in $ENVS
do
    echo "-------------------------------------------------------"
    echo " Preparing Plots for $ENV"
    echo "-------------------------------------------------------"

    # run R script to generate the plots
      Rscript makeplots-picoagents.r $ENV

      echo "-------------------------------------------------------"
      echo " PDF with plot has been generated to file plots/$ENV.pdf"
      echo "-------------------------------------------------------"
done
