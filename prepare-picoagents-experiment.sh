#!/bin/bash

# copy the binaries to the experiment folder
cp target/map4rt-1.0-SNAPSHOT-jar-with-dependencies.jar experiment/solver.jar

cd experiment

CPUS=8 # no of CPU to be used for the experiment (how many simulations to run in parallel)
MEM=4  # maximum memory used by one simulation run in GBs
SUFFIX=-Distributed

# prepare the experiment in empty-hall environment
./prepare.sh empty-hall-r25 $SUFFIX


# prepare experiment in ubremen environment
./prepare.sh ubremen-r27 $SUFFIX

# prepare the experiment in warehouse environment
./prepare.sh warehouse-r25 $SUFFIX

echo -e "\n >>>> Done! Instances generated in experiment/instances folder. \n"
