# Energy_index

This repository contains code and data that can be used to reproduce the results in the following paper: 

> Allen, S. and Otero, N (2023). 
> Standardised indices to monitor energy droughts.
> Renewable Energy.
> [https://doi.org/10.1016/j.renene.2023.119206](https://doi.org/10.1016/j.renene.2023.119206).

## Standardised Energy Indices

This paper introduces standardised energy indices that can be used to monitor energy demand, production, reisdual load, and other relevant energy variables. Standardised indices have a straightforward probabilistic interpretation, making them ideal for risk management and decision making, and are commonly used operationally to monitor hydrological droughts. Standardised energy indices could similarly be employed to define energy droughts, helping to monitor (and avoid) shortages in renewable energy systems.

## Code

The [SEI package](https://github.com/noeliaof/SEI) has been designed to calculate and visualise standardised indices in practice, and this repository presents an application of the SEI package to renewable energy production and demand.

The file main.R is an R script that reproduces the results in the above paper. The folder functions/ contains utility functions that are used to generate and plot these results. 
