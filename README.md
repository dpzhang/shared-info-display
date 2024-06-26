# Supplemental Material for "Designing Shared Information Displays for Agents of Varying Strategic Sophistication"

Authors: Dongping Zhang, Jason Hartline, Jessica Hullman

Full Paper: https://doi.org/10.1145/3637319

arXiv: https://arxiv.org/abs/2310.10858

Blog Post: https://link.growkudos.com/1mi020o13i8

## About this repository:

This directory contains all supplemental materials associated with the ACM CSCW'24 submission titled [**Designing Shared Information Displays for Agents of Varying Strategic Sophistication**](https://arxiv.org/abs/2310.10858). Due to file size constraints, certain input and output files, such as the [**Chicago Taxi Trips dataset**](https://data.cityofchicago.org/Transportation/Taxi-Trips/wrvz-psew) and the fitted Bayesian models, are not included within this repository. Instead, directories have been created to store the modeling objects, should viewers choose to reproduce the analysis using the provided scripts. The structure and contents of the repository are detailed in the table below.

**Notice**: Please ensure your system has at least 120 GB of disk space. Several processing scripts, such as those used to generate taxi drivers' priors, can take more than 12 hours to run. This timeing is based on test runs on a laptop equipped with a 2.3 GHz 8-core Intel Core i9 and 16 GB of memory.

## Content:

| Directory   | File/Subdirectory           | Content Description                                                                                                                                                                                                                                                     |
| ----------- | --------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `interface` | `interface.pdf`             | This directory contains a PDF of interface screenshots for level-1s during stage 1.                                                                                                                                                                                     |
| `report`    | `supplemental-material.pdf` | This PDF file demonstrates (1) the experimental setup, (2) the counterfactual model we used to create a three-action congestion game, (3) the Earth Mover's Distance, (4) social welfare optimization, and (5) Bayesian model checks.                                   |
| `scripts`   | `1-taxiTrips`               | Directory containing scripts used to stratify and process taxi trip data.                                                                                                                                                                                               |
| `scripts`   | `2-counterfactual`          | Directory containing scripts used to fit the Bayesian counterfactual model to create the three-action congestion game for the experiment.                                                                                                                               |
| `scripts`   | `3-experiment`              | Directory containing scripts used to process and create conditions for each stage of the experiment.                                                                                                                                                                    |
| `scripts`   | `4-database`                | Directory containing user response data.                                                                                                                                                                                                                                |
| `scripts`   | `5-process`                 | Directory containing scripts for cleaning and processing individual-level response data, downloaded from Google Firebase in JSON format. Also includes scripts to generate aggregate-level system outcomes by combining individual decisions, as detailed in the paper. |
| `scripts`   | `6-analysis`                | Directory containing scripts used to analyze the response data reported in the paper.                                                                                                                                                                                   |
