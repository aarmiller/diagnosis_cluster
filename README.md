Diagnostic Delay Clustering Repository
================

This repository contains scripts and synthetic data to perform cluster
analysis for the discovery of symptoms and other antecedent conditions
that indicate missed diagnostic opportunities have occurred prior to an
index disease diagnosis. The analysis can be performed using common
administrative datasets to generate recommendations of potential
antecedent conditions to consider.

The basic cluster analysis is performed as follows:

1.  Counts of healthcare visits each day prior to an index diagnoses are
    computed for each type of code under consideration (e.g., diagnosis,
    procedure, medication codes).

2.  Temporal trends of each visit type (e.g., visits for a given ICD-9
    code) are estimated using a selected curve fitting procedure.

3.  A clustering approach (e.g., k-means) is applied to the estimated
    parameters across the different conditions to identify clusters of
    conditions with similar visit trends

4.  A focal cluster is identified based on a well known symptom, or
    group of symptoms. This focal cluster is then analyzed for other
    conditions that are clinically-plausible markers of diagnostic
    delay.

## Overview of repository structure

There are three primary folders in this repository containing the steps
necessary for analysis.

1.  **scripts** - contains scripts for computing visit counts, fitting
    visit trends and performing cluster analysis

2.  **functions** - contains the functions necessary for performing the
    various procedures in this analysis

3.  **data** - contains synthetic datasets to replicate the steps of the
    analysis as a tutorial
    
    
## Example using synthetic data
