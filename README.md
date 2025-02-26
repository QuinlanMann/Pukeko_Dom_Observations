# Data and code required to calculate the David's Scores for scored Pukeko groups

## This repository includes all scripts and data to reproduce the analyses associated with the manuscript: 

## This dataset, repository, and readme are not static, text is incomplete
  The version of the dataset will reflect completeness when the version No. begins with 1
  
## A static copy of this repository is available on .

# Reference Information

## Provenance for this README

-   File name: README
-   Author: Quinlan M Mann
-   Other contributors: 
-   Date created: 2025-02-17
-   Date modified: 2025-02-26

## Dataset Version and Release History

-   Current Version:
    -   Number: 0.0.5
    -   Date: 2025-02-26
    -   Persistent identifier: 
    -   Summary of changes: UPLOADED DATA ON N COAST SE GROUP
-   Embargo Provenance: n/a
    -   Scope of embargo: n/a
    -   Embargo period: n/a

## Dataset Attribution and Usage

-   Dataset Title: 
-   Persistent Identifier: 
-   Dataset Contributors:

    -   Creators: Quinlan M Mann, Cody J Dey, James S Quinn

-   Date of Issue: 

-   Publisher: McMaster University

-   License: Use of these data is covered by the following license:

    -   Title: CC0 1.0 Universal (CC0 1.0)
    -   Specification: <https://creativecommons.org/publicdomain/zero/1.0/>; the authors respectfully request to be contacted by researchers interested in the re-use of these data so that the possibility of collaboration can be discussed.

-   Suggested Citations:

    -   Dataset citation: \>
    -   Corresponding publication: \> 

## Contact Information

-   Name: Quinlan M. Mann

    -   Affiliations: Department of Biology, McMaster University
    
    -   ORCID ID: 0009-0005-3835-0457
    
    -   Email: [mannq\@mcmaster.ca](mailto:mannq@mcmaster.ca){.email}
    
    -   Alternate Email: [quinlanmmann\@gmail.com](mailto:quinlanmmann@gmail.com){.email}
    
    -   Address: e-mail preferred

-   Name: Cody J Dey

    -   Affiliations: \>
    
    -   ORCID ID: \>
    
    -   Email: \>
    
    -   Alternate Email: \>
    
    -   Address: \>
    
-   Alternative Contact: Corresponding Author and Supervisor

    -   Name: James S Quinn

    -   Affiliations: Department of Biology, McMaster University

    -   ORCID ID: 0000-0003-3551-3079

    -   Email: [quinn\@mcmaster.ca](mailto:quinn@mcmaster.ca){.email}
    
    -   Address: Life Sciences Building 435, McMaster University, Hamilton, ON L8S 4L8

------------------------------------------------------------------------

# Additional Dataset Metadata

## Acknowledgements

-   Funding sources: Natural Sciences and Engineering Research Council of Canada (NSERC) discovery grant to JSQ. Housing provided by Auckland Regional Council. Permission to work on the land granted by both Auckland Regional Council, and Ngati Manuhiri trust.

## Dates and Locations

-   Dates of data collection: March 2012-April 2012; August 2024-November 2024

-   Geographic locations of data collection: Fieldwork conducted in Tawharanui Regional Park, 0986, 1181 Takatu Road, Auckland, New Zealand.

------------------------------------------------------------------------

# Methodological Information

-   Methods of data collection/generation: 

# Links to other publicly accessible locations of the data

-   DOI: 
------------------------------------------------------------------------

# Data and File Overview

## Summary Metrics

-   File count: 4
-   Total file size: 100.3 KB
-   Range of individual file sizes: 9.12 KB - 36.8 KB
-   File formats: .csv, .R, .xlsx

## Table of Contents

-   SBAN_Reproductive_success.R
-   NEST_SUMMARY_ANIS.csv
-   Supplemental_Tables.xlsx
-   README.md

## Setup

-   Unpacking instructions: n/a

-   Relationships between files/folders: n/a

-   Recommended software/tools: 
    - R version 4.4.1 
    - RStudio version 2024.4.2.764
    - Microsoft Excel: Version 2407 (or equivalent)

## File Usage

# File/Folder Details

## Details for: SBAN_Reproductive_success.R
-   Description: An r file containing code necessary to run all analyses
-   Format(s): .R
-   Size(s): 36.8 KB
-   Dimensions: 892 lines
-   Variables: n/a

## Details for: NEST_SUMMARY_ANIS.csv

-   Description: Data used for analyses of Smooth-billed ani reproductive success
-   Format(s): .csv
-   Size(s): 18.3 KB
-   Dimensions: 453R x 13C
-   Variables:
    -   YEAR: Calendar year that the nest and group were observed
    -   LOCATION: Location within the park wherein the nest was observed
    -   SITE: U.S. Fish and Wildlife Service (USFWS) National Wildlife Refuges (or adjacent property) where the group and nest was observed
        - LC: Laguna Cartagena
        - CR: Cabo Rojo
        - UNK: Unknown  
    -   ADULTS: Number of adults observed within the group
        - see manuscript for details on how this was determined
    -   NEST_ATTMPT: Numbered nesting attempt for that group within the year
    -   TOT_EGGS: total number of eggs that were counted during nest checks
        - see manuscript for details on how this was determined
    -   EGGS_BURIED: number of eggs found within the nest cup below layers of nesting material after groups had completed the nesting attempt
    -   EGGS_EJECTED: number of eggs found outside the nest during active laying
    -   EGGS_UNBURIED: number of eggs found within the nest cup that were incubated
    -   HATCHED: number of eggs that were known to have hatched
    -   FLEDGED: number of chicks that were known to have fledged 
    -   NEST FATE: IF KNOWN, the fate of the nest
        -   ABD: Abandoned
        -   ABD?: uncertain if abandoned
        -   DEST: destroyed
        -   DESTROYED: Destroyed
        -   DP: Depredated
        -   DP PSUC: Depredated but some chicks survived
        -   FAIL: Failure
        -   INS: Insufficient data
        -   PSUC: partial success
        -   SUCC: Successfully fledged
        -   UNK: Unknown
    -   Notes: IF PRESENT, notes about the nest/group

## Details for: Supplemental_Tables.xlsx
-   Description: Supplemental results for analyses of Smooth-billed ani reproductive success
-   Format(s): .xlsx
-   Size(s): 35.1 KB
-   Dimensions: 7 sheets
    - S1 Summary statistics of different datasets
        - Summary statistics of all datasets
    - S2 First nest only data, count models
        - model outputs from count data assessing overall and per capita success across multiple reproductive stages
        - uses the dataset that looked at only first (known) nests per group if multiple occurred
    - S3 First nest only data, probability models
        - model outputs from count data assessing proportion of success across multiple reproductive stages
        - uses the dataset that looked at only first (known) nests per group if multiple occurred
    - S4 First successful nest only data, count models
        - model outputs from count data assessing overall and per capita success across multiple reproductive stages
        - uses the dataset that looked at only first (known) successful nests per group if multiple occurred
    - S5 First successful nest only data, probability models
        - model outputs from count data assessing proportion of success across multiple reproductive stages
        - uses the dataset that looked at only first (known) successful nests per group if multiple occurred
    - S12 Nest count and group size
        - Dimensions: 366R x 11C
        - Variables:
            - Female group size: Female group size, see man uscript for details 
            - # of Nest attempts: Number of (known) nesting attempts
        - Also includes output of Fisher's exact test
    - S13 Survival of nest and group size:
        - Dimensions: 415 x 11C
        - Variables:
            - Female group size: Female group size, see man uscript for details 
            - Survival status of nest: Categorical variable of success
                - If group fledged at least one chick it was determined to be successful (SUCC)
                - If group did not fledge at least one chick it was determined to be a failure (FAIL)
        - Also includes output of Fisher's exact test

## Details for: README.md
-   Description: Read me containing pertinent information for the dataset and manuscript
-   Format(s): .txt
-   Size(s): 9.12 KB
-   Dimensions: 241 lines

END OF README