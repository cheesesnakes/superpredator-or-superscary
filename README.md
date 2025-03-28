# Are super-predators also super-scary? A systematic review and meta-analysis of animal responses to human interactions 

# Authors

Shawn Dsouza, Indian Institute of Science, Bangalore, India.

Kartik Shanker, Indian Institute of Science, Bangalore, India.

Maria Thaker, Indian Institute of Science, Bangalore, India.

# Abstract

Human-induced rapid environmental change poses a global threat to natural systems and the organisms that inhabit them. Human interactions with animals in natural spaces, both as predators (e.g., hunting, fishing) and through seemingly benign activities (e.g., tourism) can have significant impacts on animal behaviour. This synthesis examines the effects of lethal and non-lethal human interactions on foraging, vigilance, and movement behaviours of wild animals. We conducted a systematic review and meta-analysis of literature from the past three decades to analyse how these interactions affect animals. Lethal interactions, such as hunting and fishing, had substantial behavioural impacts, causing targeted species to increase vigilance and reduce foraging activity. In contrast, non-lethal activities like tourism and roads showed limited empirical support for fear-driven changes. The intensity of human impact may vary with the trophic level of affected species or historical interaction dynamics. To better understand these effects, future studies across diverse species and regions are needed.

# Description and Structure

This repository contains the code and data used to conduct a systematic review and meta-analysis of animal responses to human interactions. The analysis focuses on the effects of human presence on the behavior of animals, specifically looking at foraging, vigilance and movement behaviours.

The repository is divided into the following sections:

## `scoping-lit`

This folder contains data and code we used to conduct a pilot search and screening. The folder is structured as follows:

- `data/`: This folder contains a single CSV file, `scoping_search.csv`, which is the search exported from Web of Science.
- `1_lit-map.R`: This script contains code to create word clouds based on the title, abstract, and keywords of the articles in the scoping search. 
- `2_pilot-cleaning.R`: This script contains code to clean the scoping search data and create a cleaned version of the data. 
- `3_pilot-analysis.R`: This script contains code to filter out articles for full-text screening based on the cleaned data.

## `meta-analysis`

This folder contains the code and data used to conduct the meta-analysis. It is structured as follows:

- `data/`: Contains raw and processed data files used in the meta-analysis.
    - `animal_traits.csv`: Contains information on animal traits.
    - `authors.csv`: an exported list of authors and citations.
    - `chapter-1_covariates.csv`: Contains information on covariates used in the analysis such as location and habitat.
    - `chapter-1_effects.csv`: Contains information on effect sizes.
    - `chapter-1_full-text-screening.csv`: Contains information on articles that passed the full-text screening.
    - `chapter-1_study-info.csv`: Contains information on study characteristics such as study design, sampling methods, focal species, and human interaction type.
    - `clean_data.csv`: Contains the final exported tidy dataframe used in the analysis.
    - `effect-size.csv`: Contains the computed effect sizes for each study.
    - `populations.csv`: Contains a cleaned list of focal species along with information on trophic level and feeding guild.
    - `size.csv`: Contains information on the size of the focal species.
- `1_cleaning.R`: Script for cleaning and preparing the raw data for analysis.
- `1-1_populations.R`: Script for analyzing population-level data.
- `1-2_authors.R`: Script for author-level data processing.
- `2_conversions.R`: Script for converting raw data into effect sizes.
- `3_summaries.R`: Script for generating summary statistics and visualizations.
- `3-1_sankey.R`: Script for creating Sankey diagrams to visualize data flows.
- `4_effects-sizes.R`: Script for calculating effect sizes.
- `4-1_map.R`: Script for creating geographical maps of effect sizes.
- `5_comparison.R`: Script for comparing effect sizes across groups.
- `5-1_funcs.R`: Contains helper functions used in the analysis.
- `5-2_MLMA.R`: Script for conducting multilevel meta-analyses.
- `5-3_phylo.R`: Script for incorporating phylogenetic information into the analysis.
- `6_meta.R`: Script for running the main meta-analysis.
- `7_sensitivity.R`: Script for conducting sensitivity analyses.

# How to Run the Analysis

To reproduce the analysis in this repository, follow these steps:

1. **Clone the Repository**:
   Clone this repository to your local machine using the following command:
   ```bash
<<<<<<< HEAD
   git clone https://github.com/your-repo-url.git
=======
   git clone https://github.com/superpredator-or-superscary.git
>>>>>>> 007d2b9 (add Readme)
   cd analysis
   ```

2. **Install Required Packages**:
   Install the necessary R packages using the following command:
   ```R
   install.packages(c("tidyverse", "metafor", "styler", "httpgd", "languageserver"))
   ```

3. **Activate the Environment**:
   Activate the environment using the following command:
   ```R
   source("renv/activate.R")
   ```

4. **Run the Scripts**:
   Run the following scripts to conduct the analysis:
   ```R
   source("scoping-lit/1_lit-map.R")
   source("scoping-lit/2_pilot-cleaning.R")
   source("scoping-lit/3_pilot-analysis.R")
   source("meta-analysis/meta-analysis.R")
   ```

# Citation

This repository is associated with a manuscript currently under review. If you use this code or data in your research, please cite the repository as follows:

```bibtex

```