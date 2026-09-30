---
editor_options: 
  markdown: 
    wrap: 72
---

# L2 Spanish Subject Realization: German L1 Learners vs. Spanish L1 Natives

This repository contains the data and R scripts for the study: **"The
subject realization in L2 Spanish by German L1 speakers: A corpus
study"**

- **Publication DOI:** <https://doi.org/10.5565/rev/isogloss.437>
- **Primary Corpus Source:**
  [CEDEL2](https://cedel2.learnercorpora.com/) (Lozano, 2013)

------------------------------------------------------------------------

## Repository Structure

This project uses the OSF Component strategy. To replicate the results,
ensure all files are downloaded into a single working directory.

### Component: Data

- **datal2s.csv**: Subject realization data for 82 German L1 learners of
  Spanish across all proficiency levels.
- **datanatives.csv**: A random sample of 17 native Spanish speakers
  used as a comparative baseline.

### Component: Analysis

- **demographics.R**: Script for participant metadata and learner
  profile summaries.
- **descriptive.R**: Script for frequency counts.
- **mupdarfanalysis.R**: The core script implementing the MuPDARF
  deviation analysis.

------------------------------------------------------------------------

## Data Description

The datasets include sentence-level coding for subject realization in
the **"Chaplin" task** (silent film retelling).

### Inclusion Criteria

To isolate contexts where subject realization is optional/variable, the
following were excluded: \* Imperatives \* Indefinite "se" constructions
\* Impersonal sentences (e.g., hay, llueve)

### Key Variables Table

| Variable                 | Description             | Values                   |
|:-------------------------|:------------------------|:-------------------------|
| **Subject**              | Binary realization      | NS (Null), RS (Realized) |
| **SubjectType**          | Detailed classification | dp, nullsub, pron        |
| **SubjectPosition**      | Syntactic position      | PRE, POST                |
| **Thetarole**            | Semantic role           | agent, pacient           |
| **InformationStructure** | Discourse status        | GI (Given), NE (New)     |
| **Proficiency**          | Learner level           | LA to HC                 |

------------------------------------------------------------------------

## Reproducibility Instructions

### 1. Requirements

You will need **R** and the following libraries installed: \* partykit
\* dplyr \* ggplot2 \* stringr \* lattice

### 2. Setting the Environment

Because this repository does not include an `.Rproj` file, you must
manually set your working directory in R to the folder containing the
downloaded files:

`setwd("path/to/your/downloaded/files")`

### 3. Analysis Procedure: MuPDARF

The analysis follows the **MuPDARF** (Multifactorial Prediction and
Deviation Analysis using Random Forests) method: 1. **Baseline**: A
Random Forest is trained on `datanatives.csv` to establish native
speaker patterns. 2. **Prediction**: The model predicts the expected
native-like choices for the German L1 learners in `datal2s.csv`. 3.
**Deviation**: Analysis of the statistical deviation between actual
learner choices and model-predicted native choices.

------------------------------------------------------------------------

## Citation

If you use these scripts or data, please cite the original paper:

**Kocher, Anna (2025). The subject realization in L2 Spanish by German
L1 speakers: A corpus study. Isogloss. Open Journal of Romance
Linguistics, 11(2), 1–34.** <https://doi.org/10.5565/rev/isogloss.437>
And the corpus:

**Lozano, C. (2013). CEDEL2: Design, compilation and reach of a corpus
of L2 Spanish. Second Language Research, 29(1), 135-147.**
