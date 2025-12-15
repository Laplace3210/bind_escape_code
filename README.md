
# The role of receptor binding and immunity in SARS-CoV-2 fitness landscape: a modeling study

**DOI:** [https://doi.org/10.1101/2024.10.24.24316028](https://doi.org/10.1101/2024.10.24.24316028)

## Authors ## 

Zhaojun Ding (Mian developer), Hsiang-Yu Yuan

## Research Aim

In this study, we aimed to assess the impact of RBD mutations on virus fitness that is associated with ACE2 receptor binding using data from Italy, a country with high viral incidence rates and high vaccine coverage. To disentangle intervention effects (i.e. vaccinations and non-pharmaceutical interventions (NPIs)) on virus fitness, we incorporated mobility and vaccination data in an infectious disease transmission model to calibrate fitness estimates. From simulated results, and for the first time, we constructed a fitness landscape after mapping the virus's evolutionary trajectory traits (i.e. receptor binding and immune escape) over different periods across different immunity levels.

## Data Used in This Study
### Daily reported COVID-19 cases in Italy
https://github.com/owid/covid-19-data/tree/master/public/data  
### Deep mutational scanning data for ACE2 binding
https://github.com/jbloomlab/SARS-CoV-2-RBD_DMS_Omicron/blob/main/results/final_variant_scores/  
### Deep mutational scanning data
https://jbloomlab.github.io/SARS2_RBD_Ab_escape_maps/  
### Italy COVID-19 vaccination data
https://github.com/italia/covid19-opendata-vaccini  
### Google mobility data
https://www.google.com/covid19/mobility/  
### Apple mobility data
https://covid19.apple.com/mobility  
  
•	All original code has been deposited at github at https://github.com/Laplace3210/bind_escape_code and is publicly available as of the date of publication.  
•	Any additional information required to reanalyze the data reported in this paper is available from the lead contact upon request.  

## Source Code in Each Folder

## Figure1:
**Study flow and model design** 
(A) Schematic showing the extended SEIR model integrating multiple data sources to estimate the SARS-CoV-2 fitness landscape. Data inputs include mobility, immune escape, and vaccination coverage data.
These data feed into the extended SEIR model, which was calibrated using reported case data. The model outputs include virus fitness combined with ACE2 binding data and effective immunity, and show how these
factors shape virus evolution patterns. (B) Detailed structure of the SEIR compartment model. The model partitions the population based on vaccination status and infection stage, highlighting transitions between
susceptible (S), exposed (E), infectious (I), reported infectious cases (IR) and recovered (R) states, with an emphasis on the impact of vaccination doses on these dynamics. S𝑖, E𝑖, I𝑖, IR𝑖 and R𝑖 subscripts denote the compartments with different vaccine doses.
This figure was producted in the MS Power Point. No code was used to generate it.

## Figure2:
**Figure2.R: Vaccination data, mobility index, log incidence data, time series plots**  
This script compiles and harmonizes Italian COVID-19 vaccination, mobility, and case data from 2020–2022. Vaccination records are cleaned, filtered, grouped by dose type, and aggregated into a complete daily time series. Mobility data are aligned to the same period, and case data are corrected, cumulatively summed, and log-transformed. Key VOC periods are annotated using fixed date cutoffs. The script generates publication-quality figures for vaccine coverage, mobility trends, and daily/cumulative incidence, and exports them as high-resolution TIFF files.

At the top of both `data_used/data_used.R` and `Figure2/Figure2.R`, add the following lines for path configuration:

```r
ROOT_DIR <- getwd()
DATA_DIR <- file.path(ROOT_DIR, "data", "input")
OUT_DIR  <- file.path(ROOT_DIR, "results", "figure2")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
```

Then, replace the filenames in the script used in:"
```r
read.csv(file.path(DATA_DIR, "xxx.csv"))
```

In `Figure2.R`, update the default file used in:
```r
source(file.path(ROOT_DIR, "scripts", "data_used.R"))
```

Finally, update the output path in `ggsave()`:

```r
ggsave(
  figure_2A_2B_2C,
  file = file.path(OUT_DIR, "figure_2A_2B_2C.pdf"),
  width = 8, height = 11.5
)
```
**Tips:**
- `Figure2.R` requires objects from `data_used.R`. When running `Figure2.R`, source `data_used.R` after configuring the paths.
- `Figure2/Figure2.R`: generate Figure 2 panels and export the final PDF.
- Workflow: `Figure2.R` depends on objects created in `data_used.R`.  
- Recommended: run only `Figure2.R` (it will source `data_used.R` after you configure paths).

**Notes:**

The vaccine data from Italy can be downloaded from the Commissario straordinario per l'emergenza Covid-19 (Italy) (link: https://github.com/italia/covid19-opendata-vaccini). The source data of the vaccine information were not provided here. 

## Figure3:
**Figure_3_updated.R: VOC prevalence, binding/escape score (calculated from deep mutational scanning data at RBG sites)**  
(A) Variant prevalence over time. The bar chart shows the prevalence of different SARS-CoV-2 VOC (alpha, beta, delta, gamma, and omicron strains) from early 2020 through early 2022. (B) Additive binding scores of viral lineages under different VOC backgrounds. The value in parentheses after each VOC denotes the mean ACE2 binding scores. (C) The immune escape scores over time. Scatter plot showing ACE2 binding and alpha, delta, and omicron variant immune escape across the timeline. BA.1* and BA.2* including BA.1, BA.2 and their sub-lineages. BA.1# means BA.1* without BA.1 and BA.1.1. BA.2# means BA.2* without BA.2..

Usage:
Place all required input files in the locations expected by the script (configure paths if needed).
Run: `Figure_3_updated.R`

Output:
The script generates the multi-panel Figure 3 (VOC prevalence + binding/escape score). 
Output files are written to the script’s configured output directory (see the path/output settings inside the script).

## Figure4:
This script plots the temporal dynamics of COVID-19 transmission, viral fitness, and immunity in Italy. It uses daily reported cases and model fits to track transmission trends, assesses viral fitness through ACE2 binding and effective reproduction numbers (Re), and shows immunity levels within the population, both from natural immunity and vaccines. Additionally, it shows effective immunity against major VOCs over time, highlighting the impact of immunity on viral spread.

Fastest way to reproduce Figure 4:

Open an R session at the repo root (recommended), e.g.
```r
setwd("path/to/bind_escape_code")
```

Run:
```r
source("Figure4/Figure_4.R")
```

Output: the Figure 4 panels (fitness results, effective reproduction number Re, and immunity levels) generated from the input `.csv` files in `Figure4/`. 

**Model fitting part:**
Re-run the model fitting pipeline (folder `Figure4/model_fit/`)
This is only needed if you want to refit the model instead of using the provided `.csv` results.

Usage:

Prepare the input objects (cases, mobility, vaccine, escape, protection, etc.).
In `model_fit_process.R`, the code currently loads them via a `source(".../data_uesd.R")` line. Edit this path to point to your local file location (ideally a relative path inside the repository). 

In `Figure4/model_fit/`, run:
```r
source("Figure4/model_fit/model_fit_process.R")
```

`SEIARD.R` defines the SEIARD-type transmission model with time-varying inputs (mobility (`M`), recovery rate (`gamma_t`), immune protection (`p_imm_1/2/3`), immune escape score (`imm_score`), and vaccine dose counts (`V1–V3`)). 

`model_fit_process.R` compiles `SEIARD.R` via `odin.dust::odin_dust("SEIARD.R")`, builds a particle filter (pMCMC) likelihood on cumulative cases, and fits parameters using particle MCMC (`mcstate::pmcmc`) with settings like `n_steps = 3000`, `burnin = 1000` for shorter sampling time. 

**Tips:**
- The pMCMC process is computationally intensive and places a heavy load on the CPU. Given the limited processing capacity of a personal computer, running ~3,000 samples across 5–10 concurrent RStudio sessions can yield more than 100,000 samples efficiently.

## Figure5:
This script computes weekly prevalence of selected RBD mutations from SARS-CoV-2 sequences and visualizes their time trends, highlighting VOC phases and key transition periods, including grouped mutation sets (Set1–Set3) and key mutations, such as N501Y, L452R, and T478K.

**Notes:**

Ensure all required R packages are installed before running the scripts.
Adjust file paths in the scripts to match your local directory structure if necessary.

