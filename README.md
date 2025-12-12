
# The role of receptor binding and immunity in SARS-CoV-2 fitness landscape: a modeling study

**DOI:** [https://doi.org/10.1101/2024.10.24.24316028](https://doi.org/10.1101/2024.10.24.24316028)

## Paper Authors

Zhaojun, Ding, Hsiang-Yu Yuan

## Research Aim

In this study, we aimed to assess the impact of RBD mutations on virus fitness that is associated with ACE2 receptor binding using data from Italy, a country with high viral incidence rates and high vaccine coverage. To disentangle intervention effects (i.e. vaccinations and non-pharmaceutical interventions (NPIs)) on virus fitness, we incorporated mobility and vaccination data in an infectious disease transmission model to calibrate fitness estimates. From simulated results, and for the first time, we constructed a fitness landscape after mapping the virus's evolutionary trajectory traits (i.e. receptor binding and immune escape) over different periods across different immunity levels.

## Data Description
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
  
•	All original code has been deposited at github at https://github.com/DING219/bind_escape_code and is publicly available as of the date of publication.  
•	Any additional information required to reanalyze the data reported in this paper is available from the lead contact upon request.  

## Source cCode in Each Folder

## Figure1:
**Figure1.pptx:Research Scheme**  

## Figure2:
### Data: Vaccination status in Italy 2021-2023
**somministrazioni-vaccini-summary-latest.csv**  

### Code: 
**Figure_2.R: Vaccination data, mobility index, log incidence data, time series plots**  
This script compiles and harmonizes Italian COVID-19 vaccination, mobility, and case data from 2020–2022. Vaccination records are cleaned, filtered, grouped by dose type, and aggregated into a complete daily time series. Mobility data are aligned to the same period, and case data are corrected, cumulatively summed, and log-transformed. Key VOC periods are annotated using fixed date cutoffs. The script generates publication-quality figures for vaccine coverage, mobility trends, and daily/cumulative incidence, and exports them as high-resolution TIFF files.


## Figure3:
### Data: Variants of Concern (VOCs)
**Alpha_Italy_bind_esc_socre1.csv**  
**Beta_Italy_bind_esc_socre1.csv**  
**Delta_Italy_bind_esc_socre1.csv未上传**  
**Gamma_Italy_bind_esc_socre1.csv**  
**Omicron_Italy_bind_esc_socre1.csv**  
### Data: 未定义
**Italy_alg.rds**  
**Non_voc_Italy_bind_esc_socre1.csv**  
**Omicron_accession_ID.zip**  
**RBD_deep_scan_data.csv**  
**imm_eascape.csv**  

### Code: 
**Figure_3_updated.R: binding/escape score, mutation rate, deep mutational scanning at RBG sites**  
This script compiles SARS-CoV-2 variant data to generate a multi-panel figure characterizing the temporal dynamics and functional properties of major VOCs and Omicron sublineages in Italy. Binding and immune-escape scores are imported for Alpha, Beta, Gamma, Delta, and Omicron, merged with lineage-specific accession IDs, filtered, and aggregated into weekly mean trajectories (Panels 2A–2B). A logistic model is fitted to quantify the association between binding affinity and immune escape (Panel 2C). Monthly sequence counts are used to reconstruct time-varying VOC prevalence (Panel 2D). Deep mutational scanning data are integrated to summarize binding and escape effects at key RBD sites (Panel 2E), and mutation-frequency heatmaps are generated for Alpha, Delta, BA.1*, and BA.2* (Panel 2F).


## Figure4:
### Data: 未定义，res?
**beta_v_res_updated.csv**  
**effective_immunity_res.csv**  
**population_immunity_res_updated.csv**  
**model_fit_res.csv**  

### Code: 
**Figure_4.R: fitness results, effective reproduction number, immunity levels**  
This script analyzes the temporal dynamics of COVID-19 transmission, viral fitness, and immunity in Italy. It uses daily reported cases and model fits to track transmission trends, assesses viral fitness through ACE2 binding and effective reproduction numbers (Re), and examines immunity levels within the population, both from natural immunity and vaccines. Additionally, it evaluates effective immunity against major VOCs over time, highlighting the impact of immunity on viral spread.


## Figure5:
### Data: 未定义
**count_variant.csv**

### Code: 
**Figure_5.R: linage, weekly mutation counts and subsets**
This script computes weekly prevalence of selected RBD mutations from SARS-CoV-2 sequences and visualizes their time trends, highlighting VOC phases and key transition periods, including grouped mutation sets (Set1–Set3) and key mutations such as E484K, L452R, and T478K.

## pre_data_used
### Data: 未定义
**ajusted_protection_Italy.csv**  
**ajusted_protection_Italy1.csv**  
**ajusted_vac_Italy.csv**  
**input.zip**  


## Notes

- Ensure all required R packages are installed before running the scripts.
- Adjust file paths in the scripts to match your local directory structure if necessary.

## Cite
@article{ding2025the,
  title={Impact of human mobility and weather conditions on Dengue mosquito abundance during the COVID-19 pandemic in Hong Kong},
  author={Ding, Zhaojun and Yuan, Hsiang-Yu},
  journal={medRxiv},
  year={2025},
}
