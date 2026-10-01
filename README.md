# NeoSepPulmoExplorer

**An integrated web application for transcriptomics-based data mining and pathway-level heatmap visualization.**

**Web app: [https://pharmaco-omicslab.shinyapps.io/NeoSepPulmoExplorer/](https://pharmaco-omicslab.shinyapps.io/NeoSepPulmoExplorer/)**

---

## About

NeoSepPulmoExplorer is an R Shiny application for exploring lung transcriptomics results at the pathway and gene level. It integrates normalized expression data, differential expression analysis (DEA) results, and KEGG pathway enrichment results in an interactive interface for heatmap-based visualization.

The app supports pathway-based and user-defined gene selection, optional FDR filtering, sample group selection, plot customization, result history, and figure export. Users may work with the default study data or upload compatible processed datasets.

---

## Features

- **Pathway mode:** select one or two enriched KEGG pathways, from a single comparison or pooled across all comparisons. With two pathways, genes can be shown as one merged list or split into pathway-specific and shared genes.
- **Manual mode:** build a gene list by importing genes from a pathway, typing symbols with auto-completion, pasting a list, or uploading a `.txt` / `.csv` file. Genes that are not found in the data are reported.
- **Group and comparison selection:** choose the sample groups (heatmap columns) and the comparisons shown as FDR annotation columns.
- **Plot customization:** toggle legends, set a global font size, and adjust individual text elements.
- **Result history:** every heatmap generated in a session is kept for viewing and export.

---

## Software versions

The application was built with the following versions:

| Component          | Version | Source       | Role                                            |
| ------------------ | ------- | ------------ | ----------------------------------------------- |
| **R**        | 4.5.1   | CRAN         | Runtime                                         |
| `shiny`          | 1.12.1  | CRAN         | Web application framework                       |
| `shinythemes`    | 1.2.0   | CRAN         | Visual style of the interface                   |
| `shinyjs`        | 2.1.1   | CRAN         | Dynamic layouts and interactive frontend events |
| `shinyWidgets`   | 0.9.0   | CRAN         | Multi-select dropdowns and auto-completion      |
| `ComplexHeatmap` | 2.24.1  | Bioconductor | Heatmap rendering (Bioconductor 3.21)           |

Other dependencies: `circlize`, `DT`, `dplyr`, `tibble`, `magrittr`, `tidyr` (CRAN), and `BiocManager` for installing Bioconductor packages.

---

## Installation and usage

```r
install.packages(c("BiocManager", "shiny", "shinythemes", "shinyWidgets", "shinyjs",
                   "DT", "dplyr", "tibble", "magrittr", "tidyr", "circlize"))
BiocManager::install("ComplexHeatmap")
```

To reproduce the exact environment, install the versions listed above, for example with `remotes::install_version("shiny", version = "1.12.1")`.

Run the app from the repository folder:

```r
shiny::runApp()
```

The app includes public example data in the local `data/example/` folder. Users can also load their own files in **Step 1: Manage Data**. The **Download Example Data** button provides the same example files as a `.zip` archive.

The app has three steps:

1. **Manage Data (optional):** upload base data or replace the DEA / GSEA files of a comparison.
2. **Configure Analysis:** choose pathways or a gene list, then groups, comparisons and appearance.
3. **Generate Plot:** view the heatmap, switch between results, and export.

---

## Input data formats

All files are `.csv`. Gene identifiers must be NCBI Entrez Gene IDs.

| File                          | Required content                                                                                                                     |
| ----------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| Metadata                      | `RNAseq_ID` (sample ID), `SE_Control` (`SE` / `CON`), `Correct_Group` (`Low-CON`, `High-CON`, `Low-SE`, `High-SE`) |
| Normalized expression         | Genes x samples; first column holds Entrez IDs, header holds sample IDs                                                              |
| ID mapping                    | `EntrezID`, `Gene.symbol`                                                                                                        |
| DEA results (per comparison)  | First column holds Entrez IDs;`adj.P.Val` column (FDR). Include all tested genes.                                                  |
| GSEA results (per comparison) | clusterProfiler format:`Description`, `NES`, `core_enrichment` (Entrez IDs separated by `/`)                                 |
---
## Citation

If you use this work, please cite:

Nguyen Phuoc Long, Ole Bæk, Karoline Aasmul-Olsen, Richard Doughty, Bjorn Klabunde, Nguyen Quang Thu, Le Hoang Bach Dat, Bui Thanh Liem, Klaus Bønnelykke, and Duc Ninh Nguyen.  
**Reduced parenteral glucose supply in preterm neonatal infection ameliorates the pulmonary damage.**  
*bioRxiv*, 2026.06.05.730350, 2026.  
DOI: [10.64898/2026.06.05.730350](https://doi.org/10.64898/2026.06.05.730350)

```bibtex
@article{long2026reduced,
  title={Reduced parenteral glucose supply in preterm neonatal infection ameliorates the pulmonary damage},
  author={Long, Nguyen Phuoc and B{\ae}k, Ole and Aasmul-Olsen, Karoline and Doughty, Richard and Klabunde, Bjorn and Thu, Nguyen Quang and Dat, Le Hoang Bach and Liem, Bui Thanh and B{\o}nnelykke, Klaus and Nguyen, Duc Ninh},
  journal={bioRxiv},
  year={2026},
  doi={10.64898/2026.06.05.730350},
  url={https://doi.org/10.64898/2026.06.05.730350}
}
```
---

## License

This project is licensed under the MIT License.

---

## Affiliations

- [CGU - The Pharmaco-Omics Lab](https://pharmacoomics.com/), Graduate Institute of Biomedical Sciences, College of Medicine, Chang Gung University, Taoyuan 333, Taiwan; Molecular Medicine Research Center, Chang Gung University, Taoyuan 333, Taiwan.
- Cellular and Molecular Pediatrics Lab, University of Copenhagen, Copenhagen, Denmark.

---

## Contact

CGU - The Pharmaco-Omics Lab

Email: [pharmacoomicslab@gmail.com](mailto:pharmacoomicslab@gmail.com)

Nguyen Phuoc Long, MD, PhD (PI)

Dat Le (Developer)
