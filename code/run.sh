#!/bin/bash
cd code
Rscript -e "rmarkdown::render(input = 'Dose_Response_Script.Rmd', output_dir='../results/Dose_Response_Script_Output/', clean = TRUE)"
Rscript -e "rmarkdown::render(input = 'RNA_Seq_Script.Rmd', output_dir = '../results/RNA_Seq_Script_Output/', clean = TRUE)"
Rscript -e "rmarkdown::render(input = 'Resistance_and_Sensitivity_Genes_Script.Rmd', output_dir = '../results/Resistance_and_Sensitivity_Genes_Script_Output/', clean = TRUE)"
Rscript -e "rmarkdown::render(input = 'Figure_6c_Script.Rmd', output_dir='../results/Fig7b_Script_Output/', clean = TRUE)"