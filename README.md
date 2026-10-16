# Urbanphony-Venice-Convolutional_Neural_Network
This code repository contains R and Python scripts that process data and produce results from provided intermediate products for urban soundscape composition assessment. This work was funded under the Urbanphony, Minister of Science of Colombia’s Strengthening of Science and Technology in Higher Education Institutions Call 2021-890 and is citable using the Zenodo DOI. Note: Please be aware that underlying software, specifically for the CNN implementation, may not continue stability as Python libraries are regularly updated.

## This repository can:
1. Generate all code-based figures, tables, and numerical results in the published manuscript.
2. Be used to re-train the U-VNE-CNN
3. Be applied to novel acoustic data by generating 2-s Mel spectrograms, CNN inference probabilities, f-score threshold optimization, and classification based on CNN findings related to Urbanphony data

## List of scripts used to derive manuscript figures, tables, and results sections (when applicable):
- Fig 1: NA - conceptual figure, no code used
- Fig 2: NA - conceptual figure, no code used
- Fig 3: NA - conceptual figure, no code used
- Fig 4: NA - conceptual figure, no code used
- Fig 5: NA - conceptual figure, no code used
- Fig 6: NA - conceptual figure, no code used
- Fig 7: U_VNE_CNN/1_mels_generation-py-R/code/1_split_labels_pb.ipynb
- Fig 8: U_VNE_CNN/1_mels_generation-py-R/code/4_mels_gen_aug_pb.ipynb
- Fig 9: NA - conceptual figure, no code used
- Fig 10: NA - conceptual figure, no code used
- Fig 11: U_VNE_CNN/4_cnn_threshold_optimization_and_accuracy-R/code/0_CNN_threshold_optimization.R
- Fig 12: U_VNE_CNN/5_cnn_deployment_py_R/code/2_cnn_maps.R (for map TIFs)
- Fig 13: U_VNE_CNN/5_cnn_deployment_py_R/code/2_cnn_maps.R (for map TIFs)
- Fig 14: U_VNE_CNN/5_cnn_deployment_py_R/code/2_cnn_maps.R (for map TIFs)
- Fig 15: NA - conceptual figure, no code used
---
- Table 1: Not related to code, the Phonic Interaction Indices
- Table 2: Not related to code, the urban morphological indicators
- Table 3: U_VNE_CNN/4_cnn_threshold_optimization_and_accuracy-R/code/1_CNN_accuracy_numbers.R
- Table 4: U_VNE_CNN/6_stats_R/code/1_sound_acoustic.R
- Table 5: U_VNE_CNN/6_stats_R/code/2_sound_morphology.R
- Table 6: U_VNE_CNN/6_stats_R/code/3_mul_reg.R

## Results sections:
- 3.1 Model performance: most metrics in U_VNE_CNN/4_cnn_threshold_optimization_and_accuracy-R/code/1_CNN_accuracy_numbers.R; rate of unidentified sound from - U_VNE_CNN/5_cnn_deployment_py_R/code/2_cnn_maps.R
- 3.3 Dominance of soundscape components and the acoustic phenomenon: U_VNE_CNN/6_stats_R/code/1_sound_acoustic.R
- 3.4: Relationship between dominance of soundscape components and urban morphology: U_VNE_CNN/6_stats_R/code/2_sound_morphology.R
- 3.5: Dominance of soundscape components regression model: U_VNE_CNN/6_stats_R/code/3_mul_reg.R

If you have any questions or difficulty implementing the above methods, contact the author at juiian.grijaba@rai.usc.es

### Urbanphony. A project boosted from the hearth of Mantra, Science, Art and Technology.
