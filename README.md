A convenient R package for calculating PIFI from the paper  
[Multidimensional Pancreatic Islet β-cell Function Assessment Improves Predictive Effect of Diabetes Risk Scores](https://doi.org/10.1210/clinem/dgaf372)  
  
[Web-based calculator](https://multipif.com.cn/) (Simplified Chinese)

Column Name Annotations  
INS, Insulin, unit: pmol/L  
CP, C-peptide, unit: pmol/L  
GLU, Blood glucose, unit: mmol/L  
A, 0 min; B, 30 min; C, 60 min; D, 120 min; E, 180 min  

Height, unit: cm  
Age, unit: years  
Sex, Male = 1, Female = 2  

See [example_data.xlsx](https://github.com/wjcshine/PIFI/blob/main/example_data.xlsx) for an example of the data format.

The output contains PIF-i and/or PIF-c. Detailed information regarding each indicator can be found at https://doi.org/10.1210/clinem/dgaf372.

Author: [Jiachen Wang](https://orcid.org/0009-0003-8232-8500)  
Email：wjcshine@foxmail.com  

Collaborators:  ProfessTao Yang, Qi Fu, Hao Dai  

Special thanks to Grace Xu for valuable suggestions


<b>Example Data</b>
| ID | Age | Sex | Height | GLU_A | GLU_B | GLU_C | GLU_D | GLU_E | INS_A | INS_B | INS_C | INS_D | INS_E | CP_A | CP_B | CP_C | CP_D | CP_E |
|----|-----|-----|--------|-------|-------|-------|-------|-------|-------|-------|-------|-------|-------|------|------|------|------|------|
| 1 | 54 | 1 | 178 | 5.79 | 11.06 | 14.65 | 19.08 | 18.07 | 40.5 | 46.2 | 90.14 | 126 | 116.1 | 665.5 | 592 | 801.6 | 1312 | 1537 |
| 2 | 80 | 1 | 171 | 6.63 | 13.83 | 18.33 | 22.43 | 17.42 | 9.9 | 90.64 | 149.6 | 343.7 | 231.1 | 305.7 | 661.8 | 972.9 | 2657 | 2681 |
| 3 | 71 | 1 | 170 | 5.4 | 15.76 | 19.38 | 19.78 | 18.9 | 9.8 | 70.17 | 82.5 | 67.28 | 47.68 | 399.3 | 925.7 | 1105 | 1261 | 1307 |  

| Method (for reference only) | Age | Sex | Height | GLU_A | GLU_B | GLU_C | GLU_D | GLU_E | INS_A | INS_B | INS_C | INS_D | INS_E | CP_A | CP_B | CP_C | CP_D | CP_E |
|-----------------------------------------|-----|-----|--------|-------|-------|-------|-------|-------|-------|-------|-------|-------|-------|------|------|------|------|------|
| 5 POINT METHOD | √ | √ | √ | √ | √ | √ | √ | √ | √ | √ | √ | √ | √ | √ | √ | √ | √ | √ |
| 5 POINT METHOD | √ | √ | √ | √ | √ | √ | √ | √ | √ | √ | √ | √ | √ | | | | | |
| 5 POINT METHOD | √ | √ | √ | √ | √ | √ | √ | √ | | | | | | √ | √ | √ | √ | √ |
| 3 POINT METHOD A | √ | √ | √ | √ | √ | | √ | | √ | √ | | √ | | √ | √ | | √ | |
| 3 POINT METHOD A | √ | √ | √ | √ | √ | √ | √ | √ | √ | √ | | √ | | | | | | |
| 3 POINT METHOD A | √ | √ | √ | √ | | √ | √ | | √ | √ | √ | √ | √ | √ | √ | √ | √ | √ |
| 3 POINT METHOD B | √ | √ | √ | √ | | √ | √ | | √ | | √ | √ | | √ | | √ | √ | |
| 3 POINT METHOD B | √ | √ | √ | √ | | √ | √ | | √ | √ | √ | √ | √ | | | | | |
| 3 POINT METHOD B | √ | √ | √ | √ | | √ | √ | | | | | | | √ | | √ | √ | |
