# modulation_emotionalBrain
NOTE  
Some codes were adapted from others. Clear copyright and reference was stated in the relevant scripts.  

Step 1 Matrix decomposition of brain activations (Identification of functional networks within the emotion processing system)
--------------
* The package for NMF is available at https://github.com/smatmo/l0-sparse-NMF.
* The package used to compute the mean correlation between corresponding factor pairs is available at https://github.com/ondrejdee/hungarian

### Running [NMF_K2_pos.m](https://github.com/hanluyt/modulation_emotionalBrain/blob/main/NMF_K2_pos.m) for decomposition of angry>neutral activations (in the main text)
In order to determine the sparsity parameter (λ=L/m, L is the maximal number of non-zeros voxels in each factor, λ from 0.1 to 0.9) and the optimal number of factors (K from 2 to 9), we tested both the reconstruction error and the reproducibility of the obtained decompositions (K=2 in this script).

### Running [NMF_K3_neg.m](https://github.com/hanluyt/modulation_emotionalBrain/blob/main/NMF_K3_neg.m) for decomposition of neutral>angry activations (see supplement)
K=3 in this script

Step 2 Characterization analysis
-------------
Association analysis 1 and 2 in [cross_sectional.R](https://github.com/hanluyt/modulation_emotionalBrain/blob/main/cross_sectional.R) 
* Associations between the emotional symptoms at age 19 years and both childhood abuse and PRS<sub>MDD</sub>. 
* Associations between the weights of the latent factors at age 19 and various symptoms in the SDQ at age 19.
* Sex difference
* Developmental trajectory analysis in [repeated_anova.R](https://github.com/hanluyt/modulation_emotionalBrain/blob/main/repeated_anova.R) 

Step 3 Moderation analysis (genetic moderations of the brain networks' roles in resilience)
-----------
Moderation analysis in [cross_sectional.R](https://github.com/hanluyt/modulation_emotionalBrain/blob/main/cross_sectional.R) 
* Tested whether PRS<sub>MDD</sub> modulated the association between the weights of the latent factors and adolescent emotional symptoms following childhood abuse by a three-way interaction term.

Step 4 Sensitivity analyses
-------------
[sensitivity_analysis.R](https://github.com/hanluyt/modulation_emotionalBrain/blob/main/sensitivity_analysis.R) 
* Tested whether the modulation effects remained significant when the childhood abuse score was binarized by the cut-offs. 
* Tested the potential confounding effects of age, IQ and substance use in the modulation model. 
* Reran the models while 1) replacing the PRS<sub>MDD</sub> with the PRS<sub>ADHD</sub> or the PRS<sub>SCZ</sub>; 2) replacing the emotional symptom score with the conduct symptom score or hyperactivity/inattention symptom score. 

Step 5 Prediction models for emotional disorders 
-----------
* The predictability of the latent factors to emotional disorders was tested by machine learning models.  

[Prediction_model.ipynb](https://github.com/hanluyt/modulation_emotionalBrain/blob/main/Prediction_model.ipynb)







