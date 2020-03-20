# AutoRL-Trained Model

A trained model for automatic record linkage. Using this open-source code, you can apply our model on your data.

1. Reformat your data (like our demo files) and copy them in the data folder:
	- pairs.csv 
	- ffreq.csv (first name frequencies)
	- lfreq.csv (last name frequencies)
2. Run model1_for_uab.R 
3. The model will classify your pairs in to 3 groups: matched, unmatched, uncertain. The uncertain pairs need manual record linkage.
4. You can find the result in the result folder:
	- match.csv
	- unmatch.csv
	- review_pairs.csv
5. For manual record linkage, you can use our software (MINDFIRL): https://github.com/pinformatics/mindfirl

