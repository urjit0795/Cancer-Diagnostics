# Cancer-Diagnostics
(Decision tree in R) One very interesting application area of machine learning is in making medical diagnoses. In this problem you will train and test a decision tree to detect breast cancer using real world data. We will use the Wisconsin Diagnostic Breast Cancer (WDBC) dataset. The dataset consists of 569 samples of biopsied tissue. The tissue for each sample is imaged and 10 characteristics of the nuclei of cells presenting each image are characterized. 

These characteristics are
(1) Radius
(2) Texture
(3) Perimeter
(4) Area
(5) Smoothness
(6) Compactness
(7) Concavity
(8) Number of concave portions of contour
(9) Symmetry
(10) Fractal dimension

Each of the 569 samples used in the dataset consists of a feature vector of length 30. The first 10 entries in this feature vector are the mean of the characteristics listed above for each image. The second 10 are the standard deviation and last 10 are the largest value of each of these characteristics present in each image. Each sample is also associated with a label. A label of value 1 indicates the sample was for malignant (cancerous) tissue. A label of value 0 indicates the sample was for benign tissue. This dataset has already been broken up into training and test sets for you and is available on blackboard. The names of the files are “trainX.csv”, “trainY.csv”, “testX.csv” and “testY.csv.” The file names ending in “X.csv” contain feature vectors and those ending in “Y.csv” contain labels. Each file is in comma separated value format where each row represents a sample.

(a) Clean your data (if required) before running any model. Create a C&R decision tree to its full depth. How many leaves are in tree? How do you get this information?

(b) What are the major predictors of Diagnosis? Please justify your reasoning.

(c) Give two strong rules that describe who is likely to have cancer. Please justify your choices.

(d) What is the accuracy of your decision tree model on the training data? What is the accuracy of this model on the test data?

(e) Is it possible to improve the performance of your model?

(f) Construct the best possible decision tree to predict the Y labels. Explain how you construct such tree.

(g) Plot your final decision tree model.
