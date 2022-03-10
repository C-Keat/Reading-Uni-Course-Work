# -*- coding: utf-8 -*-
"""Machine Learning and AI.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1nR--A1lLlP-WPbFyNshWnlDktUygrmNv

#AI and Machine learning
#Module code: CSMAI21
#Callum Keating
#Completion date: 

#Title: Classification of Stars, Galaxies and Quasars (Multi-class classification)

##Background and problem to be addressed: 

In astronomy, stellar classification is the classification of stars based on their spectral characteristics. The classification scheme of galaxies, quasars, and stars is one of the most fundamental in astronomy. The early cataloguing of stars and their distribution in the sky has led to the understanding that they make up our own galaxy and, following the distinction that Andromeda was a separate galaxy to our own, numerous galaxies began to be surveyed as more powerful telescopes were built. This dataset aims to classification stars, galaxies, and quasars based on their spectral characteristics. 

This is a multi-class classification problem.

#The Data

The dataset being used: 
Stellar classification Dataset - https://www.kaggle.com/fedesoriano/stellar-classification-dataset-sdss17


The data consists of 100,000 observations of space taken by the SDSS (Sloan Digital Sky Survey). Every observation is described by 17 feature columns and 1 class column which identifies it to be either a star, galaxy or quasar.



1.   obj_ID = Object Identifier, the unique value that identifies the object in the image catalog used by the CAS
2.   alpha = Right Ascension angle (at J2000 epoch)
3.   delta = Declination angle (at J2000 epoch)
4.   u = Ultraviolet filter in the photometric system
5. g = Green filter in the photometric system
6. r = Red filter in the photometric system
7. i = Near Infrared filter in the photometric system
8. z = Infrared filter in the photometric system
9. run_ID = Run Number used to identify the specific scan
10. rereun_ID = Rerun Number to specify how the image was processed
11. cam_col = Camera column to identify the scanline within the run
12. field_ID = Field number to identify each field
13. spec_obj_ID = Unique ID used for optical spectroscopic objects (this means that 2 different observations with the same spec_obj_ID must share the output class)

14. class = object class (galaxy, star or quasar object)
15. redshift = redshift value based on the increase in wavelength
16. plate = plate ID, identifies each plate in SDSS
17. MJD = Modified Julian Date, used to indicate when a given piece of SDSS data was taken
18. fiber_ID = fiber ID that identifies the fiber that pointed the light at the focal plane in each observation

##Extracting data

Here we load the data and explore the data further understand it, as well as confirming that the data set is clean.
"""

#The decision tree necessary libraries
from sklearn.metrics import accuracy_score,confusion_matrix
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.tree import plot_tree

#other sklearn for classication data vis
from sklearn import metrics
from sklearn.metrics import confusion_matrix
from sklearn.metrics import plot_confusion_matrix
from sklearn.model_selection import cross_val_score
from sklearn.metrics import classification_report
from sklearn.model_selection import cross_val_predict

#libraries needed for Multilayer Perceptrons
from keras.models import Sequential
from keras.layers.core import Dense, Dropout, Activation
from tensorflow.keras.optimizers import SGD

#allows dataset to be oversampled for rebalancing
from imblearn.over_sampling import SMOTENC
from imblearn.over_sampling import SMOTE

#Needed for K-fold cross-validation
from sklearn.model_selection import KFold

#Needed for K-nearest neighbours
from sklearn.neighbors import KNeighborsClassifier


#Importing libaries
import matplotlib.pyplot as plt
from matplotlib import cm
import seaborn as sns
import pandas as pd
import numpy as np
from sklearn import preprocessing

#Allows for epoch callbacks for training neural network to help prevent overfitting
from keras import callbacks

#Storing the orginal data as a data frame
orginData_df = pd.read_csv("/content/drive/MyDrive/Education/Universitys/Reading University/Masters/Course/S2/Artificial Intelligence and Machine Learning (2021 22)/Dataset/star_classification.csv")

#Taking a look to make sure that the dataframe has formed correctly.
orginData_df.head()

"""##EDA 

In this section EDA is performed in order to identify the right areas to preprocess as well as to greater understand the dataset.
"""

#The shape of the dataframe
orginData_df.shape

orginData_df.info()

cm = sns.light_palette('orange', as_cmap=True)

#Helps define gain inside of attributes
orginData_df.describe().T.style.background_gradient(cmap = cm)

#describe the class attribute
orginData_df.describe(include = object)

#identify the only values inside of the class attribute
orginData_df['class'].unique()

orginData_df['class'].value_counts()

#show the count of the unique objects in class
sns.countplot(orginData_df['class'],label = 'Count')
plt.show()

"""Above there is a display of a mild inbalance inside of the dataset, this will be addressed in the pre-processing stage as the GALAXY class represents nearly 60% of the total data."""

#corr() used to help define the correlation of differnt attrubutes of the dataset
orginData_df.corr().style.background_gradient(cmap = cm)

"""##Data preprocessing

In this section cleaning the data takes place, for this data set this includes:

*   Checking for missing values
*   Extracting only the useful attributes
*   Dropping duplicates
*   Reseting the index for the dataset to be worked on

This dataset came from Kaggle and does not specify if the dataset is clean or not, thus these measures represent a good standard practice to, at the very least, verify that this dataset is clean.

A correlation is determined to help with feature selection. 
"""

#Dropping any duplicates that exist in the dataset
droppedDuplicateData = orginData_df.drop_duplicates()
droppedDuplicateData.describe()
#there are no dropped duplicates, thus the data is full of uniques

#selecting the colums for analysis
selected_columns = droppedDuplicateData

#removing columns that have high STD, these attributes also represent the IDs of the features and wont play a part in classification
selected_columns = selected_columns.drop(columns = ['obj_ID'])
selected_columns = selected_columns.drop(columns = ['spec_obj_ID'])

#Dropping rerun_ID as this too does not display data that will be used inside of classification
selected_columns = selected_columns.drop(columns = ['rerun_ID'])

#turning the object 'class' to a int 
le = preprocessing.LabelEncoder()
le.fit(selected_columns['class'])
selected_columns['class'] = le.transform(selected_columns['class'])

#final working data frame
final_df = selected_columns

#again looking at the correlation between remaining attributes
final_df.corr().style.background_gradient(cmap = cm)

"""#Model training and evaluation

Below will be where each of the classification algorithms selected will be defined, there will also be light justification at the start of each to discuss why the specific classification algorithm has been selected. 


The algorithms selected are:
*   Neural networks (Multilayer Perceptrons)
*   Decision trees
*   K-nearest neighbours

##Neural network (Multilayer Perceptrons)

A simple neural network includes an input layer, an output (or target) layer and, in between, a hidden layer. The layers are connected via nodes, and these connections form a “network” – the neural network – of interconnected nodes. A node is patterned after a neuron in a human brain.

* Summary of neural network process: 
* First data is split into training, testing and validation data.
* Pre-processing is performed to normalise/scale the data to help with speeding up learning and leading to faster convergence inside of the network.
* Defining and training the model.
* Evaluating the model and evaluating the training of the Multilayer perceptron.

Justification: 

Multilayer perceptron’s (MLP) are suitable for classification prediction problems where inputs area assigned a class or label. Data is often provided in a tabular format, such as a CSV file or a spreadsheet.

###Splitting the data

In this section we split the data into a 80% training data, 20% testing data. We then proceed to split the testing data of 20% into a 50-50 split, of testing and validation. This divides the full dataset into a 80% training, 10% testing and 10% validation. 

This amount of split has been selected due to the size of the dataset.
"""

nn_train_dataset, temp_test_dataset = train_test_split(final_df,test_size=0.2)
#Split the test_dataset to 50% test and 50% validation. This will devide the full dataset into a 80% train, 10% validate, and 10% test.
nn_test_dataset, nn_valid_dataset = train_test_split(temp_test_dataset, test_size=0.5)

#here shows the imbalance in the datasets
np.bincount(nn_train_dataset['class'])

"""Here we display the size and shape of each of the datasets."""

print(f"Display the datatype of the test_dataset: {type(nn_test_dataset)}")
print(f" Trai dataset       : {nn_train_dataset.shape}")
print(f" Test dataset       : {nn_test_dataset.shape}")
print(f" Validation dataset : {nn_valid_dataset.shape}")

nn_train_dataset

"""Here we remove the class column from the datasets, in order to use them as the lables for the neural network."""

nn_train_labels1 = nn_train_dataset.pop('class')
nn_test_labels1 = nn_test_dataset.pop('class')
nn_valid_labels1 = nn_valid_dataset.pop('class')

nn_train_labels1

"""Here we will change the representation of our labels, so that our labels are numbered 1 - 3. We do this by one hot encoding. One hot encoding makes the training data more useful and expressive, and it can be rescaled easily. By using numeric values, we more easily determine a probability for our values. In particular, one hot encoding is used for our output values, since it provides more nuanced predictions that single labels.

https://www.educative.io/blog/one-hot-encoding
"""

nn_train_labels = pd.get_dummies(nn_train_labels1,prefix='Label')
nn_test_labels = pd.get_dummies(nn_test_labels1,prefix='Label')
nn_valid_labels = pd.get_dummies(nn_valid_labels1,prefix='Label')

nn_train_labels

"""###Preprocessing and normalisation

Here we will normalise/scale the data.

The goal of normalization is to change the values of numeric columns in the dataset to use a common scale, without distorting differences in the ranges of values or losing information, normalization is also required for some algorithms to model the data correctly. 

Neural Networks are one of these algorithms, due to weights of the model being initialized to small random values and updated via an optimization algorithm in response to estimate of error on the training dataset. Given this use of small weights in the model and the use of error between predictions and expected values, the scale of inputs and outputs used to train the model are an important factor. Unscaled input variables can result in a slow or unstable learning process.

Viewing the stats of the dataset to decide on normalising the data.
"""

nn_train_stats = nn_train_dataset.describe()
nn_train_stats = nn_train_stats.transpose()
nn_train_stats

#defining a function to normalise datasets.
def norm(x,stats):
  return (x - stats['mean'])/ stats['std']

nn_normed_train_data = norm(nn_train_dataset,nn_train_stats)
nn_normed_test_data = norm(nn_test_dataset,nn_train_stats)
nn_normed_valid_dataset = norm(nn_valid_dataset,nn_train_stats)

#show a sample of the normed data
nn_normed_train_data.head()

"""###Creation of the model

In this section the model is created and the validation data is used to provide an unbiased evaluation of a models fit on the training dataset while tuning the models hyperparameters. Validation datasets can be used by early stopping, which has also been implemented inside of this model. 

Early stopping helps us define the number of epochs for training, this can help stop the model becoming overfitted or underfitted based on the number of epochs. In this case an arbitrary “high” amount of epochs have been used, allowing early stopping to perform its function. 

A learning rate of 0.01 has been selected via looking at the learning charts below, looking for a decreasing loss rate, to help decide on a learning rate. There is the ability to build a process to find the optimal learning rate, however this dataset would'nt benift massivly from this as accuracy + f1 score is high without the addition of more computational cost.

"""

#The neural network framework
nn_model = Sequential()
#Dense(64) is a fully-connected layer with 64 hidden units.
#in the first layer we specify the expected input data shape:
#here, 14-dimentional vectors.
nn_model.add(Dense(64, input_dim = 14)) #input layer
nn_model.add(Dropout(0.5))
nn_model.add(Dense(64,activation='relu'))
nn_model.add(Dropout(0.5))
nn_model.add(Dense(3,activation='softmax')) #output layer 3 defined as there are 3 output classes for this model

#using the optimiser
sgd = SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)

#Model compiling
nn_model.compile(loss='categorical_crossentropy', optimizer=sgd,metrics=['accuracy'])

#Fit the Nueral Network and Initialize earlystopping
EPOCH = 500
batch = 800

#defining the early stopping with a patience of 5
earlystopping = callbacks.EarlyStopping(monitor= "val_loss", mode = "min", patience = 5, restore_best_weights = True)

#stores the epochs trained
#fitting the model using early stopping to help prevent over fitting
history = nn_model.fit(nn_normed_train_data,
             nn_train_labels, epochs = EPOCH, 
             batch_size = batch, 
             steps_per_epoch= int(nn_normed_train_data.shape[0] / batch),
             validation_data = (nn_normed_valid_dataset, nn_valid_labels),
             callbacks = [earlystopping])

"""###See how the training went

Below shows some results of the actual training process, showing the cross-validation data and the training accuracy and loss. 

Both charts show improvement overtime, with the accuracy going up overtime and the loss decreasing overtime. Again early stopping is being used here to help combat overfitting to the training dataset.

The model as a whole seems to be training well overtime. 

"""

#View the model accuracy via a plot
plt.plot(history.history['accuracy'])
plt.plot(history.history['val_accuracy'])
plt.title('model accuracy')
plt.ylabel('accuracy')
plt.xlabel('epoch')
plt.legend(['Train','Cross-Validation'])
plt.show()

#Plot showing the model loss
plt.plot(history.history['loss'])
plt.plot(history.history['val_loss'])
plt.title('Model Loss')
plt.ylabel('Loss')
plt.xlabel('epoch')
plt.legend(['Train', 'Cross-Validation'], loc='upper left')
plt.show()

"""###The confustion matrix

Below represents the results of the fitted model on the test data, later we will discuss the results in greater detail.
"""

predict_results = nn_model.predict(nn_normed_test_data)

predict_results

rounded_predictions = predict_results.argmax(axis = 1)

#Plot confusion matrix
ax= plt.subplot()

cm = confusion_matrix(nn_test_labels1, rounded_predictions)

sns.heatmap(cm, annot=True, fmt = 'd',cmap = 'Blues', ax = ax); #annot=True to annotate cells


#labels, title
ax.set_xlabel('Predicted labels');ax.set_ylabel('True labels'); 
ax.set_title('Confusion Matrix');

print("report:\n",classification_report(nn_test_labels1,rounded_predictions))

"""### Neural Network summary and learnings:

Shown above is the final results for the test data displayed as a confusion matrix and classification report to summarise the effectiveness of the decision tree model created.

Most importantly we see an overall accuracy score of 96% with across the board high f1-scores, f1 scores represent a balance between the precision and recall and is a good representative of the performance of the model. 

Though neural networks are not inherently prone to being overfitted, measures have been taken to reduce the risk, this has been done using early stopping and validation data. Overall this model represents a good fit and should be good to perform analysis in the future. 

Further detail and comparison will take place in the conclusion at the end of this report.

##Decision Tree

Decision tree analysis  typically uses a hierarchy of variables or decision nodes that, when answered step by step.

Summary of decision tree process:
* First data is split into training and testing
* The creation of a preliminary tree is done to set a benchmark
* Cross validation is used for both tree depth and alpha pruning value to help combat the issue of overfitting that can be present inside of decision trees
* Building and evaluating the final decision tree, this is done after attributes of the tree (pruning and depth) have been discovered


Justification: 

Decision trees are useful when evaluating lists of distinct features, qualities or characteristics of people, places, or things. This fits well with the aim of this project, to identify celestial bodies from each other as each one has specific characteristics.

###Splitting the data for training and testing
"""

def split_data(df,target_column):
  X = df.drop('class',axis = 1).copy()
  y = df['class'].copy()  
  return X,y

X,y = split_data(final_df,'class')

X_train, X_valid, y_train, y_valid = train_test_split(X,y,test_size=0.33,random_state=42)
X_train.shape, X_valid.shape, y_train.shape, y_valid.shape

"""###Creating a preliminary tree

Here we create a basic tree in order to start the process of classication. A base is created via the confusion matrix, so that it can be used as the benchmark to be approved apone.
"""

#Creating a simple decision tree
model_tree = DecisionTreeClassifier(max_leaf_nodes=50, class_weight='balanced')
model_tree.fit(X_train, y_train)

#Plotting the decition tree

def plot_decition_tree(tree):
  plt.figure(figsize=(20,10))

  #Create the tree plot
  plot_tree(tree, class_names = ['Star','Quasar','Galaxy'], rounded = True, filled = True)

  plt.show()

plot_decition_tree(model_tree)

#show the count of the unique objects in class
y_valid.value_counts()

"""Here we use the test data to see how well the tree does with classification, and it also shows the number of wrongly classified characters."""

#Plot a confusion_matrix will run the test data down the tree and draw a confusion matrix
plot_confusion_matrix(model_tree, X_valid, y_valid,display_labels = ["Is a Galaxy","Is a Quasar","Is a Star"])

"""###Cost complexity pruning: Cross Validation for finding the best alpha

Decision Trees are at risk for being overfit in the training Dataset, and there are a lot of parameters, like max_depth and min_samples, that are designed to reduce overfitting. However, pruning a tree with cost complexity pruning can simplify the whole process of finding a smaller tree that improves the accuracy with the testing dataset. Pruning a decision tree is all about finding the right value for the pruning parameter, alpha, which controls how little or how much pruning happens.

Below a extracts the possible alpha values from the inital tree. Then for each alpha candiate value 10-fold cross validation is run in order to find the accuracy of the "prunned" tree, this can then be plotted to help indicate which alpha value represents the most accurate classifcation prediction.
"""

#Extracting the alpha values:
path = model_tree.cost_complexity_pruning_path(X_train, y_train) #determine values for alpha
ccp_alphas = path.ccp_alphas #Extract different values for alpha
ccp_alphas = ccp_alphas[:-1] #Exclude the maximum value for alpha

#Createing an array to store the results of each fold during cross validiation
alpha_loop_values = []

#For each candiate value for alpha, we will run 10-fold cross validation.
#Then the mean and standard devation will be stored for each call, this represents the accuracy
for ccp_alpha in ccp_alphas:
  model_tree = DecisionTreeClassifier(random_state=23, ccp_alpha=ccp_alpha)
  scores = cross_val_score(model_tree,X_train,y_train,cv=10)
  alpha_loop_values.append([ccp_alpha, np.mean(scores), np.std(scores)])

#Now we draw a graph of the means and standard deviation of the scores
# for each candidate value for alpha
alpha_results = pd.DataFrame(alpha_loop_values, columns=['alpha','mean_accuracy','std'])

alpha_results.plot(x='alpha', y='mean_accuracy', yerr='std', marker = 'o', linestyle = '--')

"""Due to the chart being hard to read, another method is to be used to find the ideal pruning alpha value.

As we can see the best alpha value is between 0 and 0.001, so we will search though the results for the best one.
"""

ideal_ccp_alpha = alpha_results[(alpha_results['alpha'] > 0)& (alpha_results['alpha'] < 0.001)]
ideal_ccp_alpha

#using the table above we will extract the best alpha value that being 0.000344
ideal_cpp_alpha_value = 0.000104

"""Now armed with the best alpha pruning value, we can attepmt to find the best tree depth, also using cross validation, even though the alpha pruning is normally enough to create an accurate decision tree, the extra levels taken to find the best tree depth further helps in the pursuit for overall accuracy and to help combat overfitting the training data.

###Cross validation for tree depth

Here we will use cross validation to identify the best depth for the decision tree to operate at.
"""

# function for fitting trees of various depths on the training data using cross-validation
def run_cross_validation_on_trees(X, y, tree_depths, cv=10, scoring='accuracy'):
    cv_scores_list = []
    cv_scores_std = []
    cv_scores_mean = []
    accuracy_scores = []
    for depth in tree_depths:
        tree_model = DecisionTreeClassifier(max_depth=depth)
        cv_scores = cross_val_score(tree_model, X, y, cv=cv, scoring=scoring)
        cv_scores_list.append(cv_scores)
        cv_scores_mean.append(cv_scores.mean())
        cv_scores_std.append(cv_scores.std())
        accuracy_scores.append(tree_model.fit(X, y).score(X, y))
    cv_scores_mean = np.array(cv_scores_mean)
    cv_scores_std = np.array(cv_scores_std)
    accuracy_scores = np.array(accuracy_scores)
    return cv_scores_mean, cv_scores_std, accuracy_scores

# function for plotting cross-validation results
def plot_cross_validation_on_trees(depths, cv_scores_mean, cv_scores_std, accuracy_scores, title):
    fig, ax = plt.subplots(1,1, figsize=(15,5))
    ax.plot(depths, cv_scores_mean, '-o', label='mean cross-validation accuracy', alpha=0.9)
    ax.fill_between(depths, cv_scores_mean-2*cv_scores_std, cv_scores_mean+2*cv_scores_std, alpha=0.2)
    ylim = plt.ylim()
    ax.plot(depths, accuracy_scores, '-*', label='train accuracy', alpha=0.9)
    ax.set_title(title, fontsize=16)
    ax.set_xlabel('Tree depth', fontsize=14)
    ax.set_ylabel('Accuracy', fontsize=14)
    ax.set_ylim(ylim)
    ax.set_xticks(depths)
    ax.legend()

# fitting trees of depth 1 to 24
sm_tree_depths = range(1,25)
sm_cv_scores_mean, sm_cv_scores_std, sm_accuracy_scores = run_cross_validation_on_trees(X_train, y_train, sm_tree_depths)

# plotting accuracy
plot_cross_validation_on_trees(sm_tree_depths, sm_cv_scores_mean, sm_cv_scores_std, sm_accuracy_scores, 'Accuracy per decision tree depth on training data')

#identifying the best
def identify_best_depth_level(sm_cv_scores_mean,sm_tree_depths,sm_cv_scores_std):
  idx_max = sm_cv_scores_mean.argmax()
  sm_best_tree_depth = sm_tree_depths[idx_max]
  sm_best_tree_cv_score = sm_cv_scores_mean[idx_max]
  sm_best_tree_cv_score_std = sm_cv_scores_std[idx_max]
  print('The depth-{} tree achieves the best mean cross-validation accuracy {} +/- {}% on training dataset'.format(
        sm_best_tree_depth, round(sm_best_tree_cv_score*100,5), round(sm_best_tree_cv_score_std*100, 5)))
  return sm_best_tree_depth

best_tree_depth = identify_best_depth_level(sm_cv_scores_mean,sm_tree_depths,sm_cv_scores_std)

"""###Building and Evaluating the final calssification tree

Now after using 10-fold cross validation to identify the best depth for the tree and the best alpha pruning value we can create the final classification tree. To attempt to create the most effective tree for this classification task.

Below we will implement this, showing the results on a confusion matrix as well as printing off the final decision tree layout.

A light discussion will take place discussing this technique and the limitations and advantages of decision trees. 

"""

#Here we build the final decition tree, only this time we use the optimual value for alpha found via 10-fold cross validation
pruned_model_tree = DecisionTreeClassifier(ccp_alpha = ideal_cpp_alpha_value, class_weight='balanced', max_depth=best_tree_depth)
pruned_model_tree = pruned_model_tree.fit(X_train,y_train)

#Here we print the final results via a confusion matrix to see if it performed better than the benchmark done earlier
plot_confusion_matrix(pruned_model_tree, X_valid, y_valid, display_labels = ["Is a Galaxy","Is a Quasar","Is a Star"])

#plot the final prunned decition tree
plot_decition_tree(pruned_model_tree)

predict = cross_val_predict(estimator = pruned_model_tree, X=X, y=y, cv = 10)

print("report:\n",classification_report(y,predict))

"""###Decision tree summary and learnings
Shown above is the final decision tree layout, confusion matrix and classification report to summarise the effectiveness of the decision tree model created. 

Most importantly to see the overall accuracy score is 97% this represents a high score which is promising for the capabilities of this model to perform well in future. As previously mentioned before inside of this report, decision trees can be vulnerable to over-fitting to the training data. 

Overfitting is a concept in data science, which occurs when a statistical model fits exactly against its training data. When this  happens, the algorithm unfortunately cannot perform accurately against unseen data, defeating the overall purpose of the model.

Due to this problem decision trees face, steps have been take in order to reduce the chances of overfitting. Cross validation has been used in order to define the best tree depth as well as it has been used to identify the best alpha pruning value, both these things help create a more robust and overall accurate model that can be used for other datasets. 

Further detail and comparison will take place in the conclusion at the end of this report.

##K-nearest neighbours

K-nearest neighbours is a simple, supervised machine learning algorithm that can be used to solve both classification and regression problems. It is seen as easy to implement and understand, but has a major drawback of becoming significantly slow as the size of the data increases. 

Summary of decision tree process:
* First data is split into training and testing
* Pre processing
* Create the model 
* Use cross validation to implement model with best K number

Justification: as stated above the k-nearest neighbours does represent the a good algorithm for classification and is easy to implement, which can make it appealing to try. However the drawbacks will be discussed in greater detail in the final conclusion.

###Split data
"""

X,y = split_data(final_df,'class')
#here we create a 80% 20% split between training and testing
X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=0.20,random_state=42)
X_train.shape, X_test.shape, y_train.shape, y_test.shape

"""###Data pre-processing and normalisation

Normalization is needed for K-Means neighbours
"""

#look at the stats for the training data
X_train_stats = X_train.describe()
X_train_stats = X_train_stats.transpose()
X_train_stats

#normalise the training data so that the model can be trained more effectivly

X_train = norm(X_train, X_train_stats)
X_test = norm(X_test, X_train_stats)

X_train.head()

"""###Create model

Here we create the K-Neighbors Classifier (KNN)
"""

#5 neighbours is the most commonly used neighbor value.
k_classifier = KNeighborsClassifier(n_neighbors=5)
k_classifier.fit(X_train, y_train)

y_pred = k_classifier.predict(X_test)

print(classification_report(y_test, y_pred))

plot_confusion_matrix(k_classifier, X_test, y_test, display_labels = ["Is a Galaxy","Is a Quasar","Is a Star"])

"""###Cross Validation implementation & Best K number

In this section there is a comparason being made to show if cross validation is having an impact as well as creating a method to find the best K value to help define the best accuracy score.

These results help define the hyper paramaters above, namely the K value.
"""

#Choose K between 1 to 31
k_range = range(1,100)

#keeping an array of the k_scores made from the cross validation
k_scores = []

#Use iteration to caclulator different k in models. then return the average accuracy based on the cross validation
for k in k_range:
  knn = KNeighborsClassifier(n_neighbors=k)
  scores = cross_val_score(knn, X, y, cv = 5, scoring = 'accuracy') #this is cross validation taking place 10 times on the dataset
  k_scores.append(scores.mean())

scores

#plot to see clearly
plt.plot(k_range, k_scores)
plt.xlabel('Value of K for KNN')
plt.ylabel('Cross-Validation Accuracy')
plt.show()