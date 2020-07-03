# Machine Learning (k-NN vs k-means)
Machine Learning (ML) is a study of computer algorithms that allow a system to automate an analytical modeling without being explicitly programmed. It is a branch of artificial intelligence (AI) that learns from experience, makes decisions, and finds patterns in a massive amount of data (i.e. big data) with minimal human intervention. There are two types of ML, supervised and unsupervised. Supervised ML involves using labeled inputs in a training dataset, learning the function mapped to the outputs, and predicting outputs for the new inputs. Unsupervised ML, on the other hand, does not have labeled responses for the datasets. Instead of predicting associated outputs, unsupervised ML is used for exploratory data analysis such as seeking any hidden pattern or grouping in data. Depending on the algorithms, both types of ML can work with a regression and classification problem. 

Examples
•	Supervised learning — k-nearest neighbors (k-NN), random forest (RF), and super vector machine (SVM)

•	Unsupervised learning (clustering or dimension reduction methods) — k-means clustering, hierarchical clustering, density-based spatial clustering of applications with noise (DBSCAN), and principal component analysis (PCA).  

![ML](pic.png)


In this section, I will use k-NN (supervised) and k-means clustering (unsupervised) as example algorithms and compare the similarities and differences according to the following aspects. (1) From inputs to outputs. Given the same dataset fed into k-NN and k-means algorithms, for instance, U.S. County demographic data with population, race, income, and crime rate, k-NN can predict values in the crime data, of some counties, while k-means can provide the information on whether the crime rate is one of the variables that can cluster counties into different groups. In other words, k-NN makes some prediction about the new data objects, and k-means algorithm aggregates together the observations with similar features. See the coding samples below. (2) Vectors in space (Figure 1). One major similarity between k-NN and k-means is that both algorithms store vectors in a multidimensional feature space, and assume similar things exist in close proximity. From such, k-NN finds k numbers of nearest neighbors and assigns the new objects with the class (or mean value) voted from these k neighbors. K-means, on the other hand, groups together observations that are nearby in space, i.e., observations within one k-means cluster are more similar than those in different clusters.  (3) Distance metrics. The three most famous distance functions used for k-NN and k-means are Euclidean, Manhattan and Minkowski distances. Beyond these, other types of metrics such as correlation-based (Pearson) distance are equivalently popular. (4) No explicit training steps for both algorithms. Even though k-NN is a supervised ML algorithm and has labelled attributes, it is nonparametric and no parameters will be optimized by a train-test split. As for k-means which is unsupervised, there is also no training set to “learn” from, and the model only aims to make some meaningful inferences (i.e., k meaningful clusters). (5) Tuning k value. As an alternative to (4), both models only have one k value that needs to be tuned. As mentioned above, k in k-NN represents the number of neighbors subject to the prediction vote, whereas k in k-means is the number of resultant clusters. Finding a good k value, however, is not trivial for both models. A small k in a k-NN model can potentially lead to the results susceptible to a noise or outlier data; a large k can overpower the supposedly distant points and blur the local effects. As such, k-NN is sensitive to the local structure of the data. In practice, it is recommended to run a cross-validation analysis and determine an optimal k value retrospectively. On the other hand, the determination of the k cluster number for k-means is very subjective. Algorithmically, k-means starts with a random k number of centroids (cluster centers) and assigns observations to the nearest centroid. During the clustering process, a step called “centroids update” is iterated in such a way that the cost function in k-means (within-cluster sum-of-squares) reaches a minimum. In other words, k-means is an iteration algorithm. (6) Performance. Both k-NN and k-means are one of the simplest ML algorithms and are able to outperform a powerful classifier. One major drawback for a k-NN model is the high computational cost. To speed up k-NN computation, other methods such as KD Tree can be incorporated. (7) Application. In retail business, both algorithms have been proven to be reliably performant. k-NN, for instance, is desirable for theft prevention, website categorization, and customer relationship management (CRM). As for k-means, it has a high accuracy (97%) as a spam filter, document classifier, and fraud detector.


