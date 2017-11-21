# Combine-CF-and-CB_final-project_RS
This is the Recommendation System final project, I combined the CF and CB together and used cross-validation to check the performance.

# The collaborative filtering component
The collaborative filtering component of the system used the "Pearson Correlations" to compute from the known, past user-item ratings, providing for a memory component of the recommender. Finally, I got a User-User similarity matrix(943*943) and an Item-Item similarity matrix(1682*1682).

# The content based component 
The content based componentof the system computes the "jaccard function" based on the users and items. The jaccard function
usually is used on a sparse Matrix. Finally, I got a UserDistance matrix(943*943) and an ItemDistance matrix(1682*1682).
Adding weight to CF and CB.

# Prediction
Compare cosine similarity and do User-User and Item-Item Prediction.

# Cross validation
Compute prediction for each vote while keep other original vote and Mean absolute error and Mean square error for
User-User and Item-Item separately.

# Conclusion
The collaborative filtering performed well on User-User analysis, when the weight of content-based increased, the performance decreased.

The collaborative filtering and content-based approached both performed well separately on Item-Item analysis.

But combining the CF and CB together, the User-User and Item-Item both performed worse.

The value of MSE in both two cases is higher than MAE, because of the square is a big number.

We can conclude that this hybrid method combining CF and CB does not suit the sparse matrix.

# Remind 
The detailed explanation is on the slide which I uploaded and the comments on the code.
