# Title     : SVD helper functions
# Objective : Logic of SVD recommendation system
# Created by: juan
# Created on: 5/8/21

run_source_svd <- FALSE

svd_get_decomposition<-function (utility_matrix, params) {
  d<-params$d
  U<-svd(utility_matrix,nu=d,nv=d)$u
  Vprime<-svd(utility_matrix,nu=d,nv=d)$v
  list(U=U, Vprime=Vprime)
}


svd_get_item_scores<-function (userid, U, Vprime){
  item_scores<-U[userid,] %*% t(Vprime)
  item_scores
}
