#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// TODO:s
// Use localTopicCount instead of docTopic
// Check if other uniform sampler (internal Cpp) is faster in rcategorical.


//' @title
//' rcategorical
//' @description
//' Sample a categorical variable from
//'
//' @param p a vector of (pssoble unnormalized) probabilities.
//'
// [[Rcpp::export]]
int rcategorical(NumericVector p){
  int K = p.length();
  double cumsum = 0;

  // Calculate cumsum
  for (int k = 0; k < K; k++) {
    cumsum += p(k);
    // Rcout << " cumsum:" << cumsum << std::endl;
  }

  double u = R::runif(0, 1) * cumsum;

  int x = 0;
  while(u < cumsum) {
    // Rcout << "u:" << u << " cumsum:" << cumsum << std::endl;
    cumsum -= p(x);
    x++;
  }
  x--;
  return x;
}


int vanilla_lda_sampler(IntegerMatrix type_topic, IntegerMatrix doc_topic, IntegerVector num_topic, double beta, double alpha, int v, int d){
  // Create definitions
  int K = type_topic.ncol();
  int V = type_topic.ncol();
  NumericVector prob_k(K);

  // Calculate probability
  for (int k = 0; k < K; k++) {
    prob_k(k) = (type_topic(v, k) + beta) / (num_topic(k) + beta*V);
    prob_k(k) *= (doc_topic(d, k) + alpha);
  }

  // Sample topic
  int new_topic = rcategorical(prob_k);

  return new_topic;
}




//' @title
//' Sample vanilla LDA
//'
//' @description
//' Sample a couple of iterations using simple LDA
//'
//' @param p a vector of (pssoble unnormalized) probabilities.
//'
// [[Rcpp::export]]
IntegerVector sample_vanilla_lda(IntegerVector doc, IntegerVector type, IntegerVector z, int K, int D, int V, int iter, double beta, double alpha) {
  // Remember OBOE!!!

  int N = doc.length();

  // Create datastructures
  IntegerMatrix type_topic(V, K);
  IntegerMatrix doc_topic(D, K);
  IntegerVector num_topics(K); // The number of topic indicators by

  // Initialize matrices
  // TODO: Remove doc_topic
  for (int n = 0; n < N; n++) {
    type_topic(type[n], z[n])++;
    doc_topic(doc[n], z[n])++;
    num_topics(z[n])++;
  }

  for (int i = 0; i < iter; i++) {
    for (int n = 0; n < N; n++) {

      type_topic(type[n], z[n])--;
      doc_topic(doc[n], z[n])--;
      num_topics(z[n])--;

      z[n] = vanilla_lda_sampler(type_topic, doc_topic, num_topics, alpha, beta, type[n], doc[n]);
      // Rcout << "z(n):" << z[n] << std::endl;

      type_topic(type[n], z[n])++;
      doc_topic(doc[n], z[n])++;
      num_topics(z[n])++;
    }
  }

  return z;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
# Tests
set.seed(4711)
count <- integer(3)
for(i in 1:100){
  x <- rcategorical(c(0.7, 1.7, 1)) + 1
  count[x] <- count[x] + 1
}
count

z <- c(1,2,1,2,2,3,3) - 1
sample_vanilla_lda(doc = c(1,2,1,2,2,1,2) - 1, type = c(1,3,2,4,4,5,5) - 1, z = z, K = 3, V = 5, D = 2, iter = 1000, beta = 0.1, alpha = 0.1)
z

*/
