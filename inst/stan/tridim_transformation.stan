data{
  int<lower=0, upper=6> transform; // see `transformed data` for codes

  int<lower=1> rowsN;  // number of rows in a table
  int<lower=1> varsN;  // number of variables / dimensions
  int<lower=0> betaN;  // number of matrix coefficients apart from translation
  matrix[rowsN, varsN] dv; // dependent variables
  matrix[rowsN, varsN] iv; // independent variables
  vector<lower=0>[varsN] dv_sd; //standard deviation of dependent variables, used to scale sigma in loss function

  // priors
  real translation_prior[2]; // mean and sigma for translation
  real beta_prior[2];        // common mean and sigma for all betas
  real<lower=0> sigma_prior;
}
transformed data{
  int longN = rowsN * varsN;

  // symbolic constants for transforms
  int TRANSLATION = 0;
  int EUCLIDEAN = 1;
  int AFFINE = 2;
  int PROJECTIVE = 3;
  int EUCLIDEAN_X = 4;
  int EUCLIDEAN_y = 5;
  int EUCLIDEAN_z = 6;

  // dependent variable values and standard deviations
  vector[longN] dv_long;
  vector[longN] iv_long;
  vector[longN] dv_sd_long;

  // homogenous coordinates
  matrix[rowsN, varsN+1] ivH;

  // long vector of variables and dv standard deviations
  dv_long = to_vector(dv);
  iv_long = to_vector(iv);
  dv_sd_long = to_vector(rep_matrix(dv_sd, rowsN));

  // transform to homogenous coordinates
  for(iR in 1:rowsN){
    for(iV in 1:varsN) {
      ivH[iR, iV] = iv[iR, iV];
    }
    ivH[iR, varsN+1] = 1;
  }
}
parameters{
  // transformation
  real a[varsN];
  real b[betaN];

  // loss function parameter
  real<lower=0> sigma;
}
transformed parameters{
  vector[longN] predicted;

  // building transformation matrix
  matrix[varsN + 1, varsN + 1] M;

  if (varsN == 2){
    // 2D transformations
    if (transform == TRANSLATION){
      M = [[ 1,    0,    0],
           [ 0,    1,    0],
           [ a[1], a[2], 1]];
    }
    else if (transform == EUCLIDEAN){
      M = [[ b[1],-b[2], 0],
           [ b[2], b[1], 0],
           [ a[1], a[2], 1]];
    }
    else if (transform == AFFINE) {
      M = [[ b[1], b[3], 0],
           [ b[2], b[4], 0],
           [ a[1], a[2], 1]];
    }
    else if (transform == PROJECTIVE) {
      M = [[ b[1], b[3], b[5]],
           [ b[2], b[4], b[6]],
           [ a[1], a[2], 1   ]];
    }
  }
  else {
    // 3D transformations
    if (transform == TRANSLATION){
      M = [[ 1,    0,    0,    0],
           [ 0,    1,    0,    0],
           [ 0,    0,    1,    0],
           [ a[1], a[2], a[3], 1]];
    }
    else if (transform == EUCLIDEAN_X){
      real phi; // scaling for 3D single axis Eucledian
      phi = sqrt(b[1] * b[1] + b[2] * b[2]);

      M = [[ phi,  0,    0,    0],
           [ 0,    b[1], b[2], 0],
           [ 0,   -b[2], b[1], 0],
           [ a[1], a[2], a[3], 1]];
    }
    else if (transform == EUCLIDEAN_y){
      real phi; // scaling for 3D single axis Eucledian
      phi = sqrt(b[1] * b[1] + b[2] * b[2]);

      M = [[ b[1], 0,   -b[2], 0],
           [ 0,    phi,  0   , 0],
           [ b[2], 0,    b[1], 0],
           [ a[1], a[2], a[3], 1]];
    }
    else if (transform == EUCLIDEAN_z){
      real phi; // scaling for 3D single axis Eucledian
      phi = sqrt(b[1] * b[1] + b[2] * b[2]);

      M = [[ b[1], b[2], 0,    0],
           [-b[2], b[1], 0,    0],
           [ 0,    0,    phi,  0],
           [ a[1], a[2], a[3], 1]];
    }
    else if (transform == AFFINE){
      M = [[ b[1], b[4], b[7], 0],
           [ b[2], b[5], b[8], 0],
           [ b[3], b[6], b[9], 0],
           [ a[1], a[2], a[3], 1]];
    }
    else if (transform == PROJECTIVE) {
      M = [[ b[1], b[4], b[7], b[10]],
           [ b[2], b[5], b[8], b[11]],
           [ b[3], b[6], b[9], b[12]],
           [ a[1], a[2], a[3], 1    ]];
    }
  }

  // generating prediction, discarding extra dimension
  predicted = to_vector(block(ivH * M, 1, 1, rowsN, varsN));
}
model{
  // sampling, vectorized as we pivoted data to long
  dv_long ~ normal(predicted, dv_sd_long .* rep_vector(sigma, longN));

  // priors
  sigma ~ exponential(sigma_prior);
  a ~ normal(translation_prior[1], translation_prior[2]);
  if (transform != TRANSLATION){
    b ~ normal(beta_prior[1], beta_prior[2]);
  }
}
generated quantities{
  vector[rowsN] log_lik;
  for(iR in 1:rowsN){
    log_lik[iR] = 0;
    for(iV in 1:varsN){
      log_lik[iR] += normal_lpdf(dv[iR, iV] | predicted[(iR-1) * varsN + iV], sigma * dv_sd[iV]);
    }
  }
}
