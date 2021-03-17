data{
#include /data/common_data.stan
}
transformed data{
  // symbolic constants for transforms
  int identity = 0;
  int euclidean = 1;
  int affine = 2;
  int projective = 3;

  // --- computing number of parameters to fit ---
  // Zero are defaults for no transformation / identity matrix
  int scaleN = 0;
  int betaN = 0;

  // all transformation have translation
  if (transform != identity){
    scaleN = 2;
  }

  if (transform == euclidean) {
    // eucledian: common scale (1) + rotation (1)
    betaN = 2;
  }
  else if (transform == affine) {
    // affine: scale with rotation and x and or y-sheer
    betaN = 4;
  }
  else if (transform == projective) {
    // projective : affine + projective coefficients
    betaN = 6;
  }
  else{
     reject("Unknown transformation");
  }
}
parameters{
  real dxy[scaleN];
  real b[betaN];

  real<lower=0> sigma[2];
}
model{
  // building transformation matrix
  matrix[3, 3] M;
  if (transform == euclidean){
    M = [[ b[1], b[2], dxy[1]],
         [-b[2], b[1], dxy[2]],
         [ 0,    0,    1     ]];
  }
  else if (transform == affine) {
    M = [[ b[1], b[2], dxy[1]],
         [ b[3], b[4], dxy[2]],
         [ 0,    0,    1     ]];
  }
  else if (transform == projective) {
    M = [[ b[1], b[2], dxy[1]],
         [ b[3], b[4], dxy[2]],
         [ b[5], b[6], 1     ]];
  }

  // generating prediction, discarding extra dimension
  matrix[rowsN, 2] predicted = iv;
  if (transform != identity){
    predicted = block(ivH * M, 1, 1, rowsN, 2);
  }

  // sampling
  for(iR in 1:rowsN){
    dv[iR, 1] ~ normal(predicted[iR, 1], sigma[1]);
    dv[iR, 2] ~ normal(predicted[iR, 2], sigma[2]);
  }

  // priors for betas
}
