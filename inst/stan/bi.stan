// Bidimensional regression, projective transformation

functions{
#include /functions/transformations_2d.stan
}
data{
#include /data/common_data.stan
}
transformed data{
  // symbolic constants for transforms
  int euclidean = 1;
  int affine = 2;
  int projective = 3;

// We need a +1 dimension in the original data
#include /transformed_data/add_dimension.stan
}
parameters{
  real<lower=0> scale[2];
  real<lower=0> shear[transform != euclidean ? 1 : 0]; // affine or projective
  real rotation;
  real tilt[transform == projective ? 2 : 0];  // projective only
  real translation[2];

  real<lower=0> sigma[2];
}
transformed parameters{
  matrix[rowsN, 2] predicted;
  {
    matrix[3, 3] transform_matrix = scale_matrix(scale[1], scale[2]);
    if (transform != euclidean) transform_matrix = transform_matrix * shear_matrix(0, shear[1]);
    transform_matrix = transform_matrix * rotation_matrix(rotation);
    if (transform == projective) transform_matrix = transform_matrix * tilt_matrix(tilt[1], tilt[2]);
    transform_matrix = transform_matrix * translation_matrix(translation[1], translation[2]);

    predicted = block(iv3 * transform_matrix, 1, 1, rowsN, 2);
  }
}
model{
  scale ~ lognormal(1, 1);
  if (transform != euclidean) shear ~ lognormal(1, 1);
  rotation ~ normal(0, 10);
  if (transform == projective) tilt ~ normal(0, 10);
  translation ~ normal(0, 10);

  sigma ~ cauchy(0, 1);

  for(iR in 1:rowsN){
    dv[iR, 1] ~ normal(predicted[iR, 1], sigma[1]);
    dv[iR, 2] ~ normal(predicted[iR, 2], sigma[2]);
  }
}
generated quantities{
  real log_lik[2*rowsN];
  for(iR in 1:rowsN){
    log_lik[iR] = normal_lpdf(dv[iR, 1] | predicted[iR, 1], sigma[1]);
    log_lik[iR + rowsN] = normal_lpdf(dv[iR, 2] | predicted[iR, 2], sigma[2]);
  }
}
