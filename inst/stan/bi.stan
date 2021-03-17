// Bidimensional regression, projective transformation

functions{
#include /functions/transformations_2d.stan
}
data{
#include /data/common_data.stan
#include /data/common_priors.stan
}
transformed data{
  // symbolic constants for transforms
  int euclidean = 1;
  int affine = 2;
  int projective = 3;

// transform to homogenous coordinates
#include /transformed_data/transform_to_homogenous.stan
}
parameters{
  real<lower=0> log_scale[2];
  real<lower=0> log_shear[transform != euclidean ? 1 : 0]; // affine or projective
  real rotation;
  real tilt[transform == projective ? 2 : 0];  // projective only
  real translation[2];

  real<lower=0> sigma[2];
}
transformed parameters{
  real scale[2] = exp(log_scale);
  real shear[transform != euclidean ? 1 : 0];
  matrix[rowsN, 2] predicted;
  {
    matrix[3, 3] transform_matrix = scale_matrix(scale[1], scale[2]);
    if (transform != euclidean) {
      shear = exp(log_shear);
      transform_matrix = transform_matrix * shear_matrix(0, shear[1]);
    }
    transform_matrix = transform_matrix * rotation_matrix(rotation);
    if (transform == projective) transform_matrix = transform_matrix * tilt_matrix(tilt[1], tilt[2]);
    transform_matrix = transform_matrix * translation_matrix(translation[1], translation[2]);

    predicted = block(iv3 * transform_matrix, 1, 1, rowsN, 2);
  }
}
model{
  log_scale ~ normal(scale_prior[1], scale_prior[2]);
  if (transform != euclidean) log_shear ~ normal(sheer_prior[1], sheer_prior[1]);
  if (normal_rotation_prior == 1){
    rotation ~ normal(rotation_prior[1], rotation_prior[2]);
  }
  else {
    rotation ~ uniform(-pi(), +pi());
  }
  if (transform == projective) tilt ~ normal(tilt_prior[1], tilt_prior[2]);
  translation ~ normal(translation_prior[1], translation_prior[2]);

  sigma ~ exponential(sigma_prior);

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
