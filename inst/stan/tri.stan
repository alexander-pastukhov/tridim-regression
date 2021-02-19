// Bidimensional regression, projective transformation

functions{
#include /functions/transformations_3d.stan
}
data{
#include /data/common_data.stan
#include /data/common_priors.stan
}
transformed data{
  // symbolic constants for transforms
  int euclidean_x = 4;
  int euclidean_y = 5;
  int euclidean_z = 6;
  int euclidean = 1;
  int affine = 2;
  int projective =3;

  // selecting which parameters we need to sample
  int rotationN = transform > projective ? 1 : 3;
  int shearXN = (transform == affine || transform == projective) ? 2 : 0;
  int shearYN = (transform == projective) ? 2 : 0;
  int shearZN = (transform == projective) ? 2 : 0;

  // precomputing conditions flag for which transforms we will use
  int rotate_x = transform != euclidean_y && transform != euclidean_z;
  int rotate_y = transform != euclidean_x && transform != euclidean_z;
  int rotate_z = transform != euclidean_x && transform != euclidean_y;
  int shear_x = transform == affine || transform == projective;

  // index of the rotation element in the vector or 1 (if only one angle is used)
  int irotate_x = 1;
  int irotate_y = transform == euclidean_y ? 1 : 2;
  int irotate_z = transform == euclidean_z ? 1 : 3;

// We need a +1 dimension in the original data
#include /transformed_data/add_dimension.stan
}
parameters{
  real<lower=0> log_scale[3];
  real<lower=0> log_shearX[shearXN];
  real<lower=0> log_shearY[shearYN];
  real<lower=0> log_shearZ[shearZN];
  real rotation[rotationN];
  real translation[3];

  real<lower=0> sigma[3];
}
transformed parameters{
  real scale[3] = exp(log_scale);
  real shearX[shearXN];
  real shearY[shearYN];
  real shearZ[shearZN];

  matrix[rowsN, 3] predicted;
  {
    matrix[4, 4] transform_matrix = scale_matrix(scale[1], scale[2], scale[3]);
    if (shear_x) {
      shearX = exp(log_shearX);
      transform_matrix = transform_matrix * shear_x_matrix(shearX[1], shearX[2]);
    }
    if (transform == projective) {
      shearY = exp(log_shearY);
      shearZ = exp(log_shearZ);
      transform_matrix = transform_matrix *
                         shear_y_matrix(shearY[1], shearY[2]) *
                         shear_z_matrix(shearZ[1], shearZ[2]);
    }
    if (rotate_x) transform_matrix = transform_matrix * rotation_x_matrix(rotation[irotate_x]);
    if (rotate_y) transform_matrix = transform_matrix * rotation_y_matrix(rotation[irotate_y]);
    if (rotate_z) transform_matrix = transform_matrix * rotation_z_matrix(rotation[irotate_z]);

    transform_matrix = transform_matrix * translation_matrix(translation[1], translation[2], translation[3]);

    predicted = block(iv3 * transform_matrix, 1, 1, rowsN, 3);
  }
}
model{
  log_scale ~ normal(scale_prior[1], scale_prior[2]);
  if (shearXN > 0) log_shearX ~ normal(sheer_prior[1], sheer_prior[1]);
  if (shearYN > 0) log_shearY ~ normal(sheer_prior[1], sheer_prior[1]);
  if (shearZN > 0) log_shearZ ~ normal(sheer_prior[1], sheer_prior[1]);
  if (normal_rotation_prior == 1){
    rotation ~ normal(rotation_prior[1], rotation_prior[2]);
  }
  else {
    rotation ~ uniform(-pi(), +pi());
  }

  translation ~ normal(translation_prior[1], translation_prior[2]);

  sigma ~ exponential(sigma_prior);

  for(iR in 1:rowsN){
    dv[iR, 1] ~ normal(predicted[iR, 1], sigma[1]);
    dv[iR, 2] ~ normal(predicted[iR, 2], sigma[2]);
    dv[iR, 3] ~ normal(predicted[iR, 3], sigma[3]);
  }
}
generated quantities{
  real log_lik[3*rowsN];
  for(iR in 1:rowsN){
    log_lik[iR] = normal_lpdf(dv[iR, 1] | predicted[iR, 1], sigma[1]);
    log_lik[iR + rowsN] = normal_lpdf(dv[iR, 2] | predicted[iR, 2], sigma[2]);
    log_lik[iR + 2 * rowsN] = normal_lpdf(dv[iR, 3] | predicted[iR, 3], sigma[3]);
  }
}
