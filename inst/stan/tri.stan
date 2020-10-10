// Bidimensional regression, projective transformation

functions{
#include /functions/transformations_3d.stan
}
data{
#include /data/common_data.stan
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
  real<lower=0> scale[3];
  real<lower=0> shearX[shearXN];
  real<lower=0> shearY[shearYN];
  real<lower=0> shearZ[shearZN];
  real rotation[rotationN];
  real translation[3];

  real<lower=0> sigma[3];
}
transformed parameters{
  matrix[rowsN, 3] predicted;
  {
    matrix[4, 4] transform_matrix = scale_matrix(scale[1], scale[2], scale[3]);
    if (shear_x) transform_matrix = transform_matrix * shear_x_matrix(shearX[1], shearX[2]);
    if (transform == projective) {
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
  scale ~ lognormal(1, 1);
  if (shearXN > 0) shearX ~ lognormal(1, 1);
  if (shearYN > 0) shearY ~ lognormal(1, 1);
  if (shearZN > 0) shearZ ~ lognormal(1, 1);
  rotation ~ normal(0, 10);
  translation ~ normal(0, 10);

  sigma ~ cauchy(0, 1);

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
