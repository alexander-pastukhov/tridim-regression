// Transformation matrices

matrix rotation_x_matrix(real theta) {
  matrix[4,4] m =
    [[1, 0, 0, 0],
     [0, cos(theta), sin(theta), 0],
     [0, -sin(theta), cos(theta), 0],
     [0, 0, 0, 1]];

  return(m);
}

matrix rotation_y_matrix(real theta) {
  matrix[4,4] m =
    [[cos(theta), 0, -sin(theta), 0],
     [0, 1, 0, 0],
     [sin(theta), 0, cos(theta), 0],
     [0, 0, 0, 1]];

  return(m);
}

matrix rotation_z_matrix(real theta) {
  matrix[4,4] m =
    [[cos(theta), sin(theta), 0, 0],
     [-sin(theta), cos(theta), 0, 0],
     [0, 0, 1, 0],
     [0, 0, 0, 1]];

  return(m);
}


matrix shear_x_matrix(real shx_y, real shx_z) {
  matrix[4,4] m =
    [[1, 0, 0, 0],
     [shx_y, 1, 0, 0],
     [shx_z, 0, 1, 0],
     [0, 0, 0, 1]];

  return(m);
}

matrix shear_y_matrix(real shy_x, real shy_z) {
  matrix[4,4] m =
    [[1, shy_x, 0, 0],
     [0, 1, 0, 0],
     [0, shy_z, 1, 0],
     [0, 0, 0, 1]];

  return(m);
}

matrix shear_z_matrix(real shz_x, real shz_y) {
  matrix[4,4] m =
    [[1, 0, shz_x, 0],
     [0, 1, shz_y, 0],
     [0, 0, 1, 0],
     [0, 0, 0, 1]];

  return(m);
}


matrix translation_matrix(real dx, real dy, real dz){
  matrix[4,4] m =
    [[1, 0, 0, 0],
     [0, 1, 0, 0],
     [0, 0, 1, 0],
     [dx, dy, dz, 1]];

  return(m);
}

matrix scale_matrix(real sx, real sy, real sz){
  matrix[4,4] m =
    [[sx, 0,  0,  0],
     [0,  sy, 0,  0],
     [0,  0,  sz, 0],
     [0,  0,  0,  1]];

  return(m);
}
