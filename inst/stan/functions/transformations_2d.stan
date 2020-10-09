// Transformation matrices

matrix rotation_matrix(real theta) {
  matrix[3,3] m =
    [[cos(theta), sin(theta), 0],
    [-sin(theta), cos(theta), 0],
    [0, 0, 1]];

  return(m);
}

matrix translation_matrix(real dx, real dy){
  matrix[3, 3] m =
    [[1, 0, 0],
     [0, 1, 0],
     [dx, dy, 1]];

  return(m);
}

matrix scale_matrix(real sx, real sy){
  matrix[3, 3] m =
    [[sx, 0, 0],
     [0, sy, 0],
     [0,  0, 1]];

  return(m);
}

matrix shear_matrix(real shx, real shy){
  matrix[3, 3] m =
    [[1, shy, 0],
     [shx, 1, 0],
     [0, 0, 1]];

  return(m);
}

matrix tilt_matrix(real e, real f){
  matrix[3, 3] m =
    [[1, 0, e],
     [0, 1, f],
     [0, 0, 1]];

  return(m);
}

