# data only for bidime
int<lower=1> rowsN;  // number of rows in a table
int<lower=1> varsN;  // number of variables
matrix[rowsN, varsN] dv; // dependent variables
matrix[rowsN, varsN] iv; // independent variables
int<lower=1> transform; // transform code (1 - euclidean, 2 - affine, 3 - projective)

