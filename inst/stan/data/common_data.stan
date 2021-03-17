int<lower=1> rowsN;  // number of rows in a table
int<lower=1> varsN;  // number of variables
matrix[rowsN, varsN] dv; // dependent variables
matrix[rowsN, varsN] iv; // independent variables
int<lower=0> transform; // transform code (0 - none / identity matrix, 1 - euclidean, 2 - affine, 3 - projective)
int<lower=0> betaN;  // number of matrix coefficients (apart from translation) to be fitted
