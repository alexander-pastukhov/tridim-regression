// adding extra dimension to independent variables
// so that we can use them with transformation matrix
matrix[rowsN, varsN+1] iv3;
for(iR in 1:rowsN){
  for(iV in 1:varsN) {
    iv3[iR, iV] = iv[iR, iV];
  }
  iv3[iR, varsN+1] = 1;
}
