// transform to homogenous coordinates
matrix[rowsN, varsN+1] ivH;
for(iR in 1:rowsN){
  for(iV in 1:varsN) {
    ivH[iR, iV] = iv[iR, iV];
  }
  ivH[iR, varsN+1] = 1;
}
