
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <R.h>

extern "C" void gvectorAdd(double *A, double *B, double *C, int *n);

__global__ void
  vectorAdd(const double *A, const double *B, double *C, int numElements)
  {
    int i = blockDim.x * blockIdx.x + threadIdx.x;
    if(i < numElements)
    {
      C[i] = A[i] + B[i];
    }
  }

void gvectorAdd(double *A, double *B, double *C, int *n) 
{
  // Device Memory
  double *d_A, *d_B, *d_C;
  // Define the execution configuration
  dim3 blockSize(256,1,1);
  dim3 gridSize(1,1,1);
  gridSize.x = (*n + blockSize.x - 1) / blockSize.x;
  // Allocate output array
  cudaMalloc((void**)&d_A, *n * sizeof(double));
  cudaMalloc((void**)&d_B, *n * sizeof(double));
  cudaMalloc((void**)&d_C, *n * sizeof(double));
  // copy data to device
  cudaMemcpy(d_A, A, *n * sizeof(double), cudaMemcpyHostToDevice);
  cudaMemcpy(d_B, B, *n * sizeof(double), cudaMemcpyHostToDevice);
  // GPU vector add
  vectorAdd<<<gridSize,blockSize>>>(d_A, d_B, d_C, *n);
  // Copy output
  cudaMemcpy(C, d_C, *n * sizeof(double), cudaMemcpyDeviceToHost);
  cudaFree(d_A);
  cudaFree(d_B);
  cudaFree(d_C);
}
