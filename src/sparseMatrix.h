/*
 * sparseMatrix.h
 *
 * This file contains declarations for functions in sparseMatrix.c. There may be
 * useful functions in sparseMatrix.c that are not represented here.
 */

#ifndef SPARSE_MATRIX_H_INCLUDED
#define SPARSE_MATRIX_H_INCLUDED

#include "type_sizes.h"

/*
 * Initialize a sparse matrix. Note that the matrix is stored as a static
 * variable in sparseMatrix.c, so if a matrix is already initialized then it
 * will get clobbered if this function is called a second time.
 */
void createSparseMatrix(
        int_ptrsize numberOfEquations,
        int_ptrsize sparseMatrixEntrySize,
        int_ptrsize Compression,
        double Epsilon);

/*
 * Return TRUE if i,j exists and FALSE otherwise. This function assumes that the
 * matrix has been initialized.
 */
int_ptrsize entryExists(
        int_ptrsize index_i,
        int_ptrsize index_j);

/*
 * Set the i, j entry in the matrix. This function assumes that the matrix has
 * been initialized.
 */
void setEntry(
        int_ptrsize index_i,
        int_ptrsize index_j,
        double volContrib,
        double *value);

void setDiagonalEntries();

#endif /* SPARSE_MATRIX_H_INCLUDED */
