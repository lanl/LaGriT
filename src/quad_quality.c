#include <assert.h>
#include <float.h>
#include <math.h>
#include <stdlib.h>
#include "type_sizes.h"

/*
 * If a triangle's area is less than the following fraction of the longest edge
 * of the containing quad, we say that the triangle has zero area for the
 * purposes of determing whether the quad is degenerate.
 */
#define DEGENERACY_THRESHOLD    1.0e-9

/*
 * Warning: these macros may evaluate their arguments twice.
 */
#define SQUARE(x)   ((x) * (x))
#define MIN(x, y)   ((x) < (y) ? (x) : (y))

/*
 * If you change this enum then please change the comments above the
 * quad_quality_ function.
 */
enum quality_flag {
    GOOD = 0,
    DEGENERATE = 1,
    WARPED = 2
};

/*
 * Array of pointers to double. This array consists of { xic, yic, zic }. We
 * wrap these three together in a ragged array in order to facilitate looping
 * over the coordinates.
 */
static const double *coord[3];

/*
 * Return the magnitude of a vector.
 */
static double vector_magnitude(const double v[3])
{
    int_ptrsize i;
    double return_value = 0.0;

    for (i = 0 ; i < 3; i++) {
        return_value += SQUARE(v[i]);
    }

    return_value = sqrt(return_value);
    return return_value;
}

/*
 * Return the Euclidean distance between two points.
 */
static double distance(int_ptrsize p1, int_ptrsize p2)
{
    double return_value = 0.0;
    int_ptrsize i;

    for (i = 0; i < 3; i++) {
        return_value += SQUARE(coord[i][p1] - coord[i][p2]);
    }

    return_value = sqrt(return_value);
    return return_value;
}

/*
 * Compute two quantities based on the lengths of the quad's edges and
 * diagonals:
 *  1. The longest length among all edges and diagonals.
 *  2. The sum in quadrature of the four edge lengths.
 */
static void process_lengths(const int_ptrsize *quad,
        double *longest_length,
        double *quadrature_sum)
{
    double edges[4];
    double diagonals[2];
    int_ptrsize i;

    *longest_length = 0;
    *quadrature_sum = 0;

    for (i = 0; i < 4; i++) {
        edges[i] = distance(quad[i] - 1, quad[(i + 1) % 4] - 1);

        if (edges[i] > *longest_length) {
            *longest_length = edges[i];
        }

        *quadrature_sum += SQUARE(edges[i]);
    }
    *quadrature_sum = sqrt(*quadrature_sum);

    for (i = 0; i < 2; i++) {
        diagonals[i] = distance(quad[i] - 1, quad[i + 2] - 1);
        if (diagonals[i] > *longest_length) {
            *longest_length = diagonals[i];
        }
    }
}

/*
 * Return the dot product of two 3D vectors.
 */
static double dot_product(const double v0[3], const double v1[3])
{
    int_ptrsize i;
    double result = 0;

    for (i = 0; i < 3; i++) {
        result += v0[i] * v1[i];
    }

    return result;
}

/*
 * Calculate the cross product of v0 and v1.
 */
static void cross_product(const double v0[3], const double v1[3],
        double *result)
{
    result[0] = v0[1]*v1[2] - v1[1]*v0[2];
    result[1] = v1[0]*v0[2] - v0[0]*v1[2];
    result[2] = v0[0]*v1[1] - v1[0]*v0[1];
}

/*
 * Compute the vector area of a triangle.
 */
static void triangle_area(const int_ptrsize triangle[3], double area[3])
{
    double v[2][3];
    int_ptrsize i, j, src, dest;

    src = triangle[0];

    for (i = 0; i < 2; i++) {
        dest = triangle[i + 1];
        for (j = 0; j < 3; j++) {
            v[i][j] = coord[j][dest] - coord[j][src];
        }
    }

    cross_product(v[0], v[1], area);
    for (i = 0; i < 3; i++) {
        area[i] *= 0.5;
    }
}

/*
 * Compute the planarity associated with dividing the quad into triangles along
 * a particular diagonal. If diagonal is 0 the function uses the diagonal
 * between the first and third vertices and if diagonal is 1 the function uses
 * the diagonal between the second and fourth vertices.
 *
 * I use a slightly different version of "planarity" from the definition given
 * in the text by Frey and George. By "planarity" I mean the dot product of the
 * unit normals of a pair of triangles. We compute these unit normals via the
 * right-hand rule so they can potentially point in opposite directions.
 *
 * NOTE: This function assumes that the quads have already been checked not to
 * be degenerate, i.e the triangles must not have zero area. If this assumption
 * does not hold then there may be a divide-by-zero problem below.
 */
static double compute_planarity(
        const int_ptrsize quad[4],
        const int_ptrsize diagonal)
{
    /* Determine the triangles we need to examine based on which diagonal we are
     * using. The following two lines look tricky but they save a lot of space
     * and I think they work. */
    int_ptrsize tri_a[3] = { 0, 1, 2 + diagonal };
    int_ptrsize tri_b[3] = { diagonal, 2, 3};
    double area_a[3], area_b[3];
    double mag_a, mag_b;
    int_ptrsize i;

    for (i = 0; i < 3; i++) {
        tri_a[i] = quad[tri_a[i]] - 1;
        tri_b[i] = quad[tri_b[i]] - 1;
    }

    triangle_area(tri_a, area_a);
    triangle_area(tri_b, area_b);

    mag_a = vector_magnitude(area_a);
    mag_b = vector_magnitude(area_b);

    /* Convert the area vectors into unit normals. */
    for (i = 0; i < 3; i++) {
        area_a[i] /= mag_a;
        area_b[i] /= mag_b;
    }

    return dot_product(area_a, area_b);
}

/*
 * Return the minimum area from among the four possible triangles determined by
 * the points of the quad.
 */
static double min_triangle(const int_ptrsize quad[4])
{
    int_ptrsize triangle[3];
    int_ptrsize i, j, k;
    double area[3];
    double area_mag;
    double return_value = DBL_MAX;

    /* This code looks tricky but essentially it just loops through all possible
     * triangles defined by the vertices of the quad. */
    for (i = 0; i < 4; i++) {
        k = 0;
        for (j = 0; j < 3; j++) {
            if (k != i) {
                triangle[j] = quad[k] - 1;
                k++;
            } else {
                triangle[j] = quad[k + 1]  - 1;
                k += 2;
            }
        }

        triangle_area(triangle, area);
        area_mag = vector_magnitude(area);
        if (area_mag < return_value)
            return_value = area_mag;
    }

    return return_value;
}

/*
 * Run some basic tests to verify that the code works as it should.
 */
void run_unit_tests()
{
    /*
     * vector_magnitude
     */
    double v[3] = { 3, 0, 4 };
    int i;
    assert(vector_magnitude(v) == 5.0);

    /*
     * Let's set up some points we can work with.
     */
    const int len = 17;
    double *mycoord[3];
    for (i = 0; i < 3; i++) {
        mycoord[i] = malloc(len * sizeof(double));
    }

    for (i = 0; i < 3; i++) {
        mycoord[i][0] = -1;
    }
    mycoord[0][1] = 2;
    mycoord[1][1] = -1;
    mycoord[2][1] = 3;

    mycoord[0][2] = 0;
    mycoord[1][2] = 0;
    mycoord[2][2] = 0;

    mycoord[0][3] = 1;
    mycoord[1][3] = 0;
    mycoord[2][3] = 0;

    mycoord[0][4] = 1;
    mycoord[1][4] = 1;
    mycoord[2][4] = 0;

    mycoord[0][5] = 0;
    mycoord[1][5] = 1;
    mycoord[2][5] = 0;

    mycoord[0][6] = 0;
    mycoord[1][6] = 0;
    mycoord[2][6] = 0;

    mycoord[0][7] = 1;
    mycoord[1][7] = 0;
    mycoord[2][7] = 0;

    mycoord[0][8] = 0;
    mycoord[1][8] = 0;
    mycoord[2][8] = 2;

    mycoord[0][9] = 0;
    mycoord[1][9] = -1;
    mycoord[2][9] = 0;

    mycoord[0][10] = 2;
    mycoord[1][10] = 0;
    mycoord[2][10] = 0;

    mycoord[0][11] = 2;
    mycoord[1][11] = 0;
    mycoord[2][11] = 1;

    mycoord[0][12] = 0;
    mycoord[1][12] = 1;
    mycoord[2][12] = 0;

    mycoord[0][13] = 0;
    mycoord[1][13] = 0;
    mycoord[2][13] = 0;

    mycoord[0][14] = 1;
    mycoord[1][14] = 0;
    mycoord[2][14] = 0;

    mycoord[0][15] = 10;
    mycoord[1][15] = 10;
    mycoord[2][15] = 0;

    mycoord[0][16] = 0;
    mycoord[1][16] = 1;
    mycoord[2][16] = 0;

    for (i = 0; i < 3; i++) {
        coord[i] = mycoord[i];
    }

    /*
     * distance
     */
    assert(distance(0, 1) == 5.0);

    /*
     * process_lengths
     */

    /* For quads, we use the Fortran convention of numbering from one. This is
     * because we usually grab these quads directly from itet. */
    int_ptrsize quad[4] = { 3, 4, 5, 6 };
    double longest_length;
    double quadrature_sum;

    process_lengths(quad, &longest_length, &quadrature_sum);
    assert(longest_length == sqrt(2));
    assert(quadrature_sum == 2.0);

    /*
     * dot_product
     */
    double v0[3] = { 1, 7, -3 };
    double v1[3] = { 2, 5, 4 };
    assert(dot_product(v0, v1) == 25);

    /*
     * cross_product
     */
    double result[3];
    v0[0] = 1; v0[1] = 0; v0[2] = 0;
    v1[0] = 0; v1[1] = 1; v1[2] = 0;
    cross_product(v0, v1, result);
    assert(result[0] == 0);
    assert(result[1] == 0);
    assert(result[2] == 1);

    /*
     * triangle_area
     */
    /* NOTE: Here triangles use the C convention of starting from 0. */
    int_ptrsize triangle[3] = { 6, 7, 8};
    double area[3];
    triangle_area(triangle, area);
    /* We should expect an area of (0, -1, 0). In practice the zeros are not
     * exactly zero. */
    assert(abs(area[0] - 0) < 10e-50);
    assert(area[1] == -1);
    assert(abs(area[2] - 0) < 10e-50);

    /*
     * compute_planarity
     */

    /* This is a legit planar quad, so both of its planarities should be 1. */
    quad[0] = 3;
    quad[1] = 4;
    quad[2] = 5;
    quad[3] = 6;
    assert(compute_planarity(quad, 0) == 1);
    assert(compute_planarity(quad, 1) == 1);

    /* This quad has one edge rotated 90 degrees, so one of its diagonals should
     * divide it into two triangles with perpendicular area vectors. */
    quad[0] = 10;
    quad[1] = 11;
    quad[2] = 12;
    quad[3] = 13;
    assert(compute_planarity(quad, 1) == 0);

    /*
     * min_triangle
     */

    quad[0] = 3;
    quad[1] = 4;
    quad[2] = 5;
    quad[3] = 6;
    /* This is a unit square, so its minimum triangle should be 0.5. */
    assert(min_triangle(quad) == 0.5);

    quad[0] = 14;
    quad[1] = 15;
    quad[2] = 16;
    quad[3] = 17;
    assert(min_triangle(quad) == 0.5);

    /* Clean up before exiting. */
    for (i = 0; i < 3; i++) {
        free(mycoord[i]);
    }

}

/*
 * Fill the attributes quality, regularity, and flag with various measures of
 * the quality of each quadrilateral in the mesh.
 *
 * For the quality attribute we  use the inverse of the measure described in
 * 18.2.4 of the mesh generation text by Frey and George, namely:
 *
 *          alpha * h_max * h_s
 *      Q = -------------------
 *                 S_min
 *
 * where alpha is a normalization constant (sqrt(2)/8), h_max is the longest
 * length among the four edges and two diagonals, h_s is the quadrature sum of
 * the four edge lengths, and S_min is the minimum of the areas of the four
 * triangles defined by the points of the quad.
 *
 * NOTE: We compute the inverse of the above metric. This is generally
 * considered a good idea to do in practice because it eliminates the
 * divide-by-zero problem and also gives a nice range from 0 to 1.
 *
 * For the regularity attribute we compute (1 + P)/2, where P is the smaller of
 * the two planarities of the quad (one associated with each diagonal). See the
 * comments above compute_planarity for more details on what "planarity" means.
 * This metric works out nicely to scale between 0 and 1, where 0 represents a
 * totally warped, nonplanar quad and 1 is a perfectly flat, planar quad.
 *
 * Finally, flag is an integer attribute that gives qualitative information
 * about each quad:
 *
 *      GOOD (0) -          The quad is non-degenerate and both its planarities
 *                          are positive.
 *
 *      DEGENERATE (1) -    At least one of the triangles determined by the
 *                          vertices of the quad has zero area.
 *
 *      WARPED (2) -        One or both planarities are negative. This means
 *                          that one of the quad's diagonals divides it into two
 *                          triangles whose area vectors have a negative dot
 *                          product. If the quad is planar then this means it is
 *                          self-intersecting (or someone just typed in the
 *                          vertices in the wrong order).
 */
void quad_quality_(const int_ptrsize (*quads)[4],
                   const int_ptrsize *nquads,
                   const double *xic,
                   const double *yic,
                   const double *zic,
                   double *quality,
                   double *regularity,
                   int_ptrsize *flag)
{
    const double alpha = sqrt(2) / 8.0;     /* normalization constant */
    double longest_length;
    double quadrature_sum;
    double min_tri_area;
    double n0, n1;
    int_ptrsize i;
    int_ptrsize negative_planarities;

    coord[0] = xic;
    coord[1] = yic;
    coord[2] = zic;

    for (i = 0; i < *nquads; i++) {
        /* Assume everything's good until we discover otherwise. */
        flag[i] = GOOD;
        negative_planarities = 0;

        process_lengths(quads[i], &longest_length, &quadrature_sum);
        min_tri_area = min_triangle(quads[i]);

        if (min_tri_area == 0 ||
                min_tri_area < DEGENERACY_THRESHOLD * longest_length) {
            flag[i] = DEGENERATE;
            /* If a triangle has zero area then the other metrics are not
             * defined, so we set them to zero. */
            quality[i] = 0.0;
            regularity[i] = 0.0;
            continue;
        }

        quality[i] = min_tri_area / (alpha * longest_length * quadrature_sum);

        n0 = compute_planarity(quads[i], 0);
        n1 = compute_planarity(quads[i], 1);

        negative_planarities += (n0 < 0 ? 1 : 0);
        negative_planarities += (n1 < 0 ? 1 : 0);

        if (negative_planarities > 0)
            flag[i] = WARPED;

        regularity[i] = 0.5 * (1 + MIN(n0, n1));
    }
}
