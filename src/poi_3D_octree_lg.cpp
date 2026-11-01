/*
 * poi_3D_octree_lg.cpp -- createpts / poisson_disk / 3d_box with
 * h_lookup octree: the LaGriT side of the octree (poi_3D_octree.h).
 *
 * Called from poi_oct_run (poi_driver_octree.f) in place of poisson_3d_
 * when h_lookup octree is given, in four calls: build, dump, sample and
 * free. Builds the octree; each batch of its new
 * vertices goes into the vertex-only mesh object mo_poi_h_field, with
 * h_field_att set to h_spacing_scalar, and the user's h field file
 * (.mlgi) is run on it; h_field_att is read back and the mesh object
 * deleted. The Fortran then makes the hex grid path's checks on h (at
 * least 0.49 h_spacing_scalar; a warning above 100 times) and reports
 * any error; then an optional dump of the leaves, and the same sampling
 * steps as poisson_3d_, with h looked
 * up in the octree and the neighbour grid on its leaves
 * (poi_3D_octree_sampling.cpp) instead of the uniform grid over the box.
 *
 * Fortran integers are 8 bytes in LaGriT (-fdefault-integer-8): they
 * arrive here as long.
 */
#include <cmath>
#include <cstring>
#include <iostream>
#include <string>
#include <vector>

#include "poi_3D_domain.h"
#include "poi_3D_octree.h"
#include "poi_helperFunctions.h"

#include "lg_c_interface.h"
#include "lg_f_interface.h"

using std::cout;
using std::endl;
using std::string;

static const char *HMO = "mo_poi_h_field";
static const char *HATT = "h_field_att";

static int task(const string &cmd) {
    std::vector<char> c(cmd.begin(), cmd.end());
    c.push_back('\0');
    return int(lg_dotask(c.data()));
}

// why the last h batch failed, for the ERROR block
static string g_cb_err;

// h at n points: the user's .mlgi on a vertex-only mesh object
static int hFromMlgi(std::size_t n, const double *x, const double *y, const double *z, double *h,
                     bool variable, double h_spacing, const string &mlgi) {
    if (!variable) {
        for (std::size_t i = 0; i < n; i++) h[i] = h_spacing;
        return 0;
    }
    task(string("cmo/create/") + HMO + "/" + std::to_string(n) + "/0/hex");
    task(string("cmo/select/") + HMO);
    long icmolen = long(std::strlen(HMO)), iattlen = 3, nlen = 0, ierr = 0;
    double *px = nullptr, *py = nullptr, *pz = nullptr, *ph = nullptr;
    char mo[64];
    std::strcpy(mo, HMO);
    fc_cmo_get_vdouble_(mo, (char *)"xic", &px, &nlen, &ierr, icmolen, iattlen);
    if (ierr != 0 || std::size_t(nlen) < n) {
        g_cb_err = string("no xic in ") + HMO;
        return 1;
    }
    fc_cmo_get_vdouble_(mo, (char *)"yic", &py, &nlen, &ierr, icmolen, iattlen);
    fc_cmo_get_vdouble_(mo, (char *)"zic", &pz, &nlen, &ierr, icmolen, iattlen);
    for (std::size_t i = 0; i < n; i++) { px[i] = x[i]; py[i] = y[i]; pz[i] = z[i]; }
    char hs[64];
    std::snprintf(hs, sizeof hs, "%.12e", h_spacing);
    task(string("cmo/addatt/") + HMO + "/" + HATT + "/vdouble/scalar/nnodes");
    task(string("cmo/setatt/") + HMO + "/" + HATT + "/1 0 0/" + hs);
    task("infile/" + mlgi);
    // the .mlgi may have grown or replaced arrays: get them again
    iattlen = long(std::strlen(HATT));
    fc_cmo_get_vdouble_(mo, (char *)HATT, &ph, &nlen, &ierr, icmolen, iattlen);
    if (ierr != 0 || ph == nullptr || std::size_t(nlen) < n) {
        g_cb_err = string(HATT) + " not found in " + HMO + " after " + mlgi;
        return 1;
    }
    for (std::size_t i = 0; i < n; i++) h[i] = ph[i];
    task(string("cmo/delete/") + HMO);
    return 0;
}

// The octree between poi_oct_run's calls (poi_driver_octree.f):
// build, then the Fortran checks h and reports, then dump and sample,
// then free.
static poi_octree::Octree *g_octree = nullptr;

// copy a message into a blank-padded Fortran character*80
static void toFortran(const string &msg, char *out) {
    for (int k = 0; k < 80; k++) out[k] = k < int(msg.size()) ? msg[k] : ' ';
}

extern "C" {
void poisson_3d_octree_build_(char mlgi_in[LG_NAME_SIZE], long *if_variable, double *h, double *box_look,
                              double *kfactor, long *batch, double *hmin_out, double *hmax_out, char *err_out,
                              long *ierror);
void poisson_3d_octree_dump_(char dump_in[LG_NAME_SIZE], long *ierror);
void poisson_3d_octree_sample_(char mo_out_in[LG_NAME_SIZE], double *h, double *box_samp, long *seed,
                               long *numSamples, long *resampleSweeps);
void poisson_3d_octree_free_();
}

void poisson_3d_octree_free_() {
    delete g_octree;
    g_octree = nullptr;
}

void poisson_3d_octree_build_(char mlgi_in[LG_NAME_SIZE], long *if_variable, double *h, double *box_look,
                              double *kfactor, long *batch, double *hmin_out, double *hmax_out, char *err_out,
                              long *ierror) {
    *ierror = 0;
    poisson_3d_octree_free_();
    char mlgi[LG_NAME_SIZE];
    process_lagrit_string(mlgi_in, mlgi);
    cout << "===== Begin Poisson 3D Sampling (h_lookup octree) =========\n" << endl;
    poi_octree::Params p;
    for (int q = 0; q < 6; q++) p.box[q] = box_look[q];
    p.kfactor = *kfactor;
    p.finest = *h / 1.5;
    p.rootsPerSide = 4;
    p.batch = std::size_t(*batch);
    bool variable = *if_variable != 0;
    double hs = *h;
    string file(mlgi);
    std::size_t nbatch = 0;
    poi_octree::HFunc hf = [&](std::size_t n, const double *x, const double *y, const double *z, double *hv) {
        nbatch++;
        return hFromMlgi(n, x, y, z, hv, variable, hs, file);
    };
    poi_octree::Octree *T = new poi_octree::Octree;
    g_cb_err.clear();
    if (T->build(p, hf) != 0) {
        toFortran(g_cb_err.empty() ? T->error() : g_cb_err, err_out);
        *ierror = 1;
        delete T;
        return;
    }
    const poi_octree::Stats &S = T->stats();
    cout << "\nh_lookup octree: K " << p.kfactor << ", finest cell " << p.finest << ", "
         << S.passes << " passes, " << nbatch << " batches of h_field_att\n";
    cout << "  cells " << S.cells << ", leaves " << S.leaves << ", vertices " << S.vertices << "\n";
    cout << "  (a lattice at the finest spacing would have about "
         << (long long)((std::floor((box_look[1] - box_look[0]) / p.finest) + 1) *
                        (std::floor((box_look[3] - box_look[2]) / p.finest) + 1) *
                        (std::floor((box_look[5] - box_look[4]) / p.finest) + 1))
         << " points)\n  leaves per level:";
    for (std::size_t L = 0; L < S.leaves_per_level.size(); L++) cout << " " << S.leaves_per_level[L];
    cout << "\n  tree arrays " << T->bytesUsed() / 1000000.0 << " MB" << endl;
    double hmin = 1e300, hmax = -1e300;
    for (std::size_t v = 0; v < T->numVertices(); v++) {
        hmin = std::min(hmin, T->vertexH(int32_t(v)));
        hmax = std::max(hmax, T->vertexH(int32_t(v)));
    }
    cout << "  h_field_att at the octree vertices: min " << hmin << ", max " << hmax << endl;
    *hmin_out = hmin;
    *hmax_out = hmax;
    g_octree = T;
}

void poisson_3d_octree_dump_(char dump_in[LG_NAME_SIZE], long *ierror) {
    char dump[LG_NAME_SIZE];
    process_lagrit_string(dump_in, dump);
    *ierror = (g_octree && g_octree->dumpAVS(dump) == 0) ? 0 : 1;
    if (*ierror == 0) cout << "  octree leaves written to " << dump << " (AVS hex, h_field_att, itetlev)" << endl;
}

void poisson_3d_octree_sample_(char mo_out_in[LG_NAME_SIZE], double *h, double *box_samp, long *seed,
                               long *numSamples, long *resampleSweeps) {
    char mo_out[LG_NAME_SIZE];
    process_lagrit_string(mo_out_in, mo_out);
    Domain domain;
    domain.mo_poi_pts_out = mo_out;
    domain.h = *h;
    domain.xMin = box_samp[0]; domain.xMax = box_samp[1];
    domain.yMin = box_samp[2]; domain.yMax = box_samp[3];
    domain.zMin = box_samp[4]; domain.zMax = box_samp[5];
    domain.dfNumCellsX = domain.dfNumCellsY = domain.dfNumCellsZ = 1;
    domain.numSamples = (unsigned int)(*numSamples);
    domain.resampleSweeps = (unsigned int)(*resampleSweeps);
    domain.seed = (unsigned int)(*seed);
    domain.octree = g_octree;
    domain.initializeRandomGenerator(domain.seed);
    domain.setBoundary();
    domain.initializeVariables();
    domain.initializeNeighborGrid();
    domain.sampleEdges();
    for (unsigned int i = 0; i < 4; i++) domain.sampleFaces();
    domain.mainSampling(0, false);
    domain.findEmptyCells();
    domain.fillEmptyCells();
    domain.resample();
    domain.addNodesToMeshObject();
    domain.octree = nullptr;
    cout << "\n===== Finished Poisson 3D Sampling (h_lookup octree) =========\n\n" << endl;
}
