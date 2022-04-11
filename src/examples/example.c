#include <lg.h>

int main() {
    LG_ERR err = 0;
    const char* cmds[] = {
        "cmo/create/mo1///tet",
        "createpts / random / xyz / .1 / 0 0 0 / 1 1 1 /",
        "connect"
    };

    /// Initialize LaGriT ///////////////////////////////
    LGInitialize(LG_INIT_NOISY);

    /// Send command buffers ////////////////////////////
    for (int i = 0; i < (sizeof(cmds) / sizeof(cmds[0])); i++) {
        err = LGSendCommand(cmds[i]);

        if (err != LG_ERR_SUCCESS) {
            printf("Command execution failed: %s\n", cmds[i]);
            return err;
        }
    }

    printf("getting cmo name...\n");

    char cmo_name[512];
    err = lg_cmo_get_name(&cmo_name, 512);

    if (err != LG_ERR_SUCCESS) {
        printf("Failed to get cmo name\n");
        return err;
    }

    printf("getting ptrs...\n");
    LGMeshStdPtrs mesh_ptrs = {};
    err = lg_cmo_get_stdptrs(cmo_name, &mesh_ptrs);

    return 0;

    printf("getting info...\n");

    printf("[exe] sizeof(int) = %d\n", sizeof(int));
    int nnodes = 666, nelements, nfaces, ndims_topo, ndims_geom, nattributes;

    err = lg_cmo_get_intinfo("nnodes", cmo_name, &nnodes);
    err = lg_cmo_get_intinfo("nelements", cmo_name, &nelements);
    err = lg_cmo_get_intinfo("nfaces", cmo_name, &nfaces);
    err = lg_cmo_get_intinfo("ndimensions_topo", cmo_name, &ndims_topo);
    err = lg_cmo_get_intinfo("ndimensions_geom", cmo_name, &ndims_geom);
    err = lg_cmo_get_intinfo("number_of_attributes", cmo_name, &nattributes);

    // Should not evaluate true if any of the above calls failed
    if (err == LG_ERR_SUCCESS) {
        printf("================\n");
        printf("CMO name: '%s'\n", cmo_name);
        printf("================\n");
        printf("nnodes: %d\n", nnodes);
        printf("nelements: %d\n", nelements);
        printf("nfaces: %d\n", nfaces);
        printf("number_of_attributes: %d\n", nattributes);
        printf("ndimensions_topo: %d\n", ndims_topo);
        printf("ndimensions_geom: %d\n", ndims_geom);
        printf("================\n");
    }

    return 0;
}
