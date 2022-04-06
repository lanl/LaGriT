#include <lg.h>

int main() {
    LG_ERR err = 0;
    lg_initlagrit();

    const char* cmds[] = {
        "cmo/create/mo1///tet",
        "createpts / random / xyz / .1 / 0 0 0 / 1 1 1 /",
        "connect"};

    for (int i = 0; i < sizeof(cmds)/sizeof(cmds[0]); ++i) {
        err = lg_dotask(cmds[i]);
        if (err != LG_ERR_SUCCESS) {
            printf("Command execution failed: %s\n", cmds[i]);
            return err;
        }
    }

    char cmo_name[512];
    err = lg_cmo_get_name(cmo_name, 512);

    if (err != LG_ERR_SUCCESS) {
        printf("Failed to get cmo name\n");
        return err;
    }

    int nnodes = lg_cmo_get_intinfo("nnodes", cmo_name);
    int nelements = lg_cmo_get_intinfo("nelements", cmo_name);
    int nfaces = lg_cmo_get_intinfo("nfaces", cmo_name);
    int ndims_topo = lg_cmo_get_intinfo("ndimensions_topo", cmo_name);
    int ndims_geom = lg_cmo_get_intinfo("ndimensions_geom", cmo_name);
    int nattribs = lg_cmo_get_intinfo("number_of_attributes", cmo_name);

    // Should not evaluate true if any of the above calls failed
    if (err == LG_ERR_SUCCESS) {
        printf("================\n");
        printf("CMO name: '%s'\n", cmo_name);
        printf("================\n");
        printf("nnodes: %d\n", nnodes);
        printf("nelements: %d\n", nelements);
        printf("nfaces: %d\n", nfaces);
        printf("ndimensions_topo: %d\n", ndims_topo);
        printf("ndimensions_geom: %d\n", ndims_geom);
        printf("number_of_attributes: %d\n", nattribs);
        printf("================\n");
    }

    return 0;
}
