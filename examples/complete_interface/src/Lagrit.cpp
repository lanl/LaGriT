#include "Lagrit.hpp"

namespace Lagrit {

    Mesh::Mesh() {}
    Mesh::Mesh(std::string name_) {
        name = name_;
        // TODO check if mesh exists
    }

    std::string Mesh::getName() {
        return this->name;
    }

    void Mesh::select() {
        sendCommand("cmo/select/" + this->name);
    }

    void Mesh::print() {
        std::cout << this->name << std::endl;
    }

    int Mesh::numNodes() {
        return GetIntInfo(this, (MeshIntOptions::numNodes));
    }

    int Mesh::numCells() {
        return GetIntInfo(this, (MeshIntOptions::numCells));
    }

    int GetIntInfo(Mesh* mesh, const std::string ioption) {
        int *iout;
        int *lout;
        int *itype;
        int *ierror;
        char cmo_c[MAX_STR_LEN];
        char ioption_c[MAX_STR_LEN];

        strcpy(cmo_c, (mesh->getName()).c_str());
        strcpy(ioption_c, ioption.c_str());

        //CMO_GET_INTINFO_C(ioption, cmo_name, iout, lout, itype, ierror)
        //int ierr = CMO_GET_INTINFO_C(cmo_c, ioption_c, iout, lout, itype, ierror, strlen(cmo_c), strlen(ioption_c));
        int ierr = CMO_GET_INTINFO_C();//cmo_c, ioption_c, strlen(cmo_c), strlen(ioption_c));
        //int ierr = CMO_GET_INTINFO_C(cmo_c, 20, ioption_c, 20);

        return -1;
    }

    //int GetInfo(Mesh *mesh, MeshOption ioption) {
    //    mesh->select();
    //    int ierr = CMO_GET_INFO_C(char [], char [], int*, int*, unsigned int, unsigned int);
    //}

    int initialize(bool noisy) {
        char *mode;
        char *log_file = " ";
        char *batch_file = " ";

        if (noisy) {
            mode = "noisy";
        } else {
            mode = "quiet";
        }

        INITLAGRIT(
            mode,
            log_file,
            batch_file,
            strlen(mode),
            strlen(log_file),
            strlen(batch_file)
        );
        return 0;
    }

    int sendCommand(std::string cmd) {
        int ierr = -1;
        const char finish[] = "; finish";

        char *cmd_c = new char[cmd.length() + strlen(finish) + 1];
        strcpy(cmd_c, cmd.c_str());
        strcat(cmd_c, finish);

        ierr = DOTASK_C(cmd_c, strlen(cmd_c));
        delete[] cmd_c;
        return ierr;
    }

    Mesh getActiveMesh() {
        char cmo_c[MAX_STR_LEN];
        int ierr = CMO_GET_NAME_C(cmo_c, MAX_STR_LEN);
        std::string cmo(cmo_c);
        return Mesh(cmo);
    }

}