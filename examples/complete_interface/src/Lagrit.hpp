#ifndef __LAGRIT_INTERFACE_HPP__
#define __LAGRIT_INTERFACE_HPP__

#include <string>
#include <iostream>

#define MAX_STR_LEN 100

extern "C" void INITLAGRIT(char [], char [], char [], unsigned int, unsigned int, unsigned int);
extern "C" int DOTASK_C(char [], unsigned int);
extern "C" int CMO_GET_NAME_C(char [], unsigned int);
//extern "C" void CMO_GET_INFO(char [], char [], double**, int*, int*, int*, unsigned int, unsigned int);
extern "C" int CMO_GET_INFO_C(char [], char [], int*, int*, unsigned int, unsigned int);

//extern "C" int CMO_GET_INTINFO_C(char [], char [], int*, int*, int*, int*, unsigned int, unsigned int);
//extern "C" int CMO_GET_INTINFO_C(char [], char [], unsigned int, unsigned int);
extern "C" int CMO_GET_INTINFO_C();//char [], unsigned int, char [], unsigned int);

namespace Lagrit {

namespace MeshIntOptions {
    static const std::string numAttributes = "number_of_attributes";
    static const std::string numNodes = "nnodes";
    static const std::string numCells = "nelements";
    static const std::string numFaces = "nfaces";
    static const std::string numEdges = "nedges";
    static const std::string mBoundary = "mbndry";
    static const std::string dimsTopo = "ndimensions_topo";
    static const std::string dimsGeom = "ndimensions_geom";
    static const std::string nodesPerCell = "nodes_per_element";
    static const std::string edgesPerCell = "edges_per_element";
    static const std::string facesPerCell = "faces_per_element";
}

//using MeshIntOption = std::string;

namespace MeshOptions {
    //const std::string numAttributes = "isetwd";
    //const std::string numAttributes = "ialias";
    static const std::string nodeMaterials = "imt1";
    //static constexpr std::string numAttributes = "itp1";
    //static constexpr std::string numAttributes = "icr1";
    //static constexpr std::string numAttributes = "isn1";
    //static constexpr std::string numAttributes = "ign1";
    static const std::string xVector = "xic";
    static const std::string yVector = "yic";
    static const std::string zVector = "zic";
    static const std::string cellMaterials = "itetclr";
    static const std::string cellGeometry = "itettyp";
    //static const std::string numAttributes = "xtetwd";
    //static const std::string numAttributes = "itetoff";
    //static const std::string numAttributes = "jtetoff";
    static const std::string cellVertices = "itet";
    static const std::string cellConnectivity = "jtet";
}

//using MeshOption = std::string;

class Mesh {
public:
    double* getX();

    std::string getName();
    void print();

    int numNodes();
    int numCells();
    void select();

    Mesh();
    Mesh(std::string name_);

private:
    std::string name;
};

int initialize(bool noisy);
int sendCommand(std::string cmd);
Mesh getActiveMesh();

int GetIntInfo(Mesh *mesh, const std::string ioption);

}

#endif