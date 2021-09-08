#include "Lagrit.hpp"

int main(int argc, char* argv) {

    std::cout << "Initializing...\n";
    Lagrit::initialize(true);

    std::cout << "Sending command...\n";
    Lagrit::sendCommand("cmo/create/mo1///hex");
    Lagrit::sendCommand("createpts/brick/xyz/3,2,3/0.,0.,0./1.,1.,1./1,1,1");

    std::cout << "Getting active mesh...\n";
    Lagrit::Mesh mo = Lagrit::getActiveMesh();

    std::cout << "Printing mesh...\n";
    mo.print();

    std::cout << "Getting num nodes and num cells...\n";
    int num_nodes = mo.numNodes();
    int num_cells = mo.numCells();

    std::cout << "Num. nodes: " << num_nodes << "; num. cells: " << num_cells << std::endl;

    std::cout << "Getting X vector...\n";

    double *x_vec = mo.getX();

    std::cout << "FINISHED." << std::endl;

    return 0;
}