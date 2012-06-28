#include <cassert>
#include <fstream>
#include <iostream>
#include <map>
#include <set>

#include "type_sizes.h"

using namespace std;

/* Forward reference */
struct Node;

/*
 * Note: these edges are undirected, but we maintain the invariant that the src
 * node always has a lower integer id than the dest node. I don't remember why I
 * did it that way but I think I had a good reason at the time.
 */
class Edge {
public:
    Node *src, *dest;
    bool seen;

    Edge();
    bool operator()(Edge *e, Edge *f);
    Edge(Node *src, Node *dest);
    Node *other(Node *);
};

typedef struct Node {
    int_ptrsize id;
    bool seen;
    set<Edge *> incident_edges;
} Node;

#ifdef __cplusplus
extern "C" {
#endif
void line_graph_nsort_(
        int_ptrsize edges[][2],
	int_ptrsize *nsort_key,
        int_ptrsize *len);
#ifdef __cplusplus
}
#endif

/*
 * The main driver for this code is line_graph_sort_, near the bottom. It ends
 * with an underscore but it should be called from Fortran without the
 * underscore, as 'line_graph_sort'.
 *
 * See below for details!
 */

#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) < (y) ? (y) : (x))

/*
 * A map from integer indices to Node objects.
 */
static map<int_ptrsize, Node *> *int_to_node;

/*
 * Nodes that have only a single incident edge.
 */
static set<Node *> *singletons;

/*
 * An array of Edge objects. These are equivalent to the input edges but they
 * contain more information.
 */
static Edge *myedges;

/*
 * Current position in building the sort key.
 */
static int_ptrsize nsort_index;

/*
 * Attribute arrays.
 */
static int_ptrsize *nsort_key;

/*
 * Edge constructors
 */
/*Edge::Edge() :
    src(NULL),
    dest(NULL),
    seen(false)
{
    // Nothing else to do
}*/

Edge::Edge(Node *src, Node *dest) :
    seen(false)
{
    /*
     * Maintain the invariant that the src node always has a lower id than the
     * dest node. I'm not sure if this is necessary anymore.
     */
    this->src = (*int_to_node)[MIN(src->id, dest->id)];
    this->dest = (*int_to_node)[MAX(src->id, dest->id)];
}


/*
 * Return the other Node incident to this edge. This is useful for just getting
 * to the other side of an edge.
 */
/*Node *Edge::other(Node *n)
{
    assert(n == src || n == dest);
    if (n == src)
        return dest;
    else
        return src;
}*/

/*
 * Zero out the given attribute array.
 */
/*
static void zero_out(int_ptrsize *attr, int_ptrsize len)
{
    int_ptrsize *ip = attr;
    int_ptrsize *end = attr + len;
    while (ip < end) {
        *ip = 0;
        ip++;
    }
}

/*
 * Read in the edges from the edges array and use them to populate the
 * int_to_node map, the singletons set, and the myedges array.
 */
static void parse_input(int_ptrsize edges[][2], int_ptrsize len)
{
    int_ptrsize i;
    map<int_ptrsize, Node*>::iterator it, iu;
    Node *n;
    Edge *ie, *ig;;

    (void) ie;

    for (i = 0; i < len; ++i) {
        it = int_to_node->find(edges[i][0]);
        if (it == int_to_node->end()) {
            n = new Node();
            n->id = edges[i][0];
            n->seen = false;
            int_to_node->insert(pair<int_ptrsize, Node*>(edges[i][0], n));
            singletons->insert(n);
        } else {
            /* If we've already seen this node then it must have more than one
             * incident edge. */
            singletons->erase(it->second);
        }

        iu = int_to_node->find(edges[i][1]);
        if (iu == int_to_node->end()) {
            n = new Node();
            n->id = edges[i][1];
            n->seen = false;
            int_to_node->insert(pair<int_ptrsize, Node*>(edges[i][1], n));
            singletons->insert(n);
        } else {
            /* If we've already seen this node then it must have more than one
             * incident edge. */
            singletons->erase(iu->second);
        }

        it = int_to_node->find(edges[i][0]);
        iu = int_to_node->find(edges[i][1]);

        assert(it != (*int_to_node).end());
        assert(iu != (*int_to_node).end());

        myedges[i] = Edge(iu->second, it->second);
        it->second->incident_edges.insert(myedges + i);
        iu->second->incident_edges.insert(myedges + i);
    }
}

/*
 * Starting from node n, explore using depth-first search. If sorting is true, continue building the
 * sort key.
 */
static void explore(Node *n,
        bool sorting)
{
    set<Edge *>::iterator it;

    if(sorting && !n->seen){
	nsort_key[nsort_index] = n->id;
	++nsort_index;
    }

    n->seen = true;

    for(it = n->incident_edges.begin(); it != n->incident_edges.end(); ++it) {
        if (!(*it)->other(n)->seen) {
            explore((*it)->other(n), sorting);
        }
    }
}


#ifdef __cplusplus
extern "C" {
#endif
/*
 * * * * * * * * *
 * line_graph_sort
 * * * * * * * * *
 *
 * Input arguments:
 *      int_ptrsize edges[][2]          An array of line segments, represented
 *                                      as indices into xic, yic, and zic.
 *      int *len                        The number of line segments.
 *
 * Output arguments:
 *      int_ptrsize *cid
 *      int_ptrsize *ctype
 *      int_ptrsize *lid
 *      int_ptrsize *skey
 *
 * The output arguments represent three attributes and a sort key, which are
 * described below.
 *
 * Component ID (cid):
 *
 * An integer. Each connected component has a unique value assigned to all of
 * its edges so that edges can be distinguished by the components they reside
 * in. The component ids start from 1.
 *
 * Component Type (ctype):
 *
 * An integer specifying the "type" of the component the edge lives in. The
 * types are:
 * - Polyline -- a series of connected line segments that stretch out in a
 *      single line with no branches
 * - Polygon -- a simple closed loop with no branches
 * - Other -- anything that's not a polyline or polygon
 * See the top of this file for the integer constants representing each of
 * these.
 *
 * Loop ID (lid):
 *
 * A unique integer assigned to each simple cycle without a shared edge. Edges
 * that are not part of a cycle or are in a component with cycles with shared
 * edges receive a default value of 0.
 *
 * Sort key (skey):
 *
 * The function produces a sort key which attempts to arrange the segments in a
 * reasonable order. In particular, it guarantees the following:
 *      o Each connected component will be arranged together.
 *      o Polylines will be in order from one end to the other.
 *      o Polygons will be in order starting from one point and looping back
 *        around.
 * The sorted order for components which are not polylines or polygons is
 * unspecified, but will usually be reasonable because the underlying algorithm
 * uses depth first search.
 *
 * * * * * * * * * * *
 * Notes on Algorithms
 * * * * * * * * * * *
 *
 * The algorithms here rely heavily on depth first search. This makes it easy to
 * separate apart the connected components. In order to get polylines in the
 * right order, we make sure to start exploring "singleton" nodes first. By
 * singleton, I mean a node which has only a single incident edge. This way if
 * there's a polyline we will start at one end and make our way to the other.
 *
 * The cycle detection code relies on building up a spanning tree of each
 * component. Depth first search conveniently does this for us. From there we
 * know that there is a cycle associated with each edge that is not in the
 * spanning tree. So once we know that edge (a, b) is not in the spanning tree,
 * we just find a path from a to b and we have a cycle.
 */

void line_graph_nsort_(int_ptrsize edges[][2],
	int_ptrsize *nskey,
        int_ptrsize *len)
{
    map<int_ptrsize, Node*>::iterator it;
    set<Node *>::iterator sit;
    set<Edge *>::iterator iu;

    nsort_key = nskey;
    nsort_index = 0;

    myedges = new Edge[*len];
    int_to_node = new map<int_ptrsize, Node *>;
    singletons = new set<Node *>;
    /*ordered_nodes = new set<Node *>;*/

    parse_input(edges, *len);

    /* We explore the singletons first so that if there is a polyline we make
     * sure to print it from one end to the other. Otherwise we could end up
     * starting in the middle. */
    for(sit = singletons->begin(); sit != singletons->end(); ++sit) {
        if (!(*sit)->seen) {
            explore(*sit, true);
        }
    }

    /* Explore any remaining non-singletons. */
    for(it = int_to_node->begin(); it != int_to_node->end(); ++it) {
        if (!it->second->seen) {
            explore(it->second, true);
        }
    }


    /* Free allocated memory. */
    for(it = int_to_node->begin(); it != int_to_node->end(); ++it) {
        delete it->second;
    }
    delete[] myedges;
    delete int_to_node;
    delete singletons;
}
#ifdef __cplusplus
}
#endif
