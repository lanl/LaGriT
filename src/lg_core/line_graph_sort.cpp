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
    bool in_spanning_tree;
    bool seen;

    Edge();
    bool operator()(Edge *e, Edge *f);
    Edge(Node *src, Node *dest, bool in_spanning_tree);
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
void line_graph_sort_(
        int_ptrsize edges[][2],
        int_ptrsize *component_id,
        int_ptrsize *component_type,
        int_ptrsize *loop_id,
        int_ptrsize *sort_key,
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

#define POLYLINE        1
#define TREE            2
#define POLYGON         3
#define SHARED_EDGES    4
#define OTHER           5

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
static int_ptrsize sort_index;

/*
 * Attribute arrays.
 */
static int_ptrsize *component_id;
static int_ptrsize *component_type;
static int_ptrsize *loop_id;
static int_ptrsize *sort_key;

/*
 * The ID we will assign to the next loop we find.
 */
static int_ptrsize current_loop_id;

/*
 * Edge constructors
 */
Edge::Edge() :
    src(NULL),
    dest(NULL),
    in_spanning_tree(false),
    seen(false)
{
    // Nothing else to do
}

Edge::Edge(Node *src, Node *dest, bool in_spanning_tree) :
    in_spanning_tree(in_spanning_tree),
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
Node *Edge::other(Node *n)
{
    assert(n == src || n == dest);
    if (n == src)
        return dest;
    else
        return src;
}

/*
 * Zero out the given attribute array.
 */
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

        myedges[i] = Edge(iu->second, it->second, false);
        it->second->incident_edges.insert(myedges + i);
        iu->second->incident_edges.insert(myedges + i);
    }
}

/*
 * Change all the seen flags reachable from node n back to false. Note that this
 * function assumes that all the seen flags in the component are currently true.
 */
static void reset_seen_flags(Node *n)
{
    set<Edge *>::iterator it;
    n->seen = false;

    for(it = n->incident_edges.begin(); it != n->incident_edges.end(); ++it) {
        if ((*it)->other(n)->seen) {
            reset_seen_flags((*it)->other(n));
        }
    }
}

/*
 * Starting from node n, explore using depth-first search. Also set the
 * component_id and component_type attributes for the visited nodes to the
 * values specified in the arguments. If sorting is true, continue building the
 * sort key. This function returns the number of back edges it has seen.
 */
static bool explore(Node *n,
        Node *parent,
        set<Edge *>& cycle_edges,
        int_ptrsize current_component,
        int_ptrsize current_type,
        bool sorting)
{
    set<Edge *>::iterator it;

    n->seen = true;
    bool no_branches = (n->incident_edges.size() <= 2);

    /* We need to count these back edges before recursing, because the recursive
     * calls may visit some of these adjacent nodes. */
    for(it = n->incident_edges.begin(); it != n->incident_edges.end(); ++it) {
        if ((*it)->other(n)->seen && (*it)->other(n) != parent) {
            cycle_edges.insert(*it);
        }
    }

    for(it = n->incident_edges.begin(); it != n->incident_edges.end(); ++it) {
        component_id[*it - myedges] = current_component;
        component_type[*it - myedges] = current_type;

        /* If we haven't seen this edge yet, add it to the sort key. */
        if (sorting && !(*it)->seen) {
            /* Add 1 because Fortran counts from 1. */
            sort_key[sort_index] = (*it - myedges) + 1;
            ++sort_index;
            (*it)->seen = true;
        }

        if (!(*it)->other(n)->seen) {
            (*it)->in_spanning_tree = true;
            no_branches = explore((*it)->other(n), n, cycle_edges,
                    current_component, current_type, sorting) && no_branches;
        }
    }

    return no_branches;
}

/*
 * Set the loop_id to zero throughout the component containing node n.
 */
static void reset_loop_id(Node *n, set<Node *>& seen)
{
    set<Edge *>::iterator it;
    set<Node *>::iterator iu;

    seen.insert(n);

    for (it = n->incident_edges.begin(); it != n->incident_edges.end(); ++it) {
        loop_id[*it - myedges] = 0;
        iu = seen.find((*it)->other(n));
        if (iu == seen.end()) {
            reset_loop_id((*it)->other(n), seen);
        }
    }
}

/*
 * Find a path through the spanning tree from m to n. This function returns true
 * if it succeeds and false otherwise. It also assigns a loop_id if it
 * successfully finds a path.
 */
static bool find_path(Node *m, Node *n, set<Node *> seen,
        bool& found_shared_edge)
{
    set<Edge *>::iterator it;
    set<Node *>::iterator iu;

    seen.insert(m);

    if (m == n)
        return true;

    for (it = m->incident_edges.begin(); it != m->incident_edges.end(); ++it) {
        iu = seen.find((*it)->other(m));
        if (iu == seen.end() && (*it)->in_spanning_tree) {
            if (find_path((*it)->other(m), n, seen, found_shared_edge)) {
                if (loop_id[*it - myedges] != 0)
                    found_shared_edge = true;
                loop_id[*it - myedges] = current_loop_id;
                return true;
            }
        }
    }

    return false;
}

/*
 * Deal with the connected component containing Node n. current_component is the
 * id that should be assigned to this component.
 */
static void handle_component(
        Node *n,
        int_ptrsize current_component,
        bool is_singleton)
{
    set<Edge *> cycle_edges;
    set<Edge *>::iterator it;
    bool found_shared_edge;

    bool no_branches = explore(n, NULL, cycle_edges, current_component, 0,
            true);
    int_ptrsize nbackedges = cycle_edges.size();

    if (is_singleton && no_branches) {
        /* Mark this component as a polyline. */
        reset_seen_flags(n);
        explore(n, NULL, cycle_edges, current_component, POLYLINE, false);
    } else if (nbackedges == 0) {
        /* Mark this component as a tree. */
        reset_seen_flags(n);
        explore(n, NULL, cycle_edges, current_component, TREE, false);
    } else if (!is_singleton && nbackedges == 1) {
        /* Mark this component as a polygon. */
        reset_seen_flags(n);
        explore(n, NULL, cycle_edges, current_component, POLYGON, false);
    } else {
        /* Mark this component as type "other". */
        reset_seen_flags(n);
        explore(n, NULL, cycle_edges, current_component, OTHER, false);
    }

    for (it = cycle_edges.begin(); it != cycle_edges.end(); ++it) {
        set<Node *> seen;
        found_shared_edge = false;
        assert(find_path((*it)->src, (*it)->dest, seen, found_shared_edge));
        loop_id[*it - myedges] = current_loop_id;

        if (found_shared_edge) {
            /* Mark this component as type "shared edges". */
            reset_seen_flags(n);
            explore(n, NULL, cycle_edges, current_component, SHARED_EDGES,
                    false);
        }

        ++current_loop_id;
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

void line_graph_sort_(int_ptrsize edges[][2],
        int_ptrsize *cid,
        int_ptrsize *ctype,
        int_ptrsize *lid,
        int_ptrsize *skey,
        int_ptrsize *len)
{
    map<int_ptrsize, Node*>::iterator it;
    set<Node *>::iterator sit;
    set<Edge *>::iterator iu;

    int_ptrsize current_component = 1;
    component_id = cid;
    component_type = ctype;
    loop_id = lid;
    sort_key = skey;
    sort_index = 0;
    current_loop_id = 1;

    myedges = new Edge[*len];
    int_to_node = new map<int_ptrsize, Node *>;
    singletons = new set<Node *>;

    zero_out(loop_id, *len);

    parse_input(edges, *len);

    /* We explore the singletons first so that if there is a polyline we make
     * sure to print it from one end to the other. Otherwise we could end up
     * starting in the middle. */
    for(sit = singletons->begin(); sit != singletons->end(); ++sit) {
        if (!(*sit)->seen) {
            handle_component(*sit, current_component, true);
            ++current_component;
        }
    }

    /* Explore any remaining non-singletons. */
    for(it = int_to_node->begin(); it != int_to_node->end(); ++it) {
        if (!it->second->seen) {
            handle_component(it->second, current_component, false);
            ++current_component;
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
