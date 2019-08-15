# Supported Element Types

The following element types are recognized by LaGriT. Though underlying code supports hybrid or polygon shaped elements, they are not supported by most the commands. The command pages indicate limitations on element types as they apply.

- point (pnt) (also known as node or vertex)
- line (lin)
- triangle (tri)
- quadrilateral (qud)
- tetrahedron (tet)
- pyramid (pyr)
- prism (pri)
- hexahedron (hex)
- hybrid (hyb)
- polygon (ply)


The following are the numbering conventions used by LaGriT for each element type.
The 3D face ordering is such that the right-hand-normals of all facets point outward. 
 
<font color="red">* red </font>is the face number (for 3D figures, number of front face is printed below the figure) 
<font color="blue">* blue </font>is the node number on the element vertices 
<font color="green">* green </font>is the edge number (arrow gives edge direction) 


## point

| Node Order | Nodes for faces | Edges for face |
| :--------- | :-------------- | :------------- |
| 1          | 0               | 0              |

<img width="100" src="https://lanl.github.io/LaGriT/assets/images/point.jpg">

## line

| Node Order | Nodes for faces | Edges for face |
| :--------- | :-------------- | :------------- |
| 1 2        | 1<br>2          | 0              |

<img width="120" src="https://lanl.github.io/LaGriT/assets/images/line.jpg">

## triangle

| Node Order | Nodes for faces | Edges for face |
| :--------- | :-------------- | :------------- |
| 1 2 3      | 2, 3<br>3, 1<br>1, 2 | 3<br>2<br>1 |

<img width="280" src="https://lanl.github.io/LaGriT/assets/images/triangle.jpg">

## quadrilateral

| Node Order | Nodes for faces | Edges for face |
| :--------- | :-------------- | :------------- |
| 1 2 3 4 | 1, 2<br>2, 3<br>3, 4<br>4, 1 | 1<br>3<br>4 <br>2 |

<img width="270" src="https://lanl.github.io/LaGriT/assets/images/square.jpg">


## tetrahedron

| Node Order | Nodes for faces | Edges for face |
| :--------- | :-------------- | :------------- |
| 1 2 3 4 | 2 ,3, 4<br>1, 4, 3<br>1, 2, 4<br>1, 3, 2 | 6, 5, 4<br>6, 2, 3<br>5, 3, 1<br>4, 1, 2 |

<img width="310" src="https://lanl.github.io/LaGriT/assets/images/tet1.jpg">


## pyramid

| Node Order | Nodes for faces | Edges for face |
| :--------- | :-------------- | :------------- |
| 1 2 3 4 5 | 1, 4, 3, 2<br>1, 2, 5<br>2, 3, 5<br>3, 4, 5<br>4, 1, 5 | 2, 6, 4, 1<br>1, 5, 3<br>4, 7, 5<br>6, 8, 7<br>2, 3, 8 |

<img width="320" src="https://lanl.github.io/LaGriT/assets/images/pyramid.jpg"> 


## prism

| Node Order | Nodes for faces | Edges for face |
| :--------- | :-------------- | :------------- |
| 1 2 3 4 5 6 | 1, 3, 2<br>4, 5, 6<br>1, 2, 5, 4<br>2, 3, 6, 5<br>1, 4, 6, 3 | 2, 4, 1<br>7, 9, 8<br>1, 5, 7, 3<br>4, 6, 9, 5<br>3, 8, 6, 2 |


<img width="300" src="https://lanl.github.io/LaGriT/assets/images/prism.jpg">

## hexahedron

| Node Order | Nodes for faces | Edges for face |
| :--------- | :-------------- | :------------- |
| 1 2 3 4<br>5 6 7 8 | 1, 4, 3, 2<br>5, 6, 7, 8<br>1, 2, 6, 5<br>2, 3, 7, 6<br>3, 4, 8, 7<br>1, 5, 8, 4 | 2, 6, 4, 1<br>9, 11, 12, 10<br>1, 5, 9, 3<br>4, 7 , 11, 5<br>6, 8, 12, 7<br>3, 10, 8, 2 |

<img width="340" src="https://lanl.github.io/LaGriT/assets/images/hex1.jpg">




