# Supported Element Types

The following are numbering conventions used for each element type.
The 3D face ordering is such that the right-hand-normals of all facets point outward. 
 
<font color="red"> red </font> = face number (for 3D figures, number of front face is printed below the figure) 

<font color="blue"> blue </font>  = node number 

<font color="green"> green </font> = edge number (arrow gives edge direction) 


### point (also known as node, vertex, or atom)

| Node Order | Nodes for faces | Edges for face |
| :--- | :--- | :--- |
| 1 | 0 | 0 |

<img width="100" src="https://lanl.github.io/LaGriT/assets/images/point.jpg">

### line
| Node Order | Nodes for faces | Edges for face |
| :--- | :--- | :--- |
| 1 2 | 1<br>2 | 0 |

<img width="120" src="https://lanl.github.io/LaGriT/assets/images/line.jpg">

### tri
| Node Order | Nodes for faces | Edges for face |
| :--- | :--- | :--- |
| 1 2 3 | 2, 3<br>3, 1<br>1, 2 | 3<br>2<br>1 |

<img width="280" src="https://lanl.github.io/LaGriT/assets/images/triangle.jpg">

### quad

| Node Order | Nodes for faces | Edges for face |
| :--- | :--- | :--- |
| 1 2 3 4 | 1, 2<br>2, 3<br>3, 4<br>4, 1 | 1<br>3<br>4 <br>2 |

<img width="280" src="https://lanl.github.io/LaGriT/assets/images/square.jpg">


### tet

| Node Order | Nodes for faces | Edges for face |
| :--- | :--- | :--- |
| 1 2 3 4 | 2 ,3, 4<br>1, 4, 3<br>1, 2, 4<br>1, 3, 2 | 6, 5, 4<br>6, 2, 3<br>5, 3, 1<br>4, 1, 2 |

<img width="300" src="https://lanl.github.io/LaGriT/assets/images/tet1.jpg">

### pyr

| Node Order | Nodes for faces | Edges for face |
| :--- | :--- | :--- |
| 1 2 3 4 5 | 1, 4, 3, 2<br>1, 2, 5<br>2, 3, 5<br>3, 4, 5<br>4, 1, 5 | 2, 6, 4, 1<br>1, 5, 3<br>4, 7, 5<br>6, 8, 7<br>2, 3, 8 |

<img width="320" src="https://lanl.github.io/LaGriT/assets/images/pyramid.jpg"> 

### pri

| Node Order | Nodes for faces | Edges for face |
| :--- | :--- | :--- |
| 1 2 3 4 5 6 | 1, 3, 2<br>4, 5, 6<br>1, 2, 5, 4<br>2, 3, 6, 5<br>1, 4, 6, 3 | 2, 4, 1<br>7, 9, 8<br>1, 5, 7, 3<br>4, 6, 9, 5<br>3, 8, 6, 2 |

<img width="300" src="https://lanl.github.io/LaGriT/assets/images/prism.jpg">

### hex

| Node Order | Nodes for faces | Edges for face |
| :--- | :--- | :--- |
| 1 2 3 4<br>5 6 7 8 | 1, 4, 3, 2<br>5, 6, 7, 8<br>1, 2, 6, 5<br>2, 3, 7, 6<br>3, 4, 8, 7<br>1, 5, 8, 4 | 2, 6, 4, 1<br>9, 11, 12, 10<br>1, 5, 9, 3<br>4, 7 , 11, 5<br>6, 8, 12, 7<br>3, 10, 8, 2 |

<img width="330" src="https://lanl.github.io/LaGriT/assets/images/hex1.jpg">


