<font color="red"> red </font = face number (for 3D figures, number of front face is printed below the figure) 

<font color="blue"> blue </font  = node number 

<font color="green"> green </font = edge number (arrow gives edge direction) 

The 3D face ordering is such that the right-hand-normals of all facets point outward. 
 

**Supported Element Types**
 
| Type  | Diagram | Nodes for faces | Edges for face |
| ------------- | ------------- | ------------- | ------------- |
| Node (point) | <img height="100" width="100" src="/assets/images/point.jpg"> | 0 | 0 |
| Line | <img height="100" width="100" src="/assets/images/line.jpg"> | 1 <br> 2 | 0 |
| Triangle | <img height="300" width="300" src="/assets/images/triangle.jpg"> | 2, 3 <br> 3, 1 <br> 1, 2 | 3 <br> 2 <br> 1 |
| Quad | <img height="300" width="300" src="/assets/images/square.jpg"> | 1, 2 <br> 2, 3 <br> 3, 4 <br> 4, 1 | 1 <br> 3 <br> 4 <br>2 |
| Tetrahedron | <img height="300" width="300" src="/assets/images/tet1.jpg"> | 2 ,3, 4 <br> 1, 4, 3 <br> 1, 2, 4 <br> 1, 3, 2 | 6, 5, 4 <br> 6, 2, 3 <br> 5, 3, 1 <br> 4, 1, 2 |
| Pyramid | <img height="300" width="300" src="/assets/images/pyramid.jpg"> | 1, 4, 3, 2 <br> 1, 2, 5 <br> 2, 3, 5 <br> 3, 4, 5 <br> 4, 1, 5 | 2, 6, 4, 1 <br> 1, 5, 3 <br> 4, 7, 5 <br> 6, 8, 7 <br> 2, 3, 8 |
| Prism | <img height="300" width="300" src="/assets/images/prism.jpg"> | 1, 3, 2 <br> 4, 5, 6 <br> 1, 2, 5, 4 <br> 2, 3, 6, 5 <br> 1, 4, 6, 3 | 2, 4, 1 <br> 7, 9, 8 <br> 1, 5, 7, 3 <br> 4, 6, 9, 5 <br> 3, 8, 6, 2 |
| Hexahedron | <img height="300" width="300" src="/assets/images/hex1.jpg"> | 1, 4, 3, 2 <br> 5, 6, 7, 8 <br> 1, 2, 6, 5 <br> 2, 3, 7, 6 <br> 3, 4, 8, 7 <br> 1, 5, 8, 4 | 2, 6, 4, 1 <br> 9, 11, 12, 10 <br> 1, 5, 9, 3 <br> 4, 7 , 11, 5 <br> 6, 8, 12, 7 <br> 3, 10, 8, 2 |
 
