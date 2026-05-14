 **read/ zone**
 
 **read/ zonn**
 
 **read/ zone_element**
 
 

  Read in a list of node numbers in FEHM zone or zonn file format (See
  <http://FEHM.lanl.gov). It is assumed that a MO already exists and
  the node numbering of the MO is compatible with the zone/zonn file.
  The only error checking is that the maximum node number in the input
  zone/zonn file is less than or equal to the number of nodes in the
  MO.

  
  The second token zone or zonn does not have any effect other than
  specify that the input file is of the zone/zonn format. The
  zone/zonn option is controled by the value of the zone/zonn keyword
  that is read from inside the input file. If the keyword 'zone' is
  found in the file file\_name, the attribute att\_name is first
  initialized to zero. If the keyword 'zonn' is found, the attribute
  att_name is not initialized.


  If att_name is given as -def-, then the imt array is filled.

  If att_name exits and is type VINT, it is used. If it does not exist, a VINT array is created.

  file_name can be up to 32 characters.
  
  *Note also that the [PSET](http://lagrit.lanl.gov/docs/PSET.md) command can output files in the FEHM zone file format.*

 **FORMAT:**

  **read** / **zone** or **zonn** / file_name / [mo_name or -def- ] / [att_name or -def-]
  
  **read** / **zone_element** / file_name / [mo_name or -def- ] / [att_name or -def-]

  EXAMPLE:

  Read in a mesh and a zone file and create a point set of the nodes in the mesh: 
```
read / gmv / mesh.gmv / cmo
read / zone / node_list.zone / cmo / id_nodes
pset / p_nodes / attribute / id_nodes / 1 0 0 / 0 / ne
```

  Read in a mesh and element based zone file and create element set.
```
read / gmv / mesh.gmv / cmo
read / zone_element / tet_list.zone / cmo / id_tets
eltset / e_sel / attribute / id_tets / 1 0 0 / 0 / ne
```

  **FORMAT FOR ZONE or ZONN FILE**

  The format of a zone file is specified in the FEHM Users Manual
  (<http://FEHM.lanl.gov). The general format is:
 
  1.  An arbitrary number of header lines that do not contain the
      keywords **zone** or **zonn** as the first token on the line.
  2.  The keyword **zone** or **zonn**, lower case letters only.
      &lt;new line&gt;
  3.  An integer zone number identifier and an optional character
      string name &lt;new line&gt;
  4.  The keyword **nnum** &lt;new line&gt;
  5.  An integer with the **number_of_node_numbers** in the list to
      follow &lt;new line&gt;
  6.  A free format list of **number_of_node_numbers** integer node
      identifiers.
  7.  Additional zone lists can be included by repeating again
      starting with step 3. an integer zone number identifier
  8.  zone list reading is terminated when a blank line is reached
      after step 6.
 
  Example Zone File:
 
      # comment line
      another comment line as long as the first word is not zone
      # another comment line. Note that in the list below the node numbers
      # 5 and 10 appear in the first and second list. As a result, if the
      # zone file is read, the second occurence will take precedence.
      zone
      1   first_list_name
      nnum
      3
      1 5 10
      2 second_list_name
      nnum
      10
      1  2  3  4  5
      6  7  8  9  10
      <blank line>
  

  EXAMPLE COMMANDS USING ZONE FILES:

     *
     * LaGriT control file to test read / zonezonn
     *
     *
     cmo / create / cmo / / / tet
     createpts / xyz / 10 10 10 / 0. 0. 0. / 1. 1. 1. / 1 1 1
     cmo / setatt / cmo / imt / 1 0 0 / 1
     cmo / setatt / cmo / imt / 1 100 1 / 2
     cmo / setatt / cmo / imt / 101 200 1 / 3
     *
     * Output zone file test_material.zone
     *
     dump / zone_imt / test / cmo
     *
     cmo / addatt / cmo / itmp1 / vint / scalar / nnodes
     *
     * Test various command line options.
     c
     read / zone / test_material.zone
     read / zone / test_material.zone / cmo
     read / zone / test_material.zone / -def- / -def-
     read / zone / test_material.zone / cmo   / -def-
     read / zone / test_material.zone / cmo   / imt
     read / zone / test_material.zone / cmo   / itmp1
     read / zone / test_material.zone / cmo   / itmp2
     *
     * Create some zone and zonn files to read.
     *
     pset / pzone1 / seq / 1 100 1
     pset / pzone1 / zone / pset_zone1.zone / ascii

     pset / pzonn1 / seq / 1 300 1
     pset / pzonn1 / zonn / pset_zonn1.zonn / ascii
     pset / pzonn2 / seq / 101 300 1
     pset / pzonn2 / zonn / pset_zonn2.zonn / ascii
     *
     * Read zone and zonn type files.
     *
     read / zone / pset_zone1.zone / cmo / itmp4
     *
     * Since zonn files do not first initilize the array to zero
     * one can read in multiple zonn files and only the values of
     * the entries in the list are set.
     *
     read / zonn / pset_zonn1.zonn / cmo / itmp5
     read / zonn / pset_zonn2.zonn / cmo / itmp5
     *
     * Output GMV file.
     *
     dump / gmv / test_read_zone.gmv / cmo
     *
     * Test some error conditions.
     *
     read / zone
     read / zonn
     read / zone / nofile
     read / zone / test_material.zone / nocmo
     *
     finish

