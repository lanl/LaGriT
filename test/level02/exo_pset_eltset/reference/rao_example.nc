netcdf hex_3x3x3_sets {
dimensions:
	len_string = 33 ;
	len_line = 81 ;
	four = 4 ;
	time_step = UNLIMITED ; // (0 currently)
	num_dim = 3 ;
	num_nodes = 64 ;
	num_elem = 27 ;
	num_el_blk = 3 ;
	num_node_sets = 21 ;
	num_side_sets = 21 ;
	num_elem_sets = 3 ;
	num_qa_rec = 1 ;
	num_el_in_blk1 = 9 ;
	num_nod_per_el1 = 8 ;
	num_att_in_blk1 = 1 ;
	num_el_in_blk2 = 9 ;
	num_nod_per_el2 = 8 ;
	num_att_in_blk2 = 1 ;
	num_el_in_blk3 = 9 ;
	num_nod_per_el3 = 8 ;
	num_att_in_blk3 = 1 ;
	num_nod_ns1 = 56 ;
	num_nod_ns2 = 16 ;
	num_nod_ns3 = 16 ;
	num_nod_ns4 = 16 ;
	num_nod_ns5 = 16 ;
	num_nod_ns6 = 16 ;
	num_nod_ns7 = 16 ;
	num_nod_ns8 = 16 ;
	num_nod_ns9 = 8 ;
	num_nod_ns10 = 8 ;
	num_nod_ns11 = 8 ;
	num_nod_ns12 = 8 ;
	num_nod_ns13 = 8 ;
	num_nod_ns14 = 8 ;
	num_nod_ns15 = 8 ;
	num_nod_ns16 = 8 ;
	num_nod_ns17 = 8 ;
	num_nod_ns18 = 8 ;
	num_nod_ns19 = 8 ;
	num_nod_ns20 = 8 ;
	num_nod_ns21 = 16 ;
	num_side_ss1 = 54 ;
	num_side_ss2 = 9 ;
	num_side_ss3 = 9 ;
	num_side_ss4 = 9 ;
	num_side_ss5 = 9 ;
	num_side_ss6 = 9 ;
	num_side_ss7 = 9 ;
	num_side_ss8 = 9 ;
	num_side_ss9 = 3 ;
	num_side_ss10 = 3 ;
	num_side_ss11 = 3 ;
	num_side_ss12 = 3 ;
	num_side_ss13 = 3 ;
	num_side_ss14 = 3 ;
	num_side_ss15 = 3 ;
	num_side_ss16 = 3 ;
	num_side_ss17 = 3 ;
	num_side_ss18 = 3 ;
	num_side_ss19 = 3 ;
	num_side_ss20 = 3 ;
	num_side_ss21 = 9 ;
	num_ele_els1 = 9 ;
	num_ele_els2 = 9 ;
	num_ele_els3 = 9 ;
variables:
	double time_whole(time_step) ;
	int eb_status(num_el_blk) ;
	int eb_prop1(num_el_blk) ;
		eb_prop1:name = "ID" ;
	int ns_status(num_node_sets) ;
	int ns_prop1(num_node_sets) ;
		ns_prop1:name = "ID" ;
	int ss_status(num_side_sets) ;
	int ss_prop1(num_side_sets) ;
		ss_prop1:name = "ID" ;
	int els_status(num_elem_sets) ;
	int els_prop1(num_elem_sets) ;
		els_prop1:name = "ID" ;
	double coordx(num_nodes) ;
	double coordy(num_nodes) ;
	double coordz(num_nodes) ;
	char eb_names(num_el_blk, len_string) ;
	char ns_names(num_node_sets, len_string) ;
	char ss_names(num_side_sets, len_string) ;
	char els_names(num_elem_sets, len_string) ;
	char coor_names(num_dim, len_string) ;
	char qa_records(num_qa_rec, four, len_string) ;
	double attrib1(num_el_in_blk1, num_att_in_blk1) ;
	char attrib_name1(num_att_in_blk1, len_string) ;
	int connect1(num_el_in_blk1, num_nod_per_el1) ;
		connect1:elem_type = "HEX" ;
	double attrib2(num_el_in_blk2, num_att_in_blk2) ;
	char attrib_name2(num_att_in_blk2, len_string) ;
	int connect2(num_el_in_blk2, num_nod_per_el2) ;
		connect2:elem_type = "HEX" ;
	double attrib3(num_el_in_blk3, num_att_in_blk3) ;
	char attrib_name3(num_att_in_blk3, len_string) ;
	int connect3(num_el_in_blk3, num_nod_per_el3) ;
		connect3:elem_type = "HEX" ;
	int node_ns1(num_nod_ns1) ;
	int node_ns2(num_nod_ns2) ;
	int node_ns3(num_nod_ns3) ;
	int node_ns4(num_nod_ns4) ;
	int node_ns5(num_nod_ns5) ;
	int node_ns6(num_nod_ns6) ;
	int node_ns7(num_nod_ns7) ;
	int node_ns8(num_nod_ns8) ;
	int node_ns9(num_nod_ns9) ;
	int node_ns10(num_nod_ns10) ;
	int node_ns11(num_nod_ns11) ;
	int node_ns12(num_nod_ns12) ;
	int node_ns13(num_nod_ns13) ;
	int node_ns14(num_nod_ns14) ;
	int node_ns15(num_nod_ns15) ;
	int node_ns16(num_nod_ns16) ;
	int node_ns17(num_nod_ns17) ;
	int node_ns18(num_nod_ns18) ;
	int node_ns19(num_nod_ns19) ;
	int node_ns20(num_nod_ns20) ;
	int node_ns21(num_nod_ns21) ;
	int elem_ss1(num_side_ss1) ;
	int side_ss1(num_side_ss1) ;
	int elem_ss2(num_side_ss2) ;
	int side_ss2(num_side_ss2) ;
	int elem_ss3(num_side_ss3) ;
	int side_ss3(num_side_ss3) ;
	int elem_ss4(num_side_ss4) ;
	int side_ss4(num_side_ss4) ;
	int elem_ss5(num_side_ss5) ;
	int side_ss5(num_side_ss5) ;
	int elem_ss6(num_side_ss6) ;
	int side_ss6(num_side_ss6) ;
	int elem_ss7(num_side_ss7) ;
	int side_ss7(num_side_ss7) ;
	int elem_ss8(num_side_ss8) ;
	int side_ss8(num_side_ss8) ;
	int elem_ss9(num_side_ss9) ;
	int side_ss9(num_side_ss9) ;
	int elem_ss10(num_side_ss10) ;
	int side_ss10(num_side_ss10) ;
	int elem_ss11(num_side_ss11) ;
	int side_ss11(num_side_ss11) ;
	int elem_ss12(num_side_ss12) ;
	int side_ss12(num_side_ss12) ;
	int elem_ss13(num_side_ss13) ;
	int side_ss13(num_side_ss13) ;
	int elem_ss14(num_side_ss14) ;
	int side_ss14(num_side_ss14) ;
	int elem_ss15(num_side_ss15) ;
	int side_ss15(num_side_ss15) ;
	int elem_ss16(num_side_ss16) ;
	int side_ss16(num_side_ss16) ;
	int elem_ss17(num_side_ss17) ;
	int side_ss17(num_side_ss17) ;
	int elem_ss18(num_side_ss18) ;
	int side_ss18(num_side_ss18) ;
	int elem_ss19(num_side_ss19) ;
	int side_ss19(num_side_ss19) ;
	int elem_ss20(num_side_ss20) ;
	int side_ss20(num_side_ss20) ;
	int elem_ss21(num_side_ss21) ;
	int side_ss21(num_side_ss21) ;
	int elem_els1(num_ele_els1) ;
	int elem_els2(num_ele_els2) ;
	int elem_els3(num_ele_els3) ;

// global attributes:
		:api_version = 4.93f ;
		:version = 4.93f ;
		:floating_point_word_size = 8 ;
		:file_size = 1 ;
		:title = "Lagrit-to-ExodusII" ;
data:

 eb_status = 1, 1, 1 ;

 eb_prop1 = 10000, 20000, 30000 ;

 ns_status = 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ;

 ns_prop1 = 1, 101, 102, 103, 104, 105, 106, 10001, 10002, 20001, 30001, 
    10003, 20002, 30002, 10004, 20003, 30003, 10005, 20004, 30004, 30005 ;

 ss_status = 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ;

 ss_prop1 = 1, 101, 102, 103, 104, 105, 106, 10001, 10002, 20001, 30001, 
    10003, 20002, 30002, 10004, 20003, 30003, 10005, 20004, 30004, 30005 ;

 els_status = 1, 1, 1 ;

 els_prop1 = 1, 2, 3 ;

 coordx = 0, 0.3333333333333, 0.6666666666667, 1, 0, 0.3333333333333, 
    0.6666666666667, 1, 0, 0.3333333333333, 0.6666666666667, 1, 0, 
    0.3333333333333, 0.6666666666667, 1, 0, 0.3333333333333, 0.6666666666667, 
    1, 0, 0.3333333333333, 0.6666666666667, 1, 0, 0.3333333333333, 
    0.6666666666667, 1, 0, 0.3333333333333, 0.6666666666667, 1, 0, 
    0.3333333333333, 0.6666666666667, 1, 0, 0.3333333333333, 0.6666666666667, 
    1, 0, 0.3333333333333, 0.6666666666667, 1, 0, 0.3333333333333, 
    0.6666666666667, 1, 0, 0.3333333333333, 0.6666666666667, 1, 0, 
    0.3333333333333, 0.6666666666667, 1, 0, 0.3333333333333, 0.6666666666667, 
    1, 0, 0.3333333333333, 0.6666666666667, 1 ;

 coordy = 0, 0, 0, 0, 0.3333333333333, 0.3333333333333, 0.3333333333333, 
    0.3333333333333, 0.6666666666667, 0.6666666666667, 0.6666666666667, 
    0.6666666666667, 1, 1, 1, 1, 0, 0, 0, 0, 0.3333333333333, 
    0.3333333333333, 0.3333333333333, 0.3333333333333, 0.6666666666667, 
    0.6666666666667, 0.6666666666667, 0.6666666666667, 1, 1, 1, 1, 0, 0, 0, 
    0, 0.3333333333333, 0.3333333333333, 0.3333333333333, 0.3333333333333, 
    0.6666666666667, 0.6666666666667, 0.6666666666667, 0.6666666666667, 1, 1, 
    1, 1, 0, 0, 0, 0, 0.3333333333333, 0.3333333333333, 0.3333333333333, 
    0.3333333333333, 0.6666666666667, 0.6666666666667, 0.6666666666667, 
    0.6666666666667, 1, 1, 1, 1 ;

 coordz = 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3333333333333, 
    0.3333333333333, 0.3333333333333, 0.3333333333333, 0.3333333333333, 
    0.3333333333333, 0.3333333333333, 0.3333333333333, 0.3333333333333, 
    0.3333333333333, 0.3333333333333, 0.3333333333333, 0.3333333333333, 
    0.3333333333333, 0.3333333333333, 0.3333333333333, 0.6666666666667, 
    0.6666666666667, 0.6666666666667, 0.6666666666667, 0.6666666666667, 
    0.6666666666667, 0.6666666666667, 0.6666666666667, 0.6666666666667, 
    0.6666666666667, 0.6666666666667, 0.6666666666667, 0.6666666666667, 
    0.6666666666667, 0.6666666666667, 0.6666666666667, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1 ;

 eb_names =
  "",
  "",
  "" ;

 ns_names =
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "" ;

 ss_names =
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "" ;

 els_names =
  "",
  "",
  "" ;

 coor_names =
  "",
  "",
  "" ;

 qa_records =
  "mesh3",
  "probname",
  "Today",
  "Time" ;

 attrib1 =
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0 ;

 attrib_name1 =
  "" ;

 connect1 =
  1, 2, 6, 5, 17, 18, 22, 21,
  2, 3, 7, 6, 18, 19, 23, 22,
  3, 4, 8, 7, 19, 20, 24, 23,
  5, 6, 10, 9, 21, 22, 26, 25,
  6, 7, 11, 10, 22, 23, 27, 26,
  7, 8, 12, 11, 23, 24, 28, 27,
  9, 10, 14, 13, 25, 26, 30, 29,
  10, 11, 15, 14, 26, 27, 31, 30,
  11, 12, 16, 15, 27, 28, 32, 31 ;

 attrib2 =
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0 ;

 attrib_name2 =
  "" ;

 connect2 =
  17, 18, 22, 21, 33, 34, 38, 37,
  18, 19, 23, 22, 34, 35, 39, 38,
  19, 20, 24, 23, 35, 36, 40, 39,
  21, 22, 26, 25, 37, 38, 42, 41,
  22, 23, 27, 26, 38, 39, 43, 42,
  23, 24, 28, 27, 39, 40, 44, 43,
  25, 26, 30, 29, 41, 42, 46, 45,
  26, 27, 31, 30, 42, 43, 47, 46,
  27, 28, 32, 31, 43, 44, 48, 47 ;

 attrib3 =
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0 ;

 attrib_name3 =
  "" ;

 connect3 =
  33, 34, 38, 37, 49, 50, 54, 53,
  34, 35, 39, 38, 50, 51, 55, 54,
  35, 36, 40, 39, 51, 52, 56, 55,
  37, 38, 42, 41, 53, 54, 58, 57,
  38, 39, 43, 42, 54, 55, 59, 58,
  39, 40, 44, 43, 55, 56, 60, 59,
  41, 42, 46, 45, 57, 58, 62, 61,
  42, 43, 47, 46, 58, 59, 63, 62,
  43, 44, 48, 47, 59, 60, 64, 63 ;

 node_ns1 = 1, 5, 6, 2, 9, 10, 7, 3, 13, 14, 11, 8, 4, 15, 12, 16, 18, 17, 
    19, 34, 33, 20, 35, 50, 49, 36, 51, 52, 21, 37, 25, 53, 41, 29, 57, 45, 
    61, 24, 28, 40, 32, 44, 56, 48, 60, 64, 30, 46, 31, 62, 47, 63, 54, 55, 
    58, 59 ;

 node_ns2 = 1, 5, 6, 2, 9, 10, 7, 3, 13, 14, 11, 8, 4, 15, 12, 16 ;

 node_ns3 = 1, 2, 18, 17, 3, 19, 34, 33, 4, 20, 35, 50, 49, 36, 51, 52 ;

 node_ns4 = 1, 17, 21, 5, 33, 37, 25, 9, 49, 53, 41, 29, 13, 57, 45, 61 ;

 node_ns5 = 4, 8, 24, 20, 12, 28, 40, 36, 16, 32, 44, 56, 52, 48, 60, 64 ;

 node_ns6 = 14, 13, 29, 30, 45, 46, 15, 31, 61, 62, 47, 16, 32, 63, 48, 64 ;

 node_ns7 = 49, 50, 54, 53, 51, 55, 58, 57, 52, 56, 59, 62, 61, 60, 63, 64 ;

 node_ns8 = 1, 5, 6, 2, 9, 10, 7, 3, 13, 14, 11, 8, 4, 15, 12, 16 ;

 node_ns9 = 1, 2, 18, 17, 3, 19, 4, 20 ;

 node_ns10 = 17, 18, 34, 33, 19, 35, 20, 36 ;

 node_ns11 = 33, 34, 50, 49, 35, 51, 36, 52 ;

 node_ns12 = 1, 17, 21, 5, 25, 9, 29, 13 ;

 node_ns13 = 17, 33, 37, 21, 41, 25, 45, 29 ;

 node_ns14 = 33, 49, 53, 37, 57, 41, 61, 45 ;

 node_ns15 = 4, 8, 24, 20, 12, 28, 16, 32 ;

 node_ns16 = 20, 24, 40, 36, 28, 44, 32, 48 ;

 node_ns17 = 36, 40, 56, 52, 44, 60, 48, 64 ;

 node_ns18 = 14, 13, 29, 30, 15, 31, 16, 32 ;

 node_ns19 = 30, 29, 45, 46, 31, 47, 32, 48 ;

 node_ns20 = 46, 45, 61, 62, 47, 63, 48, 64 ;

 node_ns21 = 49, 50, 54, 53, 51, 55, 58, 57, 52, 56, 59, 62, 61, 60, 63, 64 ;

 elem_ss1 = 1, 4, 2, 7, 5, 3, 8, 6, 9, 1, 2, 10, 3, 11, 19, 12, 20, 21, 1, 
    10, 4, 19, 13, 7, 22, 16, 25, 3, 6, 12, 9, 15, 21, 18, 24, 27, 7, 16, 8, 
    25, 17, 9, 26, 18, 27, 19, 20, 22, 21, 23, 25, 24, 26, 27 ;

 side_ss1 = 5, 5, 5, 5, 5, 5, 5, 5, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 4, 
    4, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 6, 
    6, 6, 6, 6, 6, 6, 6, 6 ;

 elem_ss2 = 1, 4, 2, 7, 5, 3, 8, 6, 9 ;

 side_ss2 = 5, 5, 5, 5, 5, 5, 5, 5, 5 ;

 elem_ss3 = 1, 2, 10, 3, 11, 19, 12, 20, 21 ;

 side_ss3 = 1, 1, 1, 1, 1, 1, 1, 1, 1 ;

 elem_ss4 = 1, 10, 4, 19, 13, 7, 22, 16, 25 ;

 side_ss4 = 4, 4, 4, 4, 4, 4, 4, 4, 4 ;

 elem_ss5 = 3, 6, 12, 9, 15, 21, 18, 24, 27 ;

 side_ss5 = 2, 2, 2, 2, 2, 2, 2, 2, 2 ;

 elem_ss6 = 7, 16, 8, 25, 17, 9, 26, 18, 27 ;

 side_ss6 = 3, 3, 3, 3, 3, 3, 3, 3, 3 ;

 elem_ss7 = 19, 20, 22, 21, 23, 25, 24, 26, 27 ;

 side_ss7 = 6, 6, 6, 6, 6, 6, 6, 6, 6 ;

 elem_ss8 = 1, 4, 2, 7, 5, 3, 8, 6, 9 ;

 side_ss8 = 5, 5, 5, 5, 5, 5, 5, 5, 5 ;

 elem_ss9 = 1, 2, 3 ;

 side_ss9 = 1, 1, 1 ;

 elem_ss10 = 10, 11, 12 ;

 side_ss10 = 1, 1, 1 ;

 elem_ss11 = 19, 20, 21 ;

 side_ss11 = 1, 1, 1 ;

 elem_ss12 = 1, 4, 7 ;

 side_ss12 = 4, 4, 4 ;

 elem_ss13 = 10, 13, 16 ;

 side_ss13 = 4, 4, 4 ;

 elem_ss14 = 19, 22, 25 ;

 side_ss14 = 4, 4, 4 ;

 elem_ss15 = 3, 6, 9 ;

 side_ss15 = 2, 2, 2 ;

 elem_ss16 = 12, 15, 18 ;

 side_ss16 = 2, 2, 2 ;

 elem_ss17 = 21, 24, 27 ;

 side_ss17 = 2, 2, 2 ;

 elem_ss18 = 7, 8, 9 ;

 side_ss18 = 3, 3, 3 ;

 elem_ss19 = 16, 17, 18 ;

 side_ss19 = 3, 3, 3 ;

 elem_ss20 = 25, 26, 27 ;

 side_ss20 = 3, 3, 3 ;

 elem_ss21 = 19, 20, 22, 21, 23, 25, 24, 26, 27 ;

 side_ss21 = 6, 6, 6, 6, 6, 6, 6, 6, 6 ;

 elem_els1 = 1, 2, 3, 4, 5, 6, 7, 8, 9 ;

 elem_els2 = 10, 11, 12, 13, 14, 15, 16, 17, 18 ;

 elem_els3 = 19, 20, 21, 22, 23, 24, 25, 26, 27 ;
}
