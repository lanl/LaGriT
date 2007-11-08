*cd,local_element
C ######################################################################
C
C     PURPOSE -
C
C       Declarations for local element information
C
C     CHANGE HISTORY -
C
C  $Log: local_element.h,v $
C  Revision 2.00  2007/11/05 19:46:00  spchu
C  Import to CVS
C  
CPVCS    
CPVCS       Rev 1.7   Tue Oct 26 14:07:58 1999   dcg
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.6   Tue Jun 29 13:55:04 1999   jtg
CPVCS    additional local element data structures added (ielmedge2,
CPVCS    ielmface3,ielmface4,ielmnode0,ielmnode1,ielmnode2,ielmnode3)
CPVCS    
CPVCS       Rev 1.5   Thu Jan 21 20:57:42 1999   jtg
CPVCS    common blocks moved after declarations and/or saves added
C ######################################################################
C
      integer nelmtypes, 
     *        maxnen, 
     *        maxnef,
     *        maxnee1, 
     *        maxnee2
      parameter ( nelmtypes=10, 
     *            maxnen=10, 
     *            maxnef=10,
     *            maxnee1=4, 
     *            maxnee2=12 )
C
      character*32 celmnames(nelmtypes)
      common / c_local_element_data / celmnames
      save / c_local_element_data /
C
      integer nelmnen(nelmtypes),
     *        nelmnef(nelmtypes),
     *        nelmnee(nelmtypes),
     *        ielmface0(maxnef,nelmtypes),
     *        ielmface1(maxnee1,maxnef,nelmtypes),
     *        ielmface2(maxnee1,maxnef,nelmtypes),
     *        ielmedge1(2,maxnee2,nelmtypes)
      integer ifelmpnt,
     *        ifelmlin,
     *        ifelmtri,
     *        ifelmqud,
     *        ifelmtet,
     *        ifelmpyr,
     *        ifelmpri,
     *        ifelmhex,
     *        ifelmhyb,
     *        ifelmply
C
      common / i_local_element_data / nelmnen,
     *                                nelmnef,
     *                                nelmnee,
     *                                ielmface0,
     *                                ielmface1,
     *                                ielmface2,
     *                                ielmedge1,
     *                                ifelmpnt,
     *                                ifelmlin,
     *                                ifelmtri,
     *                                ifelmqud,
     *                                ifelmtet,
     *                                ifelmpyr,
     *                                ifelmpri,
     *                                ifelmhex,
     *                                ifelmhyb,
     *                                ifelmply
      save / i_local_element_data /
C
C ######################################################################
C additional data structures for the local elements:
C see explanations in blockcom.f
C
      integer ielmedge2(2,maxnee2,nelmtypes)
     *       ,ielmface3(maxnef,nelmtypes)
     *       ,ielmface4(maxnee1,maxnef,nelmtypes)
     *       ,ielmnode0(maxnen,nelmtypes)
     *       ,ielmnode1(maxnee1,maxnen,nelmtypes)
     *       ,ielmnode2(maxnee1,maxnen,nelmtypes)
     *       ,ielmnode3(maxnee1,maxnen,nelmtypes)

      common / iadd_local_element_data / ielmedge2
     *       ,ielmface3,ielmface4
     *       ,ielmnode0,ielmnode1,ielmnode2,ielmnode3
      save / iadd_local_element_data /
C
C ######################################################################


