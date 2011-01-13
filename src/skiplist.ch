/*
  ######################################################################

         $Log: skiplist.ch,v $
         Revision 2.00  2007/11/05 19:46:03  spchu
         Import to CVS

 PVCS
 PVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
 PVCS    original version

  ######################################################################
*/

/* This header file contains the definitions for use with the generic
 * SkipList package.
 *
 *      -- THIS CODE IS UNDER THE GNU COPYLEFT --
 *
 *    Dominic Giampaolo (nick@maxine.wpi.edu) 
 */

/**** linux 32 ****/
#ifdef lin
#define FCV_UNDERSCORE
#define SIZEOF_INT 4
#define SIZEOF_LONG 4
#define SIZEOF_VOIDP 4
#endif

/**** linux x64 ****/
#ifdef linx64
#define FCV_UNDERSCORE
#define SIZEOF_INT 4
#define SIZEOF_LONG 8
#define SIZEOF_VOIDP 8
#endif

#if SIZEOF_INT == SIZEOF_VOIDP
#define int_ptrsize int
#elif SIZEOF_LONG == SIZEOF_VOIDP
#define int_ptrsize long
#else
#error "Unknown case for size of pointer."
#endif


#ifndef SKIPLIST_H
#define SKIPLIST_H


/* RAND_MAX should be defined if you are using an ANSI compiler system,
 * but alas it isn't always.  You should define it to be the correct
 * value for whatever your library rand() function returns.
 *
 * Under unix (mach, bsd, etc), that's 2^31 - 1.  On my Amiga at home 
 * it's 2^15 - 1.  It would be wise to verify what your compiler uses
 * for RAND_MAX (the maximum value returned from rand()) because otherwise
 * the code will _not_ work.
 */
#ifndef RAND_MAX 
#define RAND_MAX (2147483648 - 1)
#endif  


#define ALLOW_DUPLICATES  1   /* allow or disallow duplicates in a list */
#define NO_DUPLICATES	  0
#define DUPLICATE_ITEM	 -1   /* ret val from InsertSL if dups not allowed */


/* typedef's */
typedef struct SLNodeStruct *SLNode;

struct SLNodeStruct
{
  void	 *key;
  SLNode  forward[1]; /* variable sized array of forward pointers */
};

typedef struct _SkipList
{
  struct SLNodeStruct *header;	   /* pointer to header */

  int_ptrsize  (*compare)();
  void (*freeitem)();

  int_ptrsize flags;
  int_ptrsize level;			   /* max index+1 of the forward array */

} *SkipList;



/* protos */
SkipList   NewSL(int_ptrsize (*compare)(), void (*freeitem)(), int_ptrsize flags);
void	   FreeSL(SkipList l);
int_ptrsize	   InsertSL(SkipList l, void *key);
int_ptrsize	   DeleteSL(SkipList l, void *key);
void	  *SearchSL(SkipList l, void *key);
void	   DoForSL(SkipList  l, int_ptrsize (*function)(), void *arg);

#endif	/* SKIPLIST_H */
