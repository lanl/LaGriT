#ifndef TYPE_SIZES_H_INCLUDED
#define TYPE_SIZES_H_INCLUDED

/*  LaGriT assumes that the size of an integer is the same size as a
 *  pointer.  Use the preprocessor and configure settings to select
 *  the integer type so that it matches the size of a pointer.
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

/**** Mac 32 ****/
#ifdef mac
#define SIZEOF_INT 4
#define SIZEOF_LONG 4
#define SIZEOF_VOIDP 4
#endif

/**** Mac x64 ****/
#ifdef macx64
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

#endif /* TYPE_SIZES_H_INCLUDED */
