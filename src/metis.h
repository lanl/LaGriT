/*
 * Copyright 1997, Regents of the University of Minnesota
 *
 * metis.h
 *
 * This file includes all necessary header files
 *
 * Started 8/27/94
 * George
 *
 * $Id: metis.h,v 1.1 1998/11/27 17:59:21 karypis Exp $
 */


#include <stdio.h>
#ifdef __STDC__
#include <stdlib.h>
#else
#include <malloc.h>
#endif

#ifdef _WIN32
#include <string.h>
#include <stdlib.h>
#define drand48() ((double)rand()/RAND_MAX)
#define srand48(x) srand((int)(x))
#else
#include<strings.h>
#endif

#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <time.h>

#ifdef DMALLOC
#include <dmalloc.h>
#endif
/*
#include <metis_defs.h>
#include <metis_struct.h>
#include <metis_macros.h>
#include <metis_rename.h>
#include <metis_proto.h>
 */
#include "metis_defs.h"
#include "metis_struct.h"
#include "metis_macros.h"
#include "metis_rename.h"
#include "metis_proto.h"

