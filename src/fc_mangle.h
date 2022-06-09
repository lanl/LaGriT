#ifndef FortranCInterface_HEADER_INCLUDED
#define FortranCInterface_HEADER_INCLUDED

/* Mangling for Fortran global symbols without underscores. */
#define FortranCInterface_GLOBAL(name,NAME) name##_

/* Mangling for Fortran global symbols with underscores. */
#define FortranCInterface_GLOBAL_(name,NAME) name##_

/* Mangling for Fortran module symbols without underscores. */
#define FortranCInterface_MODULE(mod_name,name, mod_NAME,NAME) __##mod_name##_MOD_##name

/* Mangling for Fortran module symbols with underscores. */
#define FortranCInterface_MODULE_(mod_name,name, mod_NAME,NAME) __##mod_name##_MOD_##name

/*--------------------------------------------------------------------------*/
/* Mangle some symbols automatically.                                       */
#define INITLAGRIT FortranCInterface_GLOBAL(initlagrit, INITLAGRIT)
#define DOTASK FortranCInterface_GLOBAL(dotask, DOTASK)
#define CMO_GET_NAME FortranCInterface_GLOBAL_(cmo_get_name, CMO_GET_NAME)
#define CMO_GET_INFO FortranCInterface_GLOBAL_(cmo_get_info, CMO_GET_INFO)
#define CMO_GET_INTINFO FortranCInterface_GLOBAL_(cmo_get_intinfo, CMO_GET_INTINFO)
#define FC_CMO_GET_INT FortranCInterface_GLOBAL_(fc_cmo_get_int, FC_CMO_GET_INT)
#define FC_CMO_GET_VINT FortranCInterface_GLOBAL_(fc_cmo_get_vint, FC_CMO_GET_VINT)
#define FC_CMO_GET_DOUBLE FortranCInterface_GLOBAL_(fc_cmo_get_double, FC_CMO_GET_DOUBLE)
#define FC_CMO_GET_VDOUBLE FortranCInterface_GLOBAL_(fc_cmo_get_vdouble, FC_CMO_GET_VDOUBLE)
#define FPASS_TYPES FortranCInterface_GLOBAL_(fpass_types, FPASS_TYPES)
#define INSIDE_TET FortranCInterface_GLOBAL_(inside_tet, INSIDE_TET)
#define LINESEG_TRI FortranCInterface_GLOBAL_(lineseg_tri, LINESEG_TRI)

#endif
