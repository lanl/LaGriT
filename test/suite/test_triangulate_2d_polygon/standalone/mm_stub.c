/* stand-ins for LaGriT's memory manager, for the sandbox only */
#include <stdlib.h>
#include <stdint.h>
void mmgetblk_(char *name, char *sub, void **ptr, int64_t *len,
               int64_t *type, int64_t *ics, long ln, long ls)
{ *ptr = calloc((size_t)(*len > 0 ? *len : 1), 8); *ics = 0; }
void mmrelprt_(char *sub, int64_t *ics, long ls) { *ics = 0; }
