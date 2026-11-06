/*
 * avs_fast.c: fast number reading and writing for read / avs and
 * dump / avs (readavs.f, dumpavs.f).
 *
 * Reading: the integers and reals of one line, without Fortran's
 * list-directed read. The reals go through strtod, which is what
 * libgfortran's list-directed read uses, so the values are the same to
 * the bit. Anything these do not read plainly (commas, slashes, an
 * exponent without E, NaN, overflow, ...) returns ierr = 1, and the
 * caller reads the line with Fortran's list-directed read as before.
 *
 * Writing: the number fields dump / avs writes, character for
 * character as Fortran writes them (1pe20.12 and 1pe19.12, e20.12,
 * iN, iN.N), from snprintf, which rounds as libgfortran does. A value
 * Fortran would write differently (not finite, too wide for its field)
 * returns ok = 0, and the caller writes that line with Fortran's
 * formatted write as before.
 *
 * Called from Fortran (gfortran, -fdefault-integer-8): integers are
 * int64_t, names end in an underscore, a character argument's length
 * comes last as size_t.
 *
 * Nov 2026. test/suite/test_avs_io checks that the files written and
 * the meshes read are byte for byte those of the Fortran code.
 */
#include <errno.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ------------------------------------------------------------------
 * Reading
 */

static int is_blank(char c) { return c == ' ' || c == '\t' || c == '\r'; }

/* count integers from s(pos:n) (1-based pos, updated) into vals */
void avs_get_ints_(const char *s, const int64_t *n, int64_t *pos,
                   const int64_t *count, int64_t *vals, int64_t *ierr,
                   size_t slen)
{
    int64_t i = *pos - 1, k, nn = *n;
    (void) slen;
    *ierr = 0;
    for (k = 0; k < *count; k++) {
        int neg = 0, nd = 0;
        int64_t v = 0;
        while (i < nn && is_blank(s[i])) i++;
        if (i >= nn) { *ierr = 1; return; }
        if (s[i] == '+' || s[i] == '-') { neg = (s[i] == '-'); i++; }
        if (i >= nn || s[i] < '0' || s[i] > '9') { *ierr = 1; return; }
        while (i < nn && s[i] >= '0' && s[i] <= '9') {
            v = 10 * v + (s[i] - '0');
            i++;
            if (++nd > 18) { *ierr = 1; return; }
        }
        if (i < nn && !is_blank(s[i])) { *ierr = 1; return; }
        vals[k] = neg ? -v : v;
    }
    *pos = i + 1;
}

/* count reals from s(pos:n) (1-based pos, updated) into vals */
void avs_get_reals_(const char *s, const int64_t *n, int64_t *pos,
                    const int64_t *count, double *vals, int64_t *ierr,
                    size_t slen)
{
    int64_t i = *pos - 1, j, k, nn = *n;
    char buf[64];
    (void) slen;
    *ierr = 0;
    for (k = 0; k < *count; k++) {
        int64_t len, m;
        char *end;
        double v;
        while (i < nn && is_blank(s[i])) i++;
        if (i >= nn) { *ierr = 1; return; }
        j = i;
        while (j < nn && !is_blank(s[j])) j++;
        len = j - i;
        if (len < 1 || len > 60) { *ierr = 1; return; }
        for (m = 0; m < len; m++) {
            char c = s[i + m];
            if (c == 'd' || c == 'D') c = 'e';
            else if (!((c >= '0' && c <= '9') || c == '.' || c == '+' ||
                       c == '-' || c == 'e' || c == 'E')) {
                *ierr = 1;
                return;
            }
            buf[m] = c;
        }
        buf[len] = '\0';
        errno = 0;
        v = strtod(buf, &end);
        if (end != buf + len || errno != 0) { *ierr = 1; return; }
        vals[k] = v;
        i = j;
    }
    *pos = i + 1;
}

/* ------------------------------------------------------------------
 * Writing
 */

/* Fortran's exponent field: E+dd for |e| < 100, +ddd up to 999 */
static int fexp(char *p, int e)
{
    int a = e < 0 ? -e : e;
    if (a < 100) return sprintf(p, "E%c%02d", e < 0 ? '-' : '+', a);
    if (a < 1000) return sprintf(p, "%c%03d", e < 0 ? '-' : '+', a);
    return -1;
}

/* x as Fortran's 1pEw.d (d digits after the point, one before),
 * right-justified in w; 0 if Fortran would write it otherwise */
static int f1pe(char *out, double x, int w, int d)
{
    char t[64], f[64];
    char *e;
    int ex, n, k;
    if (!isfinite(x)) return 0;
    if (x == 0.0) x = 0.0;                  /* -0.0: -fno-sign-zero */
    snprintf(t, sizeof t, "%.*E", d, x);    /* -1.234567890123E-03 */
    e = strchr(t, 'E');
    if (!e) return 0;
    ex = atoi(e + 1);
    *e = '\0';
    n = sprintf(f, "%s", t);
    k = fexp(f + n, ex);
    if (k < 0) return 0;
    n += k;
    if (n > w) return 0;
    memset(out, ' ', (size_t) (w - n));
    memcpy(out + (w - n), f, (size_t) n);
    return 1;
}

/* x as Fortran's Ew.d with no scale factor: 0.ddd...E+ee */
static int f0pe(char *out, double x, int w, int d)
{
    char t[64], f[64];
    char *e, *p;
    int ex, n = 0, k;
    if (!isfinite(x)) return 0;
    if (x == 0.0) {
        x = 0.0;
        ex = 0;
        f[n++] = '0';
        f[n++] = '.';
        for (k = 0; k < d; k++) f[n++] = '0';
    } else {
        snprintf(t, sizeof t, "%.*E", d - 1, x);   /* -1.04719755120E-02 */
        e = strchr(t, 'E');
        if (!e) return 0;
        ex = atoi(e + 1) + 1;
        *e = '\0';
        p = t;
        if (*p == '-') f[n++] = *p++;
        f[n++] = '0';
        f[n++] = '.';
        for (; *p; p++)
            if (*p != '.') f[n++] = *p;
    }
    k = fexp(f + n, ex);
    if (k < 0) return 0;
    n += k;
    if (n > w) return 0;
    memset(out, ' ', (size_t) (w - n));
    memcpy(out + (w - n), f, (size_t) n);
    return 1;
}

/* i as Fortran's Iw (zero = 0) or Iw.w (zero = 1: zero padded) */
static int fint(char *out, int64_t i, int w, int zero)
{
    char t[32];
    int n;
    if (zero) {
        if (i < 0) return 0;
        n = snprintf(t, sizeof t, "%0*lld", w, (long long) i);
    } else {
        n = snprintf(t, sizeof t, "%*lld", w, (long long) i);
    }
    if (n != w) return 0;
    memcpy(out, t, (size_t) n);
    return 1;
}

/* A vertex line: iopt 1: i as Iw.w, 1x, 3 x 1pe20.12; iopt 2: the
 * three coordinates only. The line is line(1:*nout). */
void avs_put_vertex_(const int64_t *iopt, const int64_t *i,
                     const int64_t *w, const double *xyz, char *line,
                     int64_t *nout, int64_t *ok, size_t llen)
{
    int64_t n = 0;
    int k;
    *ok = 0;
    if ((size_t) (*w + 1 + 60) > llen) return;
    if (*iopt == 1) {
        if (!fint(line, *i, (int) *w, 1)) return;
        n = *w;
        line[n++] = ' ';
    }
    for (k = 0; k < 3; k++) {
        if (!f1pe(line + n, xyz[k], 20, 12)) return;
        n += 20;
    }
    *nout = n;
    *ok = 1;
}

/* A cell line, as format (Iw.w,1x,i8,1x,a5,8(1x,Iv)) (iopt 1) or
 * (1x,i8,1x,a5,8(1x,Iv)) (iopt 2) writes it. */
void avs_put_cell_(const int64_t *iopt, const int64_t *it,
                   const int64_t *w, const int64_t *imat,
                   const char *cname, const int64_t *lname,
                   const int64_t *nvert, const int64_t *ivert,
                   const int64_t *v, char *line, int64_t *nout,
                   int64_t *ok, size_t clen, size_t llen)
{
    int64_t n = 0, k;
    (void) clen;
    *ok = 0;
    if ((size_t) (*w + 16 + 8 * (*v + 1)) > llen || *lname > 5) return;
    if (*iopt == 1) {
        if (!fint(line, *it, (int) *w, 1)) return;
        n = *w;
    }
    line[n++] = ' ';
    if (!fint(line + n, *imat, 8, 0)) return;
    n += 8;
    line[n++] = ' ';
    memset(line + n, ' ', (size_t) (5 - *lname));
    memcpy(line + n + (5 - *lname), cname, (size_t) *lname);
    n += 5;
    for (k = 0; k < *nvert; k++) {
        line[n++] = ' ';
        if (!fint(line + n, ivert[k], (int) *v, 0)) return;
        n += *v;
    }
    *nout = n;
    *ok = 1;
}

/* An avs1 attribute row: (i10.10,100(1x,1pe19.12)) (iopt 1) or
 * (100(1pe19.12,1x)) (iopt 2), at most 100 values. */
void avs_put_row1_(const int64_t *iopt, const int64_t *irow,
                   const int64_t *nval, const double *vals, char *line,
                   int64_t *nout, int64_t *ok, size_t llen)
{
    int64_t n = 0, k;
    *ok = 0;
    if (*nval > 100 || (size_t) (11 + 20 * (*nval)) > llen) return;
    if (*iopt == 1) {
        if (!fint(line, *irow, 10, 1)) return;
        n = 10;
        for (k = 0; k < *nval; k++) {
            line[n++] = ' ';
            if (!f1pe(line + n, vals[k], 19, 12)) return;
            n += 19;
        }
    } else {
        /* (100(1pe19.12,1x)): the 1x after the last value writes
         * nothing (Fortran does not extend a record with X) */
        for (k = 0; k < *nval; k++) {
            if (k > 0) line[n++] = ' ';
            if (!f1pe(line + n, vals[k], 19, 12)) return;
            n += 19;
        }
    }
    *nout = n;
    *ok = 1;
}

/* An avs2 attribute row, as string_add_entry_r8 builds it: the row
 * number as I(nrow) (if nrow > 0), then each value as I(nint(k))
 * (itype 1, nint of the real) or E20.12 (itype 2). */
void avs_put_row2_(const int64_t *nrow, const int64_t *irow,
                   const int64_t *nval, const double *vals,
                   const int64_t *itype, const int64_t *nint_w,
                   char *line, int64_t *nout, int64_t *ok, size_t llen)
{
    int64_t n = 0, k;
    *ok = 0;
    if (*nrow > 0) {
        if ((size_t) *nrow > llen) return;
        if (!fint(line, *irow, (int) *nrow, 0)) return;
        n = *nrow;
    }
    for (k = 0; k < *nval; k++) {
        if (itype[k] == 1) {
            double r = vals[k];
            if (!isfinite(r) || fabs(r) > 9.0e17) return;
            if ((size_t) (n + nint_w[k]) > llen) return;
            /* Fortran's NINT: halves away from zero (llround) */
            if (!fint(line + n, (int64_t) llround(r), (int) nint_w[k], 0))
                return;
            n += nint_w[k];
        } else if (itype[k] == 2) {
            if ((size_t) (n + 20) > llen) return;
            if (!f0pe(line + n, vals[k], 20, 12)) return;
            n += 20;
        } else {
            return;
        }
    }
    *nout = n;
    *ok = 1;
}
