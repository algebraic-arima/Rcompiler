#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Print integer without trailing newline
long printInt(long x) {
    printf("%ld", x);
    return x;
}

// Print integer with trailing newline
long printlnInt(long x) {
    printf("%ld\n", x);
    return x;
}

// Print C-string with trailing newline; returns 0
long printlnStr(const char *s) {
    if (!s) {
        s = "";
    }
    printf("%s\n", s);
    return 0;
}

// Read integer from stdin; returns 0 if input fails
long getInt(void) {
    long v = 0;
    if (scanf("%ld", &v) != 1) {
        v = 0;
    }
    return v;
}

// Exit with given code
void exit_rt(long code) {
    exit((int)code);
}

// Optional string helpers (align with IR expectations)
long stringLength(const char *s) {
    return s ? (long)strlen(s) : 0;
}

long stringEquals(const char *a, const char *b) {
    if (!a || !b) return (a == b) ? 1 : 0;
    return strcmp(a, b) == 0 ? 1 : 0;
}

char *stringConcat(const char *a, const char *b) {
    if (!a) a = "";
    if (!b) b = "";
    size_t lenA = strlen(a);
    size_t lenB = strlen(b);
    char *out = (char *)malloc(lenA + lenB + 1);
    if (!out) return NULL;
    memcpy(out, a, lenA);
    memcpy(out + lenA, b, lenB);
    out[lenA + lenB] = '\0';
    return out;
}
