#ifndef STRL_H_
#define STRL_H_

#include <sys/types.h>

#ifndef _GNU_SOURCE
size_t strnlen (const char* s, size_t len);

char* strndup (char const* s, size_t n);
#endif  // _GNU_SOURCE

// str_* utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

char* str_beginswith (char* haystack, char* needle);

// Return concatenation of s1 and s2. Modify none.
char* str_concat (char* s1, char* s2);

// Create a new string with all instances of from replace to to.
char* str_replace (char* s, char* from, char* to);

// Destructively split string into a strl.  `str' cannot be const (or
// non-writable).  Empty tokens are suppressed.
struct strl* str_split (char* str, char* delims);

// Same as str_split, except there is a limit to number of delimitions.
struct strl* str_split_n (char* str, int limit, char* delims);

// Split buf of maximum size buf_size into a strl.  Empty terms are not
// allowed (terminates the strl at that point).  Quotations (single and
// double quote characters) and also escaping (using backslash) are respected.
struct strl* str_split_qe (char* buf, size_t buf_size);


// strl_* utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

struct strl {
  char* str;
  struct strl* next;
};

struct strl* strl_cons (char* str, struct strl* lst);

int strl_count (struct strl* lst);

void strl_free (struct strl* lst);

// Create a strl from a pack in the input buffer.  buf_cur will be updated
// to point to right after the pack in the buffer.  buf_lim shows the extent
// right before which we read the input buffer.
struct strl* strl_from_pack (char** buf_cur, const char* buf_lim);

struct strl* strl_from_vec (char* vec[], int beg, int end);

void strl_nconcat (struct strl** lst, struct strl* tail);

void strl_nreverse (struct strl** lst);

char* strl_pop (struct strl** lst);

// Place the strl lst in the buffer. Places a NULL character after each
// string. Two consecutive NULLs show the end of pack. buf_lim points to right
// after the end of buffer, beyond which we never write. buf_cur will be
// modified to point to right after the pack in the buffer.
// You can use the idiom: buf_cur = buf; buf_lim = buf + buffer_size;
//
// None of the strings should be empty.
// TODO: relax this restriction. use the format ~aaa0~bbb0~ccc00.
void* strl_to_pack (char** buf_cur, const char* buf_lim, struct strl* lst);

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// If dest is NULL do nothing and just return NULL.  Otherwise, copy the
// string from src to dest, up to a certain point in dest (the memory location
// of the limit is passed to the function).  If '\0' does not occur within
// that range (src string is too long), return NULL.
// Also, if src is NULL, only a 0 is appended at the end of dest.
char* strlcpy_p (char* dest, const char* src, const char* dest_limit);

#endif  // STRL_H_
