#include "strl.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* // For OSX */
/* #ifndef _GNU_SOURCE */
/* size_t strnlen (const char* s, size_t len) { */
/*   size_t i; */
/*   for (i = 0; i < len && *s; i++, s++) {} */
/*   return i; */
/* } */

/* char* strndup (const char* s, size_t n) { */
/*   size_t len = strnlen (s, n);  // Or n-1 ? */
/*   char* t; */

/*   if ((t = malloc (len + 1)) == NULL) return NULL; */

/*   t[len] = '\0'; */
/*   return memcpy (t, s, len); */
/* } */
/* #endif  // _GNU_SOURCE */

// str_* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

char* str_beginswith (char* haystack, char* needle) {
  register char* cur1;
  register char* cur2;
  for (cur1 = haystack, cur2 = needle;
       (*cur1) && (*cur2) && (*cur1 == *cur2);
       cur1++, cur2++) {}
  if ((*cur2) == '\0') {
    return cur1;
  }
  return NULL;
}

char* str_concat (char* s1, char* s2) {
  int size_1 = strlen (s1);
  int size_2 = strlen (s2);
  char* res = (char*)malloc (size_1 + size_2 + 1);
  char* resp = res;
  memcpy (resp, s1, size_1);
  resp += size_1;
  memcpy (resp, s2, size_2);
  resp += size_2;
  *resp = 0;
  return res;
}

char* str_replace (char* s, char* from, char* to) {
  int size_from = strlen (from);
  int size_to = strlen (to);
  int size_s = strlen (s);
  int i;
  char* cur;
  // Pass 1: Find the number of occurances, to find the size of the result.
  for (cur = strstr (s, from), i = 0;
       cur && *cur;
       cur = strstr (cur + size_from, from), i++) {}
  char* res = (char*)malloc (size_s + (size_to - size_from) * i + 1);
  char* resp = res;

  // Pass 2: Copy.
  char* prev;
  for (prev = s, cur = strstr (prev, from); cur;
       prev = cur + size_from, cur = strstr (prev, from)) {
    memcpy (resp, prev, cur - prev);
    resp += cur - prev;
    memcpy (resp, to, size_to);
    resp += size_to;
  }
  strcpy (resp, prev);

  return res;
}

struct strl* str_split (char* str, char* delims) {
  struct strl* head = NULL;
  char* running = strdup (str);
  char* token;
  while ((token = strsep (&running, delims))) {
    if (*token) {
      head = strl_cons (token, head);
    }
  }
  return head;
}

struct strl* str_split_n (char* str, int limit, char* delims) {
  struct strl* head = NULL;
  char* running = strdup (str);
  char* token;
  while ((limit > 1) && (token = strsep (&running, delims))) {
    if (*token) {
      head = strl_cons (token, head);
      limit--;
    }
  }
  if (*running) {
    head = strl_cons (running, head);
  }
  return head;
}

struct strl* str_split_qe (char* buf, size_t buf_size) {
  char* buf_cur, *bufout_cur, c, state, action, quote;
  const char STATE_ESCAPE = 2;
  const char STATE_TOKEN = 1;
  const char STATE_SPACE = 0;
  char* bufout = malloc (buf_size);
  for (buf_cur = buf, bufout_cur = bufout, state = STATE_SPACE, quote = 0;
       *buf_cur; buf_cur++) {
    c = *buf_cur;
    action = 0;

    // State Transitions:
    if (state & STATE_ESCAPE) {
      state ^= STATE_ESCAPE;
    } else if (c == '\\') {
      state = STATE_TOKEN | STATE_ESCAPE;
      action = 'f';
    } else if (state == STATE_SPACE) {
      if ((c == ' ') || (c == '\t') || (c == '\n') || (c == '\r')) {
        action = 'p';  // Pass.
      } else if ((c == '"') || (c == '\'')) {
        state = STATE_TOKEN;
        action = 'p';
        quote = c;
      } else {
        state = STATE_TOKEN;  // Hold head.
      }
    } else if (state == STATE_TOKEN) {
      if (quote) {
        if (c == quote) {
          state = STATE_SPACE;
          action = 'c';
          quote = 0;
        }
      } else {
        if ((c == ' ') || (c == '\t') || (c == '\n') || (c == '\r')) {
          state = STATE_SPACE;
          action = 'c';
        } else if ((c == '"') || (c == '\'')) {
          state = STATE_TOKEN;
          action = 'c';
          quote = c;
        }
      }
    }

    // Copy or flush.
    if ((action == 'c') || (action == 'f')) {
      memcpy (bufout_cur, buf, buf_cur - buf);
      bufout_cur += buf_cur - buf;
      buf = buf_cur;
      // Copy.
      if (action == 'c') {
        *bufout_cur = 0;  // NULL terminate.
        bufout_cur++;
      }
    }

    if (action) {
      buf++;
    }
  }

  // Termination:
  if (state & STATE_ESCAPE) {
    fprintf (stderr,
             "Error: Tried to escape end-of-line.\n"
             "       Reading next line is not implemented, yet.\n");
    // TODO: read next line.
  } else if (state == STATE_TOKEN) {
    if (quote) {
      fprintf (stderr,
               "Error: Tried to extend quotation to the next line.\n"
               "       Reading next line is not implemented, yet.\n");
    } else {
      memcpy (bufout_cur, buf, buf_cur - buf);
      bufout_cur += buf_cur - buf;
      buf = buf_cur + 1;  // Redundant.
      *bufout_cur = 0;    // NULL terminate.
      bufout_cur++;
    }
  }
  // End.

  *bufout_cur = 0;  // NULL terminate.
  return strl_from_pack (&bufout, bufout + buf_size);
}

// strl_* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

struct strl* strl_cons (char* str, struct strl* lst) {
  struct strl* head = (struct strl*)malloc (sizeof(struct strl));
  head->str = str;
  head->next = lst;
  return head;
}

int strl_count (struct strl* lst) {
  int count;
  for (count = 0; lst; lst = lst->next, count++) {
  }
  return count;
}

void strl_free (struct strl* lst) {
  struct strl* nxt;
  while (lst) {
    nxt = lst->next;
    free (lst->str);
    free (lst);
    lst = nxt;
  }
}

struct strl* strl_from_pack (char** buf_cur, const char* buf_lim) {
  char* buf = *buf_cur;
  int l;
  struct strl* lst = NULL;
  for (; *buf;) {
    if ((l = strnlen (buf, buf_lim - buf)) < buf_lim - buf) {
      lst = strl_cons (buf, lst);
      buf += l + 1;  // Skip the string and its NULL.
    } else {
      break;
    }
  }
  *buf_cur = buf + 1;
  // strl_nreverse (&lst);
  return lst;
}

struct strl* strl_from_vec (char* vec[], int beg, int end) {
  struct strl* lst = NULL;
  for (; end >= beg; end--) {
    lst = strl_cons (vec[end], lst);
  }
  return lst;
}

void strl_nconcat (struct strl** lst, struct strl* tail) {
  if (*lst) {
    struct strl* nxt = *lst;
    while (nxt->next) {
      nxt = nxt->next;
    }
    nxt->next = tail;
  } else {
    *lst = tail;
  }
}

void strl_nreverse (struct strl** lst) {
  if (lst) {
    struct strl* nxt;
    struct strl* prv = NULL;
    while (1) {
      nxt = (*lst)->next;
      (*lst)->next = prv;
      prv = (*lst);
      if (nxt) {
        (*lst) = nxt;
      } else {
        break;
      }
    }
  }
}

// WARNING: Currently strl_pop leaks, since the address to the next is not
// freed.
// TODO: Fix leakage.
char* strl_pop (struct strl** lst) {
  if (lst && (*lst) && (*lst)->str) {
    char* ret = (*lst)->str;
    (*lst) = (*lst)->next;
    return ret;
  }
  return NULL;
}

void* strl_to_pack (char** buf_cur, const char* buf_lim,
                        struct strl* lst) {
  char* buf = *buf_cur;
  char* cur;
  for (cur = strl_pop (&lst); cur; cur = strl_pop (&lst)) {
    buf = strlcpy_p (buf, cur, buf_lim);
  }
  buf = strlcpy_p (buf, NULL, buf_lim);
  *buf_cur = buf;
  return buf;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// If dest is NULL do nothing and just return NULL.  Otherwise, prepend the
// character `S' and copy the string from src to dest, up to a certain point
// in dest (the memory location of the limit is passed to the function).  If
// '\0' does not occur within that range (src string is too long), return
// NULL.
//
// Also, if src is NULL, only a 0 is appended at the end of dest.
char* strlcpy_p (char* dest, const char* src, const char* dest_limit) {
  int n = dest_limit - dest - 1;
  if (dest && (n > 1)) {
    if (src) {
      *(dest++) = 'S';
      dest = memccpy (dest, src, 0, n);
    } else {  // Only append a 0 at the dest.
      *(dest++) = 0;
    }
    return dest;
  }
  return NULL;
}
