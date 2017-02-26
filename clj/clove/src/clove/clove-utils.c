#include "clove-common.h"
#include "clove-utils.h"
#include "strl.h"

char* expand_file_name (char* filename) {
  return str_replace (filename, "~", getenv ("HOME"));
}

// sprintf (path, "%s/.local", getenv ("HOME"));

struct service service_init (char* service_name) {
  struct service s;
  char* prefix = expand_file_name (RTPREFIX);
  s.name = service_name;
  s.confpath = malloc (PATH_MAX);
  s.binpath = malloc (PATH_MAX);
  s.sockpath = malloc (PATH_MAX);

  if (strcmp (s.name, "broker") == 0) {
    sprintf (s.confpath, "%s/share/clove/clove.conf", prefix);
    s.binpath = NULL;
    sprintf (s.sockpath, "%s/broker", expand_file_name (RUNPATH));
  } else {
    sprintf (s.confpath, "%s/share/clove/clove-%s.conf", prefix, s.name);
    sprintf (s.binpath, "%s/share/clove/clove-%s", prefix, s.name);
    sprintf (s.sockpath, "%s/clove-%s", expand_file_name (RUNPATH), s.name);
  }

  s.confpath_last_mtime = 0;
  s.pid = -1;
  s.sock = -1;
  return s;
}

char* service_call (struct service srv, char** default_envp) {
  int pipefd[2];
  if (pipe (pipefd) != 0) {
    perror ("pipe");
  }
  int child;
  if ((child = fork ()) == 0) {
    close (pipefd[0]);
    close (1);
    if (dup2 (pipefd[1], 1) == -1) {
      perror ("dup2");
    }
    // char* service_argv_dummy[] = { NULL, "hello", "world", NULL };
    char* service_argv_dummy[] = {NULL, NULL};
    char** service_argv = service_argv_dummy;
    service_argv[0] = srv.binpath;
    char** service_envp = default_envp;
    // TODO: Devise ways to:
    //       - start,
    //       - stop,
    //       - restart,
    //       - status
    //       on a service.
    if (access (srv.confpath, F_OK) == 0) {
      struct serviceconf* sc = parse_conf_file (srv.confpath);
      service_envp = envp_dup_update_or_add (
          service_envp,
          strl_cons (str_concat ("CLOVESOCKET=", srv.sockpath), NULL));
      service_envp = envp_dup_update_or_add (service_envp, sc->envs);
      // TODO: Make sure this is working properly.
      //       Apparently the order of a duplicate env vars is different
      //       between Darwin and Linux.
      if (sc->interpretter) {
        service_argv[0] = srv.binpath;
        service_argv = argv_dup_add (service_argv, sc->interpretter);
        char* interpretter = service_argv[0];
        execve (interpretter, service_argv, service_envp);
        exit (3);
      }
    }

    if (execve (srv.binpath, service_argv, service_envp) == -1) {
      perror ("execve");
      exit (3);
    }
  }
  close (pipefd[1]);
  printf ("waiting for %s ...\n", srv.name);
  // TODO: have a time out.
  char* buf = malloc (128);  // TODO: Fix hardcoded size.
  // TODO: Fix hardcoded size.
  if (read (pipefd[0], buf, 127) < 0) {
    perror ("(read) could not communicate with the service");
    exit (1);
  }
  buf[127] = 0;  // TODO: Fix hardcoded size.
  printf ("%s says: %s", srv.name, buf);
  close (pipefd[0]);
  return buf;
}

struct serviceconf* parse_conf_file (char* filepath) {

  // TODO:
  //   cur_uname = uname ();
  //   cur_uname_m = uname ("-m");
  //   archmatcher="(^$(uname) $(uname
  // -m))|(^$(uname)/)|(^/)|(^[^/]+=)|(^[^/]+$)"
  //
  // TODO: Have platform dependent interpretters.
  //
  // TODO: Split key and value, and perform ~~ (or $THIS) replacement.
  FILE* file;
  char line_storage[CONFLINE_MAX];

  struct serviceconf* sc =
      (struct serviceconf*)malloc (sizeof(struct serviceconf));
  sc->interpretter = NULL;
  sc->envs = NULL;

  struct utsname unm;
  uname (&unm);

  char* whitespacechars = " \f\n\r\t\v";
  if ((file = fopen (filepath, "r"))) {
    while (fgets (line_storage, CONFLINE_MAX, file)) {
      char* line = strndup (line_storage, strcspn (line_storage, "\n"));
      if (line[0] == '/') {
        line = line + 1;  // Move beyond the '/'.
        char* platform_specifier;
        platform_specifier = strsep (&line, "/");
        // fprintf (stderr, "specifier: %s\n", platform_specifier);
        if ((*platform_specifier) &&
            ((platform_specifier =
                  str_beginswith (platform_specifier, unm.sysname)) == NULL)) {
          // fprintf (stderr, "sysname does not match\n");
          continue;
        }
        if (*platform_specifier) {
          platform_specifier++;  // Move past delimeter.
        }
        if ((*platform_specifier) &&
            ((platform_specifier =
                  str_beginswith (platform_specifier, unm.machine)) == NULL)) {
          // fprintf (stderr, "machine does not match\n");
          continue;
        }
      }

      /* If the line starts with a '/', there must be another '/'
         somewhere after that.  Take the string between the two
         '/'s as platform specifier, and drop the line unless the
         specifier matches current platform.  Removed. */

      // printf ("%s, %s\n", line, str_beginswith (line, "#!"));

      if (sc->interpretter == NULL) {
        char* line1 = str_beginswith (line, "#!");
        if (line1) {
          sc->interpretter = str_split (
              str_replace (line1, "~", getenv ("HOME")), whitespacechars);
          fprintf (stderr, "%s\n", line);  // printout accepted line
          continue;
        }
      }

      // Skip leading whitespace, take until '#'.
      line += strspn (line, whitespacechars);
      line = strsep (&line, "#");

      // line_eff = str_replace (line, "~~", former_value_of_key);
      // Move the above to envp_dup_update_or_add.
      line = str_replace (line, "~", getenv ("HOME"));
      // not empty
      if (line && *line) {
        fprintf (stderr, "%s\n", line);  // Printout accepted line.
        sc->envs = strl_cons (line, sc->envs);
      }
    }
    fclose (file);
  }
  return sc;
}

char** envp_dup_update_or_add (char** envp, struct strl* extraenvs) {
  // find the number of envp key-vals.
  if (extraenvs == NULL) {
    return envp;
  }
  // Otherwise, we need to make a new envp.

  int envp_count = 0;
  char** cur1;
  char** cur2;
  struct strl* cur3;
  for (cur1 = envp; *cur1; cur1++, envp_count++) {}
  for (cur3 = extraenvs; cur3; cur3 = cur3->next, envp_count++) {}

  char** envp_new = (char**)malloc ((envp_count + 2) * (sizeof(char*)));

  char* curstr;
  for (cur1 = envp, cur2 = envp_new; *cur1; cur1++, cur2++) {
    for (cur3 = extraenvs; cur3; cur3 = cur3->next) {
      if (((curstr = cur3->str) != NULL) &&
          (str_beginswith (
               *cur1, strndup (curstr, strcspn (curstr, "=") + 1)) != NULL)) {
        *cur2 = curstr;
        // fprintf (stderr, "updated: %s\n", *cur2);
        cur3->str = NULL;
      } else {
        *cur2 = *cur1;
      }
    }
  }

  for (cur3 = extraenvs; cur3; cur3 = cur3->next) {
    if ((curstr = cur3->str) != NULL) {
      *cur2 = curstr;
      // fprintf (stderr, "added: %s\n", *cur2);
      cur2++;
    }
  }
  *cur2 = NULL;
  return envp_new;
}

char** argv_dup_add (char** oldargv, struct strl* prefixargv) {
  int count_old, count;
  struct strl* cur;
  for (count_old = 0; oldargv[count_old]; count_old++) {}
  count = count_old + strl_count (prefixargv);
  char** newargv = (char**)malloc ((count + 1) * sizeof(char*));
  for (; count_old >= 0; count_old--, count--) {
    newargv[count] = oldargv[count_old];
  }
  for (cur = prefixargv; cur; count--, cur = cur->next) {
    newargv[count] = cur->str;
  }
  return newargv;
}

int makeancesdirs (char* path) {
  if (*path) {
    char* p, *q, *path_dup = strndup (path, PATH_MAX);
    for (p = path_dup + 1, q = strchr (p, '/'); q; q = strchr (p, '/')) {
      *q = 0;
      mkdir (path_dup, 0700);  // TODO: Fix hardcoded value.
      // TOOD: Make sure the mode 0777 or 01777 is OK for all platforms.
      *q = '/';
      p = q + 1;
    }
    free (path_dup);
    return 0;
  }  // Success.

  return -1;  // Path was empty failed.
}

void sigaction_inst (int signum, void (*handler)(int)) {
  struct sigaction new_action, old_action;

  new_action.sa_handler = handler;
  sigemptyset (&new_action.sa_mask);
  new_action.sa_flags = 0;

  sigaction (signum, NULL, &old_action);
  if (old_action.sa_handler != SIG_IGN) {
    sigaction (signum, &new_action, NULL);
  }
}
