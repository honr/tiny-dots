#include <getopt.h>
#include <limits.h>
#include <signal.h>  // ?
#include <sys/stat.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <time.h>

#include "clove-common.h"

#define CONFLINE_MAX 8192  // Maximum size of each line in the config file.

#define sockpath_length 256
#define args_max 256
#define envs_max 256

char* RTPREFIX;  // Has to be defined in your code. e.g.: = "~/.local";
char* RUNPATH;   // Same, e.g.: = "~/.local/var/run/clove";

// Replace "~" with $HOME
char* expand_file_name (char* filename);

struct service {
  char* name;
  char* confpath;  // The default path to the config file.
  time_t confpath_last_mtime;
  char* binpath;  // The default path to the executable.
  char* sockpath;
  int pid;
  int sock;
};

struct serviceconf {
  struct strl* interpretter;
  struct strl* envs;
};

char* service_socket_path_dir ();

struct service service_init (char* service_name);

char* service_call (struct service srv, char** default_envp);

struct serviceconf* parse_conf_file (char* filepath);

char** envp_dup_update_or_add (char** envp, struct strl* extraenvs);

char** argv_dup_add (char** oldargv, struct strl* prefixargv);

// Input should not have ".." or "." path components.
// Returns 0 on success and -1 on failure.
int makeancesdirs (char* path);

void sigaction_inst (int signum, void (*handler)(int));
