#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

#include <unistd.h>

char* get_user_id_c (void) {
   intmax_t uid;
   int size;
   char* res;

   uid = (intmax_t) geteuid ();
   size = snprintf (NULL, 0, "%jd", uid);
   res = malloc (size + 1);
   sprintf (res, "%jd", uid);

   return res;
}

bool is_running_c (pid_t handle) {
   return (bool) getpgid (handle) > 0;
}
