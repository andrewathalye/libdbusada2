#ifdef __linux__
   #define _GNU_SOURCE
   #include <sys/socket.h>
   #include <string.h>
#elif defined __APPLE__
   #include <sys/types.h>
#endif

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

char* read_credentials_c (int sockfd) {
   intmax_t uid;
   int size;
   char* res;

#ifdef __linux__
   struct ucred cred;
   int credsize = sizeof (cred);
   if (!getsockopt (sockfd, SOL_SOCKET, SO_PEERCRED, &cred, &credsize))
      return NULL;
   uid = (intmax_t) cred.uid;
#elif defined __APPLE__
   uid_t euid;
   gid_t egid;

   if (getpeereid (sockfd, &euid, &egid) != 0)
      return NULL;
   uid = (intmax_t) euid;
#else
   return NULL;
#endif

   size = snprintf (NULL, 0, "%jd", uid);
   res = malloc (size + 1);
   sprintf (res, "%jd", uid);

   return res;
}

bool write_credentials_c (int sockfd) {
#ifdef __linux__
   struct ucred cred = {.pid = getpid (), .uid = geteuid (), .gid = getgid ()};
   char nullbyte = 0;
   const int flag = 1;
   struct iovec iov = {.iov_base = &nullbyte, .iov_len = sizeof (nullbyte)};
   struct msghdr hdr = { 0 };
   struct cmsghdr *chdr;

   /* Create buffer for msg_control */
   union {
      char buf [CMSG_SPACE (CMSG_LEN (sizeof (cred)))];
      struct cmsghdr align;
   } u;
   
   /* Set required fields in hdr */
   hdr.msg_iov = &iov;
   hdr.msg_iovlen = 1;
   hdr.msg_control = u.buf;
   hdr.msg_controllen = sizeof (u.buf);

   /* Zero-initialise control buffer */
   memset (u.buf, 0, sizeof (u.buf));

   /* Get the cmsg now that it has a buffer */
   if (!(chdr = CMSG_FIRSTHDR (&hdr)))
      return false;
   hdr.msg_control = chdr;

   /* Then set control fields */
   chdr->cmsg_len = CMSG_LEN (sizeof (cred));
   chdr->cmsg_level = SOL_SOCKET;
   chdr->cmsg_type = SCM_CREDENTIALS;
   memcpy (CMSG_DATA (chdr), &cred, sizeof (cred));

   setsockopt (sockfd, SOL_SOCKET, SO_PASSCRED, &flag, sizeof (flag));

   hdr.msg_control = NULL;
   hdr.msg_controllen = 0;

   if (sendmsg (sockfd, &hdr, 0) == -1) {
      return false;
   }

   return true;
#else
   return false;
#endif
}
