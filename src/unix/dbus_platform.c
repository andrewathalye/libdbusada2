#define LOCAL __attribute__((visibility("hidden")))
/*TODO do we need*/

#ifdef __linux__
#define _GNU_SOURCE
#include <string.h>
#include <sys/socket.h>
#elif defined __APPLE__
#include <sys/types.h>
#endif

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <unistd.h>

LOCAL char *get_user_id_c(void) {
  intmax_t uid;
  int size;
  char *res;

  uid = (intmax_t)geteuid();
  size = snprintf(NULL, 0, "%jd", uid);
  res = malloc(size + 1);
  sprintf(res, "%jd", uid);

  return res;
}

#if defined(linux) || defined(__APPLE__)
LOCAL const int max_possible_fds = 253;
#else
LOCAL const int max_possible_fds = 0;
#endif

LOCAL bool is_running_c(pid_t handle) { return (bool)getpgid(handle) > 0; }

/*TODO is this impl reliable? can we end up reading multiple msgs worth of cmsg
 * data?*/
LOCAL bool read_fds_c(int socket, int *fds, int *fd_count) {
#ifdef SCM_RIGHTS
  struct iovec iov = {.iov_base = &fd_count, .iov_len = sizeof(fd_count)};
  struct msghdr hdr = {0};
  struct cmsghdr *chdr;
  int i = 0;

  /* Control message buffer */
  char buf[CMSG_SPACE(max_possible_fds * sizeof(int))];
  memset(&buf, 0, sizeof(buf));

  /* Set msghdr fields */
  hdr.msg_iov = &iov;
  hdr.msg_iovlen = 1;
  hdr.msg_control = &buf;
  hdr.msg_controllen = sizeof(buf);

  /* Try to receive a message */
  if (recvmsg(socket, &hdr, 0) == -1)
    return false;

  if ((chdr = CMSG_FIRSTHDR(&hdr)) == NULL)
    return false;

  /*TODO check cmsg nxthdr for potential ddos attack and close fds */
  /* Fail if this is the wrong kind of aux message */
  if (chdr->cmsg_level != SOL_SOCKET || chdr->cmsg_type != SCM_RIGHTS) {
    return false;
  }

  /* Calculate number of fds passed */
  while (CMSG_LEN((i + 1) * sizeof(int)) <= chdr->cmsg_len)
    i++;

  *fd_count = i;
  memcpy(fds, CMSG_DATA(chdr), i * sizeof(int));

  return true;

#else
  return false;
#endif
}

LOCAL bool write_fds_c(int socket, int *fds, int fd_count) {
  if (fd_count < 0)
    return false;

#ifdef SCM_RIGHTS
  struct iovec iov = {.iov_base = &fd_count, .iov_len = sizeof(fd_count)};
  struct msghdr hdr = {0};
  struct cmsghdr *chdr;

  /* Control message buffer */
  char buf[CMSG_SPACE(fd_count * sizeof(int))];
  memset(buf, 0, sizeof(buf));

  /* Set required fields in hdr */
  hdr.msg_iov = &iov;
  hdr.msg_iovlen = 1;
  hdr.msg_control = &buf;
  hdr.msg_controllen = sizeof(buf);

  /* Get cmsghdr */
  if ((chdr = CMSG_FIRSTHDR(&hdr)) == NULL)
    return false;
  chdr->cmsg_len = CMSG_LEN(fd_count * sizeof(int));
  chdr->cmsg_level = SOL_SOCKET;
  chdr->cmsg_type = SCM_RIGHTS;

  /* Copy data */
  memcpy(CMSG_DATA(chdr), fds, fd_count * sizeof(int));

  if (sendmsg(socket, &hdr, 0) == -1)
    return false;

  return true;
#else
  return false;
#endif
}

LOCAL char *read_credentials_c(int sockfd) {
  intmax_t uid;
  int size;
  char *res;

#ifdef __linux__
  struct ucred cred;
  socklen_t credsize = sizeof(cred);
  if (!getsockopt(sockfd, SOL_SOCKET, SO_PEERCRED, &cred, &credsize))
    return NULL;
  uid = (intmax_t)cred.uid;
#elif defined __APPLE__
  uid_t euid;
  gid_t egid;

  if (getpeereid(sockfd, &euid, &egid) != 0)
    return NULL;
  uid = (intmax_t)euid;
#else
  return NULL;
#endif

  size = snprintf(NULL, 0, "%jd", uid);
  res = malloc(size + 1);
  sprintf(res, "%jd", uid);

  return res;
}

LOCAL bool write_credentials_c(int sockfd) {
#ifdef __linux__
  struct ucred cred = {.pid = getpid(), .uid = geteuid(), .gid = getgid()};
  char nullbyte = 0;
  const int flag = 1;
  struct iovec iov = {.iov_base = &nullbyte, .iov_len = sizeof(nullbyte)};
  struct msghdr hdr = {0};
  struct cmsghdr *chdr;

  /* Create buffer for msg_control */
  union {
    char buf[CMSG_SPACE(sizeof(cred))];
    struct cmsghdr align;
  } u;

  /* Set required fields in hdr */
  hdr.msg_iov = &iov;
  hdr.msg_iovlen = 1;
  hdr.msg_control = u.buf;
  hdr.msg_controllen = sizeof(u.buf);

  /* Zero-initialise control buffer */
  memset(u.buf, 0, sizeof(u.buf));

  /* Get the cmsg now that it has a buffer */
  if (!(chdr = CMSG_FIRSTHDR(&hdr)))
    return false;

  /* Then set control fields */
  chdr->cmsg_len = CMSG_LEN(sizeof(cred));
  chdr->cmsg_level = SOL_SOCKET;
  chdr->cmsg_type = SCM_CREDENTIALS;
  memcpy(CMSG_DATA(chdr), &cred, sizeof(cred));

  if (setsockopt(sockfd, SOL_SOCKET, SO_PASSCRED, &flag, sizeof(flag)) != 0)
    return false;

  if (sendmsg(sockfd, &hdr, 0) == -1) {
    return false;
  }

  return true;
#else
  return false;
#endif
}
