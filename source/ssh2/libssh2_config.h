/* Copyright (c) 2014 Alexander Lamaison <alexander.lamaison@gmail.com>
 *
 * Redistribution and use in source and binary forms,
 * with or without modification, are permitted provided
 * that the following conditions are met:
 *
 *   Redistributions of source code must retain the above
 *   copyright notice, this list of conditions and the
 *   following disclaimer.
 *
 *   Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following
 *   disclaimer in the documentation and/or other materials
 *   provided with the distribution.
 *
 *   Neither the name of the copyright holder nor the names
 *   of any other contributors may be used to endorse or
 *   promote products derived from this software without
 *   specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 */

#ifndef LIBSSH2_CONFIG_H

#define LIBSSH2_CONFIG_H

#ifdef WIN32

  /* Headers */
  #define HAVE_UNISTD_H
  #define HAVE_INTTYPES_H
  #define HAVE_STDLIB_H
  /* #undef HAVE_SYS_SELECT_H */
  /* #undef HAVE_SYS_SOCKET_H */
  #define HAVE_SYS_TIME_H
  /* #undef HAVE_ARPA_INET_H */
  /* #undef HAVE_NETINET_IN_H */
  #define HAVE_WINSOCK2_H

  /* Functions */
  #define HAVE_STRCASECMP
  #define HAVE__STRICMP
  #define HAVE_SNPRINTF
  #define HAVE__SNPRINTF

  /* Workaround for platforms without POSIX strcasecmp (e.g. Windows) */
  #ifndef HAVE_STRCASECMP
  # ifdef HAVE__STRICMP
  # define strcasecmp _stricmp
  # define HAVE_STRCASECMP
  # endif
  #endif

  /* Symbols */
  #define HAVE___FUNC__
  #define HAVE___FUNCTION__

  /* Workaround for platforms without C90 __func__ */
  #ifndef HAVE___FUNC__
  # ifdef HAVE___FUNCTION__
  # define __func__ __FUNCTION__
  # define HAVE___FUNC__
  # endif
  #endif

#else
  /* VMS specific libssh2_config.h
   */

  #define ssize_t SSIZE_T

  typedef unsigned int uint32_t;
  typedef unsigned int socklen_t; /* missing in headers on VMS */

  /* Have's */

  #define HAVE_UNISTD_H
  #define HAVE_INTTYPES_H
  #define HAVE_SYS_TIME_H
  #define HAVE_SELECT
  #define HAVE_UIO

  #define HAVE_SYS_SOCKET_H
  #define HAVE_NETINET_IN_H
  #define HAVE_ARPA_INET_H

  #define HAVE_GETTIMEOFDAY 1
  #define HAVE_SNPRINTF 1

  #define POSIX_C_SOURCE

  /* Enable the possibility of using tracing */

  #define LIBSSH2DEBUG 1

  /* For selection of proper block/unblock function in session.c */

  #define HAVE_FIONBIO

  #include <stropts.h>

  /* In VMS TCP/IP Services and some BSD variants SO_STATE retrieves
   * a bitmask revealing amongst others the blocking state of the
   * socket. On VMS the bits are undocumented, but  SS_NBIO
   * works, I did not test the other bits. Below bitdefs are
   * from Berkely source socketvar.h at
   * http://ftp.fibranet.cat/UnixArchive/PDP-11/Trees/2.11BSD/sys/h/socketvar.h
   *  Socket state bits.
   *  #define SS_NOFDREF          0x001    no file table ref any more
   *  #define SS_ISCONNECTED      0x002    socket connected to a peer
   *  #define SS_ISCONNECTING     0x004    in process of connecting to peer
   *  #define SS_ISDISCONNECTING  0x008    in process of disconnecting
   *  #define SS_CANTSENDMORE     0x010    can't send more data to peer
   *  #define SS_CANTRCVMORE      0x020    can't receive more data from peer
   *  #define SS_RCVATMARK        0x040    at mark on input
   *  #define SS_PRIV             0x080    privileged for broadcast, raw...
   *  #define SS_NBIO             0x100    non-blocking ops
   *  #define SS_ASYNC            0x200    async i/o notify
   *
   */

  #ifdef SO_STATE

  /* SO_STATE is defined in stropts.h  by DECC
   * When running on Multinet, SO_STATE renders a protocol
   * not started error. Functionally this has no impact,
   * apart from libssh2 not being able to restore the socket
   * to the proper blocking/non-blocking state.
   */

  #define SS_NBIO         0x100

  #endif

  /* Use OpenSSL */
  #define LIBSSH2_OPENSSL 1

  /* Compile in zlib support. We link against gnv$libzshr, as available
   * from https://sourceforge.net/projects/vms-ports/files/.
   */

  #define LIBSSH2_HAVE_ZLIB

#endif /* WIN32 */
#endif /* LIBSSH2_CONFIG_H */
