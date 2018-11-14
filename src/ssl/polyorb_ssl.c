/****************************************************************************
 *                                                                          *
 *                           POLYORB COMPONENTS                             *
 *                                                                          *
 *                          P O L Y O R B _ S S L                           *
 *                                                                          *
 *                       C   s u p p o r t   f i l e                        *
 *                                                                          *
 *         Copyright (C) 2005-2018, Free Software Foundation, Inc.          *
 *                                                                          *
 * PolyORB is free software; you  can  redistribute  it and/or modify it    *
 * under terms of the  GNU General Public License as published by the  Free *
 * Software Foundation;  either version 2,  or (at your option)  any  later *
 * version. PolyORB is distributed  in the hope that it will be  useful,    *
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public *
 * License  for more details.  You should have received  a copy of the GNU  *
 * General Public License distributed with PolyORB; see file COPYING. If    *
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth    *
 * Floor, Boston, MA 02111-1301, USA.                                       *
 *                                                                          *
 * As a special exception,  if other files  instantiate  generics from this *
 * unit, or you link  this unit with other files  to produce an executable, *
 * this  unit  does not  by itself cause  the resulting  executable  to  be *
 * covered  by the  GNU  General  Public  License.  This exception does not *
 * however invalidate  any other reasons why  the executable file  might be *
 * covered by the  GNU Public License.                                      *
 *                                                                          *
 *                  PolyORB is maintained by AdaCore                        *
 *                     (email: sales@adacore.com)                           *
 *                                                                          *
 ****************************************************************************/

#include <openssl/ssl.h>

/* Functional binding for OpenSSL macros */

int __PolyORB_sk_SSL_CIPHER_num (STACK_OF(SSL_CIPHER) *sk) {
    return sk_SSL_CIPHER_num(sk);
}

SSL_CIPHER *__PolyORB_sk_SSL_CIPHER_value (STACK_OF(SSL_CIPHER) *sk, int i) {
    return sk_SSL_CIPHER_value(sk, i);
}

/* Compatibility shims for older OpenSSL versions */

/* Version-flexible method returning highest available protocol */

const SSL_METHOD __attribute__((weak)) *TLS_method(void) {
  return SSLv23_method();
}

const SSL_METHOD __attribute__((weak)) *TLS_client_method(void) {
  return SSLv23_client_method();
}

const SSL_METHOD __attribute__((weak)) *TLS_server_method(void) {
  return SSLv23_server_method();
}

/* Version-specific methods */

const SSL_METHOD __attribute__((weak)) *TLSv1_1_method(void) {
  return NULL;
}

const SSL_METHOD __attribute__((weak)) *TLSv1_1_client_method(void) {
  return NULL;
}

const SSL_METHOD __attribute__((weak)) *TLSv1_1_server_method(void) {
  return NULL;
}

const SSL_METHOD __attribute__((weak)) *TLSv1_2_method(void) {
  return NULL;
}

const SSL_METHOD __attribute__((weak)) *TLSv1_2_client_method(void) {
  return NULL;
}

const SSL_METHOD __attribute__((weak)) *TLSv1_2_server_method(void) {
  return NULL;
}
