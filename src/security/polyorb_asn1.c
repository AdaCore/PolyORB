/*****************************************************************************
**                                                                          **
**                           POLYORB COMPONENTS                             **
**                                                                          **
**                         P O L Y O R B _ A S N 1                          **
**                                                                          **
**                       C   s u p p o r t   f i l e                        **
**                                                                          **
**         Copyright (C) 2005-2011, Free Software Foundation, Inc.          **
**                                                                          **
** PolyORB is free software; you  can  redistribute  it and/or modify it    **
** under terms of the  GNU General Public License as published by the  Free **
** Software Foundation;  either version 2,  or (at your option)  any  later **
** version. PolyORB is distributed  in the hope that it will be  useful,    **
** but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- **
** TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public **
** License  for more details.  You should have received  a copy of the GNU  **
** General Public License distributed with PolyORB; see file COPYING. If    **
** not, write to the Free Software Foundation, 51 Franklin Street, Fifth    **
** Floor, Boston, MA 02111-1301, USA.                                       **
**                                                                          **
** As a special exception,  if other files  instantiate  generics from this **
** unit, or you link  this unit with other files  to produce an executable, **
** this  unit  does not  by itself cause  the resulting  executable  to  be **
** covered  by the  GNU  General  Public  License.  This exception does not **
** however invalidate  any other reasons why  the executable file  might be **
** covered by the  GNU Public License.                                      **
**                                                                          **
**                  PolyORB is maintained by AdaCore                        **
**                     (email: sales at adacore.com)                           **
**                                                                          **
*****************************************************************************/

#include <openssl/asn1.h>
#include <openssl/objects.h>

/**************************/
/* __PolyORB_OPENSSL_free */
/**************************/

void __PolyORB_OPENSSL_free(unsigned char *x) {
    if (x != NULL) {
        OPENSSL_free(x);
    }
}

/*****************************/
/* __PolyORB_i2d_ASN1_OBJECT */
/*****************************/

void __PolyORB_i2d_ASN1_OBJECT(ASN1_OBJECT *object,
                               unsigned char **buffer,
                               int *length)
{
    unsigned char *aux;

    /* Calculate required buffer length */

    *length = i2d_ASN1_OBJECT(object, NULL);
    if (*length < 0) {
        return;
    }

    /* Allocate buffer */

    *buffer = OPENSSL_malloc(*length);
    if (buffer == NULL) {
        return;
    }

    /* Encode OBJECT IDENTIFIER */

    aux = *buffer;
    *length = i2d_ASN1_OBJECT(object, &aux);
    if (*length < 0) {
        OPENSSL_free(*buffer);
        *buffer = NULL;
    }
}

/*****************************/
/* __PolyORB_d2i_ASN1_OBJECT */
/*****************************/

ASN1_OBJECT *__PolyORB_d2i_ASN1_OBJECT(const unsigned char *buf, int length) {
    const unsigned char *aux = buf;

    return d2i_ASN1_OBJECT(NULL, &aux, length);
}

/***************************/
/* PolyORB_ASN1_OBJECT_dup */
/***************************/

ASN1_OBJECT *__PolyORB_ASN1_OBJECT_dup(ASN1_OBJECT *x) {
    return (ASN1_OBJECT *)ASN1_dup
        ((int (*)())i2d_ASN1_OBJECT,
        (void * (*)(void **, const unsigned char **, long int))d2i_ASN1_OBJECT,
        (char *)x);
}
