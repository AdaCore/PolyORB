/*****************************************************************************
**                                                                          **
**                           POLYORB COMPONENTS                             **
**                                                                          **
**                         P O L Y O R B . X 5 0 9                          **
**                                                                          **
**                       C   s u p p o r t   f i l e                        **
**                                                                          **
**            Copyright (C) 2005 Free Software Foundation, Inc.             **
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
**                    (email: sales at adacore.com)                         **
**                                                                          **
*****************************************************************************/

#include <openssl/asn1.h>
#include <openssl/x509.h>

typedef STACK_OF(X509) X509_CHAIN;
DECLARE_ASN1_FUNCTIONS(X509_CHAIN)

#define POLYORB_F_PEM_READ_PRIVATEKEY_FILE   100
#define POLYORB_F_PEM_READ_CERTIFICATE_FILE  101

#define ERR_LIB_POLYORB ERR_LIB_USER
#define PolyORBerr(f,r) ERR_PUT_error(ERR_LIB_POLYORB,(f),(r),__FILE__,__LINE__)

/* End of header */

#include <openssl/asn1t.h>
#include <openssl/bio.h>
#include <openssl/err.h>
#include <openssl/evp.h>
#include <openssl/pem.h>

ASN1_ITEM_TEMPLATE(X509_CHAIN) =
    ASN1_EX_TEMPLATE_TYPE(ASN1_TFLG_SEQUENCE_OF, 0, CertificateChain, X509)
ASN1_ITEM_TEMPLATE_END(X509_CHAIN)

IMPLEMENT_ASN1_FUNCTIONS(X509_CHAIN)

/*****************************/
/* __PolyORB_Get_CRYPTO_LOCK */
/*****************************/

unsigned int __PolyORB_Get_CRYPTO_LOCK (void) {
  return (unsigned int) (CRYPTO_LOCK);
}

/****************************/
/* __PolyORB_d2i_X509_CHAIN */
/****************************/

X509_CHAIN *__PolyORB_d2i_X509_CHAIN(unsigned char *buffer, int length) {
    unsigned char *aux = buffer;

    return d2i_X509_CHAIN(NULL, &aux, length);
}

/***************************/
/* __PolyORB_d2i_X509_NAME */
/***************************/

X509_NAME *__PolyORB_d2i_X509_NAME(unsigned char *buffer, int length) {
    unsigned char *aux = buffer;

    return d2i_X509_NAME(NULL, &aux, length);
}

/****************************/
/* __PolyORB_i2d_X509_CHAIN */
/****************************/

void __PolyORB_i2d_X509_CHAIN(STACK_OF(X509) *chain,
                              unsigned char **buffer,
                              int *length)
{
    unsigned char *aux;

    /* Calculate required buffer length */

    *length = i2d_X509_CHAIN(chain, NULL);
    if (*length < 0) {
        return;
    }

    /* Allocate buffer */

    *buffer = OPENSSL_malloc(*length);
    if (*buffer == NULL) {
        return;
    }

    /* Encode X.509 Certificate Chain */

    aux = *buffer;
    *length = i2d_X509_CHAIN(chain, &aux);
    if (*length < 0) {
        OPENSSL_free(*buffer);
        *buffer = NULL;
    }
}

/***************************/
/* __PolyORB_i2d_X509_NAME */
/***************************/

void __PolyORB_i2d_X509_NAME(X509_NAME *name,
                             unsigned char **buffer,
                             int *length)
{
    unsigned char *aux;

    /* Calculate required buffer length */

    *length = i2d_X509_NAME(name, NULL);
    if (*length < 0) {
        return;
    }

    /* Allocate buffer */

    *buffer = OPENSSL_malloc(*length);
    if (*buffer == NULL) {
        return;
    }

    /* Encode X.509 NAME */

    aux = *buffer;
    *length = i2d_X509_NAME(name, &aux);
    if (*length < 0) {
        OPENSSL_free(*buffer);
        *buffer = NULL;
    }
}

/***************************************/
/* __PolyORB_PEM_read_certificate_file */
/***************************************/

X509 *__PolyORB_PEM_read_certificate_file (const char *file) {
    BIO *in;
    X509 *pkey = NULL;

    in = BIO_new(BIO_s_file_internal());
    if (in == NULL) {
        PolyORBerr(POLYORB_F_PEM_READ_CERTIFICATE_FILE, ERR_R_BUF_LIB);
        goto end;
    };

    if (BIO_read_filename(in, file) <= 0) {
        PolyORBerr(POLYORB_F_PEM_READ_CERTIFICATE_FILE, ERR_R_SYS_LIB);
        goto end;
    };

    pkey = PEM_read_bio_X509(in, NULL, NULL, NULL);

    if (pkey == NULL) {
        PolyORBerr(POLYORB_F_PEM_READ_CERTIFICATE_FILE, ERR_R_PEM_LIB);
        goto end;
    }

end:
    if (in != NULL) BIO_free(in);

    return pkey;
}

/**************************************/
/* __PolyORB_PEM_read_PrivateKey_file */
/**************************************/

EVP_PKEY *__PolyORB_PEM_read_PrivateKey_file (const char *file) {
    BIO *in;
    EVP_PKEY *pkey = NULL;

    in = BIO_new(BIO_s_file_internal());
    if (in == NULL) {
        PolyORBerr(POLYORB_F_PEM_READ_PRIVATEKEY_FILE, ERR_R_BUF_LIB);
        goto end;
    };

    if (BIO_read_filename(in, file) <= 0) {
        PolyORBerr(POLYORB_F_PEM_READ_PRIVATEKEY_FILE, ERR_R_SYS_LIB);
        goto end;
    };

    pkey = PEM_read_bio_PrivateKey(in, NULL, NULL, NULL);

    if (pkey == NULL) {
        PolyORBerr(POLYORB_F_PEM_READ_PRIVATEKEY_FILE, ERR_R_PEM_LIB);
        goto end;
    }

end:
    if (in != NULL) BIO_free(in);

    return pkey;
}

/*************************/
/* __PolyORB_sk_X509_num */
/*************************/

/*
int __PolyORB_sk_X509_num (STACK_OF(X509) *sk) {
    return sk_X509_num(sk);
}
*/

#define ERR_FUNC(func) ERR_PACK(ERR_LIB_POLYORB, func, 0)
#define ERR_REASON(reason) ERR_PACK(ERR_LIB_POLYORB, 0, reason)

static ERR_STRING_DATA USER_str_functs[] =
{
    {ERR_FUNC(POLYORB_F_PEM_READ_PRIVATEKEY_FILE),
        "__PolyORB_PEM_read_PrivateKey_file"},
    {ERR_FUNC(POLYORB_F_PEM_READ_CERTIFICATE_FILE),
        "__PolyORB_PEM_read_certificate_file"},
    {0, NULL}
};

static ERR_STRING_DATA USER_str_reasons[] =
{
    {0, NULL}
};

/****************************/
/* ERR_load_PolyORB_strings */
/****************************/

void ERR_load_PolyORB_strings(void) {
    static int init = 1;

    if (init) {
        init = 0;

        ERR_load_strings(0, USER_str_functs);
        ERR_load_strings(0, USER_str_reasons);
    }
}

/****************************/
/* __PolyORB_X509_Intialize */
/****************************/

void __PolyORB_X509_Intialize
  (void (*locking_function)(int mode, int n, const char *file, int line),
   unsigned long (*id_function)(void))
{
    CRYPTO_set_locking_callback (locking_function);
    CRYPTO_set_id_callback (id_function);
}
