AC_DEFUN([AM_WITH_OPENSSL],
[AC_ARG_WITH(openssl,
  AC_HELP_STRING([--with-openssl@<:@=ARG@:>@],
		 [enable SSL support, will check ARG/ssl <default-system-dirs>
                  /usr/local/ssl /usr/lib/ssl /usr/ssl /usr/pkg /usr/local /usr]),
[
    HAVE_SSL=no
    save_LIBS="$LIBS"
    save_CPPFLAGS="$CPPFLAGS"
    save_LDFLAGS="$LDFLAGS"

    dnl Libcrypto depends on the dynamic linking API: make sure we have
    dnl -ldl in LIBS when required. Most systems provide dlopen (),
    dnl but HP-UX uses shl_load().

    LIBS=""
    AC_SEARCH_LIBS(dlopen, dl, [], [AC_SEARCH_LIBS(shl_load, dl)])
    SSL_LIBDL="$LIBS"
    LIBS="$save_LIBS"

    dnl Libcrypto also depends on -lsocket -lnsl on Solaris: get them into
    dnl $LIBS if required (but that's just for the purpose of the test,
    dnl in real applications this will come from g-soliop.ads).

    AC_SEARCH_LIBS([gethostbyname], [nsl])
    AC_SEARCH_LIBS([socket], [socket])

    dnl Libcrypto depends on -lgdi and -lws2_32 on Windows.

    case "$host_os" in
      cygwin* | mingw*)
        SSL_WINLIBS="-lgdi32 -lws2_32"
        LIBS="$LIBS $SSL_WINLIBS"
        ;;

      *)
        SSL_WINLIBS=""
        ;;
    esac

    dnl If ARG is not specified, $withval is "yes", and we do not have any
    dnl additional user defined path to search. Special case if the user
    dnl specified --without-openssl: in this case $withval is "no" and we'll
    dnl exit early from the loop below.

    if test "$withval" != "yes"; then
        ssl_user_dir="$withval"
    fi

    # dir="-" corresponds to the default system locations

    for dir in $ssl_user_dir - /usr/local/ssl /usr/lib/ssl /usr/ssl /usr/pkg /usr/local /usr; do

        # Handle case of --without-openssl

        if test "$dir" = "no"; then
            break
        fi

        # Set candidate options for this location

        if test "$dir" != "-"; then
            SSL_INC="$dir/include"
            CPPFLAGS="$save_CPPFLAGS -I$SSL_INC"
            SSL_INC="${SSL_INC}/"

            LDFLAGS="$save_LDFLAGS -L$dir/lib"
            SSL_LDFLAGS="$LDFLAGS "
        else
            SSL_INC=""
            LDFLAGS="$save_LDFLAGS"
            SSL_LDFLAGS=""
        fi
        SSL_LDFLAGS="${SSL_LDFLAGS}-lssl -lcrypto ${SSL_LIBDL} ${SSL_WINLIBS}"
        # Note that order is important here (-lssl must come before
        # -lcrypto), since some undefined symbols in libssl are provided
        # by libcrypto.

        # Check whether we see appropriate headers and libraries

        ssl_fail="no"
        AC_CHECK_HEADERS([${SSL_INC}openssl/opensslv.h ${SSL_INC}openssl/ssl.h],
        [], [ssl_fail="yes"])

        if test "$ssl_fail" = "no"; then
            # If headers found, check for valid libcrypto and libssl.
            # unset ac_cv_XXX to prevent reuse of cached results from a previous
            # iteration.
            unset ac_cv_lib_crypto_SSLeay_version
            AC_CHECK_LIB(crypto, SSLeay_version, [], [ssl_fail="yes"],
              [$SSL_LIBDL])

            unset ac_cv_lib_ssl_SSL_CTX_new
            AC_CHECK_LIB(ssl, SSL_CTX_new, [], [ssl_fail="yes"],
              [$SSL_LIBDL])
        fi

        # Check for openssl(1)

        AC_PATH_PROG([OPENSSL], [openssl], [false], [$dir/bin:$PATH])
        if test "$OPENSSL" = "false"; then
            ssl_fail="yes"
        fi

        if test "$ssl_fail" = "no"; then
            if test "$dir" = "-"; then
                ssldir="default system directory"
            else
                ssldir="$dir"
            fi
            AC_MSG_NOTICE([OpenSSL found in $ssldir])
            CPPFLAGS="$CPPFLAGS -DHAVE_SSL"
            # Special case for RedHat Linux 9
            if test -f /usr/kerberos/include/krb5.h; then
                CPPFLAGS="-I/usr/kerberos/include/ ${CPPFLAGS}"
            fi

            HAVE_SSL=yes
            break;
        else
            CPPFLAGS="$save_CPPFLAGS"
            unset SSL_LDFLAGS
        fi
    done
    
    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"

    AC_MSG_CHECKING(if OpenSSL is available)
    AC_MSG_RESULT($HAVE_SSL)

    if test x$HAVE_SSL != xyes; then
        NO_SSL="--  "
    fi
],
[
    NO_SSL="--  "
])

# Convert the space-separated SSL_LDFLAGS into a sequence of string literals
# concatenated with ASCII.NUL separators, which is what prama Linker_Options
# expects.

set_linker_options() {
  while test @S|@# -gt 0; do
    SSL_LINKER_OPTIONS="${SSL_LINKER_OPTIONS}\"@S|@1\""
    if test @S|@# -gt 1; then
      SSL_LINKER_OPTIONS="${SSL_LINKER_OPTIONS} & ASCII.NUL & "
    fi
    shift
  done
  if test -z "${SSL_LINKER_OPTIONS}"; then
    NO_SSL_LINKER_OPTIONS="--  "
  fi
}
set_linker_options $SSL_LDFLAGS
AC_SUBST(SSL_LINKER_OPTIONS)
AC_SUBST(NO_SSL_LINKER_OPTIONS)

AC_SUBST(HAVE_SSL)
AC_SUBST(NO_SSL)
AC_SUBST(OPENSSL)
])dnl
