AC_DEFUN([AM_WITH_OPENSSL],
[AC_MSG_CHECKING(for OpenSSL library)
AC_ARG_WITH(openssl,
  AC_HELP_STRING([--with-openssl@<:@=ARG@:>@],
		 [enable SSL support, will check ARG/ssl /usr/local/ssl /usr/lib/ssl /usr/ssl
		  /usr/pkg /usr/local /usr]),
[
    AC_MSG_RESULT(yes)
    for dir in $withval /usr/local/ssl /usr/lib/ssl /usr/ssl /usr/pkg /usr/local /usr; do
        ssldir="$dir"
        if test -f "$dir/include/openssl/ssl.h"; then
            found_ssl="yes";
            CFLAGS="$CFLAGS -I$ssldir/include/openssl -DHAVE_SSL";
            #CXXFLAGS="$CXXFLAGS -I$ssldir/include/openssl -DHAVE_SSL";
            break;
        fi
        if test -f "$dir/include/ssl.h"; then
            found_ssl="yes";
            CFLAGS="$CFLAGS -I$ssldir/include/ -DHAVE_SSL";
            #CXXFLAGS="$CXXFLAGS -I$ssldir/include/ -DHAVE_SSL";
            break
        fi
    done
    if test x$found_ssl != xyes; then
        AC_MSG_ERROR(Cannot find SSL libraries)
    else
        echo "... OpenSSL found in $ssldir";
        #LIBS="$LIBS -lssl -lcrypto";
        LDFLAGS="$LDFLAGS -L$ssldir/lib";
        HAVE_SSL=yes
    fi
    AC_SUBST(HAVE_SSL)
],
[
    AC_MSG_RESULT(no)
])
])dnl
