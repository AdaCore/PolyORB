/*

COPYRIGHT

Copyright 1992, 1993, 1994 Sun Microsystems, Inc.  Printed in the United
States of America.  All Rights Reserved.

This product is protected by copyright and distributed under the following
license restricting its use.

The Interface Definition Language Compiler Front End (CFE) is made
available for your use provided that you include this license and copyright
notice on all media and documentation and the software program in which
this product is incorporated in whole or part. You may copy and extend
functionality (but may not remove functionality) of the Interface
Definition Language CFE without charge, but you are not authorized to
license or distribute it to anyone else except as part of a product or
program developed by you or with the express written consent of Sun
Microsystems, Inc. ("Sun").

The names of Sun Microsystems, Inc. and any of its subsidiaries or
affiliates may not be used in advertising or publicity pertaining to
distribution of Interface Definition Language CFE as permitted herein.

This license is effective until terminated by Sun for failure to comply
with this license.  Upon termination, you shall destroy or return all code
and documentation for the Interface Definition Language CFE.

INTERFACE DEFINITION LANGUAGE CFE IS PROVIDED AS IS WITH NO WARRANTIES OF
ANY KIND INCLUDING THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS
FOR A PARTICULAR PURPOSE, NONINFRINGEMENT, OR ARISING FROM A COURSE OF
DEALING, USAGE OR TRADE PRACTICE.

INTERFACE DEFINITION LANGUAGE CFE IS PROVIDED WITH NO SUPPORT AND WITHOUT
ANY OBLIGATION ON THE PART OF Sun OR ANY OF ITS SUBSIDIARIES OR AFFILIATES
TO ASSIST IN ITS USE, CORRECTION, MODIFICATION OR ENHANCEMENT.

SUN OR ANY OF ITS SUBSIDIARIES OR AFFILIATES SHALL HAVE NO LIABILITY WITH
RESPECT TO THE INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY
INTERFACE DEFINITION LANGUAGE CFE OR ANY PART THEREOF.

IN NO EVENT WILL SUN OR ANY OF ITS SUBSIDIARIES OR AFFILIATES BE LIABLE FOR
ANY LOST REVENUE OR PROFITS OR OTHER SPECIAL, INDIRECT AND CONSEQUENTIAL
DAMAGES, EVEN IF SUN HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

Use, duplication, or disclosure by the government is subject to
restrictions as set forth in subparagraph (c)(1)(ii) of the Rights in
Technical Data and Computer Software clause at DFARS 252.227-7013 and FAR
52.227-19.

Sun, Sun Microsystems and the Sun logo are trademarks or registered
trademarks of Sun Microsystems, Inc.

SunSoft, Inc.  
2550 Garcia Avenue 
Mountain View, California  94043

NOTE:

SunOS, SunSoft, Sun, Solaris, Sun Microsystems or the Sun logo are
trademarks or registered trademarks of Sun Microsystems, Inc.

 */

#ifndef _IDL_IDL_HH
#define _IDL_IDL_HH

// idl.hh
//
// Main include file for IDL compiler. Includes the rest of the
// files defining the different units of the compiler

#if defined(__DECCXX)
// DEC C++ compiler
#if __DECCXX_VER >= 60000000
#define HAS_Cplusplus_Namespace
#define HAS_Std_Namespace
#define __USE_STD_IOSTREAM 
#define HAS_pch
#if defined(__VMS)
#pragma message disable codeunreachable
#endif
#elif __DECCXX_VER >= 50600000
#define HAS_Cplusplus_Namespace
#endif

#elif defined(_MSC_VER)
//  Microsoft Visual C++ compiler
#if _MSC_VER >= 1000
#define HAS_Cplusplus_Namespace
#define HAS_Std_Namespace
#pragma once
#endif

#endif

#include	<intlmacros.hh>		// Define macros for intl'ion

#include	<stdlib.h>		// POSIX standard defns
#include	<string.h>		// POSIX string funcs
#if defined(__aix__) || defined(__SINIX__)
#include        <strings.h>
#endif
#include	<ctype.h>		// External functions

#ifdef HAS_Std_Namespace
#include <iostream>
#include <fstream>
#else
#include <iostream.h>
#include <fstream.h>
#ifndef std
#define std
#endif
#endif

#include	<sys/types.h>		// POSIX standard types

#include	<idl_narrow.hh>		// IDL Narrowing mechanism

#include	<idl_defines.hh>	// Constants for IDL compiler
#include	<idl_fwd.hh>		// Forward decls of classes

#include	<idl_bool.hh>		// Defines boolean for IDL

#include	<utl_string.hh>		// Defines utility string class
#include	<utl_identifier.hh>	// Defines utility identifier class
#include	<ast.hh>		// AST classes
#include	<util.hh>		// Utility classes

#endif           // _IDL_IDL_HH
