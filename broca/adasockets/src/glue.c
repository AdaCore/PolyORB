/*
 * $ReleaseVersion$
 *
 * This file implements the binding between the Ada code and the system. For portability
 * reasons, it is much simpler to use the preprocessor to implement socket calls in C
 * rather than trying to interface with all the flavours directly in Ada.
 *
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

static
struct hostent *
