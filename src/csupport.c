/*****************************************************************************
**                                                                          **
**                           POLYORB COMPONENTS                             **
**                                                                          **
**                            C S U P P O R T                               **
**                                                                          **
**                       C   s u p p o r t   f i l e                        **
**                                                                          **
**           Copyright (C) 2008, Free Software Foundation, Inc.             **
**                                                                          **
** PolyORB is free software; you  can  redistribute  it and/or modify it    **
** under terms of the  GNU General Public License as published by the  Free **
** Software Foundation;  either version 2,  or (at your option)  any  later **
** version. PolyORB is distributed  in the hope that it will be  useful,    **
** but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- **
** TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public **
** License  for more details.  You should have received  a copy of the GNU  **
** General Public License distributed with PolyORB; see file COPYING. If    **
** not, write to the Free Software Foundation, 59 Temple Place - Suite 330, **
** Boston, MA 02111-1307, USA.                                              **
**                                                                          **
**                  PolyORB is maintained by AdaCore                        **
**                     (email: sales@adacore.com)                           **
**                                                                          **
*****************************************************************************/

/* C support functions for PolyORB */

#include "config.h"
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

void
__PolyORB_detach(void) {
#ifdef HAVE_SETSID
   int devnull_fd = open ("/dev/null", O_RDWR);
   if (devnull_fd < 0)
      return;

   (void) dup2 (devnull_fd, 0);
   (void) dup2 (devnull_fd, 1);
   (void) dup2 (devnull_fd, 2);
   (void) setsid ();
#endif
}
