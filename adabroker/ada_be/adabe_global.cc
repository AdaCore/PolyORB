//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.5 $
//                                                                          //
//         Copyright (C) 1999-2000 ENST Paris University, France.           //
//                                                                          //
// AdaBroker is free software; you  can  redistribute  it and/or modify it  //
// under terms of the  GNU General Public License as published by the  Free //
// Software Foundation;  either version 2,  or (at your option)  any  later //
// version. AdaBroker  is distributed  in the hope that it will be  useful, //
// but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- //
// TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public //
// License  for more details.  You should have received  a copy of the GNU  //
// General Public License distributed with AdaBroker; see file COPYING. If  //
// not, write to the Free Software Foundation, 59 Temple Place - Suite 330, //
// Boston, MA 02111-1307, USA.                                              //
//                                                                          //
// As a special exception,  if other files  instantiate  generics from this //
// unit, or you link  this unit with other files  to produce an executable, //
// this  unit  does not  by itself cause  the resulting  executable  to  be //
// covered  by the  GNU  General  Public  License.  This exception does not //
// however invalidate  any other reasons why  the executable file  might be //
// covered by the  GNU Public License.                                      //
//                                                                          //
//             AdaBroker is maintained by ENST Paris University.            //
//                     (email: broker@inf.enst.fr)                          //
//                                                                          //
//--------------------------------------------------------------------------//
#include <adabe.h>

#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>


void 
adabe_global::set_adabe_current_file(adabe_name *new_file) 
{ pd_adabe_current_file = new_file; };
  // to set the current file which is produced 

void
adabe_global::set_root(adabe_root *v) 
{ myself = v; };
  // set the root from the AST
  
adabe_root *
adabe_global::root() 
{ return myself; };
  // which is the root ?

adabe_name*
adabe_global::adabe_current_file() 
{ return pd_adabe_current_file; };
  // which file is the current file which is produced ?

bool 
adabe_global::impl_flags() 
{ return pd_impl_flags; };
  // to produce the implementation files if true

void 
adabe_global::set_impl_flags(bool set_impl) 
{ pd_impl_flags = set_impl; };
  // to set this flags at the parsing (produce the impl?)

bool
adabe_global::force_flags() 
{ return pd_force_flags; };
  // to force the production of the implementation files if true

void 
adabe_global::set_force_flags(bool set_force) 
{ pd_force_flags = set_force; };
  // to set this flags at the parsing (produce the impl?)

bool 
adabe_global::must_create_impl(char *file_name) 
{
  if  (pd_impl_flags)
    {
      if (pd_force_flags) { return true; }
      int n = open(file_name, O_RDONLY | O_CREAT | O_EXCL, 0777);
      if (n != -1) { close(n); return true; }
    }
  return false;
};
