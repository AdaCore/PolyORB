------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . C O R B A _ P . D O M A I N _ M A N A G E M E N T     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.DomainManager.Helper;
with CORBA.Impl;

with PolyORB.Servants;

package body PolyORB.CORBA_P.Domain_Management is

   -------------------------
   -- Get_Domain_Managers --
   -------------------------

   function Get_Domain_Managers
     (Servant : PortableServer.Servant)
      return CORBA.Any
   is
      Notepad : constant PolyORB.Annotations.Notepad_Access
        := PolyORB.Servants.Notepad_Of
        (CORBA.Impl.To_PolyORB_Servant
         (CORBA.Impl.Object (Servant.all)'Access));
      Note : Domain_Manager_Note;

   begin
      PolyORB.Annotations.Get_Note
        (Notepad.all, Note, Empty_Domain_Manager_Note);

      return CORBA.DomainManager.Helper.To_Any (Note.Domain_Managers);
   end Get_Domain_Managers;

end PolyORB.CORBA_P.Domain_Management;
