------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . C O R B A _ P . D O M A I N _ M A N A G E M E N T     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
