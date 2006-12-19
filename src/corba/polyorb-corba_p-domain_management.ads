------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . C O R B A _ P . D O M A I N _ M A N A G E M E N T     --
--                                                                          --
--                                 S p e c                                  --
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

with CORBA.DomainManager;

with PortableServer;

with PolyORB.Annotations;

package PolyORB.CORBA_P.Domain_Management is

   type Domain_Manager_Note is new PolyORB.Annotations.Note with record
      Domain_Managers : CORBA.DomainManager.DomainManagersList;
   end record;

   Empty_Domain_Manager_Note : constant Domain_Manager_Note;

   function Get_Domain_Managers
     (Servant : PortableServer.Servant)
      return CORBA.Any;
   --  Return sequence of domain manager in form of Any.
   --  Implementation Note: this is an idlac helper subprogram.

private

   Empty_Domain_Manager_Note : constant Domain_Manager_Note
     := (PolyORB.Annotations.Note with
         Domain_Managers =>
           CORBA.DomainManager.IDL_SEQUENCE_DomainManager.Null_Sequence);

end PolyORB.CORBA_P.Domain_Management;
