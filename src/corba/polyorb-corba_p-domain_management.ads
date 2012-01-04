------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . C O R B A _ P . D O M A I N _ M A N A G E M E N T     --
--                                                                          --
--                                 S p e c                                  --
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
