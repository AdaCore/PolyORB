------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SERVICES.NAMING.NAMINGCONTEXT                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Exceptions;

package body PolyORB.Services.Naming.NamingContext is

   ------------------
   --  Get_Members --
   ------------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out NotFound_Members) is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out CannotProceed_Members) is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidName_Members) is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out AlreadyBound_Members) is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out NotEmpty_Members) is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

end PolyORB.Services.Naming.NamingContext;
