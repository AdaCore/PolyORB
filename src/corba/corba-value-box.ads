------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      C O R B A . V A L U E . B O X                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

pragma Ada_2005;

generic
   type Boxed is private;
   type Boxed_Access is access all Boxed;

package CORBA.Value.Box is

   type Box_Ref is new CORBA.Value.Base with private;

   --  function Is_Null (The_Ref : in Box_Ref) return Boolean;
   --  inherited from corba.abstractbase.ref

   function Create (With_Value : Boxed) return Box_Ref;
   function "+" (With_Value : Boxed) return Box_Ref
     renames Create;

   function Contents (The_Boxed : Box_Ref)
     return Boxed_Access;
   function "-" (The_Boxed : Box_Ref) return Boxed_Access
     renames Contents;

   overriding procedure Release (The_Ref : in out Box_Ref);

private

   type Box_Ref is new CORBA.Value.Base with null record;

   type Object is new CORBA.Value.Impl_Base with record
      Content : Boxed_Access;
   end record;

   type Object_Ptr is access all Object;

end CORBA.Value.Box;
