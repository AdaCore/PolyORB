------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . U T I L S . S R P                     --
--                                                                          --
--                                 S p e c                                  --
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

--  Utilities for the Simple Request Protocol.

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Any;
with PolyORB.Objects; use PolyORB.Objects;
with PolyORB.Types; use PolyORB.Types;

package PolyORB.Utils.SRP is

   pragma Elaborate_Body;

   Unmarshall_Error : exception;
   Deprecated       : exception;

   --  Record use to store the URL when it is splitted
   type Arg_Info;
   type Arg_Info_Ptr is access Arg_Info;
   type Arg_Info is record
      Name  : String_Ptr;
      Value : String_Ptr;
--      Value : Any.Any;
      Next  : Arg_Info_Ptr := null;
   end record;
   --  XXX should be reimplemented in terms of
   --  PolyORB.Utils.Chained_Lists.

   --  Record use to store the URL when it is splitted
   type Split_SRP is record
      Method : String_Ptr;
      Oid    : Object_Id_Access;
      Args   : Arg_Info_Ptr;
   end record;

   --  Set the Method in the SRP information structure
   procedure Set_SRP_Method (Method : Types.String;
                             SRP_Info : in out Split_SRP);

   --  Set the Object Id in the SRP information structure
   procedure Set_SRP_Oid (Oid : Object_Id; SRP_Info : in out Split_SRP);

   --  Set an argument in the SRP information structure
   procedure Set_SRP_Arg (Name : Types.String;
                          Value : Any.Any;
                          SRP_Info : in out Split_SRP);

   --  Split the incoming string in according to the SRP protocol
   function Split (S : Types.String) return Split_SRP;

   --  Same as above, but takes an Any.Any as an input parameter
   --  function Split (Data : Any.Any) return Split_SRP;

   --  Does just the reverse of Split
   function Join (Data : Split_SRP) return Any.Any;

   procedure Free_Arg_Info is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Arg_Info,
      Name => Arg_Info_Ptr);

end PolyORB.Utils.SRP;
