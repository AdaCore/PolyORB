------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . U T I L S . S R P                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Utilities for the Simple Request Protocol.

--  $Id$

with Ada.Unchecked_Deallocation;

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

   procedure Free_Arg_Info is new Ada.Unchecked_Deallocation
     (Arg_Info, Arg_Info_Ptr);

end PolyORB.Utils.SRP;
