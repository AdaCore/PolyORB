--  Utilities for the Simple Request Protocol.

--  $Id$

with Ada.Unchecked_Deallocation;

with Droopi.Any;
with Droopi.Objects; use Droopi.Objects;
with Droopi.Types; use Droopi.Types;
--  with Droopi.Types; use Droopi.Types;

package Droopi.Utils.SRP is

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

end Droopi.Utils.SRP;
