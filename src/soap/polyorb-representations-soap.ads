------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . R E P R E S E N T A T I O N S . S O A P          --
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

--  $Id$

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Sequences.Unbounded;

with PolyORB.Any;
with PolyORB.Storage_Pools;
with PolyORB.Types;

package PolyORB.Representations.SOAP is

   pragma Elaborate_Body;

   use PolyORB.Any;
   use PolyORB.Types;

   type XML_String is new Types.String;

   XML_Null_String  : constant XML_String :=
         XML_String (Null_Unbounded_String);

   type Empty_XML_Component is private;

   type XML_Component is tagged private;
   type XML_Component_Access is access all XML_Component'Class;
   for XML_Component_Access'Storage_Pool use PolyORB.Storage_Pools.Debug_Pool;

   type Child_List_Record is private;
   type Child_List_Access is access all Child_List_Record;
   for Child_List_Access'Storage_Pool use PolyORB.Storage_Pools.Debug_Pool;

   type Attributes_Record is private;
   type Attributes_Record_Access is access all Attributes_Record;
   for Attributes_Record_Access'Storage_Pool
     use PolyORB.Storage_Pools.Debug_Pool;

   package Attributes_Seq is new Sequences.Unbounded
       (Attributes_Record_Access);
   type Attributes_Seq_Access is access all Attributes_Seq.Sequence;
   for Attributes_Seq_Access'Storage_Pool use PolyORB.Storage_Pools.Debug_Pool;

   type Container_Element is private;
   type Container_Element_Access is access all Container_Element;
   for Container_Element_Access'Storage_Pool
     use PolyORB.Storage_Pools.Debug_Pool;

   type Stream_Char is record
      Current_Pos   : Integer := 0;
      Chars         : XML_String;
   end record;

   type Stream_Char_Access is access all Stream_Char;
   for Stream_Char_Access'Storage_Pool use PolyORB.Storage_Pools.Debug_Pool;

   type Name_Array is array (Positive range <>) of XML_String;
   type Name_Array_Access is access Name_Array;
   for Name_Array_Access'Storage_Pool use PolyORB.Storage_Pools.Debug_Pool;

   type Xsd_Types is (Xsd_Simple, Xsd_Struct, Xsd_Array);

   type Xsd_Kind  is  (Xsd_Short, Xsd_Int, Xsd_Long, Xsd_Unsigned_Short,
        Xsd_Unsigned_Int, Xsd_Unsigned_Long, Xsd_Float, Xsd_Double,
        Xsd_Boolean, Xsd_Char, Xsd_Byte, Xsd_String, Xsd_Array, Xsd_Struct,
        Xsd_Undefined);

   PolyORB_Types_To_XML_Types :
      constant array (TCKind'Range) of Xsd_Kind
       := (Tk_Short => Xsd_Short,
           Tk_Long  => Xsd_Int,
           Tk_Ushort => Xsd_Unsigned_Short,
           Tk_Ulong => Xsd_Unsigned_Int,
           Tk_Float => Xsd_Float,
           Tk_Double => Xsd_Double,
           Tk_Boolean => Xsd_Boolean,
           Tk_Char => Xsd_Char,
           Tk_Octet => Xsd_Byte,
           Tk_String =>  Xsd_String,
           Tk_Longlong => Xsd_Long,
           Tk_Ulonglong => Xsd_Unsigned_Long,
           others => Xsd_Undefined);

   PolyORB_Types_To_Xsd_Strings :
    constant array (Xsd_Kind'Range) of PolyORB.Types.String
     := (Xsd_Short => To_PolyORB_String ("short"),
         Xsd_Int  => To_PolyORB_String ("int"),
         Xsd_Unsigned_Short => To_PolyORB_String ("unsigned short"),
         Xsd_Unsigned_Int => To_PolyORB_String ("unsigned int"),
         Xsd_Float => To_PolyORB_String ("float"),
         Xsd_Double => To_PolyORB_String ("double"),
         Xsd_Boolean => To_PolyORB_String ("boolean"),
         Xsd_Byte  => To_PolyORB_String ("byte"),
         Xsd_String =>  To_PolyORB_String ("string"),
         Xsd_Long => To_PolyORB_String ("long"),
         Xsd_Unsigned_Long => To_PolyORB_String ("unsigned long"),
         others => To_PolyORB_String ("undefined"));

   SOAP_Error : exception;
   SOAP_Syntax_Fault : exception;
   Unexpected_Token : exception;

   -----------------------
   -- Utility functions --
   -----------------------

   function Split_Name (Name : XML_String)
      return Name_Array_Access;

   function First_Name (Name : XML_String)
      return XML_String;

   function Last_Name (Name : XML_String)
      return XML_String;

   function Erase_Space (S : String)
      return String;


   ---------------------------------------------
   --   Function to handle incoming XML strings
   --   and To Convert them to XML Component
   ---------------------------------------------


   function Get (Str : Stream_Char_Access)
       return Character;

   function Peek (Str : Stream_Char_Access)
       return Character;

   function End_Of_Input (Str : Stream_Char_Access)
       return Boolean;

   -----------------------------------------
   --   Procedures to convert simple types to
   --   XML Component
   ----------------------------------------

   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.Short;
       XML_Comp : out XML_Component_Access);

   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.Long;
       XML_Comp : out XML_Component_Access);

   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.Long_Long;
       XML_Comp : out XML_Component_Access);

   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.Unsigned_Short;
       XML_Comp : out XML_Component_Access);

   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.Unsigned_Long;
       XML_Comp : out XML_Component_Access);

   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.Unsigned_Long_Long;
       XML_Comp : out  XML_Component_Access);

   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.Float;
       XML_Comp : out  XML_Component_Access);

   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.Double;
       XML_Comp : out  XML_Component_Access);

   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.Boolean;
       XML_Comp : out  XML_Component_Access);

   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.Char;
       XML_Comp : out XML_Component_Access);

   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.Octet;
       XML_Comp : out XML_Component_Access);

   procedure To_XML_Component
      (Name : Types.Identifier;
       Arg  : Types.String;
       XML_Comp : out XML_Component_Access);

   function To_XML_String
      (XML_Comp : XML_Component_Access)
      return XML_String;


   -----------------------------------
   ----------------------------------

   procedure Set_Parent (Child : in out XML_Component_Access;
                          Pt : XML_Component_Access);

   procedure Add_Comp_List
      (Container_Elt : in out Container_Element_Access;
       XML_Elt  : XML_Component_Access);

   procedure Add_Child
      (Comp    : in out XML_Component_Access;
       Child   : XML_Component_Access);

   procedure Tree_Course
      (Comp : in XML_Component_Access;
       Container : in out Container_Element_Access);

   ------------------------------------
   --   Primitives to manipulating and extracting information
   --   from the XML_Component type
   -------------------------------------------

   function Tag
       (Comp : XML_Component_Access)
      return XML_String;

   function Content_Value
      (Comp : XML_Component_Access)
      return XML_String;

   function Content_Type
       (Comp : XML_Component_Access)
      return Xsd_Types;

   procedure Add_Attributes
       (Comp :  in out XML_Component_Access;
        Id   :  XML_String;
        Val  :  XML_String);

   function Get_Attributes
       (Comp : XML_Component_Access)
       return Attributes_Seq.Sequence;

   function Attributes
       (Comp : XML_Component_Access;
        Attr_Tag : XML_String)
      return XML_String;

   function XMLNS
       (Comp : XML_Component_Access;
        Name : XML_String)
      return XML_String;

   procedure Initialize
     (Comp : XML_Component_Access;
      Tag  : XML_String;
      Value : XML_String := XML_Null_String;
      Comp_Type : Xsd_Types := Xsd_Simple;
      Is_Method : Boolean := False);

   function Nieme_Child
        (Comp : XML_Component_Access;
         N    : Positive)
     return XML_Component_Access;

   function First_Element
       (Comp : XML_Component_Access)
     return XML_Component_Access;

   procedure Element_Header
    (Parent : XML_Component_Access;
     Tag    : XML_String;
     Head   : out XML_Component_Access);

   procedure Element_Body
    (Parent : XML_Component_Access;
     Tag    : XML_String;
     Head   : out XML_Component_Access);

   ----------------------------------
   --  Parse XML Incoming Strings
   ------------------------------------

   function Next_Token (Str : Stream_Char_Access)
     return XML_String;

   procedure Parse_Tag
      (XML_Comp : in out XML_Component_Access;
       Str      : Stream_Char_Access);

   procedure Parse_Component
      (XML_Comp : in out XML_Component_Access;
       Str      : Stream_Char_Access);

   procedure XML_Parse
      (XML_Comp : in out XML_Component_Access;
       Str      : Stream_Char_Access);

   ----------------------------------------
   --- Get a XML_Component from XML String
   ---------------------------------------
   function Get (this : XML_Component_Access;
                name : in XML_String)
      return XML_Component_Access;

   --------------------------------
   --   Release functions
   ---------------------------------

   procedure Release (Rec : in out Child_List_Access);

   procedure Release
      (Container : in out Container_Element_Access);

   procedure Release
      (Comp : in out XML_Component_Access);


private

   type Child_List_Record is record
       Item : XML_Component_Access := null;
       Next : Child_List_Access := null;
   end record;

   type Container_Element is record
       Nbr_Of_Items : Integer := 0;
       Head         : Child_List_Access := null;
       Tail         : Child_List_Access := null;
   end record;


   type Attributes_Record is record
       Tag_Id : XML_String := XML_Null_String;
       Value  : XML_String := XML_Null_String;
   end record;


   type Empty_XML_Component is tagged record
      Tag  : XML_String := XML_Null_String;
      Component_Type : Xsd_Types := Xsd_Simple;
      Is_Method  : Boolean := False;
      Parent  : XML_Component_Access := null;
      Childs  : Container_Element_Access := null;
   end record;


   type XML_Component is new Empty_XML_Component with record
      Value  : XML_String := XML_Null_String;
      Attributes : Attributes_Seq.Sequence :=
                  Attributes_Seq.Null_Sequence;
      Empty : Boolean := False;
   end record;



   Name_Space_Uri : XML_String;

   Array_Tag : constant XML_String := To_PolyORB_String
               ("SOAP-ENC:Array");

   Array_Type_Tag : constant XML_String := To_PolyORB_String
               ("SOAP-ENC:ArrayType=");

   Array_Member_Tag :  constant XML_String := To_PolyORB_String
               ("SOAP-ENC:");


   Ur_Type_Tag : constant XML_String := To_PolyORB_String
               ("SOAP-ENC:ur-type");


   Namespace_Tag : constant XML_String := To_PolyORB_String
               ("xmlns:");

end PolyORB.Representations.SOAP;



