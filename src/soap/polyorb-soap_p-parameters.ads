------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . S O A P _ P . P A R A M E T E R S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Any;
with PolyORB.Any.NVList;

package PolyORB.SOAP_P.Parameters is

   type List is new PolyORB.Any.NVList.Ref with null record;

   function Argument_Count (P : List) return Natural;
   --  Returns the number of parameters in P.

   function Argument (P : List; Name : String)
     return PolyORB.Any.NamedValue;
   --  Returns parameters named Name in P. Raises SOAP.Types.Data_Error if not
   --  found.

   function Argument (P : List; N : Positive)
     return PolyORB.Any.NamedValue;
   --  Returns Nth parameters in P. Raises SOAP.Types.Data_Error if not found.

   function Exist (P : List; Name : String) return Boolean;
   --  Returns True if parameter named Name exist in P and False otherwise.

   function Get (P : List; Name : String) return Integer;
   --  Returns parameter named Name in P as an Integer value. Raises
   --  SOAP.Types.Data_Error if this parameter does not exist or
   --  is not an Integer.

   function Get (P : List; Name : String) return Long_Float;
   --  Returns parameter named Name in P as a Float value. Raises
   --  SOAP.Types.Data_Error if this parameter does not exist or
   --  is not a Float.

   function Get (P : List; Name : String) return String;
   --  Returns parameter named Name in P as a String value. Raises
   --  SOAP.Types.Data_Error if this parameter does not exist or
   --  is not a String.

   function Get (P : List; Name : String) return Boolean;
   --  Returns parameter named Name in P as a Boolean value. Raises
   --  SOAP.Types.Data_Error if this parameter does not exist or
   --  is not a Boolean.

--    function Get (P : List; Name : String) return Types.SOAP_Record;
--    --  Returns parameter named Name in P as a SOAP Struct value. Raises
--    --  SOAP.Types.Data_Error if this parameter does not exist or
--    --  is not a SOAP Struct.

--    function Get (P : List; Name : String) return Types.SOAP_Array;
--    --  Returns parameter named Name in P as a SOAP Array value. Raises
--    --  SOAP.Types.Data_Error if this parameter does not exist or is
--    --  not a SOAP Array.

   ------------------
   -- Constructors --
   ------------------

   function "&" (P : List; O : PolyORB.Any.NamedValue)
     return List;
   function "+" (O : PolyORB.Any.NamedValue)
     return List;

   ----------------
   -- Validation --
   ----------------

   procedure Check (P : List; N : Natural);
   --  Checks that there is exactly N parameters or raise
   --  SOAP.Types.Data_Error.

   procedure Check_Integer (P : List; Name : String);
   --  Checks that parameter named Name exist and is an Integer value.

   procedure Check_Float (P : List; Name : String);
   --  Checks that parameter named Name exist and is a Float value.

   procedure Check_Boolean (P : List; Name : String);
   --  Checks that parameter named Name exist and is a Boolean value.

   procedure Check_Time_Instant (P : List; Name : String);
   --  Checks that parameter named Name exist and is a Time_Instant value.

   procedure Check_Base64 (P : List; Name : String);
   --  Checks that parameter named Name exist and is a Base64 value.

   procedure Check_Null (P : List; Name : String);
   --  Checks that parameter named Name exist and is a Null value.

   procedure Check_Record (P : List; Name : String);
   --  Checks that parameter named Name exist and is a Record value.

   procedure Check_Array (P : List; Name : String);
   --  Checks that parameter named Name exist and is an Array value.

end PolyORB.SOAP_P.Parameters;
