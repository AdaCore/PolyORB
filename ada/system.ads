------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S Y S T E M                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  Note: although the values in System are target dependent, the source of
--  the package System itself is target independent in GNAT. This is achieved
--  by using attributes for all values, including the special additional GNAT
--  Standard attributes that are provided for exactly this purpose.

package System is
pragma Pure (System);
--  Note that we take advantage of the implementation permission to
--  make this unit Pure instead of Preelaborable, see RM 13.7(36)

   type Name is (SYSTEM_NAME_GNAT);
   System_Name : constant Name := SYSTEM_NAME_GNAT;

   --  System-Dependent Named Numbers

   Min_Int                : constant := Long_Long_Integer'First;
   Max_Int                : constant := Long_Long_Integer'Last;

   Max_Binary_Modulus     : constant := 2 ** Long_Long_Integer'Size;
   Max_Nonbinary_Modulus  : constant := Integer'Last;

   Max_Base_Digits        : constant := Long_Long_Float'Digits;
   Max_Digits             : constant := Long_Long_Float'Digits;

   Max_Mantissa           : constant := 63;
   Fine_Delta             : constant := 2.0 ** (-Max_Mantissa);

   Tick                   : constant := Standard'Tick;

   --  Storage-related Declarations

   type Address is private;
   Null_Address : constant Address;

   Storage_Unit           : constant := Standard'Storage_Unit;
   Word_Size              : constant := Standard'Word_Size;
   Memory_Size            : constant := 2 ** Standard'Address_Size;

   --  Address comparison

   function "<"  (Left, Right : Address) return Boolean;
   function "<=" (Left, Right : Address) return Boolean;
   function ">"  (Left, Right : Address) return Boolean;
   function ">=" (Left, Right : Address) return Boolean;
   function "="  (Left, Right : Address) return Boolean;

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "=");

   --  Other System-Dependent Declarations

   type Bit_Order is (High_Order_First, Low_Order_First);
   Default_Bit_Order : constant Bit_Order;

   --  Priority-related Declarations (RM D.1)

   subtype Any_Priority is Integer
     range 0 .. Standard'Max_Interrupt_Priority;

   subtype Priority is Any_Priority
     range 0 .. Standard'Max_Priority;

   subtype Interrupt_Priority is Any_Priority
     range Standard'Max_Priority + 1 .. Standard'Max_Interrupt_Priority;

   Default_Priority : constant Priority :=
     (Priority'First + Priority'Last) / 2;

private

   type Address is mod Memory_Size;
   Null_Address : constant Address := 0;

   Default_Bit_Order : constant Bit_Order :=
                         Bit_Order'Val (Standard'Default_Bit_Order);

end System;
