------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.REPRESENTATIONS.CDR.GIOP_1_1                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

with Ada.Unchecked_Deallocation;

with PolyORB.Initialization;
with PolyORB.Representations.CDR.Common;
with PolyORB.Utils.Strings;
with PolyORB.Setup;

package body PolyORB.Representations.CDR.GIOP_1_1 is

   use PolyORB.Errors;
   use PolyORB.GIOP_P.Code_Sets.Converters;
   use PolyORB.Representations.CDR.Common;

   function Create return CDR_Representation_Access;

   procedure Deferred_Initialization;

   procedure Free is
     new Ada.Unchecked_Deallocation (Converter'Class, Converter_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation
          (Wide_Converter'Class, Wide_Converter_Access);

   ------------
   -- Create --
   ------------

   function Create return CDR_Representation_Access is
   begin
      return new GIOP_1_1_CDR_Representation;
   end Create;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      Register_Factory (1, 1, Create'Access);
      PolyORB.Setup.Default_Representation := Representation_Access (Create);
   end Deferred_Initialization;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.Char;
      Error  : in out Errors.Error_Container)
   is
   begin
      if R.C_Converter /= null then
         Marshall (R.C_Converter.all, Buffer, Data, Error);
      else
         --  Backward compatibility mode

         Marshall_Latin_1_Char (Buffer, Data);
      end if;
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.String;
      Error  : in out Errors.Error_Container)
   is
   begin
      if R.C_Converter /= null then
         Marshall (R.C_Converter.all, Buffer, Data, Error);
      else
         --  Backward compatibility mode

         Marshall_Latin_1_String (Buffer, Data);
      end if;
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.Wchar;
      Error  : in out Errors.Error_Container)
   is
   begin
      if R.W_Converter /= null then
         Marshall (R.W_Converter.all, Buffer, Data, Error);
      else
         --  Backward compatibility mode

         Throw
           (Error,
            Marshal_E,
            System_Exception_Members'(5, Completed_No));
         --  XXX Check exception and minor code.
      end if;
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   : PolyORB.Types.Wide_String;
      Error  : in out Errors.Error_Container)
   is
   begin
      if R.W_Converter /= null then
         Marshall (R.W_Converter.all, Buffer, Data, Error);
      else
         --  Backward compatibility mode

         Throw
           (Error,
            Marshal_E,
            System_Exception_Members'(5, Completed_No));
         --  XXX Check exception and minor code.
      end if;
   end Marshall;

   -------------
   -- Release --
   -------------

   procedure Release (R : in out GIOP_1_1_CDR_Representation) is
   begin
      Free (R.C_Converter);
      Free (R.W_Converter);
   end Release;

   --------------------
   -- Set_Converters --
   --------------------

   procedure Set_Converters
     (R : in out GIOP_1_1_CDR_Representation;
      C : PolyORB.GIOP_P.Code_Sets.Converters.Converter_Access;
      W : PolyORB.GIOP_P.Code_Sets.Converters.Wide_Converter_Access)
   is
   begin
      R.C_Converter := C;
      R.W_Converter := W;
   end Set_Converters;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Char;
      Error  : in out Errors.Error_Container)
   is
   begin
      if R.C_Converter /= null then
         Unmarshall (R.C_Converter.all, Buffer, Data, Error);
      else
         --  Backward compatibility mode

         Data := Unmarshall_Latin_1_Char (Buffer);
      end if;
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.String;
      Error  : in out Errors.Error_Container)
   is
   begin
      if R.C_Converter /= null then
         Unmarshall (R.C_Converter.all, Buffer, Data, Error);
      else
         --  Backward compatibility mode

         Data := Unmarshall_Latin_1_String (Buffer);
      end if;
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Wchar;
      Error  : in out Errors.Error_Container)
   is
   begin
      if R.W_Converter /= null then
         Unmarshall (R.W_Converter.all, Buffer, Data, Error);
      else
         --  Backward compatibility mode

         Throw
           (Error,
            Marshal_E,
            System_Exception_Members'(5, Completed_No));
         --  XXX Check exception and minor code.
      end if;
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (R      : GIOP_1_1_CDR_Representation;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out PolyORB.Types.Wide_String;
      Error  : in out Errors.Error_Container)
   is
   begin
      if R.W_Converter /= null then
         Unmarshall (R.W_Converter.all, Buffer, Data, Error);
      else
         --  Backward compatibility mode

         Throw
           (Error,
            Marshal_E,
            System_Exception_Members'(5, Completed_No));
         --  XXX Check exception and minor code.
      end if;
   end Unmarshall;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"representations.cdr.giop_1_1",
          Conflicts => Empty,
          Depends   => Empty,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access,
          Shutdown  => null));
   end;
end PolyORB.Representations.CDR.GIOP_1_1;
