------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              S Y S T E M . S T A N D A R D _ L I B R A R Y               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package is included in all programs. It contains declarations that
--  are required to be part of every Ada program. A special mechanism is
--  required to ensure that these are loaded, since it may be the case in
--  some programs that the only references to these required packages are
--  from C code or from code generated directly by Gigi, an in both cases
--  the binder is not aware of such references.

--  System.Standard_Library also includes data that must be present in every
--  program, in particular the definitions of all the standard and also some
--  subprograms that must be present in every program.

--  The binder unconditionally includes s-stalib.ali, which ensures that this
--  package and the packages it references are included in all Ada programs,
--  together with the included data.

with System;
with Unchecked_Conversion;

package System.Standard_Library is

   pragma Suppress (All_Checks);
   --  Suppress explicitely all the checks to work around the Solaris linker
   --  bug when using gnatmake -f -a (but without -gnatp). This is not needed
   --  with Solaris 2.6, so eventually can be removed ???

   type Big_String_Ptr is access all String (Positive);
   --  A non-fat pointer type for null terminated strings

   function To_Ptr is
     new Unchecked_Conversion (System.Address, Big_String_Ptr);

   -------------------------------------
   -- Exception Declarations and Data --
   -------------------------------------

   type Exception_Data;
   type Exception_Data_Ptr is access all Exception_Data;
   --  an equivalent of Exception_Id that is public

   --  The following record defines the underlying representation of exceptions

   --  WARNING! Any changes to this may need to be reflectd in the following
   --  locations in the compiler and runtime code:

   --    1. The Internal_Exception routine in s-exctab.adb
   --    2. The processing in gigi that tests Not_Handled_By_Others
   --    3. Expand_N_Exception_Declaration in Exp_Ch11
   --    4. The construction of the exception type in Cstand

   type Exception_Data is record
      Not_Handled_By_Others : Boolean;
      --  Normally set False, indicating that the exception is handled in the
      --  usual way by others (i.e. an others handler handles the exception).
      --  Set True to indicate that this exception is not caught by others
      --  handlers, but must be explicitly named in a handler. This latter
      --  setting is currently used by the Abort_Signal.

      Lang : String (1 .. 3);
      --  A 3-character string indicating the language raising the exception.
      --  This field is not currently used, it is set to "Ada" for exceptions
      --  defined by an Ada program.

      Name_Length : Natural;
      --  Length of fully expanded name of exception

      Full_Name : Big_String_Ptr;
      --  Fully expanded name of exception, null terminated

      HTable_Ptr : Exception_Data_Ptr;
      --  Hash table pointer used to link entries together in the hash table
      --  built (by Register_Exception in s-exctab.adb) for converting between
      --  identities and names.

      Import_Code : Integer;
      --  Value for imported exceptions. Needed only for the handling of
      --  Import/Export_Exception for the VMS case, but present in all
      --  implementations (we might well extend this mechanism for other
      --  systems in the future).

   end record;

   --  Definitions for standard predefined exceptions defined in Standard,

   --  Why are the Nul's necessary here, seems like they should not be
   --  required, since Gigi is supposed to add a Nul to each name ???

   Constraint_Error_Name : constant String := "CONSTRAINT_ERROR" & Ascii.Nul;
   Program_Error_Name    : constant String := "PROGRAM_ERROR"    & Ascii.Nul;
   Storage_Error_Name    : constant String := "STORAGE_ERROR"    & Ascii.Nul;
   Tasking_Error_Name    : constant String := "TASKING_ERROR"    & Ascii.Nul;
   Abort_Signal_Name     : constant String := "_ABORT_SIGNAL"    & Ascii.Nul;

   Numeric_Error_Name    : constant String := "NUMERIC_ERROR"    & Ascii.Nul;
   --  This is used only in the Ada 83 case, but it is not worth having a
   --  separate version of s-stalib.ads for use in Ada 83 mode.

   Constraint_Error_Def : aliased Exception_Data :=
     (Not_Handled_By_Others => False,
      Lang                  => "Ada",
      Name_Length           => Constraint_Error_Name'Length,
      Full_Name             => To_Ptr (Constraint_Error_Name'Address),
      HTable_Ptr            => null,
      Import_Code           => 0);

   Numeric_Error_Def : aliased Exception_Data :=
     (Not_Handled_By_Others => False,
      Lang                  => "Ada",
      Name_Length           => Numeric_Error_Name'Length,
      Full_Name             => To_Ptr (Numeric_Error_Name'Address),
      HTable_Ptr            => null,
      Import_Code           => 0);

   Program_Error_Def : aliased Exception_Data :=
     (Not_Handled_By_Others => False,
      Lang                  => "Ada",
      Name_Length           => Program_Error_Name'Length,
      Full_Name             => To_Ptr (Program_Error_Name'Address),
      HTable_Ptr            => null,
      Import_Code           => 0);

   Storage_Error_Def : aliased Exception_Data :=
     (Not_Handled_By_Others => False,
      Lang                  => "Ada",
      Name_Length           => Storage_Error_Name'Length,
      Full_Name             => To_Ptr (Storage_Error_Name'Address),
      HTable_Ptr            => null,
      Import_Code           => 0);

   Tasking_Error_Def : aliased Exception_Data :=
     (Not_Handled_By_Others => False,
      Lang                  => "Ada",
      Name_Length           => Tasking_Error_Name'Length,
      Full_Name             => To_Ptr (Tasking_Error_Name'Address),
      HTable_Ptr            => null,
      Import_Code           => 0);

   Abort_Signal_Def : aliased Exception_Data :=
     (Not_Handled_By_Others => True,
      Lang                  => "Ada",
      Name_Length           => Abort_Signal_Name'Length,
      Full_Name             => To_Ptr (Abort_Signal_Name'Address),
      HTable_Ptr            => null,
      Import_Code           => 0);

   pragma Export (C, Constraint_Error_Def, "constraint_error");
   pragma Export (C, Numeric_Error_Def,    "numeric_error");
   pragma Export (C, Program_Error_Def,    "program_error");
   pragma Export (C, Storage_Error_Def,    "storage_error");
   pragma Export (C, Tasking_Error_Def,    "tasking_error");
   pragma Export (C, Abort_Signal_Def,     "_abort_signal");

   Local_Partition_ID : Natural := 0;
   --  This variable contains the local Partition_ID that will be used when
   --  building exception occurrences. In distributed mode, it will be
   --  set by each partition to the correct value during the elaboration.

   -----------------
   -- Subprograms --
   -----------------

   procedure Abort_Undefer_Direct;
   pragma Inline (Abort_Undefer_Direct);
   --  A little procedure that just calls Abort_Undefer.all, for use in
   --  clean up procedures, which only permit a simple subprogram name.

end System.Standard_Library;
