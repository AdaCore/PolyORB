
--  This package is wrapped around a C++ class whose name is
--  Ada_memBufferedStream. (see Ada_memBufferedStream.hh) It provides two
--  types of methods : the C functions of the Ada_memBufferedStream class
--  and their equivalent in Ada. (he first ones have a C_ prefix.)  In
--  addition, there is a raise_ada_exception function that allows C
--  functions to raise the ada No_Initialisation exception.  At last, there
--  is only one Init procedure in place of two in Ada_memBufferedStream
--  since the second one is useless for AdaBroker.


with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Characters.Latin_1;
with Interfaces.C;

with CORBA;

package body MemBufferedStream is

   use type Ada.Strings.Unbounded.Unbounded_String;
   use type CORBA.String;
   use type CORBA.Unsigned_Long;

   ------------
   -- C_Init --
   ------------

   procedure C_Init
     (Self    : in Object'Class;
      Bufsize : in Interfaces.C.unsigned_long);

   pragma Import (CPP, C_Init, "Init__21Ada_memBufferedStreamUi");
   --  Wrapper around Ada_MemBufferedStream function Init (see
   --  Ada_MemBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : Init

   ----------
   -- Init --
   ----------

   procedure Init
     (Self      : in Object'Class;
      Bufsize   : in CORBA.Unsigned_Long)
   is
      C_Bufsize : Interfaces.C.unsigned_long;
   begin
      --  Transform the arguments into a C type ...
      C_Bufsize := Interfaces.C.unsigned_long (Bufsize);

      --  Call the C procedure
      C_Init (Self, C_Bufsize);
   end Init;

   ------------------
   -- C_Marshall_1 --
   ------------------

   procedure C_Marshall_1
     (A : in Interfaces.C.char;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Marshall_1,
      "marshall__21Ada_memBufferedStreamUcR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : Marshall

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (A : in CORBA.Char;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.char;
   begin
      --  Transform the arguments in a C type ...
      C_A := Interfaces.C.char (A);

      --  Call the C procedure
      C_Marshall_1 (C_A, S);
   end Marshall;

   --------------------
   -- C_Unmarshall_1 --
   --------------------

   procedure C_Unmarshall_1
     (A : out Interfaces.C.char;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Unmarshall_1,
      "unmarshall__21Ada_memBufferedStreamRUcR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : UnMarshall


   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (A : out CORBA.Char;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.char;
   begin
      C_Unmarshall_1 (C_A, S);
      A := CORBA.Char (C_A);
   end Unmarshall;

   ------------------
   -- C_Marshall_2 --
   ------------------
   procedure C_Marshall_2
     (A : in Sys_Dep.C_Boolean;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Marshall_2,
      "marshall__21Ada_memBufferedStreambR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : Marshall

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (A : in CORBA.Boolean;
      S : in out Object'Class)
   is
      C_A : Sys_Dep.C_Boolean;
   begin
      --  Transform the arguments in a C type ...
      C_A := Sys_Dep.Boolean_Ada_To_C (A);

      --  Call the C procedure
      C_Marshall_2 (C_A, S);
   end Marshall;

   --------------------
   -- C_UnMarshall_2 --
   --------------------

   procedure C_UnMarshall_2
     (A : out Sys_Dep.C_Boolean;
      S : in out Object'Class);

   pragma Import
     (CPP, C_UnMarshall_2,
      "unmarshall__21Ada_memBufferedStreamRbR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : UnMarshall

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (A : out CORBA.Boolean;
      S : in out Object'Class)
   is
      C_A : Sys_Dep.C_Boolean;
   begin
      C_UnMarshall_2 (C_A, S);
      A := Sys_Dep.Boolean_C_To_Ada (C_A);
   end Unmarshall;

   ------------------
   -- C_Marshall_3 --
   ------------------

   procedure C_Marshall_3
     (A : in Interfaces.C.short;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Marshall_3,
      "marshall__21Ada_memBufferedStreamsR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : Marshall

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (A : in CORBA.Short;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.short;
   begin
      --  Tranform the arguments in a C type ...
      C_A := Interfaces.C.short (C_A);
      --  Call the C procedure
      C_Marshall_3 (C_A, S);
   end Marshall;

   --------------------
   -- C_Unmarshall_3 --
   --------------------

   procedure C_Unmarshall_3
     (A : out Interfaces.C.short;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Unmarshall_3,
      "unmarshall__21Ada_memBufferedStreamRsR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : UnMarshall

   ----------------
   -- UnMarshall --
   ----------------

   procedure Unmarshall
     (A : out CORBA.Short;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.short;
   begin
      C_Unmarshall_3 (C_A, S);
      A := CORBA.Short (C_A);
   end Unmarshall;

   ------------------
   -- C_Marshall_4 --
   ------------------

   procedure C_Marshall_4
     (A : in Interfaces.C.unsigned_short;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Marshall_4,
      "marshall__21Ada_memBufferedStreamUsR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : Marshall

   --------------
   -- Marshall --
   --------------
   procedure Marshall
     (A : in CORBA.Unsigned_Short;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.unsigned_short;
   begin
      --  Transform the arguments in a C type ...
      C_A := Interfaces.C.unsigned_short (A);
      --  Call the C procedure
      C_Marshall_4 (C_A, S);
   end Marshall;

   --------------------
   -- C_UnMarshall_4 --
   --------------------

   procedure C_Unmarshall_4
     (A : out Interfaces.C.unsigned_short;
      S : in out Object'Class);
   pragma Import
     (CPP, C_Unmarshall_4,
      "unmarshall__21Ada_memBufferedStreamRUsR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : UnMarshall

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (A : out CORBA.Unsigned_Short;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.unsigned_short;
   begin
      C_Unmarshall_4 (C_A, S);
      A := CORBA.Unsigned_Short (C_A);
   end Unmarshall;

   ------------------
   -- C_Marshall_5 --
   ------------------

   procedure C_Marshall_5
     (A : in Interfaces.C.long;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Marshall_5,
      "marshall__21Ada_memBufferedStreamlR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : Marshall

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (A : in CORBA.Long;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.long;
   begin
      --  Transform the arguments in a C type ...
      C_A := Interfaces.C.long (A);
      --  Call the C procedure
      C_Marshall_5 (C_A, S);
   end Marshall;

   --------------------
   -- C_Unmarshall_5 --
   --------------------

   procedure C_Unmarshall_5
     (A : out Interfaces.C.long;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Unmarshall_5,
      "unmarshall__21Ada_memBufferedStreamRlR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : UnMarshall

   ----------------
   -- Unmarshall --
   ----------------
   procedure Unmarshall
     (A : out CORBA.Long;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.long;
   begin
      C_Unmarshall_5 (C_A, S);
      A := CORBA.Long (C_A);
   end Unmarshall;

   ------------------
   -- C_Marshall_6 --
   ------------------

   procedure C_Marshall_6
     (A : in Interfaces.C.unsigned_long;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Marshall_6,
      "marshall__21Ada_memBufferedStreamUlR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : Marshall

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (A : in CORBA.Unsigned_Long;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.unsigned_long;
   begin
      --  Transform the arguments in a C type ...
      C_A := Interfaces.C.unsigned_long (A);
      --  Call the C procedure
      C_Marshall_6 (C_A, S);
   end Marshall;

   --------------------
   -- C_Unmarshall_6 --
   --------------------

   procedure C_Unmarshall_6
     (A : out Interfaces.C.unsigned_long;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Unmarshall_6,
      "unmarshall__21Ada_memBufferedStreamRUlR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : UnMarshall

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (A : out CORBA.Unsigned_Long;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.unsigned_long;
   begin
      C_Unmarshall_6 (C_A, S);
      A := CORBA.Unsigned_Long (C_A);
   end Unmarshall;

   ------------------
   -- C_Marshall_7 --
   ------------------

   procedure C_Marshall_7
     (A : in Interfaces.C.C_float;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Marshall_7,
      "marshall__21Ada_memBufferedStreamfR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : Marshall

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (A : in CORBA.Float;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.C_float;
   begin
      --  Transform the arguments in a C type ...
      C_A := Interfaces.C.C_float (A);
      --  Call the C procedure
      C_Marshall_7 (C_A, S);
   end Marshall;

   --------------------
   -- C_Unmarshall_7 --
   --------------------

   procedure C_Unmarshall_7
     (A : out Interfaces.C.C_float;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Unmarshall_7,
      "unmarshall__21Ada_memBufferedStreamRfR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : UnMarshall

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (A : out CORBA.Float;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.C_float;
   begin
      C_Unmarshall_7 (C_A, S);
      A := CORBA.Float (C_A);
   end Unmarshall;

   ------------------
   -- C_Marshall_8 --
   ------------------

   procedure C_Marshall_8
     (A : in Interfaces.C.double;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Marshall_8,
      "marshall__21Ada_memBufferedStreamdR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : Marshall

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (A : in CORBA.Double;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.double;
   begin
      --  Transform the arguments in a C type ...
      C_A := Interfaces.C.double (A);
      --  Call the C procedure
      C_Marshall_8 (C_A, S);
   end Marshall;

   --------------------
   -- C_Unmarshall_8 --
   --------------------

   procedure C_Unmarshall_8
     (A : out Interfaces.C.double;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Unmarshall_8,
      "unmarshall__21Ada_memBufferedStreamRdR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall (see
   --  Ada_memBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : UnMarshall

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (A : out CORBA.Double;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.double;
   begin
      C_Unmarshall_8 (C_A, S);
      A := CORBA.Double (C_A);
   end Unmarshall;

   ------------------
   -- C_Marshall_9 --
   ------------------

   procedure C_Marshall_9
     (A : in CORBA.Octet;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Marshall_9,
      "marshall__21Ada_memBufferedStreamUcR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall FOR CHAR TYPE
   --  BECAUSE IT IS THE SAME TYPE IN C++ (see Ada_memBufferedStream.hh)
   --  name was changed to avoid conflict called by the Ada equivalent :
   --  Marshall

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (A : in CORBA.Octet;
      S : in out Object'Class)
   is
   begin
      C_Marshall_9 (A, S);
   end Marshall;

   --------------------
   -- C_Unmarshall_9 --
   --------------------
   procedure C_Unmarshall_9
     (A : out CORBA.Octet;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Unmarshall_9,
      "unmarshall__21Ada_memBufferedStreamRUcR21Ada_memBufferedStream");
   --  Wrapper around Ada_memBufferedStream function marshall FOR CHAR TYPE
   --  BECAUSE IT IS THE SAME TYPE IN C++ (see Ada_memBufferedStream.hh)
   --  name was changed to avoid conflict called by the Ada equivalent :
   --  UnMarshall

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (A : out CORBA.Octet;
      S : in out Object'Class) is
   begin
      C_Unmarshall_9 (A, S);
   end Unmarshall;

   --------------
   -- Marshall --
   --------------
   procedure Marshall
     (A : in CORBA.String;
      S : in out Object'Class)
   is
      Size : CORBA.Unsigned_Long;
      C    : Standard.Character;
   begin
      --  First marshall the size of the string + 1 1 is the size of the
      --  null character we must marshall at the end of the string (C
      --  style)

      Size := CORBA.Length (A) + CORBA.Unsigned_Long (1);
      Marshall (Size, S);

      --  Then marshall the string itself and a null character at the end

      for I in 1 .. Integer (Size) - 1 loop
         C := Ada.Strings.Unbounded.Element
           (Ada.Strings.Unbounded.Unbounded_String (A), I);
         Marshall (C, S);
      end loop;

      Marshall (CORBA.Char (Ada.Characters.Latin_1.NUL), S);
   end Marshall;

   ----------------
   -- UnMarshall --
   ----------------

   procedure Unmarshall
     (A : out CORBA.String;
      S : in out Object'Class)
   is
      Size : CORBA.Unsigned_Long;
      C    : Standard.Character;
   begin
      --  First unmarshalls the size of the string
      Unmarshall (Size, S);

      case Size is
         when 0 =>
            --  The size is never 0 so raise exception if it is the case
            Ada.Exceptions.Raise_Exception
              (CORBA.AdaBroker_Fatal_Error'Identity,
               "Size of the string was 0 in membufferedstream.UnMarshall.");

         when 1 =>
            --  If the size is 1 then the String is empty
            A := CORBA.String (Ada.Strings.Unbounded.To_Unbounded_String (""));

         when others =>
            --  Else we can unmarshall the string
            declare
               Tmp : String (1 .. Integer (Size) - 1);
            begin
               for I in Tmp'Range loop
                  Unmarshall (Tmp (I), S);
               end loop;
               A := CORBA.String
                 (Ada.Strings.Unbounded.To_Unbounded_String (Tmp));
            end;

      end case;

      --  Unmarshall the null character at the end of the string (C style)
      --  and verify it is null

      Unmarshall (C, S);

      if C /= Ada.Characters.Latin_1.NUL then
         Ada.Exceptions.Raise_Exception
           (CORBA.AdaBroker_Fatal_Error'Identity,
            "Size not ended by null character in UnMarshall.");
      end if;
   end Unmarshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (A : in CORBA.Completion_Status;
      S : in out Object'Class)
   is
   begin
      --  Maps the possible values on the firste shorts and marshall the
      --  right one

      case A is
         when CORBA.Completed_Yes =>
            Marshall (CORBA.Unsigned_Short (1), S);

         when CORBA.Completed_No =>
            Marshall (CORBA.Unsigned_Short (2), S);

         when CORBA.Completed_Maybe =>
            Marshall (CORBA.Unsigned_Short (3), S);

      end case;
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (A : out CORBA.Completion_Status;
      S : in out Object'Class)
   is
      Tmp : CORBA.Unsigned_Short;
   begin
      --  Unmarshalls an unsigned short
      Unmarshall (Tmp, S);

      --  And returns the corresponding Completion_Status
      case Tmp is
         when 1 =>
            A := CORBA.Completed_Yes;

         when 2 =>
            A := CORBA.Completed_No;

         when 3 =>
            A := CORBA.Completed_Maybe;

         when others =>
            Ada.Exceptions.Raise_Exception
              (CORBA.AdaBroker_Fatal_Error'Identity,
               "Expected Completion_Status in UnMarshall" & CORBA.CRLF &
               "Short out of range" & CORBA.CRLF &
               "(see membufferedstream L660)");

      end case;
   end Unmarshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall

     (A : in CORBA.Ex_Body'Class;
      S : in out Object'Class) is
   begin
      --  Just marshall each field
      Marshall (A.Minor, S);
      Marshall (A.Completed, S);
   end Marshall;

   ----------------
   -- UnMarshall --
   ----------------

   procedure Unmarshall
     (A : out CORBA.Ex_Body'Class;
      S : in out Object'Class)
   is
      Minor     : CORBA.Unsigned_Long;
      Completed : CORBA.Completion_Status;
   begin
      --  Unmarshalls the two fields
      Unmarshall (Completed, S);
      Unmarshall (Minor, S);

      --  And return the object
      A.Minor     := Minor;
      A.Completed := Completed;
   end Unmarshall;

end MemBufferedStream;
