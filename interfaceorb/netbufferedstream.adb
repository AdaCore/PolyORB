--  This package is wrapped around a C++ class whose name is
--  Ada_netBufferedStream. (see Ada_netBufferedStream.hh) It provides two
--  types of methods : the C functions of the Ada_netBufferedStream class
--  and their equivalent in Ada. (he first ones have a C_ prefix.)  In
--  addition, there is a raise_ada_exception function that allows C
--  functions to raise the ada No_Initialisation exception.  At last, there
--  is only one Init procedure in place of two in Ada_netBufferedStream
--  since the second one is useless for AdaBroker.


with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Characters.Latin_1;

with Interfaces.C;

with CORBA;
with Omni;

with Adabroker_Debug;
use Adabroker_Debug;

package body NetBufferedStream is

   use type Ada.Strings.Unbounded.Unbounded_String;

   use type CORBA.String;
   use type CORBA.Unsigned_Long;

   Debug : constant Boolean := Adabroker_Debug.Is_Active("netbufferedstream");

   ------------
   -- C_Init --
   ------------

   procedure C_Init
     (Self    : in Object'Class;
      R       : in System.Address;
      RdLock  : in Sys_Dep.C_Boolean;
      WrLock  : in Sys_Dep.C_Boolean;
      Bufsize : in Interfaces.C.Unsigned_Long);

   pragma Import (CPP, C_Init, "__17NetBufferedStreamP4RopebT2Ui");
   --  Wrapper around Ada_netBufferedStream function Init (see
   --  Ada_netBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : Init

   ----------
   -- Init --
   ----------
   procedure Init
     (Self    : in Object'Class;
      R       : in Rope.Object;
      Rdlock  : in CORBA.Boolean;
      WrLock  : in CORBA.Boolean;
      Bufsize : in CORBA.Unsigned_Long)
   is
      C_RdLock  : Sys_Dep.C_Boolean;
      C_WrLock  : Sys_Dep.C_Boolean;
      C_Bufsize : Interfaces.C.unsigned_long;
   begin
      C_RdLock  := Sys_Dep.Boolean_Ada_To_C (RdLock);
      C_WrLock  := Sys_Dep.Boolean_Ada_To_C (WrLock);
      C_Bufsize := Interfaces.C.Unsigned_Long (Bufsize);
      C_Init (Self, System.Address (R), C_RdLock, C_WrLock, C_Bufsize);
   end Init;

   ------------------
   -- C_Marshall_1 --
   ------------------

   procedure C_Marshall_1
     (A : in Interfaces.C.Char;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Marshall_1,
      "marshall__21Ada_netBufferedStreamUcR21Ada_netBufferedStream");
   --  Wrapper around Ada_netBufferedStream function marshall (see
   --  Ada_netBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : Marshall

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (A : in CORBA.Char;
      S : in out Object'Class) is
      C_A : Interfaces.C.Char;
   begin
      C_A := Interfaces.C.Char(A);
      C_Marshall_1 (C_A,S);
   end Marshall;

   --------------------
   -- C_Unmarshall_1 --
   --------------------

   procedure C_Unmarshall_1
     (A : out Interfaces.C.Char;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Unmarshall_1,
      "unmarshall__21Ada_netBufferedStreamRUcR21Ada_netBufferedStream");
   --  Wrapper around Ada_netBufferedStream function marshall (see
   --  Ada_netBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : UnMarshall

   ----------------
   -- Unmarshall --
   ----------------
   procedure Unmarshall
     (A : out CORBA.Char;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.Char;
   begin
      C_Unmarshall_1 (C_A, S);
      A := CORBA.Char (C_A);
   end Unmarshall;

   ----------------
   -- Align_Size --
   ----------------

   function Align_Size
     (A              : in CORBA.Char;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
      return CORBA.Unsigned_Long
   is
   begin
      --  No alignment needed here
      return Initial_Offset + N;
   end Align_Size;

   ------------------
   -- C_Marshall_2 --
   ------------------

   procedure C_Marshall_2
     (A : in Sys_Dep.C_Boolean;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Marshall_2,
      "marshall__21Ada_netBufferedStreambR21Ada_netBufferedStream");
   --  Wrapper around Ada_netBufferedStream function marshall (see
   --  Ada_netBufferedStream.hh) name was changed to avoid conflict called
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
      C_A := Sys_Dep.Boolean_Ada_To_C (A);
      C_Marshall_2 (C_A,S);
   end Marshall;

   --------------------
   -- C_Unmarshall_2 --
   --------------------

   procedure C_Unmarshall_2
     (A : out Sys_Dep.C_Boolean;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Unmarshall_2,
      "unmarshall__21Ada_netBufferedStreamRbR21Ada_netBufferedStream");
   --  Wrapper around Ada_netBufferedStream function marshall (see
   --  Ada_netBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : UnMarshall

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (A : out CORBA.Boolean;
      S : in out Object'Class)
   is
      C_A : Sys_Dep.C_boolean;
   begin
      C_UnMarshall_2 (C_A, S);
      A := Sys_Dep.Boolean_C_To_Ada (C_A);
   end Unmarshall;

   ----------------
   -- Align_Size --
   ----------------

   function Align_Size
     (A              : in CORBA.Boolean;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
      return CORBA.Unsigned_Long
   is
   begin
      --  No alignment needed here
      return Initial_Offset + N;
      --  Boolean is marshalled as an unsigned_char
   end Align_Size;

   ------------------
   -- C_Marshall_3 --
   ------------------

   procedure C_Marshall_3
     (A : in Interfaces.C.Short;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Marshall_3,
      "marshall__21Ada_netBufferedStreamsR21Ada_netBufferedStream");
   --  Wrapper around Ada_netBufferedStream function marshall (see
   --  Ada_netBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : Marshall

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (A : in CORBA.Short;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.Short;
   begin
      C_A := Interfaces.C.Short(A);
      C_Marshall_3 (C_A, S);
   end Marshall;

   --------------------
   -- C_Unmarshall_3 --
   --------------------

   procedure C_Unmarshall_3
     (A : out Interfaces.C.Short;
      S : in out Object'Class);

   pragma Import
     (CPP, C_UnMarshall_3,
      "unmarshall__21Ada_netBufferedStreamRsR21Ada_netBufferedStream");
   --  Wrapper around Ada_netBufferedStream function marshall (see
   --  Ada_netBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : UnMarshall

   ----------------
   -- Unmarshall --
   ----------------
   procedure Unmarshall
     (A : out CORBA.Short;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.Short;
   begin
      C_Unmarshall_3 (C_A, S);
      A := CORBA.Short (C_A);
   end Unmarshall;

   ----------------
   -- Align_Size --
   ----------------

   function Align_Size
     (A              : in CORBA.Short;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
      return CORBA.Unsigned_Long
   is
      Tmp : CORBA.Unsigned_Long;
   begin
      Tmp := Omni.Align_To (Initial_Offset, Omni.ALIGN_2);
      return Tmp + (2 * N);
   end Align_Size;

   ------------------
   -- C_Marshall_4 --
   -------------------

   procedure C_Marshall_4
     (A : in Interfaces.C.Unsigned_Short;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Marshall_4,
      "marshall__21Ada_netBufferedStreamUsR21Ada_netBufferedStream");
   --  Wrapper around Ada_netBufferedStream function marshall (see
   --  Ada_netBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : Marshall

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (A : in CORBA.Unsigned_Short;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.Unsigned_Short;
   begin
      C_A := Interfaces.C.Unsigned_Short (A);
      C_Marshall_4 (C_A, S);
   end Marshall;

   --------------------
   -- C_Unmarshall_4 --
   --------------------

   procedure C_UnMarshall_4
     (A : out Interfaces.C.Unsigned_Short;
      S : in out Object'Class);

   pragma Import
     (CPP, C_UnMarshall_4,
      "unmarshall__21Ada_netBufferedStreamRUsR21Ada_netBufferedStream");
   --  Wrapper around Ada_netBufferedStream function marshall (see
   --  Ada_netBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : UnMarshall


   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (A : out CORBA.Unsigned_Short;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.Unsigned_Short;
   begin
      C_UnMarshall_4 (C_A, S);
      A := CORBA.Unsigned_Short (C_A);
   end Unmarshall;

   ----------------
   -- Align_Size --
   ----------------

   function Align_Size
     (A              : in CORBA.Unsigned_Short;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
      return CORBA.Unsigned_Long
   is
      Tmp : CORBA.Unsigned_Long;
   begin
      Tmp := Omni.Align_To (Initial_Offset, Omni.ALIGN_2);
      return Tmp + (2 * N);
   end Align_Size;

   ------------------
   -- C_Marshall_5 --
   ------------------

   procedure C_Marshall_5
     (A : in Interfaces.C.Long;
      S : in out Object'Class);

   pragma Import
     (CPP, C_Marshall_5,
      "marshall__21Ada_netBufferedStreamlR21Ada_netBufferedStream");
   --  Wrapper around Ada_netBufferedStream function marshall (see
   --  Ada_netBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : Marshall

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (A : in CORBA.Long;
      S : in out Object'Class)
   is
      C_A : Interfaces.C.Long;
   begin
      C_A := Interfaces.C.Long (A);
      C_Marshall_5 (C_A, S);
   end Marshall;

   --------------------
   -- C_Unmarshall_5 --
   --------------------

   procedure C_UnMarshall_5
     (A : out Interfaces.C.Long;
      S : in out Object'Class);

   pragma Import
     (CPP, C_UnMarshall_5,
      "unmarshall__21Ada_netBufferedStreamRlR21Ada_netBufferedStream");
   --  Wrapperw around Ada_netBufferedStream function marshall (see
   --  Ada_netBufferedStream.hh) name was changed to avoid conflict called
   --  by the Ada equivalent : UnMarshall


   -- Unmarshall
   -------
   procedure Unmarshall (A : out CORBA.Long;
                         S : in out Object'Class) is
      C_A : Interfaces.C.Long;
   begin
      C_Unmarshall_5 (C_A,S);
      A := CORBA.Long(C_A);
   end;


   -- Align_Size
   -------
   function Align_Size (A : in CORBA.Long;
                        Initial_Offset : in CORBA.Unsigned_Long;
                        N : in CORBA.Unsigned_Long := 1)
                        return CORBA.Unsigned_Long is
      Tmp : CORBA.Unsigned_Long;
   begin
      Tmp := Omni.Align_To (Initial_Offset,Omni.ALIGN_4);
      return Tmp + (4 * N);
   end;


   -- C_Marshall_6
   ---------
   procedure C_Marshall_6 (A : in Interfaces.C.Unsigned_Long;
                           S : in out Object'Class);
   pragma Import (CPP,C_Marshall_6,"marshall__21Ada_netBufferedStreamUlR21Ada_netBufferedStream");
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -------
   procedure Marshall (A : in CORBA.Unsigned_Long;
                       S : in out Object'Class) is
      C_A : Interfaces.C.Unsigned_Long;
   begin
      pragma Debug(Output(Debug,"Netbufferedstream : marshalling Unsigned Long"));
      -- transforms the arguments in a C type ...
      C_A := Interfaces.C.Unsigned_Long (A);
      -- ... and calls the C procedure
      C_Marshall_6 (C_A,S);
   end;


   -- C_Unmarshall_6
   ---------
   procedure C_Unmarshall_6 (A : out Interfaces.C.Unsigned_Long;
                             S : in out Object'Class);
   pragma Import (CPP,C_Unmarshall_6,"unmarshall__21Ada_netBufferedStreamRUlR21Ada_netBufferedStream");
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Unmarshall


   -- Unmarshall
   -------
   procedure Unmarshall (A : out CORBA.Unsigned_Long;
                         S : in out Object'Class) is
      C_A : Interfaces.C.Unsigned_Long;
   begin
      pragma Debug(Output(Debug,"Netbufferedstream : unmarshalling Unsigned Long"));
      C_Unmarshall_6 (C_A,S);
      A := CORBA.Unsigned_Long(C_A);
   end;


   -- Align_Size
   -------
   function Align_Size (A : in CORBA.Unsigned_Long;
                        Initial_Offset : in CORBA.Unsigned_Long;
                        N : in CORBA.Unsigned_Long := 1)
                        return CORBA.Unsigned_Long is
      Tmp : CORBA.Unsigned_Long;
   begin
      Tmp := Omni.Align_To (Initial_Offset,Omni.ALIGN_4);
      return Tmp + (4 * N);
   end;


   -- C_Marshall_7
   ---------
   procedure C_Marshall_7 (A : in Interfaces.C.C_Float;
                           S : in out Object'Class);
   pragma Import (CPP,C_Marshall_7,"marshall__21Ada_netBufferedStreamfR21Ada_netBufferedStream");
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -------
   procedure Marshall (A : in CORBA.Float;
                       S : in out Object'Class) is
      C_A : Interfaces.C.C_Float;
   begin
      -- transforms the arguments in a C type ...
      C_A := Interfaces.C.C_Float (A);
      -- ... and calls the C procedure
      C_Marshall_7 (C_A,S);
   end;


   -- C_Unmarshall_7
   ---------
   procedure C_Unmarshall_7 (A : out Interfaces.C.C_Float;
                             S : in out Object'Class);
   pragma Import (CPP,C_Unmarshall_7,"unmarshall__21Ada_netBufferedStreamRfR21Ada_netBufferedStream");
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Unmarshall


   -- Unmarshall
   -------
   procedure Unmarshall (A : out CORBA.Float;
                         S : in out Object'Class) is
      C_A : Interfaces.C.C_Float;
   begin
      C_Unmarshall_7 (C_A,S);
      A := CORBA.Float(C_A);
   end;


   -- Align_Size
   -------
   function Align_Size (A : in CORBA.Float;
                        Initial_Offset : in CORBA.Unsigned_Long;
                        N : in CORBA.Unsigned_Long := 1)
                        return CORBA.Unsigned_Long is
      Tmp : CORBA.Unsigned_Long;
   begin
      Tmp := Omni.Align_To (Initial_Offset,Omni.ALIGN_4);
      return Tmp + (4 * N);
   end;


   -- C_Marshall_8
   ---------
   procedure C_Marshall_8 (A : in Interfaces.C.Double;
                           S : in out Object'Class);
   pragma Import (CPP,C_Marshall_8,"marshall__21Ada_netBufferedStreamdR21Ada_netBufferedStream");
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -------
   procedure Marshall (A : in CORBA.Double;
                       S : in out Object'Class) is
      C_A : Interfaces.C.Double;
   begin
      -- transforms the arguments in a C type ...
      C_A := Interfaces.C.Double(A);
      -- ... and calls the C procedure
      C_Marshall_8 (C_A,S);
   end;


   -- C_Unmarshall_8
   ---------
   procedure C_Unmarshall_8 (A : out Interfaces.C.Double;
                             S : in out Object'Class);
   pragma Import (CPP,C_Unmarshall_8,"unmarshall__21Ada_netBufferedStreamRdR21Ada_netBufferedStream");
   -- wrapper around Ada_netBufferedStream function marshall
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Unmarshall


   -- Unmarshall
   -------
   procedure Unmarshall (A : out CORBA.Double;
                         S : in out Object'Class) is
      C_A : Interfaces.C.Double;
   begin
      C_Unmarshall_8 (C_A,S);
      A := CORBA.Double(C_A);
   end;


   -- Align_Size
   -------
   function Align_Size (A : in CORBA.Double;
                        Initial_Offset : in CORBA.Unsigned_Long;
                        N : in CORBA.Unsigned_Long := 1)
                        return CORBA.Unsigned_Long is
      Tmp : CORBA.Unsigned_Long;
   begin
      Tmp := Omni.Align_To (Initial_Offset,Omni.ALIGN_8);
      return Tmp + (8 * N);
   end;


   -- C_Marshall_9
   ---------
   procedure C_Marshall_9 (A : in CORBA.Octet;
                           S : in out Object'Class);
   pragma Import (CPP,C_Marshall_9,"marshall__21Ada_netBufferedStreamUcR21Ada_netBufferedStream");
   -- wrapper around Ada_netBufferedStream function marshall
   -- FOR CHAR TYPE BECAUSE IT IS THE SAME TYPE IN C++
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Marshall


   -- Marshall
   -------
   procedure Marshall (A : in CORBA.Octet;
                       S : in out Object'Class) is
   begin
      C_Marshall_9 (A,S);
   end;


   -- C_Unmarshall_9
   ---------
   procedure C_Unmarshall_9 (A : out CORBA.Octet;
                             S : in out Object'Class);
   pragma Import (CPP,C_Unmarshall_9,"unmarshall__21Ada_netBufferedStreamRUcR21Ada_netBufferedStream");
   -- wrapper around Ada_netBufferedStream function marshall
   -- FOR CHAR TYPE BECAUSE IT IS THE SAME TYPE IN C++
   -- (see Ada_netBufferedStream.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Unmarshall


   -- Unmarshall
   -------
   procedure Unmarshall (A : out CORBA.Octet;
                         S : in out Object'Class) is
   begin
      C_Unmarshall_9 (A,S);
   end;


   -- Align_Size
   -------
   function Align_Size (A : in CORBA.Octet;
                        Initial_Offset : in CORBA.Unsigned_Long;
                        N : in CORBA.Unsigned_Long := 1)
                        return CORBA.Unsigned_Long is
   begin
      return Initial_Offset + N;
   end;


   -- Marshall
   -------
   procedure Marshall (A : in CORBA.String;
                       S : in out Object'Class) is
      Size : CORBA.Unsigned_Long;
      C : Standard.Character;
   begin
      -- first marshall the size of the string + 1
      -- 1 is the size of the null character we must marshall
      -- at the end of the string (C style)
      Size := CORBA.Length (A) + CORBA.Unsigned_Long (1);

      Marshall (Size , S);
      -- Then marshall the string itself and a null character at the end

      for I in 1..Integer(Size)-1 loop
         C := Ada.Strings.Unbounded.Element (Ada.Strings.Unbounded.Unbounded_String (A),I);
         Marshall (C,S);
      end loop;
      Marshall (CORBA.Char (Ada.Characters.Latin_1.nul),S);
   end;


   -- Unmarshall
   -------
   procedure Unmarshall (A : out CORBA.String;
                         S : in out Object'Class) is
      Size : CORBA.Unsigned_Long;
      C : Standard.Character;
   begin
      -- first unmarshalls the size of the string
      Unmarshall (Size,S);
      case Size is
         when 0 =>
            -- the size is never 0 so raise exception if it is the case
            Ada.Exceptions.Raise_Exception(CORBA.Adabroker_Fatal_Error'Identity,
                                           "Size of the string was 0 in netbufferedstream.Unmarshall.");
         when 1 =>
            -- if the size is 1 then the String is empty
            A := CORBA.String (Ada.Strings.Unbounded.To_Unbounded_String (""));
         when others =>
            -- else we can unmarshall the string
            declare
               Tmp : String (1..Integer(Size)-1);
            begin
               for I in 1..Integer(Size)-1 loop
                  Unmarshall (Tmp(I),S);
               end loop;
               A := CORBA.String (Ada.Strings.Unbounded.To_Unbounded_String (Tmp));
            end;
      end case;
      -- unmarshall the null character at the end of the string (C style)
      -- and verify it is null
      Unmarshall (C,S);
      if C /= Ada.Characters.Latin_1.Nul then
         Ada.Exceptions.Raise_Exception(CORBA.Adabroker_Fatal_Error'Identity,
                                        "Size not ended by null character in netbufferedstream.Unmarshall.");
      end if;
   end;


   -- Align_Size
   -------
   function Align_Size (A : in CORBA.String;
                        Initial_Offset : in CORBA.Unsigned_Long;
                        N : in CORBA.Unsigned_Long := 1)
                        return CORBA.Unsigned_Long is
      Tmp : CORBA.Unsigned_Long;
   begin
      -- alignment
      Tmp := Omni.Align_To (Initial_Offset,Omni.ALIGN_4);
      -- size of the size of the string
      Tmp := Tmp + 4;
      -- size of the string itself
      return Tmp + (N * (CORBA.Length (A) + 1));
      -- + 1 is for the null character (the strings are marshalled in C style)
   end;


   -- Marshall
   -------
   procedure Marshall (A : in CORBA.Completion_Status;
                       S : in out Object'Class) is
   begin
      -- maps the possible values on the firste shorts
      -- and marshall the right one
      case A is
         when CORBA.Completed_Yes =>
            Marshall (CORBA.Unsigned_Long (1),S);
         when CORBA.Completed_No =>
            Marshall (CORBA.Unsigned_Long (2),S);
         when CORBA.Completed_Maybe =>
            Marshall (CORBA.Unsigned_Long (3),S);
      end case;
   end;


   -- Unmarshall
   -------
   procedure Unmarshall (A : out CORBA.Completion_Status;
                         S : in out Object'Class) is
      Tmp : CORBA.Unsigned_Long;
   begin
      -- unmarshalls an unsigned short
      Unmarshall (Tmp,S);
      -- and returns the corresponding Completion_Status
      case Tmp is
         when 1 =>
            A := CORBA.Completed_Yes;
         when 2 =>
            A := CORBA.Completed_No;
         when 3 =>
            A := CORBA.Completed_Maybe;
         when others =>
            Ada.Exceptions.Raise_Exception (CORBA.AdaBroker_Fatal_Error'Identity,
                                            "Expected Completion_Status in netbufferedstream.Unmarshall" & CORBA.CRLF &
                                            "Short out of range" & CORBA.CRLF &
                                            "(see netbufferedstream L660)");
      end case;
   end;

   ----------------
   -- Align_Size --
   ----------------

   function Align_Size
     (A              : in CORBA.Completion_Status;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
      return CORBA.Unsigned_Long
   is
      Tmp : CORBA.Unsigned_Long;
   begin
      --  A Completion_Status is marshalled as an unsigned_long
      Tmp := Omni.Align_To (Initial_Offset, Omni.ALIGN_4);
      return Tmp + (4 * N);
   end Align_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (A : in CORBA.Ex_Body'Class;
      S : in out Object'Class)
   is
   begin
      Marshall (A.Minor,S);
      Marshall (A.Completed, S);
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (A : out CORBA.Ex_Body'Class;
      S : in out Object'Class)
   is
      Minor     : CORBA.Unsigned_Long;
      Completed : CORBA.Completion_Status;
   begin
      Unmarshall (Completed,S);
      Unmarshall (Minor,S);
      A.Minor := Minor;
      A.Completed := Completed;
   end Unmarshall;


   -- Align_Size
   -------
   function Align_Size (A : in CORBA.Ex_Body'Class;
                        Initial_Offset : in CORBA.Unsigned_Long;
                        N : in CORBA.Unsigned_Long := 1)
                        return CORBA.Unsigned_Long is
      Tmp : CORBA.Unsigned_Long;
   begin
      Tmp := Omni.Align_To (Initial_Offset,Omni.ALIGN_4);
      return Tmp + (8 * N);
      -- an Ex_body has two fields : an unsigned_long -> 4 bytes
      --                             and a Completion_Status -> 4 bytes
   end;



   -- C_Is_Reusing_Existing_Connection
   -------------------
   function C_Is_Reusing_Existing_Connection (Self : in Object'Class)
                                              return Sys_Dep.C_Boolean;
   pragma Import (CPP,C_Is_Reusing_Existing_Connection,"isReUsingExistingConnection__21Ada_netBufferedStream");
   -- wrapper around     _CORBA_Boolean isReUsingExistingConnection() const;
   -- (see rope.h L 395)
   -- called by the Ada equivalent : Is_Reusing_Existing_Connection


   -- Is_Reusing_Existing_Connection
   -----------------
   function Is_Reusing_Existing_Connection (Self : in Object'Class)
                                            return CORBA.Boolean is
      C_Result : Sys_Dep.C_Boolean;
   begin
      -- calls the C function ...
      C_Result := C_Is_Reusing_Existing_Connection (Self);
      -- ...and transforms the result into an Ada type
      return Sys_Dep.Boolean_C_To_Ada (C_Result);
   end;

end NetBufferedStream;
