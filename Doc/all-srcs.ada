with RCI;
with Ada.Text_IO;
procedure Check_PID is
begin
   if RCI'Partition_ID = Check_PID'Partition_ID then
      Ada.Text_IO.Put_Line ("package RCI is configured locally");
   else
      Ada.Text_IO.Put_Line ("package RCI is configured remotely");
   end if;
end Check_PID;
package Types is
   pragma Pure;

   type Customer_Type is new String;
   type Password_Type is new String;
end Types;
with Types; use Types;
package RCIBank is
   pragma Remote_Call_Interface;

   function Balance
     (Customer : in Customer_Type;
      Password : in Password_Type)
      return Integer;

   procedure Transfer
     (Donator  : in Customer_Type;
      Password : in Password_Type;
      Amount   : in Positive;
      Customer : in Customer_Type);

   procedure Deposit
     (Customer : in Customer_Type;
      Amount   : in Positive);

   procedure Withdraw
     (Customer : in Customer_Type;
      Password : in Password_Type;
      Amount   : in out Positive);
end RCIBank;
with Types; use Types;
package RASBank is
   pragma Remote_Call_Interface;

   type Balance_Type is access function
     (Customer : in Customer_Type;
      Password : in Password_Type)
      return Integer;

   procedure Register
      (Balance : in Balance_Type);

   function Get_Balance
      return Balance_Type;

   --  [...] Other services
end RASBank;
with Types; use Types;
package MirrorBank is
   pragma Remote_Call_Interface;

   function Balance
     (Customer : in Customer_Type;
      Password : in Password_Type)
      return Integer;

   --  [...] Other services
end MirrorBank;
with RASBank, Types; use RASBank, Types;
package body MirrorBank is

   function Balance
     (Customer : in Customer_Type;
      Password : in Password_Type)
      return Integer is
   begin
      return Something;
   end Balance;

begin
   --  Register a dynamically bound remote subprogram (Balance)
   --  through a statically bound remote subprogram (Register)
   Register (Balance'Access);
   --  [...] Register other services
end MirrorBank;
with Types; use Types;
with RCIBank; use RCIBank;
procedure RCIClient is
   B : Integer;
   C : Customer_Type := "rich";
   P : Password_Type := "xxxx";
begin
   B := Balance (C, P);
end RCIClient;
with Types; use Types;
with RASBank; use RASBank;
procedure BankClient is
   B : Integer;
   C : Customer_Type := "rich";
   P : Password_Type := "xxxx";
begin
   --  Through a statically bound remote subprogram (Get_Balance), get
   --  a dynamically bound remote subprogram. Dereference it to
   --  perform a dynamic invocation.
   B := Get_Balance.all (C, P);
end BankClient;
with Types; use Types;
package Terminal is
   pragma Pure;

   type Term_Type is abstract tagged limited private;

   procedure Notify
     (MyTerm   : access Term_Type;
      Donator  : in Customer_Type;
      Amount   : in Integer) is abstract;

private
   type Term_Type is abstract tagged limited null record;
end Terminal;
with Terminal, Types; use Terminal, Types;
package RACWBank is
   pragma Remote_Call_Interface;

   type Term_Access is access all Term_Type'Class;

   procedure Register
     (MyTerm   : in Term_Access;
      Customer : in Customer_Type;
      Password : in Password_Type);

   --  [...] Other services
end RACWBank;
with Types, Terminal; use Types, Terminal;
package NewTerminal is
   pragma Remote_Types;

   type New_Term_Type is
      new Term_Type with null record;

   procedure Notify
     (MyTerm   : access New_Term_Type;
      Donator  : in Customer_Type;
      Amount   : in Integer);

   function Current return Term_Access;
end NewTerminal;
with NewTerminal, RACWBank, Types; use NewTerminal, RACWBank, Types;
procedure Term1Client is
   MyTerm   : Term_Access   := Current;
   Customer : Customer_Type := "poor";
   Password : Password_Type := "yyyy";
begin
   Register (MyTerm, Customer, Password);
   --  [...] Execute other things
end Term1Client;
with NewTerminal, RACWBank, Types; use NewTerminal, RACWBank, Types;
procedure Term2Client is
   MyTerm   : Term_Access   := Current;
   Donator  : Customer_Type := "rich";
   Password : Password_Type := "xxxx";
   Customer : Customer_Type := "poor";
begin
   Register (MyTerm, Donator, Password);
   Transfer (Donator, Password, 100, Customer);
end Term2Client;
with Types; use Types;
package body RACWBank is
   procedure Register
     (MyTerm   : in Term_Access;
      Customer : in Customer_Type;
      Password : in Password_Type) is
   begin
      Insert_In_Local_Table (MyTerm, Customer);
   end Register;

   procedure Transfer
     (Donator  : in Customer_Type;
      Password : in Password_Type;
      Amount   : in Positive;
      Customer : in Customer_Type)
   is
      --  Find Customer terminal.
      Term : Term_Access
        := Find_In_Local_Table (Customer);
   begin
      Withdraw (Donator, Amount);
      Deposit  (Customer, Amount);
      if Term /= null then
         --  Notify on Customer terminal.
         Notify (Term, Donator, Amount);
      end if;
   end Transfer;

   --  [...] Other services
end RACWBank;
with Ada.Streams; use Ada.Streams;
package StringArrayStream is
   pragma Remote_Types;

   type List is private;
   procedure Append (L : access List; O : in String);
   function  Delete (L : access List) return String;

private
   type String_Access is access String;

   type Node;
   type List is access Node;

   type Node is record
      Content : String_Access;
      Next    : List;
   end record;

   procedure Read
     (S : access Root_Stream_Type'Class;
      L : out List);
   procedure Write
     (S : access Root_Stream_Type'Class;
      L : in List);
   for List'Read use Read;
   for List'Write use Write;
end StringArrayStream;
package body StringArrayStream is
   procedure Read
     (S : access Root_Stream_Type'Class;
      L : out List) is
   begin
      if Boolean'Input (S) then
         L := new Node;
         L.Content := new String'(String'Input (S));
         List'Read (S, L.Next);
      else
         L := null;
      end if;
   end Read;

   procedure Write
     (S : access Root_Stream_Type'Class;
      L : in List) is
   begin
      if L = null then
         Boolean'Output (S, False);
      else
         Boolean'Output (S, True);
         String'Output (S, L.Content.all);
         List'Write (S, L.Next);
      end if;
   end Write;

   --  [...] Other services
end StringArrayStream;
package SharedObjects is
   pragma Shared_Passive;

   Max : Positive := 10;
   type Index_Type is range 1 .. Max;
   type Rate_Type is new Float;

   type Rates_Type is array (Index_Type) of Rate_Type;

   External_Synchronization : Rates_Type;

   protected Internal_Synchronization is
      procedure Set
        (Index : in Index_Type;
         Rate  : in Rate_Type);

      procedure Get
        (Index : in Index_Type;
         Rate  : out Rate_Type);
   private
      Rates : Rates_Type;
   end Internal_Synchronization;
end SharedObjects;
package Storage is
   pragma Shared_Passive;

   protected Queue is
      procedure Insert (Q, R : Integer);
      procedure Remove
        (Q : in Integer;
         R : out Integer);
   private
      --  Other declarations
   end Queue;
end Storage;
with Storage; use Storage;
package Common is
   pragma Remote_Types;

   type Notify is
      access procedure (Q : Integer);
   pragma Asynchronous (Notify);

   type Worker is
      abstract tagged limited private;
   procedure Assign
     (W : access Worker;
      Q : in Integer;
      N : in Notify) is abstract;

   type Any_Worker is
      access all Worker'Class;
   pragma Asynchronous (Any_Worker);

private
   type Worker is abstract tagged limited null record;
end Common;
with Common, Storage; use Common, Storage;
package NewWorkers is
   pragma Remote_Types;

   type NewWorker is new Worker with private;

   procedure Assign
     (W : access NewWorker;
      Q : Integer;
      N : Notify);
private
   type NewWorker is new Worker with record
      NewField : Field_Type; --  [...] Other fields
   end record;
end NewWorkers;
with Common, Storage, NewWorkers; use Common, Storage, NewWorkers;
package NewNewWorkers is
   pragma Remote_Types;

   type NewNewWorker is new NewWorker with private;

   procedure Assign
     (W : access NewNewWorker;
      Q : Integer;
      N : Notify);
private
   type NewNewWorker is new NewWorker with record
      NewField : Field_Type; --  [...] Other fields
   end record;
end NewNewWorkers;
with Common; use Common;
package WorkerCity is
   pragma Remote_Call_Interface;

   procedure Insert (W : in  Any_Worker);
   procedure Remove (W : out Any_Worker);
end WorkerCity;
with Storage; use Storage;
generic
package Factory is
   pragma Remote_Call_Interface;

   procedure Notify (Q : Integer);
   pragma Asynchronous (Notify);
end Factory;
with Factory;
package NewFactory is new Factory;
pragma Remote_Call_Interface (NewFactory);
package Internal is
   Exc : exception;
end Internal;
package RemPkg1 is
   pragma Remote_Call_Interface;

   procedure Subprogram;
end RemPkg1;
with Internal, Ada.Exceptions; use Ada.Exceptions;
package body RemPkg1 is
   procedure Subprogram is
   begin
      Raise_Exception (Internal.Exc'Identity, "Message");
   end Subprogram;
end RemPkg1;
package RemPkg2 is
   pragma Remote_Call_Interface;

   procedure Subprogram;
end RemPkg2;
with RemPkg1, Ada.Exceptions; use Ada.Exceptions;
package body RemPkg2 is
   procedure Subprogram is
   begin
      RemPkg1.Subprogram;
   exception when E : others =>
      Raise_Exception (Exception_Identity (E), Exception_Message (E));
   end Subprogram;
end RemPkg2;
with Ada.Text_IO, Ada.Exceptions; use Ada.Text_IO, Ada.Exceptions;
with RemPkg2, Internal;
procedure RemExcMain is
begin
   RemPkg2.Subprogram;
exception when E : Internal.Exc =>
   Put_Line (Exception_Message (E)); -- Output "Message"
end RemExcMain;
with Node1, Node2;
procedure NonDeterministic is
begin
   Node1.Send (1);
   Node2.Send (2);
end NonDeterministic;
package Node1 is
   pragma Remote_Call_Interface;

   procedure Send (X : Integer);
   pragma Asynchronous (Send);
end Node1;
package Node2 is
   pragma Remote_Call_Interface;

   procedure Send (X : Integer);
   pragma Asynchronous (Send);
end Node2;
with Node2;
package body Node1 is
   procedure Send (X : Integer) is
   begin
      Node2.Send (X);
   end Send;
end Node1;
package body Node2 is
   V : Integer := 0;
   procedure Send (X : Integer) is
   begin
      V := X;
   end Send;
end Node2;
package AsynchronousRT is
   pragma Remote_Types;

   type Object is tagged limited private;

   type AsynchronousRACW is access all Object'Class;
   pragma Asynchronous (AsynchronousRACW);

   procedure Asynchronous (X : Object);
   procedure Synchronous  (X : in out Object);
   function Create return AsynchronousRACW;

private
   type Object is tagged limited null record;
end AsynchronousRT;
package AsynchronousRCI is
   pragma Remote_Call_Interface;

   procedure Asynchronous (X : Integer);
   pragma Asynchronous (Asynchronous);

   procedure Synchronous  (X : Integer);

   type AsynchronousRAS is access procedure (X : Integer);
   pragma Asynchronous (AsynchronousRAS);
end AsynchronousRCI;
with AsynchronousRCI, AsynchronousRT;
use AsynchronousRCI, AsynchronousRT;
procedure AsynchronousMain is
   RAS  : AsynchronousRAS;
   RACW : AsynchronousRACW := Create;
begin
   --  Asynchronous Dynamically Bound Remote Call (1)
   RAS := AsynchronousRCI.Asynchronous'Access;
   RAS (0);  --  Abbrev for RAS.all (0)
   --  Synchronous Dynamically Bound Remote Call (2)
   RAS := AsynchronousRCI.Synchronous'Access;
   RAS (0);
   --  Asynchronous Dynamically Bound Remote Call (3)
   Asynchronous (RACW.all);
   --  Synchronous Dynamically Bound Remote Call (4)
   Synchronous (RACW.all);
end AsynchronousMain;
generic
package GenericRCI is
   pragma Remote_Call_Interface;

   procedure P;
end GenericRCI;
with GenericRCI;
package RCIInstantiation is new GenericRCI;
pragma Remote_Call_Interface (RCIInstantiation);
with GenericRCI;
package NormalInstantiation is new GenericRCI;
package Pure is
   pragma Pure;

   Max : constant := 9;

   type My_Integer is new Integer range -Max .. Max;

   type Remote_Object is abstract tagged limited private;

   procedure Primitive_Operation
     (Object : access Remote_Object;
      My_Int : My_Integer) is abstract;

private

   type Remote_Object is abstract tagged limited null record;

end Pure;
package body New_Integers is
   procedure Read
     (S : access Root_Stream_Type'Class;
      V : out New_Integer)
   is
      B : String := String'Input (S);
   begin
      V := New_Integer'Value (B);
   end Read;

   procedure Write
     (S : access Root_Stream_Type'Class;
      V : in New_Integer)
   is
   begin
      String'Output (S, New_Integer'Image (V));
   end Write;
end New_Integers;
with Ada.Streams; use Ada.Streams;
package New_Integers is
   pragma Pure;

   type New_Integer is new Integer;

   procedure Read
     (S : access Root_Stream_Type'Class;
      V : out New_Integer);
   procedure Write
     (S : access Root_Stream_Type'Class;
      V : in New_Integer);

   for New_Integer'Read  use Read;
   for New_Integer'Write use Write;
end New_Integers;
with ACRRT; use ACRRT;
package ACRRCI is
   pragma Remote_Call_Interface;
   pragma All_Calls_Remote;

   procedure P (X : T);
end ACRRCI;
package body ACRRCI is
   procedure P (X : T) is
   begin
      null;
   end P;
end ACRRCI;
with Ada.Streams; use Ada.Streams;
package ACRRT is
   pragma Remote_Types;
   type T is private;
private
   type T is new Integer;
   procedure Read
     (S : access Root_Stream_Type'Class;
      X : out T);
   procedure Write
     (S : access Root_Stream_Type'Class;
      X : in T);
   for T'Read  use Read;
   for T'Write use Write;
end ACRRT;
package body ACRRT is
   procedure Read
     (S : access Root_Stream_Type'Class;
      X : out T) is
   begin
      raise Program_Error;
   end Read;

   procedure Write
     (S : access Root_Stream_Type'Class;
      X : in T) is
   begin
      raise Program_Error;
   end Write;
end ACRRT;
with ACRRCI, ACRRT;
procedure ACRMain is
   X : ACRRT.T;
begin
   ACRRCI.P (X);
end ACRMain;
