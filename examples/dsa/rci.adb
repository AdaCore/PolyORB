with Ada.Text_IO; use Ada.Text_IO;
with System.Address_Image;
with Ada.Real_Time; use Ada.Real_Time;

package body RCI is

   type String_Ptr is access all String;
   type Real_Obj is new RT.Obj with record
      Name : String_Ptr;
   end record;
   type Real_Obj_Ptr is access Real_Obj;
   procedure Method (Self : Real_Obj);
   procedure Method2 (Self : Real_Obj; N : Integer);
   procedure Method3 (Self : Real_Obj; Other : Real_Obj);
   function Tekitoa (Self : Real_Obj) return String;

   procedure Method (Self : Real_Obj) is
   begin
      Put_Line ("Method1 called on " & System.Address_Image (Self'Address));
   end Method;

   procedure Method2 (Self : Real_Obj; N : Integer) is
   begin
      Put_Line ("Method2 (" & N'Img & ") called on "
        & System.Address_Image (Self'Address));
   end Method2;

   procedure Method3 (Self : Real_Obj; Other : Real_Obj) is
   begin
      Put_Line ("Method2 (" & System.Address_Image (Other'Address)
         & ") called on " & System.Address_Image (Self'Address));
   end Method3;

   function Tekitoa (Self : Real_Obj) return String is
   begin
      Put_Line ("I am " & Self.Name.all);
      return Self.Name.all;
   end Tekitoa;

   procedure My_Proc (X : in Integer; Y : in out Predicate; Z : out Trit) is
   begin
      Y := Y and then Predicate (X = 0);
      Z := 1;
   end My_Proc;

   function My_Func (S : String) return Color is
   begin
      return Color'Value (S);
   end My_Func;

   function Get_Obj (Name : String) return RT.RACW is
      P : Real_Obj_Ptr;
   begin
      if Name = "" then
         return null;
      else
         P := new Real_Obj;
         P.Name := new String'(Name);
         return RT.RACW (P);
      end if;
   end Get_Obj;

   function echoString (S : String) return String is
   begin
      Put_Line ("Thus spake my client unto me: �" & S & "�.");
      return S;
   end echoString;

   function echoString_Delayed (S : String; Seconds : Integer) return String is
      use Ada.Real_Time;
   begin
      delay until Clock + To_Time_Span (Duration (Seconds));
      return echoString (S);
   end echoString_Delayed;

   function Modulus2 (Z : Complex) return Float is
   begin
      return Z.Re * Z.Re + Z.Im * Z.Im;
   end Modulus2;

   Cookie : Integer := 0;

   function Get_Cookie return Integer is
   begin
      return Cookie;
   end Get_Cookie;

   procedure Delayed_Set_Cookie (Cookie : Integer) is
   begin
      delay until Clock + Milliseconds (2_000);
      RCI.Cookie := Cookie;
   end Delayed_Set_Cookie;

   procedure Raise_Program_Error is
   begin
      raise Program_Error;
   end Raise_Program_Error;

   procedure Raise_Visible is
   begin
      raise Visible;
   end Raise_Visible;

   procedure Raise_Invisible is
      Invisible : exception;
   begin
      raise Invisible;
   end Raise_Invisible;

end RCI;
