with Ada.Text_IO; use Ada.Text_IO;
with System.Address_Image;

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
      Msg : constant String := "I am " & Self.Name.all;
   begin
      Put_Line (Msg);
      return Msg;
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
      Put_Line ("Thus spake my client unto me: «" & S & "».");
      return S;
   end echoString;

end RCI;
