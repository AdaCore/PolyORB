with CORBA.Object.OmniORB;
with Modifier.Skel;

with Ada.Text_IO;

package body Modifier.Impl is

   function modify
     (Self : access Object;
      arg : in example)
      return example
   is
      Sw : CORBA.Long := Arg.Switch;
   begin
      case Sw is
         when 1 =>
            declare
               Tmp : Example (2);
            begin
               Ada.Text_IO.Put_Line ("received an union sw = 1");
               Ada.Text_IO.Put_Line ("counter = " & Arg.Counter'Img);
               Tmp.Flag := True;
               return Tmp;
            end;
         when 2 =>
            declare
               Tmp : Example (1);
            begin
               Ada.Text_IO.Put_Line ("received an union sw = 2");
               Ada.Text_IO.Put_Line ("flag = " & Arg.Flag'Img);
               Tmp.Counter := CORBA.Long (2706);
               return Tmp;
            end;
         when others =>
            Ada.Text_IO.Put_Line ("received an union sw = others");
            return Arg;
      end case;
   end modify;

   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   procedure Initialize (Self : in out Object) is
   begin
      AdaBroker.OmniORB.Initialize
        (AdaBroker.OmniORB.ImplObject (Self),
         Modifier.Repository_Id);
      -- Add user code *BELOW* this line
   end Initialize;

   procedure Adjust (Self: in out Object) is
   begin
      AdaBroker.OmniORB.Adjust
        (AdaBroker.OmniORB.ImplObject (Self));
      -- Add user code *BELOW* this line
   end Adjust;

   procedure Finalize (Self : in out Object) is
   begin
      -- Add user code *BEFORE* this line
      AdaBroker.OmniORB.Finalize
        (AdaBroker.OmniORB.ImplObject (Self));
   end Finalize;

begin
   CORBA.Object.OmniORB.Register
     (Modifier.Repository_Id,
      Modifier.Nil_Ref,
      Modifier.Skel.Dispatch'Access);
end Modifier.Impl;
