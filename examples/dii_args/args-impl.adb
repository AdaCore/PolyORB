with CORBA.Object.OmniORB;
with Args.Skel;
with CORBA;
with Ada.Text_IO;
use Ada.Text_IO;

package body Args.Impl is

   function Plus1
     (Self : access Object;
      A : in CORBA.Long)
      return CORBA.Long is
   begin
      Put_Line ("Argument received is : "
                & A'Img);
      return (CORBA.Long (Integer (A)));
   end Plus1;

   function Plus2
     (Self : access Object;
      A : in CORBA.Long;
      B : in CORBA.Long)
      return CORBA.Long is
   begin
      Put_Line ("Arguments received are : "
                & A'Img & " "
                & B'Img);
      return (CORBA.Long (Integer (A) + Integer (B)));
   end Plus2;


   function Plus3
     (Self : access Object;
      A : in CORBA.Long;
      B : in CORBA.Long;
      C : in CORBA.Long)
      return CORBA.Long is
   begin
      Put_Line ("Arguments received are : "
                & A'Img & " "
                & B'Img & " "
                & C'Img);
      return (CORBA.Long (Integer (A) + Integer (B) + Integer (C)));
   end Plus3;


   procedure Plus_Minus
     (Self : access Object;
      A : in CORBA.Long;
      B : in CORBA.Long;
      P : out CORBA.Long;
      M : out CORBA.Long)
   is
   begin
      Put_Line ("Arguments received are : "
                & A'Img & " "
                & B'Img);
      P := CORBA.Long (Integer (A) + Integer (B));
      M := CORBA.Long (Integer (A) - Integer (B));
   end Plus_Minus;


   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   procedure Initialize (Self : in out Object) is
   begin
      AdaBroker.OmniORB.Initialize
        (AdaBroker.OmniORB.ImplObject (Self),
         Args.Repository_Id);
      -- You can add things *BELOW* this line

   end Initialize;

   procedure Adjust (Self: in out Object) is
   begin
      AdaBroker.OmniORB.Adjust
     (AdaBroker.OmniORB.ImplObject (Self));
      -- You can add things *BELOW* this line

   end Adjust;

   procedure Finalize (Self : in out Object) is
   begin

      -- You can add things *BEFORE* this line
      AdaBroker.OmniORB.Finalize
     (AdaBroker.OmniORB.ImplObject (Self));
   end Finalize;

begin
   CORBA.Object.OmniORB.Register
     (Args.Repository_Id,
      Args.Nil_Ref,
      Args.Skel.Dispatch'Access);
end Args.Impl;
