with CORBA.Object.OmniORB;
with Sema.Skel;
with AdaBroker.Exceptions; use AdaBroker.Exceptions;
package body Sema.Impl is 

   protected Semaphore is
      entry     P (N : Natural);
      procedure V (N : Natural);
      procedure Try_P (N : Natural; R : out Boolean);
   private
      entry Wait (N : Natural);
      Available : Natural := 20;
      Busy      : Boolean := False;
   end Semaphore;

   protected body Semaphore is
      entry P (N : Natural) when not Busy is
      begin
         if Available - N < 0 then
            requeue Wait with abort;
         end if;
         Available := Available - N;
      end P;

      entry Wait (N : Natural) when Busy is
      begin
         if Wait'Count = 0 then
            Busy := False;
         end if;
         requeue P with abort;
      end Wait;

      procedure V (N : Natural) is
      begin
         Available := Available + N;
         if Wait'Count > 0 then
            Busy := True;
         end if;
      end V;

      procedure Try_P (N : Natural; R : out Boolean) is
      begin
         if Available - N < 0 then
            R := False;
         else
            Available := Available - N;
            R := True;
         end if;
      end Try_P;
   end Semaphore;

   procedure Try_P
     (Self : access Object;
      Amount : in CORBA.Long)
   is
      Done : Boolean;
   begin 
      Semaphore.Try_P (Natural (Amount), Done);
      if not Done then
         declare
            M : Empty_Members;
         begin
            M.Missing := Amount;
            Raise_CORBA_Exception (Empty'Identity, M);
         end;
      end if;
   end Try_P;

   procedure P
     (Self : access Object;
      Amount : in CORBA.Long)
   is
   begin 
      Semaphore.P (Natural (Amount));
   end P;

   procedure V
     (Self : access Object;
      Amount : in CORBA.Long)
   is
   begin 
      Semaphore.V (Natural (Amount));
   end V;

   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   procedure Initialize (Self : in out Object) is
   begin
      AdaBroker.OmniORB.Initialize
        (AdaBroker.OmniORB.ImplObject (Self),
         Sema.Repository_Id);
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
     (Sema.Repository_Id,
      Sema.Nil_Ref,
      Sema.Skel.Dispatch'Access);
end Sema.Impl;
