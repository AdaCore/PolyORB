with Echo.Skeleton ;
with CORBA ;


package body Echo.Impl is 


   -----------------------
   -- IDL definitions   --
   -----------------------

   --  echoString
   -------------------------------
   function echoString(Self : access Object; mesg : in CORBA.String) return CORBA.String is
   begin 
      return mesg;
   end ;





   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   -- Initialize
   -------------
   procedure Initialize(Self : in out Object) is
   begin
      AdaBroker.OmniORB.Initialize(AdaBroker.OmniORB.ImplObject(Self));
      Initialize_Local_Object(Self,
                        Repository_Id,
                        Echo.Skeleton.Dispatch'Access);
      -- You can add things *BELOW* this line

   end Initialize;


   -- Adjust
   ---------
   procedure Adjust(Self: in out Object) is
   begin
   AdaBroker.OmniORB.Adjust(AdaBroker.OmniORB.ImplObject(Self));
      -- You can add things *BELOW* this line

   end Adjust;


   -- Finalize
   -----------
   procedure Finalize(Self : in out Object) is
   begin

      -- You can add things *BEFORE* this line
   AdaBroker.OmniORB.Finalize(AdaBroker.OmniORB.ImplObject(Self));
   end Finalize;


end Echo.Impl;
