with Echo.Skeleton ;
with Corba ;


package body Echo.Impl is


   -----------------------
   -- IDL definitions   --
   -----------------------

   function echoString(Self : access Object; mesg : in Corba.String) return Corba.String is
   begin
      return mesg ;
   end ;


   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   -- Initialize
   -------------
   procedure Initialize(Self : in Out Object) is
   begin
      Omniobject.Initialize(Omniobject.Implemented_Object(Self)) ;
      Init_Local_Object(Self,
                        Repository_Id,
                        Echo.Skeleton.Dispatch'Access,
                        Echo.Is_A'Access) ;
      -- You can add things *BELOW* this line

   end Initialize ;


   -- Adjust
   ---------
   procedure Adjust(Self: in out Object) is
   begin
      Omniobject.Adjust(Omniobject.Implemented_Object(Self)) ;
      -- You can add things *BELOW* this line

   end Adjust ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out Object) is
   begin

      -- You can add things *BEFORE* this line
      Omniobject.Finalize(Omniobject.Implemented_Object(Self)) ;
   end Finalize ;


end Echo.Impl ;
