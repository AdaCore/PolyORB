package body Alarm is

   protected Keeper is
      entry Read
        (Divider : out Natural;
         Where   : out Natural);
      procedure Write
        (Divider : in Natural;
         Where   : in Natural);
   private
      Arrived       : Boolean := False;
      Saved_Divider : Natural;
      Saved_Where   : Natural;
   end Keeper;

   protected body Keeper is
      entry Read
        (Divider : out Natural;
         Where   : out Natural) when Arrived is
      begin
         Divider := Saved_Divider;
         Where   := Saved_Where;
         Arrived := False;
      end;
      procedure Write
        (Divider : in Natural;
         Where   : in Natural) is
      begin
         Saved_Divider := Divider;
         Saved_Where   := Where;
         Arrived       := True;
      end Write;
   end Keeper;

   procedure Write
     (Divider : in Natural;
      Where   : in Natural) is
   begin
      Keeper.Write (Divider, Where);
   end Write;

   procedure Read
     (Divider : out Natural;
      Where   : out Natural) is
   begin
      Keeper.Read (Divider, Where);
   end Read;

end Alarm;
 
