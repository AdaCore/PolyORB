package body AdaBroker.OmniRopeAndKey is

   ----------------
   -- C_Get_Rope --
   ----------------

   function C_Get_Rope (Self : in Object'Class) return System.Address;
   pragma Import (CPP, C_Get_Rope, "rope__18Ada_OmniRopeAndKey");
   --  Wrapper around Ada_OmniRopeAndKey function rope (see
   --  Ada_OmniRopeAndKey.hh) called by the Ada equivalent : Get_Rope

   --------------
   -- Get_Rope --
   --------------

   function Get_Rope
     (Self : in Object'Class)
      return Rope.Object is
   begin
      --  Just calls the C function
      return Rope.Object (C_Get_Rope (Self));
   end Get_Rope;

   ----------------
   -- C_Key_Size --
   ----------------

   function C_Key_Size
     (Self : in Object'Class)
      return Interfaces.C.unsigned_long;

   pragma Import (CPP, C_Key_Size, "keysize__18Ada_OmniRopeAndKey");
   --  Wrapper around Ada_OmniRopeAndKey function keysize (see
   --  Ada_OmniRopeAndKey.hh) called by the Ada equivalent : Key_Size

   --------------
   -- Key_Size --
   --------------

   function Key_Size
     (Self : in Object'Class)
      return CORBA.Unsigned_Long
   is
      C_Result : Interfaces.C.unsigned_long;
   begin
      --  Call the C function ...
      C_Result := C_Key_Size (Self);

      --  Transforms the result in Ada type
      return CORBA.Unsigned_Long (C_Result);
   end Key_Size;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Controlled_Wrapper) is
   begin
      Init (Self.Real);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Controlled_Wrapper) is
   begin
      Free (Self.Real);
   end Finalize;

end AdaBroker.OmniRopeAndKey;
