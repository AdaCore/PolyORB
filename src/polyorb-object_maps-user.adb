
with PolyORB.Log;
with PolyORB.Types;

package body PolyORB.Object_Maps.User is

   use Map_Entry_HTables;

   use PolyORB.Log;
   use PolyORB.Types;

   package L is new Log.Facility_Log ("polyorb.object_maps.user");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization
     (O_Map : access User_Object_Map);

   procedure Ensure_Initialization
     (O_Map : access User_Object_Map) is
   begin
      if O_Map.Initialized then
         return;
      end if;

      Initialize (O_Map.User_Map);
      O_Map.Initialized := True;
   end Ensure_Initialization;

   ---------
   -- Add --
   ---------

   procedure Add
     (O_Map : access User_Object_Map;
      Obj   : in     Object_Map_Entry_Access) is
   begin
      Ensure_Initialization (O_Map);
      Insert (O_Map.User_Map, To_Standard_String (Obj.Oid.Id), Obj);
   end Add;

   ---------------
   -- Get_By_Id --
   ---------------

   function Get_By_Id
     (O_Map : in User_Object_Map;
      Item  : in PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access is
   begin
      pragma Debug (O ("User generated OID, look up in table"));

      if not O_Map.Initialized then
         return null;
      else
         return Lookup (O_Map.User_Map, To_Standard_String (Item.Id), null);
      end if;
   end Get_By_Id;

   --------------------
   -- Get_By_Servant --
   --------------------

   function Get_By_Servant
     (O_Map  : in User_Object_Map;
      Item   : in PolyORB.Servants.Servant_Access)
     return Object_Map_Entry_Access
   is
      use type PolyORB.Servants.Servant_Access;

      It : Iterator := First (O_Map.User_Map);
   begin
      while not Last (It) loop
         if not Is_Null (Value (It)) then
            pragma Debug (O ("Examinating elt: "
                             & To_Standard_String (Value (It).Oid.Id)));

            if Value (It).Servant = Item then
               pragma Debug (O ("Found !"));
               return Value (It);
            end if;
         end if;

         Next (It);
      end loop;

      pragma Debug (O ("Not Found !"));
      return null;
   end Get_By_Servant;

   ------------------
   -- Remove_By_Id --
   ------------------

   function Remove_By_Id
     (O_Map : access User_Object_Map;
      Item  : in     PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access
   is
      Old_Entry : Object_Map_Entry_Access;

      Name : constant String := To_Standard_String (Item.Id);
   begin
      Old_Entry := Lookup (O_Map.User_Map, Name, null);
      Delete (O_Map.User_Map, Name);
      return Old_Entry;
   end Remove_By_Id;

end PolyORB.Object_Maps.User;
