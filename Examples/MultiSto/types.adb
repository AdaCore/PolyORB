package body Types is
   procedure Set (W : in out Word; S : String) is
      L : Natural := S'Length;
   begin
      if L > W'Length then
         L := W'Length;
      end if;
      for I in W'First .. W'First + L - 1 loop
         W (I) := S (S'First + I - W'First);
      end loop;
      for I in W'First + L .. W'Last loop
         W (I) := ' ';
      end loop;
   end Set;
end Types;
