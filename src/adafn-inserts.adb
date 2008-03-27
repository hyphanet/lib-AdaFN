package body Adafn.Inserts is

   use Key;

   ------------
   -- Create --
   ------------

   function Create (Kind : Key.Kinds;
                    Uri  : Key.Object := Key.Null_Object) return Inserts.Uri
   is
   begin
      case Kind is
         when Chk =>
            return (Chk, Uri, Uri /= Key.Null_Object);
         when Ssk =>
            return (Ssk, Uri);
         when Ksk =>
            return (Ksk, Uri);
         when Usk =>
            return (Usk, Uri);
      end case;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Uri : Key.Object) return Inserts.Uri is
   begin
      return Create (Key.Kind (Uri), Uri);
   end Create;

   -----------
   -- Image --
   -----------

   function Image (This : Uri) return String is
   begin
      if This.Kind = Chk then
         if This.Has_Uri then
            return Image (This.Uri);
         else
            return "CHK@";
         end if;
      else
         return Image (This.Uri);
      end if;
   end Image;

   -------------
   -- Get_Uri --
   -------------

   function Get_Uri (This : Uri) return Key.Object is
   begin
      if This.Kind = Chk and then This.Has_Uri then
         return This.Uri;
      elsif This.Kind /= Chk then
         return This.Uri;
      else
         raise Constraint_Error with "Uri is incomplete CHK";
      end if;
   end Get_Uri;

   -------------
   -- Has_Uri --
   -------------

   function Has_Uri (This : Uri) return Boolean is
   begin
      return This.Kind /= Chk or else This.Has_Uri;
   end Has_Uri;

   -------------
   -- Set_Uri --
   -------------

   procedure Set_Uri (This : in out Uri; Uri : Key.Object) is
   begin
      This.Uri := Uri;
      if This.Kind = Chk then
         This.Has_Uri := True;
      end if;
   end Set_Uri;

end Adafn.Inserts;
