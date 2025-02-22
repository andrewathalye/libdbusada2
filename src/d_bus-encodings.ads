pragma Ada_2012;

package D_Bus.Encodings is
   --  Note This package produces lowercase hex output, but
   --  accepts both lowercase and uppercase hex input.

   Invalid_Encoding : exception;

   function Encode_Server_Address (Text : String) return String;
   function Decode_Server_Address (Text : String) return String;
   --  Process the `URL-Encoding` used by parameters for server
   --  addresses.
   --
   --  Raise `Invalid_Encoding` if format is invalid.

   function To_Hex (Text : String) return String;
   function From_Hex (Text : String) return String;
   --  Convert between hex-encoded strings and
   --  normal byte-strings. An exception will be
   --  raised if the input to From_Hex is not hex encoded.
   --
   --  TODO raise Invalid_Encoding
end D_Bus.Encodings;
