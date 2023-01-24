{ lib }:

{
  /* Return the input string with any newline marker removed.
     Type:
       removeNewline :: String -> String
  */
  removeNewline = lib.replaceStrings [ "\n" ] [ "" ];

  /* Return either `emptyValue` or `fileAction filepath`. Both need to be of the same type.

     Example:
       token = lib.secretManager {
         filepath = ./secrets/access-token;
         fileAction = file: lib.removeNewline (builtins.readFile file);
         encryptedSha256 = lib.fakeSha256;
         emptyValue = "SECRET";
       };

     Type:
       secretManager :: String -> (String -> a) -> String -> a -> a
  */
  secretManager =
    { filepath
    , fileAction ? import
    , encryptedSha256
    , emptyValue
    }:
    let
      fileHash = builtins.hashFile "sha256" filepath;
    in
    if fileHash == encryptedSha256 then emptyValue else (fileAction filepath);
}
