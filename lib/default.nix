{ lib }:

{
  /* Return first binary executable name of the given derivation
     Type:
       exe :: Derivation -> String
  */
  exe = drv:
    let
      regFiles = lib.mapAttrsToList (f: _: f) (lib.filterAttrs (_: t: t == "regular") (builtins.readDir "${drv}/bin"));
      mainProg = drv.meta.mainProgram or (lib.head regFiles);
    in
    "${drv}/bin/${mainProg}";

  /* Return the input string with any newline marker removed.
     Type:
       removeNewline :: String -> String
  */
  removeNewline = lib.replaceStrings [ "\n" ] [ "" ];

  /* Return either `emptyValue` or `fileAction filepath`. Both need to be of the same type.

     Example:
       token = lib.secretManager {
         filepath = ./secrets/access-token;
         fileAction = file: lib.removeNewline (lib.readFile file);
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
