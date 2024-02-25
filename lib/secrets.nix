{ lib, ... }:

{
  githubToken = lib.secretManager {
    filepath = ../home/secrets/github-token;
    fileAction = lib.readFile;
    encryptedSha256 = "ce08397723eb2c8ae9673de34fab11db8799062c2659c673b4eddbfa4e05b8e1";
    emptyValue = "";
  };

  mimeoAssociations = lib.secretManager {
    filepath = ../home/secrets/mimeo-associations.txt;
    fileAction = lib.readFile;
    encryptedSha256 = "255dcce51ca73d2370bbdf1bf7e5c3f894da2b85049a2cec91e4afecb66e2087";
    emptyValue = "";
  };

  ngrokToken = lib.secretManager {
    filepath = ../home/secrets/ngrok-token;
    fileAction = file: lib.removeNewline (lib.readFile file);
    encryptedSha256 = "c09ec94f0ce53f889cf4f26576f238371ce5f7c761d3420e1b886934b0b04eaf";
    emptyValue = "SECRET";
  };

  openaiApiKey = lib.secretManager {
    filepath = ../home/secrets/openai-api-key;
    fileAction = file: lib.removeNewline (lib.readFile file);
    encryptedSha256 = "80c866e45d4344f12fa4d98d29cadd27d31d68fd402c97fe05eced6b470ebae8";
    emptyValue = "SECRET";
  };
}
