{
  age = {
    identityPaths = [ "/home/gvolpe/.config/agenix/identity.txt" ];
    secrets = {
      github-token.file = ./github-token.age;
      ngrok-token.file = ./ngrok-token.age;
      openai-api-key.file = ./openai-api-key.age;
    };
  };
}
