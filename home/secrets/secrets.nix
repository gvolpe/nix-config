let
  age = "age1g039cd5yh7as64usdqtcan3hyayjr6ckdgkqnun6u9sungergc8src6ssx";
  aorus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINl8iI0fjtHayHg0v2dwNvVe8qKBJsWx0bn9mvBpFJc8";
  thinkpad = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJSoMji4srKCvvoWiCdznVkpvQ27lwLytTdIEYxwNMJB";
  keys = [ age aorus thinkpad ];
in
{
  "github-token.age".publicKeys = keys;
  "ngrok-token.age".publicKeys = keys;
  "openai-api-key.age".publicKeys = keys;
}
