self: super:

{
  #coc-metals = super.nodePackages.coc-metals.overrideAttrs (
    #old: {
      #name        = "coc-metals";
      #packageName = "coc-metals";
      #version     = "0.9.0";
      #src = builtins.fetchurl {
        #url    = "https://registry.npmjs.org/coc-metals/-/coc-metals-0.9.0.tgz";
        #sha512 = "rQPCK+x+/Aij/YqjGTIS5c5WA3dPba2RwubGouXSRkl8+F1I4NyAXiXtHAx2V4W1LYvmjd990i6Dtzr3JLx+5A==";
      #};
      #dependencies = with super.nodePackages; [
        #sources."@chemzqm/neovim-5.1.9"
        #sources."async-2.6.3"
        #sources."await-semaphore-0.1.3"
        #sources."balanced-match-1.0.0"
        #sources."brace-expansion-1.1.11"
        #sources."bser-2.1.1"
        #sources."chownr-1.1.4"
        #sources."coc.nvim-0.0.77"
        #sources."concat-map-0.0.1"
        #sources."date-format-2.1.0"
        #sources."debounce-1.2.0"
        #sources."debug-4.2.0"
        #sources."deep-extend-0.6.0"
        #sources."event-lite-0.1.2"
        #sources."fast-diff-1.2.0"
        #sources."fb-watchman-2.0.1"
        #sources."flatted-2.0.2"
        #sources."follow-redirects-1.12.1"
        #sources."fp-ts-2.6.7"
        #sources."fs-extra-8.1.0"
        #sources."fs-minipass-1.2.7"
        #sources."fs.realpath-1.0.0"
        #sources."glob-7.1.6"
        #sources."graceful-fs-4.2.4"
        #sources."ieee754-1.1.13"
        #sources."inflight-1.0.6"
        #sources."inherits-2.0.4"
        #sources."ini-1.3.5"
        #sources."int64-buffer-0.1.10"
        #sources."isarray-1.0.0"
        #sources."isexe-2.0.0"
        #sources."isuri-2.0.3"
        #sources."jsonc-parser-2.3.0"
        #sources."jsonfile-4.0.0"
        #(
          #sources."locate-java-home-1.1.2" // {
            #dependencies = [
              #sources."semver-5.7.1"
            #];
          #}
        #)
        #sources."lodash-4.17.15"
        #sources."log4js-5.3.0"
        #(
          #sources."metals-languageclient-0.2.7" // {
            #dependencies = [
              #sources."mkdirp-1.0.4"
              #sources."semver-7.3.2"
            #];
          #}
        #)
        #sources."minimatch-3.0.4"
        #sources."minimist-1.2.5"
        #sources."minipass-2.9.0"
        #sources."minizlib-1.3.3"
        #sources."mkdirp-0.5.5"
        #sources."ms-2.1.2"
        #sources."msgpack-lite-0.1.26"
        #(
          #sources."mv-2.1.1" // {
            #dependencies = [
              #sources."glob-6.0.4"
              #sources."rimraf-2.4.5"
            #];
          #}
        #)
        #sources."ncp-2.0.0"
        #sources."node-fetch-2.6.0"
        #sources."node-int64-0.4.0"
        #sources."once-1.4.0"
        #sources."path-is-absolute-1.0.1"
        #sources."promisify-child-process-4.1.1"
        #sources."rc-1.2.8"
        #sources."rfc-3986-1.0.1"
        #sources."rfdc-1.1.4"
        #sources."rimraf-3.0.2"
        #sources."safe-buffer-5.2.1"
        #sources."semver-6.3.0"
        #sources."shell-quote-1.7.2"
        #sources."streamroller-2.2.4"
        #sources."strip-json-comments-2.0.1"
        #sources."tar-4.4.13"
        #sources."tslib-1.13.0"
        #sources."tunnel-0.0.6"
        #sources."universalify-0.1.2"
        #sources."uuid-3.4.0"
        #sources."vscode-jsonrpc-5.0.1"
        #sources."vscode-languageserver-protocol-3.15.3"
        #sources."vscode-languageserver-types-3.15.1"
        #sources."vscode-uri-2.1.2"
        #sources."which-1.3.1"
        #sources."wrappy-1.0.2"
        #sources."yallist-3.1.1"
      #];
      #buildInputs = [];
      #meta = {
        #description = "coc.nvim extension for Metals, the Scala language server";
        #license     = "Apache-2.0";
      #};
      #production      = true;
      #bypassCache     = true;
      #reconstructLock = true;
    #}
  #);
}
