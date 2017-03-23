{ ... }@args: (import ./default.nix args).env // {
  withHoogle = true;
  shellHooks = ''
    alias hoogle="hoogle --database=.katas.hoo"
    hoogle generate
  '';
}
