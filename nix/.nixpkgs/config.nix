{
  packageOverrides = super: let self = super.pkgs; in
  {
    haskellEnv = self.haskell.packages.ghc7102.ghcWithPackages
                     (haskellPackages: with haskellPackages; [
                       # libraries
		       Decimal
                       # tools
                       cabal-install stack
		       # ghc-mod hlint stylish-haskell hasktags structured-haskell-mode
                     ]);
    firefoxBranded = self.wrapFirefox { browser =
      (self.firefox.override { enableOfficialBranding = true; });
    };
  };
}
