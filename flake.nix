{
  description = "Nix dev shells, for testing tree-guide.el with multiple Emacs versions";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # I determined the nixpkgs commits corresponding to the various
    # Emacs versions using the "nix-versions" website [1]. For further
    # explanation about how the website works, see the accompanying
    # blog post [2].
    #
    # [1]: https://lazamar.co.uk/nix-versions/?channel=nixpkgs-unstable&package=emacs
    # [2]: https://lazamar.github.io/download-specific-package-version-with-nix/
    nixpkgs-emacs27_1.url = "github:nixos/nixpkgs/0ffaecb6f04404db2c739beb167a5942993cfd87";
    nixpkgs-emacs28_2.url = "github:nixos/nixpkgs/0bd7f95e4588643f2c2d403b38d8a2fe44b0fc73";
    nixpkgs-emacs29_4.url = "github:nixos/nixpkgs/0bd7f95e4588643f2c2d403b38d8a2fe44b0fc73";
    nixpkgs-emacs30_2.url = "github:nixos/nixpkgs/4c448105b24f5ff0b8d1bc512e488120123c047e";
    nixpkgs-emacs31_1.url = "github:nixos/nixpkgs/b2433d29596bc700a008ba0c70aa1659e7595b0c";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      pkgs-emacs27_1 = import inputs.nixpkgs-emacs27_1 { inherit system; };
      pkgs-emacs28_2 = import inputs.nixpkgs-emacs28_2 { inherit system; };
      pkgs-emacs29_4 = import inputs.nixpkgs-emacs29_4 { inherit system; };
      pkgs-emacs30_2 = import inputs.nixpkgs-emacs30_2 { inherit system; };
      pkgs-emacs31_1 = import inputs.nixpkgs-emacs31_1 { inherit system; };
      mkDevShell = emacs-pkg: pkgs.mkShell {
          buildInputs = [ emacs-pkg ];
      };
    in
    {
      devShells.${system} = {
        emacs27_1 = mkDevShell pkgs-emacs27_1.emacs;
        emacs28_2 = mkDevShell pkgs-emacs28_2.emacs28;
        emacs29_4 = mkDevShell pkgs-emacs29_4.emacs;
        emacs30_2 = mkDevShell pkgs-emacs30_2.emacs;
        emacs31_1 = mkDevShell pkgs-emacs31_1.emacs;
      };
    };
}