# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

let
  customFonts = pkgs.nerdfonts.override {
    fonts = [
      "JetBrainsMono"
      "Iosevka"
    ];
  };

  myfonts = pkgs.callPackage fonts/default.nix { inherit pkgs; };
in
{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # Machine-specific configuration
      ./machine/current.nix
      # Window manager 
      ./wm/xmonad.nix
    ];

  networking = {
    # Enables wireless support and openvpn via network manager.
    networkmanager = {
      enable   = true;
      packages = [ pkgs.networkmanager_openvpn ];
    };

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "Europe/Warsaw";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    wget
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable           = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # Enable Docker & VirtualBox support.
  virtualisation = {
    docker = {
      enable = true;
    };
   virtualbox.host = {
     enable = true;
   #  enableExtensionPack = true;
   };
  };

  users.extraGroups.vboxusers.members = [ "gvolpe" ];

  # Enable sound.
  sound = {
    enable = true;
    mediaKeys.enable = true;
  };
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services = {
    # Enable the OpenSSH daemon.
    openssh.enable = true;

    # Enable CUPS to print documents.
    printing.enable = true;
  };

  # Making fonts accessible to applications.
  fonts.fonts = with pkgs; [
    customFonts
    font-awesome-ttf
    myfonts.icomoon-feather
  ];

  programs.fish.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.gvolpe = {
    isNormalUser = true;
    extraGroups  = [ "docker" "networkmanager" "wheel" ]; # wheel for ‘sudo’.
    shell        = pkgs.fish;
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
      packageOverrides = pkgs: rec {
        virtualbox = pkgs.virtualbox.overrideAttrs (attrs: {
          patches = attrs.patches ++ [ /etc/nixos/virtualbox/kernel_5_10_x.patch ];
        });
      };
    };
  };

  # Nix daemon config
  nix = {
    # Automate `nix-store --optimise`
    autoOptimiseStore = true;

    # Automate garbage collection
    gc = {
      automatic = true;
      dates     = "weekly";
      options   = "--delete-older-than 7d";
    };

    # Avoid unwanted garbage collection when using nix-direnv
    extraOptions = ''
      keep-outputs     = true
      keep-derivations = true
    '';

    # Required by Cachix to be used as non-root user
    trustedUsers = [ "root" "gvolpe" ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}
