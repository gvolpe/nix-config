# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, inputs, ... }:

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
      # Window manager
      ./wm/xmonad.nix
      # Binary cache
      ./cachix.nix
    ];

  networking = {
    # Enables wireless support and openvpn via network manager.
    networkmanager = {
      enable = true;
      plugins = [ pkgs.networkmanager-openvpn ];
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
    firejail
    vim
    wget
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
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
      autoPrune = {
        enable = true;
        dates = "weekly";
      };
    };

    virtualbox.host = {
      enable = false;
      enableExtensionPack = false;
    };
  };

  users.extraGroups.vboxusers.members = [ "gvolpe" ];

  # Enable sound.
  sound = {
    enable = true;
    mediaKeys.enable = true;
  };

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  # Scanner backend
  hardware.sane = {
    enable = true;
    extraBackends = [ pkgs.epkowa pkgs.sane-airscan ];
  };

  services = {
    # Mount MTP devices
    gvfs.enable = true;

    # Enable the OpenSSH daemon.
    openssh = {
      enable = true;
      allowSFTP = true;
    };

    # Yubikey smart card mode (CCID) and OTP mode (udev)
    pcscd.enable = true;
    udev.packages = [ pkgs.yubikey-personalization ];

    # SSH daemon.
    sshd.enable = true;

    # Enable CUPS to print documents.
    printing = {
      enable = true;
      drivers = [ pkgs.epson-escpr ];
    };
  };

  # Making fonts accessible to applications.
  fonts.fonts = with pkgs; [
    customFonts
    font-awesome
    myfonts.flags-world-color
    myfonts.icomoon-feather
  ];

  programs.fish.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.gvolpe = {
    isNormalUser = true;
    extraGroups = [ "docker" "networkmanager" "wheel" "scanner" "lp" ]; # wheel for ‘sudo’.
    shell = pkgs.fish;
  };

  security = {
    # Yubikey login & sudo
    pam.yubico = {
      enable = true;
      debug = false;
      mode = "challenge-response";
    };

    # Sudo custom prompt message
    sudo.configFile = ''
      Defaults lecture=always
      Defaults lecture_file=${misc/groot.txt}
    '';
  };

  nixpkgs.config.allowUnfree = true;

  # Nix daemon config
  nix = {
    # Automate garbage collection
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };

    # Flakes settings
    package = pkgs.nixVersions.stable;
    registry.nixpkgs.flake = inputs.nixpkgs;

    settings = {
      # Automate `nix store --optimise`
      auto-optimise-store = true;

      # Required by Cachix to be used as non-root user
      trusted-users = [ "root" "gvolpe" ];

      experimental-features = [ "nix-command" "flakes" ];

      # Avoid unwanted garbage collection when using nix-direnv
      keep-outputs = true;
      keep-derivations = true;
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.03"; # Did you read the comment?

}
