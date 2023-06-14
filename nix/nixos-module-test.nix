{ pkgs
, home-manager
, tpa-home-manager-module
}:
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "tpa-module-test";
    nodes.machine = {
      imports = [ home-manager ];
      system.stateVersion = "23.05";
      users.users.testuser.isNormalUser = true;
      home-manager = {
        useGlobalPkgs = true;
        users.testuser = { pkgs, ... }: {
          imports = [ tpa-home-manager-module ];
          home.stateVersion = "23.05";
          xdg.enable = true;
          programs.tpa = {
            enable = true;
          };
        };
      };
    };
    testScript = ''
      from shlex import quote

      machine.start()
      machine.wait_for_unit("multi-user.target")

      machine.wait_for_unit("home-manager-testuser.service")

      def su(user, cmd):
          return f"su - {user} -c {quote(cmd)}"

      machine.succeed(su("testuser", "cat ~/.config/tpa/config.yaml"))
      machine.succeed(su("testuser", "tpa --help"))
    '';
  }
)
