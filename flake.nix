{
  description = "Top-level flake for building dnsflare";

  inputs.dnsflare-src.url = "path:./src";

  outputs = { self, dnsflare-src }: {
    packages = dnsflare-src.packages;
  };

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = "true";
  };
}
