let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190623/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190623/src/packages.dhall sha256:c73ef468c55d3b788d639d0873b2f31f14df28f745b6c918855316637af8d9bb

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
