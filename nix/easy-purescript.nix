{ pkgs }:
import (pkgs.fetchFromGitHub {
  owner = "justinwoo";
  repo = "easy-purescript-nix";
  rev = "11d3bd58ce6e32703bf69cec04dc7c38eabe14ba";
  sha256 = "0q24hb4a3fvcizns17ddd4pshlbkfdq2m6pgcjfslwlvgnbrli5l";
}) {
  inherit pkgs;
}