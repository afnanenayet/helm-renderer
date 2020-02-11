require "language/haskell"

class HelmRenderer < Formula
  include Language::Haskell::Cabal
  version "1.0.0"
  desc "Render Helm charts for your k8s cluster locally"
  homepage "https://git.blendlabs.com/blend/helm-renderer"
  head "https://git.blendlabs.com/blend/helm-renderer.git"

  depends_on "stack" => :build
  depends_on "cabal-install" => :build
  depends_on "ghc" => :build

  def install
    cabal_sandbox do
      install_cabal_package
    end
  end
end
