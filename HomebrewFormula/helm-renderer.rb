require "language/haskell"

class HelmRenderer < Formula
  include Language::Haskell::Cabal
  version "1.0.0"
  desc "Render Helm charts for your k8s cluster locally"
  url "https://github.com/afnanenayet/helm-renderer/releases/download/v0.1.4.1/helm-renderer-darwin"
  homepage "https://github.com/afnan/helm-renderer"
  head "https://github.com/afnan/helm-renderer.git"

  depends_on "stack" => :build
  depends_on "cabal-install" => :build
  depends_on "ghc" => :build

  bottle do
    mv "helm-renderer-darwin" "helm-renderer"
  end

  def install
    cabal_sandbox do
      install_cabal_package
    end
  end
end
