require "language/haskell"

class HelmRenderer < Formula
  include Language::Haskell::Cabal
  version "0.1.4.1"
  desc "Render Helm charts for your k8s cluster locally"
  url "https://github.com/afnanenayet/helm-renderer/releases/download/v#{version}/helm-renderer-darwin"
  homepage "https://github.com/afnan/helm-renderer"
  head "https://github.com/afnan/helm-renderer.git"

  depends_on "stack" => :build
  depends_on "cabal-install" => :build
  depends_on "ghc" => :build

  if OS.mac?
    url "https://github.com/afnanenayet/helm-renderer/releases/download/v#{version}/helm-renderer-darwin.tar.gz"
    sha256 "92635e29711da0910f08235ea93adbccc38c1bfbe9c35a050cfd738e845bc808"
  elsif OS.linux?
    url "https://github.com/afnanenayet/helm-renderer/releases/download/v#{version}/helm-renderer-linux.tar.gz"
  end

  def install
    bin.install "helm-renderer"
  end

  test do
    shell_output "#{bin}/helm-renderer --help"
  end
end
