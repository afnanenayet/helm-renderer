require "language/haskell"

class HelmRenderer < Formula
  include Language::Haskell::Cabal
  version "0.1.4.2"
  desc "Render Helm charts for your k8s cluster locally"
  homepage "https://github.com/afnan/helm-renderer"
  head "https://github.com/afnan/helm-renderer.git"

  url "https://github.com/afnanenayet/helm-renderer/releases/download/v#{version}/helm-renderer-darwin.tar.gz"
  sha256 "75109e113dd0da5e8dca314610345d51ba912b7cc0130a76a63fdbd048156eed"

  def install
    bin.install "helm-renderer"
  end

  test do
    shell_output "#{bin}/helm-renderer --help"
  end
end
