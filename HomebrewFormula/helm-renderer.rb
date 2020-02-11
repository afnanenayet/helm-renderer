class HelmRenderer < Formula
  version "1.0.0"
  desc "Render Helm charts for your k8s cluster locally"
  homepage "https://git.blendlabs.com/blend/helm-renderer"
  url "https://git.blendlabs.com/blend/helm-renderer/releases/download/v#{version}/helm-renderer"
  sha256 "82db07665b4dc2a9f07d8e4b1a86e8e019ab67121e57f906130489e08263087a"

  def install
    bin.install "helm-renderer"
  end
end
