# helm-renderer

Render Helm charts to YAML files that can be used directly with 
`kubectl apply -f`. This provides some more functionality than using Helm to
render templates locally. This actually performs a dry-run with Helm so it
reaches out to the Kubernetes API server and fills in as many values in the
template as possible. This tool allows a user to provide a values yaml file
that overrides or fills in values for the Helm chart template.

The tool also splits up the massive YAML file that Helm provides into separate
YAML files for readability and maintainability while providing folder
structure.

For example, if you have a file of the form

```yaml
---
# Source: template/a/a.yaml
...
---
# Source: template/b/b.yaml
...
```

The tool will output:

```txt
out
--- a/a.yaml
--- b/b.yaml
```

which preserves the file structure from the actual Helm charts, so you can
mimic the Helm installation while having the charts be split up.

## Installation

### Stack

If you want (or have) to build from source:

```sh
git clone git@git.blendlabs.com:blend/helm-renderer.git
cd helm-renderer
stack install --local-bin-path=/usr/local/bin # or whatever path you want
```

### Homebrew

If you want to use Homebrew, you need to make sure you have this in your
Git config, which will force Homebrew to use SSH to clone the repo, since it
doesn't work with SAML:

```config
[url "git@git.blendlabs.com:"]
    insteadOf = "https://git.blendlabs.com/"
```

To use the Homebrew tap:

```sh
brew tap afnan/helm-renderer git@git.blendlabs.com:blend/helm-renderer.git
# Can't use versioned releases with our private git instance :/
brew install --HEAD helm-renderer
```

## Development

This project uses stack for building and testing, so all of the usual stack
commands apply.
