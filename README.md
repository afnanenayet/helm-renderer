# helm-renderer

![Haskell CI](https://github.com/afnanenayet/helm-renderer/workflows/Haskell%20CI/badge.svg)

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

## Usage

```txt
helm-renderer: render Helm charts

Usage: helm-renderer CHARTNAME [-n|--namespace NAMESPACE] [-f|--values VALUES]
                     [--out OUTDIR]
  Render Helm charts

Available options:
  CHARTNAME                The name of the chart in the form 'repo/chartname'
  -n,--namespace NAMESPACE The namespace to pass to Helm
  -f,--values VALUES       The path to the optional values YAML
  --out OUTDIR             The output directory to write the generated files to
  -h,--help                Show this help text
```

## Installation

### Stack

If you want (or have) to build from source:

```sh
git clone https://github.com/afnanenayet/helm-renderer.git
cd helm-renderer
stack install --local-bin-path=/usr/local/bin # or whatever path you want
```

### Homebrew

To use the Homebrew tap:

```sh
brew tap afnanenayet/helm-renderer
brew install helm-renderer
```

## Development

This project uses stack for building and testing, so all of the usual stack
commands apply.
