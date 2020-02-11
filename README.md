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
