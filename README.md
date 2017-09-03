Work in progress

Build:
```
pulp build -O -I test -m Test.Main -t html/test.js
```

Generate JSON documentation files for a given project:
```
bower list --json --offline > /tmp/resolutions.json
purs publish --manifest bower.json --resolutions /tmp/resolutions.json > /tmp/this-project.json
```
