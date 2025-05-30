# Create MarkDown documentation from the OpenAPI spec

```bash
cd docs/openapi
npx @openapitools/openapi-generator-cli generate -i
 ../../priv/dog_openapi.yaml -g markdown
 ```
