# Search RADOGOST files

Fetch file metadata from RADOGOST, applying filename and extension
filters and following the API's pagination. Public files are returned by
default. Restricted or embargoed files may not be downloadable; data
imports are limited to formats in the package's \`dataFileTypes\`
setting.

## Usage

``` r
fetch_radogost_files(
  base_url = "https://radogost.dariah.pl/api/search",
  include_restricted = FALSE,
  per_page = 100,
  max_results = Inf,
  name_pattern = NULL,
  extension = NULL,
  timeout = 20,
  .request = radogost_get
)
```

## Arguments

- base_url:

  RADOGOST search endpoint. Only the trusted RADOGOST HTTPS endpoint is
  accepted.

- include_restricted:

  Also search restricted files. They may return HTTP 403 when
  downloaded.

- per_page:

  Number of results requested per API page.

- max_results:

  Maximum number of results to return. The default returns all matching
  pages.

- name_pattern:

  Optional filename words to match using order-independent AND logic.

- extension:

  Optional character vector of file extensions, matched using OR logic,
  e.g. `c("tab", "csv", "xlsx")`. Case and punctuation are normalized
  and duplicates removed. `NULL`, an empty vector, or a single blank
  string applies no extension filter. Each entry in a multi-extension
  vector must contain a letter or digit. Ingested tabular files are
  indexed as \`tab\`.

- timeout:

  Request timeout in seconds.

- .request:

  Internal request function, primarily used to mock API responses in
  tests.

## Value

A data frame with file name, URL, content type, access status, and
parent dataset metadata, sorted by dataset name, dataset identifier, and
file name. Tabular-file URLs request the original uploaded format; its
actual filename and extension are obtained from the download response.
The `truncated` attribute is TRUE if more matches exist or the result
limit prevents searching a remaining access filter.
