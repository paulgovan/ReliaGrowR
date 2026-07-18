# Start the ReliaGrowR MCP Server

Starts a Model Context Protocol (MCP) server that exposes the core
ReliaGrowR analysis functions as tools for AI assistants such as Claude.

## Usage

``` r
rga_mcp_server(...)
```

## Arguments

- ...:

  Additional arguments passed to
  [`mcptools::mcp_server()`](https://posit-dev.github.io/mcptools/reference/server.html),
  e.g., `type = "stdio"` (default) or `type = "http"`.

## Value

Called for its side effect of starting the MCP server.

## Details

Requires the mcptools and ellmer packages to be installed.

## Available tools

- `rga`:

  Fit Crow-AMSAA reliability growth model

- `nhpp`:

  Fit NHPP model for repairable systems

- `duane`:

  Fit Duane log-log reliability growth model

- `mcf`:

  Compute Mean Cumulative Function

- `predict_rga`:

  Forecast from RGA model

- `predict_duane`:

  Forecast MTBF from Duane model

- `rdt`:

  Plan a Reliability Demonstration Test

- `gof_rga`:

  Goodness-of-fit statistics for RGA model

## Claude Code setup


    claude mcp add -s user reliagrowR -- Rscript -e "ReliaGrowR::rga_mcp_server()"

## Claude Desktop setup (claude_desktop_config.json)


    {
      "mcpServers": {
        "reliagrowR": {
          "command": "Rscript",
          "args": ["-e", "ReliaGrowR::rga_mcp_server()"]
        }
      }
    }

## Examples

``` r
if (FALSE) { # \dontrun{
# Start the server with the default stdio transport
rga_mcp_server()

# Start the server with an HTTP transport
rga_mcp_server(type = "http")
} # }
```
