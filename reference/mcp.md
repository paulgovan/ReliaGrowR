# MCP Tool Definitions and Server for ReliaGrowR

Exposes the core ReliaGrowR functions as Model Context Protocol (MCP)
tools so AI assistants (e.g., Claude) can call them directly. Requires
the mcptools and ellmer packages.

## Details

Start the server from the command line:


      Rscript -e "ReliaGrowR::rga_mcp_server()"

Add it to Claude Code:


      claude mcp add -s user reliagrowR -- Rscript -e "ReliaGrowR::rga_mcp_server()"
