#!/usr/bin/env ruby

# batch.rb - process a big batch of short (sentence- or paragraph-length) text units in parallel
# 2021-04-02
# William de Beaumont
#
# Run "./batch.rb --help" for usage information.

require_relative '../core/batch.rb'

Options.port_base = 6230
Options.trips_exe = 'trips-propolis'
Options.trips_argv = %w{-nouser}
Options.wait_for = 'TEXTTAGGER'
Options.web_parser_name = 'propolis'

main()
