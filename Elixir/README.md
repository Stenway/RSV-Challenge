# RSV Implementation for Elixir

See https://github.com/Stenway/RSV-Challenge/issues/3

Be sure to have Elixir and Mix installed. The tests depend on the `Jason` dependency for parsing the test fixtures, so be sure to run `mix deps.get`.

The tests can then be run with `mix test`.

The tests relies on test fixtures found in `../TestFiles`. Currently all the valid and invalid tests pass, but this might not be the case if an edge case has been added that this Elixir implementation doesn't account for. Should that be the case, please go ahead and fix it :)

The `RSV` module exposes two functions, `encode!/1` and `decode!/1`. Both will raise if they get invalid data. Many improvements could be made; non-bang versions of the two functions could be added (versions of the functions that returns ok/error-tuples instead of raising). Also, it would be interesting to support streaming. Feel free to add all this.
