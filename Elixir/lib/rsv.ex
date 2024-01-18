defmodule RSV do
  @moduledoc """
  Documentation for `RSV`.
  """

  defmodule ParseError do
    defexception [:message]
  end

  @terminate_value 0xFF
  @terminate_row 0xFD
  @null 0xFE

  @doc """
  Encode data containing lists of lists of String and Nil
  """
  def encode!(data_rows) when is_list(data_rows) do
    for row <- data_rows, into: <<>> do
      <<for(value <- row, into: <<>>, do: encode_value(value))::binary, @terminate_row>>
    end
  end

  defp encode_value(nil), do: <<@null, @terminate_value>>
  defp encode_value(value), do: <<value::binary, @terminate_value>>

  @doc """
  Decode RSV encoding data
  """
  def decode!(<<data::binary>>), do: do_decode(data, <<>>, [], [])

  defp do_decode(<<>>, <<>>, [], acc), do: Enum.reverse(acc)

  defp do_decode(<<char, rest::binary>>, value_acc, row_acc, acc) when char < 0xF8,
    do: do_decode(rest, value_acc <> <<char>>, row_acc, acc)

  defp do_decode(<<@terminate_row, rest::binary>>, <<>>, current_row, acc),
    do: do_decode(rest, <<>>, [], [current_row | acc])

  defp do_decode(<<@null, @terminate_value, rest::binary>>, <<>>, current_row, acc),
    do: do_decode(rest, <<>>, current_row ++ [nil], acc)

  defp do_decode(<<@terminate_value, rest::binary>>, current_value, current_row, acc) do
    if String.valid?(current_value) do
      do_decode(rest, <<>>, current_row ++ [current_value], acc)
    else
      # We are strict about UTF-8 encoding and decoding
      raise ParseError, "Invalid string data: #{inspect(current_value)}"
    end
  end

  # Parse Errors
  defp do_decode(<<>>, "", _row, _acc),
    do: raise(ParseError, "Reached end of data, expected an end of row terminator")

  defp do_decode(<<char, _rest::binary>>, _, _, _) when char >= 0xF8,
    do: raise(ParseError, "Encountered an invalid UTF-8 byte (<<#{char}>>).")
end
