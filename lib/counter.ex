defmodule Counter do
  def start do
    loop(0)
  end

  defp loop(count) do
    receive do
      :inc ->
        count = count + 1
        IO.puts(count)
        loop(count)
    end
  end
end
