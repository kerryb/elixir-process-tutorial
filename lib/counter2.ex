defmodule Counter2 do
  def start(pid) do
    loop(pid, 0)
  end

  defp loop(pid, count) do
    receive do
      :inc ->
        count = count + 1
        send(pid, {:count, count})
        loop(pid, count)
    end
  end
end
