defmodule Counter3 do
  def start do
    spawn(fn -> loop(0) end)
  end

  def inc(pid) do
    send(pid, {:inc, self()})

    receive do
      {:count, count} -> count
    end
  end

  defp loop(count) do
    receive do
      {:inc, pid} ->
        count = count + 1
        send(pid, {:count, count})
        loop(count)
    end
  end
end
