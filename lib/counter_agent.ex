defmodule CounterAgent do
  use Agent

  def start_link do
    Agent.start_link(fn -> 0 end)
  end

  def inc(pid) do
    Agent.get_and_update(pid, &{&1 + 1, &1 + 1})
  end
end
