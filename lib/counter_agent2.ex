defmodule CounterAgent2 do
  use Agent

  def start_link(_) do
    Agent.start_link(fn -> 0 end, name: __MODULE__)
  end

  def inc do
    Agent.get_and_update(__MODULE__, &{&1 + 1, &1 + 1})
  end
end
