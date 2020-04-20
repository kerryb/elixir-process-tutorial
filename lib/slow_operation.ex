defmodule SlowOperation do
  def run do
    seconds = :rand.uniform(10)
    Process.sleep(seconds * 1000)
    IO.puts(seconds)
    seconds
  end
end
