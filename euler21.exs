defmodule Euler do

  def divisor_sum(number) do
    Enum.sum(divisors(number))
  end

  def divisors(number) do
    upper_limit = max(1, div(number, 2))
    for n <- 1..upper_limit, rem(number, n) == 0, do: n
  end

  def is_amicable(number) do
    sum = divisor_sum(number)
    sum != number and divisor_sum(sum) == number
  end

  def euler21 do
    1..9999
    |> Enum.filter(&is_amicable/1)
    |> Enum.reduce(0, fn(x, acc) -> acc + divisor_sum(x) end )
  end
  # sum of amicable numbers under 10000
end

IO.puts(Euler.euler21())
