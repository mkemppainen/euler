defmodule Euler do
  require Integer
  
  def euler21() do
    number = 28124
    abundants = 1..number |> Enum.filter(&is_abundant/1)

    1..number
    |> Enum.filter(&!is_sum_of_numbers(&1, abundants))
    |> Enum.sum
  end

  # return true if number is sum of two numbers
  # numbers must be sorted
  def is_sum_of_numbers(number, numbers) do
    reversed =
      numbers
      |> Enum.reverse
      |> Enum.drop_while(&(&1 > number))
    is_sum_of_numbers(number, numbers, reversed)
  end

  defp is_sum_of_numbers(_, [], _), do: false
  defp is_sum_of_numbers(_, _, []), do: false
  defp is_sum_of_numbers(number, [n|_], [m|_]) when number == n + m, do: true
  defp is_sum_of_numbers(_, [n|_], [m|_]) when n == m, do: false
  defp is_sum_of_numbers(number, [n|nums], [m|revs]) when n + m < number do
    is_sum_of_numbers(number, nums, [m|revs])
  end
  defp is_sum_of_numbers(number, numbers, [_|revs]) do
    is_sum_of_numbers(number, numbers, revs)
  end

  def is_abundant(number) do
    divisor_sum(number) > number
  end

  def divisor_sum(number) do
    Enum.sum(divisors(number))
  end

  def divisors(number) do
    upper_limit = max(1, div(number, 2))
    for n <- 1..upper_limit, rem(number, n) == 0, do: n
  end
end

IO.inspect Euler.euler21()
