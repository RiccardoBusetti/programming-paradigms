require_relative "person"

module Testable
  module Assertions
    class AssertionFailed < StandardError; end

    def assert_equal(received, expected)
      unless received.eql?(expected)
        raise AssertionFailed.new("Expected #{expected} but got #{received}.")
      end
    end

    def assert_not_equal(received, expected)
      if received.eql?(expected)
        raise AssertionFailed.new("Not expecting #{expected} but got #{received}.")
      end
    end
  end

  module Engine
    class TestRunner
      def initialize(clazz)
        @clazz = clazz.new
      end

      def test_all
        successful_tests = 0
        time_elapsed = profile {
          successful_tests = clazz_test_methods
            .map { |method_name| test_method(method_name) }
            .compact
            .length
        }

        show_tests_results(
          successful_tests,
          clazz_test_methods.length,
          time_elapsed
        )
      end

      private

      def clazz_test_methods
        @clazz.methods
          .grep(/test.+/)
      end

      def test_method(method_name)
        @clazz.send(method_name)
        true
      rescue Assertions::AssertionFailed => failure
        puts failure
        nil
      end

      def show_tests_results(successful_tests, all_tests, time_elapsed)
        puts "#{successful_tests}/#{all_tests} tests executed successfully in #{time_elapsed}ms."
      end

      def profile
        start = Time.now
        yield
        finish = Time.now

        ((finish.to_f - start.to_f) * 1000).round(2)
      end
    end
  end
end

class PersonTest
  include Testable::Assertions

  def test_each_method
    riccardo = Person.new("Riccardo", "Busetti", 2000)
    riccardo.add_spouse("Anna")

    luca = Person.new("Luca", "Busetti", 2050)
    alessio = Person.new("Alessio", "Busetti", 2100)
    luca.add_child(alessio)

    giulio = Person.new("Giulio", "Busetti", 2030)
    carlo = Person.new("Carlo", "Busetti", 2090)
    giulio.add_child(carlo)

    riccardo.add_child(luca)
    riccardo.add_child(giulio)

    expected = [riccardo, giulio, luca, carlo, alessio]
    received = []
    # Instead of testing if it prints on the console, we simply save all the elements
    # and compare them with our expectations.
    riccardo.each(&proc { |person| received.append(person) })

    assert_equal(received, expected)
  end

  def test_each_method_search_existing
    riccardo = Person.new("Riccardo", "Busetti", 2000)

    luca = Person.new("Luca", "Busetti", 2050)
    alessio = Person.new("Alessio", "Busetti", 2100)
    luca.add_child(alessio)

    giulio = Person.new("Giulio", "Busetti", 2030)
    carlo = Person.new("Carlo", "Busetti", 2090)
    giulio.add_child(carlo)

    riccardo.add_child(luca)
    riccardo.add_child(giulio)

    to_search_name = giulio.name
    found = false
    riccardo.each(&proc do |person|
      if person.name == to_search_name
        found = true
        return
      end
    end)

    assert_equal(found, true)
  end

  def test_each_method_search_not_existing
    riccardo = Person.new("Riccardo", "Busetti", 2000)

    luca = Person.new("Luca", "Busetti", 2050)
    alessio = Person.new("Alessio", "Busetti", 2100)
    luca.add_child(alessio)

    giulio = Person.new("Giulio", "Busetti", 2030)
    carlo = Person.new("Carlo", "Busetti", 2090)
    giulio.add_child(carlo)

    riccardo.add_child(luca)
    riccardo.add_child(giulio)

    to_search_name = ""
    found = false
    riccardo.each(&proc do |person|
      if person.name == to_search_name
        found = true
        return
      end
    end)

    assert_equal(found, false)
  end

  def test_sort
    riccardo = Person.new("Riccardo", "Busetti", 2000)

    luca = Person.new("Luca", "Busetti", 2050)
    alessio = Person.new("Alessio", "Busetti", 2100)
    luca.add_child(alessio)

    giulio = Person.new("Giulio", "Busetti", 2030)
    carlo = Person.new("Carlo", "Busetti", 2090)
    giulio.add_child(carlo)

    riccardo.add_child(luca)
    riccardo.add_child(giulio)

    expected = [riccardo, giulio, luca, carlo, alessio]
    received = riccardo.sort

    assert_equal(received, expected)
  end
end

Testable::Engine::TestRunner
  .new(PersonTest)
  .test_all
