class String
  def humanize
    tr("_", " ")
  end
end

class Person
  include Enumerable

  attr_accessor :name, :surname, :year_birth, :children, :parent

  @@num_people = 0

  def initialize(name, surname, year_birth, spouse_name = nil)
    @name = name
    @surname = surname
    @year_birth = year_birth
    @married_with = spouse_name
    @children = []
    @parent = nil
    @@num_people += 1
  end

  def full_name
    "#{@name} #{@surname}"
  end

  def number_of_people
    @@num_people
  end

  def add_child(new_child)
    new_child.parent = self
    @children.insert(
      @children.count { |child| child.year_birth < new_child.year_birth },
      new_child
    )
  end

  def add_spouse(spouse_name, spouse_children = [])
    @married_with = spouse_name
    spouse_children.each { |spouse_child| add_child(spouse_child) }
  end

  def traverse_bfs
    each(&proc { |value| puts value })
  end

  def each(&block)
    visited = []
    to_visit = []

    to_visit.prepend(self)
    bfs(visited, to_visit, &block)
  end

  def bfs(visited, to_visit, &block)
    return if to_visit.empty?

    current_node = to_visit.pop
    block.call(current_node)
    visited.append(current_node)

    current_node.children.each do |child|
      to_visit.prepend(child) unless visited.include?(child)
    end

    bfs(visited, to_visit, &block)
  end

  def <=>(other)
    if year_birth < other.year_birth
      -1
    elsif year_birth > other.year_birth
      1
    else
      0
    end
  end

  def to_s
    {
      name: @parent ? full_name + " child of #{@parent.full_name}" : full_name,
      year: @year_birth,
      married_with: @married_with,
      number_of_children: children.length
    }.compact
      .map { |key, value| "#{key.to_s.humanize}: #{value}" }
      .join("\n")
  end
end
