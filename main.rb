# frozen_string_literal: true

require 'parser/current'
require 'CSV'
require 'optparse'

# Root of the classes that represent the structural elements of a Ruby program.
class EntityStats
  attr_reader :outer, :methods, :name

  def initialize(name, outer)
    @outer = outer
    @name = name

    @methods = []
    @classes = []
    @modules = []

    @outer_path = nil

    @locals = Set.new

    @line_usage = Hash.new('')
    @line_usage_counts = nil
  end

  def add_local(name)
    @locals.add(name)
  end

  def num_locals
    @locals.size
  end

  def num_methods
    @methods.size
  end

  def num_classes
    @classes.size
  end

  def num_modules
    @modules.size
  end

  def in_method?
    false
  end

  def line_usages(type)
    if @line_usage_counts.nil?
      @line_usage_counts = Hash.new(0)

      @line_usage.each_value do |v|
        @line_usage_counts[v] += 1
      end
    end

    @line_usage_counts[type]
  end

  def lines_used(node, type)
    return unless node.loc&.expression

    b = node.loc.expression.begin.line
    e = node.loc.expression.end.line
    @line_usage[b] = type
    @line_usage[e] = type
    return unless @outer

    @outer.lines_used(node, type)
  end

  def else_line_used(line, type)
    @line_usage[line] = type
    return unless @outer

    @outer.else_line_used(line, type)
  end

  def add_method(method)
    @methods.append(method)
    add_to_all_methods(method)
  end

  def add_class(clazz)
    @classes.append(clazz)
    add_to_all_classes(clazz)
  end

  def add_module(mod)
    @modules.append(mod)
    add_to_all_modules(mod)
  end

  def add_to_all_methods(method)
    @outer.add_to_all_methods(method)
  end

  def add_to_all_modules(mod)
    @outer.add_to_all_modules(mod)
  end

  def add_to_all_classes(clazz)
    @outer.add_to_all_classes(clazz)
  end

  def filename
    @outer.filename
  end

  def test?
    @outer.test?
  end

  def outer_path
    @outer_path = @outer.qualified_path if @outer_path.nil?
    @outer_path
  end

  def qualified_path
    "#{outer_path}.#{@name}"
  end

  def qualified_name_with_dot
    "#{@outer.qualified_name_with_dot}#{@name}."
  end
end

# FileStats is the root entity corresponding to the Ruby file.
class FileStats < EntityStats
  attr_reader :filename, :line_usage

  def initialize(filename, project_name, is_test)
    super(nil, nil)
    @filename = filename
    @project_name = project_name
    @is_test = is_test

    @instance_vars = Set.new
    @class_vars = Set.new

    @all_methods = Set.new
    @all_classes = Set.new
    @all_modules = Set.new
  end

  def add_to_all_methods(method)
    @all_methods.add(method)
  end

  def add_to_all_modules(mod)
    @all_modules.add(mod)
  end

  def add_to_all_classes(clazz)
    @all_classes.add(clazz)
  end

  def test?
    @is_test
  end

  def add_instance_var(name)
    @instance_vars.add(name)
  end

  def num_inst_vars
    @instance_vars.size
  end

  def num_class_vars
    @class_vars.size
  end

  def add_class_var(name)
    @class_vars.add(name)
  end

  def add_arg(name)
    p "FileStats.add_arg #{name} in #{@filename}"
  end

  def outer_path
    @project_name.to_s
  end

  def qualified_path
    @project_name.to_s
  end

  def qualified_name_with_dot
    ''
  end

  def line_use_type
    :file
  end

  def each_module
    @all_modules.each do |m|
      yield m
    end
  end

  def each_class
    @all_classes.each do |c|
      yield c
    end
  end

  def each_method
    @all_methods.each do |m|
      yield m
    end
  end
end

# Classes and modules have commonalities modeled here.
class ClassOrModuleStats < EntityStats
  def initialize(name, outer)
    super(name, outer)

    @instance_vars = Set.new
    @class_vars = Set.new
  end

  def add_instance_var(name)
    @instance_vars.add(name)
  end

  def num_inst_vars
    @instance_vars.size
  end

  def num_class_vars
    @class_vars.size
  end

  def add_class_var(name)
    @class_vars.add(name)
  end
end

# ClassStats represents the explicitly defined classes.
class ClassStats < ClassOrModuleStats
  attr_reader :superclass_name

  def initialize(class_name, superclass_name, outer)
    super(class_name, outer)
    @superclass_name = superclass_name

    @num_total_inst_vars = 0
    @num_superclasses = 0
  end

  def line_use_type
    :class
  end
end

# ModuleStats represents the explicitly defined modules.
class ModuleStats < ClassOrModuleStats
  def line_use_type
    :module
  end

  def qualified_name
    "#{@outer.qualified_name_with_dot}#{@name}"
  end
end

# MethodStats represents the explicitly defined methods, closures, lambdas,...
class MethodStats < EntityStats
  attr_reader :lines

  def initialize(name, lines, outer)
    super(name, outer)
    @lines = lines

    @arguments = []
  end

  def lines_of_code
    @line_usage.size
  end

  def line_use_type
    :method
  end

  def add_arg(name)
    @arguments.append(name)
  end

  def num_args
    @arguments.size
  end

  def num_locals
    @locals.size
  end

  def add_local(name)
    return if @arguments.include? name

    @locals.add(name)
  end

  def add_instance_var(name)
    @outer.add_instance_var(name)
  end

  def add_class_var(name)
    @outer.add_class_var(name)
  end

  def in_method?
    @outer.is_a? MethodStats
  end
end

# The StatsProcessor processes the AST to extract the metrics.
class StatsProcessor < Parser::AST::Processor
  # Look at https://github.com/whitequark/parser/blob/master/lib/parser/ast/processor.rb
  # https://github.com/whitequark/parser/blob/master/doc/AST_FORMAT.md

  attr_reader :current

  def initialize(filename, root_dir)
    super()
    @filename = filename
    @project_name = project_name(filename, root_dir)
    is_test = check_test(filename)
    @current = FileStats.new(filename, @project_name, is_test)

    @buffer = nil
  end

  def output_method(method_csv, method)
    method_csv << [
      @project_name, method.qualified_path, method.name,
      method.lines, method.lines_of_code, method.num_args, method.num_locals, 0, # method.num_literals,
      method.test?, method.in_method?,
      @filename
    ]
  end

  def output_module(modules_csv, mod)
    modules_csv << [
      @project_name,
      mod.qualified_name,
      mod.num_inst_vars,
      mod.num_class_vars,
      mod.num_classes,
      mod.num_methods,
      @current.test?,
      @filename
    ]
  end

  def output_class(classes_csv, clazz)
    classes_csv << [
      @project_name,
      clazz.name,
      clazz.num_inst_vars,
      clazz.num_class_vars,
      0,
      0,
      clazz.num_classes,
      clazz.num_methods,
      @current.test?,
      @filename
    ]
  end

  def output_results(file_csv, classes_csv, modules_csv, method_csv)
    # project totalLines fileScopeLines classScopeLines methodScopeLines moduleScopeLines isTest filename

    lines = @buffer.source_lines
    file_csv << [
      @project_name,
      lines.size,
      @current.line_usages(:file),
      @current.line_usages(:class),
      @current.line_usages(:method),
      @current.line_usages(:module),
      @current.test?,
      @filename
    ]

    # lines.each_with_index do |val,index|
    #     puts @current.line_usage[index + 1].to_s.ljust(10, " ") + "| " + val
    # end
    @current.each_module do |m|
      output_module(modules_csv, m)
    end

    @current.each_class do |c|
      output_class(classes_csv, c)
    end

    @current.each_method do |m|
      output_method(method_csv, m)
    end
  end

  def project_name(filename, root_dir)
    gem_dir = "/gem-local-shared/gems/"
    app_dir = "/apps/"
    local_name = filename.gsub(root_dir, '')
    if local_name.include? gem_dir
      gem_path = local_name.gsub(gem_dir, '')
      m = gem_path.match(%r{^([^/]+?)-([^-]+/)})
      return m[1] if m
    elsif local_name.include? app_dir
      app_path = local_name.gsub(app_dir, '')
      m = app_path.match(%r{^([^/]+)})
      return m[1] if m
    end
    return 'AWFY'
    raise "Can't extract project name from #{filename}"
  end

  def check_test(filename)
    (filename.end_with?  '_spec.rb' or filename.include? '/spec/' or
      filename.end_with? '_test.rb' or filename.include? '/test/' or
      filename.end_with? '_tests.rb' or filename.include? '/tests/' or
      filename.end_with? '_steps.rb' or filename.include? '/steps/' or
      filename.include? '/test_[^\/]+\.rb')
  end

  def start
    file_content = File.read(@filename)
    parser = Parser::CurrentRuby.new
    parser.diagnostics.consumer = lambda do |diag|
      # don't print out diagnostics, we're not really interested in them
    end
    @buffer = Parser::Source::Buffer.new(@filename, source: file_content)
    ast = parser.parse(@buffer)
    # ast = Parser::CurrentRuby.parse(file_content, @filename)
    process(ast)
  rescue Parser::SyntaxError => e
    # p "Parse error of #{@filename}: #{e}"
  end

  def class_name(node)
    node.children[0]&.children[1]
  end

  def superclass_name(node)
    return nil unless node.children[1]

    node.children[1].children[1]
  end

  def get_name(node)
    node.children[0]
  end

  def method_lines(node)
    b = node.loc.expression.begin.line
    e = node.loc.expression.end.line
    e - b + 1
  end

  ## Visitor/Processor Methods
  def process(node)
    @current.lines_used(node, @current.line_use_type) if node
    super
  end

  def on_class(node)
    name = class_name(node)
    super_name = superclass_name(node)
    cls = ClassStats.new(name, super_name, @current)
    @current.add_class(cls)

    outer = @current
    @current = cls
    @current.lines_used(node, @current.line_use_type)

    super

    @current = outer
  end

  def on_ivar(node)
    @current.add_instance_var(
      node.children[0][1..-1]
    )
  end

  def on_arg(node)
    @current.add_arg(get_name(node))
    super
  end

  def on_optarg(node)
    @current.add_arg(get_name(node))
    super
  end

  def on_restarg(node)
    name = get_name(node)
    @current.add_arg(get_name(node)) if name
    super
  end

  def on_blockarg(node)
    @current.add_arg(get_name(node))
    super
  end

  def on_lvar(node)
    @current.add_local(get_name(node))
    super
  end

  def on_cvar(node)
    @current.add_class_var(get_name(node))
    super
  end

  def on_def(node)
    m = start_method(node, get_name(node))
    outer = @current
    @current = m
    @current.lines_used(node, @current.line_use_type)

    super
    @current = outer
  end

  def on_defs(node)
    m = start_method(node, node.children[1])
    outer = @current
    @current = m
    @current.lines_used(node, @current.line_use_type)
    super
    @current = outer
  end

  def on_block(node)
    m = start_method(node, '$block')
    outer = @current
    @current = m
    super
    @current = outer
  end

  def on_if(node)
    is_if_node = !node.loc.respond_to?(:keyword) ||
                 node.loc.keyword.source == 'if' ||
                 node.loc.keyword.source == 'elsif'
    else_idx = is_if_node ? 2 : 1

    if node.children[else_idx]
      @current.else_line_used(node.children[else_idx].loc.expression.begin.line - 1, @current.line_use_type)
    end
    super
  end

  def on_case(node)
    if node.children[-1]
      @current.else_line_used(node.children[-1].loc.expression.begin.line - 1, @current.line_use_type)
    end
    super
  end

  def on_lambda(node)
    m = start_method(node, '$lambda')
    outer = @current
    @current = m
    super
    @current = outer
  end

  def start_method(node, name)
    lines = method_lines(node)
    m = MethodStats.new(name, lines, @current)

    @current.add_method(m)
    m
  end

  def on_module(node)
    name = node.children[0].children[1]

    mod = ModuleStats.new(name, @current)
    @current.add_module(mod)

    outer = @current
    @current = mod
    @current.lines_used(node, @current.line_use_type)

    super
    @current = outer
  end
end

if ARGV.size < 2
  puts 'Usage: main.rb input-folder stats-out-folder'
  exit 1
end

output_folder = ARGV[1]
input_folder = ARGV[0]

file_filename = "#{output_folder}/file.csv"
modules_filename = "#{output_folder}/modules.csv"
method_filename = "#{output_folder}/methods.csv"
classes_filename = "#{output_folder}/classes.csv"

file_file = File.open(file_filename, 'w')
method_file = File.open(method_filename, 'w')
modules_file = File.open(modules_filename, 'w')
classes_file = File.open(classes_filename, 'w')

file_csv = CSV.new(file_file)
file_csv << %w[project totalLines fileScopeLines classScopeLines methodScopeLines moduleScopeLines isTest filename]
classes_csv = CSV.new(classes_file)
classes_csv << %w[project class numDirectInstVars numClassVars numTotalInstVars numSuperclasses numClasses numMethods isTest filename]
modules_csv = CSV.new(modules_file)
modules_csv << %w[project module numInstVars numClassVars numClasses numMethods isTest filename]
method_csv = CSV.new(method_file)
method_csv << %w[project qualifiedPath name lines linesOfCode numArgs numLocals numLiterals isTest isInMethod filename]

root_dir = input_folder
search_glob = "#{root_dir}/**/*.rb"

Dir.glob(search_glob) do |filename|
  begin
    processor = StatsProcessor.new(filename, root_dir)
    processor.start
    processor.output_results(file_csv, classes_csv, modules_csv, method_csv)
  rescue StandardError => e
    puts "Some issue processing #{filename}. Exception: #{e}"
    puts e.backtrace
  end
end

file_file.close
method_file.close
classes_file.close

# https://www.netguru.com/blog/most-loved-ruby-on-rails-open-source
