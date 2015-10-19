#!/usr/bin/ruby

def indent(program)
  indentation = 0
  lines = program.split("\n")
  lines.delete("\n")
  newlines = lines.collect do |l|
    indentation -= 2 if l == ")"
    newl = " " * indentation + l
    indentation += 2 if l[-1] == "("
    newl
  end
  newlines.join("\n")
end

program = gets
program.gsub!(/ExceptT \(Identity \(Right \(Program {program = \[(.*)\]}\)\)\)/, '\1')
program.gsub!('DeclarationAst {decl = ', '')
program.gsub!('ExpressionAst {astExp = ', '')
program.gsub!(', declPos = "<testfile>" (line 0, column 0)', '')
program.gsub!('}', '')
program.gsub!(/, varType = \w+, varPos = "<testfile>" \(line 0, column 0\)/, '')
program.gsub!(/, expType = \w+, expPos = "<testfile>" \(line 0, column 0\)/, '')
program.gsub!(/\(Signature.*?returnType = \w+\)/, '')
program.gsub!(',Extern', '')
program.gsub!('(', "(\n")
program.gsub!(')', "\n)\n")
puts indent(program)
