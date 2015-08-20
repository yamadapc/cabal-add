import std.algorithm : countUntil;
import std.conv : to;
import std.file : getcwd;
import std.functional : not;
import std.path : buildPath;
import std.stdio : File, stderr, stdin, writeln;
import std.uni : isWhite;

int main(string[] args) {
  if(args.length < 3) {
    stderr.writeln("Usage: cabal-add <dependency> <cabal-file>");
    return 1;
  }
  string dependency = args[1];
  string target = args[2];
  auto inputfile = File(target, "r");
  auto tmp = File.tmpfile();

  foreach(line; inputfile.byLine) {
    tmp.writeln(processLine(dependency, line));
  }

  inputfile.close();
  tmp.rewind();

  auto outputfile = File(target, "w");

  foreach(line; tmp.byLine) {
    outputfile.writeln(line);
  }

  return 0;
}

string processLine(string dependency, char[] line) {
  auto dependsPos = line.countUntil("build-depends:");

  if(dependsPos == -1) {
    return line.to!string;
  }

  auto rest = line[dependsPos + "build-depends:".length..$];
  auto dependencyPos = rest.countUntil!(not!isWhite);

  string output = line.to!string ~ "\n";

  for(auto i = 0; i < dependsPos + "build-depends:".length + dependencyPos - 2; i++) {
    output ~= " ";
  }

  output ~= ", ";
  output ~= dependency;
  return output;
}
