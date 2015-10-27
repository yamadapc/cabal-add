import std.algorithm : countUntil;
import std.array : array, empty;
import std.conv : to;
import std.digest.md;
import std.file : SpanMode, dirEntries, getcwd;
import std.functional : not;
import std.path : buildPath, baseName;
import std.stdio : File, stderr, stdin, writeln, writefln;
import std.uni : isWhite;

int main(string[] args) {
  if(args.length < 2) {
    stderr.writeln("Usage: cabal-add <dependency> <cabal-file>");
    return 1;
  } else if(args.length < 3) {
    auto resolved = findCabalFile();

    if(resolved is null) {
      stderr.writeln("Usage: cabal-add <dependency> <cabal-file>");
      stderr.writeln("Error: No <cabal-file> provided and couldn't find it.");
      return 1;
    }

    writefln("Using cabal-file: %s", resolved.baseName);
    args ~= resolved;
  }

  string dependency = args[1];
  string target = args[2];
  auto inputfile = File(target, "r");
  auto tmp = File.tmpfile();

  MD5 oldMd5;
  MD5 newMd5;

  foreach(line; inputfile.byLine) {
    auto newLine = processLine(dependency, line);
    oldMd5.put(cast(ubyte[]) line);
    newMd5.put(cast(ubyte[]) newLine);
    tmp.writeln(newLine);
  }

  inputfile.close();
  tmp.rewind();

  if(oldMd5.finish() == newMd5.finish()) {
    stderr.writeln("Error: Nothing changed in the file");
    return 1;
  }

  writefln("%s -> %s", oldMd5.finish().toHexString, oldMd5.finish().toHexString);
  writefln("Dependency %s added", dependency);

  auto outputfile = File(target, "w");

  foreach(line; tmp.byLine) {
    outputfile.writeln(line);
  }

  return 0;
}

string findCabalFile() {
  auto cwd = getcwd();
  auto candidates = cwd.dirEntries("*.cabal", SpanMode.shallow).array;
  if(candidates.empty) {
    return null;
  }
  return candidates[0];
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
