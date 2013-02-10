# A sample Guardfile
# More info at https://github.com/guard/guard#readme
#
#

def clear
    print "\e[2J\e[f"
end

def trycommand(str)
  if system(str)
    puts "Success!"
  else
    puts "Failure!"
  end
end

guard :shell do
  watch /.*\.hs$/ do |m|
    clear
    trycommand('bash -o pipefail -c \'ghc ' + m[0] + ' | grep -v Loading\'')
  end
end

guard :shell do
  watch /.*\.cabal$/ do |m|
    clear
    trycommand('bash -c \'cabal build\'')
  end
end
