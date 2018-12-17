let words = [
  'abcdef',
  'bababc',
  'abbcde',
  'abcccd',
  'aabcdd',
  'abcdee',
  'ababab',
]

words.sort();

let map = {}
let prefix = ''
let prev_word = null

for(i = 0; i < words.length; ++i) {
  let word = words[i]
  
  console.log(map, prefix, word)

  if (!prev_word) {
    prev_word = word
    prefix = word
    continue
  }
  if (word.startsWith(prefix)) {
    do {
      console.log(`prefix: ${prefix}`)
      prefix = prev_word.substr(0, prefix.length + 1)
    } while (word.startsWith(prefix));
    prefix = prefix.slice(0, -1)
  } else {
    do {
      console.log(`prefix: ${prefix}`)
      prefix = prev_word.substr(0, prefix.length - 1)
      if (prefix == '') break;
    } while (!word.startsWith(prefix))
  }

  prev_word = word

  map[prefix] = [...(map[prefix] || []), word]

}

console.log(map)