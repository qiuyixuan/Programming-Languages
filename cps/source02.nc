/* See what room Jane can get to via a secret passage. No need
 * for callbacks in this language! Compare with target02.ch.
 */
(\name.
  let rm = call "getRoom"(name) in
  name + " can get to " + (call "getSecretPassage"(rm)) + " by a secret passage")
"Jane"
