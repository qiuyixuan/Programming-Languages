/* See what room Jane can get to via a secret passage. We make two
 * server calls, and so we need to have two callbacks, showRoom and
 * findSecretPassage. Compare with source02.nc.
 */
(\name.
  let showRoom = (\endRoom. name + " can get to " + endRoom + " by a secret passage") in 
  let findSecretPassage = (\startRoom. call "getSecretPassage"(startRoom) then showRoom) in
  call "getRoom"(name) then findSecretPassage)
"Jane"
