/* A hello world program, where we need a callback to
 * handle the server result. 
 * Compare with source01.nc.
 */
let f = (\x. x) in
(\name. call "GreetingService"(name) then f) "world"
