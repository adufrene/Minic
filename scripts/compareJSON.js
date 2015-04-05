/*
   Compares two JSON files for equality by parsing the JSON as objects then
   comparing the objects with deep equality. Does no error handing. Throws
   exception if JSON is not equivalent, or anything else goes wrong.

   Requires Node.js

   TODO - things that would be nice:
   verbose mode (-v)
   error handling
   say how the JSON is different
*/

var args = process.argv.slice(2);
if (args.length < 2) {
   console.log("Usage: node compareJSON.js JSONfilePath1 JSONfilePath2")
}
else {
   var fs = require('fs');
   var assert = require('assert');

   var obj1 = JSON.parse(fs.readFileSync(args[0]));
   var obj2 = JSON.parse(fs.readFileSync(args[1]));

   assert.deepEqual(obj1, obj2);
}
