(() => {
// Using ES6
'use strict';

/*
 * HELPER FUNCTIONS
 */

// contains :: (Eq a) => [a] -> a -> Bool
// FIXME: Call the `text` argument something more general, beacause a string is
//        not the only valid input
function contains(text, query) {
  const position = text.indexOf(query);

  if (position !== -1) {
    return true;
  } else {
    return false;
  }
}


/*
 * PROBLEM COUNTER
 */

// splitIntoSets :: String -> [String]
function splitIntoSets(problems) {
  if (contains(problems, ', ')) {
    return problems.split(', ');
  } else {
    return [problems];
  }
}

// NOTE: Maybe I should return an object instead?
// dissectSet :: String -> [Number, Number, String]
function dissectSet(set) {
  let x = 0;
  let y = 0;
  let mod = 'all';

  if (contains(set, '-')) { // range
    x = set.split('-')[0];
    if (contains(set, ' ')) {
      y = set.split(' ')[0]
             .split('-')[1];
      mod = set.split(' ')[1];
    } else {
      y = set.split('-')[1];
      mod = 'all';
    }
  } else { // single
    mod = 'single';
  }

  return [Number(x), Number(y), mod];
}

// evaluateSet :: [Number, Number, String] -> Number
function evaluateSet(set) {
  let [x, y, mod] = set;

  switch (mod) {
    case 'all':
      return (y - x) + 1;

    case 'even':
    case 'odd':
      return ((y - x) / 2) + 1;

    case 'eoe':
    case 'eoo':
      return Math.ceil (((y - x) / 2) + 1) / 2;

    case 'single':
      return 1;

    default:
      console.log('ERROR: Invalid modifier');
      return 0;
  }
}

// countProblems :: String -> Number
function countProblems(problems) {
  const sets = splitIntoSets(problems);
  return sets.map(dissectSet)
             .map(evaluateSet)
             .reduce((a, b) => a + b); // sum function
}


/*
 * TESTING
 * From http://evanrelf.com/problem-counter/
 */

const test1 = '1-10, 11-21 odd, 22-32 even, 33';
const test2 = '1-19, 34-64 eoe, 65, 66, 67-77 eoo';
const test3 = '1, 3, 6, 8-12, 13-23 odd';

console.log(`${countProblems(test1)} <= "${test1}"`);
console.log(`${countProblems(test2)} <= "${test2}"`);
console.log(`${countProblems(test3)} <= "${test3}"`);

})();
