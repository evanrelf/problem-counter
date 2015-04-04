splitInput = (list) ->
  if list.indexOf(', ') isnt -1
    problemSets = list.split ', '
  else
    problemSets = [list] # Make sure this is an array and not a string,
                         # otherwise the for loop below will cycle through
                         # individual characters instead of items in the array

  splitProblems = []
  for set in problemSets # e.g. ['1-10', '11-21 odd', ...]
    if set.indexOf('-') isnt -1
      x = Number set.split('-')[0]
      y = set.split('-')[1]

      if set.indexOf(' ') isnt -1
        # With modifiers
        y = Number y.split(' ')[0]
        mod = set.split(' ')[1]
        splitProblems = splitProblems.concat [[x, y, mod]]
      else
        # Without modifiers
        splitProblems = splitProblems.concat [[x, Number y]]
    else # Single problems
      splitProblems = splitProblems.concat [[Number set]]

  return splitProblems


evaluateSet = (set) ->
  x = set[0]
  y = set[1] ? 'single'
  mod = set[2] ? 'none'

  switch mod
    when 'even', 'odd'
      return ((y - x) / 2) + 1
    when 'eoe', 'eoo'
      return Math.ceil (((y - x) / 2) + 1) / 2
    else
      if y is 'single'
        return 1
      else
        return (y - x) + 1


countProblems = (problems) ->
  split = splitInput problems
  total = 0
  for set in split
    total += evaluateSet(set)

  return total

browserPrompt = (preset) ->
  alert "Total problems: #{countProblems(preset or prompt "Input math problems:")}"

browserPrompt()

