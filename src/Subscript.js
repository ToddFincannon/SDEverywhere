import util from 'util'
import R from 'ramda'
import B from 'bufx'
import yaml from 'js-yaml'
import { canonicalName, asort, vlog } from './Helpers.js'

// A subscript is a dimension or an index.
// Both have the same properties: model name, canonical name, family, values.

// |Property     |Description                                                       |
// |-------------|------------------------------------------------------------------|
// |modelName    |subscript name in Vensim format                                   |
// |modelValue   |subscript value in Vensim format                                  |
// |modelMappings|mappings from a dimension to a mapping value in Vensim format     |
// |name         |subscript name in canonical C format                              |
// |value        |subscript value in canonical C format                             |
// |mappings     |mappings from a dimension to a mapping value in canonical C format|
// |size         |number of indices in a dimension or 1 for an index                |
// |family       |subscript family on canonical C format (finalized later)          |

// A family is a dimension that is a reference to the maximal dimension containing an index or subdimension.
// The family of the maximal dimension is itself.
// Values are a single number for indices and an array of subscripts for dimensions.
// A Variable includes a list of subscripts in normal order.
// Mappings give a list of indices in the mapping value that map in order to to-dim indices.

// Subscript API
//
// Subscript(modelName, modelValue, modelFamily, modelMappings)
// with modelMappings of the form [ { toDim: dimension, value: indexArray } ]
//   sets a subscript dimension that maps the index elements in indexArray
//   to the index elements in another dimension
// Subscript(modelName)
//   gets the subscript with the model name
// Subscript(modelName, modelValue) with modelValue as array
//   sets a subscript dimension and its indices
// Subscript(modelName, modelValue, modelFamily) with modelValue as number
//   sets a subscript element and its index value in the subscript family
// Subscript(modelName, '', modelFamily, [])
//   sets modelName as an alias for the dimension named as modelFamily
//
// Call Subscript with an array of subscript indices to establish a dimension.
// If there is a mapping to another dimension, give modelMappings in the call.
// Then call Subscript with each individual subscript index name and its numeric index value.
// All dimension and index names are in model form, not canonical form.
// They must be converted to canonical form later after all subscripts are read.

// The Subscript module maintains a subscript map with the canonical
// subscript name as the key and a subscript object as the value.
let subscripts = new Map()

export function Subscript(modelName, modelValue = null, modelFamily = null, modelMappings = null) {
  let name = canonicalName(modelName)
  if (modelValue === null) {
    // Look up a subscript by its model name.
    return sub(name)
  }
  let value, size
  if (Array.isArray(modelValue)) {
    // Map the subscript value array into canonical form.
    value = R.map(x => canonicalName(x), modelValue)
    size = value.length
  } else if (typeof modelValue === 'number') {
    // The value is an index.
    value = modelValue
    size = 1
  } else if (modelValue === '') {
    // An empty value string indicates a subscript alias given by the modelFamily.
    value = ''
    size = 0
  }
  // Convert the model family into canonical form.
  if (modelFamily === null) {
    // The default family is the subscript itself.
    modelFamily = modelName
  }
  let family = canonicalName(modelFamily)
  // Convert the subscript mappings into canonical form.
  let mappings = {}
  if (modelMappings && !R.isEmpty(modelMappings)) {
    for (let m of modelMappings) {
      mappings[canonicalName(m.toDim)] = R.map(subName => canonicalName(subName), m.value)
    }
  }
  // Save the subscript as an object in the subscripts store.
  let subscript = {
    modelName,
    modelValue,
    modelMappings,
    name,
    value,
    size,
    family,
    mappings
  }
  subscripts.set(name, subscript)
  return subscript
}
export function sub(name) {
  // Look up a subscript by its canonical name.
  // Return undefined if the name is not a subscript name.
  let result
  try {
    result = subscripts.get(name)
  } catch (e) {
    console.error(`sub name ${name} not found`)
    debugger
  }
  return result
}
export function isIndex(name) {
  let s = sub(name)
  return s && typeof s.value === 'number'
}
export function isDimension(name) {
  let s = sub(name)
  return s && Array.isArray(s.value)
}
export function isTrivialDimension(name) {
  // Return true if the dimension values are trivial, i.e., {0, 1, 2, ..., n-1}
  let s = sub(name)
  if (!s || !Array.isArray(s.value)) {
    return false
  }
  // The following evaluates to true when all sub-dimensions match their position in the array
  return R.addIndex(R.all)((subdim, idx) => sub(subdim).value === idx, s.value)
}
export function addIndex(name, value, family) {
  // Add an index with arguments in canonical form.
  let subscript = {
    name: name,
    value: value,
    size: 1,
    family: family,
    mappings: {}
  }
  subscripts.set(name, subscript)
}
export function hasMapping(fromSubscript, toSubscript) {
  let result = false
  let subFrom = sub(fromSubscript)
  let subTo = sub(toSubscript)
  if (subFrom === undefined) {
    vlog('ERROR: undefined hasMapping fromSubscript', fromSubscript)
  }
  if (subTo === undefined) {
    vlog('ERROR: undefined hasMapping toSubscript', toSubscript)
  }
  if (subFrom.mappings[toSubscript]) {
    return true
  }
  return false
}
export function mapIndex(fromSubName, fromIndexName, toSubName) {
  // Return the index names that the fromSubName dimension maps from fromIndexName
  // to the toSubName dimension. Return an empty array if there is no such mapping.
  let toIndexNames = []
  let fromSub = sub(fromSubName)
  let toSub = sub(toSubName)
  if (fromSub && toSub && isDimension(fromSubName) && isDimension(toSubName)) {
    let mapping = fromSub.mappings[toSubName]
    let fromSubIndexNames = fromSub.value
    if (mapping) {
      // Find the positions of fromIndexName in the mapping.
      for (let pos = 0; pos < mapping.length; pos++) {
        if (mapping[pos] === fromIndexName) {
          // Return the index name at the same position in the toSub dimension.
          toIndexNames.push(toSub.value[pos])
        }
      }
    }
  }
  return toIndexNames
}
export function printSubscripts() {
  B.clearBuf()
  for (let [k, v] of subscripts) {
    B.emitLine(`${k}:\n${util.inspect(v, { depth: null })}\n`)
  }
  return B.getBuf()
}
export function yamlSubsList() {
  let subs = {}
  for (let [k, v] of subscripts) {
    subs[k] = v
  }
  return yaml.safeDump(subs)
}
export function loadSubscriptsFromYaml(yamlSubs) {
  // Load the subscripts map from subscripts serialized to a YAML file by yamlSubsList.
  // This function should be called instead of adding subscripts through the constructor.
  let subs = yaml.safeLoad(yamlSubs)
  for (const k in subs) {
    subscripts.set(k, subs[k])
  }
}
export function normalizeSubscripts(subscripts) {
  // Sort a list of subscript names already in canonical form according to the subscript family.
  let subs = R.map(name => sub(name), subscripts)
  subs = R.sortBy(R.prop('family'), subs)
  let normalizedSubs
  try {
    normalizedSubs = R.map(R.prop('name'), subs)
  } catch (e) {
    console.error(`normalizeSubscripts fails for ${subscripts}`)
    debugger
  }
  return normalizedSubs
}
export function extractMarkedDims(subscripts) {
  // Extract all marked dimensions and update subscripts.
  let dims = []
  for (let i = 0; i < subscripts.length; i++) {
    if (subscripts[i].includes('!')) {
      // Remove the "!" from the subscript name and save it as a marked dimension.
      subscripts[i] = subscripts[i].replace('!', '')
      dims.push(subscripts[i])
    }
  }
  return dims
}
export function subscriptFamilies(subscripts) {
  // Return a list of the subscript families for each subscript.
  try {
    return R.map(subscriptName => sub(subscriptName).family, subscripts)
  } catch (e) {
    console.error(`ERROR: subscript not found in "${subscripts.join(',')}" in subscriptFamilies`)
    debugger
  }
}
export function subscriptFamily(subscriptName) {
  // Return the subscript family object for the subscript name.
  let family = sub(subscriptName).family
  return sub(family)
}
export function allSubscripts() {
  // Return an array of all subscript objects.
  return [...subscripts.values()]
}
export function allDimensions() {
  // Return an array of all dimension subscript objects.
  return R.filter(subscript => Array.isArray(subscript.value), allSubscripts())
}
export function allAliases() {
  // Return an array of all subscript aliases.
  return R.filter(subscript => subscript.value === '', allSubscripts())
}
export function allMappings() {
  // Return an array of all subscript mappings as objects.
  let mappings = []
  R.forEach(subscript => {
    R.forEach(mapTo => {
      mappings.push({
        mapFrom: subscript.name,
        mapTo: mapTo,
        value: subscript.mappings[mapTo]
      })
    }, Object.keys(subscript.mappings))
  }, allSubscripts())
  return mappings
}
export function indexNamesForSubscript(subscript) {
  // Return a list of index names for a subscript in canonical form.
  if (isIndex(subscript)) {
    // The subscript is an index, so just return it.
    return [subscript]
  } else {
    // Return a list of index names for the dimension.
    let dim = sub(subscript)
    if (!dim) {
      vlog('ERROR: no indexNamesForSubscript', subscript)
      console.trace()
      return []
    }
    return dim.value
  }
}
export function separatedVariableIndex(rhsSub, variable, rhsSubscripts) {
  // Given an RHS subscript, find an LHS index in the separation dimension that matches it.
  // The LHS and RHS subscripts need not be in normal order or have the same number of subscripts in a var.
  // The search proceeds through three stages:
  // 1. Find a sepDim that matches rhsSub.
  // 2. Then find an lhsSub in the same family as the sepDim.
  // 3. Further qualify the lhsSub.

  // If rhsSub is found on the LHS, don't convert it into an index.
  if (!variable.subscripts.includes(rhsSub)) {
    // (1)
    for (let sepDim of variable.separationDims) {
      if (rhsSub === sepDim || hasMapping(rhsSub, sepDim)) {
        // (2)
        for (let lhsSub of variable.subscripts) {
          if (sub(lhsSub).family === sub(sepDim).family) {
            if (!isIndex(lhsSub)) {
              console.error(`ERROR: ${variable.refId} subscript in separation dimension ${sepDim} is not an index`)
            } else {
              // (3)
              if (rhsSub === sepDim) {
                // There may be more than one lhsSub in the same family as rhsSub.
                // Pick the one that belongs to the rhsSub.
                // If there are two LHS subs both in the same family, choose by position instead.
                if (
                  rhsSubscripts &&
                  variable.subscripts.length === 2 &&
                  rhsSubscripts.length === 2 &&
                  sub(variable.subscripts[0]).family === sub(variable.subscripts[1]).family
                ) {
                  let pos = rhsSubscripts.indexOf(rhsSub)
                  return variable.subscripts[pos]
                } else {
                  if (indexNamesForSubscript(rhsSub).includes(lhsSub)) {
                    return lhsSub
                  }
                }
              } else {
                // Find the index that maps from the subscript dimension to the separated var index.
                for (let fromIndexName of sub(rhsSub).value) {
                  let mappedIndices = mapIndex(rhsSub, fromIndexName, sepDim)
                  if (mappedIndices.includes(lhsSub)) {
                    return fromIndexName
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  return null
}
// Function to filter canonical dimension names from a list of names
export let dimensionNames = R.pipe(
  R.filter(subscript => isDimension(subscript)),
  asort
)
// Function to filter canonical index names from a list of names
export let indexNames = R.pipe(
  R.filter(subscript => isIndex(subscript)),
  asort
)
